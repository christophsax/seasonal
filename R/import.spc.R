#' Import X-13 \code{.spc} Files (experimental)
#' 
#' Utility function to import \code{.spc} files from X-13. It generates a list
#' of calls to \code{seas} (and  \code{import.ts}) that can be run in R.
#' Evaluating these calls should perform the same X-13 procedure as the original
#' \code{.spc} file. The \code{print} method displays the calls in a way that 
#' they can be copy-pasted into an R script.
#' 
#' @param file   character, name of the X-13 \code{.spc} file
#' @return returns an object of class \code{import.spc}, which is a list with the following (optional) objects of class \code{call}:
#'   \item{x}{the call to retrieve the data for the input series} 
#'   \item{xtrans}{the call to retrieve the data for the \code{xtrans} series (if required by the call)} 
#'   \item{xreg}{the call to retrieve the data for the \code{xreg} series (if required by the call)} 
#'   \item{seas}{the call to \code{\link{seas}}}
#' @export
#' @seealso \code{\link{import.ts}}, for importing X-13 data files.
#' @seealso \code{\link{seas}} for the main function of seasonal.
#' @examples
#' 
#' # importing the orginal X-13 example file
#' import.spc(system.file("tests", "Testairline.spc", package="seasonal"))
#' 
#' \dontrun{
#' 
#' ### reading .spc with multiple user regression and transformation series
#' 
#' # running a complex seas call and save output in a temporary directory
#' tdir <- tempdir()
#' seas(x = AirPassengers, xreg = cbind(a = genhol(cny, start = 1, end = 4,
#'     center = "calendar"), b = genhol(cny, start = -3, end = 0,
#'     center = "calendar")), xtrans = cbind(sqrt(AirPassengers), AirPassengers^3), 
#'     transform.function = "log", transform.type = "temporary", 
#'     regression.aictest = "td", regression.usertype = "holiday", dir = tdir, 
#'     out = TRUE)
#' 
#' # importing the .spc file from the temporary location
#' ll <- import.spc(file.path(tdir, "iofile.spc"))
#' 
#' # ll is list containing four calls: 
#' # - 'll$x', 'll$xreg' and 'll$xtrans': calls to import.ts(), which read the
#' #   series from the X-13 data files
#' # - 'll$seas': a call to seas() which performs the seasonal adjustment in R
#' str(ll)
#'

#' # to replicate the original X-13 operation, run all four calls in a series.
#' # You can either copy/paste and run the print() output:
#' ll
#' 
#' # or use eval() to evaluate the call(s). To evaluate the first call and
#' # import the x variable:
#' eval(ll$x)
#' 
#' # to run all four calls in 'll', use lapply() and eval():
#' ee <- lapply(ll, eval, envir = globalenv())
#' ee$seas  # the 'seas' object, produced by the final call to seas()
#' }
import.spc <- function(file){
  
  stopifnot(file.exists(file))

  z <- list()

  txt <- readLines(file)
  txt <- gsub("\\\\", "/", txt)  # window file names to unix
  txt <- gsub("#.*$", "", txt) # remove comments

  # keep everything lowercase, except filenames
  pp.cap <- parse_spc(txt)
  pp <- parse_spc(tolower(txt))
  pp[['series']][['file']] <- pp.cap[['series']][['file']]
  pp[['transform']][['file']] <- pp.cap[['transform']][['file']]
  pp[['regression']][['file']] <- pp.cap[['regression']][['file']]

  xstr <- ext_ser_call(pp$series, "x")
  xregstr <- ext_ser_call(pp$regression, "xreg")
  xtransstr <- ext_ser_call(pp$transform, "xtrans")

  # clean args that are produced by seas
  pp[c("series", "regression", "transform")] <- lapply(pp[c("series", "regression", "transform")], function(spc) spc[!names(spc) %in% c("file", "data", "start", "name", "title", "format", "period", "user")])
  
  if (identical(pp$series, structure(list(), .Names = character(0)))){
    pp$series <- NULL
  }

  # construct the main call
  ep <- expand_spclist_to_args(pp)
  ep <- rem_defaults_from_args(ep)

  # add xtrans, xreg and x as series
  if (!is.null(xtransstr)) ep <- c(list(xtrans = quote(xtrans)), ep)
  if (!is.null(xregstr)) ep <- c(list(xreg = quote(xreg)), ep)
  ep <- c(list(x = quote(x)), ep)

  z$x <- if (!is.null(xstr)) parse(text = xstr)[[1]]
  z$xtrans <- if (!is.null(xtransstr)) parse(text = xtransstr)[[1]]
  z$xreg <- if (!is.null(xregstr)) parse(text = xregstr)[[1]]

  z$seas <- as.call(c(quote(seas), ep))

  class(z) <- "import.spc"
  z

}

#' @export
#' @rdname import.spc
#' @method print import.spc
#' @param x    object of class \code{import.spc}
#' @param ... further arguments, not used
print.import.spc <- function(x, ...){

  inps <- x[!names(x) == "seas"]
  if (length(inps) > 0){
    cat("## import input series\n")
    lapply(x[!names(x) == "seas"], print)
    cat("\n")
  }

  cat("## main call to 'seas'\n")
  print(x$seas)
}




ext_ser_call <- function(spc, vname){
  if (is.null(spc)) return(NULL)

  # analyze series spec
  if ("data" %in% names(spc)){
    start <- start_date_x13_to_ts(spc$start)

    f <- if (is.null(spc$period)) 12 else spc$period

    xstr <- paste0(vname, " <- ts(", 
                paste(deparse(spc$data, control = "all"), collapse = ""), 
                ", start = ", deparse(start), ", frequency = ", f, ")")
 
  } else if ("file" %in% names(spc)){
    
    frm <- rem_quotes(spc$format)

    # fragment for name, for fortran and x11 series
    if (!is.null(spc$name)) {  
      nm <- rem_quotes(spc$name)
      if (frm %in% c("cs", "cs2")){
        nm <- substr(nm, 1, 8)
      } else {
        nm <- substr(nm, 1, 8)
      }
      nmstr <- paste0(', name = "', nm, '"')
    } else {
      nmstr <- ""
    }

    if (frm == "datevalue"){
      xstr <- paste0(vname, ' <- import.ts(', spc$file, ')')
    } else if (frm %in% c("datevaluecomma", "x13save")){
      xstr <- paste0(vname, ' <- import.ts(', spc$file, ', format = "', frm, '")')
    } else if (frm %in% c("1r", "2r", "1l", "2l", "2l2", "cs", "cs2")){
      frequency <- if (is.null(spc$period)) 12 else spc$period
      xstr <- paste0(vname, ' <- import.ts(', spc$file, ', format = "', frm, '", frequency = ', frequency, nmstr, ')')
    } else {
      start <- start_date_x13_to_ts(spc$start)
      frequency <- if (is.null(spc$period)) 12 else spc$period
      xstr <- paste0(vname, ' <- import.ts(', spc$file, ', format = "', frm, '", start = ', deparse(start) , ', frequency = ', frequency, nmstr, ')')
    } 
  } else {
    return(NULL)
  }

  xstr

}


start_date_x13_to_ts <- function(x){
  sspl <- strsplit(as.character(x), "\\.")[[1]]
  s1 <- sspl[1]
  s2 <- sspl[2]

  if (s2 %in% tolower(month.abb)){
    s2 <- match(sspl[2], tolower(month.abb))
  }
  as.numeric(c(s1, s2))
}





rem_quotes <- function(x){
  x <- gsub('"', '', x)
  gsub("'", "", x)
}

expand_spclist_to_args <- function(ll){
  # substitute empty names lists by ""
  ll[sapply(ll, identical, structure(list(), .Names = character(0)))] <- ""
  do.call("c", ll)
}



rem_defaults_from_args <- function(x) {
  z <- x

  # default arguents
  e <- list(seats.noadmiss = "yes", 
             transform.function = "auto",
             regression.aictest = c("td", "easter"),
             outlier = "",
             automdl = "")

  xe <- x[names(x) %in% names(e)]
  ex <- e[names(xe)]
  ne <- names(xe)[unlist(Map(identical, xe, ex))]
  z[ne] <- NULL


  # output defaults
  d <- list(transform.print = "aictransform",
             automdl.print = "bestfivemdl",
             estimate.save = c("model", "estimates", "lkstats", "residuals"),
             spectrum.print = "qs",
             x11.save = c("d10", "d11", "d12", "d13", "d16", "e18"),
             seats.save = c("s10", "s11", "s12", "s13", "s16", "s18")
             )



  xd <- x[names(x) %in% names(d)]
  dx <- d[names(xd)]
  z[names(xd)] <- Map(setdiff, xd, dx)

  z[lapply(z, length) == 0] <- NULL


  # set these non-present specs to NULL
  if (!any(grepl("^automdl", names(x)))) {z['automdl'] <- list(NULL)}
  if (!any(grepl("^outlier", names(x)))) {z['outlier'] <- list(NULL)}
  if (!"regression.aictest" %in% names(x)) {z['regression.aictest'] <- list(NULL)}


  # make sure not to loose x11 with default save
  if (any(grepl("^x11", names(x))) & !any(grepl("^x11", names(z)))){
    z$x11 <- ""
  } 

  z
}


#' Import Time Series from X-13 Data Files (experimental)
#' 
#' Utility function to read time series from X-13 data files. A call to
#' \code{import.ts} is constructed and included in the output of
#' \code{\link{import.spc}}.
#' 
#' @param file character, name of the X-13 file which the data are to be read from
#' @param format a valid X-13 file format as described in 7.15 of the
#'  X-13 manual: \code{"datevalue"}, \code{"datevaluecomma"}, \code{"free"}, 
#'  \code{"freecomma"}, \code{"x13save"}, \code{"tramo"} or an X-11 or Fortran format.
#' @param start vector of length 2, time of the first observation (only for
#'   formats \code{"free"} and \code{"freecomma"} and the Fortran formats.)
#' @param frequency  the number of observations per unit of time (only for 
#'   formats \code{"free"}, \code{"freecomma"} and the X-11 or Fortran formats.)
#' @param name  (X-11 formats only) name of the series, to select from a 
#'   file with multiple time series. Omit if you want to read all time series from an X-11 format file.
#' @export
#' @return an object of class \code{ts} or \code{mts}
#' @seealso \code{\link{import.spc}}, for importing X-13 \code{.spc} files.
#' @seealso \code{\link{seas}} for the main function of seasonal.

#' @examples
#' \dontrun{
#' tdir <- tempdir()
#' seas(x = AirPassengers, dir = tdir) 
#' import.ts(file.path(tdir, "data.dta"))
#' import.ts(file.path(tdir, "iofile.rsd"), format = "x13save")
#' }
import.ts <- function(file, 
                    format = "datevalue", 
                    start = NULL, frequency = NULL, name = NULL){
  
  stopifnot(file.exists(file))


  if (format == "x13save"){
    return(read_series(file))
  }  
  if (format == "tramo"){
    return(import_tramo(file))
  }
  if (format %in% c("1r", "2r", "1l", "2l", "2l2", "cs", "cs2")) {
    stopifnot(!is.null(frequency))
    format <- x11_to_fortran(format, frequency)
    return(import_fortran(file = file, format = format, frequency = frequency, name = name))
  } 


  if (format %in% c("datevalue", "datevaluecomma", "free", "freecomma")){
    txt <- readLines(file)

    dec <- if (format %in% c("datevaluecomma", "freecomma")) "," else "."
    sep <- if (grepl("\\t", txt[2])) "\t" else " "

    txt <- gsub(" +", " ", txt)
    txt <- gsub("^ | $", "", txt)

    dta <- read.table(text = txt, sep = sep, dec = dec)

    if (format %in% c("datevalue", "datevaluecomma")){
      frequency <- length(unique(dta[, 2]))
      start <- c(as.matrix(dta[1, 1:2]))
      dta <- dta[,-c(1:2)]
    } 

  } else if (grepl("[\\(.+\\)]", format)){  # fortran format
    stopifnot(!is.null(frequency))
    stopifnot(!is.null(start))
    dta <- import_fortran(file = file, format = format, frequency = frequency, start = start, name = name)
  } else {
    stop("no valid format.")
  }

  z <- ts(unname(dta), frequency = frequency, start = start)
  z <- na.omit(z)
  z
  

}



x11_to_fortran <- function(x, frequency) {
  stopifnot(frequency %in% c(4, 12))
  stopifnot(x %in% c("1r", "2r", "1l", "2l", "2l2", "cs", "cs2"))
  p <- if (frequency == 4) "q" else "m"

  # x11fort <- data.frame(m = c("(12f6.0,i2,a6)", 
  #                           "(6f12.0,/,6f12.0,i2,a6)", 
  #                           "(a6,i2,12f6.0)", 
  #                           "(a6,i2,6f12.0,/,8x,6f12.0)" , 
  #                           "(a8,i4,6f11.0,2x,/,12x,6f11.0,2x)", 
  #                           "(a8,i2,10x,12e16.10,18x) ", 
  #                           "(a8,i4,12x,12e16.10,14x)"),
  #                     q = c("(4(12x,f6.0),i2,a6)", 
  #                           "(4f12.0,24x,i2,a6)", 
  #                           "(a6,i2,4(12x,f6.0))", 
  #                           "(a6,i2,4f12.0)", 
  #                           "(a8,i4,4f11.0,2x)", 
  #                           "(a8,i2,10x,12e16.10,18x)", 
  #                           "(a8,i4,12x,12e16.10,14x)"),
  #                     stringsAsFactors = FALSE)


  x11fort <- data.frame(m = c("(12f6.0,i2,a6)", 
                            "(6f12.0,/,6f12.0,i2,a6)", 
                            "(a6,i2,12f6.0)", 
                            "(a6,i2,6f12.0,/,8x,6f12.0)" , 
                            "(a8,i4,6f11.0,2x,/,12x,6f11.0,2x)", 
                            "(a8,i2,10x,12e16.10,18x)", 
                            "(a8,i4,12x,12e16.10,14x)"),
                      q = c("(12x,f6.0,12x,f6.0,12x,f6.0,12x,f6.0,i2,a6)", 
                            "(4f12.0,24x,i2,a6)", 
                            "(a6,i2,12x,f6.0,12x,f6.0,12x,f6.0,12x,f6.0)", 
                            "(a6,i2,4f12.0)", 
                            "(a8,i4,4f11.0,2x)", 
                            "(a8,i2,10x,12e16.10,18x)", 
                            "(a8,i4,12x,12e16.10,14x)"),
                      stringsAsFactors = FALSE)



  rownames(x11fort) = c("1r", "2r", "1l", "2l", "2l2", "cs", "cs2")
  x11fort[x, p]
}



parse_fortran_format <- function(format){
  if (grepl("/", format)){
    return(lapply(strsplit(format, "/")[[1]], parse_fortran_format))
  }
  z <- strsplit(gsub("[\\(\\)]", "", format), ",")[[1]]
  z[z != ""]
}


import_fortran <- function(file, format, frequency, start = NULL, name = NULL){
  zr <- read.fortran(file, parse_fortran_format(format))

  # remove "\032" empty lines
  zr <- zr[zr[, 1] != "\032", ]

  zrcl <- sapply(zr, class)

  vcol <- which(zrcl == "character")
  ycol <- which(zrcl == "integer")

  zl <- split(zr, zr[, vcol])

  to_ts <- function(x){
    if (is.null(start)){
      sty <- x[1, c(ycol)]  
      if (nchar(sty) == 2){   # if start yeear has 2 digits, guess 4 digits
        sty <- if (sty > 45) sty + 1900 else sty + 2000
      }
      x <- x[, -c(vcol, ycol)]
      start <- c(sty, 1)
    }
    ts(c(t(as.matrix(x))), start = start, frequency = frequency)
  }

  zlts <- lapply(zl, to_ts)

  z <- do.call("cbind", zlts)


  if (!is.null(colnames(z))){
    colnames(z) <- tolower(colnames(z))
    colnames(z) <- gsub(" +$", "", colnames(z))
  }

  if (!is.null(name) && !is.null(dim(z))){
    z <- z[, name]
  }

  z2 <- try(na.omit(z), silent = TRUE)
  if (!inherits(z2, "try-error")){
    z <- z2
  }
  z
}


import_tramo <- function(file){
  txt <- readLines(file)
  ssp <- strsplit(gsub("^ +| +$", "", txt[2]), " ")[[1]]
  if (length(ssp) != 4){
    stop("tramo format: line 2 must have 4 elements.")
  }
  ssp <- as.integer(ssp)


  z <- as.numeric(txt[3:length(txt)])
  if (length(z) != ssp[1]){
    message("tramo format: number of obs. different to speicification, which will be ignored.")
  }

  ts(z, start = c(ssp[2], ssp[3]), frequency = ssp[4])
}




