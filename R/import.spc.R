#' Import X-13 \code{.spc} Files to R (experimental)
#' 
#' Helper function to read time series from X-13 input files. It generates a
#' list of calls that can be run in R, replicating the X-13 spc file. The print
#' method displays the calls in way so they can be copy-pasted into an R script.
#' \bold{This is an experimental function that may change substantially in the future.}
#' 
#' @param file   character, name of the X-13 spc file
#' @return returns an object of class \code{import.spc}, which is list with the following (optional) elements:
#'   \item{call}{object of class \code{call}, the call to \code{\link{seas}}}
#'   \item{x}{object of class \code{call}, the call to retrieve the data for the input series} 
#'   \item{xtrans}{object of class \code{call}, the call to retrieve the data for the \code{xtrans} series (if required by the call)} 
#'   \item{xreg}{object of class \code{call}, the call to retrieve the data for the \code{xreg} series (if required by the call)} 
#' @export
#' @examples
#' 
#' # importing the orginal X-13 example file
#' import.spc(file.path(path.package("seasonal"), "tests", "Testairline.spc"))
#' 
#' \dontrun{
#' # a spc with multiple user defined regression and transformation series
#' tdir <- tempdir()
#' seas(x = AirPassengers, xreg = cbind(a = genhol(cny, start = 1, end = 4,
#'     center = "calendar"), b = genhol(cny, start = -3, end = 0,
#'     center = "calendar")), xtrans = cbind(sqrt(AirPassengers), AirPassengers^3), 
#'     transform.function = "log", transform.type = "temporary", 
#'     regression.aictest = "td", regression.usertype = "holiday", dir = tdir, 
#'     out = TRUE)
#' ii <- import.spc(file.path(tdir, "iofile.spc"))
#' ii  # list with 4 calls (3 series import, 1 main call)
#' 
#' # evaluating the imported calls in R
#' ee <- lapply(ii, eval, envir = globalenv())
#' ee$call  # the 'seas' object produced from the .spc file
#' }
import.spc <- function(file){
  
  stopifnot(file.exists(file))

  z <- list()

  # file <- "~/Desktop/Testairline.spc"
  # file <- "/Users/christoph/tmp/iofile.spc"
  # file <- "/Users/christoph/tmp/urtest2.spc"

  txt <- readLines(file)
  txt <- gsub("\\\\", "/", txt)  # window file names to unix
  # keep everything lowercase, also works for filenames on mac and windows.
  # Untested on linux.
  txt <- tolower(txt)            
  txt <- gsub("#.*$", "", txt) # remove comments

  pp <- parse_spc(txt)

  ext_ser_call <- function(spc, vname){
    if (is.null(spc)) return(NULL)

    # analyze series spec
    if ("data" %in% names(spc)){
      st <- as.character(spc$start)
      stsp <- strsplit(st, "\\.")[[1]]
      stsp.y <- as.numeric(stsp[1])
      stsp.c <- as.numeric(stsp[2])  # TODO some tweaks to deal with jan, etc...
      
      f <- if (is.null(spc$period)) 12 else spc$period

      xstr <- paste0(vname, " <- ts(", 
                  paste(deparse(spc$data, control = "all"), collapse = ""), 
                  ", start = ", deparse(c(stsp.y, stsp.c)), ", frequency = ", f, ")")
   
    } else if ("file" %in% names(spc)){
      
      frm <- gsub('"', '', spc$format)

      if (frm == "tramo"){
      message("Data format '", frm, "' used by X-13 is currently not supported, mostly because of a lack of good testing examples. If you want to help out, you can do so by sending the following data file to christoph.sax@gmail.com.\n\n",
         gsub('"', '', spc$file), "\n\nalong with the following spc information:\n\n",
         paste(capture.output(print(spc)), collapse = "\n"),
         "\nThank you very much!"
        )
      return(NULL)
      }

      if (frm == "datevalue"){
        xstr <- paste0(vname, ' <- import.ts(', spc$file, ')')
      } else if (frm %in% c("datevaluecomma", "x13save")){
        xstr <- paste0(vname, ' <- import.ts(', spc$file, ', format = "', frm, '")')
      } else {
        browser()
        sspl <- strsplit(spc$start, "\\.")[[1]]
        s1 <- sspl[1]
        s2 <- sspl[2]

        if (s2 %in% tolower(month.abb)){
          s2 <- match(sspl[2], tolower(month.abb))
        }
        start <- as.numeric(c(s1, s2))
        frequency <- if (is.null(spc$period)) 12 else spc$period
        xstr <- paste0(vname, ' <- import.ts(', spc$file, ', format = "', frm, '", start = ', deparse(start) , ', frequency = ', frequency, ')')
      } 
    } else {
      return(NULL)
    }
    xstr
  }

  xstr <- ext_ser_call(pp$series, "x")
  xregstr <- ext_ser_call(pp$regression, "xreg")
  xtransstr <- ext_ser_call(pp$transform, "xtrans")

  # clean args that are produced by seas
  pp[c("series", "regression", "transform")] <- lapply(pp[c("series", "regression", "transform")], function(spc) spc[!names(spc) %in% c("file", "data", "start", "title", "format", "period", "user")])
  
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

  z$call <- as.call(c(quote(seas), ep))

  class(z) <- "import.spc"
  z

}

#' @export
#' @rdname import.spc
#' @method print import.spc
#' @param x    object of class \code{import.spc}
#' @param ... further arguments, not used
print.import.spc <- function(x, ...){

  inps <- x[!names(x) == "call"]
  if (length(inps) > 0){
    cat("# input series\n")
    lapply(x[!names(x) == "call"], print)
    cat("\n")
  }

  cat("# call\n")
  print(x$call)
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



#' Import Series from X-13 Data Files (experimental)
#' 
#' Helper function to read time series from X-13 input files. A call to
#' \code{import.ts} is constructed and included in the output of
#' \code{\link{import.spc}}.
#' \bold{This is an experimental function that may change substantially in the future.}
#' 
#' @param file character, name of the X-13 file which the data are to be read from
#' @param format a valid X-13 file format as described in 7.15, p. 173 of the
#'  X-13 manual.
#' @param start vector of length 2, time of the first observation (only for
#'   formats \code{"free"} and \code{"freecomma"})
#' @param frequency  the number of observations per unit of time (only for 
#'   formats \code{"free"} and \code{"freecomma"})
#' @export
#' @examples
#' tpath <- file.path(path.package("seasonal"), "tests")
#' 
#' import.ts(file.path(tpath, "datavalue1.dta"))
#' import.ts(file.path(tpath, "free1.txt"), format = "free", start = c(1949, 1), 
#'           frequency = 12)
#' import.ts(file.path(tpath, "free2.txt"), format = "free", start = c(1949, 1), 
#'           frequency = 12)
import.ts <- function(file, 
                    format = "datevalue", 
                    start = NULL, frequency = NULL){
  
  stopifnot(file.exists(file))

  if (format %in% c("x13save")){
    return(read_series(file))
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
    dta <- import_fortran(file, format)
    dta <- dta[,-c(1:2)]
  } else if (format %in% c("1r", "2r", "1l", "2l", "2l2", "cs", "cs2")) {


file <- (file.path(tpath, "x11_m1l.dat")); format <- "1l"; frequency = 12
file <- (file.path(tpath, "x11_m2l.dat")); format <- "2l"; frequency = 12
file <- (file.path(tpath, "x11_m2r.dat")); format <- "2r"; frequency = 12

file <- (file.path(tpath, "x11_m2l2.dat")); format <- "2l2"; frequency = 12   # start



    dta <- import_fortran(file, x11_to_fortran(format, frequency))
    dta <- dta[,-c(1:2)]
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
  p <- if (frequency == 4) "m" else "q"

  x11fort <- data.frame(m = c("(12f6.0,i2,a6)", 
                            "(6f12.0,/,6f12.0,i2,a6)", 
                            "(a6,i2,12f6.0)", 
                            "(a6,i2,6f12.0,/,8x,6f12.0)" , 
                            "(a8,i4,6f11.0,2x,/,12x,6f11.0,2x)", 
                            "(a8,i2,10x,12e16.10,18x) ", 
                            "(a8,i4,12x,12e16.10,14x)"),
                      q = c("(4(12x,f6.0),i2,a6)", 
                            "(4f12.0,24x,i2,a6)", 
                            "(a6,i2,4(12x,f6.0))", 
                            "(a6,i2,4f12.0)", 
                            "(a8,i4,4f11.0,2x)", 
                            "(a8,i2,10x,12e16.10,18x)", 
                            "(a8,i4,12x,12e16.10,14x)"),
                      stringsAsFactors = FALSE)

  rownames(x11fort) = c("1r", "2r", "1l", "2l", "2l2", "cs", "cs2")
  x11fort[x, p]
}



# format <- x11_to_fortran(format, frequency)


import_fortran <- function(file, format){
  parse_fortran_format <- function(x){
    if (grepl("/", x)){
      return(lapply(strsplit(x, "/")[[1]], parse_fortran_format))
    }
    z <- strsplit(gsub("[\\(\\)]", "", x), ",")[[1]]
    # z[z != ""]
    z
  }
  z <- read.fortran(file, parse_fortran_format(format))
  z <- z[, -c(1, 2)]
  c(t(as.matrix(z)))
}





