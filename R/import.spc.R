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
#' # a spc with multiple user defined regression and transformation series
#' tdir <- tempdir()
#' seas(x = AirPassengers, xreg = cbind(a = genhol(cny, start = 1, end = 4,
#'     center = "calendar"), b = genhol(cny, start = -3, end = 0,
#'     center = "calendar")), xtrans = cbind(sqrt(AirPassengers), AirPassengers^3), 
#'     transform.function = "log", transform.type = "temporary", 
#'     regression.aictest = "td", regression.usertype = "holiday", dir = tdir, 
#'     out = TRUE)
#' import.spc(file.path(tdir, "iofile.spc"))
#' 
import.spc <- function(file){
  
  stopifnot(file.exists(file))

  z <- list()

  # file <- "~/Desktop/Testairline.spc"
  # file <- "/Users/christoph/tmp/iofile.spc"

  txt <- readLines(file)
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

      if (frm == "datevalue"){
        xstr <- paste0(vname, ' <- import.ts(', spc$file, ')')
      } else if (frm %in% c("datevaluecomma", "x13save")){
        xstr <- paste0(vname, ' <- import.ts(', spc$file, ', format = ', frm, ')')
      } else if (frm %in% c("free", "freecomma")){
        start <- spc$start
        frequency <- if (is.null(spc$period)) 12 else spc$period
        xstr <- paste0(vname, ' <- import.ts(', spc$file, ', format = ', frm, ' start = ', start , ' frequency = ', frequency, ')')
      } else {
      message("Data format '", frm, "' used by X-13 is currently not supported, mostly because of a lack of good testing examples. If you want to help out, you can do so by sending the following data file to christoph.sax@gmail.com.\n\n",
         gsub('"', '', spc$file), "\n\nalong with the following spc information:\n\n",
         paste(capture.output(print(spc)), collapse = "\n"),
         "\nThank you very much!"
        )
      return(NULL)
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
print.import.spc <- function(x){

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
#' @export
#' @examples
#' tpath <- file.path(path.package("seasonal"), "tests")
#' 
#' import.ts(file.path(tpath, "datavalue.dta"))
#' import.ts(file.path(tpath, "free.dta"), format = "free", start = c(1949, 1), frequency = 12)
#' import.ts(file.path(tpath, "x13save.dta"), format = "x13save")
#' import.ts(file.path(tpath, "datavaluecomma.dta"), format = "datevaluecomma")
#' import.ts(file.path(tpath, "freecomma.dta"), format = "freecomma", start = c(1949, 1), frequency = 12)
#' import.ts(file.path(tpath, "datavalue_q.dta"))
#' import.ts(file.path(tpath, "x13save_q.dta"), format = "x13save")
#' import.ts(file.path(tpath, "datavalue_mult.dta"))
import.ts <- function(file, 
                    format = c("datevalue", "datevaluecomma", "free", "freecomma", "x13save"), 
                    start = NULL, frequency = NULL){
  
  format <- match.arg(format)
  stopifnot(file.exists(file))

  if (format %in% c("datevalue", "datevaluecomma")){
    dec <- if (format == "datevaluecomma") "," else "."
    dta <- read.table(file, sep = " ", dec = dec)
    ser <- dta[,-c(1:2)]
    ser <- unname(ser)
    frequency <- length(unique(dta[, 2]))
    start <- c(as.matrix(dta[1, 1:2]))
    z <- ts(ser, frequency = frequency, start = start)

  } 
  if (format %in% c("free", "freecomma")){
    dec <- if (format == "freecomma") "," else "."
    dta <- read.table(file, sep = " ", dec = dec)
    ser <- as.numeric(t(dta))
    frequency <- frequency
    start <- start

    z <- ts(ser, frequency = frequency, start = start)

  } 

  if (format %in% c("x13save")){
    z <- read_series(file)
  } 
  z
  

}



