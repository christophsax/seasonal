#' read time series from X-13 input files
#' 
#' helper function to read time series from X-13 input files
#' 
#' @param x dsfdf
#' @export
#' @examples
#' import.spc("~/Desktop/Testairline.spc")
#' import.spc("/Users/christoph/tmp/iofile.spc")
import.spc <- function(file){


  z <- list()

  # file <- "~/Desktop/Testairline.spc"
  # file <- "/Users/christoph/tmp/iofile.spc"


  # TODO some file checks...

# readLines("/Users/christoph/tmp/data.dta")


  txt <- readLines(file)
  pp <- parse_spc(txt)


  # analyze series spec
  if ("data" %in% names(pp$series)){
    st <- as.character(pp$series$start)
    stsp <- strsplit(st, "\\.")[[1]]
    stsp.y <- as.numeric(stsp[1])
    stsp.c <- as.numeric(stsp[2])  # TODO some tweaks to deal with jan, etc...
    
    f <- 12
    xstr <- paste0("x = ts(", 
                paste(deparse(pp$series$data, control = "all"), collapse = ""), 
                ", start = ", deparse(c(stsp.y, stsp.c)), ", frequency = ", f, ")")
 
  } else if ("file" %in% names(pp$series)){
    
    frm <- gsub('"', '', pp$series$format)

    if (frm == "datevalue"){
      xstr <- paste0('x = read.ts("', file, '")')
    }
  }


  # rem series spec
  pp$series$data <- NULL
  pp$series$start <- NULL
  pp$series$title <- NULL


  # construct the main call
  ep <- expand_spclist_to_args(pp)

  ep <- rem_defaults_from_args(ep)

  # add x as series
  ep <- c(list(x = quote(x)), ep)
  

  z$x <- parse(text = xstr)[[1]]
  z$call <- as.call(c(quote(seas), ep))

  z

}


expand_spclist_to_args <- function(ll){
  # substitute empty names lists by ""
  ll[sapply(ll, identical, structure(list(), .Names = character(0)))] <- ""
  do.call("c", ll)
}



rem_defaults_from_args <- function(x) {
  z <- x

  # set these non-present specs to NULL
  if (!"automdl" %in% names(z)) {z['automdl'] <- list(NULL)}
  if (!"outlier" %in% names(z)) {z['outlier'] <- list(NULL)}


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

  # make sure not to loose x11 with default save
  if (any(grepl("^x11", names(x))) & !any(grepl("^x11", names(z)))){
    z$x11 <- ""
  } 

  z
}

#' read time series from X-13 input files
#' 
#' helper function to read time series from X-13 input files
#' 
#' @param x dsfdf
#' @export
#' @examples
#' read.ts("/Users/christoph/tmp/data.dta")
read.ts <- function(file, format = "datevalue", start = NULL, frequency = NULL){

  if (format == "datevalue"){
    dta <- read.table("/Users/christoph/tmp/data.dta", sep = " ")
    ser <- dta[,3]
    frequency <- length(unique(dta[, 2]))
    start <- c(as.matrix(dta[1, 1:2]))
  }

  ts(ser, frequency = frequency, start = start)
}



