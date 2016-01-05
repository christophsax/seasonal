#' Import X-13ARIMA-SEATS Output Tables
#' 
#' With the exception the composite spec, the \code{series} function imports all
#' tables that can be saved in X-13ARIMA-SEATS.
#' 
#' If the save argument is not specified in the model call, \code{series} 
#' re-evaluates the call with the corresponding specs enabled (also returning a 
#' message). Note that re-evaluation doubles the overall computational time. If 
#' you want to accelerate the procedure, you have to be explicit about the 
#' output in the model call (see examples).
#' 
#' List of all importable tables from X-13ARIMA-SEATS:
#' 
#' \tabular{lll}{
#' \bold{spec} \tab \bold{long name} \tab \bold{short name} \cr 
#' check \tab check.acf \tab acf \cr 
#' check \tab check.acfsquared \tab ac2 \cr 
#' check \tab check.pacf \tab pcf \cr 
#' estimate \tab estimate.armacmatrix \tab acm \cr 
#' estimate \tab estimate.iterations \tab itr \cr 
#' estimate \tab estimate.regcmatrix \tab rcm \cr 
#' estimate \tab estimate.regressioneffects \tab ref \cr 
#' estimate \tab estimate.residuals \tab rsd \cr 
#' estimate \tab estimate.roots \tab rts \cr 
#' force \tab force.forcefactor \tab ffc \cr 
#' force \tab force.revsachanges \tab e6a \cr 
#' force \tab force.rndsachanges \tab e6r \cr 
#' force \tab force.saround \tab rnd \cr 
#' force \tab force.seasadjtot \tab saa \cr 
#' forecast \tab forecast.backcasts \tab bct \cr 
#' forecast \tab forecast.forecasts \tab fct \cr 
#' forecast \tab forecast.transformed \tab ftr \cr 
#' forecast \tab forecast.transformedbcst \tab btr \cr 
#' forecast \tab forecast.variances \tab fvr \cr 
#' history \tab history.chngestimates \tab che \cr 
#' history \tab history.chngrevisions \tab chr \cr 
#' history \tab history.fcsterrors \tab fce \cr 
#' history \tab history.fcsthistory \tab fch \cr 
#' history \tab history.indsaestimates \tab iae \cr 
#' history \tab history.indsarevisions \tab iar \cr 
#' history \tab history.lkhdhistory \tab lkh \cr 
#' history \tab history.outlierhistory \tab rot \cr 
#' history \tab history.saestimates \tab sae \cr 
#' history \tab history.sarevisions \tab sar \cr 
#' history \tab history.seatsmdlhistory \tab smh \cr 
#' history \tab history.sfestimates \tab sfe \cr 
#' history \tab history.sfilterhistory \tab sfh \cr 
#' history \tab history.sfrevisions \tab sfr \cr 
#' history \tab history.trendchngestimates \tab tce \cr 
#' history \tab history.trendchngrevisions \tab tcr \cr 
#' history \tab history.trendestimates \tab tre \cr 
#' history \tab history.trendrevisions \tab trr \cr 
#' identify \tab identify.acf \tab iac \cr 
#' identify \tab identify.pacf \tab ipc \cr 
#' outlier \tab outlier.finaltests \tab fts \cr 
#' outlier \tab outlier.iterations \tab oit \cr 
#' regression \tab regression.aoutlier \tab ao \cr 
#' regression \tab regression.holiday \tab hol \cr 
#' regression \tab regression.levelshift \tab ls \cr 
#' regression \tab regression.outlier \tab otl \cr 
#' regression \tab regression.regressionmatrix \tab rmx \cr 
#' regression \tab regression.regseasonal \tab a10 \cr 
#' regression \tab regression.seasonaloutlier \tab so \cr 
#' regression \tab regression.temporarychange \tab tc \cr 
#' regression \tab regression.tradingday \tab td \cr 
#' regression \tab regression.transitory \tab a13 \cr 
#' regression \tab regression.userdef \tab usr \cr 
#' seats \tab seats.adjustfac \tab s16 \cr 
#' seats \tab seats.adjustmentratio \tab s18 \cr 
#' seats \tab seats.cycle \tab cyc \cr 
#' seats \tab seats.diffseasonaladj \tab dsa \cr 
#' seats \tab seats.difftrend \tab dtr \cr 
#' seats \tab seats.irregular \tab s13 \cr 
#' seats \tab seats.longtermtrend \tab ltt \cr 
#' seats \tab seats.seasadjconst \tab sec \cr 
#' seats \tab seats.seasonal \tab s10 \cr 
#' seats \tab seats.seasonaladj \tab s11 \cr 
#' seats \tab seats.seasonaladjfcstdecomp \tab afd \cr 
#' seats \tab seats.seasonalfcstdecomp \tab sfd \cr 
#' seats \tab seats.seasonalsum \tab ssm \cr 
#' seats \tab seats.seriesfcstdecomp \tab ofd \cr 
#' seats \tab seats.totaladjustment \tab sta \cr 
#' seats \tab seats.transitory \tab s14 \cr 
#' seats \tab seats.transitoryfcstdecomp \tab yfd \cr 
#' seats \tab seats.trend \tab s12 \cr 
#' seats \tab seats.trendconst \tab stc \cr 
#' seats \tab seats.trendfcstdecomp \tab tfd \cr 
#' series \tab series.adjoriginal \tab b1 \cr 
#' series \tab series.calendaradjorig \tab a18 \cr 
#' series \tab series.outlieradjorig \tab a19 \cr 
#' series \tab series.seriesmvadj \tab mv \cr 
#' series \tab series.span \tab a1 \cr 
#' slidingspans \tab slidingspans.chngspans \tab chs \cr 
#' slidingspans \tab slidingspans.indchngspans \tab cis \cr 
#' slidingspans \tab slidingspans.indsaspans \tab ais \cr 
#' slidingspans \tab slidingspans.indsfspans \tab sis \cr 
#' slidingspans \tab slidingspans.indychngspans \tab yis \cr 
#' slidingspans \tab slidingspans.sfspans \tab sfs \cr 
#' slidingspans \tab slidingspans.tdspans \tab tds \cr 
#' slidingspans \tab slidingspans.ychngspans \tab ycs \cr 
#' spectrum \tab spectrum.speccomposite \tab is0 \cr 
#' spectrum \tab spectrum.specindirr \tab is2 \cr 
#' spectrum \tab spectrum.specindsa \tab is1 \cr 
#' spectrum \tab spectrum.specirr \tab sp2 \cr 
#' spectrum \tab spectrum.specorig \tab sp0 \cr 
#' spectrum \tab spectrum.specresidual \tab spr \cr 
#' spectrum \tab spectrum.specsa \tab sp1 \cr 
#' spectrum \tab spectrum.specseatsextresiduals \tab ser \cr 
#' spectrum \tab spectrum.specseatsirr \tab s2s \cr 
#' spectrum \tab spectrum.specseatssa \tab s1s \cr 
#' transform \tab transform.permprior \tab a2p \cr 
#' transform \tab transform.permprioradjusted \tab a3p \cr 
#' transform \tab transform.permprioradjustedptd \tab a4p \cr 
#' transform \tab transform.prior \tab a2 \cr 
#' transform \tab transform.prioradjusted \tab a3 \cr 
#' transform \tab transform.prioradjustedptd \tab a4d \cr 
#' transform \tab transform.seriesconstant \tab a1c \cr 
#' transform \tab transform.tempprior \tab a2t \cr 
#' transform \tab transform.transformed \tab trn \cr 
#' x11 \tab x11.adjoriginalc \tab c1 \cr 
#' x11 \tab x11.adjoriginald \tab d1 \cr 
#' x11 \tab x11.adjustdiff \tab fad \cr 
#' x11 \tab x11.adjustfac \tab d16 \cr 
#' x11 \tab x11.adjustmentratio \tab e18 \cr 
#' x11 \tab x11.biasfactor \tab bcf \cr 
#' x11 \tab x11.calendar \tab d18 \cr 
#' x11 \tab x11.calendaradjchanges \tab e8 \cr 
#' x11 \tab x11.combholiday \tab chl \cr 
#' x11 \tab x11.extreme \tab c20 \cr 
#' x11 \tab x11.extremeb \tab b20 \cr 
#' x11 \tab x11.irregular \tab d13 \cr 
#' x11 \tab x11.irregularadjao \tab iao \cr 
#' x11 \tab x11.irregularb \tab b13 \cr 
#' x11 \tab x11.irregularc \tab c13 \cr 
#' x11 \tab x11.irrwt \tab c17 \cr 
#' x11 \tab x11.irrwtb \tab b17 \cr 
#' x11 \tab x11.mcdmovavg \tab f1 \cr 
#' x11 \tab x11.modirregular \tab e3 \cr 
#' x11 \tab x11.modoriginal \tab e1 \cr 
#' x11 \tab x11.modseasadj \tab e2 \cr 
#' x11 \tab x11.modsic4 \tab c4 \cr 
#' x11 \tab x11.modsid4 \tab d4 \cr 
#' x11 \tab x11.origchanges \tab e5 \cr 
#' x11 \tab x11.replacsi \tab d9 \cr 
#' x11 \tab x11.replacsic9 \tab c9 \cr 
#' x11 \tab x11.robustsa \tab e11 \cr 
#' x11 \tab x11.sachanges \tab e6 \cr 
#' x11 \tab x11.seasadj \tab d11 \cr 
#' x11 \tab x11.seasadjb11 \tab b11 \cr 
#' x11 \tab x11.seasadjb6 \tab b6 \cr 
#' x11 \tab x11.seasadjc11 \tab c11 \cr 
#' x11 \tab x11.seasadjc6 \tab c6 \cr 
#' x11 \tab x11.seasadjconst \tab sac \cr 
#' x11 \tab x11.seasadjd6 \tab d6 \cr 
#' x11 \tab x11.seasonal \tab d10 \cr 
#' x11 \tab x11.seasonaladjregsea \tab ars \cr 
#' x11 \tab x11.seasonalb10 \tab b10 \cr 
#' x11 \tab x11.seasonalb5 \tab b5 \cr 
#' x11 \tab x11.seasonalc10 \tab c10 \cr 
#' x11 \tab x11.seasonalc5 \tab c5 \cr 
#' x11 \tab x11.seasonald5 \tab d5 \cr 
#' x11 \tab x11.seasonaldi\_ \tab fsd \cr 
#' x11 \tab x11.sib3 \tab b3 \cr 
#' x11 \tab x11.sib8 \tab b8 \cr 
#' x11 \tab x11.tdadjorig \tab c19 \cr 
#' x11 \tab x11.tdadjorigb \tab b19 \cr 
#' x11 \tab x11.totaladjustment \tab tad \cr 
#' x11 \tab x11.trend \tab d12 \cr 
#' x11 \tab x11.trendadjls \tab tal \cr 
#' x11 \tab x11.trendb2 \tab b2 \cr 
#' x11 \tab x11.trendb7 \tab b7 \cr 
#' x11 \tab x11.trendc2 \tab c2 \cr 
#' x11 \tab x11.trendc7 \tab c7 \cr 
#' x11 \tab x11.trendchanges \tab e7 \cr 
#' x11 \tab x11.trendconst \tab tac \cr 
#' x11 \tab x11.trendd2 \tab d2 \cr 
#' x11 \tab x11.trendd7 \tab d7 \cr 
#' x11 \tab x11.unmodsi \tab d8 \cr 
#' x11 \tab x11.unmodsiox \tab d8b \cr 
#' x11 \tab x11.yrtotals \tab e4 \cr 
#' x11regression \tab x11regression.calendar \tab xca \cr 
#' x11regression \tab x11regression.calendarb \tab bxc \cr 
#' x11regression \tab x11regression.combcalendar \tab xcc \cr 
#' x11regression \tab x11regression.combcalendarb \tab bcc \cr 
#' x11regression \tab x11regression.combtradingday \tab c18 \cr 
#' x11regression \tab x11regression.combtradingdayb \tab b18 \cr 
#' x11regression \tab x11regression.extremeval \tab c14 \cr 
#' x11regression \tab x11regression.extremevalb \tab b14 \cr 
#' x11regression \tab x11regression.holiday \tab xhl \cr 
#' x11regression \tab x11regression.holidayb \tab bxh \cr 
#' x11regression \tab x11regression.outlieriter \tab xoi \cr 
#' x11regression \tab x11regression.priortd \tab a4 \cr 
#' x11regression \tab x11regression.tradingday \tab c16 \cr 
#' x11regression \tab x11regression.tradingdayb \tab b16 \cr 
#' x11regression \tab x11regression.x11reg \tab c15 \cr 
#' x11regression \tab x11regression.x11regb \tab b15 \cr 
#' x11regression \tab x11regression.xregressioncmatrix \tab xrc \cr 
#' x11regression \tab x11regression.xregressionmatrix \tab xrm \cr 
#' }
#' 
#' 
#' @param x  an object of class \code{"seas"}.
#' @param series  character vector, short or long names of an X-13ARIMA-SEATS 
#'   table. If a long name is specified, it needs to be combined with the spec 
#'   name and separated by a dot (it is not unique, otherwise. See list below.). More than one 
#'   series can be specified (see examples).
#' @param reeval logical, if \code{TRUE}, the model is re-evaluated with the 
#'   corresponding specs enabled.
#' @param verbose logical, if \code{TRUE}, a message is returned if a spec is added
#'   during reevaluation.
#'   
#' @return depending on the table, either an object of class \code{"ts"} or
#'   \code{"data.frame"}.
#'    
#' @seealso \code{\link{seas}} for the main function.
#'   
#' @references Vignette with a more detailed description: 
#'   \url{http://www.seasonal.website/seasonal.html}
#'   
#'   Comprehensive list of R examples from the X-13ARIMA-SEATS manual: 
#'   \url{http://www.seasonal.website/examples.html}
#'   
#'   Official X-13ARIMA-SEATS manual: 
#'   \url{http://www.census.gov/ts/x13as/docX13AS.pdf}
#'   
#' @export
#' 
#' 
#' @examples
#' 
#' \dontrun{
#' 
#' m <- seas(AirPassengers)
#' series(m, "fct")  # re-evaluate with the forecast spec activated 
#' 
#' # more than one series
#' series(m, c("rsd", "fct"))
#' 
#' m <- seas(AirPassengers, forecast.save = "fct")
#' series(m, "fct") # no re-evaluation (much faster!)
#' 
#' # using long names
#' series(m, "forecast.forecasts")
#' 
#' # history spec
#' series(m, "history.trendestimates") 
#' series(m, "history.sfestimates") 
#' series(m, "history.saestimates") 
#' series(m, c("history.sfestimates", "history.trendestimates")) 
#' 
#' # slidingspans spec
#' series(m, "slidingspans.sfspans") 
#' series(m, "slidingspans.tdspans") 
#' 
#' # fundamental identities of seasonal adjustment 
#' # Y = T * I * (S * TD)
#' all.equal(AirPassengers, series(m, "seats.trend") * 
#'          series(m, "seats.irregular") * series(m, "seats.adjustfac"))
#' # Y_sa = Y / (S * TD)
#' all.equal(final(m), AirPassengers / series(m, "seats.adjustfac"))
#' 
#' ### Some X-13ARIMA-SEATS functions can be replicated in R:
#' 
#' # X-13ARIMA-SEATS spectrum
#' plot(series(m, "spectrum.specorig")[,-1], t = "l")
#' # R equivalent: spectrum from stats
#' spectrum(diff(log(AirPassengers)), method = "ar")
#' 
#' # X-13ARIMA-SEATS pacf
#' x13.pacf <- series(m, "identify.pacf")
#' plot(x13.pacf[,1:2])
#' lines(x13.pacf[,3])
#' lines(-x13.pacf[,3])
#' # R equivalent: pacf from stats
#' pacf(AirPassengers, lag.max = 35)
#' }
series <- function(x, series, reeval = TRUE, verbose = TRUE){
  stopifnot(inherits(x, "seas"))
  
  SPECS <- NULL 
  data(specs, envir = environment(), package = "seasonal")  # avoid side effects
  
  is.dotted <- grepl("\\.", series)
  
  # check validiy of short or long names
  is.valid <- logical(length = length(series))
  is.valid[is.dotted] <- series[is.dotted] %in% SPECS$long[SPECS$is.series]
  is.valid[!is.dotted] <- series[!is.dotted] %in% SPECS$short[SPECS$is.series]

  if (any(!is.valid)){
    stop(paste0("\nseries not valid: ", paste(series[!is.valid], collapse = ", "), "\nsee ?series for a list of importable series "))
  }
  
  # unique short names
  series.short <- unique(c(series[!is.dotted], 
    merge(data.frame(long = series[is.dotted]), SPECS)$short))

  # reeval with non present output
  if (reeval){
    # check which series are already there
    if (is.null(x$series)){
      series.NA <- series.short
    } else {
      series.NA <- setdiff(series.short, names(x$series))
    }
    activated <- NULL
    reeval.dots <- list()
    j <- 1  # flexible index to allow for an arbitrary number of requirements
    for (i in seq_along(series.NA)){
      series.NA.i <- series.NA[i]
      spec.i <- as.character(SPECS[SPECS$short == series.NA.i & SPECS$is.series, ]$spec)
      if (length(spec.i) > 1) stop("not unique.")
      if (!spec.i %in% names(x$spc)){
        if (spec.i %in% c("x11", "seats")){
          stop(spec.i, " is not activated. You should change the adjustment method.")
        } else {
          activated <- c(activated, spec.i)
        }
      }
      requires.i <- as.character(SPECS[SPECS$short == series.NA.i & SPECS$is.series, ]$requires)
      if (length(requires.i) > 0){
        requires.list <- eval(parse(text = paste("list(", requires.i, ")")))
        reeval.dots <- c(reeval.dots, requires.list)
        j <- length(reeval.dots) + 1
      }
      
      reeval.dots[[j]] <- series.NA.i
      names(reeval.dots)[j] <- paste0(spec.i, '.save')
      j <- j + 1
    }

    if (verbose & length(activated) > 0){
      message(paste("specs have been added to the model:", 
                    paste(unique(activated), collapse = ", ")))
    }
    
    if (length(reeval.dots) > 0){
      x <- reeval(x, reeval.dots, out = FALSE)
    }
  }

  z <- do.call(cbind, x$series[series.short])
  z
}


