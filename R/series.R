#' Import X-13ARIMA-SEATS Output Tables
#'
#' The `series` function imports all tables that can be saved in
#' X-13ARIMA-SEATS.
#'
#' If the save argument is not specified in the model call, `series`
#' re-evaluates the call with the corresponding specs enabled (also returning a
#' message). Note that re-evaluation doubles the overall computational time. If
#' you want to accelerate the procedure, you have to be explicit about the
#' output in the model call (see examples).
#'
#' List of all importable tables from X-13ARIMA-SEATS:
#'
#' \tabular{llll}{
#' **spec** \tab **long name** \tab **short name** \tab **description** \cr
#' check \tab check.acf \tab acf \tab autocorrelation function of residuals with standard errors and Ljung-Box Q-statistics computed through each lag \cr
#' check \tab check.acfsquared \tab ac2 \tab autocorrelation function of squared residuals with standard errors and Ljung-Box Q-statistics computed through each lag \cr
#' check \tab check.pacf \tab pcf \tab partial autocorrelation function of residuals with standard errors \cr
#' composite \tab composite.adjcompositesrs \tab b1 \tab aggregated time series data, prior adjusted, with associated dates \cr
#' composite \tab composite.calendaradjcomposite \tab cac \tab aggregated time series data, adjusted for regARIMA calendar effects. \cr
#' composite \tab composite.compositesrs \tab cms \tab aggregated time series data, with associated dates \cr
#' composite \tab composite.indadjsatot \tab iaa \tab final indirect seasonally adjusted series, with yearly totals adjusted to match the original series \cr
#' composite \tab composite.indadjustfac \tab iaf \tab final combined adjustment factors for the indirect seasonal adjustment \cr
#' composite \tab composite.indaoutlier \tab iao \tab final indirect AO outliers \cr
#' composite \tab composite.indcalendar \tab ica \tab final calendar factors for the indirect seasonal adjustment \cr
#' composite \tab composite.indirregular \tab iir \tab final irregular component for the indirect adjustment \cr
#' composite \tab composite.indlevelshift \tab ils \tab final indirect LS outliers \cr
#' composite \tab composite.indmcdmovavg \tab if1 \tab MCD moving average of the final indirect seasonally adjusted series \cr
#' composite \tab composite.indmodirr \tab ie3 \tab irregular component modified for extreme values from the indirect seasonal adjustment \cr
#' composite \tab composite.indmodoriginal \tab ie1 \tab original series modified for extreme values from the indirect seasonal adjustment \cr
#' composite \tab composite.indmodsadj \tab ie2 \tab seasonally adjusted series modified for extreme values from the indirect seasonal adjustment \cr
#' composite \tab composite.indreplacsi \tab id9 \tab final replacement values for extreme SI-ratios (differences) for the indirect adjustment \cr
#' composite \tab composite.indrevsachanges \tab i6a \tab percent changes for indirect seasonally adjusted series with revised yearly totals \cr
#' composite \tab composite.indrndsachanges \tab i6r \tab percent changes (differences) in the rounded indirect seasonally adjusted series \cr
#' composite \tab composite.indrobustsa \tab iee \tab final indirect seasonally adjusted series modified for extreme values \cr
#' composite \tab composite.indsachanges \tab ie6 \tab percent changes (differences) in the indirect seasonally adjusted series \cr
#' composite \tab composite.indsadjround \tab irn \tab rounded indirect seasonally adjusted series \cr
#' composite \tab composite.indseasadj \tab isa \tab final indirect seasonally adjusted series \cr
#' composite \tab composite.indseasonal \tab isf \tab final seasonal factors for the indirect seasonal adjustment \cr
#' composite \tab composite.indseasonaldiff \tab isd \tab final seasonal difference for the indirect seasonal adjustment (only for pseudo-additive seasonal adjustment) \cr
#' composite \tab composite.indtotaladjustment \tab ita \tab total indirect adjustment factors (only produced if the original series contains values that are <= 0) \cr
#' composite \tab composite.indtrend \tab itn \tab final trend-cycle for the indirect adjustment \cr
#' composite \tab composite.indtrendchanges \tab ie7 \tab percent changes (differences) in the indirect final trend component \cr
#' composite \tab composite.indunmodsi \tab id8 \tab final unmodified SI-ratios (differences) for the indirect adjustment \cr
#' composite \tab composite.origchanges \tab ie5 \tab percent changes (differences) in the original series \cr
#' composite \tab composite.outlieradjcomposite \tab oac \tab aggregated time series data, adjusted for outliers. \cr
#' composite \tab composite.prioradjcomposite \tab ia3 \tab composite series adjusted for user-defined prior adjustments applied at the component level \cr
#' estimate \tab estimate.armacmatrix \tab acm \tab correlation matrix of ARMA parameter estimates if used with the print argument; covariance matrix of same if used with the save argument \cr
#' estimate \tab estimate.iterations \tab itr \tab detailed output for estimation iterations, including log-likelihood values and parameters, and counts of function evaluations and iterations \cr
#' estimate \tab estimate.regcmatrix \tab rcm \tab correlation matrix of regression parameter estimates if used with the print argument; covariance matrix of same if used with the save argument \cr
#' estimate \tab estimate.regressioneffects \tab ref \tab Xb matrix of regression variables multiplied by the vector of estimated regression coefficients \cr
#' estimate \tab estimate.residuals \tab rsd \tab model residuals with associated dates or observation numbers \cr
#' estimate \tab estimate.roots \tab rts \tab roots of the autoregressive and moving average operators in the estimated model \cr
#' force \tab force.forcefactor \tab ffc \tab factors applied to get seasonally adjusted series with constrained yearly totals (if type = regress or type = denton) \cr
#' force \tab force.revsachanges \tab e6a \tab percent changes (differences) in seasonally adjusted series with revised yearly totals \cr
#' force \tab force.revsachangespct \tab p6a \tab percent changes in seasonally adjusted series with forced yearly totals \cr
#' force \tab force.rndsachanges \tab e6r \tab percent changes (differences) in rounded seasonally adjusted series \cr
#' force \tab force.rndsachangespct \tab p6r \tab percent changes in rounded seasonally adjusted series \cr
#' force \tab force.saround \tab rnd \tab rounded final seasonally adjusted series (if round = yes) or the rounded final seasonally adjusted series with constrained yearly totals (if type = regress or type = denton) \cr
#' force \tab force.seasadjtot \tab saa \tab final seasonally adjusted series with constrained yearly totals (if type = regress or type = denton) \cr
#' forecast \tab forecast.backcasts \tab bct \tab point backcasts on the original scale, along with upper and lower prediction interval limits \cr
#' forecast \tab forecast.forecasts \tab fct \tab point forecasts on the original scale, along with upper and lower prediction interval limits \cr
#' forecast \tab forecast.transformed \tab ftr \tab forecasts on the transformed scale, with corresponding forecast standard errors \cr
#' forecast \tab forecast.transformedbcst \tab btr \tab backcasts on the transformed scale, with corresponding forecast standard errors \cr
#' forecast \tab forecast.variances \tab fvr \tab forecast error variances on the transformed scale, showing the contributions of the error assuming the model is completely known (stochastic variance) and the error due to estimating any regression parameters (error in estimating AR and MA parameters is ignored) \cr
#' history \tab history.armahistory \tab amh \tab history of estimated AR and MA coefficients from the regARIMA model \cr
#' history \tab history.chngestimates \tab che \tab concurrent and most recent estimate of the month-tomonth (or quarter-to-quarter) changes in the seasonally adjusted data \cr
#' history \tab history.chngrevisions \tab chr \tab revision from concurrent to most recent estimate of the month-to-month (or quarter-to-quarter) changes in the seasonally adjusted data \cr
#' history \tab history.fcsterrors \tab fce \tab revision history of the accumulated sum of squared forecast errors \cr
#' history \tab history.fcsthistory \tab fch \tab listing of the forecast and forecast errors used to generate accumulated sum of squared forecast errors \cr
#' history \tab history.indsaestimates \tab iae \tab concurrent and most recent estimate of the indirect seasonally adjusted data \cr
#' history \tab history.indsarevisions \tab iar \tab revision from concurrent to most recent estimate of the indirect seasonally adjusted series \cr
#' history \tab history.lkhdhistory \tab lkh \tab history of AICC and likelihood values \cr
#' history \tab history.outlierhistory \tab rot \tab record of outliers removed and kept for the revisions history (printed only if automatic outlier identification is used) \cr
#' history \tab history.saestimates \tab sae \tab concurrent and most recent estimate of the seasonally adjusted data \cr
#' history \tab history.sarevisions \tab sar \tab revision from concurrent to most recent estimate of the seasonally adjusted data \cr
#' history \tab history.seatsmdlhistory \tab smh \tab SEATS ARIMA model history \cr
#' history \tab history.sfestimates \tab sfe \tab concurrent and most recent estimate of the seasonal factors and projected seasonal factors \cr
#' history \tab history.sfilterhistory \tab sfh \tab record of seasonal filter selection for each observation in the revisions history (printed only if automatic seasonal filter selection is used) \cr
#' history \tab history.sfrevisions \tab sfr \tab revision from concurrent to most recent estimate of the seasonal factor, as well as projected seasonal factors \cr
#' history \tab history.tdhistory \tab tdh \tab history of estimated trading day regression coefficients from the regARIMA model \cr
#' history \tab history.trendchngestimates \tab tce \tab concurrent and most recent estimate of the month-tomonth (or quarter-to-quarter) changes in the trend component \cr
#' history \tab history.trendchngrevisions \tab tcr \tab revision from concurrent to most recent estimate of the month-to-month (or quarter-to-quarter) changes in the trend component \cr
#' history \tab history.trendestimates \tab tre \tab concurrent and most recent estimate of the trend component \cr
#' history \tab history.trendrevisions \tab trr \tab revision from concurrent to most recent estimate of the trend component \cr
#' identify \tab identify.acf \tab iac \tab sample autocorrelation function(s), with standard errors and Ljung-Box Q-statistics for each lag \cr
#' identify \tab identify.pacf \tab ipc \tab sample partial autocorrelation function(s) with standard errors for each lag \cr
#' outlier \tab outlier.finaltests \tab fts \tab t-statistics for every time point and outlier type generated during the final outlier detection iteration (not saved when automdl/pickmdl is used) \cr
#' outlier \tab outlier.iterations \tab oit \tab detailed results for each iteration of outlier detection including outliers detected, outliers deleted, model parameter estimates, and robust and nonrobust estimates of the residual standard deviation \cr
#' regression \tab regression.aoutlier \tab ao \tab regARIMA additive (or point) outlier factors (table A8.AO) \cr
#' regression \tab regression.holiday \tab hol \tab regARIMA holiday factors (table A7) \cr
#' regression \tab regression.levelshift \tab ls \tab regARIMA level shift, temporary level shift and ramp outlier factors (table A8.LS) \cr
#' regression \tab regression.outlier \tab otl \tab combined regARIMA outlier factors (table A8) \cr
#' regression \tab regression.regressionmatrix \tab rmx \tab values of regression variables with associated dates \cr
#' regression \tab regression.regseasonal \tab a10 \tab regARIMA user-defined seasonal factors (table A10) \cr
#' regression \tab regression.seasonaloutlier \tab so \tab regARIMA seasonal outlier factors (table A8.SO) \cr
#' regression \tab regression.temporarychange \tab tc \tab regARIMA temporary change outlier factors (table A8.TC) \cr
#' regression \tab regression.tradingday \tab td \tab regARIMA trading day factors (table A6) \cr
#' regression \tab regression.transitory \tab a13 \tab regARIMA transitory component factors from userdefined regressors (table A13) \cr
#' regression \tab regression.userdef \tab usr \tab factors from user-defined regression variables (table A9) \cr
#' seats \tab seats.adjustfac \tab s16 \tab final SEATS combined adjustment factors \cr
#' seats \tab seats.adjustfacpct \tab psa \tab combined adjustment factors, expressed as percentages if appropriate \cr
#' seats \tab seats.adjustmentratio \tab s18 \tab final SEATS adjustment ratio \cr
#' seats \tab seats.componentmodels \tab mdc \tab models for the components \cr
#' seats \tab seats.cycle \tab cyc \tab cycle component \cr
#' seats \tab seats.difforiginal \tab dor \tab fully differenced transformed original series \cr
#' seats \tab seats.diffseasonaladj \tab dsa \tab fully differenced transformed SEATS seasonal adjustment \cr
#' seats \tab seats.difftrend \tab dtr \tab fully differenced transformed SEATS trend \cr
#' seats \tab seats.filtersaconc \tab fac \tab concurrent finite seasonal adjustment filter \cr
#' seats \tab seats.filtersasym \tab faf \tab symmetric finite seasonal adjustment filter \cr
#' seats \tab seats.filtertrendconc \tab ftc \tab concurrent finite trend filter \cr
#' seats \tab seats.filtertrendsym \tab ftf \tab symmetric finite trend filter \cr
#' seats \tab seats.irregular \tab s13 \tab final SEATS irregular component \cr
#' seats \tab seats.irregularoutlieradj \tab se3 \tab final SEATS irregular component, outlier adjusted \cr
#' seats \tab seats.irregularpct \tab psi \tab final irregular component, expressed as percentages if appropriate \cr
#' seats \tab seats.longtermtrend \tab ltt \tab long term trend \cr
#' seats \tab seats.pseudoinnovsadj \tab pia \tab pseudo-innovations of the final SEATS seasonal adjustment \cr
#' seats \tab seats.pseudoinnovseasonal \tab pis \tab pseudo-innovations of the seasonal component \cr
#' seats \tab seats.pseudoinnovtransitory \tab pit \tab pseudo-innovations of the transitory component \cr
#' seats \tab seats.pseudoinnovtrend \tab pic \tab pseudo-innovations of the trend component \cr
#' seats \tab seats.seasadjconst \tab sec \tab final SEATS seasonal adjustment with constant term included \cr
#' seats \tab seats.seasonal \tab s10 \tab final SEATS seasonal component \cr
#' seats \tab seats.seasonaladj \tab s11 \tab final SEATS seasonal adjustment \cr
#' seats \tab seats.seasonaladjfcstdecomp \tab afd \tab forecast of the final SEATS seasonal adjustment \cr
#' seats \tab seats.seasonaladjoutlieradj \tab se2 \tab final SEATS seasonal adjustment, outlier adjusted \cr
#' seats \tab seats.seasonaladjse \tab ase \tab standard error of final seasonally adjusted series \cr
#' seats \tab seats.seasonalfcstdecomp \tab sfd \tab forecast of the seasonal component \cr
#' seats \tab seats.seasonalpct \tab pss \tab final seasonal factors, expressed as percentages if appropriate \cr
#' seats \tab seats.seasonalse \tab sse \tab standard error of final steasonal component \cr
#' seats \tab seats.seasonalsum \tab ssm \tab seasonal-period-length sums of final SEATS seasonal component \cr
#' seats \tab seats.seriesfcstdecomp \tab ofd \tab forecast of the series component \cr
#' seats \tab seats.squaredgainsaconc \tab gac \tab squared gain for finite concurrent seasonal adjustment filter \cr
#' seats \tab seats.squaredgainsasym \tab gaf \tab squared gain for finite symmetric seasonal adjustment filter \cr
#' seats \tab seats.squaredgaintrendconc \tab gtc \tab squared gain for finite concurrent trend filter \cr
#' seats \tab seats.squaredgaintrendsym \tab gtf \tab squared gain for finite symmetric trend filter \cr
#' seats \tab seats.timeshiftsaconc \tab tac \tab time shift for finite concurrent seasonal adjustment filter \cr
#' seats \tab seats.timeshifttrendconc \tab ttc \tab time shift for finite concurrent trend filter \cr
#' seats \tab seats.totaladjustment \tab sta \tab total adjustment factors for SEATS seasonal adjustment \cr
#' seats \tab seats.transitory \tab s14 \tab final SEATS transitory component \cr
#' seats \tab seats.transitoryfcstdecomp \tab yfd \tab forecast of the transitory component \cr
#' seats \tab seats.transitorypct \tab psc \tab final transitory component, expressed as percentages if appropriate \cr
#' seats \tab seats.transitoryse \tab cse \tab standard error of final transitory component \cr
#' seats \tab seats.trend \tab s12 \tab final SEATS trend component \cr
#' seats \tab seats.trendadjls \tab stl \tab level shift adjusted trend \cr
#' seats \tab seats.trendconst \tab stc \tab final SEATS trend component with constant term included \cr
#' seats \tab seats.trendfcstdecomp \tab tfd \tab forecast of the trend component \cr
#' seats \tab seats.trendse \tab tse \tab standard error of final trend component \cr
#' seats \tab seats.wkendfilter \tab wkf \tab end filters of the semi-infinite Wiener-Kolmogorov filter \cr
#' series \tab series.adjoriginal \tab b1 \tab original series, adjusted for prior effects and forecast extended \cr
#' series \tab series.calendaradjorig \tab a18 \tab original series adjusted for regARIMA calendar effects \cr
#' series \tab series.outlieradjorig \tab a19 \tab original series adjusted for regARIMA outliers \cr
#' series \tab series.seriesmvadj \tab mv \tab original series with missing values replaced by regARIMA estimates \cr
#' series \tab series.span \tab a1 \tab time series data, with associated dates (if the span argument is present, data are printed and/or saved only for the specified span) \cr
#' slidingspans \tab slidingspans.chngspans \tab chs \tab month-to-month (or quarter-to-quarter) changes from all sliding spans \cr
#' slidingspans \tab slidingspans.indchngspans \tab cis \tab indirect month-to-month (or quarter-to-quarter) changes from all sliding spans \cr
#' slidingspans \tab slidingspans.indsaspans \tab ais \tab indirect seasonally adjusted series from all sliding spans \cr
#' slidingspans \tab slidingspans.indsfspans \tab sis \tab indirect seasonal factors from all sliding spans \cr
#' slidingspans \tab slidingspans.indychngspans \tab yis \tab indirect year-to-year changes from all sliding spans \cr
#' slidingspans \tab slidingspans.sfspans \tab sfs \tab seasonal factors from all sliding spans \cr
#' slidingspans \tab slidingspans.tdspans \tab tds \tab trading day factors from all sliding spans \cr
#' slidingspans \tab slidingspans.ychngspans \tab ycs \tab year-to-year changes from all sliding spans \cr
#' spectrum \tab spectrum.speccomposite \tab is0 \tab spectral plot of first-differenced aggregate series \cr
#' spectrum \tab spectrum.specextresiduals \tab ser \tab spectrum of the extended residuals \cr
#' spectrum \tab spectrum.specindirr \tab is2 \tab spectral plot of outlier-modified irregular series from the indirect seasonal adjustment \cr
#' spectrum \tab spectrum.specindsa \tab is1 \tab spectral plot of the first-differenced indirect seasonally adjusted series \cr
#' spectrum \tab spectrum.specirr \tab sp2 \tab spectral plot of outlier-modified X-11 irregular series \cr
#' spectrum \tab spectrum.specorig \tab sp0 \tab spectral plot of the first-differenced original series \cr
#' spectrum \tab spectrum.specresidual \tab spr \tab spectral plot of the regARIMA model residuals \cr
#' spectrum \tab spectrum.specsa \tab sp1 \tab spectral plot of differenced, X-11 seasonally adjusted series (or of the logged seasonally adjusted series if mode = logadd or mode = mult) \cr
#' spectrum \tab spectrum.specseatsirr \tab s2s \tab spectrum of the final SEATS irregular \cr
#' spectrum \tab spectrum.specseatssa \tab s1s \tab spectrum of the differenced final SEATS seasonal adjustment \cr
#' spectrum \tab spectrum.spectukeycomposite \tab it0 \tab Tukey spectrum of the first-differenced aggregate series \cr
#' spectrum \tab spectrum.spectukeyextresiduals \tab ter \tab Tukey spectrum of the extended residuals \cr
#' spectrum \tab spectrum.spectukeyindirr \tab it2 \tab Tukey spectrum of the outlier-modified irregular series from the indirect seasonal adjustment \cr
#' spectrum \tab spectrum.spectukeyindsa \tab it1 \tab Tukey spectrum of the first-differenced indirect seasonally adjusted series \cr
#' spectrum \tab spectrum.spectukeyirr \tab st2 \tab Tukey spectrum of the outlier-modified X-11 irregular series \cr
#' spectrum \tab spectrum.spectukeyorig \tab st0 \tab Tukey spectrum of the first-differenced original series \cr
#' spectrum \tab spectrum.spectukeyresidual \tab str \tab Tukey spectrum of the regARIMA model residuals \cr
#' spectrum \tab spectrum.spectukeysa \tab st1 \tab Tukey spectrum of the differenced, X-11 seasonally adjusted series (or of the logged seasonally adjusted series if mode = logadd or mode = mult) \cr
#' spectrum \tab spectrum.spectukeyseatsirr \tab t2s \tab Tukey spectrum of the final SEATS irregular \cr
#' spectrum \tab spectrum.spectukeyseatssa \tab t1s \tab Tukey spectrum of the differenced final SEATS seasonal adjustment \cr
#' transform \tab transform.permprior \tab a2p \tab permanent prior adjustment factors, with associated dates \cr
#' transform \tab transform.permprioradjusted \tab a3p \tab prior adjusted series using only permanent prior factors, with associated dates \cr
#' transform \tab transform.permprioradjustedptd \tab a4p \tab prior adjusted series using only permanent prior factors and prior trading day adjustments, with associated dates \cr
#' transform \tab transform.prior \tab a2 \tab prior adjustment factors, with associated dates \cr
#' transform \tab transform.prioradjusted \tab a3 \tab prior adjusted series, with associated dates \cr
#' transform \tab transform.prioradjustedptd \tab a4d \tab prior adjusted series (including prior trading day adjustments), with associated dates \cr
#' transform \tab transform.seriesconstant \tab a1c \tab original series with value from the constant argument added to the series \cr
#' transform \tab transform.tempprior \tab a2t \tab temporary prior adjustment factors, with associated dates \cr
#' transform \tab transform.transformed \tab trn \tab prior adjusted and transformed data, with associated dates \cr
#' x11 \tab x11.adjoriginalc \tab c1 \tab original series modified for outliers, trading day and prior factors, C iteration \cr
#' x11 \tab x11.adjoriginald \tab d1 \tab original series modified for outliers, trading day and prior factors, D iteration \cr
#' x11 \tab x11.adjustdiff \tab fad \tab final adjustment difference (only for pseudo-additive seasonal adjustment) \cr
#' x11 \tab x11.adjustfac \tab d16 \tab combined seasonal and trading day factors \cr
#' x11 \tab x11.adjustfacpct \tab paf \tab combined adjustment factors, expressed as percentages if appropriate \cr
#' x11 \tab x11.adjustmentratio \tab e18 \tab final adjustment ratios (original series/seasonally adjusted series) \cr
#' x11 \tab x11.biasfactor \tab bcf \tab bias correction factors \cr
#' x11 \tab x11.calendar \tab d18 \tab combined holiday and trading day factors \cr
#' x11 \tab x11.calendaradjchanges \tab e8 \tab percent changes (differences) in original series adjusted for calendar effects \cr
#' x11 \tab x11.calendaradjchangespct \tab pe8 \tab percent changes in original series adjusted for calendar factors \cr
#' x11 \tab x11.combholiday \tab chl \tab combined holiday prior adjustment factors, A16 table \cr
#' x11 \tab x11.extreme \tab c20 \tab extreme values, C iteration \cr
#' x11 \tab x11.extremeb \tab b20 \tab extreme values, B iteration \cr
#' x11 \tab x11.irregular \tab d13 \tab final irregular component \cr
#' x11 \tab x11.irregularadjao \tab ira \tab final irregular component adjusted for point outliers \cr
#' x11 \tab x11.irregularb \tab b13 \tab irregular component, B iteration \cr
#' x11 \tab x11.irregularc \tab c13 \tab irregular component, C iteration \cr
#' x11 \tab x11.irregularpct \tab pir \tab final irregular component, expressed as percentages if appropriate \cr
#' x11 \tab x11.irrwt \tab c17 \tab final weights for the irregular component \cr
#' x11 \tab x11.irrwtb \tab b17 \tab preliminary weights for the irregular component \cr
#' x11 \tab x11.mcdmovavg \tab f1 \tab MCD moving average of the final seasonally adjusted series \cr
#' x11 \tab x11.modirregular \tab e3 \tab irregular component modified for zero-weighted extreme values \cr
#' x11 \tab x11.modoriginal \tab e1 \tab original series modified for zero-weighted extreme values \cr
#' x11 \tab x11.modseasadj \tab e2 \tab seasonally adjusted series modified for zero-weighted extreme values \cr
#' x11 \tab x11.modsic4 \tab c4 \tab modified SI-ratios (differences), C iteration \cr
#' x11 \tab x11.modsid4 \tab d4 \tab modified SI-ratios (differences), D iteration \cr
#' x11 \tab x11.origchanges \tab e5 \tab percent changes (differences) in original series \cr
#' x11 \tab x11.origchangespct \tab pe5 \tab percent changes in the original series \cr
#' x11 \tab x11.replacsi \tab d9 \tab final replacement values for extreme SI-ratios (differences), D iteration \cr
#' x11 \tab x11.replacsic9 \tab c9 \tab modified SI-ratios (differences), C iteration \cr
#' x11 \tab x11.robustsa \tab e11 \tab robust final seasonally adjusted series \cr
#' x11 \tab x11.sachanges \tab e6 \tab percent changes (differences) in seasonally adjusted series \cr
#' x11 \tab x11.sachangespct \tab pe6 \tab percent changes in seasonally adjusted series \cr
#' x11 \tab x11.seasadj \tab d11 \tab final seasonally adjusted series \cr
#' x11 \tab x11.seasadjb11 \tab b11 \tab seasonally adjusted series, B iteration \cr
#' x11 \tab x11.seasadjb6 \tab b6 \tab preliminary seasonally adjusted series, B iteration \cr
#' x11 \tab x11.seasadjc11 \tab c11 \tab seasonally adjusted series, C iteration \cr
#' x11 \tab x11.seasadjc6 \tab c6 \tab preliminary seasonally adjusted series, C iteration \cr
#' x11 \tab x11.seasadjconst \tab sac \tab final seasonally adjusted series with constant from transform spec included \cr
#' x11 \tab x11.seasadjd6 \tab d6 \tab preliminary seasonally adjusted series, D iteration \cr
#' x11 \tab x11.seasonal \tab d10 \tab final seasonal factors \cr
#' x11 \tab x11.seasonaladjregsea \tab ars \tab seasonal factors adjusted for user-defined seasonal regARIMA component \cr
#' x11 \tab x11.seasonalb10 \tab b10 \tab seasonal factors, B iteration \cr
#' x11 \tab x11.seasonalb5 \tab b5 \tab preliminary seasonal factors, B iteration \cr
#' x11 \tab x11.seasonalc10 \tab c10 \tab preliminary seasonal factors, C iteration \cr
#' x11 \tab x11.seasonalc5 \tab c5 \tab preliminary seasonal factors, C iteration \cr
#' x11 \tab x11.seasonald5 \tab d5 \tab preliminary seasonal factors, D iteration \cr
#' x11 \tab x11.seasonaldiff \tab fsd \tab final seasonal difference (only for pseudo-additive seasonal adjustment) \cr
#' x11 \tab x11.seasonalpct \tab psf \tab final seasonal factors, expressed as percentages if appropriate \cr
#' x11 \tab x11.sib3 \tab b3 \tab preliminary unmodified SI-ratios (differences) \cr
#' x11 \tab x11.sib8 \tab b8 \tab unmodified SI-ratios (differences) \cr
#' x11 \tab x11.tdadjorig \tab c19 \tab original series adjusted for final trading day \cr
#' x11 \tab x11.tdadjorigb \tab b19 \tab original series adjusted for preliminary trading day \cr
#' x11 \tab x11.totaladjustment \tab tad \tab total adjustment factors (only printed out if the original series contains values that are <= 0) \cr
#' x11 \tab x11.trend \tab d12 \tab final trend-cycle \cr
#' x11 \tab x11.trendadjls \tab tal \tab final trend-cycle adjusted for level shift outliers \cr
#' x11 \tab x11.trendb2 \tab b2 \tab preliminary trend-cycle, B iteration \cr
#' x11 \tab x11.trendb7 \tab b7 \tab preliminary trend-cycle, B iteration \cr
#' x11 \tab x11.trendc2 \tab c2 \tab preliminary trend-cycle, C iteration \cr
#' x11 \tab x11.trendc7 \tab c7 \tab preliminary trend-cycle, C iteration \cr
#' x11 \tab x11.trendchanges \tab e7 \tab percent changes (differences) in final trend component series \cr
#' x11 \tab x11.trendchangespct \tab pe7 \tab percent changes in final trend cycle \cr
#' x11 \tab x11.trendconst \tab tac \tab final trend component with constant from transform spec included \cr
#' x11 \tab x11.trendd2 \tab d2 \tab preliminary trend-cycle, D iteration \cr
#' x11 \tab x11.trendd7 \tab d7 \tab preliminary trend-cycle, D iteration \cr
#' x11 \tab x11.unmodsi \tab d8 \tab final unmodified SI-ratios (differences) \cr
#' x11 \tab x11.unmodsiox \tab d8b \tab final unmodified SI-ratios, with labels for outliers and extreme values \cr
#' x11 \tab x11.yrtotals \tab e4 \tab ratio of yearly totals of original and seasonally adjusted series \cr
#' x11regression \tab x11regression.calendar \tab xca \tab final calendar factors (trading day and holiday) \cr
#' x11regression \tab x11regression.calendarb \tab bxc \tab preliminary calendar factors \cr
#' x11regression \tab x11regression.combcalendar \tab xcc \tab final calendar factors from combined daily weights \cr
#' x11regression \tab x11regression.combcalendarb \tab bcc \tab preliminary calendar factors from combined daily weights \cr
#' x11regression \tab x11regression.combtradingday \tab c18 \tab final trading day factors from combined daily weights \cr
#' x11regression \tab x11regression.combtradingdayb \tab b18 \tab preliminary trading day factors from combined daily weights \cr
#' x11regression \tab x11regression.extremeval \tab c14 \tab irregulars excluded from the irregular regression, C iteration \cr
#' x11regression \tab x11regression.extremevalb \tab b14 \tab irregulars excluded from the irregular regression, B iteration \cr
#' x11regression \tab x11regression.holiday \tab xhl \tab final holiday factors \cr
#' x11regression \tab x11regression.holidayb \tab bxh \tab preliminary holiday factors \cr
#' x11regression \tab x11regression.outlieriter \tab xoi \tab detailed results for each iteration of outlier detection including outliers detected, outliers deleted, model parameter estimates, and robust and non-robust estimates of the residual standard deviation \cr
#' x11regression \tab x11regression.priortd \tab a4 \tab prior trading day weights and factors \cr
#' x11regression \tab x11regression.tradingday \tab c16 \tab final trading day factors and weights \cr
#' x11regression \tab x11regression.tradingdayb \tab b16 \tab preliminary trading day factors and weights \cr
#' x11regression \tab x11regression.x11reg \tab c15 \tab final irregular regression coefficients and diagnostics \cr
#' x11regression \tab x11regression.x11regb \tab b15 \tab preliminary irregular regression coefficients and diagnostics \cr
#' x11regression \tab x11regression.xregressioncmatrix \tab xrc \tab correlation matrix of irregular regression parameter estimates if used with the print argument; covariance matrix of same if used with the save argument \cr
#' x11regression \tab x11regression.xregressionmatrix \tab xrm \tab values of irregular regression variables with associated dates \cr
#' }
#'
#'
#' @param x  an object of class `"seas"`.
#' @param series  character vector, short or long names of an X-13ARIMA-SEATS
#'   table. If a long name is specified, it needs to be combined with the spec
#'   name and separated by a dot (it is not unique, otherwise. See list below.). More than one
#'   series can be specified (see examples).
#' @param reeval logical, if `TRUE`, the model is re-evaluated with the
#'   corresponding specs enabled.
#' @param verbose logical, if `TRUE`, a message is returned if a spec is added
#'   during reevaluation.
#'
#' @return depending on the table, either an object of class `"ts"` or
#'   `"data.frame"`.
#'
#' @seealso [seas()] for the main function.
#'
#' @references Vignette with a more detailed description:
#'   <http://www.seasonal.website/seasonal.html>
#'
#'   Comprehensive list of R examples from the X-13ARIMA-SEATS manual:
#'   <http://www.seasonal.website/examples.html>
#'
#'   Official X-13ARIMA-SEATS manual:
#'   <https://www2.census.gov/software/x-13arima-seats/x13as/windows/documentation/docx13as.pdf>
#'
#' @export
#'
#'
#' @examples
#'
#' \donttest{
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
#' series(m, "slidingspans.ychngspans")
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
#' plot(x13.pacf[,1], t = "h")
#' lines(x13.pacf[,2])
#' lines(-x13.pacf[,2])
#' # R equivalent: pacf from stats
#' pacf(AirPassengers, lag.max = 35)
#'
#' # use with composite (see vignette("multiple", "seasonal"))
#' m_composite <- seas(
#'   cbind(mdeaths, fdeaths),
#'   composite = list(),
#'   series.comptype = "add"
#' )
#' series(m_composite, "composite.indseasadj")
#' }
#'
series <- function(x, series, reeval = TRUE, verbose = TRUE){

  if (inherits(x, "seas_multi")) {
    if (is.null(x$composite)) {
      stop("does not contain a composite element")
    }
    series.short <- series_short(series)

    if (reeval){
      reeval.dots <- reeval_dots(x = x$composite, series.short = series.short, verbose = FALSE)
      if (length(reeval.dots) > 0){
        message_rerun_hint(x$call, reeval.dots)

        x$composite$list <- c(x$composite$list, reeval.dots)
        x <- update_seas_multi(x)
      }
    }

    z <- do.call(cbind, x$composite$series[series.short])
    z
    return(z)
  }

  stopifnot(inherits(x, "seas"))

  series.short <- series_short(series)

  # reeval with non present output
  if (reeval){

    reeval.dots <- reeval_dots(x = x, series.short = series.short, verbose = verbose)

    if (length(reeval.dots) > 0) {
      message_rerun_hint(x$call, reeval.dots)

      # this is the same as in update.seas()
      ml <- x$list
      # overwrite args in existing list
      ml <- ml[!names(ml) %in% names(reeval.dots)]
      x <- seas(list = c(ml, reeval.dots))
    }
  }

  ll <- x$series[series.short]

  # remove missing series from list
  ll <- ll[!is.na(names(ll))]

  # return mts if possible
  if (all(vapply(ll, inherits, TRUE, "ts"))) {
    z <- do.call(cbind, ll)
  } else {
    if (length(ll) == 1L) {
      z <- ll[[1]]
    } else {
      z <- ll
    }
  }

  z
}

message_rerun_hint <- function(call, dots) {
  new_args <- lapply(split(dots, names(dots)), \(x) {
    unique(
      c(
        unlist(unname(x)), # new args
        call[[names(x)[1]]] # if call already has this parameter filled, add it to the new ones
      )
    )
  })
  call[names(new_args)] <- new_args

  # using a single message call because expect_message
  # apparently only considers the first one
  message(sprintf("To speed up, extend the `seas()` call (see ?series):\n%s",
                  deparse(call)))
}

series_short <- function(series) {
  SPECS <- get_specs()

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
  series.short
}




reeval_dots <- function(x, series.short, verbose = TRUE) {

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
    SPECS <- get_specs()
    spec.i <- as.character(SPECS[SPECS$short == series.NA.i & SPECS$is.series, ]$spec)
    if (length(spec.i) > 1) stop("not unique.")
    if (!spec.i %in% names(x$spc)){
      if (spec.i %in% c("x11", "seats")){
        stop(spec.i, " is not activated. You should change the adjustment method.")
      } else {
        activated <- c(activated, spec.i)
      }
    }

    # additional options that are required to produce a series
    requires.i <- as.character(SPECS[SPECS$short == series.NA.i & SPECS$is.series, ]$requires)
    if (!identical(requires.i, "")) {
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

  reeval.dots

}
