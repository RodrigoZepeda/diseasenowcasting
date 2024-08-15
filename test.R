library(NobBS)
library(cmdstanr)
library(dplyr)
library(tidybayes)
library(tidyverse)
library(janitor)
data <- denguedat
now  <- as.Date("1990-10-01")
units <- "1 week"
onset_date <- "onset_week"
report_date <- "report_week"
moving_window<- NULL
max_D<-NULL
cutoff_D<-NULL
proportion_reported<-1
quiet<-TRUE
specs=list(
  dist="NB",
  alpha1.mean.prior=0,
  alpha1.prec.prior=0.001,
  alphat.shape.prior=0.001,
  alphat.rate.prior=0.001,
  beta.priors=NULL,
  param_names=NULL,
  conf=0.95,
  dispersion.prior.shape = 0.001,
  dispersion.prior.rate = 0.001,
  nAdapt=1000,
  nChains=1,
  nBurnin=1000,
  nThin=1,
  nSamp=10000)

# Check that "now" is entered as a Date
if(inherits(now, "Date")==FALSE){
  stop("'Now' argument must be of datatype Date (as.Date)")
}

# Check that "now" is possible in the sequence of reporting data
if(dplyr::last(seq(unique(data[,onset_date])[1],now,by=units))!=now){
  stop("The date `now` is not possible to estimate: the possible nowcast dates are seq(unique(data[,onset_date])[1],now,by=units).")
}

# Print date
message(paste("Computing a nowcast for ",now))
# Define "T", the length of dates between the first date of data and "now", making sure that "T" is unaffected by skipped-over dates in the time series
# If the moving window is specified, "T" considers only the dates within the moving window; otherwise considers all historical data
now.T <- ifelse(is.null(moving_window),length(seq(min(data[,onset_date]),as.Date(now),by=units)),
                moving_window)

# Check the default arguments
if (is.null(moving_window)) {
  moving_window <- now.T
}
if (is.null(max_D)) {
  max_D <- now.T-1 # ifelse(is.null(moving_window),now.T-1,moving_window-1)
}
if (is.null(cutoff_D)) {
  cutoff_D <- TRUE
}
if(quiet==TRUE){
  progress.bar <- "none"
}
if(quiet==FALSE){
  progress.bar <- "text"
}

# Check that proportion_reported is between 0,1
if (proportion_reported > 1 | proportion_reported<=0){
  stop("The proportion_reported must be a number between (0,1].")
}

# Manipulate the control arguments
if ("Poisson"%in%(specs[["dist",exact=TRUE]])) { # if no distribution specified, take Poisson as default
  specs$dist <- "Poisson"
}
if (is.null(specs[["dist",exact=TRUE]])) {
  specs$dist <- "Poisson"
}
if (is.null(specs[["alpha1.mean.prior",exact=TRUE]])) {
  specs$alpha1.mean.prior <- 0
}
if (is.null(specs[["alpha1.prec.prior",exact=TRUE]])) {
  specs$alpha1.prec.prior <- 0.001
}
if (is.null(specs[["alphat.shape.prior",exact=TRUE]])) {
  specs$alphat.shape.prior <- 0.001
}
if (is.null(specs[["alphat.rate.prior",exact=TRUE]])) {
  specs$alphat.rate.prior <- 0.001
}
if (is.null(specs[["beta.priors",exact=TRUE]])) {
  specs$beta.priors <- rep(0.1, times=(max_D)+1)
}
if (is.null(specs[["param_names",exact=TRUE]])&(specs[["dist"]]=="Poisson")) {
  specs$param_names <- c( "lambda","alpha","beta.logged","tau2.alpha","sum.n")
}
if (is.null(specs[["param_names",exact=TRUE]])&(specs[["dist"]]=="NB")) {
  specs$param_names <- c( "lambda","alpha","beta.logged","tau2.alpha","sum.n","r")
}
if (is.null(specs[["conf",exact=TRUE]])) {
  specs$conf <- 0.95
}
if (is.null(specs[["dispersion.prior",exact=TRUE]])&(specs[["dist"]]=="NB")) {
  specs$dispersion.prior <- c(0.001,0.001)
}
if (is.null(specs[["nAdapt",exact=TRUE]])) {
  specs$nAdapt <- 1000
}
if (is.null(specs[["nChains",exact=TRUE]])) {
  specs$nChains <- 1
}
if (is.null(specs[["nBurnin",exact=TRUE]])) {
  specs$nBurnin <- 1000
}
if (is.null(specs[["nThin",exact=TRUE]])) {
  specs$nThin <- 1
}
if (is.null(specs[["nSamp",exact=TRUE]])) {
  specs$nSamp <- 10000
}
# Warnings
if(max_D>(moving_window-1)){
  stop("Maximum delay cannot be greater than the length of the moving window minus 1 time unit")
}

# Prep the data: filter only to observable cases reported at or before "now"
unit.num <- switch(units, "1 day"=1,"1 week"=7)
w.days <- max((moving_window-1)*unit.num,(now.T-1)*unit.num) # moving window converted to days

realtime.data <- subset(data,(data[,onset_date]<=now) & (data[,onset_date]>=now-w.days) & (data[,report_date]<=now) & (data[,report_date]>=now-w.days))
realtime.data$week.t <- (as.numeric(realtime.data[,onset_date]-min(realtime.data[,onset_date]))/unit.num)+1
realtime.data$delay <- as.numeric(realtime.data[,report_date]-realtime.data[,onset_date])/unit.num

if(cutoff_D==FALSE){
  realtime.data$delay <- ifelse(realtime.data$delay>=max_D,max_D,realtime.data$delay)
}

if(length(unique(realtime.data$week.t))!=now.T){
  warning("Warning! The line list has zero case reports for one or more possible onset dates at one or more delays. Proceeding under the assumption that the true number of cases at the associated delay(s) and week(s) is zero.")
}

# Build the reporting triangle, fill with NAs where unobservable
reporting.triangle <- matrix(0, nrow=now.T,ncol=(max_D+1))

for(t in 1:now.T){
  for(d in 0:max_D){
    reporting.triangle[t,(d+1)] <- nrow(realtime.data[which(realtime.data$week.t==t & realtime.data$delay==d),])
    if(now.T < (t+d)){
      reporting.triangle[t,(d+1)] <- -1
    }
  }
}

# Run the JAGS model

if(specs[["dist"]]=="Poisson"){
  params=c( "lambda","alpha","beta.logged","tau2.alpha","n","sum.n","sum.lambda")
}
if(specs[["dist"]]=="NB"){
  params=c( "lambda","alpha","beta.logged","tau2.alpha","n","sum.n","sum.lambda","r")
}
nAdapt = specs[["nAdapt"]] #default = 1000
nChains = specs[["nChains"]] # default=1
nBurnin = specs[["nBurnin"]] # default=1000
nThin = specs[["nThin"]] # default=1
nKeep = specs[["nSamp"]] # default=10,000
nIter = nKeep * nThin

if(specs[["dist"]]=="NB"){
  dataList = list(Today = now.T,
                  D = max_D,
                  n = reporting.triangle,
                  alpha1_mean_prior=specs$alpha1.mean.prior,
                  alpha1_prec_prior=specs$alpha1.prec.prior,
                  alphat_rate_prior=specs$alphat.rate.prior,
                  alphat_shape_prior=specs$alphat.shape.prior,
                  beta_priors=specs$beta.priors,
                  dispersion_prior_shape=specs$dispersion.prior[1],
                  dispersion_prior_rate=specs$dispersion.prior[2])
}

nowcastmodel <- cmdstanr::cmdstan_model("inst/stan/nowcastNB.stan")
mod <- nowcastmodel$sample(
  data = dataList,
  chains = 4,
  parallel_chains = 4,
)

ncases <- mod$summary(variables = "N_cases_predicted")

nowcastmodel2 <- cmdstanr::cmdstan_model("inst/stan/noBSrodsversion.stan")
mod <- nowcastmodel2$sample(
  data = dataList,
  chains = 4,
  parallel_chains = 4,
)

ncases2 <- mod$summary(variables = "N_cases_predicted")

test_nowcast <- NobBS(data=denguedat, now=as.Date("1990-10-01"),
                      specs = specs,
                      units="1 week",onset_date="onset_week",report_date="report_week")

nowcasts <- data.frame(test_nowcast$estimates)

nowcasts2 <- nowcasts |>
  bind_cols(ncases) |>
  bind_cols(ncases2) |>
  clean_names()

ggplot(nowcasts2) +
  geom_line(aes(onset_date,estimate,col="Nobbs estimate"),linetype="longdash") +
  geom_point(aes(onset_date,median_8,col="Rod estimate"),linetype="dashed") +
  geom_line(aes(onset_date,median_18,col="Rod estimate 2")) +
  geom_line(aes(onset_date,n_reported,col="Reported to date"),linetype="solid") +
  theme_classic()+
  #geom_ribbon(aes(x = onset_date,ymin=lower, ymax=upper, fill = "Nobbs"),alpha=0.3)+
  #geom_ribbon(aes(x = onset_date,ymin=q5, ymax=q95, fill = "Rod"),alpha=0.3)+
  xlab("Case onset date") + ylab("Estimated cases") +
  ggtitle("Observed and predicted number of cases \nat the week of nowcast (Oct 1990) and weeks prior")
ggsave("rod2.pdf", width = 8, height = 6)
