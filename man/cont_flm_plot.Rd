\name{cont_flm_plot}
\alias{cont_flm_plot}

\title{Plot Functional Linear Model Analysis Results of a Continuous Type}
\description{This function produces two plots: An effect of a continuous covariate on activity values by time and the F-test for the effect of the continuous covariate.}
\usage{cont_flm_plot(smoothdata, matchresults, flmresults, xlim, ylim, 
	ftest, nperm, lb, xat, yboxlow, yboxhigh, ytext, labelp, L)
}

\arguments{
  \item{smoothdata}{List output from the \code{fda.smoothdata} function.}
  \item{matchresults}{List output from the \code{matchid} function.}
  \item{flmresults}{List output from the \code{flm_cate} function.}
  \item{xlim}{X axis limits for activity plot.}
  \item{ylim}{Y axis limits for activity plot.}
  \item{ftest}{A logic value indicating whether to implement F test or not. F test will be implement if \code{ftest is TRUE}.}
  \item{nperm}{The number of permutations for the F-test.}
  \item{lb}{X-axis labels.}
  \item{xat}{X axis label positions.}
  \item{yboxlow}{Y axis position of the lower edge of the legend box.}
  \item{yboxhigh}{Y axis position of the upper edge of the legend box.}
  \item{ytext}{Y axis position of the scale values of the legend box}
  \item{labelp}{Y axis position of the predictor variable label on the legend box.}
  \item{L}{The length of the time points.}
}

\value{One plot of the estimated group means and a possible second plot of the F-test results.}

\author{William Shannon, Tao Li, Hong Xian, Jia Wang, Elena Deych, Carlos Gonzalez}

\examples{
data(act_29pt)
data(clinic_29pt_ahi)

colnames(act_29pt) <- sub("X", "", colnames(act_29pt)) 
data <- as.matrix(act_29pt[,-1])

matchid  <- fda.matchid(mat=data, acov=clinic_29pt_ahi, type="contin")
FDcont <- fda.smoothdata(matchid, nbasis=9, basistype="fourier")

geftFDcont <- flm_cate(FDcont ,basistype="fourier", nbasis=9)
		
L <- nrow(data)
lb <- c("Midnight", "6AM", "Noon", "6PM", "Midnight") 
xat <- c(0, L/4, L/2, 3*L/4, L)

xlim <- c(0, L) 
ylim <- c(min(predict(geftFDcont$freg$yhatfdobj)), 
	max(predict(geftFDcont$freg$yhatfdobj)) + 100)

yboxlow <- max(predict(geftFDcont$freg$yhatfdobj)) - 100 
yboxhigh <- max(predict(geftFDcont$freg$yhatfdobj))
ytext <- max(predict(geftFDcont$freg$yhatfdobj)) + 20
labelp <- max(predict(geftFDcont$freg$yhatfdobj)) - 110

cont_flm_plot(smoothdata=FDcont, matchresults=matchid, 
	flmresults=geftFDcont, xlim=xlim, ylim=ylim, ftest=TRUE, 
	nperm=10, lb=lb, xat=xat, yboxlow=yboxlow, 
	yboxhigh=yboxhigh, ytext=ytext, labelp=labelp, L=L)
}