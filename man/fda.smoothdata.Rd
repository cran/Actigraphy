\name{fda.smoothdata}
\alias{fda.smoothdata}
\title{Functional Actigraphy Data Smoothing}
\description{This function produces functional actigraphy data from matrix actigraphy data.}
\usage{
fda.smoothdata(data, basistype="fourier", nbasis=9, norder=4)
}
\arguments{
  \item{data}{A list consisting of the following two components: \cr
		\code{data$mat} A matrix where rows represent the time, columns are the samples, and the column names are the subjects. \cr
		\code{data$cov} A two column matrix that contains the actigraphy data and clinical covariate.}

  \item{basistype}{A string specifying either \code{"Fourier"} and \code{"bspline"}.}
  \item{nbasis}{The number of basis functions to be used for functional data. Default value is 9.}
  \item{norder}{The order of the bspline basis functions. Default value is 4.}
}

\details{
Note: The output of function \code{fda.matchid} can be directly used as the input for this argument. \cr

If the data is a categorical covariate

}

\value{
A list consisting of two components as follows:
	\item{fd}{A \code{fdSmooth} data object containing the functional data (see function \code{smooth.basis} in the package \code{fda} for details).}
	\item{cov}{An object that is the same as the argument \code{data$cov}.}
}
\author{William Shannon, Tao Li, Hong Xian, Jia Wang, Elena Deych, Carlos Gonzalez}

\examples{
data(act_29pt)
data(clinic_29pt_ahi)

colnames(act_29pt) <- sub("X", "", colnames(act_29pt)) 
data <- as.matrix(act_29pt[,-1])

matchid  <- fda.matchid(mat=data, acov=clinic_29pt_ahi, type="contin")

FDcont <- fda.smoothdata(matchid, nbasis=9, basistype="fourier")

### Smooth the Results
ts.plot(predict(FDcont$fd$fd, c(1:1440)), main="Smoothed Activity Data")
}

