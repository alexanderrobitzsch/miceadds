%% File Name: mice.impute.imputeR.lmFun.Rd
%% File Version: 0.11

\name{mice.impute.imputeR.lmFun}
\alias{mice.impute.imputeR.lmFun}
\alias{mice.impute.imputeR.cFun}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Wrapper Function to Imputation Methods in the \pkg{imputeR} Package
}
\description{
The imputation methods \code{"imputeR.lmFun"} and \code{"imputeR.cFun"} provide
interfaces to imputation methods in the \pkg{imputeR} package for
continuous and binary data, respectively.
}
\usage{
mice.impute.imputeR.lmFun(y, ry, x, Fun=NULL, draw_boot=TRUE, add_noise=TRUE, ... )

mice.impute.imputeR.cFun(y, ry, x, Fun=NULL, draw_boot=TRUE, ... )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{
Incomplete data vector of length \code{n}
}
  \item{ry}{
Vector of missing data pattern (\code{FALSE} -- missing,
\code{TRUE} -- observed)
}
  \item{x}{
Matrix (\code{n} x \code{p}) of complete covariates.
}
\item{Fun}{Name of imputation functions in \pkg{imputeR} package, e.g.,
\code{imputeR::ridgeR}, see Details.}
\item{draw_boot}{Logical indicating whether a Bootstrap sample is taken
for sampling model parameters}
\item{add_noise}{Logical indicating whether empirical residuals should be
added to predicted values}
\item{\dots}{
Further arguments to be passed
}
}


\details{
Methods for continuous variables:

\code{\link[imputeR:CubistR]{imputeR::CubistR}},
\code{\link[imputeR:glmboostR]{imputeR::glmboostR}},
\code{\link[imputeR:lassoR]{imputeR::lassoR}},
\code{\link[imputeR:pcrR]{imputeR::pcrR}},
\code{\link[imputeR:plsR]{imputeR::plsR}},
\code{\link[imputeR:ridgeR]{imputeR::ridgeR}},
\code{\link[imputeR:stepBackR]{imputeR::stepBackR}},
\code{\link[imputeR:stepBothR]{imputeR::stepBothR}},
\code{\link[imputeR:stepForR]{imputeR::stepForR}}
\cr

Methods for binary variables:
\code{\link[imputeR:gbmC]{imputeR::gbmC}},
\code{\link[imputeR:lassoC]{imputeR::lassoC}},
\code{\link[imputeR:ridgeC]{imputeR::ridgeC}},
\code{\link[imputeR:rpartC]{imputeR::rpartC}},
\code{\link[imputeR:stepBackC]{imputeR::stepBackC}},
\code{\link[imputeR:stepBothC]{imputeR::stepBothC}},
\code{\link[imputeR:stepForC]{imputeR::stepForC}}
}


\value{
A vector of length \code{nmis=sum(!ry)} with imputed values.
}

%\references{
%% ~put references to the literature/web site here ~
%}



%\note{
%XXX
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%\code{\link[imputeR:ridgeR]{imputeR::ridgeR}}
%}
%% ~~objects to See Also as \code{\link{help}}, ~~~

\examples{
\dontrun{
#############################################################################
# EXAMPLE 1: Example with binary and continuous variables
#############################################################################

library(mice)
library(imputeR)

data(nhanes, package="mice")
dat <- nhanes
dat$hyp <- as.factor(dat$hyp)

#* define imputation methods
method <- c(age="",bmi="norm",hyp="imputeR.cFun",chl="imputeR.lmFun")
Fun <- list( hyp=imputeR::ridgeC, chl=imputeR::ridgeR)

#** do imputation
imp <- mice::mice(dat1, method=method, maxit=10, m=4, Fun=Fun)
summary(imp)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%% \keyword{mice imputation method}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
