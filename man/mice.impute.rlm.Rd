%% File Name: mice.impute.rlm.Rd
%% File Version: 0.16

\name{mice.impute.rlm}
\alias{mice.impute.rlm}
\alias{mice.impute.lqs}
\alias{mice.impute.lm}
\alias{mice.impute.lm_fun}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Imputation of a Linear Model by Bayesian Bootstrap
}
\description{
These functions impute from linear models using the functions
\code{\link[stats:lm]{stats::lm}}, \code{\link[MASS:rlm]{MASS::rlm}}
or \code{\link[MASS:lqs]{MASS::lqs}}. The method \code{mice.impute.lm_fun}
allows the definition of a general linear regression fitting function for
which the methods \code{predict} and \code{residuals} are defined.

Parameters of the model are estimated
by Bayesian bootstrap. Predicted values are computed and residuals are randomly
drawn from the empirical distribution of residuals of observed data.
}
\usage{
mice.impute.lm(y, ry, x, wy=NULL, lm_args=NULL, trafo=NULL, antitrafo=NULL, ...)

mice.impute.rlm(y, ry, x, wy=NULL, lm_args=NULL, trafo=NULL, antitrafo=NULL, ...)

mice.impute.lqs(y, ry, x, wy=NULL, lm_args=NULL, trafo=NULL, antitrafo=NULL, ...)

mice.impute.lm_fun(y, ry, x, wy=NULL, lm_args=NULL, lm_fun="lm", trafo=NULL,
               antitrafo=NULL, ...)
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
\item{wy}{Vector of logicals indicating which entries should be imputed}
\item{lm_args}{List of arguments for \code{\link[stats:lm]{stats::lm}},
\code{\link[MASS:rlm]{MASS::rlm}}, \code{\link[MASS:lqs]{MASS::lqs}} or
a user-defined function.}
\item{lm_fun}{Linear regression fitting function, e.g. \code{stats::lm} for which
S3 methods \code{predict} and \code{residuals} are defined.}
\item{trafo}{Optional function for transforming the outcome values}
\item{antitrafo}{Optional function which is the inverse function of \code{trafo}}
\item{\dots}{
Further arguments to be passed
}
}


%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}


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
%\code{Hmisc::aregImpute}
%}
%% ~~objects to See Also as \code{\link{help}}, ~~~

\examples{
\dontrun{
#############################################################################
# EXAMPLE 1: Some toy example illustrating the methods
#############################################################################

library(MASS)
library(mice)

#-- simulate data
set.seed(98)
N <- 1000
x <- stats::rnorm(N)
z <- 0.5*x + stats::rnorm(N, sd=.7)
y <- stats::rnorm(N, mean=.3*x - .2*z, sd=1 )
dat <- data.frame(x,z,y)
dat[ seq(1,N,3), c("x","y") ] <- NA
dat[ seq(1,N,4), "z" ] <- NA

#-- define imputation methods
imp <- mice::mice(dat, maxit=0)
method <- imp$method
method["x"] <- "rlm"
method["z"] <- "lm"
method["y"] <- "lqs"

#-- impute data
imp <- mice::mice(dat, method=method)
summary(imp)

#--- example using transformations
dat1$x <- exp(dat1$x)
dat1$z <- stats::plogis(dat1$z)

trafo <- list(x=log, z=stats::qlogis)
antitrafo <- list(x=exp, z=stats::plogis)

#- impute with transformations
imp2 <- mice::mice(dat1, method=method, m=1, maxit=3, trafo=trafo, antitrafo=antitrafo)
print(imp2)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%% \keyword{mice imputation method}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
