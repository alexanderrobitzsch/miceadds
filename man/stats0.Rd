%% File Name: stats0.Rd
%% File Version: 0.12

\name{stats0}
\alias{stats0}
\alias{max0}
\alias{min0}
\alias{mean0}
\alias{sd0}
\alias{var0}
\alias{quantile0}
\alias{prop_miss}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Descriptive Statistics for a Vector or a Data Frame
}
\description{
Applies descriptive statistics to a vector or a data frame. The function
\code{stats0} is a general function. This function is used for extending
the basic descriptive statistics functions from the \pkg{base} and
\pkg{stats} package. The function \code{prop_miss} computes the proportion
of missing data for each variable.
}

\usage{
stats0(x, FUN, na.rm=TRUE,...)

max0(x, na.rm=TRUE)
mean0(x, na.rm=TRUE)
min0(x, na.rm=TRUE)
quantile0(x, probs=seq(0, 1, 0.25), na.rm=TRUE)
sd0(x, na.rm=TRUE)
var0(x, na.rm=TRUE)

prop_miss(x)
}
%- maybe also 'usage' for other objects documented here.


\arguments{
\item{x}{
Vector or a data frame
}
\item{FUN}{
Function which is applied to \code{x}
}
\item{na.rm}{Logical indicating whether missing data should be removed}
\item{probs}{Probabilities}
\item{\dots}{Further arguments to be passed}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
A vector or a matrix
}
%\references{
%% ~put references to the literature/web site here ~
%}

%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[base:max]{base::max}},
\code{\link[base:mean]{base::mean}},
\code{\link[base:min]{base::min}},
\code{\link[stats:quantile]{stats::quantile}},
\code{\link[stats:sd]{stats::sd}},
\code{\link[stats:var]{stats::var}}
}

\examples{
#############################################################################
# EXAMPLE 1: Descriptive statistics toy datasets
#############################################################################

#--- simulate vector y and data frame dat
set.seed(765)
N <- 25    # number of observations
y <- stats::rnorm(N)
V <- 4    # number of variables
dat <- matrix( stats::rnorm( N*V ), ncol=V )
colnames(dat) <- paste0("V",1:V)

#-- standard deviation
apply( dat, 2, stats::sd )
sd0( dat )
#-- mean
apply( dat, 2, base::mean )
mean0( dat )
#-- quantile
apply( dat, 2, stats::quantile )
quantile0( dat )
#-- minimum and maximum
min0(dat)
max0(dat)

#*** apply functions to missing data
dat1 <- dat
dat1[ cbind( c(2,5),2) ] <- NA

#-- proportion of missing data
prop_miss( dat1 )
#-- MAD statistic
stats0( dat, FUN=stats::mad )
#-- SD
sd0(y)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%% \keyword{R utilities}
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
