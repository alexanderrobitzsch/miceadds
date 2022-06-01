%% File Name: fleishman_sim.Rd
%% File Version: 0.193

\name{fleishman_sim}
\alias{fleishman_sim}
\alias{fleishman_coef}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Simulating Univariate Data from Fleishman Power Normal Transformations
}

\description{
Simulates univariate non-normal data by using Fleishman power transformations
(Fleishman, 1978; Demirtas & Hedeker, 2007).
}

\usage{
fleishman_sim(N=1, coef=NULL, mean=0, sd=1, skew=0, kurt=0)

fleishman_coef(mean=0, sd=1, skew=0, kurt=0)
}


%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{N}{
Number of simulated values
}
  \item{coef}{
Optional list containing coefficients of Fleishman polynomial estimated
by \code{fleishman_coef}.
}
  \item{mean}{
Mean
}
  \item{sd}{
Standard deviation
}
  \item{skew}{
Skewness
}
\item{kurt}{
(Excess) kurtosis
}

}

\details{
For \eqn{Z \sim N(0,1)}, the Fleishman power normal variable \eqn{X} is defined as
\eqn{X=a + bZ + cZ^2 + d Z^3}.
}


\value{
Vector of simulated values (\code{fleishman_sim}) or list of coefficients
(\code{fleishman_coef}).
}

\references{
Demirtas, H., & Hedeker, D. (2008). Imputing continuous data under some
non-Gaussian distributions. \emph{Statistica Neerlandica, 62}(2), 193-205.
\doi{10.1111/j.1467-9574.2007.00377.x}

Fleishman, A. I. (1978). A method for simulating non-normal distributions.
\emph{Psychometrika, 43}(4), 521-532.
\doi{10.1007/BF02293811}
}


%\author{
%%  ~~who you are~~
%}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See also the \code{BinOrdNonNor::Fleishman.coef.NN} function in the
\pkg{BinOrdNonNor} package.

See the \code{\link{nnig_sim}} function for simulating a non-normally distributed
multivariate variables.
}


\examples{
\dontrun{
#############################################################################
# EXAMPLE 1: Simulating values with Fleishman polynomial
#############################################################################

#* define mean, standard deviation, skewness and kurtosis
mean <- .75
sd <- 2
skew <- 1
kurt <- 3

#* compute coefficients of Fleishman polynomial
coeff <- miceadds::fleishman_coef(mean=mean, sd=sd, skew=skew, kurt=kurt)
print(coeff)

# sample size
N <- 1000
set.seed(2018)
#* simulate values based on estimated coefficients
X <- miceadds::fleishman_sim(N=N, coef=coeff)
#* simulate values based on input of moments
X <- miceadds::fleishman_sim(N=N, mean=mean, sd=sd, skew=skew, kurt=kurt)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
