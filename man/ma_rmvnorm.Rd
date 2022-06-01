%% File Name: ma_rmvnorm.Rd
%% File Version: 0.14

\name{ma_rmvnorm}
\alias{ma_rmvnorm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Simulating Normally Distributed Data
}
\description{
Some functions for normally distributed data.

The function \code{ma_rmvnorm} is like \code{mvtnorm::rmvnorm}, but allows
for a covariance matrix \code{sigma} which can have zero variances.
}

\usage{
ma_rmvnorm(n, mu=NULL, sigma, eps=1e-10)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{
Sample size
}
  \item{mu}{
Mean vector
}
  \item{sigma}{
Covariance matrix
}
  \item{eps}{
Trimming constant for zero variances
}
}

%%\details{
%%  ~~ If necessary, more details than the description above ~~
%% }

\value{
Matrix of simulated values
}

%\references{
%% ~put references to the literature/web site here ~
%}


%\author{
%%  ~~who you are~~
%}


%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[MASS:mvrnorm]{MASS::mvrnorm}}
}


\examples{
\dontrun{
#############################################################################
# EXAMPLE 1: Two-dimensional simulation with zero variance at dimension 1
#############################################################################

sigma <- matrix( c(0,0,0,1), nrow=2, ncol=2)
miceadds::ma_rmvnorm( n=10, sigma=sigma )
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
