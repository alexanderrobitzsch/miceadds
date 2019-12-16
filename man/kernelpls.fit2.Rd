%% File Name: kernelpls.fit2.Rd
%% File Version: 0.294

\name{kernelpls.fit2}
\alias{kernelpls.fit2}
\alias{predict.kernelpls.fit2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Kernel PLS Regression
}
\description{
Fits a PLS regression model with the kernel algorithm (Dayal & Macgregor, 1997).
}

\usage{
kernelpls.fit2(X, Y, ncomp)

\method{predict}{kernelpls.fit2}(object,X, ...)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
Matrix of regressors
}
  \item{Y}{
Vector of a univariate outcome
}
  \item{ncomp}{
Number of components to be extracted
}
\item{object}{Object of class \code{kernelpls.fit2}}
\item{\dots}{Further arguments to be passed}
}

%\details{
%
%}

\value{
The same list as in
\code{{pls::kernelpls.fit}}
is produced.

In addition, \eqn{R^2} measures are contained in
\code{R2}.
}

\references{
Dayal, B., & Macgregor, J. F. (1997). Improved PLS algorithms.
\emph{Journal of Chemometrics, 11}(1), 73-85.

Mevik, B. H., & Wehrens, R. (2007). The \pkg{pls} package:
Principal component and partial least squares regression in \R.
\emph{Journal of Statistical Software, 18}, 1-24.
\doi{10.18637/jss.v018.i02}
}

\author{
This code is a \pkg{Rcpp} translation of the original
\code{pls::kernelpls.fit} function from the \pkg{pls} package
(see Mevik & Wehrens, 2007).
}

%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See the \pkg{pls} package for further estimation algorithms.
}

\examples{
\dontrun{
#############################################################################
# SIMULATED EXAMPLE 1: 300 cases on 100 variables
#############################################################################
set.seed(789)
library(mvtnorm)

N <- 300        # number of cases
p <- 100        # number of predictors
rho1 <- .6      # correlations between predictors

# simulate data
Sigma <- base::diag(1-rho1,p) + rho1
X <- mvtnorm::rmvnorm( N, sigma=Sigma )
beta <- base::seq( 0, 1, len=p )
y <- ( X \%*\% beta )[,1] + stats::rnorm( N, sd=.6 )
Y <- base::matrix(y,nrow=N, ncol=1 )

# PLS regression
res <- miceadds::kernelpls.fit2( X=X, Y=Y, ncomp=20 )

# predict new scores
Xpred <- predict( res, X=X[1:10,] )

#############################################################################
# EXAMPLE 2: Dataset yarn from pls package
#############################################################################

# use kernelpls.fit from pls package
library(pls)
data(yarn,package="pls")
mod1 <- pls::kernelpls.fit( X=yarn$NIR, Y=yarn$density, ncomp=10 )
# use kernelpls.fit2 from miceadds package
Y <- base::matrix( yarn$density, ncol=1 )
mod2 <- miceadds::kernelpls.fit2( X=yarn$NIR, Y=Y, ncomp=10 )
}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%% \keyword{Partial least squares regression (PLS)}
%% \keyword{Dimension reduction}
%% \keyword{predict}
% __ONLY ONE__ keyword per line
