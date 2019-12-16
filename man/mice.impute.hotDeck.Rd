%% File Name: mice.impute.hotDeck.Rd
%% File Version: 0.21

\name{mice.impute.hotDeck}
\alias{mice.impute.hotDeck}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Imputation of a Variable Using Probabilistic Hot Deck Imputation
}
\description{
Imputes a variable under a random draw from a pool of donors
defined by a distance function. Uncertainty with respect
to the creation of donor pools is introduced by drawing a Bootstrap
sample (approximate Bayesian Bootstrap, ABB)
from observations with complete data (see Andridge & Little, 2010).
}

\usage{
mice.impute.hotDeck(y, ry, x, donors=5, method="Mahalanobis", ...)
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
\item{donors}{Number of donors used for random sampling of nearest
neighbors in imputation}
\item{method}{Method used for computation of weights in distance function.
    Options are the Mahalanobis metric (\code{method="Mahalanobis"}),
    weighted by correlations of covariates with the outcome
    (\code{method="cor"}) and weighting by linear regression
    coefficients (\code{method="lm"}).
}
  \item{\dots}{
Further arguments to be passed
}
}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}

\value{
A vector of length \code{nmis=sum(!ry)} with imputed values.
}

\references{
Andridge, R. R., &  and Little, R. J. A. (2010).
A review of hot deck imputation for survey non-response.
\emph{International Statistical Review, 78}(1), 40-64.
\doi{10.1111/j.1751-5823.2010.00103.x}
}

%\author{
%Alexander Robitzsch
%}

%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See also the packages \pkg{hot.deck} and
\pkg{HotDeckImputation}.
}

\examples{
\dontrun{
#############################################################################
# EXAMPLE 1: Hot deck imputation NHANES dataset
#############################################################################

data(nhanes, package="mice")
dat <- nhanes

#*** prepare imputation method
vars <- colnames(dat)
V <- length(vars)
impMethod <- rep("hotDeck", V)
method <- "cor"

#*** imputation in mice
imp <- mice::mice( data=as.matrix(dat), m=1, method=impMethod, method=method )
summary(imp)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%% \keyword{mice imputation method}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
