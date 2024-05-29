%% File Name: mice.impute.catpmm.Rd
%% File Version: 0.08

\name{mice.impute.catpmm}
\alias{mice.impute.catpmm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Imputation of a Categorical Variable Using Multivariate Predictive
Mean Matching
}
\description{
Imputes a categorical variable using multivariate predictive mean matching.
}

\usage{
mice.impute.catpmm(y, ry, x, donors=5, ridge=10^(-5), ...)
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
\item{ridge}{Numerical constant used for avioding collinearity issues. Noise
is added to covariates.
}
  \item{\dots}{
Further arguments to be passed
}
}

\details{
The categorical outcome variable is recoded as a vector of dummy variables.
A multivariate linear regression is specified for computing predicted values.
The L1 distance (i.e., sum of absolute deviations) is utilized for
predictive mean matching. Predictive mean matching for categorical variables
has been proposed by Meinfelder (2009) using a multinomial regression instead of
ordinary linear regression.
}

\value{
A vector of length \code{nmis=sum(!ry)} with imputed values.
}

\references{
Meinfelder, F. (2009). \emph{Analysis of Incomplete Survey Data - Multiple
Imputation via Bayesian Bootstrap Predictive Mean Matching}.
Dissertation thesis. University of Bamberg, Germany.
\url{https://fis.uni-bamberg.de/handle/uniba/213}
}

%\author{
%Alexander Robitzsch
%}

%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%See also the packages \pkg{hot.deck} and
%\pkg{HotDeckImputation}.
%}

\examples{
\dontrun{
#############################################################################
# EXAMPLE 1: Imputation internat data
#############################################################################

data(data.internet, package="miceadds")
dat <- data.internet

#** empty imputation
imp0 <- mice::mice(dat, m=1, maxit=0)
method <- imp0$method
predmat <- imp0$predictorMatrix

#** define factor variable

dat1 <- dat
dat1[,1] <- as.factor(dat1[,1])
method[1] <- "catpmm"

#** impute with 'catpmm''
imp <- mice::mice(dat1, method=method, m=5)
summary(imp)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%% \keyword{mice imputation method}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
