%% File Name: mice.impute.weighted.pmm.Rd
%% File Version: 0.272

\name{mice.impute.weighted.pmm}
\alias{mice.impute.weighted.pmm}
\alias{mice.impute.weighted.norm}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Imputation by Weighted Predictive Mean Matching or Weighted Normal Linear Regression
}
\description{
Imputation by predictive mean matching or normal linear regression
using sampling weights.
}
\usage{
mice.impute.weighted.pmm(y, ry, x, wy=NULL, imputationWeights=NULL,
      pls.facs=NULL, interactions=NULL, quadratics=NULL, donors=5, ...)

mice.impute.weighted.norm(y, ry, x, wy=NULL, ridge=1e-05, pls.facs=NULL,
     imputationWeights=NULL, interactions=NULL, quadratics=NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
 \item{y}{Incomplete data vector of length \code{n}}
\item{ry}{
    Vector of missing data pattern (\code{FALSE} -- missing,
    \code{TRUE} -- observed)
  }
\item{x}{Matrix (\code{n} x \code{p}) of complete covariates.
}
\item{wy}{Logical vector of length \code{length(y)}. A \code{TRUE} value indicates
locations in \code{y} for which imputations are created.}
  \item{imputationWeights}{
Optional vector of sampling weights
}
  \item{pls.facs}{
Number of factors in PLS regression (if used). The default is \code{NULL}
which means that no PLS regression is used for dimension reduction.
}
  \item{interactions}{
Optional vector of variables for which interactions should be created
}
  \item{quadratics}{
Optional vector of variables which should also be included as quadratic effects.
}
\item{donors}{Number of donors}
  \item{\dots}{
Further arguments to be passed
}
  \item{ridge}{
Ridge parameter in the diagonal of \eqn{ \bold{X}'\bold{X}}
}
}


%\details{
%%  ~~ If necessary, more details than the description above ~~
%}


\value{
A vector of length \code{nmis=sum(!ry)} with imputed values.
}

%\references{
%% ~put references to the literature/web site here ~
%}


%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

% \seealso{
% For imputation with the linear normal regression and sampling weights
% see \code{\link{mice.impute.weighted.norm}}.
% }

\examples{
\dontrun{
#############################################################################
# EXAMPLE 1: Imputation using sample weights
#############################################################################

data( data.ma01)
set.seed(977)

# select subsample
dat <- as.matrix(data.ma01)
dat <- dat[ 1:1000, ]

# empty imputation
imp0 <- mice::mice( dat, maxit=0)

# redefine imputation methods
meth <- imp0$method
meth[ meth=="pmm"  ] <- "weighted.pmm"
meth[ c("paredu", "books", "migrant" ) ] <- "weighted.norm"
# redefine predictor matrix
pm <- imp0$predictorMatrix
pm[, 1:3 ] <- 0
# do imputation
imp <- mice::mice( dat, predictorMatrix=pm, method=meth,
           imputationWeights=dat[,"studwgt"], m=3, maxit=5)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%% \keyword{Predictive mean matching}
%% \keyword{Sampling weights}
%% \keyword{mice imputation method}
