%% File Name: mice.impute.simputation.Rd
%% File Version: 0.09

\name{mice.impute.simputation}
\alias{mice.impute.simputation}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Wrapper Function to Imputation Methods in the \pkg{simputation} Package
}
\description{
This imputation method provides a wrapper function to univariate imputation methods
in the \pkg{simputation} package.
}
\usage{
mice.impute.simputation(y, ry, x, Fun=NULL, Fun_args=NULL, ... )
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
\item{Fun}{Name of imputation functions in \pkg{simputation} package, e.g.,
\code{imputeR::impute_lm}, see Details.}
\item{Fun_args}{Optional argument list for \code{Fun}}
\item{\dots}{
Further arguments to be passed
}
}


\details{
Selection of imputation methods included in the \pkg{simputation} package:

linear regression: \code{\link[simputation:impute_lm]{simputation::impute_lm}}, \cr
robist linear regression with M-estimators:
\code{\link[simputation:impute_rlm]{simputation::impute_rlm}}, \cr
regularized regression with lasso/elasticnet/ridge regression:
\code{\link[simputation:impute_en]{simputation::impute_en}}, \cr
CART models or random forests:
\code{\link[simputation:impute_cart]{simputation::impute_cart}},
\code{\link[simputation:impute_rf]{simputation::impute_rf}}, \cr
Hot deck imputation:
\code{\link[simputation:impute_rhd]{simputation::impute_rhd}},
\code{\link[simputation:impute_shd]{simputation::impute_shd}}, \cr
Predictive mean matching:
\code{\link[simputation:impute_pmm]{simputation::impute_pmm}}, \cr
k-nearest neighbours imputation:
\code{\link[simputation:impute_knn]{simputation::impute_knn}}
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
# EXAMPLE 1: Nhanes example
#############################################################################

library(mice)
library(simputation)

data(nhanes, package="mice")
dat <- nhanes

#** imputation methods
method <- c(age="",bmi="norm", hyp="norm", chl="simputation")
Fun <- list( chl=simputation::impute_lm)
Fun_args <- list( chl=list(add_residual="observed") )

#** do imputations
imp <- mice::mice(dat, method=method, Fun=Fun, Fun_args=Fun_args)
summary(imp)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%% \keyword{mice imputation method}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
