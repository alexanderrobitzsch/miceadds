%% File Name: mice.impute.bygroup.Rd
%% File Version: 0.25

\name{mice.impute.bygroup}
\alias{mice.impute.bygroup}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Groupwise Imputation Function
}
\description{
The function \code{mice.impute.bygroup} performs groupwise imputation for arbitrary
imputation methods defined in \pkg{mice}.
}
\usage{
mice.impute.bygroup(y, ry, x, wy=NULL, group, imputationFunction, ...)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
    \item{y}{Incomplete data vector of length \code{n}}

  \item{ry}{
Vector of missing data pattern (\code{FALSE} -- missing,
\code{TRUE} -- observed)
}
\item{x}{Matrix (\code{n} x \code{p}) of complete
 covariates.}
 \item{wy}{Vector of \code{length(y)} indicating which entries should be imputed.}
\item{group}{
Name of grouping variable
}
  \item{imputationFunction}{
Imputation method for \pkg{mice}
}
  \item{\dots}{
More arguments to be passed to imputation function
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
Vector of imputed values
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

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
\dontrun{
#############################################################################
# EXAMPLE 1: Cluster-specific imputation for some variables
#############################################################################

library(mice)
data( data.ma01, package="miceadds")
dat <- data.ma01

# use sub-dataset
dat <- dat[ dat$idschool <=1006, ]
V <- ncol(dat)
# create initial predictor matrix and imputation methods
predictorMatrix <- matrix( 1, nrow=V, ncol=V)
diag(predictorMatrix) <- 0
rownames(predictorMatrix) <- colnames(predictorMatrix) <- colnames(dat)
predictorMatrix[, c("idstud", "studwgt","urban" ) ] <- 0
method <- rep("norm", V)
names(method) <- colnames(dat)

#** groupwise imputation of variable books
method["books"] <- "bygroup"
# specify name of the grouping variable ('idschool') and imputation method ('norm')
group <- list( "books"="idschool" )
imputationFunction <- list("books"="norm" )

#** conduct multiple imputation in mice
imp <- mice::mice( dat, method=method, predictorMatrix=predictorMatrix,
            m=1, maxit=1, group=group, imputationFunction=imputationFunction )

#############################################################################
# EXAMPLE 2: Group-wise multilevel imputation '2l.pan'
#############################################################################

library(mice)
data( data.ma01, package="miceadds" )
dat <- data.ma01

# select data
dat <- dat[, c("idschool","hisei","books","female") ]
V <- ncol(dat)
dat <- dat[ ! is.na( dat$books), ]
# define factor variable

dat$books <- as.factor(dat$books)
# create initial predictor matrix and imputation methods
predictorMatrix <- matrix( 0, nrow=V, ncol=V)
rownames(predictorMatrix) <- colnames(predictorMatrix) <- colnames(dat)
predictorMatrix["idschool", ] <- 0
predictorMatrix[ "hisei", "idschool" ] <- -2
predictorMatrix[ "hisei", c("books","female") ] <- 1
method <- rep("", V)
names(method) <- colnames(dat)
method["hisei"] <- "bygroup"
group <- list( "hisei"="female" )
imputationFunction <- list("hisei"="2l.pan" )

#** conduct multiple imputation in mice
imp <- mice::mice( dat, method=method, predictorMatrix=predictorMatrix,
            m=1, maxit=1, group=group, imputationFunction=imputationFunction )
str(imp)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
