%% File Name: mice.impute.pls.Rd
%% File Version: 2.392

\name{mice.impute.pls}
\alias{mice.impute.pls}
\alias{mice.impute.2l.pls2}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Imputation using Partial Least Squares for Dimension Reduction
}
\description{
This function imputes a variable with missing values using
PLS regression (Mevik & Wehrens, 2007) for a dimension
reduction of the predictor space.
}
\usage{
mice.impute.pls(y, ry, x, type, pls.facs=NULL,
   pls.impMethod="pmm", donors=5, pls.impMethodArgs=NULL, pls.print.progress=TRUE,
   imputationWeights=rep(1, length(y)), pcamaxcols=1E+09,
   min.int.cor=0, min.all.cor=0, N.largest=0, pls.title=NULL, print.dims=TRUE,
   pls.maxcols=5000, use_boot=FALSE, envir_pos=NULL, extract_data=TRUE,
   remove_lindep=TRUE, derived_vars=NULL, ...)

mice.impute.2l.pls2(y, ry, x, type, pls.facs=NULL, pls.impMethod="pmm",
   pls.print.progress=TRUE, imputationWeights=rep(1, length(y)), pcamaxcols=1E+09,
   tricube.pmm.scale=NULL, min.int.cor=0, min.all.cor=0, N.largest=0,
   pls.title=NULL, print.dims=TRUE, pls.maxcols=5000, envir_pos=parent.frame(), ...)
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

  \item{type}{
\code{type=1} -- variable is used as a predictor,

\code{type=4} -- create interactions with the specified
variable with all other predictors,

\code{type=5} -- create a quadratic term of the specified variable

\code{type=6} -- if some interactions are specified, ignore
the variables with entry \code{6} when creating interactions

\code{type=-2} -- specification of a cluster variable. The cluster mean
of the outcome \code{y} (when eliminating the subject under study)
is included as a further predictor in the imputation.

}
  \item{pls.facs}{
Number of factors used in PLS regression. This argument can also
be specified as a list defining different numbers of factors
for all variables to be imputed.
}
  \item{pls.impMethod}{Imputation method used for in PLS estimation.
        Any imputation method can be used except if \code{imputationWeights}
  is provided. Imputation weights are available for \code{norm} and \code{pmm}.
  Categorical variables can be imputed with the method \code{catpmm}
 (see \code{\link{mice.impute.catpmm}}). For the method \code{catpmm},
 multivariate PLS regression is employed for dummy-coded categories of
 the outcome variable. The method \code{xplsfacs} creates only PLS factors
 of the regression model.
}
\item{donors}{Number of donors if predictive mean matching is used
(\code{pls.impMethod="pmm"}).}
\item{pls.impMethodArgs}{Arguments for imputation method
\code{pls.impMethod}.}
  \item{pls.print.progress}{
Print progress during PLS regression.
}
  \item{imputationWeights}{
Vector of sample weights to be used in imputation models.
}
  \item{pcamaxcols}{
Amount of variance explained by principal components (must be a number between
0 and 1) or number of factors used in PCA (an integer larger than 1).
}
  \item{min.int.cor}{
Minimum absolute correlation for an interaction of two predictors
to be included in the PLS regression model
}
  \item{min.all.cor}{
Minimum absolute correlation for inclusion in the PLS regression
model.
}
  \item{N.largest}{
Number of variable to be included which do have the largest
absolute correlations.
}
  \item{pls.title}{
Title for progress print in console output.
}
  \item{print.dims}{
An optional logical indicating whether dimensions of
inputs should be printed.
}
\item{pls.maxcols}{
Maximum number of interactions to be created.
}
\item{use_boot}{Logical whether Bayesian bootstrap should be used for
drawing regression parameters}
\item{envir_pos}{Position of the environment from which the data should
be extracted.}
\item{extract_data}{Logical indicating whether input data should be extracted
from parent environment within \code{\link[mice:mice]{mice::mice}}  routine}
\item{remove_lindep}{Logical indicating whether linear dependencies should
be automatically detected and some predictors are removed}
\item{derived_vars}{Optional list containing formulas with derived variables for
inclusion in PLS dimension reduction}
  \item{\dots}{
Further arguments to be passed.
}
\item{tricube.pmm.scale}{Scale factor for tricube PMM imputation.}
}

%\details{
%The function \code{mice.impute.2l.pls2} uses \code{\link{kernelpls.fit2}}
%instead of \code{kernelpls.fit} from the \pkg{pls} package and is
%a bit faster.
%}

\value{
A vector of length \code{nmis=sum(!ry)} with imputations
if \code{pls.impMethod !="xplsfacs"}. In case of
\code{pls.impMethod=="xplsfacs"} a matrix with PLS factors
is computed.
}

\references{
Mevik, B. H., & Wehrens, R. (2007). The \pkg{pls} package:
Principal component and partial least squares regression in \R.
\emph{Journal of Statistical Software, 18}, 1-24.
\doi{10.18637/jss.v018.i02}
}


\note{
The \code{mice.impute.2l.pls2} function is just included for reasons of
backward compatibility to former \pkg{miceadds} versions.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}

\examples{
\dontrun{
#############################################################################
# EXAMPLE 1: PLS imputation method for internet data
#############################################################################

data(data.internet)
dat <- data.internet

# specify predictor matrix
predictorMatrix <- matrix( 1, ncol(dat), ncol(dat) )
rownames(predictorMatrix) <- colnames(predictorMatrix) <- colnames(dat)
diag( predictorMatrix) <- 0

# use PLS imputation method for all variables
impMethod <- rep( "pls", ncol(dat) )
names(impMethod) <- colnames(dat)

# define predictors for interactions (entries with type 4 in predictorMatrix)
predictorMatrix[c("IN1","IN15","IN16"),c("IN1","IN3","IN10","IN13")] <- 4
# define predictors which should appear as linear and quadratic terms (type 5)
predictorMatrix[c("IN1","IN8","IN9","IN10","IN11"),c("IN1","IN2","IN7","IN5")] <- 5

# use 9 PLS factors for all variables
pls.facs <- as.list( rep( 9, length(impMethod) ) )
names(pls.facs) <- names(impMethod)
pls.facs$IN1 <- 15   # use 15 PLS factors for variable IN1

# choose norm or pmm imputation method
pls.impMethod <- as.list( rep("norm", length(impMethod) ) )
names(pls.impMethod) <- names(impMethod)
pls.impMethod[ c("IN1","IN6")] <- "pmm"

# some arguments for imputation method
pls.impMethodArgs <- list( "IN1"=list( "donors"=10 ),
                           "IN2"=list( "ridge2"=1E-4 ) )

# Model 1: Three parallel chains
imp1 <- mice::mice(data=dat, method=impMethod,
     m=3, maxit=5, predictorMatrix=predictorMatrix,
     pls.facs=pls.facs, # number of PLS factors
     pls.impMethod=pls.impMethod,  # Imputation Method in PLS imputation
     pls.impMethodArgs=pls.impMethodArgs, # arguments for imputation method
     pls.print.progress=TRUE, ls.meth="ridge" )
summary(imp1)

# Model 2: One long chain
imp2 <- miceadds::mice.1chain(data=dat, method=impMethod,
     burnin=10, iter=21, Nimp=3, predictorMatrix=predictorMatrix,
     pls.facs=pls.facs, pls.impMethod=pls.impMethod,
     pls.impMethodArgs=pls.impMethodArgs, ls.meth="ridge" )
summary(imp2)

# Model 3: inclusion of additional derived variables

# define derived variables for IN1
derived_vars <- list( "IN1"=~I( ifelse( IN2>IN3, IN2, IN3 ) ) + I( sin(IN2) ) )

imp3 <- miceadds::mice.1chain(data=dat, method=impMethod, derived_vars=derived_vars,
     burnin=10, iter=21, Nimp=3, predictorMatrix=predictorMatrix,
     pls.facs=pls.facs, pls.impMethod=pls.impMethod,
     pls.impMethodArgs=pls.impMethodArgs, ls.meth="ridge" )
summary(imp3)

#*** example for using imputation function at the level of a variable

# extract first imputed dataset
imp1 <- mice::complete(imp1, action=1)
data_imp1[ is.na(dat$IN1), "IN1" ] <- NA

# define variables
y <- data_imp1$IN1
x <- data_imp1[, -1 ]
ry <- ! is.na(y)
cn <- colnames(dat)
p <- ncol(dat)
type <- rep(1,p)
names(type) <- cn
type["IN1"] <- 0

# imputation of variable 'IN1'
imp0 <- miceadds::mice.impute.pls(y=y, x=x, ry=ry, type=type, pls.facs=10, pls.impMethod="norm",
             ls.meth="ridge", extract_data=FALSE )
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%% \keyword{Partial least squares regression (PLS)}
%% \keyword{Dimension reduction}
%% \keyword{mice imputation method}
