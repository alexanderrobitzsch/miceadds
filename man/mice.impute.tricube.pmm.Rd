%% File Name: mice.impute.tricube.pmm.Rd
%% File Version: 0.17

\name{mice.impute.tricube.pmm}
\alias{mice.impute.tricube.pmm}
\alias{mice.impute.tricube.pmm2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Imputation by Tricube Predictive Mean Matching
}
\description{
This function performs tricube predictive mean matching (see
\code{{Hmisc::aregImpute}})
in which donors are weighted according to distances of predicted values.
}
\usage{
mice.impute.tricube.pmm(y, ry, x, tricube.pmm.scale = 0.2, tricube.boot = FALSE, ...)

mice.impute.tricube.pmm2(y, ry, x, tricube.pmm.scale = 0.2, tricube.boot = FALSE, ...)
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
  \item{tricube.pmm.scale}{
A scaling factor for traicube matching. The default is 0.2.
}
  \item{tricube.boot}{
A logical indicating whether tricube matching should be performed
using a bootstrap sample
}
  \item{\dots}{
Further arguments to be passed
}
}


%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}


\value{
A vector of length \code{nmis=sum(!ry)} with imputed values.
}

%\references{
%% ~put references to the literature/web site here ~
%}



\note{
The imputation method \code{tricube.pmm2} is usually somewhat faster
than \code{tricube.pmm}.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{{Hmisc::aregImpute}}
}
%% ~~objects to See Also as \code{\link{help}}, ~~~

\examples{
\dontrun{
#############################################################################
# EXAMPLE 1: Tricube predictive mean matching for nhanes data
#############################################################################

library(mice)
data(nhanes, package="mice")
set.seed(9090)

#*** Model 1: Use default of tricube predictive mean matching
varnames <- colnames(nhanes) 
VV <- length(varnames)
imputationMethod <- rep("tricube.pmm2" , VV )
names(imputationMethod) <- varnames
# imputation with mice
imp.mi1 <- mice::mice( nhanes , m=5 , maxit=4 , imputationMethod= imputationMethod )

#*** Model 2: use item-specific imputation methods
iM2 <- imputationMethod
iM2["bmi"] <- "pmm6"
# use tricube.pmm2 for hyp and chl
# select different scale parameters for these variables
tricube.pmm.scale1 <- list( "hyp" = .15 , "chl" = .30 )
imp.mi2 <- miceadds::mice.1chain( nhanes , burnin=5 , iter=20 , Nimp=4 ,
    imputationMethod= iM2 , tricube.pmm.scale=tricube.pmm.scale1  )
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{mice imputation method}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
