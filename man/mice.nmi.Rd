%% File Name: mice.nmi.Rd
%% File Version: 0.21

\name{mice.nmi}
\alias{mice.nmi}
\alias{summary.mids.nmi}
\alias{print.mids.nmi}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Nested Multiple Imputation
}
\description{
Performs nested multiple imputation (Rubin, 2003) for the functions
\code{\link[mice:mice]{mice::mice}} and \code{\link{mice.1chain}}.
The function \code{mice.nmi} generates an object of class \code{mids.nmi}.
}
\usage{
mice.nmi(datlist, type="mice", ...)

\method{summary}{mids.nmi}(object, ...)

\method{print}{mids.nmi}(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{datlist}{
List of datasets for which nested multiple imputation should be applied
}
  \item{type}{
Imputation model: \code{type="mice"} for \code{\link[mice:mice]{mice::mice}}  or
\code{type="mice.1chain"} for \code{\link{mice.1chain}}.
}
  \item{\dots}{
Arguments to be passed to \code{\link[mice:mice]{mice::mice}}  or
\code{\link{mice.1chain}}.
}
\item{object}{Object of class \code{mids.nmi}.}
\item{x}{Object of class \code{mids.nmi}.}
}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}


\value{
Object of class \code{mids.nmi} with entries
  \item{imp}{List of nested multiply imputed datasets whose entries
  are of class \code{mids} or \code{mids.1chain}.}
  \item{Nimp}{Number of between and within imputations.}
}
\references{
Rubin, D. B. (2003). Nested multiple imputation of NMES via partially
incompatible MCMC. \emph{Statistica Neerlandica}, \bold{57(1)}, 3-18.
}



%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
For imputation models see \code{\link[mice:mice]{mice::mice}}
and \code{\link{mice.1chain}}.

Functions for analyses of nested multiply imputed datasets:
\code{\link{complete.mids.nmi}}, \code{\link{with.mids.nmi}},
\code{\link{pool.mids.nmi}}
}


\examples{
\dontrun{
#############################################################################
# EXAMPLE 1: Nested multiple imputation for TIMSS data
#############################################################################

library(BIFIEsurvey)
data(data.timss2, package="BIFIEsurvey" )
datlist <- data.timss2
   # list of 5 datasets containing 5 plausible values

#** define imputation method and predictor matrix
data <- datlist[[1]]
V <- ncol(data)
# variables
vars <- colnames(data)
# variables not used for imputation
vars_unused <- miceadds::scan.vec("IDSTUD TOTWGT  JKZONE  JKREP" )

#- define imputation method
impMethod <- rep("norm", V )
names(impMethod) <- vars
impMethod[ vars_unused ] <- ""

#- define predictor matrix
predM <- matrix( 1, V, V )
colnames(predM) <- rownames(predM) <- vars
diag(predM) <- 0
predM[, vars_unused ] <- 0

#***************
# (1) nested multiple imputation using mice
imp1 <- miceadds::mice.nmi( datlist, method=impMethod, predictorMatrix=predM,
                m=4, maxit=3 )
summary(imp1)

#***************
# (2) nested multiple imputation using mice.1chain
imp2 <- miceadds::mice.nmi( datlist, method=impMethod, predictorMatrix=predM,
            Nimp=4, burnin=10,iter=22, type="mice.1chain")
summary(imp2)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Nested multiple imputation}
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
