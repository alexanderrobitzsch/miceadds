%% File Name: NestedImputationList.Rd
%% File Version: 0.17

\name{NestedImputationList}
\alias{NestedImputationList}
\alias{MIcombine.NestedImputationResultList}
\alias{print.NestedImputationList}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Functions for Analysis of Nested Multiply Imputed Datasets
}


\description{
The function \code{NestedImputationList} takes a list of lists of datasets
and converts this into an object of class \code{NestedImputationList}.

Statistical models can be estimated with the function
\code{\link{with.NestedImputationList}}.

The \code{\link[mitools:MIcombine]{mitools::MIcombine}}
method can be used for objects of class
\code{NestedImputationResultList} which are the output of
\code{\link{with.NestedImputationList}}.
}

\usage{
NestedImputationList( datasets )

\method{print}{NestedImputationList}(x, ...)

\method{MIcombine}{NestedImputationResultList}(results, ...)
}


%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{datasets}{
List of lists of datasets which are created by nested multiple imputation.
}
\item{x}{Object of class \code{NestedImputationResultsList}}
\item{results}{Object of class \code{NestedImputationResultsList}}
\item{\dots}{Further arguments to be passed.}
}


%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
Function \code{NestedImputationList}: Object of class \code{NestedImputationList}.

Function \code{MIcombine.NestedImputationList}:
Object of class \code{mipo.nmi}.
}


%\references{
%% ~put references to the literature/web site here ~
%}


%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{with.NestedImputationList}},
\code{\link{within.NestedImputationList}},
\code{\link{pool.mids.nmi}},
\code{\link{NMIcombine}}
}


\examples{
\dontrun{
#############################################################################
# EXAMPLE 1: Nested multiple imputation and conversion into an object of class
#            NestedImputationList
#############################################################################

library(BIFIEsurvey)
data(data.timss2, package="BIFIEsurvey" )
datlist <- data.timss2

# remove first four variables
M <- length(datlist)
for (ll in 1:M){
    datlist[[ll]] <- datlist[[ll]][, -c(1:4) ]
                }

# nested multiple imputation using mice
imp1 <- miceadds::mice.nmi( datlist,  m=3, maxit=2 )
summary(imp1)

# create object of class NestedImputationList
datlist1 <- miceadds::mids2datlist( imp1 )
datlist1 <- miceadds::NestedImputationList( datlist1 )

# estimate linear model using with
res1 <- with( datlist1, stats::lm( ASMMAT ~ female + migrant ) )
# pool results
mres1 <- mitools::MIcombine( res1 )
summary(mres1)
coef(mres1)
vcov(mres1)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%% \keyword{Nested multiple imputation}
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
