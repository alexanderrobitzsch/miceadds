%% File Name: mice_inits.Rd
%% File Version: 0.08

\name{mice_inits}
\alias{mice_inits}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Arguments for \code{mice::mice} Function
}
\description{
Defines initial arguments of imputation method and
predictor matrix for \code{mice::mice} function.
}
\usage{
mice_inits(dat, ignore=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
Dataset
}
  \item{ignore}{
Vector of variables which should be ignored in imputation
}
}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}

\value{
List with entries
\item{method}{Imputation method}
\item{predictorMatrix}{Predictor matrix}
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

\seealso{
See \code{\link[mice:make.predictorMatrix]{mice::make.predictorMatrix}} and
\code{\link[mice:make.method]{mice::make.method}} for generating an initial predictor matrix
and a vector of imputation methods.
}

\examples{
\dontrun{
#############################################################################
# EXAMPLE 1: Inits for mice imputation
#############################################################################

data(data.ma04, package="miceadds")
dat <- data.ma04

res <- miceadds::mice_inits(dat, ignore=c("group") )
str(res)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
