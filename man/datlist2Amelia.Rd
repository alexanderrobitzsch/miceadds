%% File Name: datlist2Amelia.Rd
%% File Version: 0.06

\name{datlist2Amelia}
\alias{datlist2Amelia}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Converting an Object of class \code{amelia}
}
\description{
This function converts a list of multiply imputed data sets
to an object of class \code{amelia}.
}
%% mids-class {mice}

\usage{
datlist2Amelia(datlist)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{datlist}{
List of multiply imputed data sets or an object of class \code{mids}
}
}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}


\value{
An object of class \code{amelia}
}

%\references{
%% ~put references to the literature/web site here ~
%}


%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%See \code{\link[mice:as.mids]{mice::as.mids}} for converting
%}

\examples{
\dontrun{
#############################################################################
# EXAMPLE 1: Imputation of NHANES data using mice package
#############################################################################

library(mice)
library(Amelia)

data(nhanes,package="mice")
set.seed(566)  # fix random seed

# imputation with mice
imp <- mice::mice(nhanes, m=7)

# conversion to amelia object
amp <- miceadds::datlist2Amelia(datlist=imp)
str(amp)
plot(amp)
print(amp)
summary(amp)
}
}


%% \keyword{mids}
%% \keyword{mice utility function}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
