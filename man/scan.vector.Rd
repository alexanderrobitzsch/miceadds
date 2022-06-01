%% File Name: scan.vector.Rd
%% File Version: 0.23

\name{scan.vec}
\alias{scan.vec}
\alias{scan.vector}
\alias{scan0}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
\R Utilities: Scan a Character Vector
}
\description{
The function \code{scan.vec} function splits a string into a character
vector. The function \code{scan0} is the
\code{\link[base:scan]{base::scan}} function using the default
\code{what="character"}.
}
\usage{
scan.vec(vec)
scan.vector(vec)

scan0(file="", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{vec}{
A string which should be split according to blanks
}
\item{file}{
File to be scanned. See \code{\link[base:scan]{base::scan}}.
}
\item{\dots}{
Further arguments to be passed.
See \code{\link[base:scan]{base::scan}}.
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
%\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
%}
%\references{
%% ~put references to the literature/web site here ~
%}

%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[base:scan]{base::scan}}
}

\examples{
#############################################################################
# EXAMPLE 1: Example scan.vec | reading a string
#############################################################################


vars <- miceadds::scan.vector( "urbgrad \\n  groesse  \\t  Nausg  grpgroesse   privat  ")
vars
  ## [1] "urbgrad"    "groesse"    "Nausg"      "grpgroesse"
  ## [6] "privat"

## the next lines are only commented out to fulfill CRAN checks
## vars2 <- miceadds::scan0()
##     female urbgrad  groesse  Nausg    grpgroesse   privat
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%% \keyword{R utilities}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
