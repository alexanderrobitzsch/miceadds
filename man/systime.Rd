%% File Name: systime.Rd
%% File Version: 0.15

\name{systime}
\alias{systime}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
\R Utilities: Various Strings Representing System Time
}
\description{
This function generates system time strings in several formats.
}
\usage{
systime()
}
%- maybe also 'usage' for other objects documented here.
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
A vector with entries of system time (see Examples).
}
%\references{
%% ~put references to the literature/web site here ~
%}

%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
#############################################################################
# EXAMPLE 1: Output of systime
#############################################################################

systime()
  ##
  ##  > miceadds::systime()
  ##  [1] "2016-02-29 10:25:44"
  ##  [2] "2016-02-29"
  ##  [3] "20160229"
  ##  [4] "2016-02-29_1025"
  ##  [5] "2016-02-29_1000"
  ##  [6] "20160229_102544"
  ##  [7] "20160229102544"
  ##  [8] "IPNERZW-C014_20160229102544"
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%% \keyword{R utilities}
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
