%% File Name: round2.Rd
%% File Version: 0.20

\name{round2}
\alias{round2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
\R Utilities: Rounding DIN 1333 (Kaufmaennisches Runden)
}
\description{
This is a rounding function which rounds up for all numbers
according to the rule of 'kaufmaennisches Runden' (DIN 1333).
}
\usage{
round2(vec, digits=0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{vec}{
Numeric vector
}
  \item{digits}{
Number of digits after decimal for rounding
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
Vector with rounded values
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
# EXAMPLE 1:
#############################################################################

vec <- c( 1.5, 2.5, 3.5, 1.51,  1.49)
vec
round(vec)
round2(vec)
  ##   > vec
  ##   [1] 1.50 2.50 3.50 1.51 1.49
  ##   > round(vec)
  ##   [1] 2 2 4 2 1
  ##   > miceadds::round2(vec)
  ##   [1] 2 3 4 2 1

#############################################################################
# EXAMPLE 2:
#############################################################################

vec <- - c( 1.5, 2.5, 3.5, 1.51,  1.49)
vec
round(vec)
round2(vec)
  ##   > vec
  ##   [1] -1.50 -2.50 -3.50 -1.51 -1.49
  ##   > round(vec)
  ##   [1] -2 -2 -4 -2 -1
  ##   > miceadds::round2(vec)
  ##   [1] -2 -3 -4 -2 -1

#############################################################################
# EXAMPLE 3:
#############################################################################

vec <- c(8.4999999, 8.5, 8.501, 7.4999999, 7.5, 7.501 )
round(vec)
round2( vec )
round2( vec, digits=1)
round2( -vec )
  ##   > round(vec)
  ##   [1] 8 8 9 7 8 8
  ##   > miceadds::round2( vec )
  ##   [1] 8 9 9 7 8 8
  ##   > miceadds::round2( vec, digits=1)
  ##   [1] 8.5 8.5 8.5 7.5 7.5 7.5
  ##   > miceadds::round2( -vec )
  ##   [1] -8 -9 -9 -7 -8 -8
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%% \keyword{Rounding}
%% \keyword{R utilities}
%% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
