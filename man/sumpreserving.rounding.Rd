%% File Name: sumpreserving.rounding.Rd
%% File Version: 0.12

\name{sumpreserving.rounding}
\alias{sumpreserving.rounding}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Sum Preserving Rounding
}
\description{
This function implements sum preserving rounding. If the supplied
data is a matrix, then the sum of all row entries is preserved.
}
\usage{
sumpreserving.rounding(data, digits=0, preserve=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
Vector or data frame
}
  \item{digits}{
Number of digits to be round
}
  \item{preserve}{
Should the sum be preserved?
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

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
#############################################################################
# EXAMPLE 1:
#############################################################################

# define example data
data <- c( 1455, 1261, 1067, 970, 582, 97 )
data <- 100 * data / sum(data)

( x1 <- round( data ) )
sum(x1)
(x2 <- miceadds::sumpreserving.rounding( data ) )
sum(x2)

  ##   > ( x1 <- round( data ) )
  ##   [1] 27 23 20 18 11  2
  ##   > sum(x1)
  ##   [1] 101
  ##   > (x2 <- miceadds::sumpreserving.rounding( data ) )
  ##   [1] 27 23 20 18 10  2
  ##   > sum(x2)
  ##   [1] 100

#############################################################################
# EXAMPLE 2:
#############################################################################

# matrix input
data <- rbind( data, data )
( x1 <- round( data ) )
rowSums(x1)
(x2 <- miceadds::sumpreserving.rounding( data ) )
rowSums(x2)

#############################################################################
# EXAMPLE 3:
#############################################################################

x2 <- c( 1.4, 1.4, 1.2 )
round(x2)
sumpreserving.rounding(x2)
  ##   > round(x2)
  ##   [1] 1 1 1
  ##   > miceadds::sumpreserving.rounding(x2)
  ##   [1] 1 2 1
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%% \keyword{Rounding}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
