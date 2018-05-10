%% File Name: grep.vec.Rd
%% File Version: 0.13

\name{grep.vec}
\alias{grep.vec}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
\R Utilities: Vector Based Version of \code{grep}
}
\description{
This function imitates the usage of \code{grep} but it is
extended to a vector argument.
}
\usage{
grep.vec(pattern.vec, x, operator="AND")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pattern.vec}{
String which should be looked for in vector \code{x}
}
\item{x}{
A character vector
}
\item{operator}{
An optional string. The default argument \code{"AND"} searches all
entries in \code{x} which contain all elements of \code{pattern.vec}.
If \code{operator} is different from the default, then the \code{"OR"}
logic applies, i.e. the functions searches for vector entries which
contain at least one of the strings in \code{pattern.vec}.
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
vec <- c("abcd" , "bcde" , "aedf" , "cdf" )
# search for entries in vec with contain 'a' and 'f'
#  -> operator = "AND"
grep.vec( pattern.vec=c("a","f") , x=vec )
  ##   $x
  ##   [1] "aedf"
  ##   $index.x
  ##   [1] 3

# search for entries in vec which contain 'a' or 'f'
grep.vec( pattern.vec=c("a","f") , x=vec , operator="OR")
  ##   $x
  ##   [1] "abcd" "aedf" "cdf"
  ##   $index.x
  ##   [1] 1 3 4
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{R utilities}
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
