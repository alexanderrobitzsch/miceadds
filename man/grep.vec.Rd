%% File Name: grep.vec.Rd
%% File Version: 0.24

\name{grep.vec}
\alias{grep.vec}
\alias{grep_leading}
\alias{grepvec_leading}
\alias{grepvec}


%- Also NEED an '\alias' for EACH other topic documented here.
\title{
\R Utilities: Vector Based Versions of \code{grep}
}
\description{
These functions slightly extend the usage of \code{grep} but it is
extended to a vector argument.
}
\usage{
grep.vec(pattern.vec, x, operator="AND", ...)

grepvec( pattern.vec, x, operator="AND", value=FALSE, ...)

grep_leading( pattern, x, value=FALSE )

grepvec_leading( patternvec, x, value=FALSE )
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
\item{pattern}{String}
\item{patternvec}{Vector of strings}
\item{value}{Logical indicating whether indices or values are requested}
\item{\dots}{Arguments to be passed to \code{\link[base:grep]{base::grep}}
(e.g., \code{fixed=TRUE})}
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
# EXAMPLE 1: Toy example
#############################################################################

vec <- c("abcd", "bcde", "aedf", "cdf" )
# search for entries in vec with contain 'a' and 'f'
#  -> operator="AND"
grep.vec( pattern.vec=c("a","f"), x=vec )
  ##   $x
  ##   [1] "aedf"
  ##   $index.x
  ##   [1] 3

grepvec( pattern.vec=c("a","f"), x=vec, value=TRUE)
grepvec( pattern.vec=c("a","f"), x=vec, value=FALSE)

# search for entries in vec which contain 'a' or 'f'
grep.vec( pattern.vec=c("a","f"), x=vec, operator="OR")
  ##   $x
  ##   [1] "abcd" "aedf" "cdf"
  ##   $index.x
  ##   [1] 1 3 4
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%% \keyword{R utilities}
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
