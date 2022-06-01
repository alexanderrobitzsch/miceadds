%% File Name: VariableNames2String.Rd
%% File Version: 0.09

\name{VariableNames2String}
\alias{VariableNames2String}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Stringing Variable Names with Line Breaks
}

\description{
Stringing variable names with line breaks.
}

\usage{
VariableNames2String(vars, breaks=80, sep=" ")
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{vars}{
Vector with variable names
}
  \item{breaks}{
Numeric value for line break of variable string
}
  \item{sep}{
Separator
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
String with line breaks
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
# EXAMPLE 1: Toy example
#############################################################################

data(data.ma01)
# extract variable names
vars <- colnames(data.ma01)
# convert into a long string with line breaks at column 25
vars2 <- miceadds::VariableNames2String(vars, breaks=25)
vars
  ##   [1] "idstud"   "idschool" "studwgt"  "math"     "read"     "migrant"
  ##   [7] "books"    "hisei"    "paredu"   "female"   "urban"
vars2
  ##  idstud idschool studwgt
  ##  math read migrant books
  ##  hisei paredu female
  ##  urban
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
