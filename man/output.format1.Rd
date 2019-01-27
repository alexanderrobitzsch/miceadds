%% File Name: output.format1.Rd
%% File Version: 0.16

\name{output.format1}
\alias{output.format1}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
\R Utilities: Formatting R Output on the \R Console
}
\description{
This function does some formatting of output.
}
\usage{
output.format1(stringtype, label, rep.N=1,stringlength=70)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{stringtype}{
Type of string for display, e.g. \code{"*"}, \code{"-"}, ...
}
\item{label}{
Some comment which should be displayed at the console
}
 \item{rep.N}{
Number of lines which shall be left blank
}
\item{stringlength}{
Length of vector with \code{label}
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
Generates a string output at the \R console
}
%\references{
%% ~put references to the literature/web site here ~
%}

%\note{
%TO DO: include example here
%}

\examples{
output.format1(stringtype="*'", label="HELLO WORLD", stringlength=20)
##   *'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'
##   HELLO WORLD
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%% \keyword{R utilities}
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
