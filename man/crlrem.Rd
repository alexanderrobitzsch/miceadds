%% File Name: crlrem.Rd
%% File Version: 0.09

\name{crlrem}
\alias{crlrem}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
\R Utilities: Removing CF Line Endings
}
\description{
This function removes CF line endings from a text file
and writes the processed file in the working directory.
}
\usage{
crlrem( filename1, filename2 )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename1}{
Name of the original file (possibly with CF line endings)
}
\item{filename2}{
Name of the processed file (without CF line endings)
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
\author{
This is code by Dirk Eddelbuettel copied from
\url{https://stat.ethz.ch/pipermail/r-devel/2010-September/058480.html}
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
\dontrun{
filename1 <- "rm.arraymult__0.02.cpp"
filename2 <- "rm.arraymult__0.03.cpp"
crlrem( filename1, filename2 )}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%% \keyword{R utilities}
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
