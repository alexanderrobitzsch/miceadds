%% File Name: source.all.Rd
%% File Version: 0.08

\name{source.all}
\alias{source.all}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
\R Utilities: Source All R Files Within a Directory
}
\description{
Sources all \R files within a specified directory.
}
\usage{
source.all( path , grepstring="\\\\.R",  print.source=TRUE )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{path}{
Path where the files are located
}
\item{grepstring}{
Which strings should be looked for? 
\code{grepstring} can also be a vector.
}
\item{print.source}{
An optional logical whether the source process printed on the console?
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
Alexander Robitzsch
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
# define path
path <- "c:/myfiles/"
# source all files containing the string 'Rex'
source.all( path , "Rex" )}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{R utilities}
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
