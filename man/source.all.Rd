%% File Name: source.all.Rd
%% File Version: 0.13

\name{source.all}
\alias{source.all}
\alias{source.Rcpp.all}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
\R Utilities: Source all R or \pkg{Rcpp} Files within a Directory
}

\description{
The function \code{source.all} sources all \R files within a specified directory and
is based on \code{\link[base:source]{base::source}}.

The function \code{source.Rcpp.all} sources all \pkg{Rcpp} files within a specified directory
and is based on \code{\link[Rcpp:sourceCpp]{Rcpp::sourceCpp}}.
}

\usage{
source.all( path, grepstring="\\\\.R",  print.source=TRUE )

source.Rcpp.all( path, file_names=NULL, ext="\\\\.cpp", excl="RcppExports" )
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
\item{file_names}{Optional vector of (parts of) file names}
\item{ext}{File extension for \pkg{Rcpp} files}
\item{excl}{String indicating which files should be omitted from sourcing}
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
