%% File Name: source.all.Rd
%% File Version: 0.28

\name{source.all}
\alias{source.all}
\alias{source.Rcpp.all}
\alias{rcpp_create_header_file}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
\R Utilities: Source all R or \pkg{Rcpp} Files within a Directory
}

\description{
The function \code{source.all} sources all \R files within a specified directory and
is based on \code{\link[base:source]{base::source}}.

The function \code{source.Rcpp.all} sources all \pkg{Rcpp} files within a specified directory
and is based on \code{\link[Rcpp:sourceCpp]{Rcpp::sourceCpp}}.

The function \code{rcpp_create_header_file} creates a cpp header file for a \pkg{Rcpp}
file.
}

\usage{
source.all( path, grepstring="\\\\.R",  print.source=TRUE, file_sep="__"  )

source.Rcpp.all( path, file_names=NULL, ext="\\\\.cpp", excl="RcppExports",
   remove_temp_file=FALSE )

rcpp_create_header_file(file_name, pack=NULL, path=getwd() )
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
\item{file_sep}{String at which file name should be split for looking
for most recent files}
\item{file_names}{Optional vector of (parts of) file names}
\item{ext}{File extension for \pkg{Rcpp} files}
\item{excl}{String indicating which files should be omitted from sourcing}
\item{remove_temp_file}{Logical indicating whether temporary \pkg{Rcpp}
files should be removed.}
\item{file_name}{File name}
\item{pack}{Optional string for package}
}
\details{
For loading header files, the line \code{// [include_header_file]} has to be included
before loading the header file using a line of the form
\code{#include "my_function.h"}.
}
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
source.all( path, "Rex" )
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%% \keyword{R utilities}
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
