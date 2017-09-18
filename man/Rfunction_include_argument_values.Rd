%% File Name: Rfunction_include_argument_values.Rd
%% File Version: 0.01
%% File Last Change: 2017-08-23 19:21:18

\name{Rfunction_include_argument_values}
\alias{Rfunction_include_argument_values}
\alias{Rfunction_output_list_result_function}



%- Also NEED an '\alias' for EACH other topic documented here.

\title{
Utility Functions for Writing \R Functions 
}
\description{
Utility functions for writing \R functions.
}
\usage{
## include argument values in a function input
Rfunction_include_argument_values(string, maxlen = 80)

## assign objects to entries in a list 
Rfunction_output_list_result_function(string, mid = " <- res$")
}



%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{string}{
String
}
  \item{maxlen}{
Maximal string length for output
}
\item{mid}{
Middle term in the output
}
}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}

\value{
String
}

%\references{
%% ~put references to the literature/web site here ~
%}
%\author{
%%  ~~who you are~~
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
# EXAMPLE 1: Toy examples
#############################################################################

##**** extend missing arguments

string <- "
          mice.impute.2l.pls2(y, ry, x , type , pls.facs = pls.facs  ))
          "
cat( Rfunction_include_argument_values(string) )
  ##    mice.impute.2l.pls2( y=y, ry=ry, x=x, type=type, pls.facs=pls.facs ) 

##**** assignment to objects as entries in a list

string <- "
          list( vname=vname, p, type=type, data=data, levels_id )
          "
cat( Rfunction_output_list_result_function( string ) )
  ##  
  ##  vname <- res$vname
  ##  p <- res$p
  ##  type <- res$type
  ##  data <- res$data
  ##  levels_id <- res$levels_id
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
