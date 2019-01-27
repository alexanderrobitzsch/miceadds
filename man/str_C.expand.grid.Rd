%% File Name: str_C.expand.grid.Rd
%% File Version: 0.17

\name{str_C.expand.grid}
\alias{str_C.expand.grid}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
\R Utilities: String Paste Combined with \code{expand.grid}
}
\description{
String paste combined with \code{expand.grid}
}
\usage{
str_C.expand.grid(xlist, indices=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xlist}{
A list of character vectors
}
\item{indices}{Optional vector of indices to be permuted in \code{xlist}}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
A character vector
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
# EXAMPLE 1: Some toy examples
#############################################################################

x1 <- list( c("a","b" ), c("t", "r","v") )
str_C.expand.grid( x1 )
  ##   [1] "at" "bt" "ar" "br" "av" "bv"

x1 <- list( c("a","b" ), paste0("_", 1:4 ), c("t", "r","v") )
str_C.expand.grid( x1, indices=c(2,1,3) )
  ##    [1] "_1at" "_1bt" "_2at" "_2bt" "_3at" "_3bt" "_4at" "_4bt" "_1ar" "_1br"
  ##   [11] "_2ar" "_2br" "_3ar" "_3br" "_4ar" "_4br" "_1av" "_1bv" "_2av" "_2bv"
  ##   [21] "_3av" "_3bv" "_4av" "_4bv"

\dontrun{
##***************************************************************************
## The function 'str_C.expand.grid' is currently defined as
function( xlist, indices=NULL )
{
     xeg <- expand.grid( xlist)
     if ( ! is.null(indices) ){    xeg <- xeg[, indices ]}
     apply( xeg, 1, FUN=function(vv){ paste0( vv, collapse="") } )
}
##***************************************************************************
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%% \keyword{Utility function}
%% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
