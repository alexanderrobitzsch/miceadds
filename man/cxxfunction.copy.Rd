%% File Name: cxxfunction.copy.Rd
%% File Version: 0.24

\name{cxxfunction.copy}
\alias{cxxfunction.copy}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
\R Utilities: Copy of an \pkg{Rcpp} File
}
\description{
Copies the \pkg{Rcpp} function into the working directory.
}
\usage{
cxxfunction.copy(cppfct, name)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cppfct}{
Rcpp function
}
  \item{name}{
Name of the output Rcpp function to be generated
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
\references{
Eddelbuettel, D. & Francois, R. (2011). \pkg{Rcpp}: Seamless \R and \pkg{C++} integration.
\emph{Journal of Statistical Software, 40}(8), 1-18.
\doi{10.18637/jss.v040.i08}
}



%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~


\seealso{
\code{\link[inline:cxxfunction]{inline::cxxfunction}}
}
\examples{
\dontrun{
#############################################################################
# EXAMPLE 1: Rcpp code logistic distribution
#############################################################################

library(Rcpp)
library(inline)

# define Rcpp file
code1 <- "
    // input array A
    Rcpp::NumericMatrix AA(A);
    // Rcpp::IntegerVector dimAA(dimA);
    int nrows=AA.nrow();
    int ncolumns=AA.ncol();
    Rcpp::NumericMatrix Alogis(nrows,ncolumns)  ;
    // compute logistic distribution
    for (int ii=0; ii<nrows; ii++){
        Rcpp::NumericVector h1=AA.row(ii) ;
        Rcpp::NumericVector res=plogis( h1 ) ;
        for (int jj=0;jj<ncolumns;jj++){
            Alogis(ii,jj)=res[jj] ;
                        }
                    }
    return( wrap(Alogis) );
    "
# compile Rcpp code
fct_rcpp <- inline::cxxfunction( signature( A="matrix"), code1,
              plugin="Rcpp", verbose=TRUE )
# copy function and save it as object 'calclogis'
name <- "calclogis"  # name of the function
cxxfunction.copy( cppfct=fct_rcpp, name=name )
# function is available as object named as name
Reval( paste0( name, " <- fct_rcpp " ) )
# test function
m1 <- outer( seq( -2, 2, len=10 ), seq(-1.5,1.5,len=4) )
calclogis(m1)
    }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%% \keyword{R utilities}
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
