%% File Name: miceadds-package.Rd
%% File Version: 1.58

\name{miceadds-package}
\alias{miceadds-package}
\alias{miceadds}
\docType{package}
\title{
Some Additional Multiple Imputation Functions, Especially for \pkg{mice}
}
\description{
    Contains some auxiliary functions for multiple
    imputation which complements existing functionality
    in \R.
    In addition to some utility functions, main features
    include plausible value imputation, multilevel
    imputation functions (arbitrary number of levels,
    hierarchical and non-hierarchical datasets),
    imputation using partial least squares (PLS) for
    high dimensional predictors, and nested multiple
    imputation.
}

\details{



\itemize{

\item In addition to the usual \code{mice} imputation function which employs
parallel chains, the function \code{\link{mice.1chain}} does multiple
imputation from a single chain.

\item Nested multiple imputation can be conducted with
\code{\link{mice.nmi}}.

\item Imputation based on partial least squares regression is implemented
in \code{\link{mice.impute.pls}}.

\item Unidimensional plausible value imputation for latent variables (or
variables with measurement error) in the \pkg{mice} sequential imputation
framework can be applied by using the method
\code{\link{mice.impute.plausible.values}}.

\item Imputations for questionnaire items can be
accomplished by two-way imputation (\code{\link{tw.imputation}}).

\item The \pkg{miceadds} package also includes some functions \R utility functions
(e.g. \code{\link{write.pspp}}, \code{\link{ma.scale2}}).
}



}
\author{
Alexander Robitzsch, Simon Grund, Thorsten Henke \cr \cr
Maintainer: Alexander Robitzsch <robitzsch@ipn.uni-kiel.de> \cr \cr
}


%\references{
%}



\seealso{
See other \R packages for conducting multiple imputation:
\pkg{mice}, \pkg{Amelia}, \pkg{pan}, \pkg{mi}, \pkg{norm},
\pkg{norm2}, \pkg{BaBooN}, \pkg{VIM}, ... \cr

Some links to internet sites related to missing data:

\url{http://missingdata.lshtm.ac.uk/} \cr
\url{http://www.stefvanbuuren.nl/mi/} \cr
\url{http://www.bristol.ac.uk/cmm/software/realcom/} \cr
}

\examples{
##
##   ::'''''''''''''''''''''''''''''''''::
##   :: miceadds 0.11-69 (2013-12-01)   ::
##   ::'''''''''''''''''''''''''''''''''::
##
##  ----------------------- mice at work ---------------------------------
##
##                         (\-.
##                         / _`> .---------.
##                 _)     / _)=  |'-------'|
##                (      / _/    |O   O   o|
##                 `-.__(___)_   | o O . o |
##                               `---------'
##
##                                          oo__
##                                         <;___)------
##                                    oo__   " "
##                                   <;___)------     oo__
##                                     " "           <;___)------
##                                                     " "
}

\keyword{package}
