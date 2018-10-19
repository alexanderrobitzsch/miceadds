%% File Name: miceadds-package.Rd
%% File Version: 1.714

\name{miceadds-package}
\alias{miceadds-package}
\alias{miceadds}
\docType{package}
\title{
Some Additional Multiple Imputation Functions, Especially for \pkg{mice}
}
\description{
    Contains functions for multiple imputation which
    complements existing functionality in \R. 
    In particular, several imputation methods for the 
    \pkg{mice} package (van Buuren & Groothuis-Oudshoorn, 2011,
    <doi:10.18637/jss.v045.i03>) are included.
    Main features of the \pkg{miceadds} package include    
    plausible value imputation (Mislevy, 1991, 
    <doi:10.1007/BF02294457>), multilevel imputation for
    variables at any level or with any number of hierarchical
    and non-hierarchical levels (Grund, Luedtke & Robitzsch,
    2018, <doi:10.1177/1094428117703686>; van Buuren, 2018, 
    Ch.7, <doi:10.1201/9780429492259>), imputation using 
    partial least squares (PLS) for high dimensional 
    predictors (Robitzsch, Pham & Yanagida, 2016), 
    and nested multiple imputation (Rubin, 2003, 
    <doi:10.1111/1467-9574.00217>).
}

\details{



\itemize{

\item The \pkg{miceadds} package contains some functionality for imputation of multilevel
data. The function \code{\link{mice.impute.ml.lmer}} is a general function for 
imputing multilevel data with hierarchical or cross-classified structures for
variables at an arbitrary level. This imputation method uses the \code{lme4::lmer} function
in the \pkg{lme4} package. The imputation method \code{\link{mice.impute.2lonly.function}}
conducts an imputation for a variable at a higher level for already defined imputation
methods in the \pkg{mice} package. Two-level imputation is available in several functions
in the \pkg{mice} package (\code{\link[mice:mice.impute.2l.pan]{mice::mice.impute.2l.pan}},
\code{\link[mice:mice.impute.2l.norm]{mice::mice.impute.2l.norm}}) 
as well in \pkg{micemd} and \pkg{hmi} packages. The \pkg{miceadds} package contains
additional imputation methods for two-level datasets: 
\code{\link{mice.impute.2l.continuous}} for normally distributed data,
\code{\link{mice.impute.2l.pmm}} for predictive mean matching in multilevel models
and \code{\link{mice.impute.2l.binary}} for binary data.   

\item In addition to the usual \code{mice} imputation function which employs
parallel chains, the function \code{\link{mice.1chain}} does multiple
imputation from a single chain.

\item Nested multiple imputation can be conducted with
\code{\link{mice.nmi}}. The function \code{\link{NMIcombine}} conducts 
statistical inference for nested multiply imputed datasets.

\item Imputation based on partial least squares regression is implemented
in \code{\link{mice.impute.pls}}.

\item Unidimensional plausible value imputation for latent variables (or
variables with measurement error) in the \pkg{mice} sequential imputation
framework can be applied by using the method
\code{\link{mice.impute.plausible.values}}.

\item The \pkg{miceadds} package also includes some functions \R utility functions
(e.g. \code{\link{write.pspp}}, \code{\link{ma.scale2}}).

\item Imputations for questionnaire items can be
accomplished by two-way imputation (\code{\link{tw.imputation}}).
}



}
\author{
Alexander Robitzsch, Simon Grund, Thorsten Henke \cr \cr
Maintainer: Alexander Robitzsch <robitzsch@ipn.uni-kiel.de> \cr \cr
}


\references{
Grund, S., Luedtke, O., & Robitzsch, A. (2018). Multiple imputation of multilevel
data in organizational research. \emph{Organizational Research Methods, 21}(1), 111-149.

Mislevy, R. J. (1991). Randomization-based inference about latent variables
from complex samples. \emph{Psychometrika, 56}, 177-196.

Robitzsch, A., Pham, G., & Yanagida, T. (2016). Fehlende Daten und Plausible Values. 
In S. Breit & C. Schreiner (Hrsg.). \emph{Large-Scale Assessment mit R: Methodische 
Grundlagen der oesterreichischen Bildungsstandardueberpruefung} (S. 259-293). Wien: facultas.

Rubin, D. B. (2003). Nested multiple imputation of NMES via partially
incompatible MCMC. \emph{Statistica Neerlandica, 57}(1), 3-18.

van Buuren, S. (2018). \emph{Flexible imputation of missing data}.
Boca Raton: CRC Press.

van Buuren, S., & Groothuis-Oudshoorn, K. (2011). \pkg{mice}: 
Multivariate imputation by chained equations in \R. 
\emph{Journal of Statistical Software, 45}(3), 1-67.
}



\seealso{
See other \R packages for conducting multiple imputation:
\pkg{mice}, \pkg{Amelia}, \pkg{pan}, \pkg{mi}, \pkg{norm},
\pkg{norm2}, \pkg{BaBooN}, \pkg{VIM}, ... \cr

Some links to internet sites related to missing data:

\emph{http://missingdata.lshtm.ac.uk/} \cr
\emph{http://www.stefvanbuuren.nl/mi/} \cr
\emph{http://www.bristol.ac.uk/cmm/software/realcom/} \cr
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
