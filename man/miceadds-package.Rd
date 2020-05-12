%% File Name: miceadds-package.Rd
%% File Version: 1.727

\name{miceadds-package}
\alias{miceadds-package}
\alias{miceadds}
\docType{package}


\title{
\packageTitle{miceadds}
}

\description{
\packageDescription{miceadds}
}

\author{
\packageAuthor{miceadds}

Maintainer: \packageMaintainer{miceadds}
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

\item Substantive model compatible multiple imputation using fully conditional
specification can be conducted with \code{\link{mice.impute.smcfcs}}.

\item The function \code{\link{syn_mice}} allows the generation of
synthetic datasets with imputation methods for \pkg{mice}. It has
similar functionality as the \pkg{synthpop} package (Nowok, Raab, & Dibben, 2016).
The function \code{\link{mice.impute.synthpop}} allows the usage of
\pkg{synthpop} synthesization methods in \pkg{mice}, while
\code{\link{syn.mice}} allows the usage of
\pkg{mice} imputation methods in \pkg{synthpop}.

\item The method \code{\link{mice.impute.simputation}} is a wrapper function
to imputation methods in the \pkg{simputation} package. 
The methods \code{\link{mice.impute.imputeR.lmFun}} and
\code{\link{mice.impute.imputeR.cFun}} are wrapper functions
to imputation methods in the \pkg{imputeR} package.

\item The \pkg{miceadds} package also includes some functions \R utility functions
(e.g. \code{\link{write.pspp}}, \code{\link{ma.scale2}}).

\item Imputations for questionnaire items can be
accomplished by two-way imputation (\code{\link{tw.imputation}}).
}

}



\references{
Bartlett, J. W., Seaman, S. R., White, I. R., Carpenter, J. R., & Alzheimer's Disease
Neuroimaging Initiative (2015). Multiple imputation of covariates by fully
conditional specification: Accommodating the substantive model.
\emph{Statistical Methods in Medical Research, 24}(4), 462-487.
\doi{10.1177/0962280214521348}

Grund, S., Luedtke, O., & Robitzsch, A. (2018). Multiple imputation of multilevel
data in organizational research. \emph{Organizational Research Methods, 21}(1), 111-149.
\doi{10.1177/1094428117703686}

Mislevy, R. J. (1991). Randomization-based inference about latent variables
from complex samples. \emph{Psychometrika, 56}(2), 177-196.
\doi{10.1007/BF02294457}

Nowok, B., Raab, G., & Dibben, C. (2016).
\pkg{synthpop}: Bespoke creation of synthetic data in \R.
\emph{Journal of Statistical Software, 74}(11), 1-26.
\doi{10.18637/jss.v074.i11}

Reiter, J. P. (2005)  Releasing multiply-imputed, synthetic public use microdata: 
An illustration and empirical study.  
\emph{Journal of the Royal Statistical Society, Series A, 168}(1), 185-205.
\doi{10.1111/j.1467-985X.2004.00343.x}

Robitzsch, A., Pham, G., & Yanagida, T. (2016). Fehlende Daten und Plausible Values. 
In S. Breit & C. Schreiner (Hrsg.). \emph{Large-Scale Assessment mit R: Methodische 
Grundlagen der oesterreichischen Bildungsstandardueberpruefung} (S. 259-293). Wien: facultas.

Rubin, D. B. (2003). Nested multiple imputation of NMES via partially
incompatible MCMC. \emph{Statistica Neerlandica, 57}(1), 3-18.
\doi{10.1111/1467-9574.00217}

van Buuren, S. (2018). \emph{Flexible imputation of missing data}.
Boca Raton: CRC Press. \doi{10.1201/9780429492259}

van Buuren, S., & Groothuis-Oudshoorn, K. (2011). \pkg{mice}: 
Multivariate imputation by chained equations in \R. 
\emph{Journal of Statistical Software, 45}(3), 1-67.
\doi{10.18637/jss.v045.i03}
}



\seealso{
See also the CRAN task view \emph{Missing Data}:\cr
\url{https://CRAN.R-project.org/view=MissingData} \cr

See other \R packages for conducting multiple imputation:
\pkg{mice}, \pkg{Amelia}, \pkg{pan}, \pkg{mi}, \pkg{norm},
\pkg{norm2}, \pkg{BaBooN}, \pkg{VIM}, ... \cr

Some links to internet sites related to missing data:

\emph{http://missingdata.lshtm.ac.uk/} \cr
\emph{http://www.stefvanbuuren.nl/mi/} \cr
\emph{http://www.bristol.ac.uk/cmm/software/realcom/} \cr
\emph{https://rmisstastic.netlify.com/} \cr
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
