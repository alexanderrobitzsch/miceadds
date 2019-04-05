%% File Name: pca.covridge.Rd
%% File Version: 0.25

\name{pca.covridge}
\alias{pca.covridge}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
Principal Component Analysis with Ridge Regularization
}

\description{
Performs a principal component analysis for a dataset while a ridge
parameter is added on the diagonal of the covariance matrix.
}


\usage{
pca.covridge(x, ridge=1E-10, wt=NULL )
}


%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A numeric matrix
}
  \item{ridge}{
Ridge regularization parameter for the covariance matrix
}
\item{wt}{Optional vector of weights}
}


%\details{
%%  ~~ If necessary, more details than the description above ~~
%}


\value{
A list with following entries:

\item{loadings}{Matrix of factor loadings}
\item{scores}{Matrix of principal component scores}
\item{sdev}{
Vector of standard deviations of factors (square root of eigenvalues)
}
}


%\references{
%% ~put references to the literature/web site here ~
%}



%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
Principal component analysis in \pkg{stats}:
\code{\link[stats:princomp]{stats::princomp}}

For calculating first eigenvalues of a symmetric matrix see also
\code{sirt::sirt_eigenvalues} in the \pkg{sirt} package.
}


\examples{
\dontrun{
#############################################################################
# EXAMPLE 1: PCA on imputed internet data
#############################################################################

library(mice)
data(data.internet)
dat <- as.matrix( data.internet)

# single imputation in mice
imp <- mice::mice( dat, m=1, maxit=10 )

# apply PCA
pca.imp <- miceadds::pca.covridge( complete(imp) )
  ##   > pca.imp$sdev
  ##      Comp.1    Comp.2    Comp.3    Comp.4    Comp.5    Comp.6    Comp.7
  ##   3.0370905 2.3950176 2.2106816 2.0661971 1.8252900 1.7009921 1.6379599

# compare results with princomp
pca2.imp <- stats::princomp( complete(imp) )
  ##   > pca2.imp
  ##   Call:
  ##   stats::princomp(x=complete(imp))
  ##
  ##   Standard deviations:
  ##      Comp.1    Comp.2    Comp.3    Comp.4    Comp.5    Comp.6    Comp.7
  ##   3.0316816 2.3907523 2.2067445 2.0625173 1.8220392 1.6979627 1.6350428
}
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%% \keyword{Dimension reduction}
%% \keyword{Principal component analysis}% __ONLY ONE__ keyword per line
