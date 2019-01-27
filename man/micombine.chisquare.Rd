%% File Name: micombine.chisquare.Rd
%% File Version: 0.21

\name{micombine.chisquare}
\alias{micombine.chisquare}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Combination of Chi Square Statistics of Multiply Imputed Datasets
}

\description{
This function does inference for the \eqn{\chi^2} statistic based on multiply
imputed datasets (see e.g. Enders, 2010, p. 239 ff.; Allison, 2002).
This function is also denoted as the \eqn{D_2} statistic.
}

\usage{
micombine.chisquare(dk, df, display=TRUE, version=1)
}
%- maybe also 'usage' for other objects documented here.


\arguments{
  \item{dk}{
Vector of chi square statistics
}
  \item{df}{
Degrees of freedom of \eqn{\chi^2} statistic
}
  \item{display}{
An optional logical indicating whether results should be printed at the
\R console.
}
\item{version}{Integer indicating which calculation formula should be used.
    The default \code{version=1} refers to the correct formula as in Enders (2010),
    while \code{version=0} uses an incorrect formula as printed in Allison (2001).
    The incorrect calculation \code{version=0} was included in \pkg{miceadds} versions
smaller than version 2.0.
See also \emph{http://statisticalhorizons.com/wp-content/uploads/2012/01/combchi.sas}.
}
}

%\details{
%xxx
%}

\value{
A vector with following entries

 \item{D}{Combined \eqn{D_2} statistic which is approximately \eqn{F}-distributed
 with (\code{df}, \code{df2}) degrees of freedom}
  \item{p}{The p value corresponding to \code{D}}
\item{df}{Numerator degrees of freedom}
\item{df2}{Denominator degrees of freedom}
}


\references{
Allison, P. D. (2002). \emph{Missing data}. Newbury Park, CA: Sage.

Enders, C. K. (2010). \emph{Applied missing data analysis}. Guilford Press.
}

%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See also \code{\link[mice:pool.compare]{mice::pool.compare}}
for a Wald test to compare two fitted models in the \pkg{mice} package.
}


\examples{
#############################################################################
# EXAMPLE 1: Chi square values of analyses from 7 multiply imputed datasets
#############################################################################

# Vector of 7 chi square statistics
dk <- c(24.957, 18.051, 18.812, 17.362, 21.234, 18.615, 19.84)
dk.comb <- miceadds::micombine.chisquare(dk=dk, df=4 )
  ##  Combination of Chi Square Statistics for Multiply Imputed Data
  ##  Using 7 Imputed Data Sets
  ##  F(4, 482.06)=4.438     p=0.00157
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%% \keyword{Chi square statistic}
%% \keyword{D2 statistic}
