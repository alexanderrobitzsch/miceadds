%% File Name: micombine.F.Rd
%% File Version: 0.221

\name{micombine.F}
\alias{micombine.F}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Combination of F Statistics for Multiply Imputed Datasets Using a
Chi Square Approximation
}

\description{
Several \eqn{F} statistics from multiply imputed datasets are combined using
an approximation based on \eqn{\chi^2} statistics
(see \code{\link{micombine.chisquare}}).
}

\usage{
micombine.F(Fvalues, df1, display=TRUE, version=1)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
\item{Fvalues}{
Vector containing \eqn{F} values.
}
  \item{df1}{
Degrees of freedom of the numerator. Degrees of freedom of the
numerator are approximated by \eqn{\infty} (large number of degrees
of freedom).
}
  \item{display}{
A logical indicating whether results should be displayed at the console
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
%%  ~~ If necessary, more details than the description above ~~
%}

\value{
The same output as in \code{\link{micombine.chisquare}}
}

\references{
Allison, P. D. (2002). \emph{Missing data}. Newbury Park, CA: Sage.

Enders, C. K. (2010). \emph{Applied missing data analysis}. Guilford Press.

Grund, S., Luedtke, O., & Robitzsch, A. (2016). Pooling ANOVA results from
multiply imputed datasets: A simulation study.
\emph{Methodology, 12}(3), 75-88.
\doi{10.1027/1614-2241/a000111}
}



%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{micombine.chisquare}}
}

\examples{
#############################################################################
# EXAMPLE 1: F statistics for 5 imputed datasets
#############################################################################

Fvalues <- c( 6.76, 4.54, 4.23, 5.45, 4.78 )
micombine.F(Fvalues, df1=4 )
  ##  Combination of Chi Square Statistics for Multiply Imputed Data
  ##  Using 5 Imputed Data Sets
  ##  F(4, 52.94)=3.946     p=0.00709
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%% \keyword{F statistic}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
