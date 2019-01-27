%% File Name: Rhat.mice.Rd
%% File Version: 0.12

\name{Rhat.mice}
\alias{Rhat.mice}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Rhat Convergence Statistic of a \code{mice} Imputation
}
\description{
Computes the Rhat statistic for a \code{mids} object.
}
\usage{
Rhat.mice(mice.object)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mice.object}{
Object of class \code{mids}
}
}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
Data frame containing the Rhat statistic for mean and variances for all variables
of the Markov chains used for imputation
}
\references{
Gelman, A., & Hill, J. (2007).
\emph{Data analysis using regression and multilevel/hierarchical models}.
Cambridge University Press.
}

%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
\dontrun{
#############################################################################
# EXAMPLE 1: Rhat statistic for nhanes data
#############################################################################

library(mice)
data(nhanes, package="mice")
set.seed(9090)

# nhanes 3 parallel chains
imp1 <- mice::mice( nhanes, m=3, maxit=10, method=rep("norm", 4 ))
miceadds::Rhat.mice( imp1 )
  ##     variable MissProp Rhat.M.imp Rhat.Var.imp
  ##   1      bmi       36  1.0181998     1.155807
  ##   2      hyp       32  1.0717677     1.061174
  ##   3      chl       40  0.9717109     1.318721
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%% \keyword{Rhat statistic}
%% \keyword{Convergence}% __ONLY ONE__ keyword per line
