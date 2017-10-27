%% File Name: mi_dstat.Rd
%% File Version: 0.06

\name{mi_dstat}
\alias{mi_dstat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Cohen's d Effect Size for Missingness Indicators
}
\description{
Computes Cohen's d effect size indicating whether missingness
on a variable is related to other variables (covariates).
}
\usage{
mi_dstat(dat)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
Data frame
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
A matrix. Missingness indicators refer to rows and covariates
to columns.
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Alexander Robitzsch
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
#############################################################################
# EXAMPLE 1: d effect size for missingness indicators data.ma01
#############################################################################

data(data.ma01)
dat <- data.ma01

# compute d effect sizes
md <- miceadds::mi_dstat(dat)
round( md , 3 )
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
