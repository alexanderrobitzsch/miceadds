%% File Name: in_CI.Rd
%% File Version: 0.05

\name{in_CI}
\alias{in_CI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Indicator Function for Analyzing Coverage
}
\description{
Indicator function for analyzing coverage. The output indicates whether a
value lies within a computed confidence interval.
}
\usage{
in_CI(est, se, true, level = 0.95, df = Inf)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{est}{
Vector of estimates
}
  \item{se}{
Vector of standard errors
}
  \item{true}{
Vector of true parameters
}
  \item{level}{
Confidence level
}
  \item{df}{
Degrees of freedom for \eqn{t} distribution. The default corresponds to
the normal distribution.}
}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}

\value{
Logical vector
}

%\references{
%% ~put references to the literature/web site here ~
%}
%\author{
%%  ~~who you are~~
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
# EXAMPLE 1: Toy example
#############################################################################

#-- simulate estimates and standard errors
set.seed(987)
n <- 10
est <- stats::rnorm( n , sd = 1)
se <- stats::runif( n , 0 , .7 )
level <- .95
true <- 0

#-- apply coverage function
in_ci <- miceadds::in_CI( est, se, true)
#-- check correctness
cbind( est, se , true , in_ci )
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
