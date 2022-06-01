%% File Name: draw.pv.ctt.Rd
%% File Version: 1.262

\name{draw.pv.ctt}
\alias{draw.pv.ctt}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plausible Value Imputation Using a Known Measurement Error Variance
(Based on Classical Test Theory)
}

\description{
This function provides unidimensional plausible value imputation with a
known measurement error variance or classical test theory (Mislevy, 1991).
The reliability of the scale is estimated by Cronbach's Alpha or can be
provided by the user.
}

\usage{
draw.pv.ctt(y, dat.scale=NULL, x=NULL, samp.pars=TRUE,
      alpha=NULL, sig.e=NULL, var.e=NULL, true.var=NULL)
}



% The default of \code{NULL} assumes that there are
% no scale scores available.

\arguments{
  \item{y}{
Vector of scale scores if \code{y} should not be used.
}
  \item{dat.scale}{
Matrix of item responses
}
  \item{x}{
Matrix of covariates
}
  \item{samp.pars}{
An optional logical indicating whether scale parameters
(reliability or measurement error standard deviation) should be sampled
}
  \item{alpha}{
Reliability estimate of the scale. The default of
\code{NULL} means that Cronbach's alpha will be used
as a reliability estimate.
}
  \item{sig.e}{
Optional vector of the standard deviation of the error.
Note that it is \emph{not} the error variance.
}
  \item{var.e}{
Optional vector of the variance of the error.
}
  \item{true.var}{
True score variance
}
}


\details{
The linear model is assumed for drawing plausible values of a variable
\eqn{Y} contaminated by measurement error. Assuming \eqn{Y=\theta + e}
and a linear regression model for \eqn{\theta}
\deqn{ \theta=\bold{X} \beta + \epsilon}
(plausible value) imputations from the posterior distribution
\eqn{P( \theta | Y, \bold{X} )} are drawn. See Mislevy (1991) for details.
 }

\value{
A vector with plausible values
}

\references{
Blackwell, M., Honaker, J., & King, G. (2011).
\emph{Multiple overimputation: A unified approach to measurement error and
missing data}. Technical Report.

Mislevy, R. J. (1991). Randomization-based inference about latent variables
from complex samples. \emph{Psychometrika, 56}(2), 177-196.
\doi{10.1007/BF02294457}
}


\note{
Plausible value imputation is also labeled as multiple overimputation
(Blackwell, Honaker & King, 2011).
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See also
\code{\link[sirt:plausible.value.imputation.raschtype]{sirt::plausible.value.imputation.raschtype}}
for plausible value imputation.

Plausible value imputations can be conducted in \pkg{mice} using the
imputation method \code{\link{mice.impute.plausible.values}}.

Plausible values can be drawn in \pkg{Amelia} by specifying observation-level
priors, see \code{Amelia::moPrep} and
\code{Amelia::amelia}.
}
%% plausible.value.imputation.raschtype


\examples{
\dontrun{

#############################################################################
# SIMULATED EXAMPLE 1: Scale scores
#############################################################################

set.seed(899)
n <- 5000       # number of students
x <- round( stats::runif( n, 0,1 ) )
y <- stats::rnorm(n)
# simulate true score theta
theta <- .6 + .4*x + .5 * y + stats::rnorm(n)
# simulate observed score by adding measurement error
sig.e <- rep( sqrt(.40), n )
theta_obs <- theta + stats::rnorm( n, sd=sig.e)

# calculate alpha
( alpha <- stats::var( theta ) / stats::var( theta_obs ) )
# [1] 0.7424108
#=> Ordinarily, sig.e or alpha will be known, assumed or estimated by using items,
#    replications or an appropriate measurement model.

# create matrix of predictors
X <- as.matrix( cbind(x, y ) )

# plausible value imputation with scale score
imp1 <- miceadds::draw.pv.ctt( y=theta_obs, x=X, sig.e=sig.e )
# check results
stats::lm( imp1 ~ x + y )

# imputation with alpha as an input
imp2 <- miceadds::draw.pv.ctt( y=theta_obs, x=X, alpha=.74 )
stats::lm( imp2 ~ x + y )

#--- plausible value imputation in Amelia package
library(Amelia)

# define data frame
dat <- data.frame( "x"=x, "y"=y, "theta"=theta_obs )
# generate observation-level priors for theta
priors <- cbind( 1:n, 3, theta_obs, sig.e )
             # 3 indicates column index for theta
overimp <- priors[,1:2]
# run Amelia
imp <- Amelia::amelia( dat, priors=priors, overimp=overimp, m=10)
# create object of class datlist and evaluate results
datlist <- miceadds::datlist_create( imp$imputations )
withPool_MI( with( datlist, stats::var(theta) ) )
stats::var(theta)       # compare with true variance
mod <- with( datlist,  stats::lm( theta ~ x + y ) )
mitools::MIcombine(mod)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%% \keyword{Plausible value imputation}
%% \keyword{Latent variables}

%% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
