%% File Name: mice_imputation_2l_lmer.Rd
%% File Version: 0.47

\name{mice_imputation_2l_lmer}
\alias{mice.impute.2l.binary}
\alias{mice.impute.2l.continuous}
\alias{mice.impute.2l.pmm}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
Imputation of a Continuous or a Binary Variable From a Two-Level Regression Model
using \pkg{lme4} or \pkg{blme}
}

\description{
The function \code{mice.impute.2l.continuous}
imputes values of continuous variables with a linear
mixed effects model using \code{\link[lme4:lmer]{lme4::lmer}} or
\code{\link[blme:blmer]{blme::blmer}}.
The \code{\link[lme4:lmer]{lme4::lmer}} or
\code{\link[blme:blmer]{blme::blmer}} function is also
used for predictive mean matching where the match is based
on predicted values which contain the fixed and (sampled)
random effects. Binary variables can be imputed
from a two-level logistic regression model
fitted with the \code{\link[lme4:glmer]{lme4::glmer}} or
\code{\link[blme:bglmer]{blme::bglmer}} function.
See Snijders and Bosker (2012) and Zinn (2013) for details.
}

\usage{
mice.impute.2l.continuous(y, ry, x, type, intercept=TRUE,
    groupcenter.slope=FALSE, draw.fixed=TRUE, random.effects.shrinkage=1E-6,
    glmer.warnings=TRUE, blme_use=FALSE, blme_args=NULL, ... )

mice.impute.2l.pmm(y, ry, x, type, intercept=TRUE,
    groupcenter.slope=FALSE, draw.fixed=TRUE, random.effects.shrinkage=1E-6,
    glmer.warnings=TRUE, donors=5, match_sampled_pars=TRUE,
    blme_use=FALSE, blme_args=NULL, ... )

mice.impute.2l.binary(y, ry, x, type, intercept=TRUE,
    groupcenter.slope=FALSE, draw.fixed=TRUE, random.effects.shrinkage=1E-6,
    glmer.warnings=TRUE, blme_use=FALSE, blme_args=NULL, ... )
}


%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{
Incomplete data vector of length \code{n}
}
  \item{ry}{
Vector of missing data pattern (\code{FALSE} -- missing,
\code{TRUE} -- observed)
}
  \item{x}{
Matrix (\code{n} x \code{p}) of complete predictors.
}
\item{type}{Type of predictor variable. The cluster identifier
has type \code{-2}, fixed effects predictors without a random slope
type \code{1} and predictors with fixed effects and random effects
have type \code{2}. If the cluster mean should be included for a covariate,
\code{3} should be chosen. The specification \code{4} includes the cluster mean,
the fixed effect and the random effect.}
  \item{intercept}{
Optional logical indicating whether the intercept should be included.
}
\item{groupcenter.slope}{Optional logical indicating whether covariates
should be centered around group means}
\item{draw.fixed}{Optional logical indicating whether fixed effects
parameter should be randomly drawn}
\item{random.effects.shrinkage}{Shrinkage parameter for stabilizing the
covariance matrix of random effects}
\item{glmer.warnings}{Optional logical indicating whether warnings from
\code{glmer} should be displayed}
\item{blme_use}{Logical indicating whether the \pkg{blme}
package should be used.}
\item{blme_args}{(Prior) Arguments for \pkg{blme}, see
\code{\link[blme:blmer]{blme::blmer}} and
\code{\link[blme:bmerDist-class]{blme::bmerDist-class}}.
}
\item{donors}{Number of donors used for predictive mean matching}
\item{match_sampled_pars}{Logical indicating whether values of nearest
neighbors should also be sampled in pmm imputation.}
  \item{\dots}{
Further arguments to be passed
}
}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}

\value{
A vector of length \code{nmis=sum(!ry)} with imputed values.
}

\references{
Snijders, T. A. B., & Bosker, R. J. (2012). \emph{Multilevel analysis:
An introduction to basic and advanced multilevel modeling}.
Thousand Oaks, CA: Sage.

Zinn, S. (2013). \emph{An imputation model for multilevel binary data}.
NEPS Working Paper No 31.
}

%\author{
%Simon Grund, Alexander Robitzsch
%}

%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See \code{\link{mice.impute.ml.lmer}} for imputation for datasets with more than
two levels (e.g., three-level datasets or cross-classified datasets).

Variables at a higher level (e.g. at level 2) can be imputed using \code{2lonly}
functions, for example the
\code{\link[mice:mice.impute.2lonly.norm]{mice::mice.impute.2lonly.norm}} function
in the \pkg{mice} package or the general \code{\link{mice.impute.2lonly.function}}
function in the \pkg{miceadds} package which using an already defined imputation
method at level 1. If a level-2 variable for 3-level data should be imputed,
then \code{\link{mice.impute.ml.lmer}} can also be used to impute this variable
with a two-level imputation model in which level 1 corresponds to the original
level-2 units and level 2 corresponds to the original level-3 units.

See \code{\link[mice:mice.impute.2l.norm]{mice::mice.impute.2l.norm}} and
\code{\link[mice:mice.impute.2l.pan]{mice::mice.impute.2l.pan}} for
imputation functions in the \pkg{mice} package under fully conditional
specification for normally distributed variables. The function
\code{\link[mice:mice.impute.2l.norm]{mice::mice.impute.2l.norm}} allows for
residual variances which are allowed to vary across groups while
\code{\link[mice:mice.impute.2l.pan]{mice::mice.impute.2l.pan}} assumes
homogeneous residual variances.

The \pkg{micemd} package provides further imputation methods for the \pkg{mice}
package for imputing multilevel data with fully conditional specification. The
function \code{micemd::mice.impute.2l.jomo}
has similar functionality like \code{\link[mice:mice.impute.2l.pan]{mice::mice.impute.2l.pan}}
and imputes normally distributed two-level data with a Bayesian MCMC approach,
but relies on the \pkg{jomo} package instead of the \pkg{pan} package.
The functions \code{\link[mice:mice.impute.2l.lmer]{mice::mice.impute.2l.lmer}} and
\code{micemd::mice.impute.2l.glm.norm}
have similar functionality like \code{\link{mice.impute.2l.continuous}}
and imputes normally distributed two-level data. The function
\code{{micemd::mice.impute.2l.glm.bin}}
has similar functionality like \code{\link{mice.impute.2l.binary}}
and imputes binary two-level data.

The \pkg{hmi} package imputes single-level and multilevel data and is also
based on fully conditional specification. The package relies on the
MCMC estimation implemented in the \pkg{MCMCglmm} package. The imputation procedure
can be run with the \code{hmi::hmi} function.

See the \pkg{pan} (\code{\link[pan:pan]{pan::pan}})
and the \pkg{jomo} (\code{\link[jomo:jomo]{jomo::jomo}})
package for joint multilevel imputation. See
\code{\link[mitml:panImpute]{mitml::panImpute}} and
\code{\link[mitml:jomoImpute]{mitml::jomoImpute}} for
wrapper functions to these packages in the \pkg{mitml}
package.

Imputation by chained equations can also be conducted in blocks of multivariate
conditional distributions since \pkg{mice} 3.0.0 (see the \code{blocks}
argument in \code{\link[mice:mice]{mice::mice}}). The
\code{\link[mitml:panImpute]{mitml::panImpute}} and
\code{\link[mitml:jomoImpute]{mitml::jomoImpute}} functions can be used
with \code{\link[mice:mice]{mice::mice}} by specifying imputation
methods \code{"panImpute"}
(see \code{\link[mice:mice.impute.panImpute]{mice::mice.impute.panImpute}})) and
\code{"jomoImpute"}
(see \code{\link[mice:mice.impute.jomoImpute]{mice::mice.impute.jomoImpute}})).
}

\examples{
\dontrun{
#############################################################################
# EXAMPLE 1: Imputation of a binary variable
#############################################################################

#--- simulate missing values
set.seed(976)
G <- 30        # number of groups
n <- 8        # number of persons per group
iccx <- .2    # intra-class correlation X
iccy <- .3    # latent intra-class correlation binary outcome
bx <- .4    # regression coefficient
threshy <- stats::qnorm(.70)  # threshold for y
x <- rep( rnorm( G, sd=sqrt( iccx) ), each=n )  +
            rnorm(G*n, sd=sqrt( 1 - iccx) )
y <- bx * x + rep( rnorm( G, sd=sqrt( iccy) ), each=n )  +
                rnorm(G*n, sd=sqrt( 1 - iccy) )
y <- 1 * ( y > threshy )
dat <- data.frame( group=100+rep(1:G, each=n), x=x, y=y )

#* create some missings
dat1 <- dat
dat1[ seq( 1, G*n, 3 ),"y" ]  <- NA
dat1[ dat1$group==2, "y" ] <- NA

#--- prepare imputation in mice
vars <- colnames(dat1)
V <- length(vars)
#* predictor matrix
predmat <- matrix( 0, nrow=V, ncol=V)
rownames(predmat) <- colnames(predmat) <- vars
predmat["y", ] <- c(-2,2,0)
#* imputation methods
impmeth <- rep("",V)
names(impmeth) <- vars
impmeth["y"] <- "2l.binary"

#** imputation with logistic regression ('2l.binary')
imp1 <- mice::mice( data=as.matrix(dat1), method=impmeth,
                predictorMatrix=predmat, maxit=1, m=5 )

#** imputation with predictive mean matching ('2l.pmm')
impmeth["y"] <- "2l.pmm"
imp2 <- mice::mice( data=as.matrix(dat1), method=impmeth,
                predictorMatrix=predmat, maxit=1, m=5 )

#** imputation with logistic regression using blme package
blme_args <- list( "cov.prior"="invwishart")
imp3 <- mice::mice( data=as.matrix(dat1), method=impmeth,
                predictorMatrix=predmat, maxit=1, m=5,
                blme_use=TRUE, blme_args=blme_args )
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%% \keyword{mice imputation method}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
