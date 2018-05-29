%% File Name: mice.impute.ml.lmer.Rd
%% File Version: 0.26

\name{mice.impute.ml.lmer}
\alias{mice.impute.ml.lmer}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
Multilevel Imputation Using \pkg{lme4}
}

\description{
This function is a general imputation function based on the linear mixed effects
model as implemented in \code{lme4::lmer}. The imputation model can be hierarchical
or non-hierarchical and can be written in a general form
\eqn{\bold{y}=\bold{X} \bold{\beta} + \sum_{v=1}^V \bold{Z}_v \bold{u}_v} for \eqn{V}
multivariate random effects. While predictors can be selected by specifying the rows
in the predictor matrix in \code{mice::mice} (i.e., modification of \code{type}),
the level of random effects can be specified with \code{levels_id} and random slopes
can be selected with \code{random_slopes}.

The function \code{mice.impute.ml.lmer} allows the imputation of variables at
arbitrary levels. The corresponding level can be specified with \code{levels_id}.
All predictor variables are aggregated to the corresponding level of the variable
to be imputed.

Several strategies for the specification of the design
matrix \eqn{\bold{X}} are accommodated. By default, predictors at a lower level
are automatically aggregated to the higher level and included as further
predictors to maintain the multilevel structure in the data (Grund et al., xxxx;
Enders et al., 2016; argument \code{aggregate_automatically=TRUE}). Further,
interactions and quadratic effects can be defined by respective arguments
\code{interactions} and \code{quadratics}. The dimension
of the matrix of predictors can be reduced by applying partial least squares regression,
see \code{\link{mice.impute.pls}}.

The function now only allows continuous data (\code{model="continuous"}),
ordinal data (\code{model="pmm"}) or
binary data (\code{model="pmm"} or \code{model="binary"}). Nominal variables with
missing values cannot (yet) be handled.
}


\usage{
mice.impute.ml.lmer(y, ry, x, type, levels_id, variables_levels=NULL,
    random_slopes=NULL, aggregate_automatically=TRUE, intercept=TRUE,
    groupcenter.slope=FALSE, draw.fixed=TRUE, random.effects.shrinkage=1e-06,
    glmer.warnings=TRUE, model="continuous", donors=3, match_sampled_pars=FALSE,
    blme_use=FALSE, blme_args=NULL, pls.facs=0, interactions=NULL,
    quadratics=NULL, min.int.cor=0, min.all.cor=0, pls.print.progress=FALSE, ...)
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
Matrix (\code{n} \eqn{\times} \code{p}) of complete predictors.
}
\item{type}{Predictor variables associated with fixed effects.
}
  \item{levels_id}{
Specification of the level identifiers (see Examples)
}
  \item{variables_levels}{
Specification of the level of variables (see Examples)
}
  \item{random_slopes}{
Specification of random slopes (see Examples)
}
  \item{aggregate_automatically}{
Logical indicating whether aggregated effects at higher levels are automatically
included.
}
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

  \item{model}{
Type of model. Can be \code{"continuous"} for normally distributed data,
\code{"binary"} for dichotomous data specifying a logistic mixed effects model
and \code{"pmm"} for predictive mean matching.
}
\item{donors}{Number of donors used for predictive mean matching}
\item{match_sampled_pars}{Logical indicating whether values of nearest
neighbors should also be sampled in pmm imputation.}
\item{blme_use}{Logical indicating whether the \pkg{blme}
package should be used.}
\item{blme_args}{(Prior) Arguments for \pkg{blme}, see
\code{\link[blme:blmer]{blme::blmer}} and
\code{\link[blme:bmerDist-class]{blme::bmerDist-class}}.
}
  \item{pls.facs}{
Number of factors used in PLS dimension reduction
}
  \item{interactions}{
Specification of predictors with interaction effects
}
  \item{quadratics}{
Specification of predictors with quadratic effects
}
  \item{min.int.cor}{
Minimum absolute value of correlation with outcome for interaction effects to be retained
}
  \item{min.all.cor}{
Minimum absolute value of correlation with outcome for predictors to be retained
}
  \item{pls.print.progress}{
Logical indicating whether progress of algorithm should be displayed
}
  \item{\dots}{
Further arguments to be passed
}
}


%%%%----------------------------------------
%\details{
%some details XXX
%}

%%%%----------------------------------------
\value{
Vector of imputed values
}


%%%%----------------------------------------
\references{
Enders, C. K., Mistler, S. A., & Keller, B. T. (2016). Multilevel multiple imputation:
A review and evaluation of joint modeling and chained equations imputation.
\emph{Psychological Methods, 21}(2), 222-240.

Grund, S., Luedtke, O., & Robitzsch, A. (xxxx). Multiple imputation of multilevel
data in organizational research. \emph{Organizational Research Methods, xx}, xxx-xxx.
}


%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See \code{\link{mice.impute.2l.continuous}} for two-level imputation in \pkg{mice} and
for several links to other packages which enable multilevel imputation.
}



\examples{
\dontrun{
#############################################################################
# EXAMPLE 1: Imputation of three-level data with normally distributed residuals
#############################################################################

data(data.ma07, package="miceadds")
dat <- data.ma07

# variables at level 1 (identifier id1): x1 (some missings), x2 (complete)
# variables at level 2 (identifier id2): y1 (some missings), y2 (complete)
# variables at level 3 (identifier id3): z1 (some missings), z2 (complete)

# start with empty imputation
imp0 <- mice::mice( dat, maxit=0)

#****************************************************************************
# Imputation model 1

#----- specify levels of variables (only relevent for variables
#      with missing values)
variables_levels <- miceadds:::mice_impute_create_type_vector( colnames(dat), value="")
 # leave variables at lowest level blank (i.e., "")
variables_levels[ c("y1","y2") ] <- "id2"
variables_levels[ c("z1","z2") ] <- "id3"

#----- specify predictor matrix
predmat <- imp0$predictorMatrix
predmat[, c("id2", "id3") ] <- 0
# set -2 for cluster identifier for level 3 variable z1
# because "2lonly" function is used
predmat[ "z1", "id3" ] <- -2

#----- specify imputation methods
imputationMethod <- imp0$method
imputationMethod[c("x1","y1")] <- "ml.lmer"
imputationMethod[c("z1")] <- "2lonly.norm"

#----- specify hierarchical structure of imputation models
levels_id <- list()
#** hierarchical structure for variable x1
levels_id[["x1"]] <- c("id2", "id3")
#** hierarchical structure for variable y1
levels_id[["y1"]] <- c("id3")

#----- specify random slopes
random_slopes <- list()
#** random slopes for variable x1
random_slopes[["x1"]] <- list( "id2"=c("x2","y1"), "id3"=c("y1") )
#** random slopes for variable y1
random_slopes[["y1"]] <- list( "id3"=c("z1") )
# if no random slopes should be specified, the corresponding entry can be left empty
# and only a random intercept is used in the imputation model

#----- imputation in mice
imp1 <- mice::mice( dat, maxit=10, m=5, method=imputationMethod,
            predictorMatrix=predmat, levels_id=levels_id,  random_slopes=random_slopes,
            variables_levels=variables_levels )
summary(imp1)

#****************************************************************************
# Imputation model 2

#----- impute x1 with predictive mean matching and y1 with normally distributed residuals
model <- list( x1="pmm", y1="continuous" )

#----- assume only random intercepts
random_slopes <- NULL

#---- create interactions with z2 for all predictors in imputation models for x1 and y1
interactions <- list( "x1"=z2, "y1"=z2)

#----- imputation in mice
imp2 <- mice::mice( dat, method=imputationMethod, predictorMatrix=predmat,
                levels_id=levels_id, random_slopes=random_slopes,
                variables_levels=variables_levels, model=model, interactions=interactions)
summary(imp2)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
