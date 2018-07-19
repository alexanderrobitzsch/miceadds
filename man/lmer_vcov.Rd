%% File Name: lmer_vcov.Rd
%% File Version: 0.19


\name{lmer_vcov}
\alias{lmer_vcov}
\alias{summary.lmer_vcov}
\alias{coef.lmer_vcov}
\alias{vcov.lmer_vcov}
\alias{lmer_vcov2}
\alias{lmer_pool}
\alias{summary.lmer_pool}
\alias{lmer_pool2}


%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Statistical Inference for Fixed and Random Structure for Fitted Models
in \pkg{lme4}
}

\description{
The function \code{lmer_vcov} conducts statistical inference for
fixed coefficients and standard deviations
and correlations of random effects structure of models fitted in the
\pkg{lme4} package.

The function \code{lmer_pool} applies the Rubin formula for inference
for fitted \pkg{lme4} models for multiply imputed datasets.
}

\usage{
lmer_vcov(object, level=.95, use_reml=FALSE, ...)

\method{summary}{lmer_vcov}(object, digits=4, file=NULL, ...)
\method{coef}{lmer_vcov}(object, ...)
\method{vcov}{lmer_vcov}(object, ...)

lmer_vcov2(object, level=.95, ...)

lmer_pool( models, level=.95, ...)
\method{summary}{lmer_pool}(object, digits=4, file=NULL, ...)

lmer_pool2( models, level=.95, ...)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
Fitted object in \pkg{lme4}
}
\item{level}{Confidence level}
\item{use_reml}{Logical indicating whether REML estimates should be used for
variance components (if provided)}
\item{digits}{Number of digits used for rounding in summary}
\item{file}{Optional file name for sinking output}
\item{models}{List of models fitted in \pkg{lme4} for a multiply
  imputed dataset}
\item{\dots}{Further arguments to be passed}
}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
List with several entries:

\item{par_summary}{Parameter summary}
\item{coef}{Estimated parameters}
\item{vcov}{Covariance matrix of estimates}
\item{\ldots}{Further values}
}

%\references{
%% ~put references to the literature/web site here ~
%}

\author{
Function originally from Ben Bolker, http://rpubs.com/bbolker/varwald
}

%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~


\seealso{
\code{\link[lme4:lmer]{lme4::lmer}},
\code{\link[mitml:testEstimates]{mitml::testEstimates}}
}



\examples{
\dontrun{
#############################################################################
# EXAMPLE 1: Single model fitted in lme4
#############################################################################

library(lme4)
data(data.ma01, package="miceadds")
dat <- na.omit(data.ma01)

#* fit multilevel model
formula <- math ~ hisei + miceadds::gm( books, idschool ) + ( 1 + books | idschool )
mod1 <- lme4::lmer( formula, data=dat, REML=FALSE)
summary(mod1)

#* statistical inference
res1 <- miceadds::lmer_vcov( mod1 )
summary(res1)
coef(res1)
vcov(res1)

#############################################################################
# EXAMPLE 2: lme4 model for multiply imputed dataset
#############################################################################

library(lme4)
data(data.ma02, package="miceadds")
datlist <- miceadds::datlist_create(data.ma02)

#** fit lme4 model for all imputed datasets
formula <- math ~ hisei + miceadds::gm( books, idschool ) + ( 1 | idschool )
models <- list()
M <- length(datlist)
for (mm in 1:M){
    models[[mm]] <- lme4::lmer( formula, data=datlist[[mm]], REML=FALSE)
}

#** statistical inference
res1 <- miceadds::lmer_pool(models)
summary(res1)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
