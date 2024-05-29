%% File Name: lm.cluster.Rd
%% File Version: 0.376

\name{lm.cluster}
\alias{lm.cluster}
\alias{glm.cluster}
\alias{coef.lm.cluster}
\alias{coef.glm.cluster}
\alias{vcov.lm.cluster}
\alias{vcov.glm.cluster}
\alias{summary.lm.cluster}
\alias{summary.glm.cluster}


%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Cluster Robust Standard Errors for Linear Models and General Linear Models
}

\description{
Computes cluster robust standard errors for linear models
(\code{\link[stats:lm]{stats::lm}}) and general linear models
(\code{\link[stats:glm]{stats::glm}}) using the
\code{\link[sandwich:vcovCL]{multiwayvcov::vcovCL}}
function in the \pkg{sandwich} package.
}

% \code{\link[TAM:tam.fa]{tam.fa}} (\pkg{TAM})
% multiwayvcov::cluster.vcov( model=mod, cluster=dfr)

\usage{
lm.cluster(data, formula, cluster, weights=NULL, subset=NULL )

glm.cluster(data, formula, cluster, weights=NULL, subset=NULL, family="gaussian" )

\method{summary}{lm.cluster}(object,...)
\method{summary}{glm.cluster}(object,...)

\method{coef}{lm.cluster}(object,...)
\method{coef}{glm.cluster}(object,...)

\method{vcov}{lm.cluster}(object,...)
\method{vcov}{glm.cluster}(object,...)
}


%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
Data frame
}
  \item{formula}{
An \R formula
}
  \item{cluster}{
Variable name for cluster variable contained in \code{data} or a vector
with cluster identifiers
}
\item{subset}{Optional vector specifying a subset of observations to be used.}
\item{weights}{Optional vector of weights to be used.}
\item{family}{Description of the error distribution and link function to be used in
the model, see \code{\link[stats:glm]{stats::glm}}.}
  \item{\dots}{
Further arguments to be passed to \code{\link[stats:lm]{stats::lm}} and
\code{\link[stats:glm]{stats::glm}}
}
  \item{object}{
Object of class \code{lm.cluster} or \code{glm.cluster}
}
}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}


\value{
List with following entries
  \item{lm_res}{Value of \code{\link[stats:lm]{stats::lm}}}
  \item{glm_res}{Value of \code{\link[stats:glm]{stats::glm}}}
  \item{vcov}{Covariance matrix of parameter estimates}
}


%\references{
%% ~put references to the literature/web site here ~
%}




\note{
If \code{lm.cluster} is used inside a function, add \code{wgt__ <<- weight} for
assigning the \code{weight} to \code{wgt__} in the global environment.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[stats:lm]{stats::lm}}, \code{\link[stats:glm]{stats::glm}},
\code{\link[sandwich:vcovCL]{sandwich::vcovCL}}
}


\examples{
\dontrun{

#############################################################################
# EXAMPLE 1: Cluster robust standard errors data.ma01
#############################################################################

data(data.ma01)
dat <- data.ma01

#*** Model 1: Linear regression
mod1 <- miceadds::lm.cluster( data=dat, formula=read ~ hisei + female,
               cluster="idschool" )
coef(mod1)
vcov(mod1)
summary(mod1)

# estimate Model 1, but cluster is provided as a vector
mod1b <- miceadds::lm.cluster( data=dat, formula=read ~ hisei + female,
                 cluster=dat$idschool)
summary(mod1b)

#*** Model 2: Logistic regression
dat$highmath <- 1 * ( dat$math > 600 )   # create dummy variable
mod2 <- miceadds::glm.cluster( data=dat, formula=highmath ~ hisei + female,
                cluster="idschool", family="binomial")
coef(mod2)
vcov(mod2)
summary(mod2)

#############################################################################
# EXAMPLE 2: Cluster robust standard errors for multiply imputed datasets
#############################################################################

library(mitools)
data(data.ma05)
dat <- data.ma05

# imputation of the dataset: use six imputations
resp <- dat[, - c(1:2) ]
imp <- mice::mice( resp, method="norm", maxit=3, m=6 )
datlist <- miceadds::mids2datlist( imp )

# linear regression with cluster robust standard errors
mod <- lapply(  datlist, FUN=function(data){
            miceadds::lm.cluster( data=data, formula=denote ~ migrant+ misei,
                    cluster=dat$idclass )
            }  )
# extract parameters and covariance matrix
betas <- lapply( mod, FUN=function(rr){ coef(rr) } )
vars <- lapply( mod, FUN=function(rr){ vcov(rr) } )
# conduct statistical inference
summary( miceadds::pool_mi( qhat=betas, u=vars ) )

#------ compute global F-test for hypothesis that all predictors have zero coefficient values
library(mitml)
Nimp <- 6 # number of imputations
np <- length(betas[[1]])   # number of parameters
beta_names <- names(betas[[1]])
# define vector of parameters for which constraints should be tested
constraints <- beta_names[-1]
# create input for mitml::testConstraints function
qhat <- matrix( unlist(betas), ncol=Nimp)
rownames(qhat) <- beta_names
uhat <- array( unlist(vars), dim=c(np,np,Nimp))
dimnames(uhat) <- list( beta_names, beta_names, NULL )
# compute global F-test
Ftest <- mitml::testConstraints( qhat=betas, uhat=vars, constraints=constraints )
print(Ftest)

#############################################################################
# EXAMPLE 3: Comparing miceadds::lm.cluster() and lme4::lmer()
#############################################################################

data(data.ma01, package="miceadds")
dat <- na.omit(data.ma01)

# center hisei variable
dat$hisei <- dat$hisei - mean(dat$hisei)

# define school mean hisei
dat$hisei_gm <- miceadds::GroupMean(dat$hisei, dat$idschool, extend=TRUE)[,2]
dat$cluster_size <- miceadds::GroupSum(1+0*dat$hisei, dat$idschool, extend=TRUE)[,2]
dat$hisei_wc <- dat$hisei - dat$hisei_gm

#*** Model 1a: lm, hisei with clustering
mod1a <- miceadds::lm.cluster( data=dat, formula=read~hisei, cluster="idschool" )

#*** Model 1b: lmer, hisei
mod1b <- lme4::lmer( data=dat, formula=read~hisei+(1|idschool) )

cbind( coef(mod1a), fixef(mod1b))
 ##  > cbind( coef(mod1a), fixef(mod1b))
 ##                    [,1]        [,2]
 ##  (Intercept) 509.181691 507.8684752
 ##  hisei         1.524776   0.8161745

# variance explanation
vmod1b <- r2mlm::r2mlm(mod1b)
vmod1b$Decompositions

#*** Model 2a: lm, hisei and hisei_gm with clustering
mod2a <- miceadds::lm.cluster( data=dat, formula=read~hisei_wc+hisei_gm,
                                   cluster="idschool" )

#*** Model 2b: lmer, multilevel model
mod2b <- lme4::lmer( data=dat, formula=read~hisei_wc+hisei_gm + (1|idschool) )

# variance explanation
vmod2b <- r2mlm::r2mlm(mod2b)
vmod2b$Decompositions

cbind( coef(mod2a), fixef(mod2b))
 ##  > cbind( coef(mod2a), fixef(mod2b))
 ##                     [,1]        [,2]
 ##  (Intercept) 509.1816911 508.0478629
 ##  hisei_wc      0.7503773   0.7503773
 ##  hisei_gm      5.8424012   5.5681941

}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%% \keyword{Cluster robust standard errors}
%% \keyword{coef}
%% \keyword{vcov}
