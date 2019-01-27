%% File Name: pool_mi.Rd
%% File Version: 0.22

\name{pool_mi}
\alias{pool_mi}
\alias{summary.pool_mi}
\alias{coef.pool_mi}
\alias{vcov.pool_mi}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Statistical Inference for Multiply Imputed Datasets
}

\description{
Statistical inference for multiply imputed datasets. See
\code{\link[mitools:MIcombine]{mitools::MIcombine}} or
\code{\link[mice:pool]{mice::pool}} for
functions of the same functionality.
}

\usage{
pool_mi(qhat, u=NULL, se=NULL, dfcom=1e+07, method="smallsample")

\method{summary}{pool_mi}(object, alpha=0.05, ...)

\method{coef}{pool_mi}(object, ...)

\method{vcov}{pool_mi}(object, ...)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{qhat}{
List of parameter vectors
}
  \item{u}{
List of covariance matrices
}
  \item{se}{
List of vector of standard errors. Either \code{u} or
  \code{se} must be provided.
}
  \item{dfcom}{
Degrees of freedom of statistical analysis
}
  \item{method}{
The default is the small sample inference (\code{"smallsample"}).
Any other input provides large sample inference.
}
\item{object}{Object of class \code{pool_mi}}
\item{alpha}{Confidence level}
\item{\dots}{Further arguments to be passed}
}


%\details{
%%  ~~ If necessary, more details than the description above ~~
%}

\value{
Object of with similar output as produced by the
\code{\link[mice:pool]{mice::pool}} function.
}

%\references{
%% ~put references to the literature/web site here ~
%}


%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[mitools:MIcombine]{mitools::MIcombine}},
\code{\link[mice:pool]{mice::pool}},
\code{\link[mitml:testEstimates]{mitml::testEstimates}}

For statistical inference for nested multiply imputed datasets see
\code{\link{NMIcombine}}.
}


\examples{
\dontrun{
#############################################################################
# EXAMPLE 1: Statistical inference for models based on imputationList
#############################################################################

library(mitools)
library(mice)
library(Zelig)
library(mitml)
library(lavaan)
library(semTools)
data(data.ma02)

# save dataset as imputation list
imp <- mitools::imputationList( data.ma02 )
# mids object
imp0 <- miceadds::datlist2mids( imp )
# datlist object
imp1 <- miceadds::datlist_create(data.ma02)

#--- apply linear model based on imputationList
mod <- with( imp, stats::lm( read ~ hisei + female ) )
#--- apply linear model for mids object
mod0 <- with( imp0, stats::lm( read ~ hisei + female ) )
# extract coefficients
cmod <- mitools::MIextract( mod, fun=coef)
# extract standard errors
semod <- lapply( mod, FUN=function(mm){
    smm <- summary(mm)
    smm$coef[,"Std. Error"]
} )
# extract covariance matrix
vmod <- mitools::MIextract( mod, fun=vcov)

#*** pooling based on covariance matrices
res1 <- miceadds::pool_mi( qhat=cmod, u=vmod )
summary(res1)
coef(res1)
vcov(res1)

#*** pooling based on standard errors
res2 <- miceadds::pool_mi( qhat=cmod, se=semod )

#*** pooling with MIcombine
res3 <- mitools::MIcombine( results=cmod, variances=vmod )

#*** pooling with pool function in mice
res4 <- mice::pool( mod0 )

#*** analysis in Zelig
# convert datalist into object of class amelia
mi02 <- list( "imputations"=data.ma02)
class(mi02) <- "amelia"
res5 <- Zelig::zelig( read ~ hisei + female, model="ls", data=mi02 )

#*** analysis in lavaan
lavmodel <- "
     read ~ hisei + female
     read ~~ a*read
     read ~ 1
     # residual standard deviation
     sde :=sqrt(a)
       "
# analysis for first imputed dataset
mod6a <- lavaan::sem( lavmodel, data=imp1[[1]] )
summary(mod6a)
# analysis based on all datasets using with
mod6b <- lapply( imp1, FUN=function(data){
           res <- lavaan::sem( lavmodel, data=data )
           return(res)
                } )
# extract parameters and covariance matrices
qhat0 <- lapply( mod6b, FUN=function(ll){  coef(ll) } )
u0 <- lapply( mod6b, FUN=function(ll){  vcov(ll) } )
res6b <- mitools::MIcombine( results=qhat0, variances=u0 )

# extract informations for all parameters
qhat <- lapply( mod6b, FUN=function(ll){
        h1 <- lavaan::parameterEstimates(ll)
        parnames <- paste0( h1$lhs, h1$op, h1$rhs )
        v1 <- h1$est
        names(v1) <- parnames
        return(v1)
     } )
se <- lapply( mod6b, FUN=function(ll){
        h1 <- lavaan::parameterEstimates(ll)
        parnames <- paste0( h1$lhs, h1$op, h1$rhs )
        v1 <- h1$se
        names(v1) <- parnames
        return(v1)
     } )
res6c <- miceadds::pool_mi( qhat=qhat, se=se )

# function runMI in semTools package
res6d <- semTools::runMI(model=lavmodel, data=imp1, m=length(imp1) )
  # semTools version 0.4-9 provided an error message
# perform inference with mitml package
se2 <- lapply( se, FUN=function(ss){  ss^2  } )  # input variances
res6e <- mitml::testEstimates(qhat=qhat, uhat=se2)

#*** complete model estimation and inference in mitml

# convert into object of class mitml.list
ml02 <- mitml::as.mitml.list( data.ma02)
# estimate regression
mod7 <- with( ml02, stats::lm( read ~ hisei + female ) )
# inference
res7 <- mitml::testEstimates( mod7 )

#*** model comparison
summary(res1)
summary(res2)
summary(res3)
summary(res4)
summary(res5)
summary(res6b)
summary(res6c)
print(res6e)
print(res7)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%% \keyword{summary}
%% \keyword{coef}
%% \keyword{vcov}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
