%% File Name: pool.mids.nmi.Rd
%% File Version: 0.514

\name{pool.mids.nmi}
\alias{pool.mids.nmi}
\alias{summary.mipo.nmi}
\alias{coef.mipo.nmi}
\alias{vcov.mipo.nmi}
\alias{NMIcombine}
\alias{NMIextract}
\alias{pool_nmi}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Pooling for Nested Multiple Imputation
}

\description{
Statistical inference for scalar parameters for nested multiply imputed
datasets (Rubin, 2003; Harel & Schafer, 2002, 2003; Reiter & Raghanuthan, 2007;
Harel, 2007).

The \code{NMIcombine} (\code{pool_nmi} as a synonym)
and \code{NMIextract} functions are extensions of
\code{\link[mitools:MIcombine]{mitools::MIcombine}} and
\code{\link[mitools:MIextract]{mitools::MIextract}}.
}


\usage{
pool.mids.nmi(object, method="largesample")

NMIcombine( qhat, u=NULL, se=NULL, NMI=TRUE, comp_cov=TRUE, is_list=TRUE,
       method=1)

pool_nmi( qhat, u=NULL, se=NULL, NMI=TRUE, comp_cov=TRUE, is_list=TRUE,
       method=1)

NMIextract(results, expr, fun)

\method{summary}{mipo.nmi}(object, digits=4, ...)

\method{coef}{mipo.nmi}(object, ...)

\method{vcov}{mipo.nmi}(object, ...)
}


%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
Object of class \code{mids.nmi}. For \code{summary} it must be an object
of class \code{mipo.nmi}.
}
  \item{method}{
For \code{pool.mids.nmi}: Method for calculating degrees of freedom. Until now, only the
method \code{"largesample"} is available. \cr
For \code{NMIcombine} and \code{pool_nmi}: Computation method of fraction of missing information.
\code{method=1} is due to Harel and Schafer (2003) or Shen (2007).
\code{method=2} is due to Harel and Schafer (2002) and is coherent to
the calculation for multiply imputed datasets, while the former method is not.
}
\item{qhat}{List of lists of parameter estimates. In case of an ordinary
imputation it can only be a list.}
\item{u}{Optional list of lists of covariance matrices of parameter estimates}
\item{se}{Optional vector of standard errors. This argument overwrites
    \code{u} if it is provided.
}
\item{NMI}{Optional logical indicating whether the \code{NMIcombine}
    function should be applied for results of
    nested multiply imputed datasets.
    It is set to \code{FALSE} if only a list results of multiply imputed
datasets is available.}
\item{comp_cov}{Optional logical indicating whether covariances between
parameter estimates should be estimated.}
\item{is_list}{Optional logical indicating whether \code{qhat} and \code{u}
    are provided as lists as an input. If \code{is_list=FALSE},
appropriate arrays can be used as input.}
\item{results}{A list of objects}
\item{expr}{An expression}
\item{fun}{A function of one argument}
\item{digits}{Number of digits after decimal for printing results in
\code{summary}.}
\item{\dots}{Further arguments to be passed.}
}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}

\value{
Object of class \code{mipo.nmi} with following entries

\item{qhat}{Estimated parameters in all imputed datasets}
\item{u}{Estimated covariance matrices of parameters in all imputed datasets}
\item{qbar}{Estimated parameter}
\item{ubar}{Average estimated variance within imputations}
\item{Tm}{Total variance of parameters}
\item{df}{Degrees of freedom}
\item{lambda}{Total fraction of missing information}
\item{lambda_Between}{Fraction of missing information of between imputed
 datasets (first stage imputation)}
\item{lambda_Within}{Fraction of missing information of within imputed
 datasets (second stage imputation)}
}




\references{
Harel, O., & Schafer, J. (2002). \emph{Two stage multiple imputation}.
Joint Statistical Meetings - Biometrics Section.

Harel, O., & Schafer, J. (2003). \emph{Multiple imputation in two stages}.
In Proceedings of Federal Committee on Statistical Methodology 2003 Conference.

Harel, O. (2007). Inferences on missing information under multiple imputation
and two-stage multiple imputation. \emph{Statistical Methodology, 4}(1), 75-89.
\doi{10.1016/j.stamet.2006.03.002}

Reiter, J. P. and Raghunathan, T. E. (2007). The multiple adaptations of multiple
imputation. \emph{Journal of the American Statistical Association, 102}(480), 1462-1471.
\doi{10.1198/016214507000000932}

Rubin, D. B. (2003). Nested multiple imputation of NMES via partially
incompatible MCMC. \emph{Statistica Neerlandica, 57}(1), 3-18.
\doi{10.1111/1467-9574.00217}
}



%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[mice:pool]{mice::pool}},
\code{\link[mitools:MIcombine]{mitools::MIcombine}},
\code{\link[mitools:MIextract]{mitools::MIextract}}

\code{\link{mice.nmi}},
\code{\link{MIcombine.NestedImputationResultList}}
}


\examples{
\dontrun{
#############################################################################
# EXAMPLE 1: Nested multiple imputation and statistical inference
#############################################################################

library(BIFIEsurvey)
data(data.timss2, package="BIFIEsurvey" )
datlist <- data.timss2
# remove first four variables
M <- length(datlist)
for (ll in 1:M){
    datlist[[ll]] <- datlist[[ll]][, -c(1:4) ]
               }

#***************
# (1) nested multiple imputation using mice
imp1 <- miceadds::mice.nmi( datlist,  m=3, maxit=2 )
summary(imp1)

#***************
# (2) first linear regression: ASMMAT ~ migrant + female
res1 <- with( imp1, stats::lm( ASMMAT ~ migrant + female ) ) # fit
pres1 <- miceadds::pool.mids.nmi( res1 )  # pooling
summary(pres1)  # summary
coef(pres1)
vcov(pres1)

#***************
# (3) second linear regression: likesc ~ migrant + books
res2 <- with( imp1, stats::lm( likesc ~ migrant + books  ) )
pres2 <- miceadds::pool.mids.nmi( res2 )
summary(pres2)

#***************
# (4) some descriptive statistics using the mids.nmi object
res3 <- with( imp1, c( "M_lsc"=mean(likesc), "SD_lsc"=stats::sd(likesc) ) )
pres3 <- miceadds::NMIcombine( qhat=res3$analyses )
summary(pres3)

#*************
# (5) apply linear regression based on imputation list

# convert mids object to datlist
datlist2 <- miceadds::mids2datlist( imp1 )
str(datlist2, max.level=1)

# double application of lapply to the list of list of nested imputed datasets
res4 <- lapply( datlist2, FUN=function(dl){
    lapply( dl, FUN=function(data){
            stats::lm( ASMMAT ~ migrant + books, data=data )
                                } )
                }  )

# extract coefficients
qhat <- lapply( res4, FUN=function(bb){
            lapply( bb, FUN=function(ww){
                    coef(ww)
                        } )
                } )
# shorter function
NMIextract( results=res4, fun=coef )

# extract covariance matrices
u <- lapply( res4, FUN=function(bb){
            lapply( bb, FUN=function(ww){
                    vcov(ww)
                        } )
                } )
# shorter function
NMIextract( results=res4, fun=vcov )

# apply statistical inference using the NMIcombine function
pres4 <- miceadds::NMIcombine( qhat=qhat, u=u )
summary(pres4)

#--- statistical inference if only standard errors are available
# extract standard errors
se <- lapply( res4, FUN=function(bb){
            lapply( bb, FUN=function(ww){
                # ww <- res4[[1]][[1]]
                sww <- summary(ww)
                sww$coef[,"Std. Error"]
                        } )
                } )
se
# apply NMIcombine function
pres4b <- miceadds::NMIcombine( qhat=qhat, se=se )
# compare results
summary(pres4b)
summary(pres4)

#############################################################################
# EXAMPLE 2: Some comparisons for a multiply imputed dataset
#############################################################################

library(mitools)
data(data.ma02)

# save dataset as imputation list
imp <- mitools::imputationList( data.ma02 )
print(imp)
# save dataset as an mids object
imp1 <- miceadds::datlist2mids( imp )

# apply linear model based on imputationList
mod <- with( imp, stats::lm( read ~ hisei + female ) )
# same linear model based on mids object
mod1 <- with( imp1, stats::lm( read ~ hisei + female ) )

# extract coefficients
cmod <- mitools::MIextract( mod, fun=coef)
# extract standard errors
semod <- lapply( mod, FUN=function(mm){
                smm <- summary(mm)
                smm$coef[,"Std. Error"]
                        } )
# extract covariance matrix
vmod <- mitools::MIextract( mod, fun=vcov)

#*** pooling with NMIcombine with se (1a) and vcov (1b) as input
pmod1a <- miceadds::NMIcombine( qhat=cmod, se=semod, NMI=FALSE )
pmod1b <- miceadds::NMIcombine( qhat=cmod, u=vmod, NMI=FALSE )
# use method 2 which should conform to MI inference of mice::pool
pmod1c <- miceadds::NMIcombine( qhat=cmod, u=vmod, NMI=FALSE, method=2)

#*** pooling with mitools::MIcombine function
pmod2 <- mitools::MIcombine( results=cmod, variances=vmod )
#*** pooling with mice::pool function
pmod3a <- mice::pool( mod1 )
pmod3b <- mice::pool( mod1, method="Rubin")

#--- compare results
summary(pmod1a)   # method=1  (the default)
summary(pmod1b)   # method=1  (the default)
summary(pmod1c)   # method=2
summary(pmod2)
summary(pmod3a)
summary(pmod3b)
}
}



% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%% \keyword{Nested multiple imputation}
%% \keyword{summary}
%% \keyword{coef}
%% \keyword{vcov}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
