%% File Name: mice.1chain.Rd
%% File Version: 0.27

\name{mice.1chain}
\alias{mice.1chain}
\alias{summary.mids.1chain}
\alias{plot.mids.1chain}
\alias{print.mids.1chain}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Multiple Imputation by Chained Equations using One Chain
}
\description{
This function modifies the \code{\link[mice:mice]{mice::mice}} function to
multiply impute a dataset using a long chain instead of multiple parallel chains
which is the approach employed in \code{mice::mice}.
}
\usage{
mice.1chain(data, burnin = 10, iter = 20, Nimp = 10, 
   method = vector("character", length = ncol(data)), 
   predictorMatrix = (1 - diag(1, ncol(data))), 
   visitSequence = (1:ncol(data))[apply(is.na(data), 2, any)], 
   form = vector("character", length = ncol(data)), 
   post = vector("character", length = ncol(data)), 
   defaultMethod = c("pmm", "logreg", "polyreg", "polr"), 
   diagnostics = TRUE, printFlag = TRUE, seed = NA, imputationMethod = NULL, 
   defaultImputationMethod = NULL, data.init = NULL, ...)
   
\method{summary}{mids.1chain}(object,...)

\method{print}{mids.1chain}(x, ...)

\method{plot}{mids.1chain}(x,plot.burnin=FALSE , ask=TRUE , ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
Numeric matrix
}
  \item{burnin}{
Number of burn-in iterations
}
  \item{iter}{
Total number of imputations (larger than \code{burnin})
}
  \item{Nimp}{
Number of imputations
}
  \item{method}{
See \code{\link[mice:mice]{mice::mice}}
}
  \item{predictorMatrix}{
See \code{\link[mice:mice]{mice::mice}}
}
  \item{visitSequence}{
See \code{\link[mice:mice]{mice::mice}}
}
  \item{form}{
See \code{\link[mice:mice]{mice::mice}}
}
  \item{post}{
See \code{\link[mice:mice]{mice::mice}}
}
  \item{defaultMethod}{
See \code{\link[mice:mice]{mice::mice}}
}
  \item{diagnostics}{
See \code{\link[mice:mice]{mice::mice}}
}
  \item{printFlag}{
See \code{\link[mice:mice]{mice::mice}}
}
  \item{seed}{
See \code{\link[mice:mice]{mice::mice}}
}
  \item{imputationMethod}{
See \code{\link[mice:mice]{mice::mice}}
}
  \item{defaultImputationMethod}{
See \code{\link[mice:mice]{mice::mice}}
}
  \item{data.init}{
See \code{\link[mice:mice]{mice::mice}}
}
\item{object}{Object of class \code{mids.1chain}}
\item{x}{Object of class \code{mids.1chain}}
\item{plot.burnin}{An optional logical indicating whether burnin iterations
should be included in the traceplot}
\item{ask}{An optional logical indicating a user request for viewing next plot}
  \item{\dots}{
See \code{\link[mice:mice]{mice::mice}}
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
A list with following entries

  \item{midsobj}{Objects of class \code{mids}}  
  \item{datlist}{List of multiply imputed datasets}
  \item{datalong}{Original and imputed dataset in the long format}
  \item{implist}{List of \code{mids} objects for every imputation}
  \item{chainMpar}{Trace of means for all imputed variables}
  \item{chainVarpar}{Trace of variances for all imputed variables}    
}


%\references{
%}
%Schafer, J. L., & Kang, J. (2008). Average causal effects from nonrandomized 
%studies: a practical guide and simulated example. 
%\emph{Psychological Methods}, \bold{13}, 279-313.


\author{
Alexander Robitzsch
}

\note{
Multiple imputation can also be used for determining causal effects
(see Example 3; Schafer & Kang, 2008).
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[mice:mice]{mice::mice}}
}


\examples{
#############################################################################
# EXAMPLE 1: One chain nhanes data
#############################################################################

library(mice)
data(nhanes, package="mice")
set.seed(9090)

# nhanes data in one chain
imp.mi1 <- miceadds::mice.1chain( nhanes , burnin=5 , iter=40 , Nimp=4 ,
    imputationMethod=rep("norm" , 4 ) )
summary(imp.mi1)       # summary of mids.1chain   
plot( imp.mi1 ) # trace plot excluding burnin iterations
plot( imp.mi1 , plot.burnin=TRUE ) # trace plot including burnin iterations

# select mids object
imp.mi2 <- imp.mi1$midsobj
summary(imp.mi2)	# summary of mids

# apply mice functionality lm.mids
mod <- with( imp.mi2 , stats::lm( bmi ~ age ) )
summary( mice::pool( mod ) )

\dontrun{
#############################################################################
# EXAMPLE 2: One chain (mixed data: numeric and factor)
#############################################################################

library(mice)
data(nhanes2, package="mice")
set.seed(9090)

# nhanes2 data in one chain
imp.mi1 <- miceadds::mice.1chain( nhanes2 , burnin=5 , iter=25 , Nimp=5 )
# summary
summary( imp.mi1$midsobj )

#############################################################################
# EXAMPLE 3: Multiple imputation with counterfactuals for estimating
#            causal effects (average treatment effects)
# Schafer, J. L., & Kang, J. (2008). Average causal effects from nonrandomized 
#    studies: a practical guide and simulated example. 
#    Psychological Methods, 13, 279-313.
#############################################################################

data(data.ma01)
dat <- data.ma01[ , 4:11]

# define counterfactuals for reading score for students with and
# without migrational background
dat$read.migrant1 <- ifelse( paste(dat$migrant) == 1 , dat$read , NA )
dat$read.migrant0 <- ifelse( paste(dat$migrant) == 0 , dat$read , NA )

# define imputation method
impmethod <- rep("pls" , ncol(dat) )
names(impmethod) <- colnames(dat)

# define predictor matrix
pm <- 4*(1 - diag( ncol(dat) ) )    # 4 - use all interactions
rownames(pm) <- colnames(pm) <- colnames(dat)
pm[ c( "read.migrant0" , "read.migrant1") , ] <- 0 
# do not use counterfactuals for 'read' as a predictor
pm[ , "read.migrant0"] <- 0
pm[ , "read.migrant1"] <- 0
# define control variables for creation of counterfactuals
pm[ c( "read.migrant0" , "read.migrant1") , c("hisei","paredu","female","books") ] <- 4
  ##   > pm
  ##                 math read migrant books hisei paredu female urban read.migrant1 read.migrant0
  ##   math             0    4       4     4     4      4      4     4             0             0
  ##   read             4    0       4     4     4      4      4     4             0             0
  ##   migrant          4    4       0     4     4      4      4     4             0             0
  ##   books            4    4       4     0     4      4      4     4             0             0
  ##   hisei            4    4       4     4     0      4      4     4             0             0
  ##   paredu           4    4       4     4     4      0      4     4             0             0
  ##   female           4    4       4     4     4      4      0     4             0             0
  ##   urban            4    4       4     4     4      4      4     0             0             0
  ##   read.migrant1    0    0       0     4     4      4      4     0             0             0
  ##   read.migrant0    0    0       0     4     4      4      4     0             0             0

# imputation using mice function and PLS imputation with
# predictive mean matching method 'pmm6'
imp <- mice::mice( dat , imputationMethod=impmethod , predictorMatrix=pm ,
            maxit=4 , m=5 , pls.impMethod="pmm5" )

#*** Model 1: Raw score difference
mod1 <- with( imp , stats::lm( read ~ migrant ) )
smod1 <- summary( mice::pool(mod1) )
  ##   > smod1
  ##                  est    se      t     df Pr(>|t|)  lo 95  hi 95 nmis    fmi lambda
  ##   (Intercept) 510.21 1.460 349.37 358.26        0 507.34 513.09   NA 0.1053 0.1004
  ##   migrant     -43.38 3.757 -11.55  62.78        0 -50.89 -35.87  404 0.2726 0.2498

#*** Model 2: ANCOVA - regression adjustment
mod2 <- with( imp , stats::lm( read ~ migrant + hisei + paredu + female + books) )
smod2 <- summary( mice::pool(mod2) )
  ##   > smod2
  ##                    est      se      t      df  Pr(>|t|)    lo 95   hi 95 nmis      fmi   lambda
  ##   (Intercept) 385.1506 4.12027 93.477 3778.66 0.000e+00 377.0725 393.229   NA 0.008678 0.008153
  ##   migrant     -29.1899 3.30263 -8.838   87.46 9.237e-14 -35.7537 -22.626  404 0.228363 0.210917
  ##   hisei         0.9401 0.08749 10.745  160.51 0.000e+00   0.7673   1.113  733 0.164478 0.154132
  ##   paredu        2.9305 0.79081  3.706   41.34 6.190e-04   1.3338   4.527  672 0.339961 0.308780
  ##   female       38.1719 2.26499 16.853 1531.31 0.000e+00  33.7291  42.615    0 0.041093 0.039841
  ##   books        14.0113 0.88953 15.751  154.71 0.000e+00  12.2541  15.768  423 0.167812 0.157123

#*** Model 3a: Estimation using counterfactuals
mod3a <- with( imp , stats::lm( I( read.migrant1 - read.migrant0) ~ 1 ) )
smod3a <- summary( mice::pool(mod3a) )
  ##   > smod3a
  ##                  est    se      t    df Pr(>|t|)  lo 95  hi 95 nmis    fmi lambda
  ##   (Intercept) -22.54 7.498 -3.007 4.315  0.03602 -42.77 -2.311   NA 0.9652 0.9521

#*** Model 3b: Like Model 3a but using student weights
mod3b <- with( imp , stats::lm( I( read.migrant1 - read.migrant0) ~ 1 , 
                        weights= data.ma01$studwgt ) )
smod3b <- summary( mice::pool(mod3b) )
  ##   > smod3b
  ##                  est    se      t  df Pr(>|t|)  lo 95  hi 95 nmis    fmi lambda
  ##   (Intercept) -21.88 7.605 -2.877 4.3  0.04142 -42.43 -1.336   NA 0.9662 0.9535

#*** Model 4: Average treatment effect on the treated (ATT, migrants) 
#             and non-treated (ATN, non-migrants)
mod4 <- with( imp , stats::lm( I( read.migrant1 - read.migrant0) ~ 0 + as.factor( migrant) )   )
smod4 <- summary( mice::pool(mod4) )
  ##   > smod4
  ##                          est    se      t    df Pr(>|t|)  lo 95   hi 95 nmis    fmi lambda
  ##   as.factor(migrant)0 -23.13 8.664 -2.669  4.27 0.052182 -46.59  0.3416   NA 0.9682 0.9562
  ##   as.factor(migrant)1 -19.95 5.198 -3.837 19.57 0.001063 -30.81 -9.0884   NA 0.4988 0.4501
# ATN = -23.13 and ATT = -19.95
}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Imputation}
\keyword{mids}
\keyword{summary}
\keyword{plot}
\keyword{mids.1chain}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
