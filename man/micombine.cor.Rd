%% File Name: micombine.cor.Rd
%% File Version: 0.416

\name{micombine.cor}
\alias{micombine.cor}
\alias{micombine.cov}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Inference for Correlations and Covariances for Multiply Imputed Datasets
}
\description{
Statistical inference for correlations and covariances
for multiply imputed datasets
}
\usage{
micombine.cor(mi.res, variables=NULL, conf.level=0.95,
     method="pearson", nested=FALSE, partial=NULL )

micombine.cov(mi.res, variables=NULL, conf.level=0.95,
     nested=FALSE )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mi.res}{
Object of class \code{mids} or \code{mids.1chain}
}
  \item{variables}{
Indices of variables for selection
}
  \item{conf.level}{
Confidence level
}
\item{method}{Method for calculating correlations. Must be one
of \code{"pearson"} or \code{"spearman"}. The default is the
calculation of the Pearson correlation.
}
\item{nested}{Logical indicating whether the input dataset stems from
a nested multiple imputation.}
\item{partial}{Formula object for computing partial correlations. The
terms which should be residualized are written in the formula
object \code{partial}. Alternatively, it can be a vector of variables.}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
A data frame containing the coefficients (\code{r}, \code{cov}) and its
corresponding standard error (\code{rse}, \code{cov_se}),
fraction of missing information
(\code{fmi}) and a \eqn{t} value (\code{t}).

The corresponding coefficients can also be obtained as matrices
by requesting \code{attr(result,"r_matrix")}.
}
%\references{
%% ~put references to the literature/web site here ~
%}

%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See \code{\link[stats:cor.test]{stats::cor.test}} for testing
correlation coefficients.
}

\examples{
\dontrun{
#############################################################################
# EXAMPLE 1: nhanes data | combination of correlation coefficients
#############################################################################

library(mice)
data(nhanes, package="mice")
set.seed(9090)

# nhanes data in one chain
imp.mi <- miceadds::mice.1chain( nhanes, burnin=5, iter=20, Nimp=4,
                  method=rep("norm", 4) )

# correlation coefficients of variables 4, 2 and 3 (indexed in nhanes data)
res <- miceadds::micombine.cor(mi.res=imp.mi, variables=c(4,2,3) )
  ##     variable1 variable2       r    rse fisher_r fisher_rse    fmi       t      p
  ##   1       chl       bmi  0.2458 0.2236   0.2510     0.2540 0.3246  0.9879 0.3232
  ##   2       chl       hyp  0.2286 0.2152   0.2327     0.2413 0.2377  0.9643 0.3349
  ##   3       bmi       hyp -0.0084 0.2198  -0.0084     0.2351 0.1904 -0.0358 0.9714
  ##     lower95 upper95
  ##   1 -0.2421  0.6345
  ##   2 -0.2358  0.6080
  ##   3 -0.4376  0.4239

# extract matrix with correlations and its standard errors
attr(res, "r_matrix")
attr(res, "rse_matrix")

# inference for covariance
res2 <- miceadds::micombine.cov(mi.res=imp.mi, variables=c(4,2,3) )

# inference can also be conducted for non-imputed data
res3 <- miceadds::micombine.cov(mi.res=nhanes, variables=c(4,2,3) )

# partial correlation residualizing bmi and chl
res4 <- miceadds::micombine.cor(mi.res=imp.mi, variables=c("age","hyp" ),
                  partial=~bmi+chl )
res4
# alternatively, 'partial' can also be defined as c('age','hyp')

#############################################################################
# EXAMPLE 2: nhanes data | comparing different correlation coefficients
#############################################################################

library(psych)
library(mitools)

# imputing data
imp1 <- mice::mice( nhanes,  method=rep("norm", 4 ) )
summary(imp1)

#*** Pearson correlation
res1 <- miceadds::micombine.cor(mi.res=imp1, variables=c(4,2) )

#*** Spearman rank correlation
res2 <- miceadds::micombine.cor(mi.res=imp1, variables=c(4,2),  method="spearman")

#*** Kendalls tau
# test of computation of tau for first imputed dataset
dat1 <- mice::complete(imp1, action=1)
tau1 <- psych::corr.test(x=dat1[,c(4,2)], method="kendall")
tau1$r[1,2]    # estimate
tau1$se     # standard error

# results of Kendalls tau for all imputed datasets
res3 <- with( data=imp1,
        expr=psych::corr.test( x=cbind( chl, bmi ), method="kendall") )
# extract estimates
betas <- lapply( res3$analyses, FUN=function(ll){ ll$r[1,2] } )
# extract variances
vars <- lapply( res3$analyses, FUN=function(ll){ (ll$se[1,2])^2 } )
# Rubin inference
tau_comb <- mitools::MIcombine( results=betas, variances=vars )
summary(tau_comb)

#############################################################################
# EXAMPLE 3: Inference for correlations for nested multiply imputed datasets
#############################################################################

library(BIFIEsurvey)
data(data.timss4, package="BIFIEsurvey" )
datlist <- data.timss4

# object of class nested.datlist
datlist <- miceadds::nested.datlist_create(datlist)
# inference for correlations
res2 <- miceadds::micombine.cor(mi.res=datlist, variables=c("lang", "migrant", "ASMMAT"))
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%% \keyword{Decriptives}
%% \keyword{mids}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
