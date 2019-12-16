%% File Name: NMIwaldtest.Rd
%% File Version: 0.271

\name{NMIwaldtest}
\alias{NMIwaldtest}
\alias{create.designMatrices.waldtest}
\alias{summary.NMIwaldtest}
\alias{MIwaldtest}
\alias{summary.MIwaldtest}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Wald Test for Nested Multiply Imputed Datasets
}

\description{
Performs a Wald test for nested multiply imputed datasets (\code{NMIwaldtest})
and ordinary multiply imputed datasets (\code{MIwaldtest}),
see Reiter and Raghunathan (2007).
The corresponding statistic is also called the \eqn{D_1} statistic.

The function \code{create.designMatrices.waldtest} is a helper function
for the creation of design matrices.
}

\usage{
NMIwaldtest(qhat, u, Cdes=NULL, rdes=NULL, testnull=NULL)

MIwaldtest(qhat, u, Cdes=NULL, rdes=NULL, testnull=NULL)

\method{summary}{NMIwaldtest}(object, digits=4,...)

\method{summary}{MIwaldtest}(object, digits=4,...)

create.designMatrices.waldtest(pars, k)
}


%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{qhat}{
List or array of estimated parameters
}
  \item{u}{
List or array of estimated covariance matrices of parameters
}
  \item{Cdes}{
Design matrix \eqn{C} for parameter test (see Details)
}
  \item{rdes}{
Constant vector \eqn{r} (see Details)
}
  \item{testnull}{
Vector containing names of parameters which should be tested for
a parameter value of zero.
}
\item{object}{Object of class \code{NMIwaldtest}}
\item{digits}{Number of digits after decimal for print}
\item{\dots}{Further arguments to be passed}
\item{pars}{Vector of parameter names}
\item{k}{Number of linear hypotheses which should be tested}
}

\details{
The Wald test is performed for a linear hypothesis \eqn{C \bold{\theta}=r}
for a parameter vector \eqn{\bold{\theta}}.
}



\value{
List with following entries
  \item{stat}{Data frame with test statistic}
  \item{qhat}{Transformed parameter according to linear hypothesis}
  \item{u}{Covariance matrix of transformed parameters}
}

\references{
Reiter, J. P. and Raghunathan, T. E. (2007). The multiple adaptations of multiple
imputation. \emph{Journal of the American Statistical Association, 102}(480), 1462-1471.
\doi{10.1198/016214507000000932}
}


\note{
The function \code{create.designMatrices.waldtest} is a helper
function for the creation of design matrices.
 }

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{NMIcombine}}
}


\examples{
\dontrun{
#############################################################################
# EXAMPLE 1: Nested multiple imputation and Wald test | TIMSS data
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

#**** Model 1: Linear regression with interaction effects
res1 <- with( imp1, stats::lm( likesc ~ female*migrant + female*books  ) )
pres1 <- miceadds::pool.mids.nmi( res1 )
summary(pres1)

# test whether both interaction effects equals zero
pars <- dimnames(pres1$qhat)[[3]]
des <- miceadds::create.designMatrices.waldtest( pars=pars, k=2)
Cdes <- des$Cdes
rdes <- des$rdes
Cdes[1, "female:migrant"] <- 1
Cdes[2, "female:books"] <- 1
wres1 <- miceadds::NMIwaldtest( qhat=pres1$qhat, u=pres1$u, Cdes=Cdes, rdes=rdes )
summary(wres1)

# a simpler specification is the use of "testnull"
testnull <- c("female:migrant", "female:books")
wres1b <- miceadds::NMIwaldtest( qhat=qhat, u=u, testnull=testnull )
summary(wres1b)

#**** Model 2: Multivariate linear regression
res2 <- with( imp1, stats::lm( cbind( ASMMAT, ASSSCI ) ~
                           0 + I(1*(female==1)) + I(1*(female==0))   ) )
pres2 <- miceadds::pool.mids.nmi( res2 )
summary(pres2)

# test whether both gender differences equals -10 points
pars <- dimnames(pres2$qhat)[[3]]
  ##  > pars
  ##  [1] "ASMMAT:I(1 * (female==1))" "ASMMAT:I(1 * (female==0))"
  ##  [3] "ASSSCI:I(1 * (female==1))" "ASSSCI:I(1 * (female==0))"

des <- miceadds::create.designMatrices.waldtest( pars=pars, k=2)
Cdes <- des$Cdes
rdes <- c(-10,-10)
Cdes[1, "ASMMAT:I(1*(female==1))"] <- 1
Cdes[1, "ASMMAT:I(1*(female==0))"] <- -1
Cdes[2, "ASSSCI:I(1*(female==1))"] <- 1
Cdes[2, "ASSSCI:I(1*(female==0))"] <- -1

wres2 <- miceadds::NMIwaldtest( qhat=pres2$qhat, u=pres2$u, Cdes=Cdes, rdes=rdes )
summary(wres2)

# test only first hypothesis
wres2b <- miceadds::NMIwaldtest( qhat=pres2$qhat, u=pres2$u, Cdes=Cdes[1,,drop=FALSE],
                         rdes=rdes[1] )
summary(wres2b)

#############################################################################
# EXAMPLE 2: Multiple imputation and Wald test | TIMSS data
#############################################################################

library(BIFIEsurvey)
data(data.timss2, package="BIFIEsurvey" )
dat <- data.timss2[[1]]
dat <- dat[, - c(1:4) ]

# perform multiple imputation
imp <- mice::mice( dat, m=6, maxit=3 )

# define analysis model
res1 <- with( imp, lm( likesc ~ female*migrant + female*books  ) )
pres1 <- mice::pool( res1 )
summary(pres1)

# Wald test for zero interaction effects
qhat <- mitools::MIextract(res1$analyses, fun=coef)
u <- mitools::MIextract(res1$analyses, fun=vcov)
pars <- names(qhat[[1]])
des <- miceadds::create.designMatrices.waldtest( pars=pars, k=2)
Cdes <- des$Cdes
rdes <- des$rdes
Cdes[1, "female:migrant"] <- 1
Cdes[2, "female:books"] <- 1

# apply MIwaldtest function
wres1 <- miceadds::MIwaldtest( qhat, u, Cdes, rdes )
summary(wres1)

# use again "testnull"
testnull <- c("female:migrant", "female:books")
wres1b <- miceadds::MIwaldtest( qhat=qhat, u=u, testnull=testnull )
summary(wres1b)

#***** linear regression with cluster robust standard errors

# convert object of class mids into a list object
datlist_imp <- miceadds::mids2datlist( imp )
# define cluster
idschool <- as.numeric( substring( data.timss2[[1]]$IDSTUD, 1, 5 ) )
# linear regression
res2 <- lapply( datlist_imp, FUN=function(data){
           miceadds::lm.cluster( data=data, formula=likesc ~ female*migrant + female*books,
                            cluster=idschool ) } )
# extract parameters and covariance matrix
qhat <- lapply( res2, FUN=function(rr){ coef(rr) } )
u <- lapply( res2, FUN=function(rr){ vcov(rr) } )
# perform Wald test
wres2 <- miceadds::MIwaldtest( qhat, u, Cdes, rdes )
summary(wres2)
}
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%% \keyword{Nested multiple imputation}
%% \keyword{summary}% __ONLY ONE__ keyword per line
