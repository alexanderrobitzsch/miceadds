%% File Name: mice.impute.plausible.values.Rd
%% File Version: 0.45

\name{mice.impute.plausible.values}
\alias{mice.impute.plausible.values}


\title{
Plausible Value Imputation using Classical Test Theory and
Based on Individual Likelihood
}

\description{
This imputation function performs unidimensional plausible value
imputation if (subject-wise) measurement errors or the reliability
of the scale is known (Mislevy, 1991; see also Asparouhov & Muthen, 2010;
Blackwell, Honaker & King, 2011, 2017a, 2017b).
The function also allows the input of an individual likelihood
obtained by fitting an item response model.
}

\usage{
mice.impute.plausible.values(y, ry, x, type, alpha=NULL,
    alpha.se=0, scale.values=NULL, sig.e.miss=1e+06,
    like=NULL, theta=NULL, normal.approx=NULL,
    pviter=15, imputationWeights=rep(1, length(y)), plausible.value.print=TRUE,
    pls.facs=NULL, interactions=NULL, quadratics=NULL, extract_data=TRUE,
    control_latreg=list( progress=FALSE, ridge=1e-5 ),  ...)
}

%% like=NULL, theta=NULL, normal.approx=NULL,
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
    Matrix (\code{n} \eqn{\times} \code{p}) of complete covariates.
}
  \item{type}{
Type of predictor variables. \code{type=3} refers to items belonging to
a scale to be imputed. A cluster (grouping) variable is defined by
\code{type=-2}. If for some predictors, the cluster means should also
be included as predictors, then specify \code{type=2} (see Imputation
Model 3 of Example 1).
  }
\item{alpha}{
  A known reliability estimate. An optional standard error of the estimate
  can be provided in \code{alpha.se}
}
  \item{alpha.se}{
Optional numeric value of the standard error of the \code{alpha}
reliability estimate if in every iteration a new reliability
should be sampled.
}
  \item{scale.values}{
A list consisting of scale values of scale values and its
corresponding standard errors (see Example 1).
}
  \item{sig.e.miss}{
A standard error of measurement for cases with missing values on a scale
}
\item{like}{Individual likelihood evaluated at \code{theta}}
\item{theta}{Grid of unidimensional latent variable}
\item{normal.approx}{Logical indicating whether the
individual posterior should be approximated by a normal distribution}
  \item{pviter}{
Number of iterations in each imputation which should be run until the
plausible values are drawn
}
  \item{imputationWeights}{
Optional vector of sample weights
}
  \item{plausible.value.print}{
An optional logical indicating whether some information about the
plausible value imputation should be printed at the console
}
  \item{pls.facs}{
Number of PLS factors if PLS dimension reduction is used
}
  \item{interactions}{
Vector of variable names used for creating interactions
}
  \item{quadratics}{
Vector of variable names used for creating quadratic terms
}
\item{extract_data}{Logical indicating whether input data should be extracted
from parent environment within \code{\link[mice:mice]{mice::mice}} routine}
\item{control_latreg}{Control arguments for \code{\link[TAM:tam.latreg]{TAM::tam.latreg}}}
  \item{\dots}{
Further objects to be passed
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
A vector of length \code{nrow(x)} containing imputed plausible values.
}


\references{
Asparouhov, T., & Muthen, B. (2010). \emph{Plausible values for latent variables
using Mplus}. Technical Report. \url{https://www.statmodel.com/papers.shtml}

Blackwell, M., Honaker, J., & King, G. (2011).
\emph{Multiple overimputation: A unified approach to measurement error and
missing data}. Technical Report.

Blackwell, M., Honaker, J., & King, G. (2017a).
A unified approach to measurement error and missing data: Overview and
applications. \emph{Sociological Methods & Research}, \bold{46}(3), 303-341.

Blackwell, M., Honaker, J., & King, G. (2017b).
A unified approach to measurement error and missing data: Details and
extensions. \emph{Sociological Methods & Research}, \bold{46}(3), 342-369.

Mislevy, R. J. (1991). Randomization-based inference about latent variables
from complex samples. \emph{Psychometrika}, \bold{56}, 177-196.
}



\note{
Plausible value imputation is also known as multiple overimputation
(Blackwell, Honaker & King, 2016a, 2016b) which is implemented
in the \pkg{Amelia} package, see \code{\link[Amelia:moPrep]{Amelia::moPrep}}
and \code{\link[Amelia:amelia]{Amelia::amelia}}.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See \code{\link[TAM:tam.latreg]{TAM::tam.latreg}} for fitting latent regression
models.
}


\examples{
\dontrun{
#############################################################################
# EXAMPLE 1: Plausible value imputation for data.ma04 | 2 scales
#############################################################################

data(data.ma04, package="miceadds")
dat <- data.ma04

# Scale 1 consists of items A1,...,A4
# Scale 2 consists of items B1,...,B5
dat$scale1 <- NA
dat$scale2 <- NA

#** inits imputation method and predictor matrix
res <- miceadds::mice_inits(dat, ignore=c("group") )
predM <- res$predictorMatrix
impMethod <- res$method
impMethod <- gsub("pmm", "norm", impMethod )

# look at missing proportions
colSums( is.na(dat) )

# redefine imputation methods for plausible value imputation
impMethod[ "scale1" ] <- "plausible.values"
predM[ "scale1",  ] <- 1
predM[ "scale1", c("A1", "A2",  "A3", "A4" ) ] <- 3
    # items corresponding to a scale should be declared by a 3 in the predictor matrix
impMethod[ "scale2" ] <- "plausible.values"
predM[,"scale2"  ] <- 0
predM[ "scale2",  c("A2","A3","A4","V6","V7") ] <- 1
diag(predM) <- 0

# use imputed scale values as predictors for V5, V6 and V7
predM[ c("V5","V6","V7"), c("scale1","scale2" ) ] <- 1
# exclude for V5, V6 and V7 the items of scales A and B as predictors
predM[ c("V5","V6","V7"), c( paste0("A",2:4), paste0("B",1:5) ) ] <- 0
# exclude 'group' as a predictor
predM[,"group"] <- 0

# look at imputation method and predictor matrix
impMethod
predM

#-------------------------------
# Parameter for imputation
#***
# scale 1 (A1,...,A4)
# known Cronbach's Alpha
alpha <- NULL
alpha <- list( "scale1"=.8 )
alpha.se <- list( "scale1"=.05 )  # sample alpha with a standard deviation of .05

#***
# scale 2 (B1,...,B5)
# means and SE's of scale scores are assumed to be known
M.scale2 <- rowMeans( dat[, paste("B",1:5,sep="")  ] )
# M.scale2[ is.na( m1) ] <- mean( M.scale2, na.rm=TRUE )
SE.scale2 <- rep( sqrt( stats::var(M.scale2,na.rm=T)*(1-.8) ), nrow(dat) )
#=> heterogeneous measurement errors are allowed
scale.values <- list( "scale2"=list( "M"=M.scale2, "SE"=SE.scale2 ) )

#*** Imputation Model 1: Imputation four using parallel chains
imp1 <- mice::mice( dat, predictorMatrix=predM, m=4, maxit=5,
          alpha.se=alpha.se, method=impMethod,  allow.na=TRUE, alpha=alpha,
          scale.values=scale.values  )
summary(imp1)

# extract first imputed dataset
dat11 <- mice::complete( imp, 1 )

#*** Imputation Model 2: Imputation using one long chain
imp2 <- miceadds::mice.1chain( dat, predictorMatrix=predM, burnin=10, iter=20, Nimp=4,
          alpha.se=alpha.se, method=impMethod,  allow.na=TRUE, alpha=alpha,
          scale.values=scale.values )
summary(imp2)

#-------------
#*** Imputation Model 3: Imputation including  group level variables

# use group indicator for plausible value estimation
predM[ "scale1", "group" ] <- -2
# V7 and B1 should be aggregated at the group level
predM[ "scale1", c("V7","B1") ] <- 2
predM[ "scale2", "group" ] <- -2
predM[ "scale2", c("V7","A1") ] <- 2

# perform single imputation (m=1)
imp <- mice::mice( dat, predictorMatrix=predM, m=1, maxit=10,
            method=impMethod,  allow.na=TRUE, alpha=alpha,
            scale.values=scale.values )
dat10 <- mice::complete(imp)

# multilevel model
library(lme4)
mod <- lme4::lmer( scale1 ~ ( 1 | group), data=dat11 )
summary(mod)

mod <- lme4::lmer( scale1 ~ ( 1 | group), data=dat10)
summary(mod)

#############################################################################
# EXAMPLE 2: Plausible value imputation with chained equations
#############################################################################

# - simulate a latent variable theta and dichotomous item responses
# - two covariates X in which the second covariate has measurement error

library(sirt)
library(TAM)
library(lavaan)

set.seed(7756)
N <- 2000    # number of persons
I <- 10     # number of items

# simulate covariates
X <- MASS::mvrnorm( N, mu=c(0,0), Sigma=matrix( c(1,.5,.5,1),2,2 ) )
colnames(X) <- paste0("X",1:2)
# second covariate with measurement error with variance var.err
var.err <- .3
X.err <- X
X.err[,2] <- X[,2] + stats::rnorm(N, sd=sqrt(var.err) )
# simulate theta
theta <- .5*X[,1] + .4*X[,2] + stats::rnorm( N, sd=.5 )
# simulate item responses
itemdiff <- seq( -2, 2, length=I)  # item difficulties
dat <- sirt::sim.raschtype( theta, b=itemdiff )

#***********************
#*** Model 0: Regression model with true variables
mod0 <- stats::lm( theta ~ X )
summary(mod0)

#**********************
# plausible value imputation for abilities and error-prone
# covariates using the mice package

# creating the likelihood for plausible value for abilities
mod11 <- TAM::tam.mml( dat )
likePV <- IRT.likelihood(mod11)
# creating the likelihood for error-prone covariate X2
# The known measurement error variance is 0.3.
lavmodel <- "
  X2true=~ 1*X2
  X2 ~~ 0.3*X2
    "
mod12 <- lavaan::cfa( lavmodel, data=as.data.frame(X.err) )
summary(mod12)
likeX2 <- IRTLikelihood.cfa( data=X.err, cfaobj=mod12)
str(likeX2)

#-- create data input for mice package
data <- data.frame( "PVA"=NA, "X1"=X[,1], "X2"=NA  )
vars <- colnames(data)
V <- length(vars)
predictorMatrix <- 1 - diag(V)
rownames(predictorMatrix) <- colnames(predictorMatrix) <- vars
imputationMethod <- rep("norm", V )
names(imputationMethod) <- vars
imputationMethod[c("PVA","X2")] <- "plausible.values"

#-- create argument lists for plausible value imputation
# likelihood and theta grid of plausible value derived from IRT model
like <- list( "PVA"=likePV, "X2"=likeX2 )
theta <- list( "PVA"=attr(likePV,"theta"),
                "X2"=attr(likeX2, "theta") )
#-- initial imputations
data.init <- data
data.init$PVA <- mod11$person$EAP
data.init$X2 <- X.err[,"X2"]

#-- imputation using the mice and miceadds package
imp1 <- mice::mice( as.matrix(data), predictorMatrix=predictorMatrix, m=4,
            maxit=6, method=imputationMethod,  allow.na=TRUE,
            theta=theta, like=like, data.init=data.init )
summary(imp1)

# compute linear regression
mod4a <- with( imp1, stats::lm( PVA ~ X1 + X2 ) )
summary( mice::pool(mod4a) )

#############################################################################
# EXAMPLE 3: Plausible value imputation with known error variance
#############################################################################

#---- simulate data
set.seed(987)
N <- 1000         # number of persons
var_err <- .4     # error variance
dat <- data.frame( x1=stats::rnorm(N), x2=stats::rnorm(N) )
dat$theta <- .3 * dat$x1 - .5*dat$x2 + stats::rnorm(N)
dat$y <- dat$theta + stats::rnorm( N, sd=sqrt(var_err) )

#-- linear regression for measurement-error-free data
mod0a <- stats::lm( theta ~ x1 + x2, data=dat )
summary(mod0a)
#-- linear regression for data with measurement error
mod0b <- stats::lm( y ~ x1 + x2, data=dat )
summary(mod0b)

#-- process data for imputation

dat1 <- dat
dat1$theta <- NA
scale.values <- list( "theta"=list( "M"=dat$y, "SE"=rep(sqrt(var_err),N )))
dat1$y <- NULL

cn <- colnames(dat1)
V <- length(cn)
imputationMethod <- rep("", length(cn) )
names(imputationMethod) <- cn
imputationMethod["theta"] <- "plausible.values"

#-- imputation in mice
imp <- mice::mice( dat1, maxit=1, m=5, allow.na=TRUE, method=imputationMethod,
            scale.values=scale.values )
summary(imp)

#-- inspect first dataset
summary( mice::complete(imp, action=1) )

#-- linear regression based on imputed datasets
mod1 <- with(imp, stats::lm( theta ~ x1 + x2 ) )
summary( mice::pool(mod1) )
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{mice imputation method}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
