%% File Name: mice.impute.2l.contextual.pmm.Rd
%% File Version: 0.37

\name{mice.impute.2l.contextual.pmm}
\alias{mice.impute.2l.contextual.pmm}
\alias{mice.impute.2l.contextual.norm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Imputation by Predictive Mean Matching or Normal Linear Regression
with Contextual Variables
}
\description{
This imputation method imputes a variable using linear regression
with predictive mean matching as the imputation method.
Including a contextual effects means that an aggregated variable at a cluster
level is included as a further covariate.
}

\usage{
mice.impute.2l.contextual.pmm(y, ry, x, type, imputationWeights=NULL,
     interactions=NULL, quadratics=NULL, pls.facs=NULL, ...)

mice.impute.2l.contextual.norm(y, ry, x, type, ridge=10^(-5),
   imputationWeights=NULL, interactions=NULL, quadratics=NULL, pls.facs=NULL, ...)
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
Matrix (\code{n} x \code{p}) of complete covariates.
}
  \item{type}{
Type of predictor variables. \code{type=-2} refers to the cluster variable,
\code{type=2} denotes a variable for which also a contextual effect is
included and \code{type=1} denotes all other variables which are
included as 'ordinary' predictors.
}
\item{imputationWeights}{
Optional vector of sample weights
}
  \item{interactions}{
Vector of variable names used for creating interactions
}
  \item{quadratics}{
Vector of variable names used for creating quadratic terms
}
\item{pls.facs}{Number of factors used in partial least dimension reduction
(if requested)}
  \item{\dots}{
Further arguments to be passed
}
  \item{ridge}{
Ridge parameter in the diagonal of \eqn{ \bold{X}'\bold{X}}
}
}


%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}

\value{
A vector of length \code{nmis=sum(!ry)} with imputed values.
}


%%\references{
%% ~put references to the literature/web site here ~
%%}

%%\note{
%%  ~~further notes~~
%%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
For imputations at level 2 variables see
\code{\link[mice:mice.impute.2lonly.norm]{mice::mice.impute.2lonly.norm}}
and \code{\link[mice:mice.impute.2lonly.pmm]{mice::mice.impute.2lonly.pmm}}.
}


\examples{
\dontrun{
#############################################################################
# EXAMPLE 1: Sequential hierarchical imputation for data.ma05 dataset
#############################################################################

data(data.ma05)
dat <- data.ma05

# define predictor matrix
predM <- mice::make.predictorMatrix(data=dat)
# exclude student IDs
predM[, "idstud"] <- 0
# define idclass as the cluster variable (type=-2)
predM[, "idclass" ] <- -2

# initialize with norm method
impMethod <- mice::make.method(data=dat)
names(impMethod) <- names( imp0$method )
impMethod[ c("idstud","idclass")] <- ""

#*****
# STUDENT LEVEL (Level 1)

# Use a random slope model for Dscore and Mscore as the imputation method.
# Here, variance homogeneity of residuals is assumed (contrary to
# the 2l.norm imputation method in the mice package).
impMethod[ c("Dscore", "Mscore") ] <- "2l.pan"
predM[ c("Dscore","Mscore"), "misei" ] <- 2    # random slopes on 'misei'
predM[, "idclass" ] <- -2

# For imputing 'manote' and 'denote' use contextual effects (i.e. cluszer means)
# of variables 'misei' and 'migrant'
impMethod[ c("denote", "manote") ] <- "2l.contextual.pmm"
predM[ c("denote", "manote"), c("misei","migrant")] <- 2

# Use no cluster variable 'idclass' for imputation of 'misei'
impMethod[ "misei"] <- "norm"
predM[ "misei", "idclass"] <- 0 # use no multilevel imputation model

# Variable migrant: contextual effects of Dscore and misei
impMethod[ "migrant"] <- "2l.contextual.pmm"
predM[ "migrant", c("Dscore",  "misei" ) ] <- 2
predM[ "migrant", "idclass" ] <- -2

#****
# CLASS LEVEL (Level 2)
# impute 'sprengel' and 'groesse' at the level of classes
impMethod[ "sprengel"] <- "2lonly.pmm"
impMethod[ "groesse"] <- "2lonly.norm"
predM[ c("sprengel","groesse"), "idclass" ] <- -2

# do imputation
imp <- mice::mice( dat, predictorMatrix=predM, m=3,  maxit=4,
           method=impMethod, paniter=100)
summary(imp)

#**** imputation model 2 with PLS dimension reduction

# define some interaction effects
interactions <- list( manote=c("migrant", "misei") )
# number of PLS factors (5 factors)
pls.facs <- list( manote=5 )

# do imputation
imp2 <- mice::mice( dat, predictorMatrix=predM, interactions=interactions,
            pls.facs=pls.facs, method=impMethod, paniter=100)
summary(imp2)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%% \keyword{mice imputation method}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
