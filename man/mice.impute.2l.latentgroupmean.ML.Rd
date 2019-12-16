%% File Name: mice.impute.2l.latentgroupmean.ml.Rd
%% File Version: 1.382

\name{mice.impute.2l.latentgroupmean.ml}
\alias{mice.impute.2l.latentgroupmean.ml}
\alias{mice.impute.2l.latentgroupmean.mcmc}
\alias{mice.impute.2l.groupmean}
\alias{mice.impute.2l.groupmean.elim}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Imputation of Latent and Manifest Group Means for Multilevel Data
}


\description{
The imputation method \code{2l.latentgroupmean} imputes a latent group mean
assuming an infinite population of subjects within a group
(Grund, Luedtke & Robitzsch, 2018; see also
Luedtke, Marsh, Robitzsch, Trautwein, Asparouhov & Muthen, 2008
 or Croon & van Veldhoven, 2007).
Therefore, unreliability of group means when treating subjects as
indicators is taken into account.

The imputation method \code{mice.impute.2l.groupmean} just imputes (i.e. computes)
the manifest group mean. See also
\code{\link[mice:mice.impute.2lonly.mean]{mice::mice.impute.2lonly.mean}}.

The imputation method \code{mice.impute.2l.groupmean.elim} computes the
group mean eliminating the subject under study from the calculation.
Therefore, this imputation method will lead to different values of
individuals within the same group.
}
\usage{
mice.impute.2l.latentgroupmean.ml(y, ry, x, type, pls.facs=NULL,
    imputationWeights=NULL, interactions=NULL, quadratics=NULL,
    EAP=FALSE, ...)

mice.impute.2l.latentgroupmean.mcmc(y, ry, x, type, pls.facs=NULL,
    imputationWeights=NULL, interactions=NULL, quadratics=NULL,
    mcmc.burnin=100, mcmc.adapt=100, mcmc.iter=1000, draw.fixed=TRUE, EAP=FALSE, ...)

mice.impute.2l.groupmean(y, ry, x, type, grmeanwarning=TRUE, ...)

mice.impute.2l.groupmean.elim(y, ry, x, type, ...)
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
\code{type=2} denotes a variable for which also a (latent) group mean
should be calculated. Predictors with \code{type=1} denote all other variables.
}
  \item{pls.facs}{
Number of factors used for PLS regression (optional).
}
  \item{imputationWeights}{
Optional vector of sample weights.
}
  \item{interactions}{
Vector of variable names used for creating interactions
}
  \item{quadratics}{
Vector of variable names used for creating quadratic terms
}
\item{draw.fixed}{Optional logical indicating whether parameters for fixed
effects should be sampled.}
\item{EAP}{Logical indicating whether EAPs should be used for imputation.
The default \code{FALSE} corresponds to sampling from the posterior
distribution.
}
\item{mcmc.burnin}{Number of MCMC burn-in iterations.}
\item{mcmc.adapt}{Number of MCMC iterations in adaptation phase.}
\item{mcmc.iter}{Total number of MCMC iterations.}
  \item{grmeanwarning}{
An optional logical indicating whether some group means cannot be calculated.
}
  \item{\dots}{
Further arguments to be passed.
}
}


\details{
The imputation of the latent group mean uses the
\code{\link[lme4:lmer]{lme4::lmer}} function of the \pkg{lme4} package
for \code{mice.impute.2l.latentgroupmean.ml} and the
\code{\link[MCMCglmm:MCMCglmm]{MCMCglmm::MCMCglmm}} function
of the \pkg{MCMCglmm} package for \code{mice.impute.2l.latentgroupmean.ml}.
Latent group mean imputation also follows
Mislevy (1991).
}

\value{
A vector of length \code{y} containing imputed group means.
}

\references{
Croon, M. A., & van Veldhoven, M. J. (2007). Predicting group-level outcome
variables from variables measured at the individual level: a latent variable
multilevel model. \emph{Psychological Methods, 12}(1), 45-57.
\doi{10.1037/1082-989X.12.1.45}

Grund, S., Luedtke, O., & Robitzsch, A. (2018). Multiple imputation of missing data at
level 2: A comparison of fully conditional and joint modeling in multilevel designs.
\emph{Journal of Educational and Behavioral Statistics, 43}(3), 316-353.
\doi{10.3102/1076998617738087}

Luedtke, O., Marsh, H. W., Robitzsch, A., Trautwein, U., Asparouhov, T., &
Muthen, B. (2008). The multilevel latent covariate model: a new, more
reliable approach to group-level effects in contextual studies.
\emph{Psychological Methods, 13}(3), 203-229.
\doi{10.1037/a0012869}

Mislevy, R. J. (1991). Randomization-based inference about latent variables
from complex samples. \emph{Psychometrika, 56}(2), 177-196.
\doi{10.1007/BF02294457}
}

%\author{
%Alexander Robitzsch, Simon Grund
%}

%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[mice:mice.impute.2lonly.mean]{mice::mice.impute.2lonly.mean}}
}

\examples{
\dontrun{
#############################################################################
# EXAMPLE 1: Two-level imputation data.ma05 dataset with imputation
#            of a latent group mean
#############################################################################

data(data.ma05)
dat <- data.ma05

# include manifest group mean for 'Mscore'
dat$M.Mscore <- NA
# include latent group group for 'Mscore'
dat$LM.Mscore <- NA    #=> LM: latent group mean

# define predictor matrix
predM <- mice::make.predictorMatrix(data=dat)
# exclude student ISs
predM[, "idstud"] <- 0
# idclass is the cluster identifier
predM[, "idclass" ] <- -2

# define imputation methods
impMethod <- mice::make.method(data=dat)
# initialize with norm
impMethod <- rep( "norm", length(impMethod) )
names(impMethod) <- names( imp$method )
impMethod[ c("idstud","idclass")] <- ""

#*****
# STUDENT LEVEL (Level 1)

# Use a random slope model for Dscore and Mscore as the imputation method.
# Here, variance homogeneity of residuals is assumed (contrary to
# the 2l.norm imputation method in the mice package).
impMethod[ c("Dscore", "Mscore") ] <- "2l.pan"
predM[ c("Dscore","Mscore"), "misei" ] <- 2    # random slopes on 'misei'
predM[, "idclass" ] <- -2

# For imputing 'manote' and 'denote' use contextual effects (i.e. cluster means)
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
impMethod[ "sprengel"] <- "2lonly.pmm2"
impMethod[ "groesse"] <- "2lonly.norm2"
predM[ c("sprengel","groesse"), "idclass" ] <- -2

# manifest group mean for Mscore
impMethod[ "M.Mscore" ] <- "2l.groupmean"
# latent group mean for Mscore
impMethod[ "LM.Mscore" ] <- "2l.latentgroupmean.ml"
predM[ "M.Mscore", "Mscore" ] <- 2

# covariates for latent group mean of 'Mscore'
predM[ "LM.Mscore", "Mscore" ] <- 2
predM[ "LM.Mscore", c( "Dscore", "sprengel" ) ] <- 1

# do imputations
imp <- mice::mice( dat, predictorMatrix=predM, m=3,  maxit=4,
         method=impMethod, allow.na=TRUE, pan.iter=100)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%% \keyword{mice imputation method}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
