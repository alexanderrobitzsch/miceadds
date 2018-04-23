%% File Name: mice.impute.eap.Rd
%% File Version: 0.17

\name{mice.impute.eap}
\alias{mice.impute.eap}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Imputation of a Variable with a Known Posterior Distribution
}
\description{
This function imputes values of a variable for which the
mean and the standard deviation of the posterior distribution
is known.
}

\usage{
mice.impute.eap(y, ry, x, eap, ...)
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
  \item{eap}{
List with means and standard deviations of the posterior distribution
(see Examples). If for multiple variables posterior distributions are
known, then it is a list named in which each list entry is named according
th variable to be imputed and each list entry contains the variable's
EAP and standard deviation of the EAP.
}
  \item{\dots}{
Further arguments to be passed
}
}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}

\value{
A vector of length \code{nmis=sum(!ry)} with imputed values.
}

%\references{
%% ~put references to the literature/web site here ~
%}


%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}

\examples{
\dontrun{
#############################################################################
# EXAMPLE 1: Imputation based on known posterior distribution
#############################################################################	
	
data(data.ma03)
dat <- data.ma03

# definiere variable 'math_PV' as the plausible value imputation of math
dat$math_PV <- NA
vars <- colnames(dat)
dat1 <- as.matrix( dat[,vars] )

# define imputation methods
impmethod <- rep( "pmm" , length(vars ))
names(impmethod) <- vars
# define plausible value imputation based on EAP and SEEAP for 'math_PV'
impmethod[ "math_PV" ] <- "eap"
eap <- list( "math_PV" = list( "M" = dat$math_EAP  , "SE" = dat$math_SEEAP  ) )
# define predictor matrix
pM <- 1 - diag(1,length(vars))
rownames(pM) <- colnames(pM) <- vars
pM[,c("idstud","math_EAP" , "math_SEEAP") ] <- 0     
    # remove some variables from imputation model

# imputation using three parallel chains
imp1 <- mice::mice( dat1 , m=3 , maxit=5 , imputationMethod=impmethod ,  
               predictorMatrix = pM , allow.na =TRUE , eap=eap )
summary(imp1)   # summary

# imputation using one long chain
imp2 <- miceadds::mice.1chain( dat1 , burnin=10 , iter=20 , Nimp =3, 
            imputationMethod=impmethod, predictorMatrix = pM ,  allow.na =TRUE , eap=eap)
summary(imp2)    # summary
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{mice imputation method}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
