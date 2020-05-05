%% File Name: mice.impute.smcfcs.Rd
%% File Version: 0.26

\name{mice.impute.smcfcs}
\alias{mice.impute.smcfcs}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Substantive Model Compatible Multiple Imputation (Single Level)
}

\description{
Computes substantive model compatible multiple imputation (Bartlett et al., 2015;
Bartlett & Morris, 2015). Several regression functions are allowed (see \code{dep_type}).
}

\usage{
mice.impute.smcfcs(y, ry, x, wy=NULL, sm, dep_type="norm", sm_type="norm",
       fac_sd_proposal=1, mh_iter=20, ...)
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
\item{wy}{Logical vector indicating positions where imputations
should be conducted.}
\item{sm}{Formula for substantive model.}
\item{dep_type}{Distribution type for variable which is imputed.
Possible choices are \code{"norm"} (normal distribution),
\code{"lognorm"} (lognormal distribution),
\code{"yj"} (Yeo-Johnson distribution,
see \code{\link[mdmb:yjt_regression]{mdmb::yjt_regression}}), \code{"bc"}
(Box-Cox distribution, see
\code{\link[mdmb:bct_regression]{mdmb::bct_regression}}), \code{"logistic"}
(logistic distribution).
}
\item{sm_type}{Distribution type for dependent variable in substantive model.
One of the distribution mentioned in \code{dep_type} can be chosen.}
\item{fac_sd_proposal}{Starting value for factor of standard deviation in
Metropolis-Hastings sampling.}
\item{mh_iter}{Number iterations in Metropolis-Hasting sampling}
\item{\dots}{Further arguments to be passed}
}

\details{
Imputed values are drawn based on a Metropolis-Hastings sampling algorithm
in which the standard deviation of the proposal distribution is adaptively tuned.
}

\value{
A vector of length \code{nmis=sum(!ry)} with imputed values.
}

\references{
Bartlett, J. W., & Morris, T. P. (2015). Multiple imputation of covariates by
substantive-model compatible fully conditional specification.
\emph{Stata Journal, 15}(2), 437-456.

Bartlett, J. W., Seaman, S. R., White, I. R., Carpenter, J. R., & Alzheimer's Disease
Neuroimaging Initiative (2015). Multiple imputation of covariates by fully
conditional specification: Accommodating the substantive model.
\emph{Statistical Methods in Medical Research, 24}(4), 462-487.
\doi{10.1177/0962280214521348}
}


%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See the \pkg{smcfcs} package for an alternative implementation of substantive model
multiple imputation in a fully conditional specification approach.
}

\examples{
\dontrun{
#############################################################################
# EXAMPLE 1: Substantive model with interaction effects
#############################################################################

library(mice)
library(mdmb)

#--- simulate data
set.seed(98)
N <- 1000
x <- stats::rnorm(N)
z <- 0.5*x + stats::rnorm(N, sd=.7)
y <- stats::rnorm(N, mean=.3*x - .2*z + .7*x*z, sd=1 )
dat <- data.frame(x,z,y)
dat[ seq(1,N,3), c("x","y") ] <- NA


#--- define imputation methods
imp <- mice::mice(dat, maxit=0)
method <- imp$method
method["x"] <- "smcfcs"

# define substantive model
sm <- y ~ x*z
# define formulas for imputation models
formulas <- as.list( rep("",ncol(dat)))
names(formulas) <- colnames(dat)
formulas[["x"]] <- x ~ z
formulas[["y"]] <- sm
formulas[["z"]] <- z ~ 1

#- Yeo-Johnson distribution for x
dep_type <- list()
dep_type$x <- "yj"

#-- do imputation
imp <- mice::mice(dat, method=method, sm=sm, formulas=formulas, m=1, maxit=10,
                   dep_type=dep_type)
summary(imp)

#############################################################################
# EXAMPLE 2: Substantive model with quadratic effects
#############################################################################

#** simulate data with missings
set.seed(50)
n <- 1000
x <- stats::rnorm(n)
z <- stats::rnorm(n)
y <- 0.5 * z + x + x^2 + stats::rnorm(n)
mm <- stats::runif(n)
x[sample(1:n, size=370, prob=mm)] <- NA
z[sample(1:n, size=480, prob=mm)] <- NA
y[sample(1:n, size=500, prob=mm)] <- NA

df <- data.frame(x=x,y=y,z=z)

#** imputation
imp <- mice::mice(df, method="smcfcs", sm=y ~ z + x + I(x^2), m=6, maxit=10)
summary(imp)

#** analysis
summary(mice::pool(with(imp, stats::lm(y ~ z + x + I(x^2)))))

#** imputation using the smcfcs package
df$x_sq <- df$x^2
nonmice <- smcfcs::smcfcs(df, smtype="lm", smformula=y ~ z + x + x_sq,
             method=c("norm", "", "norm", "x^2"))
mice::pool(lapply(nonmice$impDatasets, function(x) stats::lm(y ~ z + x + x_sq, data=x)))
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%% \keyword{mice imputation method}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
