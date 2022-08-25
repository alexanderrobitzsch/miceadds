%% File Name: ml_mcmc.Rd
%% File Version: 0.448

\name{ml_mcmc}
\alias{ml_mcmc}
\alias{ml_mcmc_fit}
\alias{plot.ml_mcmc}
\alias{summary.ml_mcmc}
\alias{coef.ml_mcmc}
\alias{vcov.ml_mcmc}
\alias{miceadds_rcpp_ml_mcmc_sample_beta}
\alias{miceadds_rcpp_ml_mcmc_sample_u}
\alias{miceadds_rcpp_ml_mcmc_sample_psi}
\alias{miceadds_rcpp_ml_mcmc_sample_sigma2}
\alias{miceadds_rcpp_ml_mcmc_sample_latent_probit}
\alias{miceadds_rcpp_ml_mcmc_sample_thresholds}
\alias{miceadds_rcpp_ml_mcmc_predict_fixed_random}
\alias{miceadds_rcpp_ml_mcmc_predict_random_list}
\alias{miceadds_rcpp_ml_mcmc_predict_random}
\alias{miceadds_rcpp_ml_mcmc_predict_fixed}
\alias{miceadds_rcpp_ml_mcmc_subtract_fixed}
\alias{miceadds_rcpp_ml_mcmc_subtract_random}
\alias{miceadds_rcpp_ml_mcmc_compute_ztz}
\alias{miceadds_rcpp_ml_mcmc_compute_xtx}
\alias{miceadds_rcpp_ml_mcmc_probit_category_prob}
\alias{miceadds_rcpp_pnorm}
\alias{miceadds_rcpp_qnorm}
\alias{miceadds_rcpp_rtnorm}


%- Also NEED an '\alias' for EACH other topic documented here.
\title{
MCMC Estimation for Mixed Effects Model
}
\description{
Fits a mixed effects model via MCMC. The outcome can be normally distributed
or ordinal (Goldstein, 2011; Goldstein, Carpenter, Kenward & Levin, 2009).
}


\usage{
ml_mcmc( formula, data, iter=3000, burnin=500, print_iter=100, outcome="normal",
     nu0=NULL, s0=1, psi_nu0_list=NULL, psi_S0_list=NULL, inits_lme4=FALSE,
     thresh_fac=5.8, ridge=1e-5)

\method{summary}{ml_mcmc}(object, digits=4, file=NULL, ...)

\method{plot}{ml_mcmc}(x, ask=TRUE, ...)

\method{coef}{ml_mcmc}(object, ...)

\method{vcov}{ml_mcmc}(object, ...)

ml_mcmc_fit(y, X, Z_list, beta, Psi_list, sigma2, alpha, u_list, idcluster_list,
    onlyintercept_list, ncluster_list, sigma2_nu0, sigma2_sigma2_0, psi_nu0_list,
    psi_S0_list, est_sigma2, est_probit, parameter_index, est_parameter, npar, iter,
    save_iter, verbose=TRUE, print_iter=500, parnames0=NULL, K=9999, est_thresh=FALSE,
    thresh_fac=5.8, ridge=1e-5, parm_summary=TRUE )

## exported Rcpp functions
miceadds_rcpp_ml_mcmc_sample_beta(xtx_inv, X, Z_list, y, u_list, idcluster_list, sigma2,
     onlyintercept_list, NR, ridge)
miceadds_rcpp_ml_mcmc_sample_u(X, beta, Z_list, y, ztz_list, idcluster_list,
    ncluster_list, sigma2, Psi_list, onlyintercept_list, NR, u0_list, ridge)
miceadds_rcpp_ml_mcmc_sample_psi(u_list, nu0_list, S0_list, NR, ridge)
miceadds_rcpp_ml_mcmc_sample_sigma2(y, X, beta, Z_list, u_list, idcluster_list,
     onlyintercept_list, nu0, sigma2_0, NR, ridge)
miceadds_rcpp_ml_mcmc_sample_latent_probit(X, beta, Z_list, u_list, idcluster_list, NR,
     y_int, alpha, minval, maxval)
miceadds_rcpp_ml_mcmc_sample_thresholds(X, beta, Z_list, u_list, idcluster_list, NR, K,
     alpha, sd_proposal, y_int)
miceadds_rcpp_ml_mcmc_predict_fixed_random(X, beta, Z_list, u_list, idcluster_list, NR)
miceadds_rcpp_ml_mcmc_predict_random_list(Z_list, u_list, idcluster_list, NR, N)
miceadds_rcpp_ml_mcmc_predict_random(Z, u, idcluster)
miceadds_rcpp_ml_mcmc_predict_fixed(X, beta)
miceadds_rcpp_ml_mcmc_subtract_fixed(y, X, beta)
miceadds_rcpp_ml_mcmc_subtract_random(y, Z, u, idcluster, onlyintercept)
miceadds_rcpp_ml_mcmc_compute_ztz(Z, idcluster, ncluster)
miceadds_rcpp_ml_mcmc_compute_xtx(X)
miceadds_rcpp_ml_mcmc_probit_category_prob(y_int, alpha, mu1, use_log)
miceadds_rcpp_pnorm(x, mu, sigma)
miceadds_rcpp_qnorm(x, mu, sigma)
miceadds_rcpp_rtnorm(mu, sigma, lower, upper)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{formula}{An \R formula in \pkg{lme4}-like specification}
\item{data}{Data frame}
  \item{iter}{
Number of iterations
}
\item{burnin}{Number of burnin iterations}
  \item{print_iter}{
Integer indicating that every \code{print_iter}th iteration progress
should be displayed}
\item{outcome}{Outcome distribution: \code{"normal"} or \code{"probit"}}
\item{nu0}{Prior sample size}
\item{s0}{Prior guess for variance}
\item{inits_lme4}{Logical indicating whether initial values should
be obtained from fitting the model in the \pkg{lme4} package}
\item{thresh_fac}{Factor for proposal variance for estimating thresholds
which is determined as \code{thresh_fac}\eqn{/N} (\eqn{5.8/N} as default).
}
\item{ridge}{Ridge parameter for covariance matrices in sampling}
\item{object}{Object of class \code{ml_mcmc}}
\item{digits}{Number of digits after decimal used for printing}
\item{file}{Optional file name}
\item{\dots}{Further arguments to be passed}
\item{x}{Object of class \code{ml_mcmc}}
\item{ask}{Logical indicating whether display of the next plot should be requested
via clicking}
  \item{y}{
Outcome vector
}
  \item{X}{
Design matrix fixed effects
}
  \item{Z_list}{
Design matrices random effects
}
  \item{beta}{
Initial vector fixed coefficients
}
  \item{Psi_list}{
Initial covariance matrices random effects
}
  \item{sigma2}{
Initial residual covariance matrix
}
\item{alpha}{Vector of thresholds}
  \item{u_list}{
List with initial values for random effects
}
  \item{idcluster_list}{
List with cluster identifiers for random effects
}
  \item{onlyintercept_list}{
List of logicals indicating whether only random intercepts are used
for a corresponding random effect
}
  \item{ncluster_list}{
List containing number of clusters for each random effect
}
  \item{sigma2_nu0}{
Prior sample size residual variance
}
  \item{sigma2_sigma2_0}{
Prior guess residual variance
}
  \item{psi_nu0_list}{
List of prior sample sizes for covariance matrices of random effects
}
  \item{psi_S0_list}{
List of prior guesses for covariance matrices of random effects
}
  \item{est_sigma2}{
Logical indicating whether residual variance should be estimated
}
  \item{est_probit}{
Logical indicating whether probit model for ordinal outcomes should be estimated
}
  \item{parameter_index}{
List containing integers for saving parameters
}
  \item{est_parameter}{
List of logicals indicating which parameter type should be estimated
}
  \item{npar}{
Number of parameters
}
  \item{save_iter}{
Vector indicating which iterations should be used
}
  \item{verbose}{
Logical indicating whether progress should be displayed
}
  \item{parnames0}{
Optional vector of parameter names
}
\item{K}{Number of categories}
\item{est_thresh}{Logical indicating whether thresholds should be estimated}
\item{parm_summary}{Logical indicating whether a parameter summary table
should be computed}
\item{xtx_inv}{Matrix}
\item{NR}{Integer}
\item{ztz_list}{List containing design matrices for random effects}
\item{u0_list}{List containing random effects}
\item{nu0_list}{List with prior sample sizes}
\item{S0_list}{List with prior guesses}
\item{sigma2_0}{Numeric}
\item{y_int}{Integer vector}
\item{minval}{Numeric}
\item{maxval}{Numeric}
\item{sd_proposal}{Numeric vector}
\item{N}{Integer}
\item{Z}{Matrix}
\item{u}{Matrix containing random effects}
\item{idcluster}{Integer vector}
\item{onlyintercept}{Logical}
\item{ncluster}{Integer}
\item{mu1}{Vector}
\item{use_log}{Logical}
\item{mu}{Vector}
\item{sigma}{Numeric}
\item{lower}{Vector}
\item{upper}{Vector}
}

\details{
Fits a linear mixed effects model \eqn{y=\bm{X}\bm{beta}+
\bm{Z}\bm{u}+e} with MCMC sampling. In case of ordinal data,
the ordinal variable \eqn{y} is replaced by an underlying latent normally
distributed variable \eqn{y^\ast} and the residual variance is fixed to 1.
}

\value{
List with following entries (selection)

 \item{sampled_values}{Sampled values}
\item{par_summary}{Parameter summary}
}
\references{
Goldstein, H. (2011). \emph{Multilevel statistical models}. New York: Wiley.
\doi{10.1002/9780470973394}

Goldstein, H., Carpenter, J., Kenward, M., & Levin, K. (2009). Multilevel models with
multivariate mixed response types. \emph{Statistical Modelling, 9}(3), 173-197.
\doi{10.1177/1471082X0800900301}
}

%\author{
%%  ~~who you are~~
%}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See also the \pkg{MCMCglmm} package for MCMC estimation and
the \pkg{lme4} package for maximum likelihood estimation.
}

\examples{
\dontrun{
#############################################################################
# EXAMPLE 1: Multilevel model continuous data
#############################################################################

library(lme4)

#*** simulate data
set.seed(9097)

# number of clusters and units within clusters
K <- 150
n <- 15
iccx <- .2
idcluster <- rep( 1:K, each=n )
w <- stats::rnorm( K )
x <- rep( stats::rnorm( K, sd=sqrt(iccx) ), each=n) +
               stats::rnorm( n*K, sd=sqrt( 1 - iccx ))
X <- data.frame(int=1, "x"=x, xaggr=miceadds::gm(x, idcluster),
        w=rep( w, each=n ) )
X <- as.matrix(X)
Sigma <- diag( c(2, .5 ) )
u <- MASS::mvrnorm( K, mu=c(0,0), Sigma=Sigma )
beta <- c( 0, .3, .7, 1 )
Z <- X[, c("int", "x") ]
ypred <- as.matrix(X) \%*\% beta + rowSums( Z * u[ idcluster, ] )
y <- ypred[,1] + stats::rnorm( n*K, sd=1 )
data <- as.data.frame(X)
data$idcluster <- idcluster
data$y <- y

#*** estimate mixed effects model with miceadds::ml_mcmc() function
formula <- y ~ x + miceadds::gm(x, idcluster) + w + ( 1 + x | idcluster)
mod1 <- miceadds::ml_mcmc( formula=formula, data=data)
plot(mod1)
summary(mod1)

#*** compare results with lme4 package
mod2 <- lme4::lmer(formula=formula, data=data)
summary(mod2)

#############################################################################
# EXAMPLE 2: Multilevel model for ordinal outcome
#############################################################################

#*** simulate data
set.seed(456)
# number of clusters and units within cluster
K <- 500
n <- 10
iccx <- .2
idcluster <- rep( 1:K, each=n )
w <- rnorm( K )
x <- rep( stats::rnorm( K, sd=sqrt(iccx)), each=n) +
                 stats::rnorm( n*K, sd=sqrt( 1 - iccx ))
X <- data.frame("int"=1, "x"=x, "xaggr"=miceadds::gm(x, idcluster),
        w=rep( w, each=n ) )
X <- as.matrix(X)
u <- matrix( stats::rnorm(K, sd=sqrt(.5) ), ncol=1)
beta <- c( 0, .3, .7, 1 )
Z <- X[, c("int") ]
ypred <- as.matrix(X) \%*\% beta + Z * u[ idcluster, ]
y <- ypred[,1] + stats::rnorm( n*K, sd=1 )
data <- as.data.frame(X)
data$idcluster <- idcluster
alpha <- c(-Inf, -.4, 0, 1.7,  Inf)
data$y <- cut( y, breaks=alpha, labels=FALSE ) - 1

#*** estimate model
formula <- y ~ miceadds::cwc(x, idcluster) + miceadds::gm(x,idcluster) + w + ( 1 | idcluster)
mod <- miceadds::ml_mcmc( formula=formula, data=data, iter=2000, burnin=500,
                outcome="probit", inits_lme4=FALSE)
summary(mod)
plot(mod)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
