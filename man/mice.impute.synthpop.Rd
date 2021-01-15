%% File Name: mice.impute.synthpop.Rd
%% File Version: 0.13

\name{mice.impute.synthpop}
\alias{mice.impute.synthpop}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Using a \pkg{synthpop} Synthesizing Method in the \pkg{mice} Package
}
\description{
The function allows to use a \pkg{synthpop} synthesizing method to be used
in the \code{\link[mice:mice]{mice::mice}} function of the \pkg{mice} package.
}

\usage{
mice.impute.synthpop(y, ry, x, synthpop_fun="norm", synthpop_args=list(),
     proper=TRUE, ...)
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
\item{synthpop_fun}{Synthesizing method in the \pkg{synthpop} package}
\item{synthpop_args}{Function arguments of \code{syn_fun}}
\item{proper}{Logical value specifying whether proper synthesis should be conducted.}
\item{\dots}{Further arguments to be passed}
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

\seealso{
See \code{\link{syn.mice}} for using a \pkg{mice} imputation method in the
\pkg{synthpop} package.

See \code{\link[synthpop:syn]{synthpop::syn}} for generating synthetic datasets
with the \pkg{synthpop} package.
}

\examples{
\dontrun{
#############################################################################
# EXAMPLE 1: Imputation of NHANES data using the 'syn.normrank' method
#############################################################################

library(synthpop)
data(nhanes, package="mice")
dat <- nhanes

#* empty imputation
imp0 <- mice::mice(dat, maxit=0)
method <- imp0$method

#* define synthpop method 'normrank' for variable 'chl'
method["chl"] <- "synthpop"
synthpop_fun <- list( chl="normrank" )
synthpop_args <- list( chl=list(smoothing="density") )

#* conduct imputation
imp <- mice::mice(dat, method=method, m=1, maxit=3, synthpop_fun=synthpop_fun,
            synthpop_args=synthpop_args)
summary(imp)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%% \keyword{mice imputation method}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
