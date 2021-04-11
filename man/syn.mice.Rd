%% File Name: syn.mice.Rd
%% File Version: 0.172

\name{syn.mice}
\alias{syn.mice}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Using a \pkg{mice} Imputation Method in the \pkg{synthpop} Package
}


\description{
The function allows to use a \pkg{mice} imputation method to be used
in the \code{\link[synthpop:syn]{synthpop::syn}} function of
the \pkg{synthpop} package (Nowok, Raab, & Dibben, 2016).
}

\usage{
syn.mice(y, x, xp, mice_fun, mice_args, ...)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
\item{y}{Original data vector of length \eqn{n}}
\item{x}{Matrix (\eqn{n \times p}) of original covariates}
\item{xp}{Matrix (\eqn{k \times p}) of synthesised covariates}
\item{mice_fun}{Name of imputation method for \pkg{mice}}
\item{mice_args}{Optional list of arguments for \code{mice_fun}, see Examples.}
\item{\dots}{
Further arguments to be passed
}
}


\details{
When using the synthesis method \code{"mice"} in
\code{\link[synthpop:syn]{synthpop::syn}}, the function arguments have to appear
as \code{rf.mice_fun} and \code{rf.mice_arg} (convention in \pkg{synthpop}).
}


\value{
A vector of length \code{k} with synthetic values of \code{y}.
}


\references{
Nowok, B., Raab, G., & Dibben, C. (2016).
\pkg{synthpop}: Bespoke creation of synthetic data in \R.
\emph{Journal of Statistical Software, 74}(11), 1-26.
\doi{10.18637/jss.v074.i11}
}



%\note{
%XXX
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[synthpop:syn]{synthpop::syn}}, \code{\link{syn_mice}}
}
%% ~~objects to See Also as \code{\link{help}}, ~~~

\examples{
\dontrun{
#############################################################################
# EXAMPLE 1: SD2011 | Minimal example for using a mice imputation method
#############################################################################

library(synthpop)

#** selection of dataset
data(SD2011, package="synthpop")
vars <- c("sex","age","ls","smoke")
dat  <- SD2011[1:1000, vars]
dat$ls <- as.numeric(dat$ls)
dat$smoke <- 1*(paste(dat$smoke)=="YES")

#** default synthesis
imp0 <- synthpop::syn(dat)
pred <- imp0$predictor.matrix
method <- imp0$method

#** use mice imputation method 'rlm' for variable 'ls'
method[c("ls","smoke")] <- c("mice","mice")
mice_fun <- list( ls="rlm", smoke="pmm")
mice_args <- list( ls=list( trafo=log, antitrafo=exp) )

#* synthesize data
imp <- synthpop::syn( dat, method=method, predictor.matrix=pred, k=2000, m=1,
            rf.mice_fun=mice_fun, rf.mice_args=mice_args)
summary(imp)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%% \keyword{mice imputation method}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
