%% File Name: syn.formula.Rd
%% File Version: 0.09

\name{syn.formula}
\alias{syn.formula}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Synthesizing Method for \pkg{synthpop} Using a Formula Interface
}


\description{
Defines a synthesizing method for for \pkg{synthpop} using a formula interface.
}

\usage{
syn.formula(y, x, xp, proper=FALSE, syn_formula, syn_fun, syn_args, ...)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
\item{y}{Original data vector of length \eqn{n}}
\item{x}{Matrix (\eqn{n \times p}) of original covariates}
\item{xp}{Matrix (\eqn{k \times p}) of synthesised covariates}
\item{proper}{Logical value specifying whether proper synthesis should be conducted.}
\item{syn_formula}{A formula object}
\item{syn_fun}{Synthesizing method in \pkg{synthpop} package}
\item{syn_args}{Function arguments of \code{syn_fun}}
\item{\dots}{
Further arguments to be passed
}
}


\details{
When using the synthesis method \code{"mice"} in
\code{\link[synthpop:syn]{synthpop::syn}}, the function arguments have to appear
as \code{rf.syn_formula}, \code{rf.syn_fun} and \code{rf.syn_args}
 (convention in \pkg{synthpop}).
}


\value{
A vector of length \code{k} with synthetic values of \code{y}.
}


% \references{
% Nowok, B., Raab, G., & Dibben, C. (2016).
% }



%\note{
%XXX
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[synthpop:syn]{synthpop::syn}}
}
%% ~~objects to See Also as \code{\link{help}}, ~~~

\examples{
\dontrun{
#############################################################################
# EXAMPLE 1: SD2011 | using a formula for defining the regression model
#############################################################################

library(synthpop)

#** selection of dataset
data(SD2011, package="synthpop")
vars <- c("sex","age","ls","smoke")
dat  <- SD2011[1:1000, vars]
dat$ls <- as.numeric(dat$ls)

#** default synthesis
imp0 <- synthpop::syn(dat)
pred <- imp0$predictor.matrix
method <- imp0$method

#** use synthesizing method 'formula'
method["ls"] <- "formula"
syn_fun <- list( ls="normrank" )
syn_args <- list( ls=list( smoothing="density" ) )
syn_formula <- list( ls=~ sex + age + I(age^2) + I(age>50) )

#* synthesize data
imp <- synthpop::syn( dat, method=method, predictor.matrix=pred, k=2000, m=1,
            rf.syn_fun=syn_fun, rf.syn_args=syn_args, rf.syn_formula=syn_formula)
summary(imp)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%% \keyword{mice imputation method}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
