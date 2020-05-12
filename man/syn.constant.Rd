%% File Name: syn.constant.Rd
%% File Version: 0.12

\name{syn.constant}
\alias{syn.constant}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Synthesizing Method for Fixed Values by Design in \pkg{synthpop}
}


\description{
Defines a synthesizing method for fixed values of a variable by design
in the \pkg{synthpop} package.
}

\usage{
syn.constant(y, x, xp, fixed_values, ...)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
\item{y}{Original data vector of length \eqn{n}}
\item{x}{Matrix (\eqn{n \times p}) of original covariates}
\item{xp}{Matrix (\eqn{k \times p}) of synthesised covariates}
\item{fixed_values}{Vector containing fixed values}
\item{\dots}{
Further arguments to be passed
}
}


\details{
When using the synthesis method \code{"mice"} in
\code{\link[synthpop:syn]{synthpop::syn}}, the function argument has to appear
as \code{rf.fixed_values} (convention in \pkg{synthpop}).
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
\code{\link[synthpop:syn]{synthpop::syn}},
\code{\link{mice.impute.constant}}
}
%% ~~objects to See Also as \code{\link{help}}, ~~~

\examples{
\dontrun{
#############################################################################
# EXAMPLE 1: SD2011 | Fixed values for variable sex
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

#** constant vector
method["sex"] <- "constant"
fixed_values <- data.frame( sex=rep(dat$sex[c(1,2)], each=1000) )
imp <- synthpop::syn( dat, method=method, k=2000, m=1,
                rf.fixed_values=fixed_values)
table(imp$syn$sex)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%% \keyword{mice imputation method}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
