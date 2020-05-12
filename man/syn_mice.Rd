%% File Name: syn_mice.Rd
%% File Version: 0.07

\name{syn_mice}
\alias{syn_mice}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Constructs Synthetic Dataset with \pkg{mice} Imputation Methods
}


\description{
Constructs synthetic dataset with \pkg{mice} imputation methods. The functionality
is very similar to the functionality of
\code{\link[synthpop:syn]{synthpop::syn}} in the \pkg{synthpop} package
(Nowok, Raab, & Dibben, 2016). Methods defined in \pkg{synthpop} are accessible
via \code{\link{mice.impute.synthpop}} (see Examples).
}

\usage{
syn_mice(data, m=5, k=NULL, syn_check=TRUE, ...)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
\item{data}{Original data frame}
\item{m}{Number of synthetic datasets}
\item{k}{Number of observations in synthetic data}
\item{syn_check}{Logical indicating whether checks in
\code{\link[synthpop:syn]{synthpop::syn}} should be performed.
}
\item{\dots}{
Further arguments to be passed, with conventions in
\code{\link[mice:mice]{mice::mice}}
}
}


% \details{
% When using the synthesis method \code{"mice"} in
% }


\value{
Object of class \code{synds}, see \code{\link[synthpop:syn]{synthpop::syn}}.
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
\code{\link[mice:mice]{mice::mice}},
\code{\link[synthpop:syn]{synthpop::syn}}
}
%% ~~objects to See Also as \code{\link{help}}, ~~~

\examples{
\dontrun{
#############################################################################
# EXAMPLE 1: Synthesization of SD2011 using mice functionality
#############################################################################

library(synthpop)

#** selection of dataset
data(SD2011, package="synthpop")
vars <- c("sex","age","ls","smoke")
dat  <- SD2011[1:1000, vars]
dat$ls <- as.numeric(dat$ls)

#** default synthesis
imp0 <- synthpop::syn(dat)
pred0 <- imp0$predictor.matrix
method0 <- imp0$method

#* define imputation methods
method <- c(sex="synthpop", age="synthpop", ls="synthpop", smoke="logreg")
# only for smoke, an original mice imputation method is used

#- define synthpop functions
synthpop_fun <- list(sex="constant", age="constant", ls="cart")

#- arguments for 'syn.cart' method
synthpop_args <- list(ls=list(smoothing="density"))

#- fixed values for 'syn.constant' method
fixed_values <- dat[,1:2]

#- do synthesization
imp <- miceadds::syn_mice(dat, m=1, synthpop_fun=synthpop_fun, method=method,
            pedictorMatrix=pred0, rf.fixed_values=fixed_values, synthpop_args=synthpop_args)
summary(imp)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%% \keyword{mice imputation method}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
