%% File Name: mice.impute.constant.Rd
%% File Version: 0.02

\name{mice.impute.constant}
\alias{mice.impute.constant}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Imputation Using a Fixed Vector
}
\description{
Defines a fixed vector of values for imputation of a variable.
The method is particularly useful for the generation of
synthetic datasets, see \code{\link{syn_mice}} (Example 1).
}
\usage{
mice.impute.constant(y, ry, x, fixed_values, ... )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
    \item{y}{Incomplete data vector of length \code{n}}

  \item{ry}{
Vector of missing data pattern (\code{FALSE} -- missing,
\code{TRUE} -- observed)
}
\item{x}{Matrix (\code{n} x \code{p}) of complete
 covariates.}
\item{fixed_values}{
Vector containing fixed values
}
  \item{\dots}{
More arguments to be passed to imputation function
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
Vector of imputed values
}
%\references{
%% ~put references to the literature/web site here ~
%}
%\author{
%%  ~~who you are~~
%}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{syn.constant}}
}

\examples{
\dontrun{
#############################################################################
# EXAMPLE 1: Example with fixed imputed values
#############################################################################

data(nhanes, package="mice")
dat <- nhanes

#* define methods
method <- c(age="", bmi="constant", hyp="norm", chl="pmm")
fixed_values <- list( bmi=rep(27,9) )

#* impute
imp <- mice::mice(dat, method=method, m=1, maxit=3, fixed_values=fixed_values)
table(mice::complete(imp, action=1)$bmi)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
