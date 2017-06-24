\name{mice.impute.2l.contextual.norm}
\alias{mice.impute.2l.contextual.norm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Imputation by Normal Linear Regression with Contextual Variables
}

\description{
This imputation method imputes a variable using linear regression
with normally distributed residuals. Including a contextual effects
means that an aggregated variable at a cluster level is included
as a further covariate.
}


\usage{
mice.impute.2l.contextual.norm(y, ry, x, type, ridge = 10^(-5), 
   imputationWeights = NULL, interactions = NULL, quadratics = NULL, ...)
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
\code{type=2} denotes a variable for which also a contextual effect is
included and \code{type=1} denotes all other variables which are
included as 'ordinary' predictors.
}
  \item{ridge}{
Ridge parameter in the diagonal of \eqn{ \bold{X}'\bold{X}}
}
\item{imputationWeights}{
Optional vector of sample weights
}
  \item{interactions}{
Vector of variable names used for creating interactions
}
  \item{quadratics}{
Vector of variable names used for creating quadratic terms
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

\author{
Alexander Robitzsch
}

%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
For examples see \code{\link{mice.impute.2l.contextual.pmm}}.
}

%\examples{
%## include an example here
%}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{mice imputation method}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
