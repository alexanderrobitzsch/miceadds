\name{mice.impute.weighted.norm}
\alias{mice.impute.weighted.norm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Imputation by a Weighted Linear Normal Regression
}
\description{
Imputation by a weighted linear normal regression.
}
\usage{
mice.impute.weighted.norm(y, ry, x, ridge = 1e-05, pls.facs = NULL, 
     imputationWeights = NULL, interactions = NULL, quadratics = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
 \item{y}{Incomplete data vector of length \code{n}}  
\item{ry}{
	Vector of missing data pattern (\code{FALSE} -- missing,
	\code{TRUE} -- observed)
  }
\item{x}{Matrix (\code{n} x \code{p}) of complete covariates.
}
  \item{ridge}{
Ridge parameter in the diagonal of \eqn{ \bold{X}'\bold{X}}
}
  \item{imputationWeights}{
Optional vector of sampling weights
}
  \item{pls.facs}{
Number of factors in PLS regression (if used). The default is \code{NULL}
which means that no PLS regression is used for dimension reduction.
}
  \item{interactions}{
Optional vector of variables for which interactions should be created
}
  \item{quadratics}{
Optional vector of variables which should also be included as quadratic effects.
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
For examples see \code{\link{mice.impute.weighted.pmm}}
}
%%\examples{
%%
%% }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Sampling weights}
\keyword{mice imputation method}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
