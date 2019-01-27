%% File Name: library_install.Rd
%% File Version: 0.13

\name{library_install}
\alias{library_install}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
\R Utilities: Loading a Package or Installation of a Package if Necessary
}
\description{
Loads packages specified in vector \code{pkg}. If some packages are not
yet installed, they will be automatically installed by this function using
\code{\link[utils:install.packages]{install.packages}}.
}
\usage{
library_install( pkg, ... )
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pkg}{
Vector with package names
}
  \item{\dots}{
Further arguments to be passed to
\code{\link[utils:install.packages]{install.packages}}
}
}

%\details{
%The string is evaluated in the parent environment. See
%}
%\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
%}
%\references{
%% ~put references to the literature/web site here ~
%}

%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
\dontrun{
# try to load packages PP and MCMCglmm
library_install( pkg=c("PP", "MCMCglmm") )
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%% \keyword{R utilities}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
