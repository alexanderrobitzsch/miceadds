%% File Name: ma_lme4_formula_terms.Rd
%% File Version: 0.11

\name{ma_lme4_formula}
\alias{ma_lme4_formula}
\alias{ma_lme4_formula_terms}
\alias{ma_lme4_formula_design_matrices}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Utility Functions for Working with \pkg{lme4} Formula Objects
}
\description{
Utility functions for working with \pkg{lme4} formula objects. The function
\code{ma_lme4_formula_terms} decomposes an \pkg{lme4} formula into several
parts for further processing.
}
\usage{
ma_lme4_formula_terms(formula)

ma_lme4_formula_design_matrices(formula, data, start_index=0, formula_terms=NULL,
        only_design_matrices=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{
An \R formula object
}
\item{data}{Data frame}
\item{start_index}{Starting index for cluster identifiers}
\item{formula_terms}{Optional argument with processed formula terms using the
function \code{ma_lme4_formula_terms}}
\item{only_design_matrices}{Logical indicating whether only design matrices should
be created}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
List with several entries
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

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
\dontrun{
#############################################################################
# EXAMPLE 1: Splitting a lme4 formula
#############################################################################

#*** formula for a multilevel model
formula <- y ~ I( miceadds::cwc(x, idcluster)) + z + I(z^2) + I( miceadds::gm(x, idcluster) ) + w +
                        ( x + I(x^2) | idcluster)  + (0 +  w | idcluster ) +
                        ( 0 + I(as.factor(f)) | idcluster)
miceadds::ma_lme4_formula_terms(formula)

#*** formula for a single level model
formula2 <- y ~ I( miceadds::cwc(x, idcluster)) + z + I(z^2) + I( miceadds::gm(x, idcluster) ) + w
miceadds::ma_lme4_formula_terms(formula2)

#############################################################################
# EXAMPLE 2: Design matrices for multilevel model
#############################################################################

data(data.ma07, package="miceadds")
dat <- data.ma07

formula <- x1 ~ x2 + I( miceadds::gm( x2, id2)) + I( miceadds::gm( x2, id3)) + y1 + z1 +
                    ( x2 | id2:id3 ) + ( 1 | id3 ) + ( 0 + x2 | id3 )
res <- miceadds::ma_lme4_formula_design_matrices(formula, data=dat)
str(res)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
