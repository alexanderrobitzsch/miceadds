%% File Name: Reval.Rd
%% File Version: 0.24

\name{Reval}
\alias{Reval}

\alias{Revalpr}
\alias{Revalprstr}
\alias{Revalpr_round}
\alias{Revalpr_maxabs}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
\R Utilities: Evaluates a String as an Expression in \R
}
\description{
This function evaluates a string as an \R expression.
}
\usage{
Reval(Rstring, print.string=TRUE, n.eval.parent=1)

# Reval( print(Rstring) )
Revalpr(Rstring, print.string=TRUE)

#  Reval( print(str(Rstring)) )
Revalprstr(Rstring, print.string=TRUE)

#  Reval( print(round(Rstring, digits)) )
Revalpr_round( Rstring, digits=5, print.string=TRUE)

#  Reval( print(max(abs(Rstring_x - Rstring_y)) ) )
Revalpr_maxabs( Rstring_x, Rstring_y, print.string=TRUE, na.rm=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Rstring}{
String which shall be evaluated in \R
}
  \item{print.string}{
Should the string printed on the console?
}
\item{n.eval.parent}{Index of parent environment in which the \R
command should be evaluated.
}
\item{digits}{Number of digits after decimal.}
\item{Rstring_x}{String corresponding to an \R object}
\item{Rstring_y}{String corresponding to an \R object}
\item{na.rm}{Logical indicating whether missing values should be removed
from calculation}
}

\details{
The string is evaluated in the parent environment. See
\code{\link[base:eval]{base::eval}}  for the definition of environments in \R.
}
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
# This function is simply a shortage function
# See the definition of this function:
Reval <- function( Rstring, print.string=TRUE){
    if (print.string){ cat( paste( Rstring ), "\n"  ) }
        eval.parent( parse( text=paste( Rstring )), n=1 )
            }

Reval( "a <- 2^3" )
  ## a <- 2^3
a
  ## [1] 8
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%% \keyword{R utilities}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
