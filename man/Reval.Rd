\name{Reval}
\alias{Reval}

\alias{Revalpr}
\alias{Revalprstr}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
\R Utilities: Evaluates a String as an Expression in \R
}
\description{
This function evaluates a string as an \R expression.
}
\usage{
Reval(Rstring, print.string=TRUE)

Revalpr(Rstring, print.string=TRUE)	# = Reval( print(Rstring) )

Revalprstr(Rstring, print.string=TRUE)   #  = Reval( print(str(Rstring)) )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Rstring}{
String which shall be evaluated in \R
}
  \item{print.string}{
Should the string printed on the console?
}
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
\author{
Alexander Robitzsch
}
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
Reval <- function( Rstring , print.string=TRUE){ 
    if (print.string){ cat( paste( Rstring ) , "\n"  ) }
    	eval.parent( parse( text = paste( Rstring )) , n=1 )
            }
  
Reval( "a <- 2^3" )
  ## a <- 2^3 
a
  ## [1] 8  
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{R utilities}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
