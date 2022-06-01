%% File Name: save.Rdata.Rd
%% File Version: 0.13

\name{save.Rdata}
\alias{save.Rdata}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
\R Utilities: Save a Data Frame in \code{Rdata} Format
}
\description{
This function saves a data frame in a \code{Rdata} format.
}
\usage{
save.Rdata(dat, name, path=NULL, part.numb=1000)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
Data frame
}
  \item{name}{
Name of the \R object to be saved
}
  \item{path}{
Directory for saving the object
}
  \item{part.numb}{
Number of rows of the data frame which should also
be saved in csv format. The default is saving 1000 rows.
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
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
dfr <- matrix( 2*1:12-3, 4,3 )
save.Rdata( dfr, "dataframe_test" )
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%% \keyword{R utilities}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
