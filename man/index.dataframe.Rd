%% File Name: index.dataframe.Rd
%% File Version: 0.16

\name{index.dataframe}
\alias{index.dataframe}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
\R Utilities: Include an Index to a Data Frame
}
\description{
This function includes an index variable to a data frame
in the first column.
}
\usage{
index.dataframe(data,systime=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
Data frame
}
\item{systime}{Should system time be included in the second column
of the data frame?}
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
dfr <- matrix( 2*1:12-3, 4,3 )
colnames(dfr) <- paste0("X",1:ncol(dfr))
index.dataframe( dfr)
  ##     index X1 X2 X3
  ##   1     1 -1  7 15
  ##   2     2  1  9 17
  ##   3     3  3 11 19
  ##   4     4  5 13 21
index.dataframe( dfr, systime=TRUE)
  ##     index         file_created X1 X2 X3
  ##   1     1  2013-08-22 10:26:28 -1  7 15
  ##   2     2  2013-08-22 10:26:28  1  9 17
  ##   3     3  2013-08-22 10:26:28  3 11 19
  ##   4     4  2013-08-22 10:26:28  5 13 21
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%% \keyword{R utilities}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
