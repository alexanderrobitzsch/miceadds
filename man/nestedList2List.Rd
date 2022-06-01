%% File Name: nestedList2List.Rd
%% File Version: 0.08



\name{nestedList2List}
\alias{nestedList2List}
\alias{List2nestedList}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
Converting a Nested List into a List (and Vice Versa)
}


\description{
Converts a nested list into a list (and vice versa).
}

\usage{
nestedList2List(nestedList)

List2nestedList(List, N_between, N_within=NULL, loop_within=TRUE)
}



%- maybe also 'usage' for other objects documented here.
\arguments{
\item{nestedList}{A nested list}
  \item{List}{
A list
}
  \item{N_between}{
Number of between list elements
}
  \item{N_within}{
Number of within list elements
}
  \item{loop_within}{
Optional logical indicating whether looping should start from within
list
}
}

%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}

\value{
A list or a nested list
}

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
#############################################################################
# EXAMPLE 1: List conversions using a small example
#############################################################################

# define a nestedList
nestedList <- as.list(1:3)
nestedList[[1]] <- as.list( 2:4 )
nestedList[[2]] <- as.list( 34 )
nestedList[[3]] <- as.list( 4:9 )

# convert a nested list into a list
v2 <- miceadds::nestedList2List( nestedList)

## reconvert list v2 into a nested list, looping within first
v3 <- miceadds::List2nestedList(v2, N_between=5)
# looping between first
v4 <- miceadds::List2nestedList(v2, N_between=5, loop_within=FALSE)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
