\name{GroupMean}
\alias{GroupMean}
\alias{GroupSum}
\alias{GroupSD}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculation of Groupwise Descriptive Statistics for Matrices
}
\description{
Calculates some groupwise descriptive statistics
}
\usage{
GroupMean(data, group, weights=NULL, extend=FALSE)

GroupSum(data, group, weights=NULL, extend=FALSE)	

GroupSD(data, group, weights=NULL, extend=FALSE)	
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
A numeric data frame
}
  \item{group}{
A vector of group identifiers
}
\item{weights}{An optional vector of sample weights}
\item{extend}{Optional logical indicating whether the group means (or sums)
should be extended to the original dimensions of the dataset.}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
A data frame with groupwise calculated statistics
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
\code{\link[mitml:clusterMeans]{mitml::clusterMeans}} 

\code{\link[base:rowsum]{base::rowsum}},
\code{\link[stats:aggregate]{stats::aggregate}},
\code{\link[stats:ave]{stats::ave}}
}

\examples{
#############################################################################
# EXAMPLE 1: Group means and SDs data.ma02
#############################################################################

data( data.ma02 )
dat <- data.ma02[[1]] # select first dataset

#--- group means for read and math
GroupMean( dat[ , c("read","math") ] , group=dat$idschool )
# using rowsum
a1 <- base::rowsum( dat[ , c("read","math") ] , dat$idschool )
a2 <- base::rowsum( 1+0*dat[ , c("read","math") ] , dat$idschool )
(a1/a2)[1:10,]
# using aggregate
stats::aggregate(  dat[ , c("read","math") ] , list(dat$idschool) , mean )[1:10,]

#--- extend group means to original dataset
GroupMean( dat[ , c("read","math") ] , group=dat$idschool , extend =TRUE )
# using ave
stats::ave( dat[ , "read" ] , dat$idschool  )
stats::ave( dat[ , "read" ] , dat$idschool , FUN = mean )

\dontrun{
#--- group standard deviations
GroupSD( dat[ , c("read","math") ] , group=dat$idschool)[1:10,]
# using aggregate
stats::aggregate(  dat[ , c("read","math") ] , list(dat$idschool) , sd )[1:10,]
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Utility function}
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
