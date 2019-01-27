%% File Name: ma.scale2.Rd
%% File Version: 0.22

\name{ma.scale2}
\alias{ma.scale2}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
Standardization of a Matrix
}
\description{
This function performs a z-standardization for a numeric matrix.
Note that in a case of a zero standard deviation all matrix entries
are divided by a small number such that no \code{NaN}s occur.
}
\usage{
ma.scale2(x, missings=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A numeric matrix in which missing values are permitted
}
\item{missings}{
A logical indicating whether missings occur (or could occur) in the dataset
}
}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}

\value{
A matrix
}

%\references{
%% ~put references to the literature/web site here ~
%}


%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[base:scale]{base::scale}}
}


\examples{
#############################################################################
# EXAMPLE 1: z-standardization data.internet
#############################################################################

data(data.internet)
dat <- data.internet

# z-standardize all variables in this dataset
zdat <- miceadds::ma.scale2( dat, missings=TRUE )

\dontrun{
#############################################################################
# SIMULATED EXAMPLE 2: Speed comparison for many cases and many variables
#############################################################################

set.seed(9786)
# 3000 cases, 200 variables
N <- 3000
p <- 200
# simulate some data
x <- matrix( stats::rnorm( N*p ), N, p )
x <- round( x, 2 )

# compare computation times for 10 replications
B <- 10
    s1 <- Sys.time()        # scale in R
for (bb in 1:B){
    res <- scale(x)
} ; s2 <- Sys.time() ; d1 <- s2-s1

    s1 <- Sys.time()        # scale in miceadds
for (bb in 1:B){
    res1 <- miceadds::ma.scale2(x)
} ; s2 <- Sys.time() ; d2 <- s2-s1

# scale in miceadds with missing handling
    s1 <- Sys.time()
for (bb in 1:B){
    res1 <- miceadds::ma.scale2(x,missings=TRUE)
} ; s2 <- Sys.time() ; d3 <- s2-s1
d1      # scale in R
d2      # scale in miceadds (no missing handling)
d3      # scale in miceadds (with missing handling)
  ##   > d1      # scale in R
  ##   Time difference of 1.622431 secs
  ##   > d2      # scale in miceadds (no missing handling)
  ##   Time difference of 0.156003 secs
  ##   > d3      # scale in miceadds (with missing handling)
  ##   Time difference of 0.2028039 secs
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%% \keyword{z-Standardization}
%% \keyword{Utility function}
% __ONLY ONE__ keyword per line
