%% File Name: jomo2datlist.Rd
%% File Version: 0.16

\name{jomo2datlist}
\alias{jomo2datlist}
\alias{jomo2mids}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Converts a \pkg{jomo} Data Frame in Long Format into a List of Datasets or an Object
of Class \code{mids}
}
\description{
Converts a \pkg{jomo} data frame in long format into a list of datasets
or an object of class \code{mids}.
}
\usage{
jomo2datlist(jomo.dataframe, variable="Imputation")

jomo2mids(jomo.dataframe, variable="Imputation")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{jomo.dataframe}{
Data frame generated in \pkg{jomo} package
}
  \item{variable}{
Variable name for imputation index
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}

\value{
List of multiply imputed datasets
}
%\references{
%% ~put references to the literature/web site here ~
%}


%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See the \pkg{jomo} package.
}

\examples{
\dontrun{
#############################################################################
# EXAMPLE 1: Dataset nhanes | jomo imputation and conversion into a data list
#############################################################################

data(nhanes, package="mice")
dat <- nhanes

# impute under multivariate normal model in jomo
imp1 <- jomo::jomo1con(Y=dat, nburn=100, nbetween=10, nimp=5)
# convert into a list of datasets
datlist1 <- miceadds::jomo2datlist(imp1)
# convert into mids object
datlist2 <- miceadds::jomo2datlist(imp1)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
