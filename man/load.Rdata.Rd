%% File Name: load.Rdata.Rd
%% File Version: 0.22

\name{load.Rdata}
\alias{load.Rdata}
\alias{load.Rdata2}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
\R Utilities: Loading \code{Rdata} Files in a Convenient Way
}

\description{
These functions loads a \code{Rdata} object saved as a data frame or a matrix
in the current \R environment. The function \code{load.Rdata}
saves the loaded object in the global environment while \code{load.Rdata2}
loads the object only specified environments. Hence, usage of
\code{load.Rdata2} instead of \code{load.Rdata} is recommended.
}

\usage{
load.Rdata(filename, objname)

load.Rdata2(filename, path=getwd(), RDS=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{filename}{
Rdata file (matrix or data frame)
}
  \item{objname}{
Object name. This object will be a global variable in \R.
}
\item{path}{Directory from which the dataset should be loaded}
\item{RDS}{logical if object is saved as an RDS object}
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

\seealso{
See also \code{\link{save.Rdata}} for saving data frames in
a \code{Rdata} format.

See also: \code{\link[base:load]{base::load}}, \code{\link[base:save]{base::save}}

}


\examples{
\dontrun{
# load a data frame in the file "data_s3.Rdata" and save this
# as the object "dat.s3"
load.Rdata( filename="data_s3.Rdata", "dat.s3" )
head(dat.s3)

# Alternatively one can use the function
dat.s3 <- miceadds::load.Rdata2( filename="data_s3.Rdata")
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%% \keyword{R utilities}
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
