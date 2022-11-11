%% File Name: load.data.Rd
%% File Version: 0.449

\name{load.data}
\alias{load.data}
\alias{load.files}


%- Also NEED an '\alias' for EACH other topic documented here.
\title{
\R Utilities: Loading/Reading Data Files using \pkg{miceadds}
}

\description{
The function \code{load.data} is a wrapper function for loading or reading data frames
or matrices.

The function \code{load.files} loads multiple files in a data frame.
}

\usage{
load.data( filename, type=NULL, path=getwd(), load_fun=NULL, spss.default=TRUE, ...)

load.files( files, type=NULL, path=getwd(), ...)
}


%- maybe also 'usage' for other objects documented here.
\arguments{
\item{filename}{
Name of the data file (matrix or data frame). This can also be a part
of the file name and the most recent file is loaded.
\code{filename} can also be a vector which strings and a file
is loaded which contains all the specified strings.
}
  \item{type}{
The type of file in which the data frame or matrix should be loaded.
This can be \code{Rdata} (for \R binary format, using \code{\link{load.Rdata2}}),
\code{csv} (using \code{\link[utils:read.csv2]{utils::read.csv2}}),
\code{csv2} (using \code{\link[utils:read.csv]{utils::read.csv}}),
\code{table} (using \code{\link[utils:read.table]{utils::read.table}}; the dataset
must have the file extension \code{dat} or \code{txt}),
\code{xlsx} (using \code{\link[readxl:read_excel]{readxl::read_excel}}; or using
the extension \code{xls}),
\code{sav} (using \code{\link[foreign:read.spss]{foreign::read.spss}}),
\code{RDS}. If an alternative data loading function \code{load_fun} is chosen,
\code{type} must be the file extension.
}
\item{path}{Directory from which the dataset should be loaded.
It can also be set to \code{NULL} if the absolute path is already
included in \code{filename}.}
\item{load_fun}{User-specified loading function}
\item{spss.default}{Optional logical which is only applied
    for \code{type="sav"} indicating whether the arguments
    \code{to.data.frame=TRUE} and \code{use.value.labels=FALSE}
    are used.}
\item{\dots}{Further arguments to be passed to \code{load.Rdata2},
    \code{utils::read.csv2}, \code{utils::read.csv}, \code{utils::read.table},
    \code{readxl::read_excel}, \code{foreign::read.spss}, or \code{load_fun}.
}
\item{files}{Vector of file names}
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
See also \code{\link{load.Rdata}} for loading \R data frames.

See \code{\link{save.Rdata}} and \code{\link{save.data}}
for saving/writing \R data frames.
}


\examples{
\dontrun{
#############################################################################
# EXAMPLE 1: Toy example
#############################################################################

# load a data frame in the file "data_s3.Rdata" and save this
# as the object "dat.s3"
dat.s3 <- miceadds::load.data( filename="data_s3.Rdata", type="Rdata" )
print(str(dat.s3))

# load text input with base::readLines() function using the 'load_fun' argument
dat <- miceadds::load.data( "my_output_", type="Rout", load_fun=readLines, path=path)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%% \keyword{R utilities}
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
