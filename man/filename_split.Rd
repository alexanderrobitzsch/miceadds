\name{filename_split}
\alias{filename_split}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Splits a File Name into Parts
}
\description{
Splits a file name into parts.
}
\usage{
filename_split(file_name, file_sep = "__", file_ext = ".")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file_name}{
File name
}
  \item{file_sep}{
Separator within file name
}
  \item{file_ext}{
Separator for file extension
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}

\value{
List witrh components of the file name (see Examples).
}
%\references{
%% ~put references to the literature/web site here ~
%}
%\author{
%%  ~~who you are~~
%}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{files_move}}
}


\examples{
#############################################################################
# EXAMPLE 1: Demonstration example
#############################################################################	

# file name
file_name <- "pisa_all_waves_invariant_items_DATA_ITEMS_RENAMED__DESCRIPTIVES__2016-10-12_1000.csv"

# apply function
filename_split( file_name )
  ##  $file_name
  ##  [1] "pisa_all_waves_invariant_items_DATA_ITEMS_RENAMED__DESCRIPTIVES__2016-10-12_1000.csv"
  ##  $stem
  ##  [1] "pisa_all_waves_invariant_items_DATA_ITEMS_RENAMED__DESCRIPTIVES"
  ##  $suffix
  ##  [1] "2016-10-12_1000"
  ##  $ext
  ##  [1] "csv"
  ##  $main
  ##  [1] "pisa_all_waves_invariant_items_DATA_ITEMS_RENAMED__DESCRIPTIVES.csv"
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
