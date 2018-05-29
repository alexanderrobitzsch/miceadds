%% File Name: filename_split.Rd
%% File Version: 0.17

\name{filename_split}
\alias{filename_split}
\alias{string_extract_part}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Some Functionality for Strings and File Names
}
\description{
The function \code{filename_split} splits a file name into parts.

The function \code{string_extract_part} extracts a part of a string.
}
\usage{
filename_split(file_name, file_sep="__", file_ext=".")

string_extract_part( vec, part=1, sep="__", remove_empty=TRUE )
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
\item{vec}{Vector with strings}
\item{part}{Integer indicating the part of the string to be selected}
\item{sep}{String separator}
\item{remove_empty}{Logical indicating whether empty entries (\code{" "}")
should be removed.}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}

\value{
List with components of the file name (see Examples).
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
# EXAMPLE 1: Demonstration example for filename_split
#############################################################################

# file name
file_name <- "pisa_all_waves_invariant_items_DATA_ITEMS_RENAMED__DESCRIPTIVES__2016-10-12_1000.csv"

# apply function
miceadds::filename_split( file_name )
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

#############################################################################
# EXAMPLE 2: Example string_extract_part
#############################################################################

vec <- c("ertu__DES", "ztu__DATA", "guzeuue745_ghshgk34__INFO", "zzu78347834_ghghwuz")

miceadds::string_extract_part( vec=vec, part=1, sep="__" )
miceadds::string_extract_part( vec=vec, part=2, sep="__" )
  ##  > miceadds::string_extract_part( vec=vec, part=1, sep="__" )
  ##  [1] "ertu"                "ztu"                 "guzeuue745_ghshgk34"
  ##  [4] "zzu78347834_ghghwuz"
  ##  > miceadds::string_extract_part( vec=vec, part=2, sep="__" )
  ##  [1] "DES"  "DATA" "INFO" NA
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
