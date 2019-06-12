%% File Name: filename_split.Rd
%% File Version: 0.363

\name{filename_split}
\alias{filename_split}
\alias{string_extract_part}
\alias{string_to_matrix}
\alias{filename_split_vec}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Some Functionality for Strings and File Names
}
\description{
The function \code{filename_split} splits a file name into parts.

The function \code{string_extract_part} extracts a part of a string.

The function \code{string_to_matrix} converts a string into a matrix.
}
\usage{
filename_split(file_name, file_sep="__", file_ext=".")
filename_split_vec( file_names, file_sep="__", file_ext=".")

string_extract_part( vec, part=1, sep="__", remove_empty=TRUE )

string_to_matrix(x, rownames=NULL, col_elim=NULL, as_numeric=FALSE,
               diag_val=NULL, extend=FALSE, col1_numeric=FALSE, split=" ")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file_name}{
File name
}
\item{file_names}{File names}
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
\item{x}{String vector}
\item{rownames}{Column index for row names}
\item{col_elim}{Indices for elimination of columns}
\item{as_numeric}{Logical indicating whether numeric conversion is requested}
\item{diag_val}{Optional values for inclusion in diagonal of matrix}
\item{extend}{Optional indicating whether numeric matrix should be
extended to become a symmetric matrix}
\item{col1_numeric}{Logical indicating whether second column is selected in such
a way that it has to be always a numeric (see Example 5)}
\item{split}{String used for splitting}
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

\dontrun{
#############################################################################
# EXAMPLE 3: Reading descriptive information from published articles
#############################################################################
data(data.ma08)
library(stringr)

#**** reading correlations (I)
dat <- data.ma08$mat1
miceadds::string_to_matrix(dat, rownames=2, col_elim=c(1,2))

#**** reading correlations including some processing (II)
dat0 <- data.ma08$mat2
dat <- dat0[1:14]

# substitute "*"
dat <- gsub("*", "", dat, fixed=TRUE )

# replace blanks in variable names
s1 <- stringr::str_locate(dat, "[A-z] [A-z]")
start <- s1[,"start"] + 1
for (ss in 1:length(start) ){
    if ( ! is.na( start[ss] ) ){
        substring( dat[ss], start[ss], start[ss] ) <- "_"
    }
}

# character matrix
miceadds::string_to_matrix(dat)
# numeric matrix containing correlations
miceadds::string_to_matrix(dat, rownames=2, col_elim=c(1,2), as_numeric=TRUE, diag_val=1,
           extend=TRUE )
#** reading means and SDs
miceadds::string_to_matrix(dat0[ c(15,16)], rownames=1, col_elim=c(1), as_numeric=TRUE )

#**** reading correlations (III)
dat <- data.ma08$mat3
dat <- gsub(" age ", "_age_", dat, fixed=TRUE )
miceadds::string_to_matrix(dat, rownames=2, col_elim=c(1,2), as_numeric=TRUE, diag_val=1,
       extend=TRUE )

#**** reading correlations (IV)
dat <- data.ma08$mat4 <- dat0

# remove spaces in variable names
dat <- gsub(" age ", "_age_", dat, fixed=TRUE )
s1 <- stringr::str_locate_all(dat, "[A-z,.] [A-z]")
NL <- length(dat)
for (ss in 1:NL ){
    NR <- nrow(s1[[ss]])
    if (NR>1){
        start <- s1[[ss]][2,1]+1
        if ( ! is.na( start ) ){
            substring( dat[ss], start, start ) <- "_"
        }
    }
}

miceadds::string_to_matrix(dat, rownames=2, col_elim=c(1,2), as_numeric=TRUE, diag_val=1,
     extend=TRUE )

#############################################################################
# EXAMPLE 4: Input string of length one
#############################################################################


pm0 <- "
0.828
0.567 0.658
0.664 0.560 0.772
0.532 0.428 0.501 0.606
0.718 0.567 0.672 0.526 0.843"

miceadds::string_to_matrix(x=pm0, as_numeric=TRUE, extend=TRUE)

#############################################################################
# EXAMPLE 5: String with variable names and blanks
#############################################################################

tab1 <- "
Geometric Shapes .629 .021 (.483) -.049 (.472)
Plates .473 .017 (.370) .105 (.405)
Two Characteristics .601 .013 (.452) -.033 (.444)
Crossing Out Boxes .597 -.062 (.425) -.036 (.445)
Numbers/Letters .731 .004 (.564) .003 (.513)
Numbers/Letters mixed .682 .085 (.555) .082 (.514)"

miceadds::string_to_matrix(x=tab1, col1_numeric=TRUE)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
