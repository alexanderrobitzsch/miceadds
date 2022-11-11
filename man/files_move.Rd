%% File Name: files_move.Rd
%% File Version: 0.181

\name{files_move}
\alias{files_move}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Moves Files from One Directory to Another Directory
}
\description{
Moves older (defined in alphanumeric order) files from one
directory to another directory. If directories do not exist,
they will be automatically created.
}

\usage{
files_move(path1, path2, file_sep="__", pattern=NULL, path2_name="__ARCH")
}


%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{path1}{
Original directory
}
  \item{path2}{
Target directory in which the files should be moved
}
  \item{file_sep}{
Separator for files
}
  \item{pattern}{
Pattern in file names to be searched for
}
\item{path2_name}{Part of the name of \code{path2} if argument
\code{path2} is missing. If \code{path2} is not provided, it has to be
a subdirectory of \code{path1}.}
}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}


%\value{
%%  ~Describe the value returned
%}

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
\code{\link{filename_split}}
}

\examples{
\dontrun{
#############################################################################
# EXAMPLE 1: Move older files in '__ARCHIVE' directory
#############################################################################

# specify path
path1 <- "p:/IPN/Projects/PISA/Trend_2015/2__Data/All_Waves/"
# specify target directory which is an archive
path2 <- file.path( path1, "__ARCHIVE" )
# move files
files_move( path1, path2 )
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
