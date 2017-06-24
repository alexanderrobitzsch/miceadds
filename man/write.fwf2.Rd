\name{write.fwf2}
\alias{write.fwf2}
\alias{read.fwf2}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
Reading and Writing Files in Fixed Width Format
}

\description{
Reads and writes files in fixed width format. The functions are written
for being more performant than \code{\link[utils:read.fwf]{utils::read.fwf}}.
}

\usage{
write.fwf2(dat, format.full, format.round, file)

read.fwf2( file , format.full , variables = NULL)
}
%- maybe also 'usage' for other objects documented here.

\arguments{
  \item{dat}{
Data frame (or numeric matrix)
}
  \item{format.full}{
Vector with fixed width variable lengths
}
  \item{format.round}{
Vector with digits after decimals
}
  \item{file}{
File name
}
\item{variables}{Optional vector with variable names}
}

%%\details{
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

\author{
Alexander Robitzsch
}

%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[utils:read.fwf]{utils::read.fwf}}
}
\examples{
\dontrun{	
#############################################################################
# EXAMPLE 1: Write and read a file in fixed width format
#############################################################################

# set working directory
path <- "P:/ARb/temp"
setwd(path)

# define a data frame
set.seed(9876)
dat <- data.frame( "x" = seq( 1 , 21 , len=5) , "y" = stats::runif( 5 ) ,
            "z" = stats::rnorm( 5 ) )

# save data frame in fixed width format
format.full <- c(6 , 6 , 8 )
format.round <- c( 0 , 2 , 3 )
write.fwf2( dat , format.full=format.full, format.round=format.round ,
                file = "testdata" )

# read the data
dat1 <- miceadds::read.fwf2( file = "testdata.dat" , format.full = c(6,6,8) , 
               variables = c("x","y","z") )
# check differences between data frames
dat - dat1
}	
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
