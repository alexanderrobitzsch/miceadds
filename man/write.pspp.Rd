%% File Name: write.pspp.Rd
%% File Version: 0.35

\name{write.pspp}
\alias{write.pspp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Writing a Data Frame into SPSS Format Using PSPP Software
}
\description{
Writes a data frame into SPSS format using the \emph{PSPP} software.
To use this function, download and install PSPP at first:
\url{http://www.gnu.org/software/pspp/pspp.html}.
}

\usage{
write.pspp(data, datafile, pspp.path, decmax=6,
   as.factors=TRUE, use.bat=FALSE)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
Data frame
}
  \item{datafile}{
Name of the output file (without file ending)
}
  \item{pspp.path}{
Path where the PSPP executable is located, e.g.
\code{"C:/Program Files (x86)/PSPP/bin/"}
}
\item{decmax}{
Maximum number of digits after decimal
}
\item{as.factors}{A logical indicating whether all factors and string entries
should be treated as factors in the output file.}
\item{use.bat}{A logical indicating whether PSPP executed via a batch file in
    the DOS mode (\code{TRUE}) or directly invoked via the \code{system} command
     from within \R (\code{FALSE}).
    }
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}

\value{
A dataset in \emph{sav} format (SPSS format).
}

%\references{
%% ~put references to the literature/web site here ~
%}


\author{
The code was adapted from
\url{https://stat.ethz.ch/pipermail/r-help/2006-January/085941.html}
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See also \code{\link[foreign:write.foreign]{foreign::write.foreign}}.

For writing sav files see also \code{haven::write_sav}
and  \code{\link[sjlabelled:write_spss]{sjlabelled::write_spss}}.

For convenient viewing \emph{sav} files we recommend the freeware
program \emph{ViewSav}, see \url{http://www.asselberghs.dds.nl/stuff.htm}.
}

\examples{
\dontrun{
#############################################################################
# EXAMPLE 1: Write a data frame into SPSS format
#############################################################################

#****
# (1) define data frame
data <- data.frame( "pid"=1000+1:5, "height"=round(rnorm( 5 ),4),
                "y"=10*c(1,1,1,2,2), "r2"=round( rnorm(5),2),
                "land"=as.factor( c( rep("A",1), rep("B", 4 ) ) ) )
#****
# (2) define variable labels
v1 <- rep( "", ncol(data) )
names(v1) <-  colnames(data)
attr( data, "variable.labels" ) <- v1
attr(data,"variable.labels")["pid"] <- "Person ID"
attr(data,"variable.labels")["height"] <- "Height of a person"
attr(data,"variable.labels")["y"] <- "Gender"

#****
# (3) define some value labels
v1 <- c(10,20)
names(v1) <- c("male", "female" )
attr( data$y, "value.labels" ) <- v1

#****
# (4a) run PSPP to produce a sav file
write.pspp( data, datafile="example_data1",
        pspp.path="C:/Program Files (x86)/PSPP/bin/" )

#****
# (4b) produce strings instead of factors
write.pspp( data, datafile="example_data2",
        pspp.path="C:/Program Files (x86)/PSPP/bin/", as.factors=FALSE )

#****
# write sav file using haven package
library(haven)
haven::write_sav( data, "example_data1a.sav" )

#****
# write sav file using sjlabelled package
library(sjlabelled)
data <- sjlabelled::set_label( data, attr(data, "variable.labels") )
sjlabelled::write_spss( data, "example_data1b.sav" )
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%% \keyword{Utility function}
%% \keyword{SPSS}
%% \keyword{PSPP (SPSS)}
