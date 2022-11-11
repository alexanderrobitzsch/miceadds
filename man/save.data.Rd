%% File Name: save.data.Rd
%% File Version: 0.33

\name{save.data}
\alias{save.data}


%- Also NEED an '\alias' for EACH other topic documented here.
\title{
\R Utilities: Saving/Writing Data Files using \pkg{miceadds}
}

\description{
This function is a wrapper function for saving or writing data frames
or matrices.
}

\usage{
save.data( data, filename, type="Rdata", path=getwd(), row.names=FALSE, na=NULL,
      suffix=NULL, suffix_space="__", index=FALSE, systime=FALSE, ...)
}


%- maybe also 'usage' for other objects documented here.
\arguments{
\item{data}{Data frame or matrix to be saved}
\item{filename}{
Name of data file
}
  \item{type}{
The type of file in which the data frame or matrix should be loaded.
This can be \code{Rdata} (for \R binary format, using
\code{\link[base:save]{base::save}},
\code{csv} (using \code{\link[utils:write.csv2]{utils::write.csv2}}),
\code{csv} (using \code{\link[utils:write.csv]{utils::write.csv}}),
\code{table} (using \code{\link[utils:write.table]{utils::write.table}}),
\code{sav} (using \code{\link[sjlabelled:write_spss]{sjlabelled::write_spss}}),
\code{RDS} (using \code{\link{saveRDS}}).
  \code{type} can also be a vector if the data frame should be
  saved in multiple formats.
        }
\item{path}{Directory from which the dataset should be loaded}
\item{row.names}{Optional logical indicating whether row names
should be included in saved \code{csv} or \code{csv2} files.}
\item{na}{Missing value handling. The default is \code{""} for \code{type="csv"}
and \code{type="csv2"} and is \code{"."} for \code{type="table"}.}
\item{suffix}{Optional suffix in file name.}
\item{suffix_space}{Optional place holder if a suffix is used.}
\item{index}{Optional logical indicating whether an index should be
    included in the first column using the function
    \code{\link{index.dataframe}}.
}
\item{systime}{If \code{index=TRUE}, this optional logical indicates
    whether a time stamp should be included in the second column.
}
\item{\dots}{Further arguments to be passed to \code{save},
    \code{write.csv2}, \code{write.csv}, \code{write.table}
    or \code{sjlabelled::write_spss}.
}
}

%\details{
%For writing \code{sav} files, an installation of PSPP is necessary.
%See \code{\link{write.pspp}}.
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
See \code{\link{load.Rdata}} and \code{\link{load.data}}
for saving/writing \R data frames.
}


\examples{
\dontrun{
#############################################################################
# EXAMPLE 1: Save dataset data.ma01
#############################################################################

#*** use data.ma01 as an example for writing data files using save.data
data(data.ma01)
dat <- data.ma01

# set a working directory
pf2 <- "P:/ARb/temp_miceadds"

# save data in Rdata format
miceadds::save.data( dat, filename="ma01data", type="Rdata", path=pf2)

# save data in table format without row and column names
miceadds::save.data( dat, filename="ma01data", type="table", path=pf2,
            row.names=FALSE, na=".", col.names=FALSE)

# save data in csv2 format, including time stamp in file name
# and row index and time stamp in saved data
miceadds::save.data( dat, filename="ma01data", type="csv2", path=pf2,
            row.names=FALSE, na="", suffix=systime()[5],
            index=TRUE, systime=TRUE )

# save data in sav format
miceadds::save.data( dat, filename="ma02data", type="sav",  path=pf2 )

# save data file in different formats
types <- c("Rdata", "csv2", "sav")
sapply( types, FUN=function(type){
    miceadds::save.data( dat, filename="ma02data", type=type,  path=pf2,
               suffix=miceadds::systime()[3], row.names=TRUE  )
                                    } )

# save data frame in multiple file formats (sav, table and csv2)
miceadds::save.data( dat, filename="ma03data", type=c("sav","table","csv2"),  path=pf2,
            suffix=miceadds::systime()[7]  )
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%% \keyword{R utilities}
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
