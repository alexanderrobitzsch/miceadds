%% File Name: subset_datlist.Rd
%% File Version: 0.41

\name{subset_datlist}
\alias{subset_datlist}
\alias{subset.datlist}
\alias{subset.imputationList}
\alias{subset.mids}
\alias{subset.mids.1chain}
\alias{subset_nested.datlist}
\alias{subset.nested.datlist}
\alias{subset.NestedImputationList}

%- Also NEED an '\alias' for EACH other topic documented here.

\title{
Subsetting Multiply Imputed Datasets and Nested Multiply Imputed Datasets
}


\description{
Returns a subsets of multiply imputed datasets or nested multiply imputed datasets.
These function allows choosing parts of the imputed datasets using the
\code{index} argument for multiply imputed datasets and \code{index_between} and
\code{index_within} for nested multiply imputed datasets as well as the application
of the \code{\link[base:subset]{base::subset}} S3 method for selecting
cases and variables in datasets.
}


\usage{
subset_datlist(datlist, subset=TRUE, select=NULL, expr_subset=NULL,
        index=NULL, toclass="datlist")

\method{subset}{datlist}(x, subset, select=NULL, expr_subset=NULL,
                     index=NULL, ...)
\method{subset}{imputationList}(x, subset, select=NULL, expr_subset=NULL,
                     index=NULL, ...)
\method{subset}{mids}(x, subset, select=NULL, expr_subset=NULL,
                     index=NULL, ...)
\method{subset}{mids.1chain}(x, subset, select=NULL, expr_subset=NULL,
                     index=NULL, ...)

subset_nested.datlist( datlist, subset=TRUE, select=NULL, expr_subset=NULL,
      index_between=NULL, index_within=NULL, toclass="nested.datlist",
          simplify=FALSE )

\method{subset}{nested.datlist}(x, subset, select=NULL, expr_subset=NULL,
                index_between=NULL, index_within=NULL, simplify=FALSE, ...)
\method{subset}{NestedImputationList}(x, subset, select=NULL, expr_subset=NULL,
                index_between=NULL, index_within=NULL, simplify=FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.

\arguments{
  \item{datlist}{
For \code{subset_datlist} it is a list of datasets or an object of class
\code{datlist}, \code{imputationList}, \code{mids} or \code{mids.1chain}. \cr
For \code{subset_nested.datlist} it is a list of datasets or an object of class
\code{nested.datlist} or \code{NestedImputationList}.
}
\item{subset}{
Logical expression indicating elements or rows to keep, see
\code{\link[base:subset]{base::subset}}. \code{subset} can also
be a numeric vector containing row indices.
}
\item{select}{Expression indicating columns to select from a data frame}
\item{expr_subset}{Expression indicating a selection criterion for
selection rows.}
  \item{index}{
Vector of indices indicating which of the multiply imputed datasets
should be selected.
}

\item{toclass}{
The object class in which the datasets should be saved.
}
\item{index_between}{Index for between nest datasets}
\item{index_within}{Index for within nest datasets}
\item{simplify}{Optional logical indicating whether a nested multiply
    imputed dataset should be simplified to a multiplied imputed dataset.
}
\item{x}{Object containing multiply imputed or nested multiply imputed
datasets}
\item{\dots}{Further arguments to be passed.}
}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}

\value{
For multiply imputed datasets: Object of class \code{datlist},
\code{imputationList} or \code{mids} \cr
For nested multiply imputed datasets: Object of class
\code{nested.datlist} or \code{NestedImputationList}.
}

%\references{
%% ~put references to the literature/web site here ~
%}



\note{
If subsetting is applied to objects of class \code{mids} (or \code{mids.1chain}),
then informations about the imputation procedure are lost.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[base:subset]{base::subset}}
}


\examples{
\dontrun{
#############################################################################
# EXAMPLE 1: Subsetting and selection of multiply imputed datasets
#############################################################################

data(data.ma02)

# define original list of datasets
datlist1a <- data.ma02
# object of class datlist
datlist1b <- miceadds::datlist_create(datlist1a)
datlist1b
# object of class imputationList
datlist1c <- mitools::imputationList(datlist1a)
datlist1c
# object of class mids
datlist1d <- miceadds::datlist2mids(datlist1a)
datlist1d

# select some imputed datasets
datlist2a <- miceadds::subset_datlist( datlist1a, index=c(5,3,7) )
datlist2a
# convert to class imputationList
datlist2b <- miceadds::subset_datlist( datlist1a, index=c(5,3,7),
                      toclass="imputationList")
datlist2b
# convert to class mids
datlist2c <- miceadds::subset_datlist( datlist1a, index=1:3, toclass="mids")
datlist2c

# select some variables
datlist3a <- miceadds::subset_datlist( datlist1a, select=c("idstud", "books")  )
datlist3a
# Because datlist1b is a datlist it is equivalent to
datlist3b <- subset( datlist1b, select=c("idstud", "books")  )
datlist3b
# operating on imputationList class
datlist3c <- miceadds::subset_datlist( datlist1c, select=c("idstud", "books")  )
datlist3c
# operating on mids class
datlist3d <- miceadds::subset_datlist( datlist1d, select=c("idstud", "books")  )
datlist3d
# selection of rows and columns in multiply imputed datasets
datlist4a <- miceadds::subset_datlist( datlist1a, index=1:5,
                  subset=datlist1a[[1]]$idschool < 1067,
                  select=c("idstud", "idschool","hisei") )
datlist4a
# convert to class mids
datlist4b <- miceadds::subset_datlist( datlist1a, index=1:5,
                  subset=datlist1a[[1]]$idschool < 1067,
                  select=c("idstud", "idschool","hisei"), toclass="mids" )
datlist4b
# The same functionality, but now applying to object of class mids datlist1d
datlist4c <- miceadds::subset_datlist( datlist1d, index=1:5,
               subset=datlist1a[[1]]$idschool < 1067,
               select=c("idstud", "idschool","hisei") )
datlist4c

# expression for selecting rows specific in each data frame
# which can result in differently sized datasets (because the variable
# migrant is imputed)
datlist5a <- miceadds::subset_datlist( datlist1a,  expr_subset=expression(migrant==1) )
datlist5a

# select the first 100 cases
datlist6a <- miceadds::subset_datlist( datlist1a, select=c("idstud", "books"),
                       subset=1:100 )
datlist6a

#############################################################################
# EXAMPLE 2: Subsetting and selection of nested multiply imputed datasets
#############################################################################

library(BIFIEsurvey)
data(data.timss4, package="BIFIEsurvey")
dat <- data.timss4

# create object of class 'nested.datlist'
datlist1a <- miceadds::nested.datlist_create( dat )
# create object of class 'NestedImputationList'
datlist1b <- miceadds::NestedImputationList(dat)

# select some between datasets
datlist2a <- subset_nested.datlist( datlist1a, index_between=c(1,3,4) )
datlist2a
# shorter version
datlist2b <- subset( datlist1a, index_between=c(1,3,4) )
datlist2b
# conversion of a NestedImputationList
datlist2c <- subset( datlist1b, index_between=c(1,3,4))
datlist2c
# select rows and columns
sel_cases <- datlist1a[[1]][[1]]$JKZONE <=42
datlist3a <- subset( datlist1a, subset=sel_cases,
                 select=c("IDSTUD","books", "ASMMAT") )
datlist3a
# remove within nest
datlist4a <- subset( datlist1a, index_within=1 )
datlist4a
# remove within nest and simplify structure
datlist4b <- subset( datlist1a, index_within=1, simplify=TRUE)
datlist4b
datlist4c <- subset( datlist1b, index_within=1, simplify=TRUE)
datlist4c
# remove between nest
datlist5a <- subset( datlist1a, index_between=1, simplify=TRUE)
datlist5a
datlist5b <- subset( datlist1b, index_between=1, simplify=TRUE)
datlist5b
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%% \keyword{subset}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
