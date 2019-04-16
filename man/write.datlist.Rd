%% File Name: write.datlist.Rd
%% File Version: 0.14

\name{write.datlist}
\alias{write.datlist}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Write a List of Multiply Imputed Datasets
}
\description{
Writes a list of multiply imputed datasets.
}

\usage{
write.datlist(datlist, name, include.varnames=TRUE, type="csv2",
     separate=TRUE, Mplus=FALSE, round=NULL, Rdata=TRUE,
     subdir=TRUE, ...)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{datlist}{
List of imputed datasets. Can also be an object of class
  \code{mids}, \code{mids.1chain} or \code{imputationList}
  }
  \item{name}{
Name of files to be saved
}
  \item{include.varnames}{
Logical indicating whether variables should be saved
}
  \item{type}{
File type of datasets to be saved, see \code{\link{save.data}}.
}
\item{separate}{Logical indicating whether imputed datasets
    should be written in separate files.
}
  \item{Mplus}{
Logical indicating whether files should be written for usage
in Mplus software
}
  \item{round}{
Number of digits to round after decimal. The default is no rounding.
}
  \item{Rdata}{
Logical indicating whether \code{datlist} should also be saved
in \R binary format.
  }
\item{subdir}{Logical indicating whether results should be written into
a subdirectory.}
\item{\dots}{
Further arguments to be passed to \code{\link{save.data}}.
}
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
See also \code{\link[mice:mids2mplus]{mice::mids2mplus}},
\code{\link[mice:mids2spss]{mice::mids2spss}} and
\code{\link{write.mice.imputation}} for writing objects of class
\code{mids}.

See also \code{Amelia::write.amelia} for
writing imputed datasets in \pkg{Amelia}.
}


\examples{
\dontrun{
#############################################################################
# EXAMPLE 1: Write data list imputed in mice
#############################################################################

data(data.ma01)
dat <- as.matrix(data.ma01)

# start with empty imputation
imp0 <- mice::mice( dat, maxit=0)

# modify predictor matrix
predM <- imp0$predictorMatrix
predM[, c("idschool", "idstud" ) ] <- 0
# modify imputation method
impMeth <- imp0$method
impMeth[ impMeth=="pmm" ] <- "norm"

# do imputations in mice
imp <- mice::mice( dat, predictorMatrix=predM, method=impMeth, m=3, maxit=4 )

# write imputed data in format "csv2" and round after 4 digits
write.datlist( datlist=imp, name="mice_imp_csv2", round=4 )
# write imputed data in R binary format
write.datlist( datlist=imp, name="mice_imp_Rdata", type="Rdata")
# write data for Mplus usage
write.datlist( datlist=imp, name="mice_imp_Mplus", Mplus=TRUE, round=5)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
