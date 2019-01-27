%% File Name: datlist_create.Rd
%% File Version: 0.17

\name{datlist_create}
\alias{datlist_create}
\alias{nested.datlist_create}
\alias{print.datlist}
\alias{print.nested.datlist}
\alias{nested.datlist2datlist}
\alias{datlist2nested.datlist}

%- Also NEED an '\alias' for EACH other topic documented here.

\title{
Creates Objects of Class \code{datlist} or \code{nested.datlist}
}
\description{
Creates objects of class \code{datlist} or \code{nested.datlist}.

The functions \code{nested.datlist2datlist} and
\code{datlist2nested.datlist} provide list conversions for imputed
datasets.
}
\usage{
datlist_create(datasets)

nested.datlist_create(datasets)

\method{print}{datlist}(x, ...)

\method{print}{nested.datlist}(x, ...)

nested.datlist2datlist(datlist)

datlist2nested.datlist(datlist, Nimp)
}
%- maybe also 'usage' for other objects documented here.

\arguments{
  \item{datasets}{
For \code{datlist_create}: List of datasets, objects of class
\code{imputationList}, \code{mids}, \code{mids.1chain},

For \code{nested.datlist_create}: nested list of datasets,
\code{NestedImputationList}, \code{mids.nmi}
}
\item{x}{Object of classes \code{datlist} or
\code{nested.datlist}
}
\item{datlist}{Object of classes \code{datlist}, \code{imputationList},
    \code{nested.datlist} or \code{NestedImputationList}.
}
\item{Nimp}{Vector of length 2 containing numbers of between
and within imputations.}
\item{\dots}{Further arguments to be passed}
}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}


\value{
Object of class \code{datlist} or \code{nested.datlist}
}

%\references{
%% ~put references to the literature/web site here ~
%}



%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}

\examples{
\dontrun{
## The function datlist_create is currently defined as
function (datasets)
{
    class(datasets) <- "datlist"
    return(datasets)
  }

#############################################################################
# EXAMPLE 1: Create object of class datlist
#############################################################################

library(BIFIEsurvey)
data(data.timss2, package="BIFIEsurvey" )
datlist <- data.timss2

# class datlist
obj1 <- miceadds::datlist_create(data.timss2)

#############################################################################
# EXAMPLE 2: Multiply imputed datasets: Different object classes
#############################################################################

library(mice)
data(nhanes2, package="mice")
set.seed(990)

# nhanes2 data imputation
imp1 <- miceadds::mice.1chain( nhanes2, burnin=5, iter=25, Nimp=5 )
# object of class datlist
imp2 <- miceadds::mids2datlist(imp1)
# alternatively, one can use datlist_create
imp2b <- miceadds::datlist_create(imp1)
# object of class imputationList
imp3 <- mitools::imputationList(imp2)
# retransform object in class datlist
imp2c <- miceadds::datlist_create(imp3)
str(imp2c)

#############################################################################
# EXAMPLE 3: Nested multiply imputed datasets
#############################################################################

library(BIFIEsurvey)
data(data.timss2, package="BIFIEsurvey" )
datlist <- data.timss2
   # list of 5 datasets containing 5 plausible values

#** define imputation method and predictor matrix
data <- datlist[[1]]
V <- ncol(data)
# variables
vars <- colnames(data)
# variables not used for imputation
vars_unused <- miceadds::scan.vec("IDSTUD TOTWGT  JKZONE  JKREP" )

#- define imputation method
impMethod <- rep("norm", V )
names(impMethod) <- vars
impMethod[ vars_unused ] <- ""

#- define predictor matrix
predM <- matrix( 1, V, V )
colnames(predM) <- rownames(predM) <- vars
diag(predM) <- 0
predM[, vars_unused ] <- 0

# object of class nmi
imp1 <- miceadds::mice.nmi( datlist, method=impMethod, predictorMatrix=predM,
                m=4, maxit=3 )
# object of class nested.datlist
imp2 <- miceadds::mids2datlist(imp1)
# object of class NestedImputationList
imp3 <- miceadds::NestedImputationList(imp2)
# redefine class nested.datlist
imp4 <- miceadds::nested.datlist_create(imp3)

#############################################################################
# EXAMPLE 4: Conversions between nested lists of datasets and lists of datasets
#############################################################################

library(BIFIEsurvey)
data(data.timss4, package="BIFIEsurvey" )
datlist <- data.timss4

# object of class nested.datlist
datlist1a <- miceadds::nested.datlist_create(datlist)
# object of class NestedImputationList
datlist1b <- miceadds::NestedImputationList(datlist)

# conversion to datlist
datlist2a <- miceadds::nested.datlist2datlist(datlist1a)  # class datlist
datlist2b <- miceadds::nested.datlist2datlist(datlist1b)  # class imputationList

# convert into a nested list with 2 between nests and 10 within nests
datlist3a <- miceadds::datlist2nested.datlist(datlist2a, Nimp=c(2,10) )
datlist3b <- miceadds::datlist2nested.datlist(datlist2b, Nimp=c(4,5) )
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%% \keyword{print}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
