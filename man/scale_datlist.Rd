%% File Name: scale_datlist.Rd
%% File Version: 0.23

\name{scale_datlist}
\alias{scale_datlist}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
Adding a Standardized Variable to a List of Multiply Imputed Datasets or a
Single Datasets
}

\description{
Adds a standardized variable to a list of multiply imputed datasets or
a single dataset. This function extends \code{\link[base:scale]{base::scale}}
for a data frame to a list of multiply imputed datasets.
}

\usage{
scale_datlist(datlist, orig_var, trafo_var, weights=NULL, M=0, SD=1,
    digits=NULL)
}


%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{datlist}{
A data frame, a list of multiply imputed datasets of one of the classes
\code{datlist} or \code{imputationList} or a list of nested multiply
imputed datasets of one of the classes \code{nested_datlist} or
\code{NestedImputationList}.
}
  \item{orig_var}{
Vector with names of the variables to be transformed
}
  \item{trafo_var}{
Vector with names of the standardized variables
}
  \item{weights}{
Optional vector of sample weights. Alternatively, the \code{weights}
can also be a string indicating the variable used from
\code{datlist}.
}
  \item{M}{
Mean of the transformed variable
}
  \item{SD}{
Standard deviation of the transformed variable
}
  \item{digits}{
Number of digits used for rounding the standardized variable
}
}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}

\value{
A vector or a matrix
}
%\references{
%% ~put references to the literature/web site here ~
%}

%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~


\seealso{
\code{\link[base:scale]{base::scale}}, \code{\link{ma.scale2}}
}


\examples{
\dontrun{
#############################################################################
# EXAMPLE 1: Standardized variables in list of multiply imputed datasets
#############################################################################

data(data.ma02)
datlist <- data.ma02

#--- object of class 'datlist'
datlist <- miceadds::datlist_create( datlist )

# mean and SD of variable hisei
miceadds::ma.wtd.meanNA(data=datlist, weights=datlist[[1]]$studwgt, vars="hisei" )
mean( unlist( lapply( datlist, FUN=function(data){
        stats::weighted.mean( data$hisei, data$studwgt )  } ) ) )
miceadds::ma.wtd.sdNA(data=datlist, weights=datlist[[1]]$studwgt, vars="hisei" )
mean( unlist( lapply( datlist, FUN=function(data){
        sqrt( Hmisc::wtd.var( data$hisei, data$studwgt ) ) } ) ) )

# standardize variable hisei to M=100 and SD=15
datlist1a <- miceadds::scale_datlist( datlist=datlist, orig_var="hisei",
               trafo_var="hisei100", weights=datlist[[1]]$studwgt, M=100, SD=15 )

# check mean and SD
miceadds::ma.wtd.meanNA(data=datlist1a, weights=datlist[[1]]$studwgt, vars="hisei100")
miceadds::ma.wtd.sdNA(data=datlist1a, weights=datlist[[1]]$studwgt, vars="hisei100")

#--- do standardization for unweighted sample with books <=3
# -> define a weighting variable at first
datlist0 <- mitools::imputationList( datlist )
datlist2a <- miceadds::within.imputationList( datlist0, {
             # define weighting variable
                 wgt_books <- 1 * ( books <=3 )
                    } )

# standardize variable hisei to M=100 and SD=15 with respect to weighting variable
datlist2b <- miceadds::scale_datlist( datlist=datlist2a, orig_var="hisei", trafo_var="hisei100",
         weights="wgt_books", M=100, SD=15 )

# check mean and SD (groupwise)
miceadds::ma.wtd.meanNA(data=datlist1a, weights=datlist[[1]]$studwgt, vars="hisei100")
miceadds::ma.wtd.sdNA(data=datlist1a, weights=datlist[[1]]$studwgt, vars="hisei100")

#--- transformation for a single dataset
dat0 <- datlist[[1]]
dat0a <- miceadds::scale_datlist( datlist=dat0, orig_var="hisei", trafo_var="hisei100",
                    weights=dat0$studwgt, M=100, SD=15 )
stats::weighted.mean( dat0a[,"hisei"],  w=dat0a$studwgt )
stats::weighted.mean( dat0a[,"hisei100"],  w=dat0a$studwgt )
sqrt( Hmisc::wtd.var( dat0a[,"hisei100"],  weights=dat0a$studwgt ) )

#--- Standardizations for objects of class imputationList
datlist2 <- mitools::imputationList(datlist)   # object class conversion
datlist2a <- miceadds::scale_datlist( datlist=datlist2, orig_var="hisei",
                 trafo_var="hisei100", weights=datlist[[1]]$studwgt, M=100, SD=15 )

#############################################################################
# EXAMPLE 2: Standardized variables in list of nested multiply imputed datasets
#############################################################################

# nested multiply imputed dataset in BIFIEsurvey package
data(data.timss4, package="BIFIEsurvey")
datlist <- data.timss4
wgt <- datlist[[1]][[1]]$TOTWGT

# class nested.datlist
imp1 <- miceadds::nested.datlist_create( datlist )
# class NestedImputationList
imp2 <- miceadds::NestedImputationList( datlist )

# standardize variable scsci
imp1a <- miceadds::scale_datlist( datlist=imp1, orig_var="scsci", trafo_var="zscsci", weights=wgt)
# check descriptives
miceadds::ma.wtd.meanNA( imp1a, weights=wgt, vars=c("scsci", "zscsci" ) )
miceadds::ma.wtd.sdNA( imp1a, weights=wgt, vars=c("scsci", "zscsci" ) )

#############################################################################
# EXAMPLE 3: Standardization of variables for imputed data in mice package
#############################################################################

data(nhanes, package="mice")
set.seed(76)

#--- impute nhanes data
imp <- mice::mice(nhanes)
#--- convert into datlist
datlist <- miceadds::mids2datlist(imp)
#--- scale datlist (all variables)
vars <- colnames(nhanes)
sdatlist <- miceadds::scale_datlist(datlist, orig_var=vars, trafo_var=paste0("z",vars) )
#--- reconvert to mids object
imp2 <- miceadds::datlist2mids(sdatlist)
#*** compare descriptive statistics of objects
round( miceadds::mean0( mice::complete(imp, action=1) ), 2 )
round( miceadds::mean0( mice::complete(imp2, action=1) ), 2 )
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%% \keyword{z-Standardization}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
