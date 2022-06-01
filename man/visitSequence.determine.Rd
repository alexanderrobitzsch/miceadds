%% File Name: visitSequence.determine.Rd
%% File Version: 0.151

\name{visitSequence.determine}
\alias{visitSequence.determine}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Automatic Determination of a Visit Sequence in \code{mice}
}
\description{
This function automatically determines a visit sequence for a specified
model in \code{\link[mice:mice]{mice::mice}} when passive variables are defined
as imputation methods. Note that redundant visits could be computed and
a user should check the plausibility of the result.
}
\usage{
visitSequence.determine(impMethod, vis, data, maxit=10)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{impMethod}{
Vector with imputation methods
}
  \item{vis}{
Initial vector of visit sequence
}
  \item{data}{
Data frame to be used for multiple imputations
}
  \item{maxit}{
Maximum number of iteration for computation of the updated visit sequence
}
}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
Updated vector of the visit sequence
}
%\references{
%% ~put references to the literature/web site here ~
%}


%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
Used in  the \code{\link[mice:mice]{mice::mice}} function as an argument.
The function \code{\link[mice:make.visitSequence]{mice::make.visitSequence}}
creates a visit sequence.
}


\examples{
\dontrun{
#############################################################################
# EXAMPLE 1: Visit sequence for a small imputation model
#############################################################################

data( data.smallscale )
# select a small number of variables
dat <- data.smallscale[, paste0("v",1:4) ]
V <- ncol(dat)

# define initial vector of imputation methods
impMethod <- rep("norm", V)
names(impMethod) <- colnames(dat)
# define variable names and imputation method for passive variables in a data frame
dfr.impMeth <- data.frame( "variable"=NA,
                  "impMethod"=NA )
dfr.impMeth[1,] <- c("v1_v1", "~ I(v1^2)" )
dfr.impMeth[2,] <- c("v2_v4", "~ I(v2*v4)" )
dfr.impMeth[3,] <- c("v4log", "~ I( log(abs(v4)))" )
dfr.impMeth[4,] <- c("v12", "~ I( v1 + v2 + 3*v1_v1 - v2_v4 )" )
# add variables to dataset and imputation methods
VV <- nrow(dfr.impMeth)
for (vv in 1:VV){
    impMethod[ dfr.impMeth[vv,1] ] <- dfr.impMeth[vv,2]
    dat[, dfr.impMeth[vv,1] ] <- NA
}

# run empty imputation model to obtain initial vector of visit sequence
imp0 <- mice::mice( dat, m=1, method=impMethod, maxit=0 )
imp0$vis

# update visit sequence
vis1 <- miceadds::visitSequence.determine( impMethod=impMethod, vis=imp0$vis, data=dat)

# imputation with updated visit sequence
imp <- mice::mice( dat, m=1, method=impMethod, visitSequence=vis1, maxit=2)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
