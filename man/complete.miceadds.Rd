%% File Name: complete.miceadds.Rd
%% File Version: 0.214

\name{complete.miceadds}
\alias{complete.mids.nmi}
\alias{complete.mids.1chain}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Creates Imputed Dataset from a \code{mids.nmi} or \code{mids.1chain} Object
}



\description{
Creates imputed dataset from a \code{mids.nmi} or \code{mids.1chain} object.
}

\usage{
\method{complete}{mids.nmi}(data, action=c(1,1), ...)

\method{complete}{mids.1chain}(data, action=1, ...)
}

% complete.mids.nmi( x, action=c(1,1) )

% complete.mids.1chain( x, action=1 )

%- maybe also 'usage' for other objects documented here.
\arguments{
\item{data}{
Object of class \code{mids.nmi} (for \code{complete.mids.nmi}) or
\code{mids.1chain} (for \code{complete.mids.1chain})
}
  \item{action}{
A vector of length two indicating to indices of between and within
imputed dataset for for \code{complete.mids.nmi} and an integer for the
index of imputed dataset for \code{complete.mids.1chain}.
}
\item{\dots}{More arguments to be passed}
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
See also the corresponding \code{\link[mice:complete]{mice::complete}} function
and \code{\link[mitml:mitmlComplete]{mitml::mitmlComplete}}.

Imputation methods: \code{\link{mice.nmi}}, \code{\link{mice.1chain}}
}


\examples{
\dontrun{
#############################################################################
# EXAMPLE 1: Nested multiple imputation and dataset extraction for TIMSS data
#############################################################################

library(BIFIEsurvey)
data(data.timss2, package="BIFIEsurvey" )
datlist <- data.timss2

# remove first four variables
M <- length(datlist)
for (ll in 1:M){
    datlist[[ll]] <- datlist[[ll]][, -c(1:4) ]
}

#***************
# (1) nested multiple imputation using mice
imp1 <- miceadds::mice.nmi( datlist,  m=4, maxit=3 )
summary(imp1)

#***************
# (2) nested multiple imputation using mice.1chain
imp2 <- miceadds::mice.nmi( datlist, Nimp=4, burnin=10,iter=22, type="mice.1chain")
summary(imp2)

#**************
# extract dataset for third orginal dataset the second within imputation
dat32a <- miceadds::complete.mids.nmi( imp1, action=c(3,2) )
dat32b <- miceadds::complete.mids.nmi( imp2, action=c(3,2) )

#############################################################################
# EXAMPLE 2: Imputation from one chain and extracting dataset for nhanes data
#############################################################################

library(mice)
data(nhanes, package="mice")

# nhanes data in one chain
imp1 <- miceadds::mice.1chain( nhanes, burnin=5, iter=40, Nimp=4,
            method=rep("norm", 4 ) )

# extract first imputed dataset
dati1 <- miceadds::complete.mids.1chain( imp1, action=1 )
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%% \keyword{R utilities}
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
