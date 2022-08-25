%% File Name: with.miceadds.Rd
%% File Version: 0.43

\name{with.miceadds}
\alias{with.mids.1chain}
\alias{with.mids.nmi}
\alias{with.NestedImputationList}
\alias{with.datlist}
\alias{with.nested.datlist}
\alias{within.imputationList}
\alias{within.NestedImputationList}
\alias{within.datlist}
\alias{within.nested.datlist}
\alias{summary.mira.nmi}
\alias{withPool_MI}
\alias{withPool_NMI}

%- Also NEED an '\alias' for EACH other topic documented here.

\title{
Evaluates an Expression for (Nested) Multiply Imputed Datasets
}

\description{
Evaluates an expression for (nested) multiply imputed datasets. These
functions extend the following functions:
\code{\link[mice:with.mids]{mice::with.mids}},
\code{\link[base:with]{base::with}},
\code{\link[base:within.data.frame]{base::within.data.frame}},
\code{\link[mitools:with.imputationList]{mitools::with.imputationList}}.

The \code{withPool} functions try to pool estimates (by simple averaging)
obtained by \code{with} or a list of results of imputed datasets.
}

\usage{
\method{with}{mids.1chain}(data, expr, ...)
\method{with}{datlist}(data, expr, fun, ...)

\method{with}{mids.nmi}(data, expr, ...)
\method{with}{nested.datlist}(data, expr, fun, ...)
\method{with}{NestedImputationList}(data, expr, fun, ...)

\method{within}{datlist}(data, expr, ...)
\method{within}{imputationList}(data, expr, ...)

\method{within}{nested.datlist}(data, expr, ...)
\method{within}{NestedImputationList}(data, expr, ...)

withPool_MI(x, ...)

withPool_NMI(x, ...)

\method{summary}{mira.nmi}(object, ...)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
Object of class \code{mids.1chain}, \code{mids.nmi}, \code{imputationList}
or \code{NestedImputationList}
}
  \item{expr}{
Expression with a formula object.
}
\item{fun}{A function taking a data frame argument}
  \item{\dots}{
Additional parameters to be passed to \code{expr}.
}
\item{object}{Object of class \code{mira.nmi}.}
\item{x}{List with vectors or matrices as results of an analysis
for (nested) multiply imputed datasets.}
}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}


\value{
\code{with.mids.1chain}: List of class \code{mira}.

\code{with.mids.nmi}: List of class \code{mira.nmi}.

\code{with.datlist}: List of class \code{imputationResultList}.

\code{with.NestedImputationList} or \code{with.nested.datlist}: List of class
\code{NestedImputationResultList}.

\code{within.imputationList}: List of class \code{imputationList}.

\code{within.NestedImputationList}: List of class
\code{NestedImputationList}.

\code{withPool_MI} or \code{withPool_NMI}: Vector or matrix with
pooled estimates
}

%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Slightly modified code of \code{\link[mice:with.mids]{mice::with.mids}},
\code{\link[mice:summary.mira]{mice::summary.mira}},
\code{\link[base:within.data.frame]{base::within.data.frame}}
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See the corresponding functionality in \pkg{base}, \pkg{mice},
\pkg{mitools} and \pkg{mitml} packages:\cr
\code{\link[mice:with.mids]{mice::with.mids}},
\code{\link[mitools:with.imputationList]{mitools::with.imputationList}},
\code{\link[mitml:with.mitml.list]{mitml::with.mitml.list}},
\code{\link[base:with]{base::with}}

\code{\link[base:within.data.frame]{base::within.data.frame}},
\code{\link[mitml:within.mitml.list]{mitml::within.mitml.list}},

\code{\link[mice:summary.mira]{mice::summary.mira}},

Imputation functions in \pkg{miceadds}:
\code{\link{mice.1chain}}, \code{\link{mice.nmi}}
}


\examples{
\dontrun{
#############################################################################
# EXAMPLE 1: One chain nhanes data | application of 'with' and 'within'
#############################################################################

library(mice)
data(nhanes, package="mice")
set.seed(9090)

# nhanes data in one chain
imp <- miceadds::mice.1chain( nhanes, burnin=5, iter=40, Nimp=4 )
# apply linear regression
res <- with( imp, expr=stats::lm( hyp ~ age + bmi  ) )
summary(res)
# pool results
summary( mice::pool(res))

# calculate some descriptive statistics
res2 <- with( imp, expr=c("M1"=mean(hyp), "SD_age"=stats::sd(age) ) )
# pool estimates
withPool_MI(res2)

# with method for datlist
imp1 <- miceadds::datlist_create(imp)
res2b <- with( imp1, fun=function(data){
                    dfr <- data.frame("M"=colMeans(data),
                             "Q5"=apply( data, 2, stats::quantile, .05 ),
                             "Q95"=apply( data, 2, stats::quantile, .95 ) )
                    return(dfr)
                        } )
withPool_MI(res2b)

# convert mids object into an object of class imputationList
datlist <- miceadds::mids2datlist( imp )
datlist <- mitools::imputationList(datlist)

# define formulas for modification of the data frames in imputationList object
datlist2 <- within( datlist, {
                     age.D3 <- 1*(age==3)
                     hyp_chl <- hyp * chl
                        } )
# look at modified dataset
head( datlist2$imputations[[1]] )

# convert into a datlist
datlist2b <- miceadds::datlist_create( datlist2 )

# apply linear model using expression
mod1a <- with( datlist2, expr=stats::lm( hyp ~ age.D3 ) )
# do the same but now with a function argument
mod1b <- with( datlist2, fun=function(data){
                    stats::lm( data$hyp ~ data$age.D3 )
                        } )
# apply the same model for object datlist2b
mod2a <- with( datlist2b, expr=lm( hyp ~ age.D3 ) )
mod2b <- with( datlist2b, fun=function(data){
                    stats::lm( data$hyp ~ data$age.D3 )
                        } )

mitools::MIcombine(mod1a)
mitools::MIcombine(mod1b)
mitools::MIcombine(mod2a)
mitools::MIcombine(mod2b)

#############################################################################
# EXAMPLE 2: Nested multiple imputation and application of with/within methods
#############################################################################

library(BIFIEsurvey)
data(data.timss2, package="BIFIEsurvey" )
datlist <- data.timss2

# remove first four variables
M <- length(datlist)
for (ll in 1:M){
    datlist[[ll]] <- datlist[[ll]][, -c(1:4) ]
                }

# nested multiple imputation using mice
imp1 <- miceadds::mice.nmi( datlist,  m=4, maxit=3 )
summary(imp1)
# apply linear model and use summary method for all analyses of imputed datasets
res1 <- with( imp1, stats::lm( ASMMAT ~ migrant + female ) )
summary(res1)

# convert mids.nmi object into an object of class NestedImputationList
datlist1 <- miceadds::mids2datlist( imp1 )
datlist1 <- miceadds::NestedImputationList( datlist1 )
# convert into nested.datlist object
datlist1b <- miceadds::nested.datlist_create(datlist1)

# use with function
res1b <- with( datlist1, stats::glm( ASMMAT ~ migrant + female ) )
# apply for nested.datlist
res1c <- with( datlist1b, stats::glm( ASMMAT ~ migrant + female ) )

# use within function for data transformations
datlist2 <- within( datlist1, {
                highsc <- 1*(ASSSCI > 600)
                books_dum <- 1*(books>=3)
                rm(scsci)   # remove variable scsci
                    } )

# include random number in each dataset
N <- attr( datlist1b, "nobs")
datlist3 <- within( datlist1b, {
                rn <- stats::runif( N, 0, .5 )
                    } )

#-- some applications of withPool_NMI
# mean and SD
res3a <- with( imp1, c( "m1"=mean(ASMMAT), "sd1"=stats::sd(ASMMAT) ) )
withPool_NMI(res3a)
# quantiles
vars <- c("ASMMAT", "lang", "scsci")
res3b <- with( datlist1b, fun=function(data){
                dat <- data[,vars]
                res0 <- sapply( vars, FUN=function(vv){
                    stats::quantile( dat[,vv], probs=c(.25, .50, .75) )
                                    } )
                t(res0)
                    } )
withPool_NMI(res3b)
}
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

%% \keyword{with}
%% \keyword{summary}
%% \keyword{within}
