%% File Name: datalist2mids.Rd
%% File Version: 0.31

\name{datlist2mids}
\alias{datalist2mids}
\alias{datlist2mids}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Converting a List of Multiply Imputed Data Sets into a \code{mids} Object
}
\description{
This function converts a list of multiply imputed data sets
to a \code{\link[mice:mids]{mice::mids}} object.
}
%% mids-class {mice}

\usage{
datlist2mids(dat.list, progress=FALSE)
datalist2mids(dat.list, progress=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat.list}{
List of multiply imputed data sets or an object of class
\code{imputationList} (see
\code{\link[mitools:imputationList]{mitools::imputationList}}
)
}
  \item{progress}{
An optional logical indicating whether conversion process be displayed
}
}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}


\value{
An object of class \code{mids}
}

%\references{
%% ~put references to the literature/web site here ~
%}


%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See \code{\link[mice:as.mids]{mice::as.mids}} for converting
a multiply imputed dataset in long format into a \code{mids} object.
}

\examples{
\dontrun{
#############################################################################
# EXAMPLE 1: Imputation of NHANES data using Amelia package
#############################################################################

library(mice)
library(Amelia)

data(nhanes,package="mice")
set.seed(566)  # fix random seed

# impute 10 datasets using Amelia
a.out <- Amelia::amelia(x=nhanes, m=10)
# plot of observed and imputed data
plot(a.out)

# convert list of multiply imputed datasets into a mids object
a.mids <- miceadds::datlist2mids( a.out$imputations )

# linear regression: apply mice functionality lm.mids
mod <- with( a.mids, stats::lm( bmi ~ age ) )
summary( mice::pool( mod ) )
  ##                    est       se         t       df     Pr(>|t|)     lo 95
  ##  (Intercept) 30.624652 2.626886 11.658158 8.406608 1.767631e-06 24.617664
  ##  age         -2.280607 1.323355 -1.723352 8.917910 1.192288e-01 -5.278451
  ##                   hi 95 nmis       fmi    lambda
  ##  (Intercept) 36.6316392   NA 0.5791956 0.4897257
  ##  age          0.7172368    0 0.5549945 0.4652567

# fit linear regression model in Zelig
library(Zelig)
mod2 <- Zelig::zelig( bmi ~ age, model="ls", data=a.out, cite=FALSE)
summary(mod2)
  ##  Model: Combined Imputations
  ##              Estimate Std.Error z value Pr(>|z|)
  ##  (Intercept)   30.625     2.627  11.658  0.00000 ***
  ##  age           -2.281     1.323  -1.723  0.08482
  ##  ---
  ##  Signif. codes:  '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# fit linear regression using mitools package
library(mitools)
datimp <- mitools::imputationList(a.out$imputations)
mod3 <- with( datimp, stats::lm( bmi ~ age ) )
summary( mitools::MIcombine( mod3 ) )
  ##  Multiple imputation results:
  ##        with(datimp, stats::lm(bmi ~ age))
  ##        MIcombine.default(mod3)
  ##                results       se    (lower     upper) missInfo
  ##  (Intercept) 30.624652 2.626886 25.304594 35.9447092     51 %
  ##  age         -2.280607 1.323355 -4.952051  0.3908368     49 %
}
}


%% \keyword{mids}
%% \keyword{mice utility function}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
