%% File Name: mi.anova.Rd
%% File Version: 0.13
%% File Last Change: 2017-02-06 11:12:48

\name{mi.anova}
\alias{mi.anova}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Analysis of Variance for Multiply Imputed Data Sets (Using the \eqn{D_2} Statistic)
}
\description{
This function combines \eqn{F} values from analysis of variance using
the \eqn{D_2} statistic which is based on combining \eqn{\chi^2} statistics
(see Allison, 2001; \code{\link{micombine.F}}, \code{\link{micombine.chisquare}}).
}
\usage{
mi.anova(mi.res, formula, type=2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mi.res}{
Object of class \code{mids} or \code{mids.1chain}
}
  \item{formula}{
Formula for \code{lm} function. Note that this can be also a string.
}
\item{type}{
Type for ANOVA calculations. For \code{type=3}, the 
\code{\link[car:Anova]{car::Anova}} function
from the \pkg{car} package is used.
}
}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}

\value{
A list with the following entries:
\item{r.squared}{Explained variance \eqn{R^2}}
\item{anova.table}{ANOVA table}
}

\references{
Allison, P. D. (2002). \emph{Missing data}. Newbury Park, CA: Sage.
}


\author{
Alexander Robitzsch
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
This function uses \code{\link{micombine.F}} and
\code{\link{micombine.chisquare}}.

See \code{\link[mice:pool.compare]{mice::pool.compare}} and
\code{\link[mitml:testModels]{mitml::testModels}} for model
comparisons based on the \eqn{D_1} statistic. The \eqn{D_2} statistic
is also included in \code{\link[mitml:testConstraints]{mitml::testConstraints}}.
}

\examples{
#############################################################################
# EXAMPLE 1: nhanes2 data | two-way ANOVA
#############################################################################

library(mice)
library(car)
data(nhanes2, package="mice")
set.seed(9090)

# nhanes data in one chain and 8 imputed datasets
mi.res <- miceadds::mice.1chain( nhanes2 , burnin=4 , iter=20 , Nimp=8 )
# 2-way analysis of variance (type 2)
an2a <- miceadds::mi.anova(mi.res=mi.res, formula="bmi ~ age * chl" )
# 2-way analysis of variance (type 3)
an2b <- miceadds::mi.anova(mi.res=mi.res, formula="bmi ~ age * chl" , type=3)

#****** analysis based on first imputed dataset

# extract first dataset
dat1 <- mice::complete( mi.res$mids )

# type 2 ANOVA
lm1 <- stats::lm( bmi ~ age * chl , data = dat1 )
summary( stats::aov( lm1 ) )
# type 3 ANOVA
lm2 <- stats::lm( bmi ~ age * chl , data= dat1, contrasts=list(age=contr.sum))
car::Anova( lm2 , type=3)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ANOVA}
\keyword{mids}
