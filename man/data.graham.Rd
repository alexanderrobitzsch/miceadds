%% File Name: data.graham.Rd
%% File Version: 0.24

\name{data.graham}
\alias{data.graham}

\alias{data.graham.ex3}
\alias{data.graham.ex6}
\alias{data.graham.ex8a}
\alias{data.graham.ex8b}
\alias{data.graham.ex8c}

\docType{data}

\title{
Datasets from Grahams \emph{Missing Data} Book
}


\description{
Datasets from Grahams missing data book (2012).
}


\usage{
data(data.graham.ex3)
data(data.graham.ex6)
data(data.graham.ex8a)
data(data.graham.ex8b)
data(data.graham.ex8c)
}

\format{


\itemize{
\item Dataset \code{data.graham.ex3}:

\code{'data.frame':   2756 obs. of  20 variables:} \cr
\code{ $ school  : int  1 1 1 1 1 1 1 1 1 1 ...} \cr
\code{ $ alc7    : int  1 1 1 7 3 6 1 5 4 3 ...} \cr
\code{ $ rskreb71: int  1 3 1 2 1 NA 1 2 1 2 ...} \cr
\code{ $ rskreb72: int  NA NA NA NA NA NA NA 3 2 3 ...} \cr
\code{ $ rskreb73: int  NA NA NA NA NA NA NA 2 1 2 ...} \cr
\code{ $ rskreb74: int  NA NA NA NA NA NA NA 3 2 4 ...} \cr
\code{ $ likepa71: int  4 2 3 3 2 NA 1 4 3 3 ...} \cr
\code{ $ likepa72: int  5 2 4 2 2 NA 5 3 3 2 ...} \cr
\code{ $ likepa73: int  4 1 3 3 2 NA 1 3 2 3 ...} \cr
\code{ $ likepa74: int  5 3 1 5 4 4 3 4 3 2 ...} \cr
\code{ $ likepa75: int  4 4 4 4 3 3 4 4 3 3 ...} \cr
\code{ $ posatt71: int  1 1 1 1 1 2 1 NA NA NA ...} \cr
\code{ $ posatt72: int  1 2 1 1 1 2 4 NA NA NA ...} \cr
\code{ $ posatt73: int  1 1 1 1 1 2 1 NA NA NA ...} \cr
\code{ $ alc8    : int  1 8 4 8 5 7 1 3 5 3 ...} \cr
\code{ $ rskreb81: int  1 4 1 2 2 3 2 3 1 4 ...} \cr
\code{ $ rskreb82: int  NA NA NA NA NA NA NA 3 1 4 ...} \cr
\code{ $ rskreb83: int  NA NA NA NA NA NA NA 2 1 2 ...} \cr
\code{ $ rskreb84: int  NA NA NA NA NA NA NA 3 2 4 ...} \cr
\code{ $ alc9    : int  3 NA 7 NA 5 7 NA 6 6 7 ...} \cr


\item Dataset \code{data.graham.ex6}:

\code{'data.frame':   2756 obs. of  9 variables:} \cr
\code{ $ school  : int  1 1 1 1 1 1 1 1 1 1 ...} \cr
\code{ $ program : int  0 0 0 0 0 0 0 0 0 0 ...} \cr
\code{ $ alc7    : int  1 1 1 7 3 6 1 5 4 3 ...} \cr
\code{ $ riskreb7: int  1 3 1 2 1 NA 1 2 1 2 ...} \cr
\code{ $ likepar7: int  4 2 3 3 2 NA 1 4 3 3 ...} \cr
\code{ $ posatt7 : int  1 1 1 1 1 2 1 NA NA NA ...} \cr
\code{ $ alc8    : int  1 8 4 8 5 7 1 3 5 3 ...} \cr
\code{ $ riskreb8: int  1 4 1 2 2 3 2 3 1 4 ...} \cr
\code{ $ alc9    : int  3 NA 7 NA 5 7 NA 6 6 7 ...} \cr



\item Dataset \code{data.graham.ex8a}:

\code{'data.frame':   1023 obs. of  20 variables:} \cr
\code{ $ skill1 : int  28 29 27 29 29 NA NA NA 29 NA ...} \cr
\code{ $ skill2 : int  NA NA 29 29 NA NA NA NA NA 21 ...} \cr
\code{ $ skill3 : int  NA NA 29 29 29 NA 28 10 29 25 ...} \cr
\code{ $ skill4 : int  NA 29 25 29 29 28 29 NA NA NA ...} \cr
\code{ $ skill5 : int  29 29 28 28 29 NA 29 10 NA 25 ...} \cr
\code{ $ iplanV1: int  14 18 15 17 16 NA NA NA 18 NA ...} \cr
\code{ $ iplanV2: int  NA NA 17 16 NA NA NA NA NA 16 ...} \cr
\code{ $ iplanV3: int  NA NA 16 18 18 NA 17 1 18 16 ...} \cr
\code{ $ iplanV4: int  NA 18 14 18 14 6 18 NA NA NA ...} \cr
\code{ $ iplanV5: int  13 18 12 18 18 NA 18 3 NA 5 ...} \cr
\code{ $ planA1 : int  1 0 2 8 3 NA NA NA 7 NA ...} \cr
\code{ $ planA2 : int  NA NA 0 4 NA NA NA NA NA 6 ...} \cr
\code{ $ planA3 : int  NA NA 1 4 7 NA 2 0 1 7 ...} \cr
\code{ $ planA4 : int  NA 8 0 4 6 0 0 NA NA NA ...} \cr
\code{ $ planA5 : int  0 7 1 5 7 NA 2 0 NA 6 ...} \cr
\code{ $ planV1 : int  NA NA NA NA NA NA NA NA NA NA ...} \cr
\code{ $ planV2 : int  NA NA NA NA NA NA NA NA NA 1 ...} \cr
\code{ $ planV3 : int  NA NA 1 NA NA NA NA 0 NA 1 ...} \cr
\code{ $ planV4 : int  NA NA NA NA 2 NA NA NA NA NA ...} \cr
\code{ $ planV5 : int  2 NA 2 NA NA NA NA 0 NA NA ...} \cr


\item Dataset \code{data.graham.ex8b}:

\code{'data.frame':   2570 obs. of  6 variables:} \cr
\code{ $ rskreb71: int  1 3 1 2 1 NA 1 2 1 2 ...} \cr
\code{ $ rskreb72: int  NA NA NA NA NA NA NA 3 2 3 ...} \cr
\code{ $ posatt71: int  1 1 1 1 1 2 1 NA NA NA ...} \cr
\code{ $ posatt72: int  1 2 1 1 1 2 4 NA NA NA ...} \cr
\code{ $ posatt73: int  1 1 1 1 1 2 1 NA NA NA ...} \cr
\code{ $ posatt  : int  3 4 3 3 3 6 6 NA NA NA ...} \cr



\item Dataset \code{data.graham.ex8c}:

\code{'data.frame':   2756 obs. of  16 variables:} \cr
\code{ $ s1      : int  1 1 1 1 1 1 1 1 1 1 ...} \cr
\code{ $ s2      : int  0 0 0 0 0 0 0 0 0 0 ...} \cr
\code{ $ s3      : int  0 0 0 0 0 0 0 0 0 0 ...} \cr
\code{ $ s4      : int  0 0 0 0 0 0 0 0 0 0 ...} \cr
\code{ $ s5      : int  0 0 0 0 0 0 0 0 0 0 ...} \cr
\code{ $ s6      : int  0 0 0 0 0 0 0 0 0 0 ...} \cr
\code{ $ s7      : int  0 0 0 0 0 0 0 0 0 0 ...} \cr
\code{ $ s8      : int  0 0 0 0 0 0 0 0 0 0 ...} \cr
\code{ $ s9      : int  0 0 0 0 0 0 0 0 0 0 ...} \cr
\code{ $ s10     : int  0 0 0 0 0 0 0 0 0 0 ...} \cr
\code{ $ s11     : int  0 0 0 0 0 0 0 0 0 0 ...} \cr
\code{ $ xalc7   : int  1 1 1 7 3 6 1 5 4 3 ...} \cr
\code{ $ rskreb72: int  NA NA NA NA NA NA NA 3 2 3 ...} \cr
\code{ $ likepa71: int  4 2 3 3 2 NA 1 4 3 3 ...} \cr
\code{ $ posatt71: int  1 1 1 1 1 2 1 NA NA NA ...} \cr
\code{ $ alc8    : int  1 8 4 8 5 7 1 3 5 3 ...} \cr
}

}

%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%}


\source{
The datasets were downloaded from
http://methodology.psu.edu/pubs/books/missing.
}


\references{
Graham, J. W. (2012). \emph{Missing data}. New York: Springer.
\doi{10.1007/978-1-4614-4018-5}
}

\examples{
\dontrun{
library(mitools)
library(mice)
library(Amelia)
library(jomo)

#############################################################################
# EXAMPLE 1: data.graham.8a | Imputation under multivariate normal model
#############################################################################

data(data.graham.ex8a)
dat <- data.graham.ex8a
dat <- dat[,1:10]
vars <- colnames(dat)
V <- length(vars)
# remove persons with completely missing data
dat <- dat[ rowMeans( is.na(dat) ) < 1, ]
summary(dat)

# some descriptive statistics
psych::describe(dat)

#**************
# imputation under a multivariate normal model
M <- 7  # number of imputations

#--------- mice package
# define imputation method
impM <- rep("norm", V)
names(impM) <- vars
# mice imputation
imp1a <- mice::mice( dat, method=impM, m=M, maxit=4 )
summary(imp1a)
# convert into a list of datasets
datlist1a <- miceadds::mids2datlist(imp1a)

#--------- Amelia package
imp1b <- Amelia::amelia( dat, m=M )
summary(imp1b)
datlist1b <- imp1b$imputations

#--------- jomo package
imp1c <- jomo::jomo1con(Y=dat, nburn=100, nbetween=10, nimp=M)
str(imp1c)
# convert into a list of datasets
datlist1c <- miceadds::jomo2datlist(imp1c)

# alternatively one can use the jomo wrapper function
imp1c1 <- jomo::jomo(Y=dat, nburn=100, nbetween=10, nimp=M)

#############################################################################
# EXAMPLE 2: data.graham.8b | Imputation with categorical variables
#############################################################################

data(data.graham.ex8b)
dat <- data.graham.ex8b
vars <- colnames(dat)
V <- length(vars)

# descriptive statistics
psych::describe(dat)

#*******************************
# imputation in mice using predictive mean matching
imp1a <- mice::mice( dat, m=5, maxit=10)
datlist1a <- mitools::imputationList( miceadds::mids2datlist(imp1a) )
print(datlist1a)

#*******************************
# imputation in jomo treating all variables as categorical

# Note that variables must have values from 1 to N
# use categorize function from sirt package here
dat.categ <- sirt::categorize( dat, categorical=colnames(dat), lowest=1 )
dat0 <- dat.categ$data

# imputation in jomo treating all variables as categorical
Y_numcat <- apply( dat0, 2, max, na.rm=TRUE )
imp1b <- jomo::jomo1cat(Y.cat=dat0, Y.numcat=Y_numcat, nburn=100,
                 nbetween=10, nimp=5)

# recode original categories
datlist1b <- sirt::decategorize( imp1b, categ_design=dat.categ$categ_design )
# convert into a list of datasets
datlist1b <- miceadds::jomo2datlist(datlist1b)
datlist1b <- mitools::imputationList( datlist1b )

# Alternatively, jomo can be used but categorical variables must be
# declared as factors
dat <- dat0
# define two variables as factors
vars <- miceadds::scan.vec(" rskreb71 rskreb72")
for (vv in vars){
    dat[, vv] <- as.factor( dat[,vv] )
          }
# use jomo
imp1b1 <- jomo::jomo(Y=dat, nburn=30, nbetween=10, nimp=5)

#****************************
# compare frequency tables for both imputation packages
fun_prop <- function( variable ){
            t1 <- table(variable)
            t1 / sum(t1)
                }

# variable rskreb71
res1a <-  with( datlist1a, fun_prop(rskreb71) )
res1b <-  with( datlist1b, fun_prop(rskreb71) )
summary( miceadds::NMIcombine(qhat=res1a, NMI=FALSE ) )
summary( miceadds::NMIcombine(qhat=res1b, NMI=FALSE ) )

# variable posatt
res2a <-  with( datlist1a, fun_prop(posatt) )
res2b <-  with( datlist1b, fun_prop(posatt) )
summary( miceadds::NMIcombine(qhat=res2a, NMI=FALSE ) )
summary( miceadds::NMIcombine(qhat=res2b, NMI=FALSE ) )
}
}
%% \keyword{datasets}
