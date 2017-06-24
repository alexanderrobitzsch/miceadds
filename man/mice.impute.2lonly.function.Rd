\name{mice.impute.2lonly.function}
\alias{mice.impute.2lonly.function}



\title{Imputation at Level 2 (in \pkg{miceadds})}


\description{
The imputation method \code{mice.impute.2lonly.function} is a general
imputation function for level 2 imputation which allow any
defined imputation function at level 1 in \pkg{mice}.
}



\usage{
mice.impute.2lonly.function(y, ry, x, type, imputationFunction, ...)	
}


\arguments{
  \item{y}{Incomplete data vector of length \code{n}}

  \item{ry}{Vector of missing data pattern
  (\code{FALSE}=missing, \code{TRUE}=observed)}

  \item{x}{Matrix (\code{n} x \code{p}) of complete
  covariates. Only numeric variables are permitted for
  usage of this function.}

  \item{type}{Group identifier must be specified by '-2'.
  Predictors must be specified by '1'.}

 \item{imputationFunction}{Imputation function for \pkg{mice}. Any
 	 imputation method which is defined at level 1 can be used
 	 for level 2 imputation.
 } 
  
  \item{...}{Other named arguments.}
}

\value{
A vector of length \code{nmis} with imputations.
}



% Link to functions in other packages
%\code{\link[TAM:tam.fa]{tam.fa}}

% \details{
% This function allows in combination with
% }



\seealso{
See \code{\link[mice:mice.impute.2lonly.norm]{mice::mice.impute.2lonly.norm}} and
the \code{\link[mice:mice.impute.2lonly.pmm]{mice::mice.impute.2lonly.pmm}}
function.

See also the \pkg{jomo} package (\code{\link[jomo:jomo2]{jomo::jomo2}})
for joint multilevel imputation of level 1 and
level 2 variables.
}


\examples{
\dontrun{
#############################################################################
# EXAMPLE 1: Imputation of level 2 variables
#############################################################################

#**** Simulate some data
# x,y ... level 1 variables
# v,w ... level 2 variables

G <- 250            # number of groups
n <- 20             # number of persons
beta <- .3          # regression coefficient
rho <- .30          # residual intraclass correlation
rho.miss <- .10     # correlation with missing response
missrate <- .50     # missing proportion
y1 <- rep( rnorm( G , sd = sqrt( rho ) ) , each=n ) + rnorm(G*n , sd = sqrt( 1 - rho ))
w <- rep( round( rnorm(G ) , 2 ) , each=n )
v <- rep( round( runif( G , 0 , 3 ) ) , each=n )
x <-  stats::rnorm( G*n )
y <- y1 + beta  * x + .2 * w + .1 * v
dfr0 <- dfr <- data.frame( "group" = rep(1:G , each=n ) , "x" = x , "y" = y , 
		"w" = w , "v" = v )
dfr[ rho.miss * x + rnorm( G*n , sd = sqrt( 1 - rho.miss ) ) < qnorm( missrate ) , 
		"y" ] <- NA
dfr[ rep( rnorm(G) , each=n ) < qnorm( missrate ) , "w" ] <- NA
dfr[ rep( rnorm(G) , each=n ) < qnorm( missrate ) , "v" ] <- NA

#....
# empty mice imputation
imp0 <- mice( as.matrix(dfr)  , maxit=0 )
predM <- imp0$predictorMatrix
impM <- imp0$method

#...
# multilevel imputation 
predM1 <- predM
predM1[c("w","y","v"),"group"] <- -2
predM1["y","x"] <- 1        # fixed x effects imputation
impM1 <- impM
impM1[c("y","w","v")] <- c("2l.continuous" , "2lonly.function" , "2lonly.function" )
# define imputation functions 
imputationFunction <- list( "w" = "sample" , "v" = "pmm5" )

# do imputation
imp1 <- mice::mice( as.matrix(dfr) , m = 1 , predictorMatrix = predM1 ,
           imputationMethod = impM1 , maxit = 5, 
           imputationFunction = imputationFunction )
}
}


\author{
Alexander Robitzsch
}




% \seealso{
% ccc	
% }




