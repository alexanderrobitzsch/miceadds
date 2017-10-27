%% File Name: mice.impute.grouped.Rd
%% File Version: 0.08

\name{mice.impute.grouped}
\alias{mice.impute.grouped}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Imputation of a Variable with Grouped Values
}
\description{
Imputes a variable with continuous values 
whose original values are only available as grouped values.
}

\usage{
mice.impute.grouped(y, ry, x, low=NULL , upp=NULL ,  ...)
}


%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{
Incomplete data vector of length \code{n}
}
  \item{ry}{
Vector of missing data pattern (\code{FALSE} -- missing,
\code{TRUE} -- observed)
}
  \item{x}{
Matrix (\code{n} x \code{p}) of complete covariates.
}
\item{low}{Vector with lower bound of grouping interval}
\item{upp}{Vector with upper bound of grouping interval}
  \item{\dots}{
Further arguments to be passed
}
}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}

\value{
A vector of length \code{nmis=sum(!ry)} with imputed values.
}

%\references{
%% ~put references to the literature/web site here ~
%}

\author{
Alexander Robitzsch
}

%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
This function uses the \code{\link[grouped:grouped]{grouped::grouped}}
function in the \pkg{grouped} package.	
}

\examples{
\dontrun{
#############################################################################
# EXAMPLE 1: Imputation of grouped data
#############################################################################	
	
data(data.ma06)
data <- data.ma06

# define the variable "FC_imp" which should contain the variables to be imputed
data$FC_imp <- NA
V <- ncol(data)
# variables not to be used for imputation 
vars_elim <-  c("id" , "FC","FC_low","FC_upp")

# define imputation methods
impM <- rep("norm" , V)
names(impM) <- colnames(data)
impM[  vars_elim ] <- ""
impM[ "FC_imp" ] <- "grouped"

# define predictor matrix
predM <- 1 - diag( 0 , V)
rownames(predM) <- colnames(predM) <- colnames(data)
predM[vars_elim, ] <- 0
predM[,vars_elim] <- 0

# define lower and upper boundaries of the grouping intervals
low <- list("FC_imp" = data$FC_low )
upp <- list("FC_imp" = data$FC_upp )

# perform imputation
imp <- mice::mice( data , imputationMethod = impM , predictorMatrix = predM , 
	    m=1 , maxit=3 , allow.na=TRUE ,  low=low , upp=upp)
head( mice::complete(imp))
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{mice imputation method}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
