## File Name: mice_imputation_tricube_pmm_match.R
## File Version: 0.04



#---- tricube predictive mean matching
#----   weighted according Tukey's tricube function
mice_imputation_tricube_pmm_match <- function (z, yhat=yhat, y=y, donors=3,
        tricube.pmm.scale=.2, ...)
{
    eps1 <- 1e-4
    d <- abs(yhat - z)
    donorset <- which( rank(d, ties.method="ran") <=donors )
    s.tricube <- tricube.pmm.scale * mean( d )
    prob.x <- unlist( sapply( d, FUN=function(dd){
                        ( 1- min( dd / s.tricube, 1 )^3  )^3 } ) )
    # prevent the case that all weights are equal to zero
    prob.x[ donorset ] <- prob.x[donorset] + eps1
    # standardize weights to probabilities
    prob.x <- prob.x / sum(prob.x)
    m <- sample( y, size=1, prob=prob.x )
    return(m)
}

.tricube.pmm.match <- mice_imputation_tricube_pmm_match
