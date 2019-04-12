## File Name: miceadds_weighted_colMeans.R
## File Version: 0.02

miceadds_weighted_colMeans <- function(x, imputationWeights)
{
    cx <- colSums(x*imputationWeights) / sum(imputationWeights)
    return(cx)
}
