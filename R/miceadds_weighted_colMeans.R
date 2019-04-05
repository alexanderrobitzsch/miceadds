## File Name: miceadds_weighted_colMeans.R
## File Version: 0.01

miceadds_weighted_colMeans <- function(x, imputationWeights)
{
    cx <- colMeans(x*imputationWeights) / sum(imputationWeights)
    return(cx)
}
