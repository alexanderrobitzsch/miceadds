## File Name: mice_imputation_draw_bootstrap_sample.R
## File Version: 0.03

mice_imputation_draw_bootstrap_sample <- function(ry, imputationWeights, use_boot)
{
    if (use_boot){
        ind <- which(ry)
        n <- sum(ry)
        vals <- sample(ind, replace=TRUE)
        t1 <- table(vals)
        imputationWeights <- rep(0,length(ry))
        imputationWeights[as.numeric(names(t1))] <- t1
    }
    return(imputationWeights)
}
