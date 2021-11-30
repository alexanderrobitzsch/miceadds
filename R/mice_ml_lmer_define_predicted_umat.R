## File Name: mice_ml_lmer_define_predicted_umat.R
## File Version: 0.02

mice_ml_lmer_define_predicted_umat <- function(y, NL, levels_id)
{
    predicted_umat <- matrix(0, nrow=length(y), ncol=NL+2)
    colnames(predicted_umat) <- paste0("u_",1:(NL+2))
    colnames(predicted_umat)[1:NL] <- levels_id
    return(predicted_umat)
}
