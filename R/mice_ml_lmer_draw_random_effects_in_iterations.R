## File Name: mice_ml_lmer_draw_random_effects_in_iterations.R
## File Version: 0.06



mice_ml_lmer_draw_random_effects_in_iterations <- function(y, ry, pred_fixed, iter_re,
    NL, levels_id, clus, fit_vc, predicted_umat, predicted)
{
    if (iter_re>0){
        nj <- list()
        sig2h <- list()
        ry1 <- 1*ry
        ytilde <- y - pred_fixed

        iter <- 1

        for (iter in 1:iter_re){
            for (ll in 1:NL){
                levels_id_ll <- levels_id[ll]
                clus_ll <- clus[[levels_id_ll]]
                if (iter==1){
                    nj[[ll]] <- ( rowsum(ry1, clus_ll) )[,1]
                    sig2h[[ll]] <- as.numeric(fit_vc[[levels_id_ll]])
                    sig2p <- attr(fit_vc, "sc")^2
                }
                pred_re_other <- rowSums( predicted_umat[,-ll] )
                y_res <- ( rowsum(ry1*(ytilde-pred_re_other), clus_ll) )[,1]
                y_res[ is.na(y_res) ] <- 0
                Vj <- 1 / ( nj[[ll]] / sig2p + 1 / sig2h[[ll]] )
                mj <- Vj / sig2p * y_res
                nll <- length(mj)
                u_imp <- stats::rnorm(nll, mean=mj, sd=sqrt(Vj))
                predicted_umat[,ll] <- u_imp[ clus_ll ]

            }
        }
        predicted <- pred_fixed + rowSums(predicted_umat)
    }
    #--- output
    return(predicted)
}
