## File Name: syn_da_compute_pls_factors.R
## File Version: 0.07

syn_da_compute_pls_factors <- function(dat2, ncomp, syn_vars_ss, form, ind0, ind1)
{
    #-- extract design matrices
    Y <- as.matrix(dat2[ind0, syn_vars_ss, drop=FALSE])
    X <- model.matrix(form, dat2[ind0,])
    form1 <- as.formula( paste0( " ~ ", paste0( paste(form)[3], collapse=" + ") ) )
    X1 <- model.matrix(form1, dat2[ind1,])
    N0 <- length(ind0)

    ncomp <- min(ncol(X), ncomp)

    #-- perform PLS dimension reduction
    mod0 <- kernelpls.fit2(X=as.matrix(X), Y=Y, ncomp=ncomp)
    scores <- mod0$scores
    scores1 <- predict.kernelpls.fit2(mod0,X=X1)

    #--- include PLS factors in dataset
    plsfacs <- paste0("F",1:ncomp)
    S2 <- rbind(scores, scores1)
    colnames(S2) <- plsfacs
    dat20 <- data.frame(dat2, S2)
    form20 <- paste0( "y ~ ", paste0(plsfacs, collapse=" + ") )

    #--- output
    res <- list( ncomp=ncomp, N0=N0, form=form, scores=scores,
                    scores1=scores1, plsfacs=plsfacs, dat20=dat20, form20=form20)
    return(res)
}
