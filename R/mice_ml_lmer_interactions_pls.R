## File Name: mice_ml_lmer_interactions_pls.R
## File Version: 0.14

mice_ml_lmer_interactions_pls <- function(type, interactions, quadratics,
        y,ry, x, pls.facs,     pls.print.progress, min.int.cor, pos,
        min.all.cor)
{
    type <- replace_values_vector(vec=type, vec_names=interactions, value=4)
    type <- replace_values_vector(vec=type, vec_names=quadratics, value=5)
    res <- mice.impute.2l.pls2( y=y, ry=ry, x=x, type=type, pls.facs=pls.facs,
                pls.print.progress=pls.print.progress, min.int.cor=min.int.cor,
                envir_pos=pos, min.all.cor=min.all.cor, pls.impMethod='xplsfacs' )
    x <- res[,-1,drop=FALSE]
    if (pls.facs > 0){
        nx <- ncol(x)
        colnames(x) <- paste0("plsf", 1:nx)
    }
    type <- mice_imputation_create_type_vector( variables=colnames(x), value=1)
    #--- output
    res <- list( x=x, type=type)
    return(res)
}
