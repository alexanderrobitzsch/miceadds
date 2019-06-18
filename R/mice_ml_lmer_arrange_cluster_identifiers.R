## File Name: mice_ml_lmer_arrange_cluster_identifiers.R
## File Version: 0.12


mice_ml_lmer_arrange_cluster_identifiers <- function( levels_id, data )
{
    NL <- length(levels_id)
    clus_unique__ <- list()
    clus__ <- list()
    ngr <- list()
    for ( ll in 1:NL){
        clus <- data[, levels_id[ll] ]
        clus__[[ll]] <- clus
        clus_unique <- unique(clus)
        clus_unique__[[ll]] <- clus_unique
        ngr[[ll]] <- length(clus_unique)
    }
    names(clus_unique__) <- levels_id
    names(ngr) <- levels_id
    names(clus__) <- levels_id
    #---- output
    res <- list( NL=NL, clus=clus__, ngr=ngr, clus_unique=clus_unique__ )
    return(res)
}
