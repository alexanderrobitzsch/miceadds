## File Name: mice_ml_lmer_include_cluster_means.R
## File Version: 0.182


mice_ml_lmer_include_cluster_means <- function(y, ry, type, x, levels_id,
        aggregate_automatically, clus, groupcenter.slope, variables_levels )
{
    types_sel <- names(type)[ type==1 ]
    types_sel <- intersect(types_sel, colnames(x))
    x_sel <- x[, types_sel, drop=FALSE ]
    NL <- length(levels_id)
    if (aggregate_automatically){
        for (ll in 1:NL){
            id_ll <- levels_id[ll]
            clus_ll <- clus[[ll]]
            clus_name_ll <- levels_id[ll]
            vars_aggr <- mice_ml_lmer_choice_aggregated_variables( x_sel=x_sel,
                                clus=clus_ll, eps=1e-5)
            LV <- length(vars_aggr)
            if (LV > 0){
                ind_aggr <- which( substring( names(vars_aggr), 1, 2 )=="M." )
                if ( length(ind_aggr) > 0 ){
                    vars_aggr <- vars_aggr[ - ind_aggr ]
                }
            }
            x_sel1 <- cbind( clus_ll, x_sel )
            colnames(x_sel1)[1] <- clus_name_ll
            type1 <- c( -2, rep( 1, ncol(x_sel) ) )
            names(type1) <- c( clus_name_ll, colnames(x_sel) )
            if ( LV > 0 ){
                type1[ names(vars_aggr) ] <- 3
            }
            res <- mice_multilevel_add_groupmeans( y=y, ry=ry, x=x_sel1, type=type1,
                        groupcenter.slope=groupcenter.slope,
                        aggr_label=paste0( "M.", clus_name_ll, "_" ) )
            x <- res$x
            type <- res$type
            x_sel <- x[,-1,drop=FALSE]
            type1 <- type[-1]
        }
    }
    #--- type
    type_sel <- mice_imputation_create_type_vector( variables=colnames(x_sel), value=1)

    #--- output
    res <- list( x=x_sel, type=type_sel)
    return(res)
}
