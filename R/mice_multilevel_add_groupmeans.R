## File Name: mice_multilevel_add_groupmeans.R
## File Version: 0.155


mice_multilevel_add_groupmeans <- function( y, ry, x, type,
        groupcenter.slope, type3=3, type4=4, aggr_label="M._" )
{
    #----
    # type 3: group means should be included
    # type 4: group means and random slopes should be included
    #----
    # add groupmeans in the regression model

    if ( any( type %in% c( type3, type4) ) ){
        # x0 <- as.matrix(cbind( x[,type==-2], x[,type %in% c(type3, type4)] ))
        sel_types <- names(type)[ type %in% c(-2, type3, type4) ]
        sel_types <- intersect(sel_types, colnames(x))
        x0 <- as.matrix(x[, sel_types, drop=FALSE] )
        colnames(x0) <- sel_types
        type0 <- c( -2, rep(1,ncol(x0)-1) )
        x0.aggr <- as.matrix( mice_multilevel_impute_groupmean(y=y, ry=ry, x=x0,
                        type=type0, grmeanwarning=FALSE ))
        if (ncol(x0.aggr)>0){
            colnames(x0.aggr) <- paste0( aggr_label, colnames(x0)[-1])
        }
        # group mean centering
        if ( groupcenter.slope ){
          x0.aggr1 <- as.matrix(x0.aggr)
          colnames(x0.aggr1) <- colnames(x0)[-1]
          x0cent <- x0[,-1] - x0.aggr1
          x[, colnames(x0cent) ] <- x0cent
        }
        # combine covariate matrix
        x <- cbind( x, x0.aggr )
        # add type
        type1 <- c( type, rep(1, ncol(x0.aggr) ) )
        names(type1) <- c( names(type), colnames(x0.aggr) )
        type1[ type1==type3 ] <- 1
        type1[ type1==type4 ] <- 2
        type <- type1
    }
    res <- list( x=x, type=type)
    return(res)
}
