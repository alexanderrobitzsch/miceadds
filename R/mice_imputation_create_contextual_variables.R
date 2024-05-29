## File Name: mice_imputation_create_contextual_variables.R
## File Version: 1.081


mice_imputation_create_contextual_variables <- function( y, ry, x, type, ...)
{
    # extract cluster index
    clusterx <- x[,type==-2 ]
    x1 <-  as.matrix(x[,type %in% c(1,2) ])
    clusterx_list <- list(clusterx)
    if ( sum(type==2) > 0 ){
        z <-  as.matrix(x[,type==2 ])
        # calculate aggregated values
        a1 <- stats::aggregate( z, clusterx_list, mean, na.rm=FALSE)
        colnames(a1)[-1] <- paste0( 'M.', colnames(z) )
    }
    # calculate aggregated value for y
    a21 <- stats::aggregate( y, clusterx_list, sum, na.rm=FALSE)
    a22 <- stats::aggregate( 1+0*y, clusterx_list, sum, na.rm=FALSE)
    ic <- match( clusterx, a21[,1] )
    y2 <- ( a21[ ic, 2] - y ) / ( a22[ ic, 2 ] - 1 )
    y2[ is.na(y2) ] <- mean(y2,na.rm=TRUE)
    if ( sum( type==2)  > 0 ){
        xcov <- as.matrix( cbind(  x1, a1[ ic, -1 ], y2 ) )
    } else {
        xcov <- as.matrix( cbind(  x1,  y2 ) )
    }
    vname <- ma_exists_get('vname', pos=parent.frame(n=1))
    colnames(xcov)[ ncol(xcov) ] <- paste0('M1.', vname )
    return(xcov)
}


miceadds_create_contextual_variables <- mice_imputation_create_contextual_variables
.a2l.contextual.auxiliary <- mice_imputation_create_contextual_variables
