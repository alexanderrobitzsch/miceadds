## File Name: mice.impute.2l.contextual.norm.R
## File Version: 0.08
mice.impute.2l.contextual.norm <- function (y, ry, x, type , ridge = 10^(-5) , 
			imputationWeights = NULL , interactions=NULL , quadratics = NULL ,  ...){									
	res <- mice_imputation_get_states( pos = parent.frame(n=1) )	
	vname <- res$vname
	newstate <- res$newstate   
	# data preparation
	xcov <- .a2l.contextual.auxiliary( y = y  , ry=ry , x=x , type=type , ...)     
	#------
	# norm imputation at level 2
	ximp <- mice.impute.weighted.norm( y= y , ry=ry, x = xcov , ridge = ridge , 
				imputationWeights = imputationWeights , 
                interactions= interactions , quadratics = quadratics ,  ... ) 
	return(ximp)
}

	
	

#......................................
.a2l.contextual.auxiliary <- function( y , ry , x , type , ...){
	# extract cluster index
	clusterx <- x[,type == -2 ]
	x1 <-  as.matrix(x[,type %in% c(1,2) ])
	if ( sum( type==2)  > 0 ){   
        z <-  as.matrix(x[,type == 2 ]) 
        # calculate aggregated values
        a1 <- stats::aggregate( z , list( clusterx ) , mean , na.rm=FALSE)
        colnames(a1)[-1] <- paste0( "M." , colnames(z) )
    } 
	# calculate aggregated value for y
	a21 <- stats::aggregate( y , list( clusterx ) , sum , na.rm=FALSE)
	a22 <- stats::aggregate( 1+0*y , list( clusterx ) , sum , na.rm=FALSE)
	ic <- match( clusterx , a21[,1] )
	y2 <- ( a21[ ic , 2] - y ) / ( a22[ ic , 2 ] - 1 )
	y2[ is.na(y2) ] <- mean(y2,na.rm=TRUE)                   
	if ( sum( type==2)  > 0 ){   
		xcov <- as.matrix( cbind(  x1 , a1[ ic , -1 ] , y2 ) )
    } else {
		xcov <- as.matrix( cbind(  x1 ,  y2 ) )
    }
	vname <- get("vname", pos = parent.frame()) 
	colnames(xcov)[ ncol(xcov) ] <- paste0("M1." , vname )
	return(xcov)     
}
###########################################################################################


