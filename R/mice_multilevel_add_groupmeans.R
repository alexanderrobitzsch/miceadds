

mice_multilevel_add_groupmeans <- function( y , ry , x , type ,
		groupcenter.slope ){
	# add groupmeans in the regression model
	if ( any( type %in% c(3,4) ) ){ 
		x0 <- as.matrix(cbind( x[,type==-2], x[,type %in% c(3,4)] ))
		colnames(x0) <- c( colnames(x)[type==-2], colnames(x)[type %in% c(3,4)] )
		type0 <- c( -2, rep(1,ncol(x0)-1) )
		x0.aggr <- as.matrix( mice_multilevel_impute_groupmean(y=y, ry=ry, x=x0, 
						type=type0, grmeanwarning=FALSE ))
		colnames(x0.aggr) <- paste0("M._", colnames(x0)[-1])
		# group mean centering
		if ( groupcenter.slope ){ 
		  x0.aggr1 <- as.matrix(x0.aggr)
		  colnames(x0.aggr1) <- colnames(x0)[-1]
		  x0cent <- x0[,-1] - x0.aggr1
		  x[ , colnames(x0cent) ] <- x0cent
		}
		# combine covariate matrix
		x <- cbind( x , x0.aggr )
		# add type
		type1 <- c( type , rep(1 , ncol(x0.aggr) ) )
		names(type1) <- c( names(type) , colnames(x0.aggr) )   
		type1[ type1 == 3 ] <- 1
		type1[ type1 == 4 ] <- 2
		type <- type1
	}		
	res <- list( "x" = x , "type" = type)	
	return(res)	
}	
