## File Name: GroupSum.R
## File Version: 1.02


GroupSum <- function( data , group , weights=NULL , extend=FALSE){
	groups <- sort( unique( group ) )
	index.group <- match( group , groups )
	if ( is.null(weights) ){
	    data1 <- rowsum( data , index.group , na.rm=TRUE)		
	} else {
	    data1 <- rowsum( data*weights , index.group , na.rm=TRUE)		
	}
	colnames(data1) <- colnames(data)
	data1 <- data.frame( "group" = groups , data1 )
	if (extend){
	   data1 <- data1[ index.group , ]
	   rownames(data1) <- NULL
	}	
	return(data1)
}
