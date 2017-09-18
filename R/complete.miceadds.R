## File Name: complete.miceadds.R
## File Version: 0.06
## File Last Change: 2017-02-06 11:05:47


#######################################################
# complete function for nested multiple imputation
complete.mids.nmi <- function( x , action = c(1,1) ){
	if ( x$type == "mice" ){
        x1 <- x$imp		
		data <- mice::complete( x1[[ action[1] ]] , action = action[2] )
	}
	if ( x$type == "mice.1chain" ){
		data <- complete.mids.1chain( x$imp[[ action[1] ]] , action = action[2] )
	}							
	return(data)					
}
#######################################################


#######################################################
# complete function for objects of class mids.1chain
complete.mids.1chain <- function( x , action = 1 ){
    mice::complete( x$midsobj , action=action )
}
#######################################################
