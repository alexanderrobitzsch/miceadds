## File Name: grep.vec.R
## File Version: 1.02
## File Last Change: 2017-02-06 11:05:47

##################################################
# vector version of grep
grep.vec <- function( pattern.vec , x , operator="AND"){
	x0 <- x
	xv <- NULL
    for (vv in 1:(length(pattern.vec) ) ){
	if (operator == "AND"){
        x <- x[ grep( pattern.vec[vv] , x ) ]
					} else {
			xv <- union( xv ,x0[ grep( pattern.vec[vv] , x0 ) ] )
						}
                }
	if (operator!="AND"){ x <- xv }
		index.x <- which( x0 %in% x )
		res <- list( "x" = x , "index.x" = index.x )
        return(res)
        }
##################################################	
