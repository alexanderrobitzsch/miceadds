## File Name: library_install.R
## File Version: 0.03
## File Last Change: 2017-02-06 11:05:48


#################################################
# load packages or install some packages
# if they are needed.
library_install <- function( pkg , ... ){
	PP <- length(pkg)
	for (vv in 1:PP){
		pp <- pkg[vv]
		ab1 <- do.call( what = require , 
					args = list( package=pp , quietly=TRUE ) )
		if ( ! ab1 ){
			utils::install.packages( pp , ...)
		}
		do.call( require , list( package=pp ) )
	}
}
################################################			
