## File Name: fisher2cor.R
## File Version: 0.04
## File Last Change: 2017-02-06 11:05:47

##################################################
# back transform fisher correlations
fisher2cor <- function(z){ 
	c1 <- ( exp(2*z) - 1 )/ ( exp(2*z) + 1 )
	return(c1)
}
#####################################################

#####################################################
# derivative of fisher to cor
fisher2cor.D1 <- function(z, h = .001){
	( fisher2cor(z+h) - fisher2cor(z) ) / h
		}
######################################################
