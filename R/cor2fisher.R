## File Name: cor2fisher.R
## File Version: 0.03
## File Last Change: 2017-02-06 11:05:47

######################################
# transformation of correlation
cor2fisher <- function(r){
	f1 <- 1/2* log( ( 1 + r) / ( 1 - r ) )
	return(f1)
}
########################################		
  
