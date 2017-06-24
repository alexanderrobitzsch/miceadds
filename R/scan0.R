
####################################################
scan.vector <- function( vec ){ 
    vec <- unlist( strsplit( vec , split="\n" , fixed=TRUE) )
    vec <- unlist( strsplit( vec , split=" " , fixed=TRUE) )
    vec <- vec[ vec != "" ] 
    return(vec)
}
scan.vec <- scan.vector
####################################################				
# scan function with default what = "character"
scan0 <- function( file="" , ...){
	scan( file=file , what="character" , ...)			
}
#########################################################		
