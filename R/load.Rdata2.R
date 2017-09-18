## File Name: load.Rdata2.R
## File Version: 1.02
## File Last Change: 2017-02-06 11:05:48

#**********************************************
# load Rdata objects			
load.Rdata2 <- function( filename , path=getwd() ){
    d1 <- load( file=file.path(path,filename) )
    objname <- "ma01"
	eval(parse(text = paste(objname, "<- ", d1)))	
    eval(parse(text= paste0( "return( " , objname , ")" ) ) )			
}
