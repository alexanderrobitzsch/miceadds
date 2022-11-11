## File Name: load.Rdata2.R
## File Version: 1.11

#**********************************************
# load Rdata objects
load.Rdata2 <- function( filename, path=getwd(), RDS=FALSE )
{
    if (RDS){
        d1 <- readRDS( file=file.path(path,filename) )
    } else {
        d1 <- load( file=file.path(path,filename) )
    }
    objname <- "ma01"
    eval(parse(text=paste(objname, "<- ", d1)))
    eval(parse(text=paste0( "return( ", objname, ")" ) ) )
}
