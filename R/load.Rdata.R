## File Name: load.Rdata.R
## File Version: 1.02
## File Last Change: 2017-02-06 11:05:48

#****************************************
# load R data objects globally
load.Rdata <- function( filename , objname ){ 
    d1 <- load( filename )
    eval( parse( text=paste( objname ,  "<<- " , d1  ) ) )
}
