## File Name: load.Rdata.R
## File Version: 1.05

#****************************************
# load R data objects globally
load.Rdata <- function( filename, objname )
{
    d1 <- load( filename )
    eval( parse( text=paste( objname,  "<<- ", d1  ) ) )
}
