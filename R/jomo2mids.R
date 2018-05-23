## File Name: jomo2mids.R
## File Version: 0.03


jomo2mids <- function( jomo.dataframe, variable="Imputation" )
{
    datlist <- jomo2datlist( jomo.dataframe=jomo.dataframe, variable=variable )
    datlist <- datlist2mids( dat.list=datlist)
    return(datlist)
}
