## File Name: complete.miceadds.R
## File Version: 0.125

#*** complete function for nested multiple imputation
complete.mids.nmi <- function( data, action=c(1,1), ... )
{
    x <- data
    if ( x$type=='mice' ){
        x1 <- x$imp
        data <- mice::complete( data=x1[[ action[1] ]], action=action[2], ... )
    }
    if ( x$type=='mice.1chain' ){
        data <- complete.mids.1chain( x$imp[[ action[1] ]], action=action[2], ...)
    }
    return(data)
}


#*** complete function for objects of class mids.1chain
complete.mids.1chain <- function( data, action=1, ...)
{
    x <- data
    data <- mice::complete( data=x$midsobj, action=action, ...)
    return(data)
}

