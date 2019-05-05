## File Name: GroupSD.R
## File Version: 0.11


GroupSD <- function( data, group, weights=NULL, extend=FALSE)
{
    m1 <- GroupSum( data=data^2, group=group, weights=weights, extend=extend )
    m2 <- GroupSum( data=1+0*data, group=group, weights=weights, extend=extend )
    m3 <- GroupMean( data=data, group=group, weights=weights, extend=extend )
    res <- ( m1[,-1] - m2[,-1] * m3[,-1]^2 ) / ( abs( m2[,-1] - 1 ) + 1E-10 )
    res <- abs(res)
    res <- sqrt(res)
    res <- data.frame( m1[,1], res)
    return(res)
}
