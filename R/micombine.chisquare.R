## File Name: micombine.chisquare.R
## File Version: 0.23


micombine.chisquare <- function( dk, df, display=TRUE, version=1)
{
    M <- length(dk)
    if (version==0){
        mean.dk <-  mean( dk )
        sdk.square <- stats::var( sqrt(dk) )
        Dval <- ( mean.dk / df  - ( 1 - 1/M) * sdk.square )/ (1+(1 + 1/M) * sdk.square)
        df2 <- ( M - 1 )/ df^(3/M)  * ( 1  + M / ( M + 1/M) / sdk.square )^2
    }
    if (version==1){
        g2 <- dk
        m <- length(g2)
        g <- sqrt(g2)
        mg2 <- sum(g2)/m
        r <- (1+1/m)*(sum(g^2)-(sum(g)^2)/m)/(m-1)
        Dval <- (mg2/df - r*(m+1)/(m-1))/(1+r)
        df2 <-  (m-1)*(1+1/r)**2/df^(3/m)
    }
    pval <- stats::pf( Dval, df1=df, df2=df2, lower.tail=FALSE)

    #--- chi square approximation
    chisq.approx <- Dval * df
    p.approx <- 1 - stats::pchisq( chisq.approx, df=df )
    res <- c( D=Dval, p=pval, df=df, df2=df2 )
    if (display){
        cat("Combination of Chi Square Statistics for Multiply Imputed Data\n")
        cat(paste( "Using", M, "Imputed Data Sets\n"))
        cat( paste( "F(",df,", ", round(df2,2),")", "=", round( Dval, 3 ),
                    "     p=", round(pval,5), sep=""), "\n" )
    }
    invisible(res)
}


