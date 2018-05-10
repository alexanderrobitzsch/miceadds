## File Name: ANSI_matrix_include_rows.R
## File Version: 0.06

ANSI_matrix_include_rows <- function(mat , empty , fill = "")
{
    dfr <- mat
    dfr0 <- dfr
    N2 <- ncol(dfr)
    N1 <- nrow(dfr)        
    dfr0 <- rbind( dfr0 , rep(fill,N2) )    
    # do some checks here!!!
    index <- 1:N1
    ie <- N1+1
    index <- NULL
    V1 <- length(empty)
    index <- seq(1,empty[1])
    if (V1>=2){
        for (vv in 2:V1){
            index <- c( index , N1 + 1 , seq(empty[vv-1]+1,empty[vv]))
        }        
    }
    index <- c( index , N1 + 1 , seq(empty[V1]+1 , N1  ) )
    dfr <- dfr0[ index , ]
    return(dfr)
}
