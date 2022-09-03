## File Name: extract_response_matrix.R
## File Version: 0.05

extract_response_matrix <- function(datlist)
{
    m <- length(datlist)
    dat1 <- datlist[[1]]
    ri <- 0*dat1
    for (ii in 2:m){
        ri <- ri + datlist[[ii]] !=dat1
    }
    ri <- ri==0
    return(ri)
}
