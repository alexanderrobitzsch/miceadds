## File Name: index.dataframe.R
## File Version: 1.07


####################################################
# adds an index to a data frame
index.dataframe <- function (data, systime=FALSE)
{
    data <- as.data.frame(data)
    data$index <- NULL
    data$file_created <- NULL
    data1 <- data.frame(index=seq(1, nrow(data)))
    if (systime) {
        data1$file_created <- paste0(" ", substring(Sys.time(), 1, 20))
        }
    data1 <- cbind( data1, data )
    return(data1)
}
########################################################
