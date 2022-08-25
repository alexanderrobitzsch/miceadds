## File Name: filename_split_vec.R
## File Version: 0.05


filename_split_vec <- function( file_names, file_sep="__", file_ext=".")
{
    NF <- length(file_names)
    dfr <- NULL
    for (ff in seq_len(NF) ){
        res0 <- filename_split( file_name=file_names[ff], file_sep=file_sep,
                                    file_ext=file_ext)
        res0 <- as.data.frame(res0)
        dfr <- rbind(dfr, res0)
    }
    return(dfr)
}
