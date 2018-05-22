## File Name: ANSI_matrix_include_cols.R
## File Version: 0.06

ANSI_matrix_include_cols <- function(mat, empty, fill="")
{
    dfr <- ANSI_matrix_include_rows(mat=t(mat), empty=empty, fill=fill)
    dfr <- t(dfr)
    return(dfr)
}
