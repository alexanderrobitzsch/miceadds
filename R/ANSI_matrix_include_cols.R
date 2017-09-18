## File Name: ANSI_matrix_include_cols.R
## File Version: 0.01
## File Last Change: 2017-02-07 17:06:50

ANSI_matrix_include_cols <- function(mat , empty , fill = ""){
	dfr <- ANSI_matrix_include_rows(mat=t(mat) , empty = empty , fill=fill)
	dfr <- t(dfr)
	return(dfr)
}	
