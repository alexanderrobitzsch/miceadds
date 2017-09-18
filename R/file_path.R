## File Name: file_path.R
## File Version: 0.02
## File Last Change: 2017-02-06 11:05:47

file_path <- function( dir , file)
{
	if ( is.null(dir) ){
		p1 <- file
	} else {
		p1 <- file.path(dir, file)
	}
	return(p1)
}
