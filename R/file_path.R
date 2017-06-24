
file_path <- function( dir , file)
{
	if ( is.null(dir) ){
		p1 <- file
	} else {
		p1 <- file.path(dir, file)
	}
	return(p1)
}
