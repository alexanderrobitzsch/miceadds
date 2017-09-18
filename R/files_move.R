## File Name: files_move.R
## File Version: 0.14
## File Last Change: 2017-07-09 14:36:19

files_move <- function( path1 , path2 , file_sep ="__", pattern = NULL)
{

	#*** search for all relevant files
	files <- list.files( path1 , "\\."  )
	if ( ! is.null( pattern) ){
		files <- grep( pattern = pattern , x = files , value=TRUE)
	}
	#*** create file overview
	NF <- length(files)
	if (NF > 0 ){
		matr <- matrix( NA , nrow=NF , ncol=5)	
		for (ff in 1:NF){
			file_ff <- files[ff]
			res_ff <- filename_split( file_name = file_ff , file_sep = file_sep )
			matr[ff,] <- unlist(res_ff)
		}	
		matr <- as.data.frame(matr)
		colnames(matr) <- names(res_ff)
		matr$main_id <- match( matr$main , unique( matr$main) )
		
		matr <- matr[ order( matr$main ) , ]
		matr$eq <- c(0,matr$main[ - NF ] != matr$main[-1])
		t1 <- table( matr$main_id )
		ind1 <- match( matr$main_id , names(t1))
		matr$freq <- t1[ind1]		
		for (ff in 2:NF){
			if ( matr[ff,"main"] == matr[ ff -1 , "main"] ){
				file.rename( from= file.path( path1 , matr[ff-1,"file_name"] ) ,
						to = file.path( path2 , matr[ff-1,"file_name"] ) )					
				cat("*** Move " , paste0(matr[ff-1,"file_name"]) , "\n" )
				utils::flush.console();
			}
	
		}
	}
}
