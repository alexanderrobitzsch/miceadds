## File Name: Rcppfunction_remove_classes.R
## File Version: 0.07

Rcppfunction_remove_classes <- function(string, maxlen=70, remove=TRUE)
{
    string <- gsub("\n", "", string )
    string <- gsub("\t", "", string )
    string <- gsub("  ", "", string )

    ind1 <- string_find_first(string=string, symbol="(" )
    a1 <- c( substring(string,1, ind1-1), substring(string, ind1+1, nchar(string) ) )
    s1 <- a1[2]
    ind1 <- string_find_last(string=s1, symbol=")" )
    s1 <- substring(s1,1, ind1-1)

    s1 <- strsplit( s1, split=",", fixed=TRUE )[[1]]

    #*** Rcpp classes
    rcpp_classes <- c("double", "bool", "int", "arma::mat", "arma::colvec", "arma::umat",
                "Rcpp::NumericVector", "Rcpp::IntegerVector", "Rcpp::LogicalVector",
                "Rcpp::CharacterVector", "Rcpp::CharacterMatrix", "Rcpp::List",
                "Rcpp::NumericMatrix", "Rcpp::IntegerMatrix",
                "Rcpp::LogicalMatrix", "char" )
    rcpp_classes1 <- paste0( rcpp_classes, " " )
    if (remove){
        for (rr in rcpp_classes1 ){
            s1 <- gsub( rr, "", s1, fixed=TRUE )
            a1[1] <- gsub( rr, "", a1[1], fixed=TRUE )
        }
        a1[1] <- gsub( " ", "", a1[1] )
    }
    NS <- length(s1)
    s2 <- s1
    if (remove){
        s2 <- gsub( " ", "", s2 )
    }
    M0 <- nchar(a1[1])
    for (ss in 1:NS){
        if (remove){
            s2[ss] <- gsub( " ", "", s2[ss] )
        }
        nss <- nchar(s2[ss])
        M0 <- M0 + nss
        if (M0 > maxlen ){
            s2[ss] <- paste0("\n ", s2[ss] )
            M0 <- nss
        }
    }

    s2 <- paste0( a1[1], "( ", paste0( s2, collapse=", " ), " )\n" )
    s2 <- gsub( ",  ", ", ", s2, fixed=TRUE)
    s2 <- gsub( "(  ", "( ", s2, fixed=TRUE)
    s2 <- gsub( "  )", " )", s2, fixed=TRUE)
    #--- delete blanks at begin of lines
    for (uu in 1:2){
        s2 <- gsub("\n ", "\n", s2, fixed=TRUE)
    }
    #--- output
    return(s2)
}
