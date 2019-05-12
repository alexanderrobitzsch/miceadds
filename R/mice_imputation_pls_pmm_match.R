## File Name: mice_imputation_pls_pmm_match.R
## File Version: 0.02


mice_imputation_pls_pmm_match <- function(yhatobs, yhatmis, y, ry, donors=5)
{
    x1 <- mice_multilevel_imputation_pmm5(y=y, ry=ry, x=NULL, yhatobs=yhatobs,
                        yhatmis=yhatmis, donors=donors)
    return(x1)
}
