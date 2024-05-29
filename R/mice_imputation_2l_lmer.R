## File Name: mice_imputation_2l_lmer.R
## File Version: 0.624


#**** main function for multilevel imputation with lme4 which
# is just wrapper by methods "2l.continuous", "2l.binary" and "2l.pmm"
mice_imputation_2l_lmer <- function(y, ry, x, type, intercept=TRUE,
                    groupcenter.slope=FALSE, draw.fixed=TRUE,
                    random.effects.shrinkage=1E-6,
                    glmer.warnings=TRUE, model="continuous", donors=5,
                    match_sampled_pars=FALSE,
                    blme_use=FALSE, blme_args=NULL,...)
{
    require_namespace('lme4')
    if (blme_use){
        require_namespace('blme')
    }

    #--- preliminary calculations
    clus <- x[,type==-2]
    clus_unique <- unique(clus)
    ngr <- length(clus_unique)
    clus_name <- colnames(x)[type==-2]  # name of cluster identifier

    #* select previous lme4 optimizer 'bobyqa'
    # control_input <- FALSE
    control_input <- TRUE
    if (control_input){
        control <- mice_imputation_multilevel_lmerControl_define_optimizer(
                                    model=model, ...)
    }

    # arguments for lmer model
    if ( model=='binary'){
        lmer_family <- stats::binomial(link='logit')
        if (blme_use){
            lmer_function <- blme::bglmer
        } else {
            lmer_function <- lme4::glmer
        }
    }
    if ( model %in% c('continuous','pmm') ){
        if (blme_use){
            lmer_function <- blme::blmer
        } else {
            lmer_function <- lme4::lmer
        }
    }

    #--- add group means (if needed)
    res <- mice_multilevel_add_groupmeans( y=y, ry=ry, x=x, type=type,
                groupcenter.slope=groupcenter.slope)
    x <- res$x
    type <- res$type

    #--- create formulas for lme4
    rhs.f <- mice_multilevel_create_formula( variables=colnames(x)[type %in% c(1,2)],
                    include_intercept=intercept )
    rhs.r <- mice_multilevel_create_formula( variables=colnames(x)[type==2],
                    include_intercept=TRUE )

    # combine formula elements
    fml <- paste0( 'dv._lmer ~ ', rhs.f, '+(', rhs.r,'|', clus_name,')' )

    #*** prepare arguments for lmer estimation
    y1 <- y
    y1[!ry] <- NA
    dat_lme4 <- data.frame(dv._lmer=y1, x)
    lmer_args <- list( formula=fml, data=dat_lme4, na.action='na.omit')
    if (control_input){
        lmer_args$control <- control
    }
    if ( model=='binary'){
        lmer_args$family <- lmer_family
    }

    # apply blme arguments if provided
    lmer_args <- mice_multilevel_imputation_blme_args(
                        lmer_args=lmer_args, blme_args=blme_args )

    # fit based on observed y
    fit <- mice_multilevel_doCall_suppressWarnings( what=lmer_function,
                    args=lmer_args, warnings=glmer.warnings )

    # clusters without missing values
    clus0 <- clus[!ry]

    #--- draw fixed effects
    b.est <- b.star <- lme4::fixef(fit)
    if( draw.fixed ){     # posterior draw for fixed effects
        b.star <- mice_multilevel_draw_rnorm1( mu=b.star, Sigma=vcov(fit) )
    }

    #--- extract posterior distribution of random effects
    fl <- lme4::getME(fit, 'flist')[[1]]
    # ind <- match( clus0, clus_unique)
    index_clus <- match( clus, clus_unique)
    # clusters with at least one observation
    clus_obs <- match( unique(fl), clus_unique)
    fit_VarCorr <- lme4::VarCorr(fit)
    vu <- fit_VarCorr[[1]][,,drop=FALSE ]     # random effects (co-)variance
    # extract random effects
    re0 <- lme4::ranef(fit, condVar=TRUE)[[1]]
    NR <- ncol(re0)
    re <- matrix(0, nrow=ngr, ncol=NR)     # re: 0 if fully unobserved
    re[clus_obs,] <- as.matrix(re0)        # re: EAP if partially observed

    pv0 <- attr(re0, 'postVar')
    pv <- array(0, dim=c(NR,NR,ngr))
    pv[,,clus_obs] <- pv0                # pv: post. variance if partially observed
    pv[,,-clus_obs] <- vu                # pv: random effects cov. if fully unobserved

    #--- draw random effects
    u <- mice_multilevel_imputation_draw_random_effects( mu=re, Sigma=pv,
                ridge=random.effects.shrinkage )
    #--- x and z for prediction
    x0 <- as.matrix( x[,type>=1,drop=FALSE ] )
    z0 <- as.matrix( x[,type==2,drop=FALSE ] )
    if(intercept){
        x0 <- cbind(1,x0)
        z0 <- cbind(1,z0)
    }

    if (length(b.est)!=ncol(x0)){
        x00 <- stats::model.matrix(fit)
        select_cols <- intersect(colnames(x0), colnames(x00))
        x0 <- x0[, select_cols]
        if (intercept){
            x0 <- cbind(1,x0)
        }
    }

    #--- compute predicted values including fixed and random part
    predicted <- x0 %*% b.star + rowSums( z0 * u[index_clus, 1L:NR, drop=FALSE])

    # predicted values for cases with missing data
    predicted0 <- predicted[ !ry ]
    # predicted values for cases with observed data
    if ( model=='pmm'){
        # use non-sampled values here, see corresponding mice approach
        # after this function
        if (match_sampled_pars){
            # non-mice approach
            pred <- x0 %*% b.star + rowSums( z0 * u[index_clus,1L:NR,drop=FALSE] )
        } else {
            # 'the mice approach'
            pred <- x0 %*% b.est + rowSums( z0 * re[index_clus,1L:NR,drop=FALSE] )
        }
        predicted1 <- pred[ ry ]
    }

    #---- draw imputations
    if ( model=='binary'){
        imp <- mice_multilevel_draw_binomial( probs=inverse_logit(predicted0) )
    }
    if ( model=='continuous'){
        sigma <- attr( fit_VarCorr,'sc')
        imp <- mice_multilevel_imputation_draw_residuals( predicted=predicted0,
                            sigma=sigma )
    }
    if ( model=='pmm'){
        imp <- mice_multilevel_imputation_pmm5(y=y, ry=ry, x=x,
                    yhatobs=predicted1, yhatmis=predicted0,
                    donors=donors, noise=1E5, ...)
    }

    #--- output imputed values
    return(imp)
}

#----------------------------------
# mice: predictive mean matching

##            yhatobs <- x[ry,] %*% parm$coef
##            yhatmis <- x[!ry,] %*% parm$beta

