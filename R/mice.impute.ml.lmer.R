## File Name: mice.impute.ml.lmer.R
## File Version: 0.736


#*** main function for multilevel imputation with lme4 with several levels
mice.impute.ml.lmer <- function(y, ry, x, type, levels_id, variables_levels=NULL,
                random_slopes=NULL, aggregate_automatically=TRUE, intercept=TRUE,
                groupcenter.slope=FALSE, draw.fixed=TRUE, random.effects.shrinkage=1E-6,
                glmer.warnings=TRUE, model="continuous", donors=3,
                match_sampled_pars=FALSE, blme_use=FALSE, blme_args=NULL, pls.facs=0,
                interactions=NULL, quadratics=NULL, min.int.cor=0, min.all.cor=0,
                pls.print.progress=FALSE, group_index=NULL, iter_re=0, ... )
{
    require_namespace("lme4")
    res <- mice_imputation_factor_pmm_prepare(y=y)
    y <- res$y
    y_aggr <- res$y_aggr
    is_factor <- res$is_factor

    if (blme_use){
        require_namespace("blme")
    }

    #--- extraction of arguments
    pos <- parent.frame(n=2)
    res <- mice_ml_lmer_extract_input( pos=pos, levels_id=levels_id,
                random_slopes=random_slopes, variables_levels=variables_levels,
                pls.facs=pls.facs, min.int.cor=min.int.cor,
                min.all.cor=min.all.cor, interactions=interactions,
                quadratics=quadratics, model=model, group_index=group_index,
                iter_re=iter_re)
    vname <- res$vname
    p <- res$p
    type <- res$type
    data0 <- data <- res$data
    levels_id <- res$levels_id
    random_slopes <- res$random_slopes
    imp.temp <- res$imp.temp
    vname_level <- res$vname_level
    pls.facs <- res$pls.facs
    min.int.cor <- res$min.int.cor
    min.all.cor <- res$min.all.cor
    interactions <- res$interactions
    quadratics <- res$quadratics
    model <- res$model
    iter_re <- res$iter_re
    y0 <- y
    ry0 <- ry

    #--- aggregate data to a higher level if requested
    res <- mice_ml_lmer_aggregate_data_higher_level( vname_level=vname_level, y=y, ry=ry,
                    x=x, data=data, levels_id=levels_id, vname=vname )
    data <- res$data
    y <- res$y
    ry <- res$ry
    x <- res$x

    #--- define lmer functions
    res <- mice_ml_lmer_define_lmer_function( model=model, blme_use=blme_use )
    lmer_family <- res$lmer_family
    lmer_function <- res$lmer_function

    #--- arrange cluster identifiers
    res <- mice_ml_lmer_arrange_cluster_identifiers( levels_id=levels_id, data=data )
    NL <- res$NL
    ngr <- res$ngr
    clus <- res$clus
    clus_unique <- res$clus_unique

    #--- aggregate group effects for mixed effects model
    res <- mice_ml_lmer_include_cluster_means( y=y, ry=ry, type=type, x=x,
                levels_id=levels_id, aggregate_automatically=aggregate_automatically,
                clus=clus, groupcenter.slope=groupcenter.slope,
                variables_levels=variables_levels )
    x <- res$x
    type <- res$type

    #--- pls regression if required
    res <- mice_ml_lmer_interactions_pls( type=type, interactions=interactions,
                quadratics=quadratics, y=y, ry=ry, x=x, pls.facs=pls.facs,
                pls.print.progress=pls.print.progress, min.int.cor=min.int.cor,
                pos=pos, min.all.cor=min.all.cor )
    x <- res$x
    type <- res$type

    #--- create formulas for lme4
    res <- mice_ml_lmer_construct_lme4_formula( x=x, intercept=intercept,
                levels_id=levels_id, fixed_effects=colnames(x), NL=NL,
                random_slopes=random_slopes )
    fml <- res$fml
    fixed_effects <- res$fixed_effects
    used_slopes <- res$used_slopes

    #--- prepare arguments for lmer estimation
    lmer_args <- mice_ml_lmer_collect_lme4_input( y=y, x=x, ry=ry, data=data,
                        levels_id=levels_id, NL=NL, fml=fml, lmer_family=lmer_family,
                        model=model, lmer_args=lmer_args, blme_args=blme_args )
    # control_input <- FALSE
    control_input <- TRUE
    if (control_input){
        control <- mice_imputation_multilevel_lmerControl_define_optimizer(model=model,
                            ...)
        lmer_args$control <- control
    }

    #--- fit lme4 or blme model based on observed y
    fit <- mice_multilevel_doCall_suppressWarnings( what=lmer_function, args=lmer_args,
                        warnings=glmer.warnings )

    #--- draw fixed effects
    b.est <- b.star <- lme4::fixef(object=fit)
    if( draw.fixed ){     # posterior draw for fixed effects
        b.star <- mice_multilevel_draw_rnorm1( mu=b.star, Sigma=vcov(fit) )
    }

    #--- extract posterior distribution of random effects
    fl <- lme4::getME(object=fit, name="flist")

    #--- variance matrix of random effects
    fit_vc <- lme4::VarCorr(x=fit)

    # extract random effects
    re0 <- lme4::ranef(object=fit, condVar=TRUE)
    predicted <- 0
    predicted_umat <- mice_ml_lmer_define_predicted_umat( y=y, NL=NL,
                            levels_id=levels_id )

    for (ll in 1:NL){
        levels_id_ll <- levels_id[ll]
        predicted_u <- mice_ml_lmer_draw_random_effects( clus=clus[[levels_id_ll]],
                            clus_unique=clus_unique[[levels_id_ll]], y=y,
                            ry=ry, fl=fl[[levels_id_ll]], fit_vc=fit_vc[[levels_id_ll]],
                            re0=re0[[levels_id_ll]], ngr=ngr[[levels_id_ll]],
                            used_slopes=used_slopes, levels_id_ll=levels_id_ll, x=x,
                            random.effects.shrinkage=random.effects.shrinkage,
                            iter_re=iter_re)
        predicted_umat[,ll] <- predicted_u
        predicted <- predicted + predicted_u
    }

    #--- x and z for prediction
    x0 <- as.matrix( x[,fixed_effects, drop=FALSE ] )
    # handle cases of removed predictors due to singularity
    if (length(b.star) > 1){
        if ( length(b.star)-1 < ncol(x0) ){
            vars1 <- intersect(colnames(x0), names(b.star))
            x0 <- x0[, vars1, drop=FALSE]
        }
    }
    if (intercept){
        x0 <- cbind( 1, x0 )
    }
    #--- compute predicted values including fixed and random part
    pred_fixed <- x0 %*% b.star
    predicted <- pred_fixed + predicted

    #--- iterations for random effects
    predicted <- mice_ml_lmer_draw_random_effects_in_iterations( y=y,
                    ry=ry, pred_fixed=pred_fixed, iter_re=iter_re, NL=NL,
                    levels_id=levels_id, clus=clus, fit_vc=fit_vc,
                    predicted_umat=predicted_umat, predicted=predicted )

    #--- extract predicted values for cases with missing data
    predicted0 <- predicted[ !ry ]

    #--- predicted values for cases with observed data
    if ( model=="pmm"){
        pred <- predicted
        predicted1 <- pred[ ry ]
    }

    #---- draw imputations
    if ( model=="binary"){
        imp <- mice_multilevel_draw_binomial( probs=inverse_logit(predicted0) )
    }
    if ( model=="continuous"){
        sigma <- attr( fit_vc,"sc")
        imp <- mice_multilevel_imputation_draw_residuals(
                    predicted=predicted0, sigma=sigma  )
    }
    if ( model=="pmm"){
        imp <- mice_multilevel_imputation_pmm5(y=y, ry=ry, x,
                    yhatobs=predicted1, yhatmis=predicted0,
                    donors=donors, noise=1E5, ...)
    }

    #*** data postprocessing: extend data according to entries of missing values
    #    at lowest level
    imp <- mice_ml_lmer_extend_imputed_values_lower_level( imp_upper=imp, ry_lower=ry0,
                ry_upper=ry, level_ids_lower=data0[, vname_level ],
                level_ids_upper=data[, vname_level ], extend=vname_level !="" )
    imp <- mice_imputation_factor_pmm_convert_factor(imp=imp,
                    is_factor=is_factor, y_aggr=y_aggr)

    #--- output imputed values
    return(imp)
}
