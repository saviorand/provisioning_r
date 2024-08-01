# note this is a quick & (really) crappy way of doing this, basically just repeating an altered (and stripped-down) version of the mod_analysis loop

efn <-  paste( fp,"mod_analysis/mod_multisp_", v_run , "_300dpi.png", sep="" )

a$ymins <- c ( 46 , 40  , 7    )   # hale fooddef sani
a$ymaxs <- c ( 72 , 101 , 101  )   # hale fooddef sani


ll     <-  list ( ) # loop variable for temporary results


#### ----- create ps data sets / aux variables to be used (_u) ------- ####
# depending on which ps data set (variable selection) to use: ps_m or ps_hsv


if ( sw$mod_use_ps_hsv ){ 
  
  ps_u      <-  ps_hsv 
  
  ps_os_u   <-  ps_os_hsv
  
  nv$ps_u   <-  nv$ps_hsv
  fn$ps_u   <-  fn$ps_hsv
  
  tf$ps_u_tf_use  <- tf$ps_hsv_tf_use
  
  pn$ps_u   <- pn$ps_hsv
  
  
  
}  else {  
  
  ps_u      <-  ps_m 
  
  ps_os_u   <-  ps_os4m
  
  nv$ps_u   <-  nv$ps_m 
  fn$ps_u   <-  fn$ps_m
  
  tf$ps_u_tf_use  <- tf$ps_m_tf_use
  
  pn$ps_u   <- pn$ps_m
  
}



g$ns_m_more_is_better_as_sign <-  0 * g$ns_m_more_is_better 
g$ns_m_more_is_better_as_sign [ g$ns_m_more_is_better ]   <-   1
g$ns_m_more_is_better_as_sign [ ! g$ns_m_more_is_better ] <-  -1

a$ns_floor    <- c( min( ns_os4m$hale ) , -9999 , min(ns_os4m$fooddef) , -9999 , min(ns_os4m$water) ,
                    min(ns_os4m$sani), min(ns_os4m$edubas) ,   min(ns_os4m$povgap320)  , -9999 ) 
a$ns_ceiling  <- c( max( ns_os4m$hale ) , 9999  , max(ns_os4m$fooddef) , 9999  , max(ns_os4m$water) ,
                    max(ns_os4m$sani), max(ns_os4m$edubas) ,   max(ns_os4m$povgap320)  , 9999 ) 



g$sign_normaliser_imprNS_dNS_dPS_u  <- array(  data = 1 , dim = c ( nv$ns_m , nv$ps_u ) )

for ( nn in 1:nv$ns_m) {
  for ( pp in 1:nv$ps_u ){
    
    if ( ! tf$ps_u_tf_use[ pp ] == "sat"    ) { g$sign_normaliser_imprNS_dNS_dPS_u [ nn , pp ]  <-   g$sign_normaliser_imprNS_for_effects_on_ns_m[ nn ] }
    if ( tf$ps_u_tf_use[ pp ] == "sat"    )  { g$sign_normaliser_imprNS_dNS_dPS_u [ nn , pp ]   <-   ( - 1 )* g$sign_normaliser_imprNS_for_effects_on_ns_m[ nn ] }
    
    
  }
}




############# START PLOT #############


if ( sw$plot_mod_multisp ) {
  
  
  dir.create( paste( fp, "mod_analysis/" , sep="" )  , recursive = TRUE )
  
  png( filename= efn ,res = 300, width=14.8,height=18.5, unit = "cm" , type = "cairo" )   # activates png engine if figures are to be exported

  par(mfcol = c(5,3), oma = c(0.6, 0, 0, 0), mar = c( 1.6 , 2.3, 1, 0.7), mgp = c(2,0.4,0))  
  # bottom left top right
  
  

###################### START MEGA-LOOP ###############################
###################### START MEGA-LOOP ###############################
###################### START MEGA-LOOP ###############################


  
  
  ll$nrow  <-  0
  
  ll  <- list()  # create list for output variables
  
  
  
  ni <- 0
  
  for ( nn in which( fn$ns_m  %in%  c("hale" , "fooddef", "sani" ) ) ) {
    
    ni <- ni + 1 
    
    ll$ns  <-  ns_m[ , nn ]
    ll$ns_os   <-  ns_os4m[ , nn ]
    
    ll$ns_os_range  <-  abs(  a$ns_ceiling[nn] - a$ns_floor[nn] )
    

    for ( ee in which( fn$enu_m  %in%  c("tfec") ) ) {
      
      ll$enu  <-  enu_m[ , ee ]
      ll$enu_os   <-   enu_os4m[ , ee ]
      
      ll$enu_sortENU      <-  sort ( ll$enu )
      ll$ind_sortENU      <-  sort ( ll$enu  , index.return = TRUE )$ix
      
      ll$enu_os_sortENU   <-   ll$enu_os[ ll$ind_sortENU ]
      

        for ( pp in c(6,5,2,12,4) ) {  # gov gini elecacc resrent gdp 
        
          ll$nrow <-  ll$nrow + 1
          
          ll$ps     <-  ps_u[ , pp ]
          ll$ps_os  <-  ps_os_u[ , pp ]
          
          ll$ps_sortPS    <-   sort ( ll$ps )
          ll$ind_sortPS   <-   sort ( ll$ps , index.return = TRUE )$ix
          
          ll$ps_os_sortPS    <-   sort ( ll$ps_os )     ## note this is NOT necessarily the same as sortPS because for sat trafo, the order is reversed in fs vs os
          ll$ind_sortPSos   <-   sort ( ll$ps_os , index.return = TRUE )$ix    ## note this is NOT necessarily the same as sortPS because for sat trafo, the order is reversed in fs vs os
          
          ll$enu_sortPS         <-   ll$enu[ ll$ind_sortPS ]
          ll$enu_os_sortPS      <-   ll$enu_os[ ll$ind_sortPS ]
          ll$ps_os_sortPS       <-   ll$ps_os[ ll$ind_sortPS ]
          
          ll$ps_sortENU      <-   ll$ps [ ll$ind_sortENU ]
          ll$ps_os_sortENU   <-   ll$ps_os [ ll$ind_sortENU ]
          
          ll$ns_sortENU     <-   ll$ns [ ll$ind_sortENU ]
          ll$ns_os_sortENU  <-   ll$ns [ ll$ind_sortENU ]
          
          
          ### ---- calculate ps values at given percentiles (for constrained prediction) ----- ###
          
          ll$ps_p50  <-  median ( ll$ps_sortPS )
          
       
          
          ### ---- bivariate models --- ###
          
          mo1e  <-   lm( ll$ns  ~  ll$enu )   # mo1e = model object, 1 predictor, energy as predictor
          mo1p  <-   lm( ll$ns  ~  ll$ps  )   # mo1p = model object, 1 predictor, , PS as predictor
          
          mo1e_rob  <- lm_robust( ll$ns  ~  ll$enu , se_type = sw$HC )
          mo1p_rob  <- lm_robust( ll$ns  ~  ll$ps , se_type = sw$HC )
          
          
          mo1e$b1_enu_gate    <-  (summary(mo1e_rob))$coefficients[2,4]   <   ch$th_p_b_bivar
          mo1p$b1_ps_gate     <-  (summary(mo1p_rob))$coefficients[2,4]   <   ch$th_p_b_bivar
          
          
          
          
          ####################################################################
          ####################################################################
          
          ## multiplicative 2-predictor OLS regression (with interaction)
          mo2i  <-   lm( ll$ns  ~  ll$enu * ll$ps )
          mo2i_rob  <-   lm_robust( ll$ns  ~  ll$enu * ll$ps , se_type = sw$HC )
          
          
          # extracting model coefficients
          mo2i$a       <-  mo2i$coefficients[1]
          mo2i$b1_enu  <-  mo2i$coefficients[2]
          mo2i$b2_ps   <-  mo2i$coefficients[3]
          mo2i$b3_int  <-  mo2i$coefficients[4]
          
          
          # calculate variance-covariance matrix (and extract terms)
          mo2i_rob$varcov  <-   vcovHC ( mo2i , type = sw$HC)
          
          mo2i_rob$var_b1_enu       <- mo2i_rob$varcov[2,2]
          mo2i_rob$var_b2_ps        <- mo2i_rob$varcov[3,3]
          mo2i_rob$var_b3_int       <- mo2i_rob$varcov[4,4]
          mo2i_rob$cov_b1ENU_b3INT  <- mo2i_rob$varcov[4,2]
          mo2i_rob$cov_b2PS_b3INT   <- mo2i_rob$varcov[4,3]
        
          mo2i$ps_signi_b2b3joint  <-  (wald.test(b = coef(mo2i_rob), Sigma = vcovHC( mo2i , type = sw$HC), Terms = 3:4))$result$chi2[[3]] # tests joint significance of b2(PS) and b3 (ENU*PS).
          
          
          # calculate standard errors for the marginal effects of ENU and PS, respectively
          
          mo2i$marg_eff_enu_sortPS  <-   mo2i$b1_enu  +  ( mo2i$b3_int * ll$ps_sortPS )  # this is dNS/dENU as a function of PS, sorted by PS
          
          mo2i$std_err_marg_eff_enu_sortPS  <-   sqrt ( mo2i_rob$var_b1_enu  +  ( ll$ps_sortPS ^2 ) * mo2i_rob$var_b3_int  +
                                                          2 * ll$ps_sortPS * mo2i_rob$cov_b1ENU_b3INT )
          
          
          mo2i$marg_eff_ps_sortENU  <-   mo2i$b2_ps  +  ( mo2i$b3_int * ll$enu_sortENU )   # this is dNS/dPS as a function of ENU, sorted by ENU
          
          mo2i$std_err_marg_eff_ps_sortENU  <-   sqrt ( mo2i_rob$var_b2_ps + ( ll$enu_sortENU ^2 ) * mo2i_rob$var_b3_int  +
                                                          2 * ll$enu_sortENU * mo2i_rob$cov_b2PS_b3INT )
          
          
          
          
          # calculate 95% confidence intervals, assuming normal distribution (z = 1.96 for 95% confidence intervals) ; zscore now set in ch$zscore_se_margeff_mod
          mo2i$marg_eff_enu_CIlower_sortPS  <-  mo2i$marg_eff_enu_sortPS   -  ch$zscore_se_margeff_mod * mo2i$std_err_marg_eff_enu_sortPS
          mo2i$marg_eff_enu_CIupper_sortPS  <-  mo2i$marg_eff_enu_sortPS   +  ch$zscore_se_margeff_mod * mo2i$std_err_marg_eff_enu_sortPS
          
          mo2i$marg_eff_ps_CIlower_sortENU  <-  mo2i$marg_eff_ps_sortENU   -  ch$zscore_se_margeff_mod * mo2i$std_err_marg_eff_ps_sortENU
          mo2i$marg_eff_ps_CIupper_sortENU  <-  mo2i$marg_eff_ps_sortENU   +  ch$zscore_se_margeff_mod * mo2i$std_err_marg_eff_ps_sortENU
          
          mo2i$marg_eff_enu_flag_signi_sortPS  <-  (( mo2i$marg_eff_enu_CIlower_sortPS < 0 ) & (mo2i$marg_eff_enu_CIupper_sortPS < 0 )) |
            (( mo2i$marg_eff_enu_CIlower_sortPS > 0 ) & (mo2i$marg_eff_enu_CIupper_sortPS > 0 ))
          
          mo2i$marg_eff_ps_flag_signi_sortENU   <-  (( mo2i$marg_eff_ps_CIlower_sortENU < 0 ) & (mo2i$marg_eff_ps_CIupper_sortENU < 0 )) |
            (( mo2i$marg_eff_ps_CIlower_sortENU > 0 ) & (mo2i$marg_eff_ps_CIupper_sortENU > 0 ))
          
          
          ## robustness / continutiy check: for cases where the min / max significant values are the actual min / max, check that the bottom 5 / top 5 are all significant (for robustness), else set them to FALSE
          if (  ( min( which( mo2i$marg_eff_enu_flag_signi_sortPS ) ) == 1 ) & any( ! mo2i$marg_eff_enu_flag_signi_sortPS[c(1:5)] ) ) {
            mo2i$marg_eff_enu_flag_signi_sortPS[c(1:5)] <- FALSE
          }  
          
          if (  ( max( which( mo2i$marg_eff_enu_flag_signi_sortPS ) ) == nc$hs ) & any( ! mo2i$marg_eff_enu_flag_signi_sortPS[c( (nc$hs-4) : nc$hs)] ) ) {
            mo2i$marg_eff_enu_flag_signi_sortPS[ c( (nc$hs-4) : nc$hs) ] <- FALSE
          }  
          
          
          if (  ( min( which( mo2i$marg_eff_ps_flag_signi_sortENU ) ) == 1 ) & any( ! mo2i$marg_eff_ps_flag_signi_sortENU[c(1:5)] ) ) {
            mo2i$marg_eff_ps_flag_signi_sortENU[c(1:5)] <- FALSE
          }  
          
          if (  ( max( which( mo2i$marg_eff_ps_flag_signi_sortENU ) ) == nc$hs ) & any( ! mo2i$marg_eff_ps_flag_signi_sortENU[c( (nc$hs-4) : nc$hs)] ) ) {
            mo2i$marg_eff_ps_flag_signi_sortENU[ c( (nc$hs-4) : nc$hs) ] <- FALSE
          }  
          
          
          
          
          
          mo2i$marg_eff_enu_perc_datapoints_signif  <-  sum ( mo2i$marg_eff_enu_flag_signi_sortPS ) / length ( ll$ns )  # the denominator here is N (which is equal to length(ll$ns) ), but leave as is because it is easier to adopt (or not to forget) for the non-hs cases
          mo2i$marg_eff_ps_perc_datapoints_signif   <-  sum ( mo2i$marg_eff_ps_flag_signi_sortENU ) / length ( ll$ns )   # the denominator here is N (which is equal to length(ll$ns) ), but leave as is because it is easier to adopt (or not to forget) for the non-hs cases
          
          
          mo2i$marg_eff_enu_sortPS_ind_first_last_signif   <-   c( min( which( mo2i$marg_eff_enu_flag_signi_sortPS ) ),
                                                                   max( which( mo2i$marg_eff_enu_flag_signi_sortPS) ) )
          mo2i$marg_eff_ps_sortENU_ind_first_last_signif   <-   c( min( which( mo2i$marg_eff_ps_flag_signi_sortENU ) ),
                                                                   max( which( mo2i$marg_eff_ps_flag_signi_sortENU ) ) )
          
          
          mo2i$marg_eff_enu_sortPS_percentiles_first_last_signif   <-   round( mo2i$marg_eff_enu_sortPS_ind_first_last_signif * 100 / nc$hs )
          mo2i$marg_eff_ps_sortENU_percentiles_first_last_signif   <-   round( mo2i$marg_eff_ps_sortENU_ind_first_last_signif * 100 / nc$hs )
          
          
          
          ### ----- prepare predictions at representative values ------ ####
          
          ll$ps_mean   <-  mean (  ll$ps ) 
          ll$enu_mean  <-  mean ( ll$enu ) 
          
          mo2i$ind_of_mean_in_ps_sortPS     <-  which( abs( ll$ps_sortPS - ll$ps_mean ) == min( abs( ll$ps_sortPS - ll$ps_mean ) ) )[1]  # index of the mean of PS ; [1] is just in case of ties, as is the case with infra
          mo2i$perc_of_mean_in_ps_sortPS    <-  round ( mo2i$ind_of_mean_in_ps_sortPS * 100 / nc$hs )
          
          mo2i$ind_of_mean_in_enu_sortENU     <-  which( abs( ll$enu_sortENU - ll$enu_mean ) == min( abs( ll$enu_sortENU - ll$enu_mean ) ) )[1]  # index of the mean of PS
          mo2i$perc_of_mean_in_enu_sortENU    <-  round ( mo2i$ind_of_mean_in_enu_sortENU * 100 / nc$hs )
          
          
          if( ! tf$ps_u_tf_use[ pp ] == "sat" ){
            
            mo2i$min_PS_with_signi_dNS_dENU  <-  ll$ps_sortPS[ mo2i$marg_eff_enu_sortPS_ind_first_last_signif[1] ]
            mo2i$max_PS_with_signi_dNS_dENU  <-  ll$ps_sortPS[ mo2i$marg_eff_enu_sortPS_ind_first_last_signif[2] ]
            
            mo2i$perc_of_min_PS_with_signi_dNS_dENU  <-  mo2i$marg_eff_enu_sortPS_percentiles_first_last_signif[1]
            mo2i$perc_of_max_PS_with_signi_dNS_dENU  <-  mo2i$marg_eff_enu_sortPS_percentiles_first_last_signif[2]
            
          }
          
          ### need to reverse the order for those ps variables that have been sat-transformed (all sat-transformed are max-type = sign-inverting! )
          
          if( tf$ps_u_tf_use[ pp ] == "sat" )  {
            
            mo2i$min_PS_with_signi_dNS_dENU  <-  ll$ps_sortPS[ mo2i$marg_eff_enu_sortPS_ind_first_last_signif[2] ]
            mo2i$max_PS_with_signi_dNS_dENU  <-  ll$ps_sortPS[ mo2i$marg_eff_enu_sortPS_ind_first_last_signif[1] ]
            
            mo2i$perc_of_min_PS_with_signi_dNS_dENU  <-  ( 100 - mo2i$marg_eff_enu_sortPS_percentiles_first_last_signif[2] ) + 1
            mo2i$perc_of_max_PS_with_signi_dNS_dENU  <-  ( 100 - mo2i$marg_eff_enu_sortPS_percentiles_first_last_signif[1] ) + 1
            
            mo2i$perc_of_mean_in_ps_sortPS   <-  100 - mo2i$perc_of_mean_in_ps_sortPS + 1
          }
          
          
          mo2i$is_margeff_enu_at_ps_mean_signi <-
            ( mo2i$ind_of_mean_in_ps_sortPS  >  mo2i$marg_eff_enu_sortPS_ind_first_last_signif[1] ) &
            ( mo2i$ind_of_mean_in_ps_sortPS  <  mo2i$marg_eff_enu_sortPS_ind_first_last_signif[2] )
          
          
          # (slightly convoluted way of ) finding out whether the marginal effect of ENU at mean PS is significant
          
          
          
          ### for dNS/dPS ( = f(ENU)), don't need to reverse the order for sat-transformed ps variables, and don't need to invert the sign of the coefficients either: dNS/dPS_fs will just be negative, but dPS_os / dPS_fs is negative, so that's ok

          
          
          ### ----- predictions at representative values of PS: NS = f (ENU) , m(PS) ------ ####
          # If dNS/dENU is not significant for any PS, then mo2i$max_PS_with_signi_dNS_dENU will be NaN
          
          
          mo2i$ypred$NSpred_fENU_at_min_signi_PS  <-
            mo2i$a +   mo2i$b2_ps * mo2i$min_PS_with_signi_dNS_dENU  +
            ( mo2i$b3_int  * mo2i$min_PS_with_signi_dNS_dENU + mo2i$b1_enu ) * ll$enu
          
                 
          mo2i$ypred$NSpred_fENU_at_max_signi_PS  <-
            mo2i$a  +   mo2i$b2_ps * mo2i$max_PS_with_signi_dNS_dENU +
            (mo2i$b3_int  * mo2i$max_PS_with_signi_dNS_dENU + mo2i$b1_enu ) * ll$enu
          
  
          mo2i$ypred$NSpred_fENU_at_median_PS  <-
            mo2i$a +   mo2i$b2_ps * ll$ps_p50 +
            ( mo2i$b3_int  * ll$ps_p50 + mo2i$b1_enu ) * ll$enu
          
          
          
          
          ### ---------- BACK-TRAFOS to original space (os) ----------- ###
          
          if (  tf$ns_m_tf_use[ nn ] == "lin" ) {   # for linear = identical ( = no ) transformation of NS variable
            
            ## NS = f (ENU), m(PS)
            
            mo2i$ypred$enu_only_from_blr_retrafo_sortENU  <-
              ( ( ( mo1e$fitted.values * st$ns_m_sd[ nn ] ) + st$ns_m_means[ nn ] ) - tf$ns_m_offsets[[ nn ]] ) [ ll$ind_sortENU ]
  
            mo2i$ypred$NSpred_fENU_at_median_PS_retrafo_sortENU       <-
              ( ( ( mo2i$ypred$NSpred_fENU_at_median_PS * st$ns_m_sd[ nn ] ) + st$ns_m_means[ nn ] ) - tf$ns_m_offsets[[ nn ]] ) [ ll$ind_sortENU ]
            
            
            mo2i$ypred$NSpred_fENU_at_min_signi_PS_retrafo_sortENU  <-
              ( ( ( mo2i$ypred$NSpred_fENU_at_min_signi_PS * st$ns_m_sd[ nn ] )  + st$ns_m_means[ nn ] ) - tf$ns_m_offsets[[ nn ]] ) [ ll$ind_sortENU ]
            

            mo2i$ypred$NSpred_fENU_at_max_signi_PS_retrafo_sortENU  <-
              ( ( ( mo2i$ypred$NSpred_fENU_at_max_signi_PS * st$ns_m_sd[ nn ] )  + st$ns_m_means[ nn ] ) - tf$ns_m_offsets[[ nn ]] ) [ ll$ind_sortENU ]
   
            mo2i$ypred$NSpred_ENUobs_PSobs_retrafo_sortENU  <-
              ( ( ( mo2i$fitted.values * st$ns_m_sd[ nn ] )  + st$ns_m_means[ nn ] ) - tf$ns_m_offsets[[ nn ]] ) [ ll$ind_sortENU ]
            
            
          }
          
          
          if (  tf$ns_m_tf_use[ nn ] == "log" ) {   # for logarithmic transformation of NS variable
            
            
            ## NS = f (ENU), m(PS)
            
            mo2i$ypred$enu_only_from_blr_retrafo_sortENU  <-  ( exp(
              (  mo1e$fitted.values * st$ns_m_sd[ nn ] ) + st$ns_m_means[ nn ] )  -  tf$ns_m_offsets[[ nn ]] ) [ ll$ind_sortENU ]
            
            
            mo2i$ypred$NSpred_fENU_at_mean_PS_retrafo_sortENU       <-  ( exp( (
              mo2i$ypred$NSpred_fENU_at_mean_PS * st$ns_m_sd[ nn ] ) + st$ns_m_means[ nn ] )   -  tf$ns_m_offsets[[ nn ]] )  [ ll$ind_sortENU ]
            
            mo2i$ypred$NSpred_fENU_at_median_PS_retrafo_sortENU       <-  ( exp( (
              mo2i$ypred$NSpred_fENU_at_median_PS * st$ns_m_sd[ nn ] ) + st$ns_m_means[ nn ] )   -  tf$ns_m_offsets[[ nn ]] )  [ ll$ind_sortENU ]
            
            
            mo2i$ypred$NSpred_fENU_at_min_signi_PS_retrafo_sortENU  <-  ( exp( (
              mo2i$ypred$NSpred_fENU_at_min_signi_PS * st$ns_m_sd[ nn ] ) + st$ns_m_means[ nn ]  ) -  tf$ns_m_offsets[[ nn ]] ) [ ll$ind_sortENU ]
            

            mo2i$ypred$NSpred_fENU_at_max_signi_PS_retrafo_sortENU  <-  ( exp( (
              mo2i$ypred$NSpred_fENU_at_max_signi_PS * st$ns_m_sd[ nn ] ) + st$ns_m_means[ nn ]  ) -  tf$ns_m_offsets[[ nn ]] ) [ ll$ind_sortENU ]
            

            mo2i$ypred$NSpred_ENUobs_PSobs_retrafo_sortENU  <-  ( exp((
              mo2i$fitted.values * st$ns_m_sd[ nn ] ) + st$ns_m_means[ nn ] ) -  tf$ns_m_offsets[[ nn ]] ) [ ll$ind_sortENU ]
            
            
          }
          
          
          
          
          
          if (  tf$ns_m_tf_use[ nn ] == "sat" ) {   # for saturation transformation of NS variable
            
            
            ## NS = f (ENU), m(PS)
            
            mo2i$ypred$enu_only_from_blr_retrafo_sortENU <-
              ( tf$ns_m_sat_val[[ nn ]]  -  tf$ns_m_offsets[[ nn ]]  +  g$ns_m_sign_for_sat_retrafo[ nn ]  *
                  exp((( mo1e$fitted.values * st$ns_m_sd[ nn ] ) + st$ns_m_means[ nn ] ) ) ) [ ll$ind_sortENU ]
            
            
            mo2i$ypred$NSpred_fENU_at_mean_PS_retrafo_sortENU   <-
              ( tf$ns_m_sat_val[[ nn ]]  -  tf$ns_m_offsets[[ nn ]]   +  g$ns_m_sign_for_sat_retrafo[ nn ]  *
                  exp(((  mo2i$ypred$NSpred_fENU_at_mean_PS * st$ns_m_sd[ nn ] ) + st$ns_m_means[ nn ] )) ) [ ll$ind_sortENU ]
            
            mo2i$ypred$NSpred_fENU_at_median_PS_retrafo_sortENU   <-
              ( tf$ns_m_sat_val[[ nn ]]  -  tf$ns_m_offsets[[ nn ]]   +  g$ns_m_sign_for_sat_retrafo[ nn ]  *
                  exp(((  mo2i$ypred$NSpred_fENU_at_median_PS * st$ns_m_sd[ nn ] ) + st$ns_m_means[ nn ] )) ) [ ll$ind_sortENU ]
            
            
            
            mo2i$ypred$NSpred_fENU_at_min_signi_PS_retrafo_sortENU  <-
              ( tf$ns_m_sat_val[[ nn ]]  -  tf$ns_m_offsets[[ nn ]]   +  g$ns_m_sign_for_sat_retrafo[ nn ] *
                  exp((( mo2i$ypred$NSpred_fENU_at_min_signi_PS * st$ns_m_sd[ nn ] ) + st$ns_m_means[ nn ] ) ) ) [ ll$ind_sortENU ]

            
            mo2i$ypred$NSpred_fENU_at_max_signi_PS_retrafo_sortENU  <-
              ( tf$ns_m_sat_val[[ nn ]]  -  tf$ns_m_offsets[[ nn ]]    +  g$ns_m_sign_for_sat_retrafo[ nn ] *
                  exp((( mo2i$ypred$NSpred_fENU_at_max_signi_PS * st$ns_m_sd[ nn ] ) + st$ns_m_means[ nn ] ) ) ) [ ll$ind_sortENU ]
            

            mo2i$ypred$NSpred_ENUobs_PSobs_retrafo_sortENU  <-
              ( tf$ns_m_sat_val[[ nn ]]  -  tf$ns_m_offsets[[ nn ]]  +   g$ns_m_sign_for_sat_retrafo[ nn ] *
                  exp((( mo2i$fitted.values * st$ns_m_sd[ nn ] ) + st$ns_m_means[ nn ] ) ) ) [ ll$ind_sortENU ]
            
            
          }
          
          
          ### prediction and back-transform finished ####
          
    
          ##### calculate ENU levels (regions) where the effect of PS is non-significant
           
          a$enu_region_nonsigf_patch_1_ind <- vector(  length = 2 )
          a$enu_region_nonsigf_patch_2_ind <- vector(  length = 2 )
          
          
          if ( ! any ( mo2i$marg_eff_ps_flag_signi_sortENU ) ) {   # case where effect of PS is not significant at all
            a$enu_region_nonsigf_patch_1_ind[1]   <-  1
            a$enu_region_nonsigf_patch_1_ind[2]   <-  nc$hs
          } else {
            
            if ( ! mo2i$marg_eff_ps_flag_signi_sortENU[1] ) {  # case where the effect of PS at the lowest ENU level is non-significant (i.e. patch 1 starts from 1st element of ENU)
              
              a$enu_region_nonsigf_patch_1_ind[1]   <-   1 
              a$enu_region_nonsigf_patch_1_ind[2]   <-   min ( which( mo2i$marg_eff_ps_flag_signi_sortENU ) ) - 1
              
              # in this case, a second non-significant patch is possible
              if ( any ( ! mo2i$marg_eff_ps_flag_signi_sortENU[ ( a$enu_region_nonsigf_patch_1_ind[2] + 1 ) : nc$hs ] ) ) { 
                a$enu_region_nonsigf_patch_2_ind[1]  <-  min ( which( ! mo2i$marg_eff_ps_flag_signi_sortENU[ 
                  ( a$enu_region_nonsigf_patch_1_ind[2] + 1 ) : nc$hs ] ) ) + a$enu_region_nonsigf_patch_1_ind[2]
                
                a$enu_region_nonsigf_patch_2_ind[2]  <-  max ( which( ! mo2i$marg_eff_ps_flag_signi_sortENU[ 
                  ( a$enu_region_nonsigf_patch_1_ind[2] + 1 ) : nc$hs ] ) ) + a$enu_region_nonsigf_patch_1_ind[2]  # this should equal nc$hs (= 104)
                
              }
            }  else { # if the effect of PS at the lowest ENU level is significant, there can be only 1 non-significant patch, and the case is easy
              
              a$enu_region_nonsigf_patch_1_ind[1]   <-   min ( which( ! mo2i$marg_eff_ps_flag_signi_sortENU ) ) 
              a$enu_region_nonsigf_patch_1_ind[2]   <-   max ( which( ! mo2i$marg_eff_ps_flag_signi_sortENU ) ) 
              
            }
            
          }
          
  
        
   
        ########### PLOTS ##########################################################
        ########### PLOTS ##########################################################
        ########### PLOTS ##########################################################
        
        ### ----- PLOT PREDICTIONS for NS = f(ENU), m(PS), pred @ PS mean,min signif, max signif, incl pred @ PS obs, back-trafo (btf), plot os ( = x lin, y lin) ----- ###
        #

          if( pp == 4){ a$xlabels = paste( pn$enu_m[ ee ] , pn$enu_units_m[ ee ] , sep="") } else { a$xlabels = ""}


          plot(  ll$enu_os , ll$ns_os ,  
                 ylim = c ( a$ymins[ni] , a$ymaxs[ni]), 
                 log = "x" ,
                 type = "n",
                 xlab = NA , ylab = NA , axes =  F ,
                 cex.main = 1.2, cex.lab = 1, cex.axis = 1.1 )
          

            box()
            axis(side = 1, tck =-0.02 , labels = NA , cex.axis  = 0.8 )
            axis(side = 2, tck = -0.025 , labels = NA , cex.axis  = 0.8 )
            axis(side = 1, lwd = 0, line = -.4 , cex.axis  = 0.75)
            axis(side = 2, lwd = 0, line = -.1, las = 1 , cex.axis  = 0.75 )
            mtext(side = 1, a$xlabels , line = 1, cex = 0.6  , font.lab = 2)
            mtext(side = 2, paste( pn$ns_m[ nn ], pn$ns_units_m[ nn ] , sep="" ) , line = 1.2 , cex = 0.575 , font.lab = 2)
            mtext(side = 3, pn$ps_m[ pp ] , line = 0 , cex = 0.575 , font.main = 2 , font = 2)
            

          lim <- par("usr")

          
          ### plot grey patches (effect of PS non-significant)
          
          
          rect( 4.3 , lim[3]+0.25 ,
                386 , lim[4]-0.25 , 
                border = rgb(0.97,0.97,0.97,0.97) , col = rgb(0.96,0.96,0.96,0.96) )
          
          
          if ( ! any ( mo2i$marg_eff_ps_flag_signi_sortENU ) ){
            
            rect( 4.35 , lim[3]+0.5 ,
                  380 , lim[4]-0.5 , 
                  border = "grey", col = rgb(0.9,0.9,0.9,0.85) )
          } else {
            
            if ( a$enu_region_nonsigf_patch_1_ind[1] == 1  ) {   # first non-significant patch starting at first element
              
              rect( 4.35 , lim[3]+0.5 ,
                    ll$enu_os_sortENU[ a$enu_region_nonsigf_patch_1_ind[2] ] , lim[4]-0.5 , 
                    border = "grey", col = rgb(0.9,0.9,0.9,0.85) )
              
            } else { # first non-significant patch not starting at first element
              
              if ( a$enu_region_nonsigf_patch_1_ind[2] == nc$hs  ) {
                
                rect( ll$enu_os_sortENU[ a$enu_region_nonsigf_patch_1_ind[1] ] ,  lim[3]+0.5 ,
                      380 ,  lim[4]-0.5 , 
                      border = "grey", col = rgb(0.9,0.9,0.9,0.85) )
                
              } else {
                
                rect( ll$enu_os_sortENU[ a$enu_region_nonsigf_patch_1_ind[1] ] ,  lim[3]+0.5 ,
                    ll$enu_os_sortENU[ a$enu_region_nonsigf_patch_1_ind[2] ] ,  lim[4]-0.5 , 
                    border = "grey", col = rgb(0.9,0.9,0.9,0.85) )
              }
              
            }
            
            if ( any ( a$enu_region_nonsigf_patch_2_ind > 0 )  ) {
  
              rect( ll$enu_os_sortENU[ a$enu_region_nonsigf_patch_2_ind[1] ] ,  lim[3]+0.5,
                    380 ,  lim[4]-0.5 , 
                    border = "grey", col = rgb(0.9,0.9,0.9,0.85) )
              
            }
          
          }
          

          
            # observed provisioning 
          if ( pp != 2) {
            points( ll$enu_os_sortENU   , mo2i$ypred$NSpred_ENUobs_PSobs_retrafo_sortENU , pch = 3 , 
            col = cols$purple , cex = 0.575, lwd = 0.33 )  
          }   
          
          if ( pp == 2 ) {
            points( 
              ll$enu_os_sortENU [mo2i$ypred$NSpred_ENUobs_PSobs_retrafo_sortENU != mo2i$ypred$NSpred_fENU_at_max_signi_PS_retrafo_sortENU ] , 
              mo2i$ypred$NSpred_ENUobs_PSobs_retrafo_sortENU[ mo2i$ypred$NSpred_ENUobs_PSobs_retrafo_sortENU != mo2i$ypred$NSpred_fENU_at_max_signi_PS_retrafo_sortENU ] , 
               pch = 3 ,  col = cols$purple , cex = 0.575, lwd = 0.33 )  
          }   
          
            
          lines( ll$enu_os_sortENU   , mo2i$ypred$NSpred_fENU_at_min_signi_PS_retrafo_sortENU , col = cols$blue , lty = 2 , lwd = 2)
          

            if ( pp != 2 ){
              
            lines( ll$enu_os_sortENU   , mo2i$ypred$NSpred_fENU_at_max_signi_PS_retrafo_sortENU , col = cols$yellow ,lty = 2 , lwd = 2.8)

            lines( ll$enu_os_sortENU   , mo2i$ypred$NSpred_fENU_at_median_PS_retrafo_sortENU , col = cols$orange ,lty = 2 , lwd = 2.8 )

            }
            
           if( pp == 2 ){
             
            lines( ll$enu_os_sortENU   , mo2i$ypred$NSpred_fENU_at_max_signi_PS_retrafo_sortENU , col = cols$yellow ,lty = 2 , lwd = 3.1 )

           points( 
             ll$enu_os_sortENU [mo2i$ypred$NSpred_ENUobs_PSobs_retrafo_sortENU == mo2i$ypred$NSpred_fENU_at_max_signi_PS_retrafo_sortENU ] , 
             mo2i$ypred$NSpred_ENUobs_PSobs_retrafo_sortENU[ mo2i$ypred$NSpred_ENUobs_PSobs_retrafo_sortENU == mo2i$ypred$NSpred_fENU_at_max_signi_PS_retrafo_sortENU ] , 
             pch = 3 , col = cols$purple , cex = 0.35, lwd = 0.175 )    
            } 
            
          lines ( ll$enu_os_sortENU  ,  mo2i$ypred$enu_only_from_blr_retrafo_sortENU , col="black"  , lty =  1 , lwd = 0.4 )     
        
        
      }
    }
  }
  


  if (  sw$plot_mod_multisp ) { dev.off() }  # deactivates png engine if figures are to be exported

} ### end figure prediction NS = f(ENU) , m(PS) , RETRAFO




###################### END MEGA-LOOP ###############################
###################### END MEGA-LOOP ###############################
###################### END MEGA-LOOP ###############################





