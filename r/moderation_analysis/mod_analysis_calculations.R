

r_mod  <-  list( )  # store results from mod analysis
r_bvr_ks  <-  list( )  # store results from mod analysis

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

r_mod$dNS_dPS_norm_imprNS_sortENU                 <-  array ( data = NA, dim = c ( nv$ps_u , nv$ns_m ,  nc$hs ) )
r_mod$dNS_dPS_is_signif_sortENU                    <-  array ( data = NA, dim = c ( nv$ps_u , nv$ns_m ,  nc$hs ) )

r_mod$NSpred_fs_PSmaxsg_minus_PSminsg_sortENU      <-  array ( data = NA, dim = c ( nv$ps_u , nv$ns_m ,  nc$hs ) )
r_mod$flag_ps_signif_sortENU                        <-  array ( data = NA, dim = c ( nv$ps_u , nv$ns_m ,  nc$hs ) )


r_mod$NSpred_fs_PSmax_minus_PSmin_sortENU     <-    array ( data = NA, dim = c ( nv$ps_u , nv$ns_m ,  nc$hs ) )
r_mod$NSpred_fs_PSp90_minus_PSp10_sortENU   <-   array ( data = NA, dim = c ( nv$ps_u , nv$ns_m ,  nc$hs ) )

r_mod$flag_ENU_where_eff_ps_signif_sortENU    <-   array ( data = NA, dim = c ( nv$ps_u , nv$ns_m ,  nc$hs ) )

r_mod$NSpred_os_PSmaxsg_minus_PSminsg_percrange_sortENU   <-   array ( data = NA, dim = c ( nv$ps_u , nv$ns_m , nc$hs ) )

r_mod$NSpred_os_PSmax_minus_PSmin_percrange_sortENU   <-   array ( data = NA, dim = c ( nv$ps_u , nv$ns_m ,  nc$hs ) )
r_mod$NSpred_os_PSp90_minus_PSp10_percrange_sortENU   <-   array ( data = NA, dim = c ( nv$ps_u , nv$ns_m ,  nc$hs ) )
r_mod$NSpred_os_PSp95_minus_PSp5_percrange_sortENU   <-   array ( data = NA, dim = c ( nv$ps_u , nv$ns_m ,  nc$hs ) )


r_mod$NSpred_os_PSmax_minus_PSmin_percrange_cutoff_sortENU   <-   array ( data = NA, dim = c ( nv$ps_u , nv$ns_m ,  nc$hs ) )

r_mod$NSpred_os_PSp90_minus_PSp10_percrange_cutoff_sortENU   <-   array ( data = NA, dim = c ( nv$ps_u , nv$ns_m ,  nc$hs ) )
r_mod$NSpred_os_PSp95_minus_PSp5_percrange_cutoff_sortENU   <-   array ( data = NA, dim = c ( nv$ps_u , nv$ns_m ,  nc$hs ) )



r_mod_test <- list()

r_mod_test$sigma_dNS_dENU_sqrt_arg  <- array ( data = 999, dim = c ( nc$hs, nv$ns_m , nv$ps_m ) )

r_mod_test$sigma_dNS_dPS_sqrt_arg <- array ( data = 999, dim = c ( nc$hs, nv$ns_m , nv$ps_m ) )

r_mod_test$sigma_dNS_dENU   <- array ( data = 999, dim = c ( nc$hs, nv$ns_m , nv$ps_m ) )

r_mod_test$sigma_dNS_dPS   <- array ( data = 999, dim = c ( nc$hs, nv$ns_m , nv$ps_m ) )

r_mod_test$sdNS_dENU_ind_first_last_signif  <- array ( data = 999, dim = c ( 2, nv$ns_m , nv$ps_m ) )
r_mod_test$sdNS_dPS_ind_first_last_signif   <- array ( data = 999, dim = c ( 2, nv$ns_m , nv$ps_m ) )


r_bvr_ks <- list()

r_mod$kstest <- array ( data = NA, dim = c ( nv$ps_u , nv$ns_m   ) )
r_mod$kstest_below_th <- array ( data = NA, dim = c ( nv$ps_u , nv$ns_m  ) )
 
r_bvr_ks$kstest  <- array ( data = NA, dim = c ( nv$ns_m  ) )
r_bvr_ks$kstest_below_th  <- array ( data = NA, dim = c ( nv$ns_m  ) )


g$sign_normaliser_imprNS_dNS_dPS_u  <- array(  data = 1 , dim = c ( nv$ns_m , nv$ps_u ) )

for ( nn in 1:nv$ns_m) {
  for ( pp in 1:nv$ps_u ){
  
    if ( ! tf$ps_u_tf_use[ pp ] == "sat"    ) { g$sign_normaliser_imprNS_dNS_dPS_u [ nn , pp ]  <-   g$sign_normaliser_imprNS_for_effects_on_ns_m[ nn ] }
    if ( tf$ps_u_tf_use[ pp ] == "sat"    )  { g$sign_normaliser_imprNS_dNS_dPS_u [ nn , pp ]   <-   ( - 1 )* g$sign_normaliser_imprNS_for_effects_on_ns_m[ nn ] }
    
  
  }
}





###################### START MEGA-LOOP ###############################
###################### START MEGA-LOOP ###############################
###################### START MEGA-LOOP ###############################




ll$nrow  <-  0



for ( nn in 1 : nv$ns_m ) {

  ll$ns  <-  ns_m[ , nn ]
  ll$ns_os   <-  ns_os4m[ , nn ]
  
  ll$ns_os_range  <-  abs(  a$ns_ceiling[nn] - a$ns_floor[nn] )
  
  # for ( ee in 1 : nv$enu_m ){
    for ( ee in which( fn$enu_all == "tfec"  ) ){

    ll$enu  <-  enu_m[ , ee ]
    ll$enu_os   <-   enu_os4m[ , ee ]

    ll$enu_sortENU      <-  sort ( ll$enu )
    ll$ind_sortENU      <-  sort ( ll$enu  , index.return = TRUE )$ix

    ll$enu_os_sortENU   <-   ll$enu_os[ ll$ind_sortENU ]


    for ( pp in 1 : nv$ps_u ) {

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
      
      ll$ps_p100  <-  ll$ps_sortPS[ nc$hs ]
      ll$ps_p1    <-  ll$ps_sortPS[ 1 ]
      
      ll$ps_p95  <-  ll$ps_sortPS[ 1 + round ( 0.95 * (nc$hs -1 ) )  ]
      ll$ps_p90  <-  ll$ps_sortPS[ 1 + round ( 0.9 * (nc$hs -1 ) )  ]
      ll$ps_p80  <-  ll$ps_sortPS[ 1 + round ( 0.8 * (nc$hs -1 ) )  ]
      ll$ps_p75  <-  ll$ps_sortPS[ 1 + round ( 0.75 * (nc$hs -1 ) )  ]
      ll$ps_p70  <-  ll$ps_sortPS[ 1 + round ( 0.7 * (nc$hs -1 ) )  ]
      ll$ps_p30  <-  ll$ps_sortPS[ 1 + round ( 0.3 * (nc$hs -1 ) )  ]
      ll$ps_p25  <-  ll$ps_sortPS[ 1 + round ( 0.25 * (nc$hs -1 ) )  ]
      ll$ps_p20  <-  ll$ps_sortPS[ 1 + round ( 0.2 * (nc$hs -1 ) )  ]
      ll$ps_p10  <-  ll$ps_sortPS[ 1 + round ( 0.1 * (nc$hs -1 ) )  ]
      ll$ps_p5   <-  ll$ps_sortPS[ 1 + round ( 0.05 * (nc$hs -1 ) )  ]
      
      ll$ps_os_p95  <-  ll$ps_os_sortPS[ 1 + round ( 0.95 * (nc$hs -1 ) ) ]
      ll$ps_os_p90  <-  ll$ps_os_sortPS[ 1 + round ( 0.9 * (nc$hs -1 ) ) ]
      ll$ps_os_p80  <-  ll$ps_os_sortPS[ 1 + round ( 0.8 * (nc$hs -1 ) )  ]
      ll$ps_os_p75  <-  ll$ps_os_sortPS[ 1 + round ( 0.75 * (nc$hs -1 ) )  ]
      ll$ps_os_p70  <-  ll$ps_os_sortPS[ 1 + round ( 0.7 * (nc$hs -1 ) )  ]
      ll$ps_os_p30  <-  ll$ps_os_sortPS[ 1 + round ( 0.3 * (nc$hs -1 ) )  ]
      ll$ps_os_p25  <-  ll$ps_os_sortPS[ 1 + round ( 0.25 * (nc$hs -1 ) )  ]
      ll$ps_os_p20  <-  ll$ps_os_sortPS[ 1 + round ( 0.2 * (nc$hs -1 ) )  ]
      ll$ps_os_p10  <-  ll$ps_os_sortPS[ 1 + round ( 0.1 * (nc$hs -1 ) )  ]
      ll$ps_os_p5   <-  ll$ps_os_sortPS[ 1 + round ( 0.05 * (nc$hs -1 ) )  ]
      

      
      if ( ! tf$ps_u_tf_use[ pp ] == "sat"  ) {   
        
        ll$ps_fs_p75os   <-   ll$ps_p75               # these are the PS values in fitspace (fs) = transformed, at the percentiles of the os variables (i.e. sign-sensitive)
        ll$ps_fs_p80os   <-   ll$ps_p80               # these are the PS values in fitspace (fs) = transformed, at the percentiles of the os variables (i.e. sign-sensitive)
        ll$ps_fs_p90os   <-   ll$ps_p90 
        ll$ps_fs_p95os   <-   ll$ps_p95 
        ll$ps_fs_p100os  <-   ll$ps_p100 
        
        ll$ps_fs_p25os  <-   ll$ps_p25 
        ll$ps_fs_p20os  <-   ll$ps_p20 
        ll$ps_fs_p10os  <-   ll$ps_p10 
        ll$ps_fs_p5os   <-   ll$ps_p5 
        ll$ps_fs_p1os   <-   ll$ps_p1   }

      
      if ( tf$ps_u_tf_use[ pp ] == "sat"  ) {   
        
        ll$ps_fs_p75os   <-   ll$ps_p25               # these are the PS values in fitspace (fs) = transformed, at the percentiles of the os variables (i.e. sign-sensitive)
        ll$ps_fs_p80os   <-   ll$ps_p20               # these are the PS values in fitspace (fs) = transformed, at the percentiles of the os variables (i.e. sign-sensitive)
        ll$ps_fs_p90os   <-   ll$ps_p10 
        ll$ps_fs_p95os   <-   ll$ps_p5 
        ll$ps_fs_p100os  <-   ll$ps_p1 
        
        ll$ps_fs_p25os   <-   ll$ps_p75               # these are the PS values in fitspace (fs) = transformed, at the percentiles of the os variables (i.e. sign-sensitive)
        ll$ps_fs_p20os   <-   ll$ps_p80
        ll$ps_fs_p10os   <-   ll$ps_p90  
        ll$ps_fs_p5os    <-   ll$ps_p95   
        ll$ps_fs_p1os    <-   ll$ps_p100   }
      
      
      
      

      ### ---- bivariate models --- ###

      mo1e  <-   lm( ll$ns  ~  ll$enu )   # mo1e = model object, 1 predictor, energy as predictor
      mo1p  <-   lm( ll$ns  ~  ll$ps  )   # mo1p = model object, 1 predictor, , PS as predictor

      mo1e_rob  <- lm_robust( ll$ns  ~  ll$enu , se_type = sw$HC)
      mo1p_rob  <- lm_robust( ll$ns  ~  ll$ps  , se_type = sw$HC)


      mo1e$b1_enu_gate    <-  (summary(mo1e_rob))$coefficients[2,4]   <   ch$th_p_b_bivar
      mo1p$b1_ps_gate     <-  (summary(mo1p_rob))$coefficients[2,4]   <   ch$th_p_b_bivar

      # ks test
      r_bvr_ks$kstest[ nn ] <-  ks.test ( sort( mo1e$residuals ) , "pnorm",  0 , sd (  mo1e$residuals  ) )$p.value
    


      ####################################################################
      ####################################################################

      ## multiplicative 2-predictor OLS regression (with interaction)
      mo2i  <-   lm( ll$ns  ~  ll$enu * ll$ps )
      mo2i_rob  <-   lm_robust( ll$ns  ~  ll$enu * ll$ps , se_type = sw$HC )
      
      # ks test
      r_mod$kstest[ pp , nn ] <-  ks.test ( sort( mo2i$residuals ) , "pnorm",  0 , sd (  mo2i$residuals  ) )$p.value

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

      
      
      r_mod_test$sigma_dNS_dENU_sqrt_arg[ , nn , pp ]  <- mo2i_rob$var_b1_enu  +  ( ll$ps_sortPS ^2 ) * mo2i_rob$var_b3_int  +
                                              2 * ll$ps_sortPS * mo2i_rob$cov_b1ENU_b3INT
      
      r_mod_test$sigma_dNS_dPS_sqrt_arg[ , nn , pp ]  <- mo2i_rob$var_b2_ps + ( ll$enu_sortENU ^2 ) * mo2i_rob$var_b3_int  +
                                              2 * ll$enu_sortENU * mo2i_rob$cov_b2PS_b3INT
      
      r_mod_test$sigma_dNS_dENU[ , nn , pp ]   <- mo2i$std_err_marg_eff_enu_sortPS
      
      r_mod_test$sigma_dNS_dPS[ , nn , pp ]   <- mo2i$std_err_marg_eff_ps_sortENU 
      
        
      
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
      
      r_mod_test$sdNS_dENU_ind_first_last_signif[ , nn , pp ]    <- mo2i$marg_eff_enu_sortPS_ind_first_last_signif
      r_mod_test$sdNS_dPS_ind_first_last_signif[ , nn , pp ]     <- mo2i$marg_eff_ps_sortENU_ind_first_last_signif
      
      mo2i$marg_eff_enu_sortPS_percentiles_first_last_signif   <-   round( mo2i$marg_eff_enu_sortPS_ind_first_last_signif * 100 / nc$hs )
      mo2i$marg_eff_ps_sortENU_percentiles_first_last_signif   <-   round( mo2i$marg_eff_ps_sortENU_ind_first_last_signif * 100 / nc$hs )

      
      ### export into results variable
      
      
      r_mod$dNS_dPS_norm_imprNS_sortENU          [ pp , nn ,   ]            <-  mo2i$marg_eff_ps_sortENU * g$sign_normaliser_imprNS_dNS_dPS_u[ nn , pp]
      r_mod$dNS_dPS_is_signif_sortENU[ pp , nn ,   ]                        <-  mo2i$marg_eff_ps_flag_signi_sortENU 
 

      
      ### ----- prepare predictions at representative values ------ ####


      ll$ps_mean   <-  mean (  ll$ps ) # this should be zero, but oh well
      ll$ps_median   <-  median (  ll$ps ) 
      ll$enu_mean  <-  mean ( ll$enu ) # this should be zero, but oh well

      mo2i$ind_of_mean_in_ps_sortPS     <-  which( abs( ll$ps_sortPS - ll$ps_mean ) == min( abs( ll$ps_sortPS - ll$ps_mean ) ) )[1]  # index of the mean of PS ; [1] is just in case of ties, as is the case with infra
      mo2i$perc_of_mean_in_ps_sortPS    <-  round ( mo2i$ind_of_mean_in_ps_sortPS * 100 / nc$hs )

      mo2i$ind_of_mean_in_enu_sortENU     <-  which( abs( ll$enu_sortENU - ll$enu_mean ) == min( abs( ll$enu_sortENU - ll$enu_mean ) ) )[1]  # index of the mean of PS
      mo2i$perc_of_mean_in_enu_sortENU    <-  round ( mo2i$ind_of_mean_in_enu_sortENU * 100 / nc$hs )

      # for median, don't need to determine index: it's the median index (median ( nc$hs ) )
      
      
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

      
      mo2i$ypred$NSpred_fENU_at_PSfs_p1os  <-
        mo2i$a +   mo2i$b2_ps * ll$ps_fs_p1os  +
        ( mo2i$b3_int  * ll$ps_fs_p1os + mo2i$b1_enu ) * ll$enu
      
      mo2i$ypred$NSpred_fENU_at_PSfs_p5os  <-
        mo2i$a +   mo2i$b2_ps * ll$ps_fs_p5os  +
        ( mo2i$b3_int  * ll$ps_fs_p5os + mo2i$b1_enu ) * ll$enu

      mo2i$ypred$NSpred_fENU_at_PSfs_p10os  <-
        mo2i$a +   mo2i$b2_ps * ll$ps_fs_p10os  +
        ( mo2i$b3_int  * ll$ps_fs_p10os + mo2i$b1_enu ) * ll$enu
      
      mo2i$ypred$NSpred_fENU_at_PSfs_p20os  <-
        mo2i$a +   mo2i$b2_ps * ll$ps_fs_p20os  +
        ( mo2i$b3_int  * ll$ps_fs_p20os + mo2i$b1_enu ) * ll$enu
      
      mo2i$ypred$NSpred_fENU_at_PSfs_p25os  <-
        mo2i$a +   mo2i$b2_ps * ll$ps_fs_p25os  +
        ( mo2i$b3_int  * ll$ps_fs_p25os + mo2i$b1_enu ) * ll$enu
      
      
      mo2i$ypred$NSpred_fENU_at_max_signi_PS  <-
        mo2i$a  +   mo2i$b2_ps * mo2i$max_PS_with_signi_dNS_dENU +
        (mo2i$b3_int  * mo2i$max_PS_with_signi_dNS_dENU + mo2i$b1_enu ) * ll$enu
      
      mo2i$ypred$NSpred_fENU_at_PSfs_p100os  <-
        mo2i$a +   mo2i$b2_ps * ll$ps_fs_p100os  +
        ( mo2i$b3_int  * ll$ps_fs_p100os + mo2i$b1_enu ) * ll$enu

      mo2i$ypred$NSpred_fENU_at_PSfs_p95os  <-
        mo2i$a +   mo2i$b2_ps * ll$ps_fs_p95os  +
        ( mo2i$b3_int  * ll$ps_fs_p95os + mo2i$b1_enu ) * ll$enu
            
      mo2i$ypred$NSpred_fENU_at_PSfs_p90os  <-
        mo2i$a +   mo2i$b2_ps * ll$ps_fs_p90os  +
        ( mo2i$b3_int  * ll$ps_fs_p90os + mo2i$b1_enu ) * ll$enu
      
      mo2i$ypred$NSpred_fENU_at_PSfs_p80os  <-
        mo2i$a +   mo2i$b2_ps * ll$ps_fs_p80os  +
        ( mo2i$b3_int  * ll$ps_fs_p80os + mo2i$b1_enu ) * ll$enu

      mo2i$ypred$NSpred_fENU_at_PSfs_p75os  <-
        mo2i$a +   mo2i$b2_ps * ll$ps_fs_p75os  +
        ( mo2i$b3_int  * ll$ps_fs_p75os + mo2i$b1_enu ) * ll$enu
      
      
      mo2i$ypred$NSpred_fENU_at_mean_PS  <-
        mo2i$a +   mo2i$b2_ps * ll$ps_mean +
        ( mo2i$b3_int  * ll$ps_mean + mo2i$b1_enu ) * ll$enu
      # if (! mo2i$is_margeff_enu_at_ps_mean_signi ) {  mo2i$ypred$NSpred_fENU_at_mean_PS  <- NULL  }
      # mo2i$is_margeff_enu_at_ps_mean_signi establishes a gate for this one
      
      mo2i$ypred$NSpred_fENU_at_median_PS  <-
        mo2i$a +   mo2i$b2_ps * ll$ps_p50 +
        ( mo2i$b3_int  * ll$ps_p50 + mo2i$b1_enu ) * ll$enu
      
      

      
      
      ##### ----------- export results ------------------- #####

      
      r_mod$NSpred_fs_PSmax_minus_PSmin_sortENU[ pp , nn ,  ]  <-  ( mo2i$ypred$NSpred_fENU_at_PSfs_p100os [ ll$ind_sortENU ] - 
                                                                            mo2i$ypred$NSpred_fENU_at_PSfs_p1os[ ll$ind_sortENU ] ) * 
                                                                              g$sign_normaliser_imprNS_for_NSpred_fs_PSmaxsg_minus_PSminsg_m [ nn ]
      
       
      r_mod$NSpred_fs_PSp90_minus_PSp10_sortENU[ pp , nn ,  ]  <-  ( mo2i$ypred$NSpred_fENU_at_PSfs_p90os [ ll$ind_sortENU ] - 
                                                                           mo2i$ypred$NSpred_fENU_at_PSfs_p10os[ ll$ind_sortENU ] ) * 
                                                                            g$sign_normaliser_imprNS_for_NSpred_fs_PSmaxsg_minus_PSminsg_m [ nn ]
      
      
      r_mod$flag_ENU_where_eff_ps_signif_sortENU[ pp , nn ,   ]    <-   mo2i$marg_eff_ps_flag_signi_sortENU
      
      
      
      r_mod$NSpred_fs_PSmaxsg_minus_PSminsg_sortENU[ pp , nn ,  ]  <-  ( mo2i$ypred$NSpred_fENU_at_max_signi_PS[ ll$ind_sortENU ] - 
                                                                            mo2i$ypred$NSpred_fENU_at_min_signi_PS[ ll$ind_sortENU ] ) * 
                                                                        g$sign_normaliser_imprNS_for_NSpred_fs_PSmaxsg_minus_PSminsg_m [ nn ]
      
      
      r_mod$flag_ps_signif_sortENU[ pp , nn ,   ]  <-  rep(  ( ( mo2i$marg_eff_enu_sortPS_ind_first_last_signif[1] < 300 )
                                                              & ( mo2i$marg_eff_enu_sortPS_ind_first_last_signif[2] > 0 ) ) &
                                                            (   mo2i$ps_signi_b2b3joint  <  ch$th_p_mod_PS_b2b3joint  ) , nc$hs )




      ### ---------- BACK-TRAFOS to original space (os) ----------- ###


      if (  tf$ns_m_tf_use[ nn ] == "lin" ) {   # for linear = identical ( = no ) transformation of NS variable

        ## NS = f (ENU), m(PS)

        mo2i$ypred$enu_only_from_blr_retrafo_sortENU  <-
          ( ( ( mo1e$fitted.values * st$ns_m_sd[ nn ] ) + st$ns_m_means[ nn ] ) - tf$ns_m_offsets[[ nn ]] ) [ ll$ind_sortENU ]

        
        mo2i$ypred$NSpred_fENU_at_mean_PS_retrafo_sortENU       <-
          ( ( ( mo2i$ypred$NSpred_fENU_at_mean_PS * st$ns_m_sd[ nn ] ) + st$ns_m_means[ nn ] ) - tf$ns_m_offsets[[ nn ]] ) [ ll$ind_sortENU ]
        
        mo2i$ypred$NSpred_fENU_at_median_PS_retrafo_sortENU       <-
          ( ( ( mo2i$ypred$NSpred_fENU_at_median_PS * st$ns_m_sd[ nn ] ) + st$ns_m_means[ nn ] ) - tf$ns_m_offsets[[ nn ]] ) [ ll$ind_sortENU ]
        
        
        mo2i$ypred$NSpred_fENU_at_min_signi_PS_retrafo_sortENU  <-
          ( ( ( mo2i$ypred$NSpred_fENU_at_min_signi_PS * st$ns_m_sd[ nn ] )  + st$ns_m_means[ nn ] ) - tf$ns_m_offsets[[ nn ]] ) [ ll$ind_sortENU ]

        mo2i$ypred$NSpred_fENU_at_PSfs_p5os_retrafo_sortENU  <-
          ( ( ( mo2i$ypred$NSpred_fENU_at_PSfs_p5os * st$ns_m_sd[ nn ] )  + st$ns_m_means[ nn ] ) - tf$ns_m_offsets[[ nn ]] ) [ ll$ind_sortENU ]
                
        mo2i$ypred$NSpred_fENU_at_PSfs_p1os_retrafo_sortENU  <-
          ( ( ( mo2i$ypred$NSpred_fENU_at_PSfs_p1os * st$ns_m_sd[ nn ] )  + st$ns_m_means[ nn ] ) - tf$ns_m_offsets[[ nn ]] ) [ ll$ind_sortENU ]
        
        mo2i$ypred$NSpred_fENU_at_PSfs_p10os_retrafo_sortENU  <-
          ( ( ( mo2i$ypred$NSpred_fENU_at_PSfs_p10os * st$ns_m_sd[ nn ] )  + st$ns_m_means[ nn ] ) - tf$ns_m_offsets[[ nn ]] ) [ ll$ind_sortENU ]
        
        mo2i$ypred$NSpred_fENU_at_PSfs_p20os_retrafo_sortENU  <-
          ( ( ( mo2i$ypred$NSpred_fENU_at_PSfs_p20os * st$ns_m_sd[ nn ] )  + st$ns_m_means[ nn ] ) - tf$ns_m_offsets[[ nn ]] ) [ ll$ind_sortENU ]
        
        mo2i$ypred$NSpred_fENU_at_PSfs_p25os_retrafo_sortENU  <-
          ( ( ( mo2i$ypred$NSpred_fENU_at_PSfs_p25os * st$ns_m_sd[ nn ] )  + st$ns_m_means[ nn ] ) - tf$ns_m_offsets[[ nn ]] ) [ ll$ind_sortENU ]
        
        
        mo2i$ypred$NSpred_fENU_at_max_signi_PS_retrafo_sortENU  <-
          ( ( ( mo2i$ypred$NSpred_fENU_at_max_signi_PS * st$ns_m_sd[ nn ] )  + st$ns_m_means[ nn ] ) - tf$ns_m_offsets[[ nn ]] ) [ ll$ind_sortENU ]

        mo2i$ypred$NSpred_fENU_at_PSfs_p100os_retrafo_sortENU  <-
          ( ( ( mo2i$ypred$NSpred_fENU_at_PSfs_p100os * st$ns_m_sd[ nn ] )  + st$ns_m_means[ nn ] ) - tf$ns_m_offsets[[ nn ]] ) [ ll$ind_sortENU ]

        mo2i$ypred$NSpred_fENU_at_PSfs_p95os_retrafo_sortENU  <-
          ( ( ( mo2i$ypred$NSpred_fENU_at_PSfs_p95os * st$ns_m_sd[ nn ] )  + st$ns_m_means[ nn ] ) - tf$ns_m_offsets[[ nn ]] ) [ ll$ind_sortENU ]
        
        mo2i$ypred$NSpred_fENU_at_PSfs_p90os_retrafo_sortENU  <-
          ( ( ( mo2i$ypred$NSpred_fENU_at_PSfs_p90os * st$ns_m_sd[ nn ] )  + st$ns_m_means[ nn ] ) - tf$ns_m_offsets[[ nn ]] ) [ ll$ind_sortENU ]
        
        mo2i$ypred$NSpred_fENU_at_PSfs_p80os_retrafo_sortENU  <-
          ( ( ( mo2i$ypred$NSpred_fENU_at_PSfs_p80os * st$ns_m_sd[ nn ] )  + st$ns_m_means[ nn ] ) - tf$ns_m_offsets[[ nn ]] ) [ ll$ind_sortENU ]
       
        mo2i$ypred$NSpred_fENU_at_PSfs_p75os_retrafo_sortENU  <-
          ( ( ( mo2i$ypred$NSpred_fENU_at_PSfs_p75os * st$ns_m_sd[ nn ] )  + st$ns_m_means[ nn ] ) - tf$ns_m_offsets[[ nn ]] ) [ ll$ind_sortENU ]
        
        
        mo2i$ypred$NSpred_ENUobs_PSobs_retrafo_sortENU  <-
          ( ( ( mo2i$fitted.values * st$ns_m_sd[ nn ] )  + st$ns_m_means[ nn ] ) - tf$ns_m_offsets[[ nn ]] ) [ ll$ind_sortENU ]


      }


      # if ( match(  ch$ns_trafos[ n_i ] , "log" , nomatch = 99 ) == 1 ) # for logarithmic transformation of NS variable

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
        
        mo2i$ypred$NSpred_fENU_at_PSfs_p1os_retrafo_sortENU  <-  ( exp( (
          mo2i$ypred$NSpred_fENU_at_PSfs_p1os * st$ns_m_sd[ nn ] ) + st$ns_m_means[ nn ]  ) -  tf$ns_m_offsets[[ nn ]] ) [ ll$ind_sortENU ]

        mo2i$ypred$NSpred_fENU_at_PSfs_p5os_retrafo_sortENU  <-  ( exp( (
          mo2i$ypred$NSpred_fENU_at_PSfs_p5os * st$ns_m_sd[ nn ] ) + st$ns_m_means[ nn ]  ) -  tf$ns_m_offsets[[ nn ]] ) [ ll$ind_sortENU ]
                
        mo2i$ypred$NSpred_fENU_at_PSfs_p10os_retrafo_sortENU  <-  ( exp( (
          mo2i$ypred$NSpred_fENU_at_PSfs_p10os * st$ns_m_sd[ nn ] ) + st$ns_m_means[ nn ]  ) -  tf$ns_m_offsets[[ nn ]] ) [ ll$ind_sortENU ]
        
        mo2i$ypred$NSpred_fENU_at_PSfs_p20os_retrafo_sortENU  <-  ( exp( (
          mo2i$ypred$NSpred_fENU_at_PSfs_p20os * st$ns_m_sd[ nn ] ) + st$ns_m_means[ nn ]  ) -  tf$ns_m_offsets[[ nn ]] ) [ ll$ind_sortENU ]
        
        mo2i$ypred$NSpred_fENU_at_PSfs_p25os_retrafo_sortENU  <-  ( exp( (
          mo2i$ypred$NSpred_fENU_at_PSfs_p25os * st$ns_m_sd[ nn ] ) + st$ns_m_means[ nn ]  ) -  tf$ns_m_offsets[[ nn ]] ) [ ll$ind_sortENU ]
        
        
        mo2i$ypred$NSpred_fENU_at_max_signi_PS_retrafo_sortENU  <-  ( exp( (
          mo2i$ypred$NSpred_fENU_at_max_signi_PS * st$ns_m_sd[ nn ] ) + st$ns_m_means[ nn ]  ) -  tf$ns_m_offsets[[ nn ]] ) [ ll$ind_sortENU ]
        
        mo2i$ypred$NSpred_fENU_at_PSfs_p100os_retrafo_sortENU  <-  ( exp( (
          mo2i$ypred$NSpred_fENU_at_PSfs_p100os * st$ns_m_sd[ nn ] ) + st$ns_m_means[ nn ]  ) -  tf$ns_m_offsets[[ nn ]] ) [ ll$ind_sortENU ]
        
        mo2i$ypred$NSpred_fENU_at_PSfs_p95os_retrafo_sortENU  <-  ( exp( (
          mo2i$ypred$NSpred_fENU_at_PSfs_p95os * st$ns_m_sd[ nn ] ) + st$ns_m_means[ nn ]  ) -  tf$ns_m_offsets[[ nn ]] ) [ ll$ind_sortENU ]
        
        mo2i$ypred$NSpred_fENU_at_PSfs_p90os_retrafo_sortENU  <-  ( exp( (
          mo2i$ypred$NSpred_fENU_at_PSfs_p90os * st$ns_m_sd[ nn ] ) + st$ns_m_means[ nn ]  ) -  tf$ns_m_offsets[[ nn ]] ) [ ll$ind_sortENU ]
        
        mo2i$ypred$NSpred_fENU_at_PSfs_p80os_retrafo_sortENU  <-  ( exp( (
          mo2i$ypred$NSpred_fENU_at_PSfs_p80os * st$ns_m_sd[ nn ] ) + st$ns_m_means[ nn ]  ) -  tf$ns_m_offsets[[ nn ]] ) [ ll$ind_sortENU ]
        
        mo2i$ypred$NSpred_fENU_at_PSfs_p75os_retrafo_sortENU  <-  ( exp( (
          mo2i$ypred$NSpred_fENU_at_PSfs_p75os * st$ns_m_sd[ nn ] ) + st$ns_m_means[ nn ]  ) -  tf$ns_m_offsets[[ nn ]] ) [ ll$ind_sortENU ]
        
        
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
              exp(( mo2i$ypred$NSpred_fENU_at_min_signi_PS * st$ns_m_sd[ nn ] ) + st$ns_m_means[ nn ] )  ) [ ll$ind_sortENU ]
        
        mo2i$ypred$NSpred_fENU_at_PSfs_p5os_retrafo_sortENU  <-
          ( tf$ns_m_sat_val[[ nn ]]  -  tf$ns_m_offsets[[ nn ]]   +  g$ns_m_sign_for_sat_retrafo[ nn ] *
              exp((( mo2i$ypred$NSpred_fENU_at_PSfs_p5os * st$ns_m_sd[ nn ] ) + st$ns_m_means[ nn ] ) ) ) [ ll$ind_sortENU ]
        
        mo2i$ypred$NSpred_fENU_at_PSfs_p1os_retrafo_sortENU  <-
          ( tf$ns_m_sat_val[[ nn ]]  -  tf$ns_m_offsets[[ nn ]]   +  g$ns_m_sign_for_sat_retrafo[ nn ] *
              exp((( mo2i$ypred$NSpred_fENU_at_PSfs_p1os * st$ns_m_sd[ nn ] ) + st$ns_m_means[ nn ] ) ) ) [ ll$ind_sortENU ]
        
        mo2i$ypred$NSpred_fENU_at_PSfs_p10os_retrafo_sortENU  <-
          ( tf$ns_m_sat_val[[ nn ]]  -  tf$ns_m_offsets[[ nn ]]   +  g$ns_m_sign_for_sat_retrafo[ nn ] *
              exp((( mo2i$ypred$NSpred_fENU_at_PSfs_p10os * st$ns_m_sd[ nn ] ) + st$ns_m_means[ nn ] ) ) ) [ ll$ind_sortENU ]
        
        mo2i$ypred$NSpred_fENU_at_PSfs_p20os_retrafo_sortENU  <-
          ( tf$ns_m_sat_val[[ nn ]]  -  tf$ns_m_offsets[[ nn ]]   +  g$ns_m_sign_for_sat_retrafo[ nn ] *
              exp((( mo2i$ypred$NSpred_fENU_at_PSfs_p20os * st$ns_m_sd[ nn ] ) + st$ns_m_means[ nn ] ) ) ) [ ll$ind_sortENU ]
        
        mo2i$ypred$NSpred_fENU_at_PSfs_p25os_retrafo_sortENU  <-
          ( tf$ns_m_sat_val[[ nn ]]  -  tf$ns_m_offsets[[ nn ]]   +  g$ns_m_sign_for_sat_retrafo[ nn ] *
              exp((( mo2i$ypred$NSpred_fENU_at_PSfs_p25os * st$ns_m_sd[ nn ] ) + st$ns_m_means[ nn ] ) ) ) [ ll$ind_sortENU ]
        
       
        mo2i$ypred$NSpred_fENU_at_max_signi_PS_retrafo_sortENU  <-
          ( tf$ns_m_sat_val[[ nn ]]  -  tf$ns_m_offsets[[ nn ]]    +  g$ns_m_sign_for_sat_retrafo[ nn ] *
              exp((( mo2i$ypred$NSpred_fENU_at_max_signi_PS * st$ns_m_sd[ nn ] ) + st$ns_m_means[ nn ] ) ) ) [ ll$ind_sortENU ]
        
        mo2i$ypred$NSpred_fENU_at_PSfs_p100os_retrafo_sortENU  <-
          ( tf$ns_m_sat_val[[ nn ]]  -  tf$ns_m_offsets[[ nn ]]   +  g$ns_m_sign_for_sat_retrafo[ nn ] *
              exp((( mo2i$ypred$NSpred_fENU_at_PSfs_p100os * st$ns_m_sd[ nn ] ) + st$ns_m_means[ nn ] ) ) ) [ ll$ind_sortENU ]
        
        mo2i$ypred$NSpred_fENU_at_PSfs_p95os_retrafo_sortENU  <-
          ( tf$ns_m_sat_val[[ nn ]]  -  tf$ns_m_offsets[[ nn ]]   +  g$ns_m_sign_for_sat_retrafo[ nn ] *
              exp((( mo2i$ypred$NSpred_fENU_at_PSfs_p95os * st$ns_m_sd[ nn ] ) + st$ns_m_means[ nn ] ) ) ) [ ll$ind_sortENU ]
        
        mo2i$ypred$NSpred_fENU_at_PSfs_p90os_retrafo_sortENU  <-
          ( tf$ns_m_sat_val[[ nn ]]  -  tf$ns_m_offsets[[ nn ]]   +  g$ns_m_sign_for_sat_retrafo[ nn ] *
              exp((( mo2i$ypred$NSpred_fENU_at_PSfs_p90os * st$ns_m_sd[ nn ] ) + st$ns_m_means[ nn ] ) ) ) [ ll$ind_sortENU ]

        mo2i$ypred$NSpred_fENU_at_PSfs_p80os_retrafo_sortENU  <-
          ( tf$ns_m_sat_val[[ nn ]]  -  tf$ns_m_offsets[[ nn ]]   +  g$ns_m_sign_for_sat_retrafo[ nn ] *
              exp((( mo2i$ypred$NSpred_fENU_at_PSfs_p80os * st$ns_m_sd[ nn ] ) + st$ns_m_means[ nn ] ) ) ) [ ll$ind_sortENU ]
        
        mo2i$ypred$NSpred_fENU_at_PSfs_p75os_retrafo_sortENU  <-
          ( tf$ns_m_sat_val[[ nn ]]  -  tf$ns_m_offsets[[ nn ]]   +  g$ns_m_sign_for_sat_retrafo[ nn ] *
              exp((( mo2i$ypred$NSpred_fENU_at_PSfs_p75os * st$ns_m_sd[ nn ] ) + st$ns_m_means[ nn ] ) ) ) [ ll$ind_sortENU ]
        
        
        mo2i$ypred$NSpred_ENUobs_PSobs_retrafo_sortENU  <-
          ( tf$ns_m_sat_val[[ nn ]]  -  tf$ns_m_offsets[[ nn ]]  +   g$ns_m_sign_for_sat_retrafo[ nn ] *
              exp((( mo2i$fitted.values * st$ns_m_sd[ nn ] ) + st$ns_m_means[ nn ] ) ) ) [ ll$ind_sortENU ]


      }


      ### prediction and back-transform finished ####

      
      
      
      
      #### export results
      
      r_mod$NSpred_os_PSmax_minus_PSmin_percrange_sortENU[ pp , nn ,   ]  <-  ( ( mo2i$ypred$NSpred_fENU_at_PSfs_p100os_retrafo_sortENU  - 
                                                                                      mo2i$ypred$NSpred_fENU_at_PSfs_p1os_retrafo_sortENU) *100 / ll$ns_os_range ) * 
                                                                                      g$ns_m_more_is_better_as_sign [ nn ]
      
      r_mod$NSpred_os_PSmaxsg_minus_PSminsg_percrange_sortENU[ pp , nn ,   ]  <-  ( ( mo2i$ypred$NSpred_fENU_at_max_signi_PS_retrafo_sortENU  - 
                                                                                        mo2i$ypred$NSpred_fENU_at_min_signi_PS_retrafo_sortENU) *100 / ll$ns_os_range ) * 
                                                                                        g$ns_m_more_is_better_as_sign[ nn ]
      
      
      r_mod$NSpred_os_PSp90_minus_PSp10_percrange_sortENU[ pp , nn ,  ]  <-  ( ( mo2i$ypred$NSpred_fENU_at_PSfs_p90os_retrafo_sortENU  - 
                                                                                        mo2i$ypred$NSpred_fENU_at_PSfs_p10os_retrafo_sortENU) *100 / ll$ns_os_range ) * 
                                                                                        g$ns_m_more_is_better_as_sign [ nn ]
     
       r_mod$NSpred_os_PSp95_minus_PSp5_percrange_sortENU[ pp , nn ,   ]  <-  ( ( mo2i$ypred$NSpred_fENU_at_PSfs_p95os_retrafo_sortENU  - 
                                                                                        mo2i$ypred$NSpred_fENU_at_PSfs_p5os_retrafo_sortENU) *100 / ll$ns_os_range ) * 
                                                                                        g$ns_m_more_is_better_as_sign [ nn ]
      
      mo2i$ypred$NSpred_fENU_at_PSfs_p100os_retrafo_cutoff_sortENU  <- mo2i$ypred$NSpred_fENU_at_PSfs_p100os_retrafo_sortENU
      mo2i$ypred$NSpred_fENU_at_PSfs_p100os_retrafo_cutoff_sortENU [  mo2i$ypred$NSpred_fENU_at_PSfs_p100os_retrafo_cutoff_sortENU > a$ns_ceiling[nn] ] <- a$ns_ceiling[nn]
      mo2i$ypred$NSpred_fENU_at_PSfs_p100os_retrafo_cutoff_sortENU [  mo2i$ypred$NSpred_fENU_at_PSfs_p100os_retrafo_cutoff_sortENU < a$ns_floor[nn] ] <- a$ns_floor[nn]
      
            
      mo2i$ypred$NSpred_fENU_at_PSfs_p1os_retrafo_cutoff_sortENU  <- mo2i$ypred$NSpred_fENU_at_PSfs_p1os_retrafo_sortENU
      mo2i$ypred$NSpred_fENU_at_PSfs_p1os_retrafo_cutoff_sortENU [  mo2i$ypred$NSpred_fENU_at_PSfs_p1os_retrafo_cutoff_sortENU > a$ns_ceiling[nn] ] <- a$ns_ceiling[nn]
      mo2i$ypred$NSpred_fENU_at_PSfs_p1os_retrafo_cutoff_sortENU [  mo2i$ypred$NSpred_fENU_at_PSfs_p1os_retrafo_cutoff_sortENU < a$ns_floor[nn] ]   <- a$ns_floor[nn]
      
      
      mo2i$ypred$NSpred_fENU_at_PSfs_p90os_retrafo_cutoff_sortENU  <- mo2i$ypred$NSpred_fENU_at_PSfs_p90os_retrafo_sortENU
      mo2i$ypred$NSpred_fENU_at_PSfs_p90os_retrafo_cutoff_sortENU [  mo2i$ypred$NSpred_fENU_at_PSfs_p90os_retrafo_cutoff_sortENU > a$ns_ceiling[nn] ] <- a$ns_ceiling[nn]
      mo2i$ypred$NSpred_fENU_at_PSfs_p90os_retrafo_cutoff_sortENU [  mo2i$ypred$NSpred_fENU_at_PSfs_p90os_retrafo_cutoff_sortENU < a$ns_floor[nn] ] <- a$ns_floor[nn]
      
      
      mo2i$ypred$NSpred_fENU_at_PSfs_p10os_retrafo_cutoff_sortENU  <- mo2i$ypred$NSpred_fENU_at_PSfs_p10os_retrafo_sortENU
      mo2i$ypred$NSpred_fENU_at_PSfs_p10os_retrafo_cutoff_sortENU [  mo2i$ypred$NSpred_fENU_at_PSfs_p10os_retrafo_cutoff_sortENU > a$ns_ceiling[nn] ] <- a$ns_ceiling[nn]
      mo2i$ypred$NSpred_fENU_at_PSfs_p10os_retrafo_cutoff_sortENU [  mo2i$ypred$NSpred_fENU_at_PSfs_p10os_retrafo_cutoff_sortENU < a$ns_floor[nn] ]   <- a$ns_floor[nn]
      
      mo2i$ypred$NSpred_fENU_at_PSfs_p95os_retrafo_cutoff_sortENU  <- mo2i$ypred$NSpred_fENU_at_PSfs_p95os_retrafo_sortENU
      mo2i$ypred$NSpred_fENU_at_PSfs_p95os_retrafo_cutoff_sortENU [  mo2i$ypred$NSpred_fENU_at_PSfs_p95os_retrafo_cutoff_sortENU > a$ns_ceiling[nn] ] <- a$ns_ceiling[nn]
      mo2i$ypred$NSpred_fENU_at_PSfs_p95os_retrafo_cutoff_sortENU [  mo2i$ypred$NSpred_fENU_at_PSfs_p95os_retrafo_cutoff_sortENU < a$ns_floor[nn] ] <- a$ns_floor[nn]
      
      
      mo2i$ypred$NSpred_fENU_at_PSfs_p5os_retrafo_cutoff_sortENU  <- mo2i$ypred$NSpred_fENU_at_PSfs_p5os_retrafo_sortENU
      mo2i$ypred$NSpred_fENU_at_PSfs_p5os_retrafo_cutoff_sortENU [  mo2i$ypred$NSpred_fENU_at_PSfs_p5os_retrafo_cutoff_sortENU > a$ns_ceiling[nn] ] <- a$ns_ceiling[nn]
      mo2i$ypred$NSpred_fENU_at_PSfs_p5os_retrafo_cutoff_sortENU [  mo2i$ypred$NSpred_fENU_at_PSfs_p5os_retrafo_cutoff_sortENU < a$ns_floor[nn] ]   <- a$ns_floor[nn]
      
      
      r_mod$NSpred_os_PSmax_minus_PSmin_percrange_cutoff_sortENU[ pp , nn ,  ] <-  ( ( mo2i$ypred$NSpred_fENU_at_PSfs_p100os_retrafo_cutoff_sortENU  - 
                                                                                              mo2i$ypred$NSpred_fENU_at_PSfs_p1os_retrafo_cutoff_sortENU) *100 / ll$ns_os_range ) * 
                                                                                          g$ns_m_more_is_better_as_sign [ nn ]
      

      r_mod$NSpred_os_PSp90_minus_PSp10_percrange_cutoff_sortENU[ pp , nn ,  ] <-  ( ( mo2i$ypred$NSpred_fENU_at_PSfs_p90os_retrafo_cutoff_sortENU  - 
                                                                                              mo2i$ypred$NSpred_fENU_at_PSfs_p10os_retrafo_cutoff_sortENU) *100 / ll$ns_os_range ) * 
                                                                                        g$ns_m_more_is_better_as_sign [ nn ]
      
      
      r_mod$NSpred_os_PSp95_minus_PSp5_percrange_cutoff_sortENU[ pp , nn ,  ] <-  ( ( mo2i$ypred$NSpred_fENU_at_PSfs_p95os_retrafo_cutoff_sortENU  - 
                                                                                              mo2i$ypred$NSpred_fENU_at_PSfs_p5os_retrafo_cutoff_sortENU) *100 / ll$ns_os_range ) * 
                                                                                              g$ns_m_more_is_better_as_sign [ nn ]


      ########### PLOTS ##########################################################
      ########### PLOTS ##########################################################
      ########### PLOTS ##########################################################


      # plot marginal effects of each predictor, with confidence intervals, and save to file
      if ( sw$plot_mod_MargEff )
      {

        dir.create( paste( fp, "mod_analysis/MargEff2_NSsignflipped" , sep="" )  , recursive = TRUE )


        efn = paste( fp , "mod_analysis/MargEff2_NSsignflipped/","MargEff_NSsignlipped", ee,"-", fn$enu_m[ee],"_", nn, "-",fn$ns_m[nn],"_", pp,"-", fn$ps_u[pp],".png", sep="" )

        if ( sw$plot_mod_MargEff ) { png( filename= efn , width=1600,height=1000,res=144) }  # activates png engine if figures are to be exported

        par ( mfrow = c (1,2))

        plot (  ll$ps_sortPS , - 1* mo2i$marg_eff_enu_sortPS ,  # the factor (-1) is a quick & crappy solution to flip the effect to account for the flipping due to NS sat trafos
                ylim= c (-1.3 , 1.3 ),
                xlab = paste( pn$ps_u[pp] , " (standaridsed)", sep=""),
                ylab = paste( "effect ON " , pn$ns_m[ nn ] , " (standardised)" , sep=""),
                col = "blue", type="p" , lwd = 1 ,
                cex.main = 1.2, cex.lab = 1.3, cex.axis = 1.3  )
        title( main = paste( "marginal effect OF " , pn$enu_m[ee] ," (dNS/dENU)" ,  sep="") , font = 1, cex.main = 1.05)
        
        lines( ll$ps_sortPS , -1 *mo2i$marg_eff_enu_CIlower_sortPS , col="cyan" , lty = 1 , lwd = 2  )
        lines ( ll$ps_sortPS , -1 * mo2i$marg_eff_enu_CIupper_sortPS , col="cyan" , lty = 1 , lwd = 2  )
        lines ( c(-5,5) , c(0,0) , col="black" ,lty=2,lwd=2  )


        plot (  ll$enu_sortENU , - 1 * mo2i$marg_eff_ps_sortENU ,  # the factor (-1) is a quick & crappy solution to flip the effect to account for the flipping due to NS sat trafos
                ylim= c (-1.3 , 1.3 ),
                xlab = paste( pn$enu_m[ ee ] , " (standardised)", sep=""),
                ylab = paste("effect ON " , pn$ns_m[ nn ] , " (standardised)", sep=""),
                col = "blue", type="p" , lwd = 1 ,
                cex.main = 1.2, cex.lab = 1.3, cex.axis = 1.3  )
        title( main = paste( "marginal effect OF " , pn$ps_u[pp], " (dNS/dPF)", sep="") , font = 1, cex.main = 1.05)
        
        lines( ll$enu_sortENU , -1 * mo2i$marg_eff_ps_CIlower_sortENU , col="cyan" , lty=1, lwd= 2 )
        lines ( ll$enu_sortENU , -1 * mo2i$marg_eff_ps_CIupper_sortENU , col="cyan" , lty=1 , lwd= 2  )
        lines ( c(-5,5) , c(0,0) , col="black" ,lty=2,lwd=2  )


        if ( sw$plot_mod_MargEff ) { dev.off() }  # deactivates png engine if figures are to be exported
      }  ## end figure MargEff



    }
  }
}



r_mod$kstest_below_th <-  r_mod$kstest  <  ch$th_p_KS
r_bvr_ks$kstest_below_th <-  r_bvr_ks$kstest  <  ch$th_p_KS

r_mod$kstest_allow_for_agg  <- array( data = NA , dim = c ( nv$ps_u , nv$ns_m , nc$hs ) )

for (pp in 1:nv$ps_u){
 for ( nn in 1:nv$ns_m ){ 

     r_mod$kstest_allow_for_agg[ pp , nn  , ]  <-  rep ( ! r_mod$kstest_below_th[ pp , nn ] , nc$hs )
   }
  }

