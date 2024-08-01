
#### transform need satisfaction variables ####

if ( sw$exp_NStrafo_plots_resQQ ) { dir.create( paste( fp , "var_trafos/NStrafos_NSvsENU_residuals_qq_autoplots/", sep="" ) , recursive = TRUE ) }
if ( sw$exp_NStrafo_plots_fit ) { dir.create( paste( fp , "var_trafos/NStrafos_NSvsENU_fits//", sep="" ) , recursive = TRUE ) }


#### set saturation values for sat regression ####
# set with regards to the offset data sets (d_clo)

## !!!!! -------->  these have to be defined in the right order (same order as the ns vars) for the below code to work <----- !!!!!! ##
tf$ns_sat_val$prot      <-  145  # note this is below the values of ISL, ISR and LTU , but these are more like outliers
tf$ns_sat_val$hale      <- 77  
tf$ns_sat_val$unempl    <-  2
tf$ns_sat_val$edubas    <-  102 
tf$ns_sat_val$water     <-  100.7 
tf$ns_sat_val$sani       <-  100.7 
tf$ns_sat_val$fooddef   <-  100.3 # after flipping fooddef
tf$ns_sat_val$povgap320 <-  100.3 # after flipping povgap320  
tf$ns_sat_val$povhcrnl  <-  10.5
tf$ns_sat_val$doctors   <-  8
## !!!!! -------->  these have to be defined in the right order (same order as the ns vars) for the below code to work <----- !!!!!! ##


#### compare different transformations (lin =identity, log, sat = hyperbolic) ####
## note: use logged enu variables for determining NS trafos


tfm_ns <- list()  # transformation metrics for ns trafos


tfm_ns$R2_os_oneill   <-  array ( NA , dim = c ( nv$ns_all ,  3 ) ) 
tfm_ns$R2_fs          <-    array ( NA , dim = c ( nv$ns_all ,  3 ) ) 
tfm_ns$AIC_os_oneill  <-    array ( NA , dim = c ( nv$ns_all ,  3 ) ) 
tfm_ns$AICc_comp       <-   array ( NA , dim = c ( nv$ns_all ,  3 ) ) 
tfm_ns$p_KS_test      <-   array ( NA , dim = c ( nv$ns_all ,  3 ) ) 
tfm_ns$p_model_rob        <-    array ( NA , dim = c ( nv$ns_all ,  3 ) ) 


ll <- list()

for ( nn in 1:nv$ns_all ) {
  
    ee = which( fn$enu_all == "tfec")   # this is a leftover from an earlier version with several energy variables; this means the elements of the code with "ee" are redundant now, still need to clean
         
    ll$ns_var   =  ns_p [ cu , nn]  # use only harmonised sample ( hs ---> cu ) for determining the transformations
    ll$enu_var_logged  = enu_tf [ cu , ee ]  # fix logging the energy variable ;  # use only harmonised sample ( hs ---> cu ) for determining the transformations
    ll$enu_var  = enu_p [ cu , ee ]    # use only harmonised sample ( hs ---> cu ) for determining the transformations
    
    # below 4 lines are only relevant for NS variables not used for harmonising the sample, i.e. povhcrnl
    ll$com_nan  <-  is.na ( ll$ns_var + ll$enu_var_logged )
    
    ll$ns_var   <-  ll$ns_var [ ! ll$com_nan ]
    ll$enu_var  <-  ll$enu_var [ ! ll$com_nan ]
    ll$enu_var_logged  <- ll$enu_var_logged  [ ! ll$com_nan ]
    
    ll$enu_var_logged_sortENU  <-  sort( ll$enu_var_logged , index.return = TRUE )
    
    ll$enu_var_sortENU  <- ll$enu_var [ ll$enu_var_logged_sortENU$ix ]
    
    ll$N      <-  length ( ll$ns_var )
    

    ##################### ns linear
    
    ll$lin$modobj       <-  lm  (  ll$ns_var ~ ll$enu_var_logged )
    ll$lin$modobj_rob   <-  lm_robust  (  ll$ns_var ~ ll$enu_var_logged , se_type = sw$HC )
    
    ll$lin$a        <-  summary(ll$lin$modobj)$coefficients[1,1]
    ll$lin$b        <-  summary(ll$lin$modobj)$coefficients[2,1]
    ll$lin$res      <-  ll$lin$modobj$residuals

    ll$lin$res_sort <-  sort ( ll$lin$res  )
    
    ll$lin$kstest   <-  ks.test ( ll$lin$res_sort, "pnorm",  0 , sd ( ll$lin$res_sort ) )
    
    ll$lin$f_stat_rob   <-   summary(ll$lin$modobj_rob)$fstatistic
    ll$lin$p_model_rob  <-   pf  (  ll$lin$f_stat_rob[1] , ll$lin$f_stat_rob[2] , ll$lin$f_stat_rob[3] , lower.tail = FALSE )  
    
    ll$lin$os$ns_fit  <-  ll$lin$a +  ll$lin$b * ll$enu_var_logged
    ll$lin$os$RSS     <-  sum ( ( ll$ns_var  - ll$lin$os$ns_fit ) ^ 2 )
    ll$lin$os$TSS     <-  sum ( ( ll$ns_var  - mean ( ll$ns_var ) ) ^ 2 )
    ll$lin$os$R2_oneill   <-  1 - ( ll$lin$os$RSS  / ll$lin$os$TSS ) 
    ll$lin$os$AIC_oneill  <-  ( ll$N  * log ( ll$lin$os$RSS / ll$N )) + 2 * 2
    
    ll$lin$k  <- 2
    ll$lin$os$AICc         <-   AIC ( ll$lin$modobj , k = 2 )  +  (  ( 2 * (ll$lin$k ^2) + 2 * ll$lin$k  ) / ( ll$N - ll$lin$k  - 1)  )  # note the k that is the input into R's AIC function does NOT denote the number of parameters, but the weight for the number of parameters. The number of parameters is here ll$hyp$k
    ll$lin$os$AICc_comp    <-   ll$lin$os$AICc 
    
    
    tfm_ns$R2_os_oneill  [ nn ,  1 ]    <-  ll$lin$os$R2_oneill
    tfm_ns$R2_fs         [ nn ,  1 ]   <-  summary(ll$lin$modobj)$r.squared
    tfm_ns$AIC_os_oneill [ nn ,  1 ]   <-  ll$lin$os$AIC_oneill
    tfm_ns$AICc_comp     [ nn ,  1 ]    <-  ll$lin$os$AICc_comp
    tfm_ns$p_KS_test     [ nn ,  1 ]    <-  ll$lin$kstest$p.value
    tfm_ns$p_model_rob       [ nn ,  1 ]    <-  ll$lin$p_model_rob   # low p_values indicate low probability of residuals stemming from a normal distribution ( technically: low probability of getting the given (or higher) ks-test statistic if the residuals stem from a normal distribution)
    
    
    # plot residuals and QQ
    
    efn = paste( fp , "var_trafos/NStrafos_NSvsENU_residuals_qq_autoplots/",
                "NStrafos_NSvsENU_res_qq_ns", nn , fn$ns_all[nn],"_enu", ee , fn$enu_all[ee],"_NSlin", ".png", sep="" )
    
    if ( sw$exp_NStrafo_plots_resQQ ) { 
      png( filename= efn , width=1600,height=1200,res=144) 
      par (mfrow = ( c(2,2) ) )
      plot (  ll$lin$modobj )
      dev.off()
    } 
    
    # plot fit
    
    efn =paste( fp , "var_trafos/NStrafos_NSvsENU_fits/",
                "NStrafos_NSvsENU_fitOS_ns",nn , fn$ns_all[nn],"_enu", ee , fn$enu_all[ee],"_NSlin", ".png", sep="" )
    
    if ( sw$exp_NStrafo_plots_fit ) { 
      png( filename= efn , width=1600,height=1200,res=144) 
      par (mfrow = ( c(2,2) ) )
      plot ( ll$enu_var_sortENU , ll$lin$os$ns_fit[ ll$enu_var_logged_sort$ix ] , type = "l", col = "red",
             xlab = g$fn_pl[ee] , 
             ylab = g$fn_pl[nn] ,
             ylim = c ( min(ll$ns_var) , max( ll$ns_var ) ) , 
             main = "NS lin ; plot lin ENU")
      points ( ll$enu_var_sortENU , ll$ns_var[ ll$enu_var_logged_sort$ix ] )
      
      plot ( ll$enu_var_logged_sortENU$x  , ll$lin$os$ns_fit[ ll$enu_var_logged_sort$ix ] , type = "l", col = "red",
             xlab = g$fn_pl[ee] , 
             ylab = g$fn_pl[nn] ,
             ylim = c ( min(ll$ns_var) , max( ll$ns_var ) ) , 
             main = "NS lin ; plot log ENU")
      points ( ll$enu_var_logged_sortENU$x , ll$ns_var[ ll$enu_var_logged_sort$ix ] )
      
      plot ( ll$enu_var_logged_sortENU$x  , ll$lin$modobj$fitted.values[ ll$enu_var_logged_sort$ix ] , type = "l", col = "red",
             xlab = g$fn_pl[ee] , 
             ylab = g$fn_pl[nn] ,
             ylim = c ( min(  ll$ns_var ) , max(  ll$ns_var ) ) , 
             main = "NS lin ; plot fitspace")
      points ( ll$enu_var_logged_sortENU$x , ll$ns_var[ ll$enu_var_logged_sort$ix ] )
      
      dev.off()
    } 
    
    
    ##################### ns log
    
    ll$log$modobj       <-  lm  (  log( ll$ns_var ) ~ ll$enu_var_logged )
    ll$log$modobj_rob   <-  lm_robust  (  log( ll$ns_var ) ~ ll$enu_var_logged , se_type = sw$HC)
    
    ll$log$a        <-  summary(ll$log$modobj)$coefficients[1,1]
    ll$log$b        <-  summary(ll$log$modobj)$coefficients[2,1]
    ll$log$res      <-  ll$log$modobj$residuals 
    ll$log$res_sort <-  sort ( ll$log$res  )
    
    ll$log$kstest  <-  ks.test ( ll$log$res_sort, "pnorm",  0 , sd ( ll$log$res_sort ) )
    
    ll$log$f_stat_rob   <-   summary(ll$log$modobj_rob)$fstatistic
    ll$log$p_model_rob  <-   pf  (  ll$log$f_stat_rob[1] , ll$log$f_stat_rob[2] , ll$log$f_stat_rob[3] ,  lower.tail = FALSE )  
    
    ll$log$os$ns_fit  <-  exp ( ll$log$a +  ll$log$b * ll$enu_var_logged )
    ll$log$os$RSS     <-  sum ( ( ll$ns_var  - ll$log$os$ns_fit ) ^ 2 )
    ll$log$os$TSS     <-  sum ( ( ll$ns_var  - mean ( ll$ns_var ) ) ^ 2 )
    ll$log$os$R2_oneill   <-  1 - ( ll$log$os$RSS  / ll$log$os$TSS ) 
    ll$log$os$AIC_oneill  <-  ( ll$N  * log ( ll$log$os$RSS / ll$N )) + 2 * 2
    
    ll$log$k  <- 2
    ll$log$os$AICc         <-  AIC ( ll$log$modobj , k = 2 )  +  (  ( 2 * (ll$log$k ^2) + 2 * ll$log$k  ) / ( ll$N - ll$log$k  - 1)  )   # note the k that is the input into R's AIC function does NOT denote the number of parameters, but the weight for the number of parameters. The number of parameters is here ll$hyp$k
    ll$log$os$AICc_comp    <-   ll$log$os$AICc  + 2 * sum ( log ( ll$ns_var  ) )

    
    tfm_ns$R2_os_oneill  [ nn ,  2 ]   <-  ll$log$os$R2_oneill
    tfm_ns$R2_fs         [ nn ,  2]   <-  summary(ll$log$modobj)$r.squared
    tfm_ns$AIC_os_oneill [ nn , 2 ]   <-  ll$log$os$AIC_oneill
    tfm_ns$AICc_comp      [ nn ,  2 ]   <-  ll$log$os$AICc_comp 
    tfm_ns$p_KS_test     [ nn ,  2 ]   <-  ll$log$kstest$p.value
    tfm_ns$p_model_rob       [ nn ,  2 ]   <-  ll$log$p_model_rob
    
    # plot residuals and QQ
    
    efn =paste( fp , "var_trafos/NStrafos_NSvsENU_residuals_qq_autoplots/",
                "NStrafos_NSvsENU_res_qq_ns", nn , fn$ns_all[nn],"_enu", ee , fn$enu_all[ee],"_NSlog", ".png", sep="" )
    
    if ( sw$exp_NStrafo_plots_resQQ ) { 
      png( filename = efn , width=1600,height=1200,res=144) 
      par (mfrow = ( c(2,2) ) )
      plot (  ll$log$modobj )
      dev.off()
    } 
    
    # plot fit
    
    
    efn =paste( fp , "var_trafos/NStrafos_NSvsENU_fits/",
                "NStrafos_NSvsENU_fitOS_ns", nn , fn$ns_all[nn],"_enu", ee , fn$enu_all[ee],"_NSlog", ".png", sep="" )
    
    if ( sw$exp_NStrafo_plots_fit ) { 
      png( filename = efn , width=1600,height=1200,res=144) 
      par (mfrow = ( c(2,2) ) )
      
      plot ( ll$enu_var_sortENU , ll$log$os$ns_fit[ ll$enu_var_logged_sort$ix ] , type = "l", col = "red",
             xlab = g$fn_pl[ee] , 
             ylab = g$fn_pl[nn] ,
             ylim = c ( min(ll$ns_var) , max( ll$ns_var ) ) , 
             main = "NS log ; plot lin ENU")
      points ( ll$enu_var_sortENU , ll$ns_var[ ll$enu_var_logged_sort$ix ] )
      
      plot ( ll$enu_var_logged_sortENU$x  , ll$log$os$ns_fit[ ll$enu_var_logged_sort$ix ] , type = "l", col = "red",
             xlab = g$fn_pl[ee] , 
             ylab = g$fn_pl[nn] ,
             ylim = c ( min(ll$ns_var) , max( ll$ns_var ) ) , 
             main = "NS log ; plot log ENU")
      points ( ll$enu_var_logged_sortENU$x , ll$ns_var[ ll$enu_var_logged_sort$ix ] )
      
      plot ( ll$enu_var_logged_sortENU$x  , ll$log$modobj$fitted.values[ ll$enu_var_logged_sort$ix ] , type = "l", col = "red",
             xlab = g$fn_pl[ee] , 
             ylab = g$fn_pl[nn] ,
             ylim = c ( min( log ( ll$ns_var)) , max( log ( ll$ns_var) ) ) , 
             main = "NS log ; plot fitspace")
      points ( ll$enu_var_logged_sortENU$x , log (ll$ns_var[ ll$enu_var_logged_sort$ix ]) )
      
      dev.off()
    } 
    
    
    
    ##################### ns hyperbolic
    
    ll$ns_sat_val  <-  tf$ns_sat_val[[ nn ]]
    
    if ( g$ns_more_is_better[ nn ] ) {    
      ll$hyp$modobj       <-  lm  (  log( ll$ns_sat_val -  ll$ns_var ) ~ ll$enu_var_logged )  
      ll$hyp$modobj_rob   <-  lm_robust  (  log( ll$ns_sat_val -  ll$ns_var ) ~ ll$enu_var_logged , se_type = sw$HC)   }
    if ( ! g$ns_more_is_better[ nn ] ) {    
      ll$hyp$modobj       <-  lm  (  log( ll$ns_var  - ll$ns_sat_val ) ~ ll$enu_var_logged ) 
      ll$hyp$modobj_rob   <-  lm_robust  (  log( ll$ns_var  - ll$ns_sat_val  ) ~ ll$enu_var_logged , se_type = sw$HC)  }
    
    ll$hyp$a        <-  summary(ll$hyp$modobj)$coefficients[1,1]
    ll$hyp$b        <-  summary(ll$hyp$modobj)$coefficients[2,1]
    
    ll$hyp$res   <-   ll$hyp$modobj$residuals
    ll$hyp$res_sort <-  sort ( ll$hyp$res  )
    
    ll$hyp$kstest  <-  ks.test ( ll$hyp$res_sort, "pnorm",  0 , sd ( ll$hyp$res_sort ) )
    
    ll$hyp$f_stat_rob   <-   summary(ll$hyp$modobj_rob)$fstatistic
    ll$hyp$p_model_rob  <-   pf  (  ll$hyp$f_stat_rob[1] , ll$hyp$f_stat_rob[2] , ll$hyp$f_stat_rob[3] ,  lower.tail = FALSE )  
    
    if ( g$ns_more_is_better[ nn ] ) {    
      ll$hyp$os$ns_fit  <-  ll$ns_sat_val   - exp ( ll$hyp$a +  ll$hyp$b * ll$enu_var_logged ) }
    if ( ! g$ns_more_is_better[nn] ) {    
      ll$hyp$os$ns_fit  <-  ll$ns_sat_val   + exp ( ll$hyp$a +  ll$hyp$b * ll$enu_var_logged ) }
    
    ll$hyp$os$RSS     <-  sum ( ( ll$ns_var  - ll$hyp$os$ns_fit ) ^ 2 )
    ll$hyp$os$TSS     <-  sum ( ( ll$ns_var  - mean ( ll$ns_var ) ) ^ 2 )
    ll$hyp$os$R2_oneill   <-  1 - ( ll$hyp$os$RSS  / ll$hyp$os$TSS ) 
    ll$hyp$os$AIC_oneill  <-  ( ll$N  * log ( ll$hyp$os$RSS / ll$N )) + 2 * 3
    
    ll$hyp$k  <- 3
    ll$hyp$os$AICc         <-  AIC ( ll$hyp$modobj , k = 2 )  +  (  ( 2 * (ll$hyp$k ^2) + 2 * ll$hyp$k  ) / ( ll$N - ll$hyp$k  - 1)  )  # note the k that is the input into R's AIC function does NOT denote the number of parameters, but the weight for the number of parameters. The number of parameters is here ll$hyp$k

    
    if ( g$ns_more_is_better[nn] ) {    
      ll$hyp$os$AICc_comp    <-   ll$hyp$os$AICc  + 2 * sum ( log ( ll$ns_sat_val -  ll$ns_var  ) ) }  
    
    if ( ! g$ns_more_is_better[nn] ) {    
      ll$hyp$os$AICc_comp    <-   ll$hyp$os$AICc  + 2 * sum ( log ( ll$ns_var - ll$ns_sat_val  ) ) } 
    
    
    tfm_ns$R2_os_oneill  [ nn ,  3 ]    <-  ll$hyp$os$R2_oneill
    tfm_ns$R2_fs         [ nn ,  3 ]   <-  summary(ll$hyp$modobj)$r.squared
    tfm_ns$AIC_os_oneill [ nn ,  3 ]   <-  ll$hyp$os$AIC_oneill
    tfm_ns$AICc_comp     [ nn ,  3 ]   <-  ll$hyp$os$AICc_comp 
    tfm_ns$p_KS_test     [ nn ,  3 ]   <-  ll$hyp$kstest$p.value
    tfm_ns$p_model_rob   [ nn ,  3 ]   <-  ll$hyp$p_model_rob 
    
    # plot residuals and QQ
    
    efn =paste( fp , "var_trafos/NStrafos_NSvsENU_residuals_qq_autoplots/",
                "NStrafos_NSvsENU_res_qq_ns", nn , fn$ns_all[nn],"_enu", ee , fn$enu_all[ee],"_NSsat", ".png", sep="" )
    
    if ( sw$exp_NStrafo_plots_resQQ ) { 
      png( filename = efn , width=1600,height=1200,res=144) 
      par (mfrow = ( c(2,2) ) )
      plot (  ll$hyp$modobj )
      dev.off()
    } 
    
    # plot fit
    
    efn =paste( fp , "var_trafos/NStrafos_NSvsENU_fits/",
                "NStrafos_NSvsENU_fitOS_ns", nn , fn$ns_all[nn],"_enu", ee , fn$enu_all[ee],"_NSsat", ".png", sep="" )
    
    
    if ( g$ns_more_is_better[ nn ] ) {    
      ll$ylim_hyp  <-  c ( min(  log( ll$ns_sat_val -  ll$ns_var ) )  , max(  log( ll$ns_sat_val -  ll$ns_var ) ) ) } 
    if ( ! g$ns_more_is_better[ nn ] ) {    
      ll$ylim_hyp  <-  c ( min(    log( ll$ns_var  - ll$ns_sat_val  ) )  , max(  log( ll$ns_var  - ll$ns_sat_val  )) ) }
    
    if ( sw$exp_NStrafo_plots_fit ) { 
      png( filename = efn , width=1600,height=1200,res=144) 
      par (mfrow = ( c(2,2) ) )
      
      plot ( ll$enu_var_sortENU , ll$hyp$os$ns_fit[ ll$enu_var_logged_sort$ix ] , type = "l", col = "red",
             xlab = g$fn_pl[ee] , 
             ylab = g$fn_pl[nn] ,
             ylim = c ( min(ll$ns_var) , max( ll$ns_var ) ) , 
             main = paste("NS hyp ; plot lin ENU ; sat = ", ll$ns_sat_val , sep="") )
      points ( ll$enu_var_sortENU , ll$ns_var[ ll$enu_var_logged_sort$ix ] )
      
      plot ( ll$enu_var_logged_sortENU$x  , ll$hyp$os$ns_fit[ ll$enu_var_logged_sort$ix ] , type = "l", col = "red",
             xlab = g$fn_pl[ee] , 
             ylab = g$fn_pl[nn] ,
             ylim = c ( min(ll$ns_var) , max( ll$ns_var ) ) , 
             main = paste("NS hyp ; plot log ENU ; sat = ", ll$ns_sat_val , sep="") )
      points ( ll$enu_var_logged_sortENU$x , ll$ns_var[ ll$enu_var_logged_sort$ix ] )
      
      plot ( ll$enu_var_logged_sortENU$x  , ll$hyp$modobj$fitted.values[ ll$enu_var_logged_sort$ix ] , type = "l", col = "red",
             xlab = g$fn_pl[ee] , 
             ylab = g$fn_pl[nn] ,
             ylim = ll$ylim ,
             main = paste("NS hyp ; plot fitspace ; sat = ", ll$ns_sat_val , sep="") )
      
      if ( g$ns_more_is_better[ nn ] ) {    
        points ( ll$enu_var_logged_sortENU$x ,  log( ll$ns_sat_val  - ll$ns_var[ ll$enu_var_logged_sort$ix ]  ) , main = "NS hyp ; plot fitspace") }
      if ( ! g$ns_more_is_better[ nn ] ) {    
        points ( ll$enu_var_logged_sortENU$x ,  log( ll$ns_var[ ll$enu_var_logged_sort$ix ] - ll$ns_sat_val  ) , main = "NS hyp ; plot fitspace")  }
      


      dev.off()
    } 
    
}


tfm_ns$p_KS_below_th     <-    tfm_ns$p_KS_test  <  ch$th_p_KS   # TRUE suggests non-normally distributed residuals
tfm_ns$p_model_above_th  <-    tfm_ns$p_model    <  ch$th_p_model   # FALSE indicates poor model likelihood (poor model quality)


tf$ns_tf_use <- vector ( mode = "character" , length = length ( fn$ns_all ) ) 
  
tf$ns_tfn <- c ( "lin" , "log" , "sat")

for( nn in 1:length ( fn$ns_all ) ){

   tf$ns_tf_use[ nn ] <- tf$ns_tfn[ which ( tfm_ns$AICc_comp[ nn , ] == min( tfm_ns$AICc_comp[ nn , ] )) ]

}

names ( tf$ns_tf_use )  <- fn$ns_all


ns_tf <- ns_p

for ( nn in 1:nv$ns_all ){

  if ( tf$ns_tf_use[[ nn ]] == "log" ) {    ns_tf[ , nn ]  <-  log ( ns_p[ , nn ] ) }
  
  if ( ( tf$ns_tf_use[[ nn ]] == "sat" ) & ( g$ns_more_is_better[nn]  )) {  ns_tf[ , nn ]  <-  log ( tf$ns_sat_val[[nn]] - ns_p[ , nn ] ) }
  if ( ( tf$ns_tf_use[[ nn ]] == "sat" ) & ( ! g$ns_more_is_better[nn] )) {  ns_tf[ , nn ]  <-  log ( ns_p[ , nn ] - tf$ns_sat_val[[nn]] ) }

  # if neither "log" nor "sat", ns_tf[,nn] will remain equal to ns_p[,nn], i.e. identity trafo
}


ns9_tf  <-  ns_tf[ ,  ! fn$ns_all == "povhcrnl"  ]  # create data set of transformed ns data, excluding povhcrnl (because it lacks data for most rich countries)


