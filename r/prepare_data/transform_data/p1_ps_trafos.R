
g$ns_final_set <- c( "hale" , "edubas" , "water" , "sani" , "fooddef" , "povgap320" ) 
g$l_n  <- length( g$ns_final_set )

fn$ns9 <- colnames(ns9_tf)
fn$ns6 <- g$ns_final_set

ns6_tf  <- ns9_tf[ , fn$ns9  %in%  fn$ns6 ]



tfm_ps <- list()  # transformation metrics for ps trafos


tfm_ps$AICc            <-   array  (  NA , dim = c ( nv$ps_all , 3 , g$l_n   )  ) # exclude ns var povhcrnl from ns vars because it is not representative ( no data for most rich countries)
tfm_ps$r2_fs           <-   array  (  NA , dim = c ( nv$ps_all , 3 , g$l_n  )  )
tfm_ps$p_ks_test       <-   array  (  NA , dim = c ( nv$ps_all , 3 , g$l_n ) )
tfm_ps$p_model_rob     <-   array  (  NA , dim = c ( nv$ps_all , 3 , g$l_n  )  )

tfm_ps$AICc_modelsum   <-  array  (  NA , dim = c ( nv$ps_all , 3  )  )  # for each ps var and tt type, sum of AICc over all ns vars


rownames( tfm_ps$AICc )            <- fn$ps_all
rownames( tfm_ps$AICc_modelsum )   <- fn$ps_all
rownames( tfm_ps$r2_fs )           <- fn$ps_all
rownames( tfm_ps$p_ks_test )       <- fn$ps_all
rownames( tfm_ps$p_model_rob)      <- fn$ps_all


ll$pool <- array ( NA , dim = c ( nc$all , 1 + g$l_n  + 1 ) )

for ( nn in 1 : g$l_n  ) { 
  
  ll$pool[  , nn + 1]   <-  ns6_tf[ , nn  ]

  }



ll$pool[  ,  1 + length( ns6_tf) + 1 ]    <-   enu_tf$tfec 
  

rownames ( ll$pool )   <-   g$isos_all
colnames ( ll$pool )   <-   c( "PSvar", names(ns6_tf) , "tfec" )


#### manually set saturation values for access-type ps variables ####


tf$ps_sat_val             <-    vector ( mode = "numeric" , length = nv$ps_all   )
names ( tf$ps_sat_val )   <-    fn$ps_all


## manually set saturation values

tf$ps_sat_val[ names ( tf$ps_sat_val ) == "elecacc" ]   <-  100.7
tf$ps_sat_val[ names ( tf$ps_sat_val ) == "nsolfuel" ]  <-  100.7
tf$ps_sat_val[ names ( tf$ps_sat_val ) == "democ" ]  <-  1.93  + tf$iv_dcl_offsets_applied$democ 
tf$ps_sat_val[ names ( tf$ps_sat_val ) == "tmin" ]      <-  28  +  tf$iv_dcl_offsets_applied$tmin
tf$ps_sat_val[ names ( tf$ps_sat_val ) == "tmean" ]     <-  30  +  tf$iv_dcl_offsets_applied$tmean  

#### initiate variable to store transformed ps variables ####

ps_tf  <-  ps_p  


#### start transformation + regression loop ####


for ( pp in 1 : nv$ps_all ){
  
  ll$ps_pre <-  ps_p[ , pp ]
  # ll$ps_pre <-  ps_p[ cu , pp ]
  
  ## create pool to be used (_u)
  ll$pool_u          <- ll$pool
  ll$pool_u[  , 1 ]  <- ll$ps_pre
  
  colnames( ll$pool_u )[ 1 ]   <- names(ps_p)[pp]
  
  
  #### restrict pool sample ####
  ## if ps var has data for all countries in hs, use hs as sample; 
  ## else create maximum joint sample for current pool (i.e. NS vars + current PS var)
  
  if   ( hs$hs_compatible_ps[ pp ] ) {  ll$pool_u    <-    ll$pool_u[ cu , ]  
    }  else { 
      
    ll$pool_u_flag_nans  <-  is.na( ll$pool_u )
    
    ll$cu_cpool  <-  ! apply ( ll$pool_u_flag_nans , 1 , function(k) any( k %in%  ll$pool_u_flag_nans[k,] ) )  
    
    ll$pool_u   <-  ll$pool_u [ ll$cu_cpool , ]
    }
  
  ll$ps_u   <-  ll$pool_u[ , 1 ]
  ll$ns_p_u <-  ll$pool_u[ , 2 : ( 1 + length( ns6_tf )  ) ]
  
  ll$N      <-   length( ll$ps_u )

    #### set saturation values for ps variables where no saturation value has been manually defined  ####
  
  if  ( tf$ps_sat_val[ pp ] ==  0 )    {   tf$ps_sat_val[ pp ]   <-  1.1  *  max ( ll$ps_u  )   }
  
  
  
  for ( tt in 1:3 ){   # transformation types
    
    if( tt == 1 )  { 
      ll$ps_tf       <-    ll$ps_u               # identity transformation
      ll$k  = 2                 }                     
    if( tt == 2 )  { 
      ll$ps_tf       <-    log ( ll$ps_u )       # log transformation
      ll$k  = 2   }             
    if( tt == 3 )  {
      ll$ps_tf       <-    log ( tf$ps_sat_val[ pp ]  -   ll$ps_u  )    # sat transformation, maximum type only (see above comments)
      ll$k  = 3   }   
    
    
    for ( nn in 1: length ( ns6_tf) ){
      
      ll$mo        <-  lm  (  ll$ns_p_u[ , nn ]  ~  ll$ps_tf   )
      ll$mo_rob    <-  lm_robust  (  ll$ns_p_u[ , nn ]  ~  ll$ps_tf   , se_type = sw$HC )
      
      ll$AICc        <-   AIC( ll$mo  , k = 2 )  +  (  ( 2 * ( ll$k ^2 ) + 2 * ll$k  ) / ( ll$N - ll$k - 1 ) )  # note the k that is the input into R's AIC function does NOT denote the number of parameters, but the weight for the number of parameters. The number of parameters is here ll$k 

      ll$p_ks_test   <-   ks.test ( sort ( ll$mo$residuals ) , "pnorm",  0 , sd ( sort ( ll$mo$residuals ) ) )$p.value
      
      ll$f_stat_rob    <-   summary( ll$mo_rob )$fstatistic
      ll$p_model_rob   <-   pf  (  ll$f_stat_rob[1] , ll$f_stat_rob[2] , ll$f_stat_rob[3] , lower.tail = FALSE )  
      
      tfm_ps$AICc        [ pp , tt , nn ]    <-   ll$AICc
      tfm_ps$p_ks_test   [ pp , tt , nn ]    <-   ll$p_ks_test
      tfm_ps$p_model_rob [ pp , tt , nn ]    <-   ll$p_model_rob
      tfm_ps$r2_fs       [ pp , tt , nn ]    <-   summary( ll$mo )$r.squared
      
    }
  
    tfm_ps$AICc_modelsum [ pp , tt ]  <-  sum ( tfm_ps$AICc[ pp , tt ,  ] )
    
  }
  
  
  ### evaluate trafo metrics and apply the best trafo for each ps var
  
  tf$ps_tf_use[ pp ]    <-    tf$ns_tfn[ which ( tfm_ps$AICc_modelsum [ pp ,  ] == min( tfm_ps$AICc_modelsum [ pp ,  ] ) )  ]
  
  # if neither log nor sat, then it's lin = identity and can just remain as is
  
  if ( tf$ps_tf_use[ pp ] == "lin" ) {   ps_tf[ , pp ]    <-   ps_p [ , pp ]      }
  
  if ( tf$ps_tf_use[ pp ] == "log" ) {   ps_tf[ , pp ]    <-   log ( ps_p [ , pp ] )     }
  
  if ( tf$ps_tf_use[ pp ] == "sat" ) {   ps_tf[ , pp ]    <-   log ( tf$ps_sat_val[ pp ]  -  ps_p [ , pp ] )     }  # maximum-type only (ok-ish)

}


names(  tf$ps_tf_use  )  <-  fn$ps_all



#### hard-wire transformations for growth and net-imports to be linear (because of meaningful negative/positive distinction) ??? ####

  if( sw$hardwire_lintrafos_growth_netimpo ){ 

  ps_tf$growth   <-   ps_p$growth 
  ps_tf$netimpo  <-   ps_p$netimpo
  
  tf$ps_tf_use[ which( names( tf$ps_tf_use)  == "growth" )]    <-   "lin"
  tf$ps_tf_use[ which( names( tf$ps_tf_use)  == "netimpo" )]   <-   "lin"
                            
} 


tfm_ps$p_model_above_th_rob  <-    tfm_ps$p_model_rob  >   ch$th_p_model
tfm_ps$p_kstest_below_th     <-    tfm_ps$p_ks_test    <   ch$th_p_KS


# use the transformation with the lowest sum of AICc

