


mvr <- list()
bvr <- list()

mvr$b1_enu <- array( data = NA , dim = c ( g$l_n  ) )
mvr$b2_gini <- array( data = NA , dim = c ( g$l_n   ) )
mvr$b3_resrent <- array( data = NA , dim = c ( g$l_n    ) )
mvr$b4_gov <- array( data = NA , dim = c ( g$l_n    ) )

mvr$p_b1_enu <- array( data = NA , dim = c ( g$l_n    ) )
mvr$p_b2_gini <- array( data = NA , dim = c ( g$l_n   ) )
mvr$p_b3_resrent <- array( data = NA , dim = c (g$l_n    ) )
mvr$p_b4_gov <- array( data = NA , dim = c ( g$l_n   ) )

mvr$r2_adj_mvr  <- array( data = NA , dim = c ( g$l_n    ) )
mvr$r2_adj_bvr  <- array( data = NA , dim = c ( g$l_n   ) )


mvr$f2  <-  array( data = NA , dim = c ( 4 , g$l_n    ) )
mvr$power  <-  array( data = NA , dim = c ( 4 , g$l_n    ) )

mvr$kstest_pval  <-  array( data = NA , dim = c (  g$l_n    ) )


for( ni in 1: g$l_n ){
  # for ( ee in 1 : nv$enu_m ) {
  
  nn = c(1,3,5:8)[ ni ]
  
    for ( ee in which ( fn$enu_all == "tfec" )  ) {
      
    mvr$mo_full       <-   lm ( ns_m[ , nn ]  ~  enu_m[ , ee ] +  ps_m$gini  +  ps_m$resrent  + ps_m$gov   )
    mvr$mo_full_rob   <-   lm_robust ( ns_m[ , nn ]  ~  enu_m[ , ee ] +  ps_m$gini  +  ps_m$resrent  + ps_m$gov  , se_type = sw$HC )
    
      
    mvr$kstest_pval[ ni  ]  <- ks.test ( sort( mvr$mo_full$residuals ) , "pnorm",  0 , sd (  mvr$mo_full$residuals  ) )$p.value
    
    mvr$b1_enu[ ni  ]       <-  summary ( mvr$mo_full )$coefficient[ 2 , 1 ]
    mvr$b2_gini[ ni ]      <-  summary ( mvr$mo_full )$coefficient[ 3 , 1 ]
    mvr$b3_resrent[ ni  ]   <-  summary ( mvr$mo_full )$coefficient[ 4 , 1 ]
    mvr$b4_gov[ ni ]       <-  summary ( mvr$mo_full )$coefficient[ 5 , 1 ]
    
    mvr$p_b1_enu[ ni ]      <-  summary ( mvr$mo_full_rob )$coefficient[ 2 , 4 ]
    mvr$p_b2_gini[ ni  ]     <-  summary ( mvr$mo_full_rob )$coefficient[ 3 , 4 ]
    mvr$p_b3_resrent[ ni  ]  <-  summary ( mvr$mo_full_rob )$coefficient[ 4 , 4 ]
    mvr$p_b4_gov[ ni ]      <-  summary ( mvr$mo_full_rob )$coefficient[ 5 , 4 ]
    
    mvr$mo_no_enu        <-   lm ( ns_m[ , nn ]  ~  ps_m$gini  +  ps_m$resrent  + ps_m$gov   )
    mvr$mo_no_gini       <-   lm ( ns_m[ , nn ]  ~  enu_m[ , ee ] +  ps_m$resrent + ps_m$gov   )
    mvr$mo_no_resrent    <-   lm ( ns_m[ , nn ]  ~  enu_m[ , ee ] +  ps_m$gini    + ps_m$gov   )
    mvr$mo_no_gov        <-   lm ( ns_m[ , nn ]  ~  enu_m[ , ee ] +  ps_m$gini    +  ps_m$resrent   )
   
    mvr$r2_adj_mvr[ ni  ]    <-      summary ( mvr$mo_full )$adj.r.squared
    mvr$r2_adj_bvr[ ni  ]    <-      summary ( lm ( ns_m[ , nn ]  ~  enu_m[ , ee ]  ) )$adj.r.squared
    
    
    mvr$f2 [ 1, ni   ]   <-   ( summary( mvr$mo_full )$r.squared  -  summary( mvr$mo_no_enu )$r.squared ) /
                                      ( 1 - summary( mvr$mo_full )$r.squared ) 
    mvr$f2 [ 2, ni   ]   <-   ( summary( mvr$mo_full )$r.squared  -  summary( mvr$mo_no_gini )$r.squared ) /
                                      ( 1 - summary( mvr$mo_full )$r.squared ) 
    mvr$f2 [ 3, ni   ]   <-   ( summary( mvr$mo_full )$r.squared  -  summary( mvr$mo_no_resrent )$r.squared ) /
                                      ( 1 - summary( mvr$mo_full )$r.squared ) 
    mvr$f2 [ 4 , ni  ]   <-   ( summary( mvr$mo_full )$r.squared  -  summary( mvr$mo_no_gov )$r.squared ) /
                                      ( 1 - summary( mvr$mo_full )$r.squared ) 
    
    for ( ii in 1:4 ) { 
      mvr$power [ ii , ni  ]   <-  wp.regression( n = nc$hs, p1 = 4 , p2 = 3 , f2 = mvr$f2 [ ii , ni  ] ,
                                                      alpha = 0.05, power = NULL)$power
    }
    
  }
}



mvr$kstest_pval_below_th   <-   mvr$kstest_pval  < ch$th_p_KS


g$normalise_sign_of_b_on_ns  <-  rep( 1 , g$l_n )
g$normalise_sign_of_b_on_ns[ ( tf$ns_m_tf_use[c(1,3,5:8) ] == "sat" ) & ( g$ns_m_more_is_better[c(1,3,5:8)] ) ]  <-   -  1
g$normalise_sign_of_b_on_ns[ !  g$ns_m_more_is_better[c(1,3,5:8)] ]  <-   ( -  1 ) * g$normalise_sign_of_b_on_ns[ !  g$ns_m_more_is_better[c(1,3,5:8)] ] 



mvr$b1_enu_normalised       <-  mvr$b1_enu      *   g$normalise_sign_of_b_on_ns 
mvr$b2_gini_normalised      <-  ( - 1 ) * mvr$b2_gini     *   g$normalise_sign_of_b_on_ns   # -1 because of sat trafo (note: gini is now flipped)
mvr$b3_resrent_normalised   <-  mvr$b3_resrent  *   g$normalise_sign_of_b_on_ns
mvr$b4_gov_normalised       <-  mvr$b4_gov      *   g$normalise_sign_of_b_on_ns  


mvr$p_b1_enu_stars <- array( data = NA , dim = c ( g$l_n ) )
mvr$p_b2_gini_stars <- array( data = NA , dim = c ( g$l_n  ) )
mvr$p_b3_resrent_stars <- array( data = NA , dim = c ( g$l_n  ) )
mvr$p_b4_gov_stars <- array( data = NA , dim = c ( g$l_n ) )

mvr$p_b1_enu_stars[ mvr$p_b1_enu < 0.001]  <-  "***"
mvr$p_b1_enu_stars[ ( mvr$p_b1_enu > 0.001 ) & ( mvr$p_b1_enu < 0.01 )]  <-  "**"
mvr$p_b1_enu_stars[ ( mvr$p_b1_enu > 0.01 ) & ( mvr$p_b1_enu < 0.05 )]  <-  "*"
mvr$p_b1_enu_stars[ ( mvr$p_b1_enu > 0.05 ) & ( mvr$p_b1_enu < 0.1 )]  <-  "."
mvr$p_b1_enu_stars[ ( mvr$p_b1_enu > 0.1 )]  <-  ""

mvr$p_b2_gini_stars[ mvr$p_b2_gini < 0.001]  <-  "***"
mvr$p_b2_gini_stars[ ( mvr$p_b2_gini > 0.001 ) & ( mvr$p_b2_gini < 0.01 )]  <-  "**"
mvr$p_b2_gini_stars[ ( mvr$p_b2_gini > 0.01 ) & ( mvr$p_b2_gini < 0.05 )]  <-  "*"
mvr$p_b2_gini_stars[ ( mvr$p_b2_gini > 0.05 ) & ( mvr$p_b2_gini < 0.1 )]  <-  "."
mvr$p_b2_gini_stars[ ( mvr$p_b2_gini > 0.1 )]  <-  ""

mvr$p_b3_resrent_stars[ mvr$p_b3_resrent < 0.001]  <-  "***"
mvr$p_b3_resrent_stars[ ( mvr$p_b3_resrent > 0.001 ) & ( mvr$p_b3_resrent < 0.01 )]  <-  "**"
mvr$p_b3_resrent_stars[ ( mvr$p_b3_resrent > 0.01 ) & ( mvr$p_b3_resrent < 0.05 )]  <-  "*"
mvr$p_b3_resrent_stars[ ( mvr$p_b3_resrent > 0.05 ) & ( mvr$p_b3_resrent < 0.1 )]  <-  "."
mvr$p_b3_resrent_stars[ ( mvr$p_b3_resrent > 0.1 )]  <-  ""

mvr$p_b4_gov_stars[ mvr$p_b4_gov < 0.001]  <-  "***"
mvr$p_b4_gov_stars[ ( mvr$p_b4_gov > 0.001 ) & ( mvr$p_b4_gov < 0.01 )]  <-  "**"
mvr$p_b4_gov_stars[ ( mvr$p_b4_gov > 0.01 ) & ( mvr$p_b4_gov < 0.05 )]  <-  "*"
mvr$p_b4_gov_stars[ ( mvr$p_b4_gov > 0.05 ) & ( mvr$p_b4_gov < 0.1 )]  <-  "."
mvr$p_b4_gov_stars[ ( mvr$p_b4_gov > 0.1 )]  <-  ""


mvr$b1_enu_4table   <-   array( data = NA , dim = c (  g$l_n ) )
mvr$b2_gini_4table   <-   array( data = NA , dim = c ( g$l_n ) )
mvr$b3_resrent_4table   <-   array( data = NA , dim = c (  g$l_n ) )
mvr$b4_gov_4table   <-   array( data = NA , dim = c (  g$l_n ) )

for( ni in 1:g$l_n ){

    mvr$b1_enu_4table[ ni ]      <-  paste( round( mvr$b1_enu_normalised[ni] , 2) , mvr$p_b1_enu_stars[ni]  , sep="  ")
    mvr$b2_gini_4table[ ni ]     <-  paste( round( mvr$b2_gini_normalised[ni] , 2) , mvr$p_b2_gini_stars[ni]  , sep="  ")
    mvr$b3_resrent_4table[ ni ]  <-  paste( round( mvr$b3_resrent_normalised[ni] , 2) , mvr$p_b3_resrent_stars[ni]  , sep="  ")
    mvr$b4_gov_4table[ ni ]      <-  paste( round( mvr$b4_gov_normalised[ni] , 2) , mvr$p_b4_gov_stars[ni]  , sep="  ")
    
}



mvr$coeffs_allNS_allENU_4table  <-   array( data = NA , dim = c ( 4  , g$l_n) ) # 4 in this case is for # predictors
mvr$powers_allNS_allENU_4table  <-   array( data = NA , dim = c ( 4  , g$l_n  ) ) # 4 in this case is for # predictors

rownames( mvr$coeffs_allNS_allENU_4table )  <- c( "TFEC" , "equality of income" , "extractivism" , "public services")
rownames(mvr$powers_allNS_allENU_4table)   <- c( "TFEC" , "equality of income" , "extractivism" , "public services"  )

colnames( mvr$coeffs_allNS_allENU_4table )   <-   pn$ns_m[ c(1,3,5:8)]
colnames( mvr$powers_allNS_allENU_4table )   <-   pn$ns_m[ c(1,3,5:8)]

rownames ( mvr$r2_adj_mvr )   <- fn$ns_m[ c(1,3,5:8)]
rownames ( mvr$r2_adj_bvr )   <- fn$ns_m[ c(1,3,5:8)]


mvr$coeffs_allNS_allENU_4table[ 1  , ]    <-   mvr$b1_enu_4table
mvr$coeffs_allNS_allENU_4table[ 2  , ]    <-   mvr$b2_gini_4table
mvr$coeffs_allNS_allENU_4table[ 3  , ]    <-   mvr$b3_resrent_4table
mvr$coeffs_allNS_allENU_4table[ 4  , ]    <-   mvr$b4_gov_4table
  
mvr$powers_allNS_allENU_4table[ 1:4  , ]  <-  mvr$power [ 1:4 ,  ]




if ( sw$mvr_exp_regr_tables ) {

  dir.create( paste( fp, "mvr_gini_resrent_gov/", sep="" ) )

  write.csv(  mvr$coeffs_allNS_allENU_4table , file=paste(fp, "mvr_gini_resrent_gov/coeffs_allNS_allENU" , v_run , ".csv",sep="") )
  write.csv(  mvr$powers_allNS_allENU_4table , file=paste(fp, "mvr_gini_resrent_gov/powers_allNS_allENU" , v_run , ".csv",sep="") )

}




########     SCENARIOS      ##########
########     SCENARIOS      ##########
########     SCENARIOS      ##########
########     SCENARIOS      ##########


sc <- list()  # scenarios
# "obs" scenario is for predictions based on observed values
# p10 / p90 = 10th / 90th percentile

sc$gini$median          <-  median( ps_m$gini )
sc$gini$obs   <-  ps_m$gini 
sc$gini$p10    <-   sort ( ps_m$gini)[ round ( 0.1 * nc$hs)]  # 10th percentile = "good" because gini is now flipped (positive upwards) but sat trafo is employed (v.6.4 )


sc$resrent$median          <-  median( ps_m$resrent )
sc$resrent$obs   <-  ps_m$resrent 
sc$resrent$p10    <-   sort ( ps_m$resrent)[ round ( 0.1 * nc$hs)]


sc$gov$median          <-  median( ps_m$gov )
sc$gov$obs   <-  ps_m$gov 
sc$gov$p90    <-   sort ( ps_m$gov)[ round ( 0.9 * nc$hs)]


#### calculate predictions for scenarios

bvr$NSpred_fs <-  array (  data = NA ,  dim = c ( nc$hs , length( c( 1,3,5:8 ) ) ) )
bvr$NSpred_os <-  array (  data = NA ,  dim = c ( nc$hs , length( c( 1,3,5:8 ) ) ) )

mvr$sc_prov_obs$NSpred_fs   <-    array (  data = NA ,  dim = c ( nc$hs , length( c( 1,3,5:8 ) ) ) )
mvr$sc_prov_obs$NSpred_os   <-    array (  data = NA ,  dim = c ( nc$hs , length( c( 1,3,5:8 ) ) ) )
  
mvr$sc_prov_med$NSpred_fs   <-    array (  data = NA ,  dim = c ( nc$hs , length( c( 1,3,5:8 ) ) ) )
mvr$sc_prov_med$NSpred_os   <-    array (  data = NA ,  dim = c ( nc$hs , length( c( 1,3,5:8 ) ) ) )

mvr$sc_prov_good$NSpred_fs   <-    array (  data = NA ,  dim = c ( nc$hs , length( c( 1,3,5:8 ) ) ) )
mvr$sc_prov_good$NSpred_os   <-    array (  data = NA ,  dim = c ( nc$hs , length( c( 1,3,5:8 ) ) ) )


for ( ni in 1:length( c( 1,3,5:8 ) ) ){
  
  nn <-   c( 1,3,5:8 )[ ni ]
  
  bvr$mo          <-  lm ( ns_m[ , nn ]  ~  enu_m$tfec   )
  
  mvr$sc$mo       <-   lm ( ns_m[ , nn ]  ~  enu_m$tfec + ps_m$gini + ps_m$resrent +  ps_m$gov  )
  mvr$sc$mo_rob   <-   lm_robust ( ns_m[ , nn ]  ~  enu_m$tfec + ps_m$gini + ps_m$resrent +  ps_m$gov  , se_type = sw$HC )
  
  mvr$sc$a            <-  mvr$sc$mo$coefficients[1]
  
  mvr$sc$b1_tfec      <-  summary ( mvr$sc$mo )$coefficient[ 2 , 1 ]
  mvr$sc$b2_gini      <-  summary ( mvr$sc$mo )$coefficient[ 3 , 1 ]
  mvr$sc$b3_resrent   <-  summary ( mvr$sc$mo )$coefficient[ 4 , 1 ]
  mvr$sc$b4_gov       <-  summary ( mvr$sc$mo )$coefficient[ 5 , 1 ]

  
  bvr$NSpred_fs[ , ni ]  <-  bvr$mo$fitted.values 
  
  
  mvr$sc_prov_obs$NSpred_fs[ , ni ]    <-  mvr$sc$a +     mvr$sc$b1_tfec * enu_m$tfec   + 
      mvr$sc$b2_gini * ps_m$gini   +     mvr$sc$b3_resrent * ps_m$resrent    +      mvr$sc$b4_gov * ps_m$gov 
  
  
  mvr$sc_prov_med$NSpred_fs[ , ni ]    <-  mvr$sc$a +     mvr$sc$b1_tfec * enu_m$tfec   + 
    mvr$sc$b2_gini * sc$gini$median   +     mvr$sc$b3_resrent * sc$resrent$median    +      mvr$sc$b4_gov * sc$gov$median
  
  
  mvr$sc_prov_good$NSpred_fs[ , ni ]    <-  mvr$sc$a +     mvr$sc$b1_tfec * enu_m$tfec   + 
    mvr$sc$b2_gini * sc$gini$p10   +     mvr$sc$b3_resrent * sc$resrent$p10    +      mvr$sc$b4_gov * sc$gov$p90
  

  

  
  ####### back-trafos #######
  # this is based on current variables choice and implementation where all NS variables use sat-trafo but NS variables still with min/max type ( i.e. not yet standardised to positive upwards)
  
  if( tf$ns_m_tf_use[[nn]] == "lin" ){
    
    bvr$NSpred_os [ , ni ]  <-  ( bvr$NSpred_fs [ , ni ] * st$ns_m_sd[ nn ] ) + st$ns_m_means[ nn ]   -  tf$ns_m_offsets[[ nn ]] 
    
    
    mvr$sc_prov_obs$NSpred_os [ , ni ]  <-    ( mvr$sc_prov_obs$NSpred_fs [ , ni ] * st$ns_m_sd[ nn ] ) + st$ns_m_means[ nn ]  -  tf$ns_m_offsets[[ nn ]] 
    
    mvr$sc_prov_med$NSpred_os [ , ni ]  <-    ( mvr$sc_prov_med$NSpred_fs [ , ni ] * st$ns_m_sd[ nn ] ) + st$ns_m_means[ nn ] -  tf$ns_m_offsets[[ nn ]] 
    
    mvr$sc_prov_good$NSpred_os [ , ni ]  <-  ( mvr$sc_prov_good$NSpred_fs [ , ni ] * st$ns_m_sd[ nn ] ) + st$ns_m_means[ nn ] -  tf$ns_m_offsets[[ nn ]] 
    

  }
  
  if( tf$ns_m_tf_use[[nn]] == "log" ){
    
    bvr$NSpred_os [ , ni ]  <-   exp ( ( bvr$NSpred_fs [ , ni ] * st$ns_m_sd[ nn ] ) + st$ns_m_means[ nn ] )    -  tf$ns_m_offsets[[ nn ]] 
    
    mvr$sc_prov_obs$NSpred_os [ , ni ]  <-   exp ( ( mvr$sc_prov_obs$NSpred_fs [ , ni ] * st$ns_m_sd[ nn ] ) + st$ns_m_means[ nn ] )    -  tf$ns_m_offsets[[ nn ]] 
    
    mvr$sc_prov_med$NSpred_os [ , ni ]  <-   exp ( ( mvr$sc_prov_med$NSpred_fs [ , ni ] * st$ns_m_sd[ nn ] ) + st$ns_m_means[ nn ] ) -  tf$ns_m_offsets[[ nn ]] 
    
    mvr$sc_prov_good$NSpred_os [ , ni ]  <-   exp ( ( mvr$sc_prov_good$NSpred_fs [ , ni ] * st$ns_m_sd[ nn ] ) + st$ns_m_means[ nn ] ) -  tf$ns_m_offsets[[ nn ]] 
    
 
  }
  
  if( tf$ns_m_tf_use[[nn]] == "sat" ){
    
    bvr$NSpred_os [ , ni ]  <-   tf$ns_m_sat_val[[ nn ]]  -  tf$ns_m_offsets[[ nn ]]  +  g$ns_m_sign_for_sat_retrafo[ nn ]  *    
      exp ( ( bvr$NSpred_fs [ , ni ] * st$ns_m_sd[ nn ] ) + st$ns_m_means[ nn ] )    
 
    
    mvr$sc_prov_obs$NSpred_os [ , ni ]  <-   tf$ns_m_sat_val[[ nn ]]  -  tf$ns_m_offsets[[ nn ]]  +  g$ns_m_sign_for_sat_retrafo[ nn ]  *    
      exp ( ( mvr$sc_prov_obs$NSpred_fs [ , ni ] * st$ns_m_sd[ nn ] ) + st$ns_m_means[ nn ] )    
    
    mvr$sc_prov_med$NSpred_os [ , ni ]  <-   tf$ns_m_sat_val[[ nn ]]  -  tf$ns_m_offsets[[ nn ]]  +  g$ns_m_sign_for_sat_retrafo[ nn ]  *    
      exp ( ( mvr$sc_prov_med$NSpred_fs [ , ni ] * st$ns_m_sd[ nn ] ) + st$ns_m_means[ nn ] ) 
    
    mvr$sc_prov_good$NSpred_os [ , ni ]  <-   tf$ns_m_sat_val[[ nn ]]  -  tf$ns_m_offsets[[ nn ]]  +  g$ns_m_sign_for_sat_retrafo[ nn ]  *    
      exp ( ( mvr$sc_prov_good$NSpred_fs [ , ni ] * st$ns_m_sd[ nn ] ) + st$ns_m_means[ nn ] ) 
    
 
  }
  
   
}



a$ix_sortTFEC  <-  sort(  enu_m$tfec , index.return = TRUE )$ix


