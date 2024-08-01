# jack-knife resampling analysis of alternative provisioning scenarios (jack-knife loop AFTER trafos etc.)

sc_jk5 <- list()  # scenarios
mvr_jk5 <- list()  # scenarios

sample_factor <- 10
cnt_to_remove <- 5
l_s  <-  nc$hs * sample_factor  # sample size for bootstrap / jackknife

mvr_jk5$sc_prov_obs$NSpred_fs <- array (  data = NA ,  dim = c ( nc$hs , length( c( 1,3,5:8 )) , l_s) ) 
mvr_jk5$sc_prov_med$NSpred_fs <- array (  data = NA ,  dim = c ( nc$hs , length( c( 1,3,5:8 )) , l_s  ) ) 
mvr_jk5$sc_prov_good$NSpred_fs <- array (  data = NA ,  dim = c ( nc$hs , length( c( 1,3,5:8 )) , l_s ) ) 


mvr_jk5$sc_prov_obs$NSpred_os <- array (  data = NA ,  dim = c ( nc$hs , length( c( 1,3,5:8 )) , l_s ) ) 
mvr_jk5$sc_prov_med$NSpred_os <- array (  data = NA ,  dim = c ( nc$hs , length( c( 1,3,5:8 )) , l_s ) ) 
mvr_jk5$sc_prov_good$NSpred_os <- array (  data = NA ,  dim = c ( nc$hs , length( c( 1,3,5:8 )) , l_s ) ) 



#### start jack-knife loop

cu_all <-   rep( TRUE , 1 ,nc$hs )

ind_all_cnt <- 1:nc$hs

for ( ss in 1: l_s ){ 

  cu_jk5  <- cu_all
  
  cc <-  floor ( ( (ss - 1) / sample_factor ) +1 )  # make sure each country gets removed at least the same number of times
  
  cu_jk5[ cc ]  <- FALSE   # remove country from current sample
  
  a$cnt_rm  <-  sample( ind_all_cnt[ cu_jk5 ] , cnt_to_remove - 1 , replace = FALSE  )  # randomly pick (cnt_to_remove - 1 = currently 5 - 1) other (!) countries to be removed, without replacement
  
  cu_jk5[ a$cnt_rm ] <- FALSE
  
# construct scenarios

  sc_jk5$gini$median <-  median( ps_m$gini[ cu_jk5 ] )
  sc_jk5$gini$obs    <-  ps_m$gini[ cu_jk5 ] 
  sc_jk5$gini$good    <-   sort ( ps_m$gini[ cu_jk5 ] )[ round ( 0.1 * (nc$hs - cnt_to_remove) )] # 10th percenile is "good" because gini is now flipped (positive upwards) but using sat trafo
  
  
  sc_jk5$resrent$median <-  median( ps_m$resrent[ cu_jk5 ] )
  sc_jk5$resrent$obs    <-  ps_m$resrent[ cu_jk5 ]  
  sc_jk5$resrent$good    <-   sort ( ps_m$resrent[ cu_jk5 ] )[ round ( 0.1 * (nc$hs - cnt_to_remove))]
  
  
  sc_jk5$gov$median <-  median( ps_m$gov[ cu_jk5 ] )
  sc_jk5$gov$obs    <-  ps_m$gov[ cu_jk5 ] 
  sc_jk5$gov$good    <-   sort ( ps_m$gov[ cu_jk5 ] )[ round ( 0.9 * (nc$hs - cnt_to_remove))]
  
  
  #### calculate predictions for scenarios
  
  for ( ni in 1:length( c( 1,3,5:8 ) ) ){
    
    nn <-   c( 1,3,5:8 )[ ni ]
    
    mvr_jk5$sc$mo       <-   lm ( ns_m[ cu_jk5 , nn ]  ~  enu_m$tfec[ cu_jk5 ] + ps_m$gini[ cu_jk5 ] + ps_m$resrent[ cu_jk5 ] +  ps_m$gov[ cu_jk5 ]  )
    mvr_jk5$sc$mo_rob   <-   lm_robust ( ns_m[ cu_jk5 , nn ]  ~  enu_m$tfec[ cu_jk5 ] + ps_m$gini[ cu_jk5 ] + ps_m$resrent[ cu_jk5 ] +  ps_m$gov[ cu_jk5 ]  , se_type = sw$HC )
    
    mvr_jk5$sc$a            <-  mvr_jk5$sc$mo$coefficients[1]
    
    mvr_jk5$sc$b1_tfec      <-  summary ( mvr_jk5$sc$mo )$coefficient[ 2 , 1 ]
    mvr_jk5$sc$b2_gini      <-  summary ( mvr_jk5$sc$mo )$coefficient[ 3 , 1 ]
    mvr_jk5$sc$b3_resrent   <-  summary ( mvr_jk5$sc$mo )$coefficient[ 4 , 1 ]
    mvr_jk5$sc$b4_gov       <-  summary ( mvr_jk5$sc$mo )$coefficient[ 5 , 1 ]
    

    # scenario observed provisioning: all predictors @ OBS
    mvr_jk5$sc_prov_obs$NSpred_fs[ cu_jk5 , ni , ss ]    <-  mvr_jk5$sc$a + 
      mvr_jk5$sc$b1_tfec * enu_m$tfec[ cu_jk5 ]   +   mvr_jk5$sc$b2_gini * ps_m$gini[ cu_jk5 ]   +
      mvr_jk5$sc$b3_resrent * ps_m$resrent[ cu_jk5 ]    +   mvr_jk5$sc$b4_gov * ps_m$gov[ cu_jk5 ] 
    
    # scenario median provisioning: enu obs, all PS vars @ median
    mvr_jk5$sc_prov_med$NSpred_fs[ cu_jk5 , ni , ss ]    <-  mvr_jk5$sc$a + mvr_jk5$sc$b1_tfec * enu_m$tfec[ cu_jk5 ] + 
      mvr_jk5$sc$b2_gini * sc_jk5$gini$median +
      mvr_jk5$sc$b3_resrent * sc_jk5$resrent$median + mvr_jk5$sc$b4_gov * sc_jk5$gov$median 
    
    
    # scenario good provisioning : enu obs , all PS vars @ p90/p10 ("good")
    mvr_jk5$sc_prov_good$NSpred_fs[ cu_jk5  , ni , ss ]    <-  mvr_jk5$sc$a + mvr_jk5$sc$b1_tfec * enu_m$tfec[ cu_jk5 ] + 
      mvr_jk5$sc$b2_gini *  sc_jk5$gini$good +
      mvr_jk5$sc$b3_resrent * sc_jk5$resrent$good +    mvr_jk5$sc$b4_gov  *  sc_jk5$gov$good
    

    
    ####### back-trafos #######
    # this is based on current variables choice and implementation where all NS variables use sat-trafo but NS variables still with min/max type ( i.e. not yet standardised to positive upwards )
    
    mvr_jk5$sc_prov_obs$NSpred_os [ cu_jk5  , ni , ss ]  <-   tf$ns_m_sat_val[[ nn ]]  -  tf$ns_m_offsets[[ nn ]]  +  g$ns_m_sign_for_sat_retrafo[ nn ]  *    
      exp ( ( mvr_jk5$sc_prov_obs$NSpred_fs[ cu_jk5  , ni , ss ] * st$ns_m_sd[ nn ] ) + st$ns_m_means[ nn ] ) 
    
    mvr_jk5$sc_prov_med$NSpred_os [ cu_jk5 , ni , ss ]  <-   tf$ns_m_sat_val[[ nn ]]  -  tf$ns_m_offsets[[ nn ]]  +  g$ns_m_sign_for_sat_retrafo[ nn ]  *    
      exp ( ( mvr_jk5$sc_prov_med$NSpred_fs[ cu_jk5  , ni , ss ] * st$ns_m_sd[ nn ] ) + st$ns_m_means[ nn ] ) 
    
    mvr_jk5$sc_prov_good$NSpred_os [ cu_jk5  , ni , ss ]  <-   tf$ns_m_sat_val[[ nn ]]  -  tf$ns_m_offsets[[ nn ]]  +  g$ns_m_sign_for_sat_retrafo[ nn ]  *    
      exp ( ( mvr_jk5$sc_prov_good$NSpred_fs[ cu_jk5  , ni , ss ] * st$ns_m_sd[ nn ] ) + st$ns_m_means[ nn ] ) 
    

  }

}


### produce lower and upper bounds of 95% confidence interval (2.5th and 97.5th percentile ) 

mvr_jk5$sc_prov_med$NSpred_os_CI95upper  <- array ( data = NA,  dim = c( nc$hs , length(c(1,3,5:8) ) ))
mvr_jk5$sc_prov_med$NSpred_os_CI95lower  <- array ( data = NA,  dim = c( nc$hs , length(c(1,3,5:8) ) ))

mvr_jk5$sc_prov_good$NSpred_os_CI95upper  <- array ( data = NA,  dim = c( nc$hs , length(c(1,3,5:8) ) ))
mvr_jk5$sc_prov_good$NSpred_os_CI95lower  <- array ( data = NA,  dim = c( nc$hs , length(c(1,3,5:8) ) ))

for ( cc in 1: nc$hs ){
  for ( ni in 1: length(c(1,3,5:8) )){
  
    if( g$ns_m_more_is_better[ c(1,3,5:8)[ni] ] ){
      
      mvr_jk5$sc_prov_med$NSpred_os_CI95upper[ cc , ni ] <-  sort( mvr_jk5$sc_prov_med$NSpred_os[ cc , ni , ] )[ round( 0.975 * ( length(sort( mvr_jk5$sc_prov_med$NSpred_os[ cc , ni , ] )) ) ) ] 
      mvr_jk5$sc_prov_med$NSpred_os_CI95lower[ cc , ni ] <-  sort( mvr_jk5$sc_prov_med$NSpred_os[ cc , ni , ] )[ round( 0.025 * ( length(sort( mvr_jk5$sc_prov_med$NSpred_os[ cc , ni , ] )) ) ) ] 
        
      mvr_jk5$sc_prov_good$NSpred_os_CI95upper[ cc , ni ] <-  sort( mvr_jk5$sc_prov_good$NSpred_os[ cc , ni , ]  )[ round( 0.975 * ( length(sort( mvr_jk5$sc_prov_good$NSpred_os[ cc , ni , ] )) ) ) ] 
      mvr_jk5$sc_prov_good$NSpred_os_CI95lower[ cc , ni ] <-  sort( mvr_jk5$sc_prov_good$NSpred_os[ cc , ni , ]  )[ round( 0.025 * ( length(sort( mvr_jk5$sc_prov_good$NSpred_os[ cc , ni , ] )) ) ) ] 
    
    } else {
      
      mvr_jk5$sc_prov_med$NSpred_os_CI95upper[ cc , ni ] <-  sort( mvr_jk5$sc_prov_med$NSpred_os[ cc , ni , ]  )[ round( 0.025 * ( length(sort( mvr_jk5$sc_prov_med$NSpred_os[ cc , ni , ] )) ) ) ] 
      mvr_jk5$sc_prov_med$NSpred_os_CI95lower[ cc , ni ] <-  sort( mvr_jk5$sc_prov_med$NSpred_os[ cc , ni , ]  )[ round( 0.975 * ( length(sort( mvr_jk5$sc_prov_med$NSpred_os[ cc , ni , ] )) ) ) ] 
      
      mvr_jk5$sc_prov_good$NSpred_os_CI95upper[ cc , ni ] <-  sort( mvr_jk5$sc_prov_good$NSpred_os[ cc , ni , ]  )[ round( 0.025 * ( length(sort( mvr_jk5$sc_prov_good$NSpred_os[ cc , ni , ] )) ) ) ] 
      mvr_jk5$sc_prov_good$NSpred_os_CI95lower[ cc , ni ] <-  sort( mvr_jk5$sc_prov_good$NSpred_os[ cc , ni , ]  )[ round( 0.975 * ( length(sort( mvr_jk5$sc_prov_good$NSpred_os[ cc , ni , ] )) ) ) ] 
      
    }
  }
}
  