a$cols <- c( "black" , "dodgerblue","green3" , "red")
a$cols_txt <- c( "black" , "dodgerblue","springgreen4" , "red")
a$ltys <- c( 1,2,2,2  )
a$lwds <- c( 4,4.85,4,3.5)
a$lwds_medline <- c ( 1.5 , 1.5 ,1.5 , 1.5 )
a$txt_ycoords  <- c( 47   , 80  , 52   , 10  , 23 , 53  )
a$txt_xcoords  <- 280
a$ymins <- c ( 46 , 52.5  ,50 ,  7  , 19   , 51 ) 
a$ymaxs <- c ( 72 , 101 , 101 , 100 , 93 , 101  )  



r_bvr <- list()

r_bvr$NSpred_ENUonly_sortENU  <-  array ( data = NA , dim = c( nc$hs , g$l_n  ))

r <- list()

r$r_enu_p75_minus_med_to_med         <-   array ( data = NA , dim = c( 6    ) ) 
r$r_enu_p75_minus_med_to_med_frommin         <-   array ( data = NA , dim = c( 6    ) ) 

r$r_ns_pred_p75_minus_med_to_med      <-   array ( data = NA , dim = c( 6   ) ) 
r$r_ns_pred_p75_minus_med_to_med_frommin      <-   array ( data = NA , dim = c( 6   ) ) 

r$ns_enu_r2_adj  <-   array ( data = NA , dim = c( 6 , nv$enu_m   ) ) 


par ( mfrow = c(3,2))


if ( sw$exp_bvr_ns_enu  ) {
  
  
  dir.create( paste( fp, "bvr_ns_enu/" , sep="" )  , recursive = TRUE )
  
  
  efn <-  paste( fp,"bvr_ns_enu/bvr_ns_enu_27GJ_R2_greybg" , v_run , "_300dpi", ".png", sep="" )
  
  png( filename= efn ,res = 300, width=16,height=12, unit = "cm" , type = "cairo" )   # activates png engine if figures are to be exported
  
  par(  mfrow = c( 3,2) , oma = c( 1.25 , 0, 0 , 0 ), mar = c( 1.6 , 2.4, 0.9, 1), mgp = c(2.6,0.75,0))  
  
  
  for ( ni in 1:6 ){
    
    nn <-  c(1,3,5:8)[ ni ]
    
    
    if ( ni > 4 ){  ll$xlab  = "Total final energy use [ GJ / cap ]" } else {  ll$xlab  = ""  }
    

    #### --- calculations --- ####
    
    # for ( ee in 1:4 ) {
      for ( ee in which( fn$enu_all == "tfec" ) ) {
      
        a$ind_sortENU   <-  sort( enu_os4m[ , ee ] ,  index.return = TRUE )
        
        a$enu_p75_os  <-  sort( enu_os4m[ , ee ])[0.75 * nc$hs ]

        a$enu_p75_fs  <-  sort( enu_m[ , ee ])[0.75 * nc$hs ]
        
        ll$mo <-  lm ( ns_m[ , nn ] ~ enu_m[ , ee ] )
        
        ll$a  <- ll$mo$coefficients[1]
        ll$b  <- ll$mo$coefficients[2]
        
        if ( tf$ns_m_tf_use[[ nn ]] == "lin"  ){
          
          ll$ns_pred_os  <-    ( ll$mo$fitted.values * st$ns_m_sd[ nn ] ) + st$ns_m_means[ nn ]  -  tf$ns_m_offsets[[ nn ]]  
          
          ll$ns_pred_os_for_enu_p75  <-  ( ( ll$a + ll$b * a$enu_p75_fs ) * st$ns_m_sd[ nn ] ) + st$ns_m_means[ nn ]  - tf$ns_m_offsets[[ nn ]]
          
        }
        
        if ( tf$ns_m_tf_use[[ nn ]] == "log"  ){
          
          ll$ns_pred_os  <-   exp ( ( ll$mo$fitted.values * st$ns_m_sd[ nn ] ) + st$ns_m_means[ nn ] ) -  tf$ns_m_offsets[[ nn ]]  
          
          ll$ns_pred_os_for_enu_p75  <- exp ( ( ( ll$a + ll$b * a$enu_p75_fs ) * st$ns_m_sd[ nn ] ) + st$ns_m_means[ nn ] ) - tf$ns_m_offsets[[ nn ]]
          
        }
        
        if ( tf$ns_m_tf_use[[ nn ]] == "sat"  ){
          
          ll$ns_pred_os  <-  tf$ns_m_sat_val[[ nn ]]  -  tf$ns_m_offsets[[ nn ]]  +  g$ns_m_sign_for_sat_retrafo[ nn ]  *    
            exp ( ( ll$mo$fitted.values * st$ns_m_sd[ nn ] ) + st$ns_m_means[ nn ] )    
          
          ll$ns_pred_os_for_enu_p75  <-  tf$ns_m_sat_val[[ nn ]]  -  tf$ns_m_offsets[[ nn ]]  +  g$ns_m_sign_for_sat_retrafo[ nn ]  *    
            exp ( ( ( ll$a + ll$b * a$enu_p75_fs ) * st$ns_m_sd[ nn ] ) + st$ns_m_means[ nn ] )  
        }
        
        
        r$ns_enu_r2_adj[ ni  ]  <- summary( ll$mo  )$adj.r.squared
        
        r$r_enu_p75_minus_med_to_med[ ni  ]  <-  (  a$enu_p75_os  - median( enu_os4m[ , ee ]) ) / median( enu_os4m[ , ee ] )
        r$r_enu_p75_minus_med_to_med_frommin[ ni  ]  <-  (  a$enu_p75_os  - median( enu_os4m[ , ee ]) ) / ( median( enu_os4m[ , ee ] ) - min(enu_os4m[ , ee ]) )
        

        if( g$ns_m_more_is_better[ nn ] ){ 
          r$r_ns_pred_p75_minus_med_to_med[ ni  ]  <-  ( ll$ns_pred_os_for_enu_p75   - median( ll$ns_pred_os ) ) / median( ll$ns_pred_os )
          r$r_ns_pred_p75_minus_med_to_med_frommin [ ni ]  <-  ( ll$ns_pred_os_for_enu_p75   - median( ll$ns_pred_os ) ) / ( median( ll$ns_pred_os ) - min ( ns_os4m[ , nn ] ) )
          
        }
        if( ! g$ns_m_more_is_better[ nn ] ){   # not really needed anymore
          r$r_ns_pred_p75_minus_med_to_med[ ni ]  <- ( ( 100 - ll$ns_pred_os_for_enu_p75 ) - (100 - median( ll$ns_pred_os )) ) / ( 100 -  median( ll$ns_pred_os ) )
        }

        
        r_bvr$NSpred_ENUonly_sortENU[  , ni ] <-  ll$ns_pred_os[ a$ind_sortENU$ix ]
        
      }
    

    
    #### --- plots --- ####
    
    for ( ee in 1 ) {
      
      plot (enu_os4m[,1], ns_os4m[ , nn ] , 
            col = "grey68", pch = 21,  cex = 0.65 ,
            bg = "grey92",
            xlim = c( 12,335),
            ylim = c( a$ymins[ni] , a$ymaxs[ni]),
            xlab = NA , ylab = NA , axes =  F ,
            cex.main  = 2 , cex.lab = 1.42 , cex.axis = 1.5 , font.lab  = 2 )
      
      rect( 1 , 0 ,
            395 , 105 , 
            border = rgb(0.97,0.97,0.97,0.97) , col = rgb(0.97,0.97,0.97,0.97) )
      
      points (enu_os4m[,1], ns_os4m[ , nn ] , 
            col = "grey68", pch = 21,  cex = 0.65 ,
            bg = "grey92" )
      
      box()
      axis(side = 1, tck = -.01, labels = NA , cex.axis  = 0.8 )
      axis(side = 2, tck = -.015, labels = NA , cex.axis  = 0.8 )
      axis(side = 1, lwd = 0, line = -.6 , cex.axis  = 0.8)
      axis(side = 2, lwd = 0, line = -.4, las = 1 , cex.axis  = 0.8 )
      mtext(side = 1, ll$xlab , line = 1.6, cex = 0.7  , font.lab = 2)
      mtext(side = 2, paste( pn$ns_m[ nn ], pn$ns_units_m[ nn ] , sep="" ) , line = 1.4 , cex = 0.65 , font.lab = 2)
      
      
      a$ind_sortENU   <-  sort( enu_os4m[ , ee ] ,  index.return = TRUE )
      

      ll$mo <-  lm ( ns_m[ , nn ] ~ enu_m[ , ee ] )
      
      ll$a  <- ll$mo$coefficients[1]
      ll$b  <- ll$mo$coefficients[2]
  
      if ( tf$ns_m_tf_use[[ nn ]] == "lin"  ){
        ll$ns_pred_os  <-    ( ll$mo$fitted.values * st$ns_m_sd[ nn ] ) + st$ns_m_means[ nn ]  -  tf$ns_m_offsets[[ nn ]]  
      }
      
      if ( tf$ns_m_tf_use[[ nn ]] == "log"  ){
        ll$ns_pred_os  <-   exp ( ( ll$mo$fitted.values * st$ns_m_sd[ nn ] ) + st$ns_m_means[ nn ] ) -  tf$ns_m_offsets[[ nn ]]  
      }
      
      if ( tf$ns_m_tf_use[[ nn ]] == "sat"  ){
        ll$ns_pred_os  <-  tf$ns_m_sat_val[[ nn ]]  -  tf$ns_m_offsets[[ nn ]]  +  g$ns_m_sign_for_sat_retrafo[ nn ]  *    
          exp ( ( ll$mo$fitted.values * st$ns_m_sd[ nn ] ) + st$ns_m_means[ nn ] )    
      }
      
      lines(  c(0.1, 380) , c( 1, 1) * ch$ns_th[[ fn$ns_m[ nn ] ]] ,  col = cols4$lightblue_alt  , lwd = 2 , lty = 3 )
      lines(  c(1, 1) * 27 , c( -2, 110) ,  col =  cols4$lightgreen_alt4  , lwd = 2.65 , lty = 3 )
      }
    
    lines( enu_os4m[ a$ind_sortENU$ix ,ee], 
           ll$ns_pred_os[ a$ind_sortENU$ix ] , col = "black" , lwd = 1.4 , type = "l"  )
    
    text( 306 , a$txt_ycoords[ ni ] , paste( "R2_adj = ", round( (summary(ll$mo))$adj.r.squared , 2), sep=""), 
          cex = 0.82 , col = "grey43" )
  
    
    
    if ( ni == 2 ){
      legend( "bottomright" ,
              legend = c ( "observed data",
                           "model fit (energy only)",
                           "max. sustainable energy use",
                           "need satisfaction threshold"),
              col = c( "grey64","black", cols4$lightgreen_alt4  , cols4$lightblue_alt ),
              pt.bg =  c ("grey82" , NA , NA ,NA ),
              pch = c( 21 , NA ,NA , NA ),
              lty = c ( 0 , 1 , 3 ,  3 ),
              lwd = c ( 1.2 , 1.6 , 2.5 , 2 )  ,
              pt.cex = c ( 0.8, 1 , 1, 1 ),
              y.intersp = 1,
              cex = 0.9,
              bg= "grey92"
      )
      
    }
  }
  dev.off()
}

r_enu_in_perc  <- r$r_enu_p75_minus_med_to_med[] * 100
r_enu_in_perc_frommin  <- r$r_enu_p75_minus_med_to_med_frommin[] * 100

r_ns_in_perc <- r$r_ns_pred_p75_minus_med_to_med * 100
r_ns_in_perc_frommin <- r$r_ns_pred_p75_minus_med_to_med_frommin * 100

ns_response  <- r$r_ns_pred_p75_minus_med_to_med / r$r_enu_p75_minus_med_to_med


if ( sw$exp_table_bvr_ns_enu){

  dir.create( paste( fp, "bvr_ns_enu/" , sep="" )  , recursive = TRUE )
  
  write.csv(  r_enu_in_perc , file=paste(fp, "bvr_ns_enu/r_enu_p75_minus_p50_to_p50_in_perc", v_run, ".csv",sep="") )
  write.csv(  r_enu_in_perc_frommin , file=paste(fp, "bvr_ns_enu/r_enu_p75_minus_p50_to_p50_in_perc_frommin", v_run, ".csv",sep="") )
  
  write.csv(  t(r_ns_in_perc) , file=paste(fp, "bvr_ns_enu/r_ns_predp75enu_minus_predp50enu_to_predp50enu_in_perc", v_run, ".csv",sep="") )
  write.csv(  t(r_ns_in_perc_frommin) , file=paste(fp, "bvr_ns_enu/r_ns_predp75enu_minus_predp50enu_to_predp50enu_in_perc_frommin", v_run, ".csv",sep="") )
  
    write.csv(  t(ns_response) , file=paste(fp, "bvr_ns_enu/perc_ns_increase_per_perc_enu_increase_ave_from_p50_to_p75", v_run, ".csv",sep="") )
  write.csv(  t(r$ns_enu_r2_adj ) , file=paste(fp, "bvr_ns_enu/bvr_ns_enu_adj_r2", v_run, ".csv",sep="") )
}

