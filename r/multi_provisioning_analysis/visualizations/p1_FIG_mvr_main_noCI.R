
a$ix_sortTFEC  <-  sort(  enu_m$tfec , index.return = TRUE )$ix

a$ymins <- c ( 54 , 76  , 66 ,  20  , 25   , 74 ) 
a$ymaxs <- c ( 72 , 101 , 100.5 , 100.5 , 91, 101  )


sw$mvr_exp_scenario_predictions <-  TRUE

if ( sw$mvr_exp_scenario_predictions  ) {
  
  
  dir.create( paste( fp, "mvr_gini_resrent_gov/" , sep="" )  , recursive = TRUE )
  
  efn <-  paste( fp,"mvr_gini_resrent_gov/mvr_gini_resrent_gov_goodp90_nogates_noCI", v_run , ".png", sep="" )
  
  png( filename= efn , width=1600,height=1700,res=144)   # activates png engine if figures are to be exported
  
  par(  mfrow = c( 3,2) , oma = c( 0.5, 0, 0,0), mar = c( 3 , 4.8, 1.5, 1), mgp = c(2,0.4,0))  
  
  for ( ni in 1: length ( c( 1,3,5:8 ) ) ) {
    
    nn <-  c( 1,3,5:8 )[ ni ]
    
    if ( ni > 4 ){  ll$xlab  = paste( pn$enu_m[ 1 ] , pn$enu_units_m[1] , sep=""  ) } else {  ll$xlab  = ""  }

    plot(  enu_os4m$tfec , mvr$sc_prov_obs$NSpred_os[ , ni] ,  log = "x" , 
           col = cols$purple  , pch = 3 ,  cex  = 0.9, lwd = 1.1 ,
           # main = pn$ns_m[ nn ] , 
           xlab = ll$xlab , ylab =  paste( pn$ns_m[ nn ], pn$ns_units_m[ nn ] , sep="" ) ,
           cex.main  = 2 , cex.lab = 1.65 , cex.axis = 1.5 ,
           ylim = c( a$ymins[ ni ] , a$ymaxs[ ni ] ) )
    
    
    lines (  enu_os4m$tfec[ a$ix_sortTFEC ] ,   mvr$sc_prov_good$NSpred_os[ a$ix_sortTFEC , ni ] , col  = cols$green  , add = TRUE , lty = 2 , lwd=4 )

    lines (  enu_os4m$tfec[ a$ix_sortTFEC ] ,   mvr$sc_prov_med$NSpred_os[ a$ix_sortTFEC , ni ] , col= cols$orange , add = TRUE , lty = 2 , lwd=3.8 )
    
    lines(  c(0.001, 400) , c( 1, 1) * ch$ns_th[[ fn$ns_m[ nn ] ]] ,  col = "turquoise2" , lwd = 2 , lty = 3 )
    
    if ( ni == 2 ){
      legend( "bottomright" ,
              legend = c ( "de facto provisioning" ,
                           "average provisioning",
                           "good provisioning" ,
                           "need satisfaction threshold" ),
               col = c( cols$purple , cols$orange , 
                       cols$green, "turquoise2" ),
              pch = c(   3 , NA , NA , NA ),
              lty = c (   0 , 2  ,2,  3 ),
              lwd = c (   1.7 , 2.7 , 2.7 ,   2 ),
              pt.cex = c (   1.4 , 1 , 1 ,1 ),
              y.intersp = c(  1.1 ,  1.1 , 1.1 , 1.1 ),
              cex = 1.44 
      )
      
    }
    

  }
  
  
  dev.off()
  
}
