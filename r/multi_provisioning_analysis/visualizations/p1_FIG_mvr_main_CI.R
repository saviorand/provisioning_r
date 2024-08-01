
a$ix_sortTFEC  <-  sort(  enu_m$tfec , index.return = TRUE )$ix

a$ymins <- c ( 54 , 71  , 63 ,  12  , 26   , 64 )   # v1 mvr based
a$ymaxs <- c ( 71.5 , 100.5 , 100.5 , 100.5 , 90.5 , 100.5  )



sw$mvr_exp_scenario_predictions <-  TRUE

if ( sw$mvr_exp_scenario_predictions  ) {
  
  
  dir.create( paste( fp, "mvr_gini_resrent_gov/" , sep="" )  , recursive = TRUE )
  
  efn <-  paste( fp,"mvr_gini_resrent_gov/mvr_gini_resrent_gov_CI_27GJ_500dpi_withbvr.png", sep="" )

  png( filename= efn ,res = 500, width=16,height=16.5, unit = "cm" , type = "cairo" , antialias = "subpixel")   # activates png engine if figures are to be exported
  
  par(  mfrow = c( 3,2) , oma = c( 1.25 , 0, 0 , 0 ), mar = c( 1.6 , 3, 0.9, 0.7), mgp = c(2.6,0.75,0))  
  # bottom left top right
  
  
  for ( ni in 1: length ( c( 1,3,5:8 ) ) ) {
    
    nn <-  c( 1,3,5:8 )[ ni ]
    
    if ( ni > 4 ){  ll$xlab  = "Total final energy use [ GJ / cap ]" } else {  ll$xlab  = ""  }
    

    plot(  enu_os4m$tfec , mvr$sc_prov_obs$NSpred_os[ , ni] , log = "x" , 
           pch = 3 , col = cols4$pink ,  cex = 0.575, lwd = 0.33  ,
           ylim = c( a$ymins[ni] , a$ymaxs[ni]),
           xlim = c( 5.2 , 320 ),
           xlab = NA , ylab = NA , axes =  F )

    box()
    axis(side = 1, tck = -.0175, labels = NA , cex.axis  = 0.8 )
    axis(side = 2, tck = -.0175, labels = NA , cex.axis  = 0.8 )
    axis(side = 1, lwd = 0, line = -.6 , cex.axis  = 0.95)
    axis(side = 2, lwd = 0, line = -.35, las = 1 , cex.axis  = 0.95 )
    mtext(side = 1, ll$xlab , line = 1.6, cex = 0.8  , font.lab = 2)
    mtext(side = 2, paste( pn$ns_m[ nn ], pn$ns_units_m[ nn ] , sep="" ) , line = 1.65 , cex = 0.8 , font.lab = 2)
    
        
    lim <- par("usr")
    
    rect( 4.3 , lim[3]+0.25 ,
          386 , lim[4]-0.25 , 
          border = rgb(0.97,0.97,0.97,0.97) , col = rgb(0.96,0.96,0.96,0.96) )
    
    
    
    lines(  c(0.1, 400) , c( 1, 1) * ch$ns_th[[ fn$ns_m[ nn ] ]] ,  col = cols4$lightblue_alt  , lwd = 1.3 , lty = 3 )
    
    lines(  c(1, 1) * 27 , c( -2, 110) ,  col =  cols4$lightgreen_alt4  , lwd = 1.5 , lty = 3 )
    
    
    # good provisioning 
    polygon( c( enu_os4m$tfec[ a$ix_sortTFEC ],rev(enu_os4m$tfec[ a$ix_sortTFEC ])), 
             c( mvr_jk5$sc_prov_good$NSpred_os_CI95lower[ a$ix_sortTFEC , ni ] , rev(mvr_jk5$sc_prov_good$NSpred_os_CI95upper[ a$ix_sortTFEC , ni ]) ),
             col =  cols4$green_trans ,  border = cols4$green_trans2 , lwd = 0.8  )

    
    # average (median) provisioning
    polygon( c( enu_os4m$tfec[ a$ix_sortTFEC ],rev(enu_os4m$tfec[ a$ix_sortTFEC ])), 
             c( mvr_jk5$sc_prov_med$NSpred_os_CI95lower[ a$ix_sortTFEC , ni ] , rev(mvr_jk5$sc_prov_med$NSpred_os_CI95upper[ a$ix_sortTFEC , ni ]) ),
             col = cols4$orange_trans ,  border = cols4$orange_trans2   , lwd = 0.8    )

    points(  enu_os4m$tfec , mvr$sc_prov_obs$NSpred_os[ , ni] ,  pch = 3 , col = cols4$pink ,  cex = 0.65, lwd = 0.4  )
    
    
    lines (  enu_os4m$tfec[ a$ix_sortTFEC ] ,   mvr$sc_prov_good$NSpred_os[ a$ix_sortTFEC , ni ] , col  = cols$green  , add = TRUE , lty = 2 , lwd=3.7 )
    
    lines (  enu_os4m$tfec[ a$ix_sortTFEC ] ,   mvr$sc_prov_med$NSpred_os[ a$ix_sortTFEC , ni ] , col= cols$orange , add = TRUE , lty = 2 , lwd=3.5 )
    
    lines ( ll$enu_os_sortENU  ,  r_bvr$NSpred_ENUonly_sortENU[ , ni ] , col="black"  , lty =  1 , lwd = 0.35 )     
  
    
    if ( ni == 2){
      legend( "bottomright" ,
              legend = c ( "status-quo provisioning" ,
                           "median provisioning",
                           "jointly beneficial provisioning" ,
                           "not considering provisioning",
                           "need satisfaction threshold",
                           "max. sustainable energy use" ),
              col = c( cols4$pink , cols4$orange ,  
                       cols4$green,  "black" , cols4$lightblue_alt , cols4$lightgreen_alt4 ),
              pch = c(   3 , NA , NA , NA , NA , NA , NA ),
              lty = c (   0 , 2 , 2,  1 , 3 , 3 ),
              lwd = c (   0.7 , 1.9 ,  1.9 ,    0.6 , 1.8 , 1.8 ),
              pt.cex = c (  0.8 ,   1  , 1 ,  1 , 1, 1),
              y.intersp = 1.05 ,
              cex = 0.985,
              bg = rgb( 0.91 , 0.91, 0.91 , 0.91)
      )
    }


    }
    
   
  dev.off()
  
}



