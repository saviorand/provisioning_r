

if ( sw$plot_mod_effects_scatter  ) {
  
  dir.create( paste( fp, "SI/mod/" , sep="" )  , recursive = TRUE )
  
  p <- list()
  
  # efn <-  paste( fp,"mod_analysis/synthesis/" , "mod_synth_NSpred_PSmaxsg_minus_PSminsg_norm_imprNS_hist_multisp_nounempl_nodoctors_prefinal.png", sep="" )
  # efn <-  paste( fp,"SI/mod/" , "mod_scatter_PSmaxsg_minus_PSminsg_percrange" , v_run , "_tfeconly_enuOSlog" , ".png", sep="" )
  efn <-  paste( fp,"SI/mod/" , "mod_spaghetti_4x3_1000dpi" , ".png", sep="" )
  
  ll$total_cases_per_ps <-   length( c(1,3,5:8) ) * nv$enu_m * nc$hs  
  
  a$enu_ind <- which( fn$enu_all == "tfec")  # 1 = tfec; can use 1:4 for all enu variables
  
  ll$ps_overall_effect_percrange_PSmaxminsg_signi  <- r_mod$NSpred_os_PSmaxsg_minus_PSminsg_percrange_sortENU [ , c(1,3,5:8) ,  ]
  
  ll$ps_overall_effect_percrange_PSmaxminsg_signi [ ! r_mod$flag_ENU_where_eff_ps_signif_sortENU[ , c(1,3,5:8),  ] ]  <- NA
  
  ll$ps_overall_effect_percrange_PSmaxminsg_signi_cut <- ll$ps_overall_effect_percrange_PSmaxminsg_signi
  
  ll$ps_overall_effect_percrange_PSmaxminsg_signi_cut[ 
    ( ll$ps_overall_effect_percrange_PSmaxminsg_signi < -100 )  |  ( ll$ps_overall_effect_percrange_PSmaxminsg_signi > 100 ) ]  <- NA
  
  
  
  
  ### create auxiliary matrix of energy values for plots
  
  a$enu_os_mat_sortENU <- array( data = NA , dim = c( 6 ,  nc$hs) )
  
  for ( nn in 1:6 ){ a$enu_os_mat_sortENU[ nn,  ] <- sort( enu_os4m[ , a$enu_ind ] ) }
  
  
  
  
  # png( filename= efn , width=2000,height=1200,res=144)  # activates png engine if figures are to be exported
  png( filename= efn ,res = 1000, width=16,height=20, unit = "cm" , type = "cairo" )   # activates png engine if figures are to be exported
  
  
  # par(mfrow = c( 3,4), oma = c(0, 0, 0, 0), mar = c( 3.25 , 3, 0.75,0.5), mgp = c(2,0.25,0))  
  
  par(mfrow = c( 4,3), oma = c(0.7, 0.5, 0, 0), mar = c( 1.9 , 2.6, 1.3,0.7), mgp = c(2,0.4,0))  
  
  # bottom, left, top, right
  
  
  # for ( ppi in 1: length(c(1:8,11:14))  ) { 
  for ( ppi in 1: 12  ) { 
    

    pp  <-   c(6,8,1,2,3,14,5,7,11,12,4,13)[ ppi ] # reshuffle 2
    
    # c(6,7,8,5,4,14,12,11,13,2,3,1)
    
    if ( ppi %in% c(10,11,12) ) {
      p$xlab = "Total final energy use [ GJ/cap ]" 
      p$xaxt= "s"
    } else { 
      p$xlab = "" 
      p$xaxt = "n" }
    if ( ppi %in% c( 1,4,7,10) ) { 
      p$ylab = "Need satisfaction improvement [%]" 
      p$yaxt = "s"
    } else {
      p$ylab = "" 
      p$yaxt = "n"
    }
    
  

    

    
    if ( any( sum( ll$ps_overall_effect_percrange_PSmaxminsg_signi_cut[ pp ,  , ] > 0,na.rm=TRUE ) )) {
      
      plot( a$enu_os_mat_sortENU[ ll$ps_overall_effect_percrange_PSmaxminsg_signi_cut[ pp ,  , ] > 0 ] ,
            ll$ps_overall_effect_percrange_PSmaxminsg_signi_cut[ pp ,  , ][
              ll$ps_overall_effect_percrange_PSmaxminsg_signi_cut[ pp ,  , ] > 0 ]  , col = cols4$green ,
            ylim=c(-96,96), xlim = c(5,320),
            bty = "n" , 
            type = "n",
            xlab = NA , ylab = NA , main = NA , axes =  F , log= "x" )
      
            # xlab = p$xlab , ylab = p$ylab , main = pn$ps_u[ pp ] ,
            # cex = 0.45, cex.main = 1.5, cex.axis = 1.5 , cex.lab = 1.475  , bty = "l" ,log="x" )
            # 
      
      
      
      # plot(  ll$cPS_hist_obj_pos ,   
      #        col= cols4$green , 
      #        # ylim = c ( 0, 1.025 * max( ll$freq_perc_max )  ) 
      #        bty = "n" , 
      #        type = "n",
      #        xlab = NA , ylab = NA , main = NA , axes =  F ,
      #        ylim = c ( 0.875, 27 ) ,
      #        xlim = c( -92.75 , 92.75 ) ,
      #        cex.main = 1.1, cex.axis = 0.9 , cex.lab = 0.7  )
      
      
      
      rect( 1 ,   -110 ,
            400 , 110 , 
            border = rgb(0.97,0.97,0.97,0.97) , col = rgb(0.97,0.97,0.97,0.97) )
      
      box()
      axis(side = 1, tck =-0.02 , labels = NA , cex.axis  = 0.8 )
      axis(side = 2, tck = -0.02 , labels = NA , cex.axis  = 0.8 )
      axis(side = 1, lwd = 0, line = -.15 , cex.axis  = 0.875)
      axis(side = 2, lwd = 0, line = 0, las = 1 , cex.axis  = 0.875 )
      mtext(side = 1, p$xlab , line = 1.5, cex = 0.675  , font.lab = 2)
      mtext(side = 2, p$ylab  , line = 2 , cex = 0.675 , font.lab = 2)
      mtext(side = 3, pn$ps_m[ pp ] , line = 0 , cex = 0.65 , font.main = 2 , font = 2)
      
      
      points( a$enu_os_mat_sortENU[ ll$ps_overall_effect_percrange_PSmaxminsg_signi_cut[ pp ,  , ] > 0 ] ,
            ll$ps_overall_effect_percrange_PSmaxminsg_signi_cut[ pp ,  , ][
              ll$ps_overall_effect_percrange_PSmaxminsg_signi_cut[ pp ,  , ] > 0 ]  , col = cols4$green ,
            cex = 0.45 , lwd = 0.3 )
      
      
      
      
      
      if ( any( sum(ll$ps_overall_effect_percrange_PSmaxminsg_signi_cut[ pp ,  , ] < 0,na.rm=TRUE ) )) {
        
        points( a$enu_os_mat_sortENU[ ll$ps_overall_effect_percrange_PSmaxminsg_signi_cut[ pp ,  , ] < 0 ] ,
                ll$ps_overall_effect_percrange_PSmaxminsg_signi_cut[ pp ,  , ][
                  ll$ps_overall_effect_percrange_PSmaxminsg_signi_cut[ pp ,  , ] < 0 ]  , col = cols4$red ,
                ylim=c(-98,98), xlim = c(10,340), add = TRUE ,
                cex = 0.45  , lwd = 0.3 )
        
      }
    }
    
    
    if ( ! any( sum(ll$ps_overall_effect_percrange_PSmaxminsg_signi_cut[ pp , , ] > 0,na.rm=TRUE ) ) & 
         any( sum(ll$ps_overall_effect_percrange_PSmaxminsg_signi_cut[ pp , , ] < 0,na.rm=TRUE ) ) ) {
      
      plot( a$enu_os_mat_sortENU[ ll$ps_overall_effect_percrange_PSmaxminsg_signi_cut[ pp , , ] < 0 ] ,
            ll$ps_overall_effect_percrange_PSmaxminsg_signi_cut[ pp , , ][
              ll$ps_overall_effect_percrange_PSmaxminsg_signi_cut[ pp , , ] < 0 ]  ,col = cols4$red ,
            ylim=c(-96,96), xlim = c(10,340), 
            bty = "n" , 
            type = "n",
            xlab = NA , ylab = NA , main = NA , axes =  F , log= "x" )
      
            # xlab = p$xlab , ylab = p$ylab , main = pn$ps_u[ pp ]  ,
            # cex = 0.45, cex.main = 1.5, cex.axis = 1.5 , cex.lab = 1.475  , bty = "l" ,log="x" )
            # 
      
      rect( 1 ,   -110 ,
            400 , 110 , 
            border = rgb(0.97,0.97,0.97,0.97) , col = rgb(0.97,0.97,0.97,0.97) )
      
      box()
      axis(side = 1, tck =-0.02 , labels = NA , cex.axis  = 0.8 )
      axis(side = 2, tck = -0.02 , labels = NA , cex.axis  = 0.8 )
      axis(side = 1, lwd = 0, line = -.2 , cex.axis  = 0.875)
      axis(side = 2, lwd = 0, line = 0, las = 1 , cex.axis  = 0.875 )
      mtext(side = 1, p$xlab , line = 1.5, cex = 0.675  , font.lab = 2)
      mtext(side = 2, p$ylab  , line = 2 , cex = 0.675 , font.lab = 2)
      mtext(side = 3, pn$ps_m[ pp ] , line = 0, cex = 0.65 , font.main = 2 , font = 2)
      
      
      points( a$enu_os_mat_sortENU[ ll$ps_overall_effect_percrange_PSmaxminsg_signi_cut[ pp , , ] < 0 ] ,
             ll$ps_overall_effect_percrange_PSmaxminsg_signi_cut[ pp , , ][
               ll$ps_overall_effect_percrange_PSmaxminsg_signi_cut[ pp , , ] < 0 ]  , col = cols4$red ,
             cex = 0.45 , lwd = 0.3 )
      
      
      
      
    }
    
    
    lines( c(1,400) , c(0, 0 )    ,  col = "black" , lty = 2 , lwd = 1.2 , add = TRUE )
    lines( c(0,0) , c(-120,120)  ,  col = "black" , lty = 2 , lwd = 1.2 , add = TRUE )
    
    
  }
  
  dev.off()
  
}


