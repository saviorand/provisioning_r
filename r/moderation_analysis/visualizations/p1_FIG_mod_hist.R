p = list()

if ( sw$plot_mod_synth_NSpred_PSmaxsg_minus_PSminsg_norm_imprNS_hist_foreachPS_allNS_allENU ) {
  
  dir.create( paste( fp, "mod_analysis/" , sep="" )  , recursive = TRUE )
  

  efn <-  paste( fp , "mod_analysis/" , "mod_hist" , v_run , ".png", sep="" )
  
  ll$total_cases_per_ps <-   length( c(1,3,5:8) ) * 1 * nc$hs  
  
  ll$int <- 8.5  
  ll$limit <-  ceiling( 100 / ll$int ) * ll$int
  ll$break_seq <- seq( from = -1 * ll$limit  , to = 1 * ll$limit  , by = ll$int   )
  
  
  ll$ps_overall_effect_percrange_PSmaxminsg_signi  <- r_mod$NSpred_os_PSmaxsg_minus_PSminsg_percrange_sortENU [ , c(1,3,5:8) , ]
  
  ll$ps_overall_effect_percrange_PSmaxminsg_signi [ ! r_mod$flag_ENU_where_eff_ps_signif_sortENU[ , c(1,3,5:8) , ] ]  <- NA
   
  ll$ps_overall_effect_percrange_PSmaxminsg_signi_cut <- ll$ps_overall_effect_percrange_PSmaxminsg_signi
  
  ll$ps_overall_effect_percrange_PSmaxminsg_signi_cut[ 
    ( ll$ps_overall_effect_percrange_PSmaxminsg_signi < -100 )  |  ( ll$ps_overall_effect_percrange_PSmaxminsg_signi > 100 ) ]  <- NA
  
   
  for ( ppi in 1:length( c(1:8,11:14) ) ) {

    pp <-  c(1:8,11:14)[ppi]
    
    ll$freq_max[ppi] <-  max( hist( ll$ps_overall_effect_percrange_PSmaxminsg_signi_cut[ pp ,  , ], 
                              breaks = ll$break_seq , plot = FALSE )$counts , na.rm = TRUE)
  }
  
  ll$freq_perc_max <- max ( ll$freq_max / ll$total_cases_per_ps ) * 100
  
  

  png( filename= efn ,res = 300, width=16,height=20, unit = "cm" , type = "cairo" )   # activates png engine if figures are to be exported

  par(mfrow = c( 4,3), oma = c(0.7, 0.1, 0, 0.2), mar = c( 1.9 , 2.4, 1,0.95), mgp = c(2,0.4,0))  
  # bottom left top right
  
  
  for ( ppi in 1: length(c(1:8,11:14))  ) { 

    pp  <-   c(6,8,1,2,3,14,5,7,11,12,4,13)[ ppi ] # reshuffle 2  ## defines the order of variables in the subplots
    
    if ( ppi %in% c(10,11,12) ) {
      p$xlab = "Need satisfaction improvement [%]" 
      p$xaxt= "s"
    } else { 
      p$xlab = "" 
      p$xaxt = "n" }
    if ( ppi %in% c( 1,4,7,10) ) { 
      p$ylab = "Relative frequency [%]" 
      p$yaxt = "s"
    } else {
      p$ylab = "" 
      p$yaxt = "n"
    }
    
    
    ll$cPS_hist_obj_pos <- hist (  ll$ps_overall_effect_percrange_PSmaxminsg_signi_cut[ pp ,  , ][
                                    ll$ps_overall_effect_percrange_PSmaxminsg_signi_cut[ pp ,  , ] > 0 ] ,
                                   breaks = ll$break_seq , plot = FALSE ) 
    
    ll$cPS_hist_obj_neg <- hist (  ll$ps_overall_effect_percrange_PSmaxminsg_signi_cut[ pp ,  , ][
                                    ll$ps_overall_effect_percrange_PSmaxminsg_signi_cut[ pp ,  , ] < 0 ]  ,
                                   breaks = ll$break_seq , plot = FALSE ) 

    # divide counts by total number of cases per PS to get fraction of cases, expressed as percentage
    ll$cPS_hist_obj_pos$counts   <-  ( ll$cPS_hist_obj_pos$counts /  ll$total_cases_per_ps ) * 100
    ll$cPS_hist_obj_neg$counts   <-  ( ll$cPS_hist_obj_neg$counts /  ll$total_cases_per_ps ) * 100

    ll$check_sum[ ppi ] <- sum ( ll$cPS_hist_obj_pos$counts ) + sum ( ll$cPS_hist_obj_neg$counts )    # this must always be smaller than 100
    
    par(lwd=0.7)
    
    plot(  ll$cPS_hist_obj_pos ,   
           col= cols4$green , 
           # ylim = c ( 0, 1.025 * max( ll$freq_perc_max )  ) 
           bty = "n" , 
           type = "n",
           xlab = NA , ylab = NA , main = NA , axes =  F ,
           ylim = c ( 0.875, 27 ) ,
           xlim = c( -92.75 , 92.75 ) ,
           cex.main = 1.1, cex.axis = 0.9 , cex.lab = 0.7  )
    
    
    rect( -110 , -1 ,
          110 , 35 , 
          border = rgb(0.97,0.97,0.97,0.97) , col = rgb(0.97,0.97,0.97,0.97) )
    
    box()
    axis(side = 1, tck =-0.025 , labels = NA , cex.axis  = 0.8 )
    axis(side = 2, tck = -0.02 , labels = NA , cex.axis  = 0.8 )
    axis(side = 1, lwd = 0, line = -.15 , cex.axis  = 0.95)
    axis(side = 2, lwd = 0, line = 0, las = 1 , cex.axis  = 0.95 )
    mtext(side = 1, p$xlab , line = 1.5, cex = 0.75  , font.lab = 2)
    mtext(side = 2, p$ylab  , line = 1.5 , cex = 0.75 , font.lab = 2)
    mtext(side = 3, pn$ps_m[ pp ] , line = 0.1 , cex = 0.7 , font.main = 2 , font = 2)
    
    
    plot(  ll$cPS_hist_obj_pos ,   
           col= cols4$green , 
           add = TRUE)
    
     plot(  ll$cPS_hist_obj_neg ,   
           col= cols4$red , 
           add = TRUE )   
    
  
    
    lines( c(0,0) , c(0, 1 * max( ll$freq_perc_max )  )  ,
           # col = "grey55" , lty = 2 , lwd = 1.9 , add = TRUE )
    col = "blue" , lty = 2 , lwd = 1.4 , add = TRUE )


  
  }
  
  dev.off()

}




