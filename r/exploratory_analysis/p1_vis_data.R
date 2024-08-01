
#### checking input variables for outliers


dir.create( paste( fp , "check_data_and_outliers/stage1_clean_offset_hs_preTF/", sep="" )  , recursive = TRUE )



for ( vv in 1:nv$iv_all ){
  
  efn = paste( fp , "check_data_and_outliers/stage1_clean_offset_hs_preTF/",
              vv , "_", fn$iv_all[vv], ".png", sep="" ) # export file name
  
  
  png( filename= efn , width=1600,height=1200,res=144)
  
  par(mfrow = c(2,2))
  
  plot( log ( d_origZZ_fl_offset$tfec ) , d_origZZ_fl_offset[ , vv ] , col = "GREY" ,
        cex = 0.1 , xlab = "log TFEC" , ylab = paste( fn$iv_all[vv] , " ( + " , tf$iv_dcl_offsets_applied[vv] , " )" , sep="") ,
        main = paste( fn$iv_all[vv] , " vs log TFEC" , sep="") )
  text(log ( d_origZZ_fl_offset$tfec ) , d_origZZ_fl_offset[ , vv ] , g$isos_all , cex = 0.7 , col="RED2")
  text( log ( d_clo$tfec  ) , d_clo[ , vv ] , g$isos_all , cex = 0.7 , col="BLACK")
  text( log ( d_clo$tfec[cu] ) , d_clo[ cu , vv ] , g$isos_all[cu] , cex = 0.7 ,  col="GREEN")
  
  

  plot( log ( d_origZZ_fl_offset$tfec  ) , log( d_origZZ_fl_offset[ , vv ] ), col = "GREY", 
        cex = 0.1  , xlab = "log TFEC" ,
        ylab =  paste( "log ( " , fn$iv_all[vv] , " ( + " , tf$iv_dcl_offsets_applied[vv] , " ) )" , sep="")  ,
        main = paste( "log " , fn$iv_all[vv] , " vs log TFEC" , sep="") )
  text(log ( d_origZZ_fl_offset$tfec ) , log( d_origZZ_fl_offset[ , vv ] ) , g$isos_all , cex = 0.7 , col="RED2")
  text( log ( d_clo$tfec  ) , log ( d_clo[ , vv ]) , g$isos_all , cex = 0.7 , col="BLACK")
  text( log ( d_clo$tfec[cu] ) , log( d_clo[ cu , vv ]) , g$isos_all[cu] , cex = 0.7 , col="GREEN") 
  
  
  a$lin = seq( from =  min(d_origZZ_fl_offset[, vv ] , na.rm = TRUE) , to = max(d_origZZ_fl_offset[ , vv ] , na.rm = TRUE) ,
           length.out = 30)
  hist( d_origZZ_fl_offset[, vv ], breaks = a$lin  , col=rgb(1, 0, 0, 1),
        xlab = paste( fn$iv_all[vv] , " ( + " , tf$iv_dcl_offsets_applied[vv] , " )" , sep="") ,
        main = paste( "hist " , fn$iv_all[vv] , sep = "" ) )
  hist( d_clo[ , vv ] , breaks = a$lin ,  add = TRUE  ,col=rgb(0.1, 0.1, 0.1, 1) )
  hist( d_clo[ cu , vv ] , breaks = a$lin ,  add = TRUE  , col=rgb(0, 1, 0, 1) )
  
  
  a$h1  <- hist( log( d_origZZ_fl_offset[, vv ]), 30 , plot = FALSE )
  a$h2  <- hist( log( d_clo [, vv  ]), breaks = a$h1$breaks , plot = FALSE )
  a$h3  <- hist( log( d_clo[ cu , vv  ]), breaks = a$h1$breaks, plot = FALSE )
  
  plot ( a$h1 , col="RED",
           xlab = paste( "log ( " , fn$iv_all[vv] , " ( + " , tf$iv_dcl_offsets_applied[vv] , " ) )" , sep="")  ,
         main = paste( "hist log " , fn$iv_all[vv] , sep = "" ) )
  plot ( a$h2 , col=rgb(0.1, 0.1, 0.1, 1), add = TRUE )
  plot ( a$h3 , col="GREEN", add = TRUE )
  
  
  dev.off()
  
}






#### d-plus variables ( variables generated from input variables ) ####

for ( vv in (nv$iv_all+1):length(d_plus) ){
  
  efn = paste( fp , "check_data_and_outliers/stage1_clean_offset_hs_preTF/",
               vv , "_", names(d_plus)[vv], ".png", sep="" ) # export file name
  
  
  png( filename= efn , width=1600,height=1200,res=144)
  
  par(mfrow = c(2,2))
  
  plot( log ( d_plus$tfec ) , d_plus[ , vv ] , col = "GREY" ,
        cex = 0.1 , xlab = "log TFEC" , ylab =  names(d_plus)[vv] ,
        main = paste( names(d_plus)[vv] , " vs log TFEC" , sep="") )
  text(log ( d_plus$tfec ) , d_plus[ , vv ] , g$isos_all , cex = 0.7 , col="RED2")
  text( log ( d_plus$tfec  ) , d_plus[ , vv ] , g$isos_all , cex = 0.7 , col="BLACK")
  text( log ( d_plus$tfec[cu] ) , d_plus[ cu , vv ] , g$isos_all[cu] , cex = 0.7 ,  col="GREEN")
  
  
  
  plot( log ( d_plus$tfec  ) , log( d_plus[ , vv ] ), col = "GREY", 
        cex = 0.1  , xlab = "log TFEC" ,
        ylab =  paste( "log ( " , names(d_plus)[vv] ," )" , sep="")  ,
        main = paste( "log " , names(d_plus)[vv] , " vs log TFEC" , sep="") )
  text(log ( d_plus$tfec ) , log( d_plus[ , vv ] ) , g$isos_all , cex = 0.7 , col="RED2")
  text( log ( d_plus$tfec  ) , log ( d_plus[ , vv ]) , g$isos_all , cex = 0.7 , col="BLACK")
  text( log ( d_plus$tfec[cu] ) , log( d_plus[ cu , vv ]) , g$isos_all[cu] , cex = 0.7 , col="GREEN") 
  
  
  a$lin = seq( from =  min(d_plus[, vv ] , na.rm = TRUE) , to = max(d_plus[ , vv ] , na.rm = TRUE) ,
               length.out = 30)
  hist( d_plus[, vv ], breaks = a$lin  , col=rgb(1, 0, 0, 1),
        xlab = names(d_plus)[vv]  ,
        main = paste( "hist " , names(d_plus)[vv] , sep = "" ) )
  hist( d_plus[ , vv ] , breaks = a$lin ,  add = TRUE  ,col=rgb(0.1, 0.1, 0.1, 1) )
  hist( d_plus[ cu , vv ] , breaks = a$lin ,  add = TRUE  , col=rgb(0, 1, 0, 1) )
  
  a$h1  <- hist( log( d_plus[, vv ]), 30 , plot = FALSE )
  a$h2  <- hist( log( d_plus [, vv  ]), breaks = a$h1$breaks , plot = FALSE )
  a$h3  <- hist( log( d_plus[ cu , vv  ]), breaks = a$h1$breaks, plot = FALSE )
  
  plot ( a$h1 , col="RED",
         xlab = paste( "log ( " , names(d_plus)[vv] , " )" , sep="")  ,
         main = paste( "hist log " , names(d_plus)[vv] , sep = "" ) )
  plot ( a$h2 , col=rgb(0.1, 0.1, 0.1, 1), add = TRUE )
  plot ( a$h3 , col="GREEN", add = TRUE )
  
  
  dev.off()
  
}





