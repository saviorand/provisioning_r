#### transform energy variables (log all) ####

# log-transforming enu vars is justified by their distributions, and is well established in energy research
# logged energy vars are reasonably close to normally distributed


enu_tf  <-  enu_p

colSums ( enu_p == 0 , na.rm = TRUE ,  dims = 1 )   # check for zeros

for ( ee in 1:nv$enu_all ){
  
  enu_tf[ , ee ]   <-   log ( enu_p[ , ee ] )
}


