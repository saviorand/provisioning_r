
# in future, could have this immediately after cleaning the data and generating the trade data


#### preliminary harmonised sample (hs)

hs <- list()


# hs$pool  <-  array ( data = NA, dim = c ( nc$all , 4 + 9 + 5  ) ) # 4 enu vars, 9 ns vars (all but povhcrnl), 5 ps vars
hs$pool  <-  array ( data = NA, dim = c ( nc$all ,19  ) ) # 4 enu vars, 9 ns vars (all but povhcrnl), 5 ps vars

hs$pool[ , 1 ]  <-   d_cl$tfec
# hs$pool[ , 2 ]  <-    #  rep (999, 237 ) #  d_cl$trp
# hs$pool[ , 3 ]  <-    #  rep (999, 237 ) # d_cl$hhelec
# hs$pool[ , 4 ]  <-    #  rep (999, 237 )  # d_cl$hhtot

# hs$pool [ , 5]  <-   d_cl$prot
hs$pool [ , 2]  <-   d_cl$hale
hs$pool [ , 3]  <-   d_cl$edubas
hs$pool [ , 4]  <-   d_cl$sani
hs$pool [ , 5]  <-   d_cl$water
hs$pool [ , 6]  <-   d_cl$povgap320
hs$pool [ , 7]  <-   d_cl$fooddef
# hs$pool [ , 12]  <-   d_cl$doctors
# hs$pool [ , 13]  <-   d_cl$unempl

hs$pool [ , 8 ]  <- ps_p$gov
hs$pool [ , 9 ]  <- ps_p$resrent
hs$pool [ , 10 ]  <- ps_p$trade
hs$pool [ , 11 ]  <- ps_p$gini
hs$pool [ , 12 ]  <- ps_p$elecacc

hs$pool[ , 13 ]  <- ps_p$democ
hs$pool[ , 14 ]  <- ps_p$growth
hs$pool[ , 15 ]  <- ps_p$hlthxpd
hs$pool[ , 16 ]  <- ps_p$urbpop
hs$pool[ , 17 ]  <- ps_p$nsolfuel
hs$pool[ , 18 ]  <- ps_p$infra
hs$pool[ , 19 ]  <- ps_p$fdi

# hs$pool[ , 23 ]  <- ps_p$remitt
# hs$pool[ , 24 ]  <- ps_p$tmean
# hs$pool[ , 35 ]  <- ps_p$netimpo
# hs$pool[ , 26 ]  <- ps_p$actage

# hs$pool[ , 27 ]  <- ps_p$gdp

fn$hs_pool_vars  <-  c(  "tfec" , "hale"  , "edubas" , "sani" , "water" ,
                          "povgap320" , "fooddef" ,"gov" , "resrent" , "trade" , "gini" , "elecacc" ,
                         "democ" , "growth" , "hlthxpd" , "urbpop" , "nsolfuel" , "infra" , "fdi")
# 
colnames( hs$pool)   <-  fn$hs_pool_vars

hs$pool_flag_nans  <- is.na ( hs$pool )
hs$pool_rows_with_any_nans  <-   apply ( hs$pool_flag_nans, 1 , function(k) any( k %in%  hs$pool_flag_nans[k,] ) )

cu  <-   ! hs$pool_rows_with_any_nans

#### manually remove countries from harmonised sample ####



cu[ g$isos_all == "LUX" ]  <- FALSE  # kick out Luxembourg as it is so non-representative  (this code line is strictly speaking redundant as already done by removing data point in FDI in p1_clean_data.R )


hs$N  <- sum ( cu )

# relevant countries NOT INCLUDED in sample: Cuba, Bhutan, Haiti, Saudi Arabia
# all LiLi case study countries included :)

# note povgap data for Japan and edubas data for Nigeria slightly problematic because manually included from another data source
# Bhutan (BTN) lacking energy data (also in up-to-date IEA data): no chance of including
# Cuba (CUB) lacking data in povgap and gini (neither SWIID nor WDI2018 have gini data for cuba.... give up on it?!) could somehow find it, but too dodgy?

# which of the countries in Goldemberg's corner (as per Lamb et al. 2014) are included in my hs?
# ALB CRI URY ECU PAN LKA PER NIC COL PRY VNM TUN THA BRA GEO EGY ARM MAR GTM (i.e. 19 out of 20 = 95% )
# which aren't?
# AZE ( lacking data on povgap320 and gini ) 

# which of the countries in Goldemberg's corner (as per Lamb 2016 !!) are included in my hs?
# ALB ARM BRA COL CRI ECU EGY SLV (El Salvador) GEO GTM HND (Honduras)  IDN (Indonesia) MAR PRY PER LKA TUN URY   ( 18 out of 19 = almost 95%)
# which aren't?
# KHM (Cambodia: lacking data on povgap 320)

# which of the countries in Goldemberg's corner (as per Steinberger et al. 2012 !!) are included in my hs?
# CRI CHL URY ALB PAN ECU TUN ARG CHN TUR EGY PER MAR GEO BRA ARM PHL PRY LKA NIC COL VNM  ( 22 out of 22  = 100 % )




hs$pool_number_of_nans_per_country  <- vector ( mode = "numeric" , length = nc$all   )

for ( cc in  1:nc$all  ){
  
  hs$pool_number_of_nans_per_country [ cc ]  <-  sum ( hs$pool_flag_nans[ cc , ] )
  
}


hs$pool_number_of_nans_per_country_antihs  <- vector ( mode = "numeric" , length = nc$all - hs$N  )


hs$pool_antihsonly_flag_nans  <- hs$pool_flag_nans[ ! cu , ]
 
names( hs$pool_number_of_nans_per_country_antihs )  <- g$isos_all[ !cu ]
rownames( hs$pool_antihsonly_flag_nans )<- g$isos_all[ !cu ]

for ( cc in  1: (nc$all - hs$N) ){
  
  hs$pool_number_of_nans_per_country_antihs [ cc ]  <-  sum ( hs$pool_antihsonly_flag_nans[ cc , ] )
  
}

# missing data in only 1 variable for hs: 
# MDA (Moldova): fooddef
# YEM (Yemen): trade (not included in WDI)
# KHM (Cambodia: lacking data on povgap 320)
# give up on these for now


#### How many PS variables have data for all countries in the hs?

hs$hs_compatible_ps  <-  vector ( mode = "logical" , length = nv$ps_all )
hs$nans_per_ps  <-  vector ( mode = "logical" , length = nv$ps_all )

names ( hs$hs_compatible_ps ) <- names( ps_p )
names ( hs$nans_per_ps ) <- names( ps_p )

for  ( pp in 1:nv$ps_all ) { 
  hs$hs_compatible_ps [ pp ]     <-    ! any ( is.na ( ps_p[ cu, pp] ) )
  hs$nans_per_ps [ pp ]    <- sum ( is.na ( ps_p[ cu, pp] ) )
  
  hs$nans_per_ps_antihsonly[ pp ]   <-  sum  ( )
}

### PS vars with data for all countries in hs: gini gdp gov democ growth hlthxpd fdi urbpop actage resrent elecacc nsolfuiel trade netimpo hhelshare



## climate variables (hdd,cdd) lack data for COG CYP ISL ISR MLT MUS (of hs sample)

## cars (vehicles per capita) variable lacks data for ARM BIH CIV COG MNE MNG NER NPL

g$isos_all[cu][ which( is.na( ps_p$cars[cu] )  )]

g$isos_all[cu][ which( is.na( ps_p$infra[cu] )  )]


sum(a$pop[cu]) /  a$pop[ g$isos_all == "WLD" ]
sum(a$gdp_ext[cu], na.rm= TRUE) /  a$gdp_ext[ g$isos_all == "WLD" ]
sum(a$tfec_ext[cu], na.rm= TRUE) / a$tfec_ext[ g$isos_all == "WLD" ]

sum(a$pop[cu]) /  sum(a$pop[ ! g$isos_all == "WLD" ] , na.rm = TRUE)
sum(a$gdp_ext[cu],na.rm = TRUE) /  sum(a$gdp_ext[ ! g$isos_all == "WLD" ] , na.rm = TRUE)
sum(a$tfec_ext[cu] ) /  sum(a$tfec_ext[ ! g$isos_all == "WLD" ] , na.rm = TRUE)

