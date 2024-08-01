
st <- list()  # standardisaton data (mean and sd), saved for back-trafos

isos  <- list()  ### change from using g$isos_all  to isos$all  in previous code


hs$hs_compatible_ps


#### define variables to be used for main analysis


ch$enu_m   <-  c( "tfec" , "trp" , "hhelec"  , "hhtot" )

ch$ns_m   <-  c( "hale"  , "doctors" , "fooddef" , "prot" ,  "water" , "sani" ,  "edubas"  , "povgap320" , "unempl" )

ch$ps_m   <-  c( "infra" , "elecacc" , "nsolfuel" , "growth" , "gini" , "gov" , "democ" ,  "hlthxpd" ,
                 "remitt" , "netimpo" , "trade" , "resrent" ,  "fdi" , "urbpop" , "actage" , "tmean"  )     ## may want to kick out gdp as well, if not used in main paper


ch$ps_hsv   <-  c( "infra" , "elecacc" , "hhelshare" , "nsolfuel" ,  "gdp" , "growth" , "gini" , "gov" , "democ" ,  "hlthxpd" ,
                  "remitt" , "netimpo" , "trade" , "impo" , "expo" , "resrent" ,  "fdi" , "urbpop" , "actage" , "tmean"  )

fn$enu_m   <-  ch$enu_m 
fn$ns_m    <-  ch$ns_m 
fn$ps_m    <-  ch$ps_m

fn$ps_hsv   <-  ch$ps_hsv


#### define plot names to be used for main analysis ####


pn$ns_m   <-   c("Healthy life expectancy", "# doctors" , "Sufficient nourishment" , "protein supply" , "Drinking water access" , "Safe sanitation access",
               "Basic education" , "Minimum income" , "unemployment"  )

pn$enu_m   <-  c( "Total final energy use" , "total transport energy use" ,
                  "household electricity use"  , "total household energy use" )


# pn$ps_m    <-  c( "infrastructure quality" , "electricity access" , "clean fuels access" , "recent gdp growth" , "income inequality" , 
#                  "governance effectiveness" , "democracy" ,  "gov. coverage of health expenditure" ,
#                  "personal remittances" , "net import value" , "trade orientation" , "resource rents" ,  "foreign direct investments" ,
#                  "urban population" , "population at active age" , "mean temperature"  )  

pn$ps_m    <-  c( "Trade & transport infrastructure" , "Electricity access" , "Access to clean fuels" , "Economic growth" , "Income equality" , 
                 "Public service quality" , "Democratic quality" ,  "Public health coverage" ,
                 "Personal remittances" , "Net imports" , "Trade penetration" , "Extractivism" ,  "Foreign direct investments" ,
                 "Urban population" , "Working age population" , "Mean temperature"  )     ## may want to kick out gdp as well, if not used in main paper

pn$ps_hsv  <-  c( "infrastructure quality" , "electricity access" , "elec. share in hh(+) energy" , "clean fuels access" , "gdp ppp" , "recent gdp growth" , "income inequality" , 
                 "governance effectiveness" , "democracy" ,  "gov. coverage of health expenditure" ,
                 "personal remittances" , "net import value" , "trade openness" , "imports value" , "exports value" , "resource rents" ,  "foreign direct investments" ,
                 "urban population" , "pop. at active age" , "mean temperature"  )     ## may want to kick out gdp as well, if not used in main paper


pn$enu_units_m  <- c("  [ GJ / cap. ]" , "  [ GJ / cap. ]" , "  [ GJ / cap. ]" , "  [ GJ / cap. ]" )
# pn$ns_units_m  <- c("  [ years ]" , "  [ # / cap ]" , "  [ % of pop. ]" , "  [ calories per day / cap. ]" , "  [ % of pop. ]" , 
#                     "  [ % of pop. ]" , "  [ % of pop. ]" , "  [ mean rel. shortfall (%) ]"  , " % of workforce")

pn$ns_units_m  <- c(" [ yrs ]" , "  [ # / cap ]" , " [ % ]" , "  [ calories per day / cap. ]" , " [ % ]" , 
                    " [ % ]" , " [ score ]" , " [ % ]"  , " [ % ]")

#### create data sets for main (regression) analysis ( _m )

enu_m    <-    enu_tf[ cu , ]                          
ns_m     <-    ns_tf[ cu , ch$ns_m ]
ps_m     <-    ps_tf[ cu ,  ch$ps_m  ]

ps_hsv    <-   ps_tf[ cu , ch$ps_hsv  ]   # hsv = harmonised sample (PS-)variables: those ps variables that have data for all hs countries (in addition to ps_m, this includes impo, expo, hhelshare)


nv$enu_m  <-  length ( ch$enu_m )
nv$ns_m   <-  length ( ch$ns_m )
nv$ps_m   <-  length ( ch$ps_m )

nv$ps_hsv   <-  length ( ch$ps_hsv )


#### create corresponding data sets in original space (_os) for comparison with regression predictions in main analysis (4m) ####

enu_os4m   <-   d_cl[  cu , ch$enu_m ]
ns_os4m    <-   d_cl[  cu , ch$ns_m ]
ps_os4m    <-   d_cl_plus [  cu , ch$ps_m ] # d_cl_plus data set contains the "plus" variables, but all variables are in original (but cleaned) form, i.e. non-offset

ps_os_hsv    <-   d_cl_plus[  cu , ch$ps_hsv ]  # d_cl_plus data set contains the "plus" variables, but all variables are in original (but cleaned) form, i.e. non-offset



### standardise main data sets and save mean and standard deviations for back-trafos


st$enu_m_means  <-  vector ( mode = "numeric"  , length = nv$enu_m   )
st$enu_m_sd     <-  vector ( mode = "numeric"  , length = nv$enu_m   )
names( st$enu_m_means)  <- fn$enu_m
names( st$enu_m_sd)  <- fn$enu_m


st$ns_m_means   <-  vector ( mode = "numeric"  , length = nv$ns_m   )
st$ns_m_sd      <-  vector ( mode = "numeric"  , length = nv$ns_m    )
names( st$ns_m_means)  <- fn$ns_m
names( st$ns_m_sd)     <- fn$ns_m

st$ps_m_means   <-  vector ( mode = "numeric"  , length = nv$ps_m   )
st$ps_m_sd      <-  vector ( mode = "numeric"  , length = nv$ps_m    )
names( st$ps_m_means)  <- fn$ps_m
names( st$ps_m_sd)     <- fn$ps_m

st$ps_hsv_means   <-  vector ( mode = "numeric"  , length = nv$ps_hsv   )
st$ps_hsv_sd      <-  vector ( mode = "numeric"  , length = nv$ps_hsv    )
names( st$ps_hsv_means)  <- fn$ps_hsv
names( st$ps_hsv_sd)     <- fn$ps_hsv



for ( ee in 1: length( enu_m ) ){  
  st$enu_m_means[ ee ]  <-    mean ( enu_m[ , ee ] )
  st$enu_m_sd[  ee ]    <-    sd   ( enu_m[ , ee ] )
  
  enu_m [ , ee ]      <-    std_JV( enu_m[ , ee ] ) }


for ( nn in 1: length( ns_m ) ) { 
  st$ns_m_means[ nn ]  <-    mean ( ns_m[ , nn ] )
  st$ns_m_sd[ nn ]     <-    sd   ( ns_m[ , nn ] )
  
  ns_m  [ , nn ]     <-    std_JV( ns_m[ , nn ] ) }


for ( pp in 1: length( ps_m ) ) {
  st$ps_m_means[ pp ]  <-  mean ( ps_m[ , pp] )
  st$ps_m_sd[ pp ]     <-  sd   ( ps_m[ , pp ] )
  
  ps_m  [ , pp ]     <-  std_JV( ps_m[ , pp ] ) }


for ( pp in 1: length( ps_hsv ) ) {
  st$ps_hsv_means[ pp ]  <-  mean ( ps_hsv[ , pp] )
  st$ps_hsv_sd[ pp ]     <-  sd   ( ps_hsv[ , pp ] )
  
  ps_hsv  [ , pp ]     <-  std_JV( ps_hsv[ , pp ] ) }



#### special "standardisation" for growth and netimpo (because of meaningful zero-line): only standardise the variance, WITHOUT shifting their mean (so as to conserve the zero-crossings)

if ( sw$hardwire_lintrafos_growth_netimpo ) {

  ps_m$growth     <-   ps_tf$growth[cu] / st$ps_m_sd[ "growth"] 
  ps_hsv$growth   <-   ps_tf$growth[cu] / st$ps_hsv_sd[ "growth"] 
  
  ps_m$netimpo   <-   ps_tf$netimpo[cu] / st$ps_m_sd[ "netimpo"] 
  ps_hsv$netimpo   <-   ps_tf$netimpo[cu] / st$ps_hsv_sd[ "netimpo"] 

}


#### create auxiliary variables and axes vectors for main data sets


tf$ns_m_tf_use      <-  tf$ns_tf_use[ ch$ns_m ]
tf$ns_m_sat_val      <-  tf$ns_sat_val[ ch$ns_m ]

tf$ps_m_tf_use      <-  tf$ps_tf_use[ ch$ps_m ]
tf$ps_m_sat_val     <-  tf$ps_sat_val[ ch$ps_m ]
tf$ps_hsv_tf_use    <-  tf$ps_tf_use[ ch$ps_hsv ]
tf$ps_hsv_sat_val   <-  tf$ps_sat_val[ ch$ps_hsv ]

tf$ns_m_offsets       <-    tf$dplus_offsets[ ch$ns_m ]
tf$ps_m_offsets       <-    tf$dplus_offsets[ ch$ps_m ]
tf$ps_hsv_offsets     <-    tf$dplus_offsets[ ch$ps_hsv ]


g$ns_m_more_is_better  <-  g$ns_more_is_better[ ch$ns_m ]
g$ns_m_sign_for_sat_retrafo   <-  g$ns_sign_for_sat_retrafo[ ch$ns_m ]

g$leg_pos_backtf_m  <-  g$leg_pos_backtf[ ch$ns_m ] 

nc$hs      <-  hs$N

isos$hs    <-  g$isos_all[ cu ]



#### create sign-normaliser vector for marginal effects (independent, e.g. b_ENU  or condional, e.g. dNS/dENU  ) of variables onto NS  #### 
## in such a way that a marginal effect gets a positive sign if higher values of the predictor variable/term lead to "improved NS" ##
## (where "improved" is sensitive to the direction of "good" for each NS) ##


g$sign_normaliser_imprNS_for_effects_on_ns_m     <-  rep(  1 , nv$ns_m ) 
g$sign_normaliser_imprNS_for_effects_on_ns_m[ ( tf$ns_m_tf_use == "sat" ) & ( g$ns_m_more_is_better) ]  <-   -  1
g$sign_normaliser_imprNS_for_effects_on_ns_m[ !  g$ns_m_more_is_better ]  <-   ( -  1 ) * g$sign_normaliser_imprNS_for_effects_on_ns_m [ !  g$ns_m_more_is_better ]
names(g$sign_normaliser_imprNS_for_effects_on_ns_m ) <- fn$ns_m

g$sign_normaliser_imprNS_for_NSpred_fs_PSmaxsg_minus_PSminsg_m   <-      rep(  1 , nv$ns_m ) 
g$sign_normaliser_imprNS_for_NSpred_fs_PSmaxsg_minus_PSminsg_m[ ( tf$ns_m_tf_use == "sat" ) ]       <-  - 1
#  note the sign on the min-type sat trafo IS ALREADY inverted in the trafo, so wrong to "double-invert" it!
