

d_plus  <-  d_clo

rownames(d_plus)   <-   g$isos_all

#### calculate a few derived variables: use non-offset variables (d_clo) for these

d_plus$trade       <-   d_cl$expo + d_cl$impo  

d_plus$netimpo     <-   d_cl$impo  -  d_cl$expo

d_plus$hhelshare   <-   d_cl$hhelec / d_cl$hhtot  # share of electricity in total energy in hh + cops

d_plus$dddiscomf   <-   d_cl$colddays + d_cl$hotdays  # degree days of thermal discomfort (cold / hot)


#### apply and update offsets variable accordingly


tf$dplus_offsets  <-   tf$iv_dcl_offsets_applied

tf$dplus_offsets$trade        <-   0
tf$dplus_offsets$hhelshare    <-   0
tf$dplus_offsets$dddiscomf    <-   0

if ( sw$hardwire_lintrafos_growth_netimpo ) {   tf$dplus_offsets$netimpo   <-  0 
  } else {                                      tf$dplus_offsets$netimpo   <-  30  } # this is based on the hs


d_plus$netimpo  <- d_plus$netimpo  + tf$dplus_offsets$netimpo


#### also create a clean but non-offset data set with the (hs-compatible) "plus" variables, for original space ####


d_cl_plus  <-  d_cl    # hsv = harmo

d_cl_plus$trade       <-     d_cl$expo   +  d_cl$impo  
d_cl_plus$netimpo     <-     d_cl$impo   -  d_cl$expo
d_cl_plus$hhelshare   <-     d_cl$hhelec /  d_cl$hhtot  # share of electricity in total energy in hh + cops



fn$all_dplus  <-  names( d_plus )
  
  
nv$all_dplus  <-  length ( d_plus )


ix$enu_all  <-    1  :   4
ix$ns_all   <-    5  :   14
ix$ps_all   <-   15  :   nv$all_dplus


nv$enu_all  <-  length ( ix$enu_all  )
nv$ns_all   <-  length ( ix$ns_all  )
nv$ps_all   <-  length ( ix$ps_all  )


fn$enu_all  <-  fn$all_dplus[  ix$enu_all  ]
fn$ns_all   <-  fn$all_dplus[  ix$ns_all  ]
fn$ps_all   <-  fn$all_dplus[  ix$ps_all  ]

#### make data-subpools for enu, ns, and ps vars for easier handling ( _p = pool )

enu_p  <-  d_plus[ , ix$enu_all ]
ns_p   <-  d_plus[ , ix$ns_all ]
ps_p   <-  d_plus[ , ix$ps_all ]

rownames(enu_p) <-  g$isos_all
rownames(ns_p)  <-  g$isos_all
rownames(ps_p)  <-  g$isos_all

g$ns_more_is_better <- c ( TRUE , TRUE ,FALSE , TRUE , TRUE, TRUE , TRUE , TRUE , FALSE , TRUE  ) # after flipping povgap & fooddef (not using unemply, povhcnl and doctors anyways); for each NS, sets whether high values are seen as "positive" (TRUE) or "negative" (FALSE)


names ( g$ns_more_is_better )  <- names ( ns_p )
  
g$ns_sign_for_sat_retrafo   <-  -1 * g$ns_more_is_better + ( ! g$ns_more_is_better * 1 )  #  to give NS_pred  =  NS_sat[nn]  +  g$ns_sign_for_sat_retrafo[nn] * exp ( a + b_j X_j )



# pn$all  <-  c ( "enu_TFEC_total" , "enu_TRP_total" , "enu_HHCOPS_ELEC", 
#               "ns_PROTEIN_SUPPLY" , "ns_hLIFE_EXP", "ns_FOOD_DEF",
#               "ns_POV_GAP_310" , "ns_HOSP_BEDS" , "ns_BASIC_EDU", "ns_WATER_ACC", 
#               "ns_SANITATION" , 
#               "ps_ROADKM" , "ps_GINI" , "ps_GDP_PPP"  , "ps_GOVERNANCE" , 
#               "ps_DEMOCRACY", "ps_GROWTH_last3yr" , "ps_HEALTH_XPD_GOV2TOT" , 
#               "ps_GOV_XPD_EDU2TOT" , "ps_SOCIAL_SECU_COVER" , "ps_UNION_DENSITY" , 
#               "ps_CORPORATE_POWER", "ps_FDI" , "ps_VEHICLES" , "ps_PROFIT_TAX" ,
#               "ps_POP_URBAN" , "ps_POP_CITIES1m" , "ps_POP_AGE1564" ,"ps_RESOURCE_RENT",
#               "ps_ELEC_ACC" , "ps_NSOLFUEL_ACC", "ps_RAIL_PKMPC" , "ps_INFRA_QUAL" , 
#               "ps_TAX_REV"  , "ps_AID" , "ps_REMITTANCE" , "ps_SOC_CONTRIB",
#               "ps_EXPO" , "ps_IMPO" , "ps_HDD" , "ps_CDD")