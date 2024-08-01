#### set export file path ####

v_run <- "_main"        # name of the run
fp  <- "results/"       # file path for exports

setwd("/Users/supernaturval/Downloads/JV_PF_RCode_main")



#### preparatory steps ####

source( "JV_functions.R" ) # load my own functions

source( "p1_libs.R" ) # load packages and libraries   ### check if I can load them as default upon startup
source( "p1_ini_vars.R" ) # initiate list variables
source( "p1_switches.R" ) # set switches (analysis options, plot options)
source( "p1_pars.R" ) # set parameter choices
source("p1_cols.R") # load my colour palette for this paper



#### load and prepare data ###

source( "p1_load_data.R" ) # load and  pre-trim data  # changed "gini" to "gini_inv" as gini is flipped

source( "p1_flip_ns.R" )
source( "p1_flip_gini.R" )

source( "p1_clean_data.R" ) # fill key data gaps, remove outliers

source( "p1_offset.R" ) # offset variables for transformations

source( "p1_vars.R" ) # create new combined variables and sets

source( "p1_set_aux.R" ) # set auxiliary variables



#### create harmonised country sample  ####

source( "p1_hs.R" )  # create harmonised sample (hs) of countries to be used for main analysis



#### plot data distributions (as a check) ####

if ( sw$plot_data_clean_offset_hs_preTF   ) { source( "p1_vis_data.R" )  }



#### transform ENU, NS and PS variables to the form used for regressions (i.e. linearise) ####

source( "p1_enu_trafos.R" )

source( "p1_ns_trafos.R" )

source( "p1_ps_trafos.R" )



#### create data sets in the form to be used for main analysis

source( "p1_create_main_ds.R" )

source( "p1_def_ns_th.R" )    # define need satisfaction (sufficiency) thresholds



#### bivariate analysis and plots ####

source ( "p1_FIG_TAB_bvr.R")



#### moderation analysis and plots ####
# "mod_analysis" is where the calculations of the moderation analysis are happening; 
# "FIG_mod_multisp" is a version of those calculations for a subset of variables, with integrated plot code
# doing it in this order because the histogram figure ("FIG_mod_hist") requires calculations for all variables, not just the subset (bad coding...)

source ( "p1_FIG_mod_multisp.R" )

source ( "p1_mod_analysis.R" ) 

source( "p1_FIG_mod_hist.R" ) 


if(  sw$plot_additional_mod_hist ) { source ( "p1_SI_FIG_mod_spaghetti_4x3.R" ) }  



#### multi-provisioning analysis and plots ####

source( "p1_mvr_main.R") # version without jack-knife loop    

if( sw$mvr_run_jkd5 ) { source( "p1_mvr_main_jkd5.R") }        # version with jack-knife loop   

if( sw$mvr_run_jkd5 ) { source("p1_FIG_mvr_main_CI.R") }  else { source( "p1_FIG_mvr_main_noCI.R")   }  # for version with jack-knife loop, add confidence intervals (CI)    
