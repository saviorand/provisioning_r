



### JV code

#### ----------------------------------------------------- ####
####  --------------  computation switches  ------------  ####
#### ----------------------------------------------------- ####

sw$use_ns_sat_factor   <- FALSE   # if true, calculates sat values based on the factor; if false, uses manually defined value
sw$use_ps_sat_factor   <- FALSE   # if true, calculates sat values based on the factor; if false, uses manually defined value

sw$mod_use_ps_hsv                      <-  FALSE    # if TRUE , use all hs-compatible variables in moderation analysis; if FALSE, use only "main" ones (i.e. excluding impo, expo, hhelshare)

sw$hardwire_lintrafos_growth_netimpo   <-  FALSE  # has been TRUE before v5 (or so)
sw$mvr_run_jkd5  <-  TRUE

sw$HC  <-  "HC2"

#### ----------------------------------------------------- ####
#### -------------- figure & table switches ------------- ####
#### ----------------------------------------------------- ####


### --- data orig, clean, hs ---- ###

sw$plot_data_clean_offset_hs_preTF    <-  FALSE # hist and scatter with tfec for all variables,  d_origZZ_offset  vs  d_clo   vs   d_clo[ cu ]


### --- trafos --- ###

sw$exp_NStrafo_plots_resQQ            <-   FALSE
sw$exp_NStrafo_plots_fit              <-   FALSE


### --- bivariate fits --- ###

sw$exp_bvr_ns_enu <-  TRUE

sw$exp_table_bvr_ns_enu  <- TRUE   # table


### --- moderation analysis --- ###

sw$plot_mod_MargEff                    <-   FALSE

sw$plot_mod_NSpred_fENU_mPS_pred_at_PSminmaxsignif_PSobs_PSperccontours_btf_plotos   <-   FALSE


### --- moderation analysis: synthesis --- ###

sw$plot_mod_synth_NSpred_PSmaxsg_minus_PSminsg_norm_imprNS_hist_foreachPS_allNS_allENU   <-  TRUE

sw$plot_mod_multisp  <-  TRUE


### --- multiple regression analysis --- ####

sw$mvr_exp_regr_tables  <- TRUE

sw$mvr_exp_scenario_predictions  <- TRUE


# sw$mvr_exp_scenario_pred_anomalies  <- FALSE 





#### ----------------------------------------------------- ####
#### --------- SI calculations and plots------------------ ####
#### ----------------------------------------------------- ####

sw$plot_mod_effects_scatter  <- TRUE

sw$calc_and_plot_additional_mvrs  <- FALSE

sw$plot_additional_mod_multisps <- TRUE
  
sw$plot_additional_mod_hist  <-  TRUE
