
#### apply offsets such that all data are > 0  and such that data close to zero are not as massive outliers , to allow (testing) log transformations ####

colSums ( d_cl < 0.0000001 , na.rm = TRUE ,  dims = 1 )  

tf$iv_dcl_offsets_applied  <-  vector ( mode = "numeric" , length = nv$iv_all   )

names ( tf$iv_dcl_offsets_applied )   <-   fn$iv_all

### offset ns vars
tf$iv_dcl_offsets_applied$povhcrnl    <-   10        # offset to avoid MYS (Malaysia) being made an artifical outlier as artefact of log trafo
tf$iv_dcl_offsets_applied$doctors     <-   0.2       # offset to address issues of log at values close to zero, wihtout moving it too far from zero
tf$iv_dcl_offsets_applied$unempl      <-   2.5       # offset to address issues of log at values close to zero, which isolates a few countries

### offset ps vars
tf$iv_dcl_offsets_applied$resrent     <-   0.05     # otherwise issue with log(0)

tf$iv_dcl_offsets_applied$aid         <-   0.0005    # offset to address negative value (CHN) --> could just set CHN to NAN though?
tf$iv_dcl_offsets_applied$colddays    <-   3         #  offset to address issues of log at values very close to zero, wihtout moving it too far from zero

tf$iv_dcl_offsets_applied$gov         <-   3.5      # does NOT matter because lin trafo is currently used  # shifting scale from [-2.5  2.5] to [1  6]; that's no problem, it's a relative scale anyways
tf$iv_dcl_offsets_applied$democ       <-   3.5      # doesn't matter though, as sat trafo is used anyways  # shifting scale from [-2.5  2.5] to [1  6]; that's no problem, it's a relative scale anyways

tf$iv_dcl_offsets_applied$infra       <-   0         

tf$iv_dcl_offsets_applied$tmin        <-   15        # offset to allow taking log (after removing Greenland, i.e. new min is -11.1 )
tf$iv_dcl_offsets_applied$tmean       <-   7         # offset to allow taking log (after removing Greenland, i.e. new min is -11.1 )

tf$iv_dcl_offsets_applied$fdi         <-   0.3      

if ( ! sw$hardwire_lintrafos_growth_netimpo ) {

    tf$iv_dcl_offsets_applied$growth   <-  10 # for growth as growth rate from fit to 3 years of data (and hs sample) (Gurajat method)
  # growth has meaningful negative values that should be maintained; maybe best not to offset and to stick to identity transformation (so don't offset and use log! )original distribution is very close to normal)
  
}



#### create dataset with offsets
d_clo  <-  d_cl  # 

for  (  vv in  1 : nv$iv_all ) {
  
  d_clo[ , vv ]  <-  d_cl[ , vv ] +  tf$iv_dcl_offsets_applied[[  vv ]]
  
}

#### create an offset version of the original data set
d_origZZ_offset  <-  d_origZZ  # 

for  (  vv in  1 : nv$iv_all ) {
  
  d_origZZ_offset[ , vv ]  <-  d_origZZ[ , vv ] +  tf$iv_dcl_offsets_applied[[  vv ]]
  
}


#### create an offset version of the original data set with flippped but non-cleaned variable
d_origZZ_fl_offset  <-  d_origZZ_fl  # 

for  (  vv in  1 : nv$iv_all ) {
  
  d_origZZ_fl_offset[ , vv ]  <-  d_origZZ_fl[ , vv ] +  tf$iv_dcl_offsets_applied[[  vv ]]
  
}