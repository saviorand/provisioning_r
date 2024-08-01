
#### load data ####

d_orig  <-  read.csv( "JV_p1_inputdata_preprocessed.csv" ) # leave this variable entirely untouched


#### trim ZZ isos ####

d_origZZ  <-  d_orig [ ! c( d_orig$isos == "ZZKO" | d_orig$isos == "ZZZA" | d_orig$isos == "ZZZZ" | d_orig$isos == "ZZ_NaN") , ] 
# delete these as they are multiply assingned (ZZZA, ZZZZ, ZZ_NaN ) or likely to have weird / no data (ZZKO = Kosovo)


#### split off isos from data ####

g$isos_all  <-  d_origZZ$isos

rownames( d_origZZ )  <-  g$isos_all


#### split off auxiliary extensive variables (pop,gdp,tfec) from other data ####

d_origZZ$aux_gdp_ext[ which( g$isos_all == "MMR")]  <-  211527658640 # data gap, filled from WB WITS

a$pop        <-   d_origZZ$aux_pop
a$gdp_ext    <-   d_origZZ$aux_gdp_ext
a$tfec_ext   <-   d_origZZ$aux_tfec_ext

d_origZZ    <-  d_origZZ[ , 2: ( length( d_orig) -3 ) ]  # 1st column = isos, last 3 columns = pop, gdp_ext, tfec_ext

nv$iv_all   <-  length ( d_origZZ )



#### apply field names ####

vn$iv_all  <-  names( d_origZZ )  # iv =  input variables

fn$iv_all <-  c ( "tfec" ,"trp" , "hhelec", "hhtot",
              "prot" , "hale", "unempl", "edubas",
              "water", "sani","fooddef" , "povgap320",
              "povhcrnl","doctors" , 
              "roadkm" , "gini_inv" , "gdp"  ,"gov" , 
              "democ", "growth" , "hlthxpd" , 
              "eduxpd" , "socsec" , "unions" , 
              "corppower", "fdi" , "cars" , 
              "urbpop" , "actage","resrent",
              "elecacc", "nsolfuel","railkm","infra",
              "taxrev","aid","remitt","soccont",
              "expo","impo", "colddays" , "hotdays" ,
              "tmin" , "tmean" )        

names( d_origZZ )  <- fn$iv_all

## note aid is primarily a response to levels of needs satisfaction rather than (initially) directly providing need satisfying goods / services (feedback loop)


#### calculate N for each variable, after excluding "ZZ" isos

nc$all  <- dim( d_origZZ )[1]

for ( vv in 1 : nv$iv_all ){
  
  nc$origZZ_by_var[ vv ]   <-  sum ( ! is.na ( d_origZZ[ , vv ]  )  )
  
}

names( nc$origZZ_by_var )   <-   fn$iv_all

