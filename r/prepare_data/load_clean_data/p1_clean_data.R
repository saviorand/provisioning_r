
#### cleaning data (removing outliers, filling gaps) ####

d_cl  <-  d_fl  # d_cl will be the "clean" data set

colSums ( d_cl == 0 , na.rm = TRUE ,  dims = 1 )   # check for zeros


#### manually filling a few key data gaps ####

d_cl$povgap320[ g$isos_all == "JPN"]  <-  100 - 0.3  # source: WB WDI 2018 (= same data set as the rest), data for Japan for 2008 (only existing year). It seems about right though.

# d_cl$gdp[ g$isos_all == "MMR" ]  <- 4114.23  # source: WB WDI 2018  <--- now using that data set for GDP completely

d_cl$remitt[ g$isos_all == "COG"]  <-  0.499  # source: WB WDI 2018, data for COG for 2012  (only hs-country with no data for remittances)

# d_cl$ps_gini[ g$isos_all == "CUB"]  <- 38 # source Wikipedia; data for 2000



#### correcting erronous data ####

d_cl$tmean[ which( g$isos_all == "MMR" ) ]  <- 23.1   # in the original data, this is 13.05 which must be wrong (lower than tmin; stated differently in all sources) ; the value used here (23.1) is from https://crudata.uea.ac.uk/~timo/climgen/national/web/Myanmar/obs_tmp.htm



#### removing outliers ####    (based on histograms and scatters with TFEC(mainly), or GDP or hale)

# d_cl$enu_trp_total_pc[which(d_cl$enu_trp_total_pc > 150)]  <-  NA  # this is LUX. looks like an outlier but might be real. Might have to leave in there.

# d_cl$unempl [ which( d_origZZ$unempl < 0.25)]  <- NA  # this is Cambodia(KHM) and is statistically an outlier-ish (log histogram), though probably accurate; if not NaNned here, would be in hs

d_cl$fooddef [ g$isos_all == "MAC" ]  <- NA  # fooddef (undernourishment): MAC (Macau) must be wrong data. ; not needed for hs

d_cl$corppower [ which(d_origZZ$corppower < 0.01)]   <-  NA  # This is Namibia. Log histogram suggests this is an extreme outlier, and it's probably not true.
d_cl$corppower [ which ( d_origZZ$corppower > 800) ] <- NA # This is HongKong. Probably true, but is it representative? Probably not. kick out or leave in? ; not needed for hs

d_cl$unions[ which(d_origZZ$unions < 0.25)]  <- NA  # this is Venezuela (VEN) and is statistically a massive outlier (log scatter, histogram); not needed for hs

d_cl$fdi [ which( d_origZZ$fdi > 250 ) ]  <- NA  ### This is Luxembourg, and it is not impossible, but a massive outlier
# d_cl$fdi [ which( d_origZZ$fdi < 0 ) ]  <- NA  ## These are AGO COD KIR MWI PNG YEM ; ## do not activate: negative FDI net inflows DO make sense; however, these countries are not in hs anyways
# d_cl$fdi[ d_origZZ$fdi < -5 ]  <- NA  ## This is Angola. Massive outlier (log histogram)
# d_cl$fdi[ d_origZZ$fdi < - 1.8 ]  <- NA ## This is Kiribati. Massive outlier (log histogram), and very small island country, so not too representative.

d_cl$growth[which(d_origZZ$growth > 28 )]  <- NA  # does not exist if I use Gujarati fit method to calculate growth rate ; # This is Lybia. And actually very likley correct. However, a massive outlier

d_cl$urbpop[ which(d_origZZ$urbpop < 10 ) ]  <-  NA # This Trinidad & TObago (TTO), and most likely incorrect, based on data from WIkipedia. Also massive outlier (scatter)

d_cl$actage[ which(d_origZZ$actage > 80 ) ]  <-  NA # This is ARE & QAT; most likely incorrect (90% of pop 15--64???), definintely outliers

# d_cl$ps_res_rent[ g$isos_all == "MUS"  ]  <-  NA # This is Mauritius (small island state, not very representative); probably correct, but a massive outlier

# d_cl$infra[ which(d_origZZ$infra < 1.5 ) ]  <-  NA # This is Congo (COG) and might be realistic, but is a massive outlier.

d_cl$remitt[ d_origZZ$remitt < 0.007] <- NA # This is Angola and Kuwait. In particular Angola is a massive outlier.

d_cl$soccont [  g$isos_all == "TZA"] <- NA # This is Tansania. A massive outlier in log space

d_cl$soccont[  g$isos_all == "ESP"] <- NA # This is Spain. A massive outlier in lin space. Check if it's accurate

d_cl$tmin [  g$isos_all == "GRL"] <- NA   # This is Greenland. A massive outlier in lin space, and too special and small a country to be representative


#### notes ####

# hale: SWZ, CAF, LSO (Swaziland probably real due to world highest HIV rate; Lesotho possible, also very high HIV rate; Central African Republic data seems plausible)
# important to leave these in there as these are NOT explained by energy. But then, don't have anything on HIV in there currently...

# unemployment: QAT, THA and BLR very low unemployment rates are most likely real. 

# education: BTN poor education is real, COD relatively good education is plasuible, too! 

# water: PNG (probably true), GNQ (Eq. Guinea: probably accurate), AGO (plausible)

# sanitation: GAB (plausible

# unernoursihment (fooddef): MAC (Macau) must be wrong data. KIR (Kiribati) plausible, but should perhaps take out overall, as small island country

# doctors: CUB (Cuba) and GRC (Greece) data is real.

# gov: gdp data for GNQ (Eq. Guinea) seems weird, but most likely accurate (massive boom and bust)

# eduxpd: GHA (Ghana) and ETH (Ethiopia) look like outliers, but most likely accurate


