

d_fl$gini  <-  100 - d_origZZ$gini



# create original data set with flips but no cleaning 


d_origZZ_fl  <-  d_origZZ

d_origZZ_fl$fooddef    <-   102.5 - d_origZZ$fooddef  # maybe leave variable name as is, for now, but change plot name to "sufficient nourishment" / "sufficient nutrition"

d_origZZ_fl$povgap320  <-   100 - d_origZZ$povgap  # maybe leave variable name as is, for now, but change plot name to "minimum monetary income" , "absence of absolute monetary poverty" , "ending absolute monetary poverty" , "monetary poverty elimination", "eradication of absolute income poverty" or "closing the monetary poverty gap" or "eradication of absolute poverty"

d_origZZ_fl$gini  <-  100 - d_origZZ$gini