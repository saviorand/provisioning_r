

### standardise data to center = 0   and standard deviation = 1 , whilst ignoring NaNs in data
std_JV  <- function( input_data  ) {
  
  output_data   <-   (  input_data  - mean ( input_data , na.rm = TRUE ) ) / sd ( input_data , na.rm = TRUE )

  return( output_data )

}