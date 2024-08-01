### Functions used for cleaning and formatting data sets, mostly used on WDI and WGI (world bank data)
initializeWBDataset <- function(dataset, path, extension, match, startyear, endyear) {
 return(formatWBYears(
        readAndPivotWB(
        paste(path, dataset, extension, sep=""), match))
        %>% select(Country.Name, Country.Code, Year, !!quo_name(dataset) := value) 
        %>% filter(Year %in% (startyear:endyear), Country.Code != ""))
}

readAndPivotWB <- function(path, match, sep = ",") {
  df <- read.csv(path, sep = sep)
  df_year_colnames <- df %>% select(matches(match)) %>% colnames()
  df[df == ".."] <- NA
  cols <- names(df)[5:length(df)]
  df[cols] <- lapply(df[cols], as.numeric)
  df_pivot <- df %>% pivot_longer(cols=df_year_colnames,
                                  names_to='year',
                                  values_to='value')
  return(df_pivot)
}

formatWBYears <- function(df) {
  dfcopy <- df
  dfyears <- dfcopy$year
  dfyearsnum <- gsub("\\D+","", dfyears)
  dfyearsnumclean <- dfyearsnum %>% substr(1,4)
  dfcopy$Year <- as.numeric(dfyearsnumclean)
  return(dfcopy)
}

readAndPivotWBDirectory <- function(path) {
  for (file in path){
    df_pivot_output <- readAndPivotWB(paste(paste(staticPathWB, "initial/", sep=""), file, sep=""), "X.*")
    pivotPath = paste(staticPathWB, "pivoted/", sep="")
    write.csv(df_pivot_output, paste(pivotPath, file, sep=""), row.names=FALSE)
  }
}

readAndFormatWBYearsDirectory <- function(path) {
  for (file in path){
    if (file != "formatted"){
      fileName <- sub('\\.csv$', '', file)
      importedDF <- read.csv(paste(staticPathWB, file, sep=""))
      formatYearsDF <- formatWBYears(importedDF)
      renameYearsDF <- formatYearsDF %>% rename_at('value', ~fileName) %>% select(c("Country.Code", "Year", fileName))
      formattedPath <- paste(staticPathWB, "formatted/", sep="")
      write.csv(renameYearsDF, paste(formattedPath, file, sep=""), row.names = FALSE)
    }
  }
}