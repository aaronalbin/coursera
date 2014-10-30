
complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  
  filenames <- sprintf("%03d.csv", id)
  filenames <- paste(directory, filenames, sep="/")
  df_list <- lapply(filenames, read.csv, header=T)
  
  nobs <- vector(mode="integer", length = length(df_list))
  for (i in 1:length(df_list)) {
    nobs[i] <- sum(complete.cases(df_list[[i]]))
  }
  
  return(data.frame(id, nobs))
}
