

corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  # use complete dataset
  id = 1:332
  
  # read in dataset
  filenames <- sprintf("%03d.csv", id)
  filenames <- paste(directory, filenames, sep="/")
  df_list <- lapply(filenames, read.csv, header=T)
  
  # get those with complete cases
  nobs <- vector(mode="integer", length = length(df_list))
  for (i in 1:length(df_list)) {
    nobs[i] <- sum(complete.cases(df_list[[i]]))
  }
  
  # select where nobs > threshold
  x <- data.frame(id, nobs)
  y <- subset(x, nobs > threshold)
  
  if (nrow(y) > 0) {
    # get cor for each sulfate/nitrate in original list which is complete
    cor_result <- vector(mode="numeric", length = nrow(y))
    for (i in 1:nrow(y)) {
      sulfate <- df_list[[y$id[i]]][complete.cases(df_list[[y$id[i]]]), ]$sulfate
      nitrate <- df_list[[y$id[i]]][complete.cases(df_list[[y$id[i]]]), ]$nitrate
      cor_result[i] <- cor(sulfate, nitrate)
    }
    return(cor_result)
  }
  else {
    cor_result <- vector("numeric", length = 0)
    return(cor_result)
  }
}
