
best <- function(state, outcome) {  
  valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
  valid_states <- c("AL","AK","AZ","AR","CA","CO","CT","DE","FL","GA","HI","ID","IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY")
  
  # Check that state and outcome are valid (as a side effect, set i to the column index of the outcome)
  i = -1
  if (outcome == valid_outcomes[1])
    i = 11
  else if (outcome == valid_outcomes[2])
    i = 17
  else if (outcome == valid_outcomes[3])
    i = 23
  else
    stop("invalid outcome")
  
  if (!state %in% valid_states)
    stop("invalid state")
  
  # Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  # set column as numeric for 30-day mortality rates for the outcome interested in
  data[, i] <- suppressWarnings(as.numeric(data[, i]))
  
  # only use data for the state of interest
  data_sub <- subset(data, State == state)
  
  # get min value for the outcome
  min_val <- min(data_sub[, i], na.rm=TRUE)
  
  # get data such that outcome matches the min value
  result <- data_sub[which(data_sub[, i] == min_val), ]
  
  # Return hospital names in selected state with lowest 30-day mortality rate in sorted order
  hosp_names = result[, 2]
  return(sort(hosp_names))
}
