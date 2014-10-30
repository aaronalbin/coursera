
rankall <- function(outcome, num = "best") {
  valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
  all_states <- c("AL","AK","AZ","AR","CA","CO","CT","DE","FL","GA","HI","ID","IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY")
  
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
  
  # check if num is integer, or best/worst
  valid_num = FALSE
  if (num == "best" || num == "worst")
    valid_num = TRUE
  else if (num %% 1 == 0)
    valid_num = TRUE
  
  if (!valid_num)
    stop("invalid num")
  
  # Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  # set column as numeric for 30-day mortality rates for the outcome interested in
  data[, i] <- suppressWarnings(as.numeric(data[, i]))
  
  # Initialize empty vectors for data frame
  df_hospital <- character(0)
  df_state <- character(0)
  
  for (state in all_states) {
    # only use data for the current state of interest
    data_sub <- subset(data, State == state)
    
    # sort subset by outcome, then by name (if there is a tie)
    data_sub_sorted <- data_sub[order(data_sub[, i], data_sub[, 2]), ]
    
    # remove rows with NA in outcome
    data_sub_sorted <- data_sub_sorted[!is.na(data_sub_sorted[, i]),]
    
    if (num == "best")
      current_hospital <- data_sub_sorted[1, 2]
    else if (num == "worst")
      current_hospital <- data_sub_sorted[length(data_sub_sorted[, i]), 2]
    else
      # if num is greater than length of the results, return NA
      if (length(data_sub_sorted[, i]) < num)
        current_hospital <- NA
    else
      current_hospital <- data_sub_sorted[num, 2]

    df_hospital <- c(df_hospital, current_hospital)
    df_state <- c(df_state, state)
  }

  # Return a data frame
  return(data.frame(hospital = df_hospital, state = df_state))
}

