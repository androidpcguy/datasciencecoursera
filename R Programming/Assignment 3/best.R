best <- function(state, outcome) {
  outcome_data <- read.csv(file = "outcome-of-care-measures.csv");
  possible_outcomes <- c("heart attack", "heart failure", "pneumonia")
  
  if(!(outcome %in% possible_outcomes )) {
    stop("invalid outcome")
  }
  if (!(state %in% unique(outcome_data$State))) {
    stop("invalid state")
  }
  
  my_state <- split(outcome_data, outcome_data$State)[[state]]
  hospital_list <- as.vector(subset(my_state,select=Hospital.Name)$Hospital.Name)
  
  indices <- vector("integer")
  if(outcome == "heart attack") {
          my_data <- suppressWarnings(as.numeric(as.vector(my_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)))
          indices <- which(my_data == min(my_data,na.rm=TRUE))
  }
  else if(outcome == "heart failure") {
    my_data <- suppressWarnings(as.numeric(as.vector(my_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)))
    indices <- which(my_data == min(my_data,na.rm=TRUE))
  }
  else {
    my_data <- suppressWarnings(as.numeric(as.vector(my_state$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)))
    indices <- which(my_data == min(my_data,na.rm=TRUE))
  }

  sort(as.vector(hospital_list[indices]))[[1]]
}