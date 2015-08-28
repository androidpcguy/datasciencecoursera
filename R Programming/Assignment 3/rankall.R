

rankall <- function(outcome, num = "best") {
  outcome_data <- read.csv(file = "outcome-of-care-measures.csv");
  possible_outcomes <- c("heart attack", "heart failure", "pneumonia")
  
  if(!(outcome %in% possible_outcomes )) {
    stop("invalid outcome")
  }
  
  split_data <- split(outcome_data,outcome_data$State)
  master_hospital <- vector("character")
  master_state <- vector("character")
  master_rank <- vector("integer")
  
  my_state <- data.frame()
  for(i in 1:54) {
    rank <- vector("integer")
    my_state <- split_data[[i]]
    my_data <- if(outcome == "heart attack") suppressWarnings(as.numeric(as.vector(my_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)))
    else if(outcome == "heart failure") suppressWarnings(as.numeric(as.vector(my_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)))
    else suppressWarnings(as.numeric(as.vector(my_state$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)))
    
    hospital_list <- as.vector(subset(my_state,select=Hospital.Name)$Hospital.Name)
    
    d <- data.frame(cbind(hosp = hospital_list, rate = my_data))
    a <- d[complete.cases(d),]
    a$rate <- as.numeric(levels(a$rate))[a$rate]
    ordered_a <- a[order(a$rate,a$hosp),]
    
     if(num == "best") {master_hospital[[i]] <- as.vector(ordered_a$hosp[[1]])} else if(num == "worst") {master_hospital[[i]] <- as.vector(ordered_a$hosp[[nrow(a)]])}
    master_state[[i]] <- names(split_data)[[i]]
    
    if(i == 52)
      print(ordered_a)    
    if(num == "best" || num == "worst")
      next
    
    if(num > nrow(a)) {
      master_hospital[[i]] <- NA
      next
    }

    master_hospital[[i]] <- as.vector(ordered_a$hosp[[num]])
    
  }
  data.frame(cbind(hospital = master_hospital, state = master_state))
      
}