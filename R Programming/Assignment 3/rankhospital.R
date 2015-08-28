rankhospital <- function(state, outcome, num = "best") {
  outcome_data <- read.csv(file = "outcome-of-care-measures.csv");
  possible_outcomes <- c("heart attack", "heart failure", "pneumonia")
  possible_num <- c("best","worst")
  if(!(outcome %in% possible_outcomes )) {
    stop("invalid outcome")
  }
  if (!(state %in% unique(outcome_data$State))) {
    stop("invalid state")
  }
 # if(class(num)!="integer" || class(num) != "numeric" && !(num %in% possible_num)) {
  #  stop("invalid num")
  #}
  
  my_state <- split(outcome_data, outcome_data$State)[[state]]
  hospital_list <- as.vector(subset(my_state,select=Hospital.Name)$Hospital.Name)
  if(num > length(hospital_list)) {
    return(NA)
  }
  sorted_list <- vector("character")
  indices <- vector("integer")
  m <- data.frame()
  if(outcome == "heart attack") {
    my_data <- suppressWarnings(as.numeric(as.vector(my_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)))
    m <- data.frame(cbind(Hospital.Name = hospital_list, Mortality.Rate = my_data))
    sorted_data <- m[order(m$Mortality.Rate,m$Hospital.Name),]
    d <- sorted_data[complete.cases(sorted_data),]
    sorted_list <- as.vector(d$Hospital.Name)
    #print(sorted_list)
  }
  else if(outcome == "heart failure") {
    my_data <- suppressWarnings(as.numeric(as.vector(my_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)))
    m <- data.frame(cbind(Hospital.Name = hospital_list, Mortality.Rate = my_data))
    sorted_data <- m[order(m$Mortality.Rate,m$Hospital.Name),]
    d <- sorted_data[complete.cases(sorted_data),]
    sorted_list <- as.vector(d$Hospital.Name)
  }
  else {
    my_data <- suppressWarnings(as.numeric(as.vector(my_state$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)))
    m <- data.frame(cbind(Hospital.Name = hospital_list, Mortality.Rate = my_data))
    sorted_data <- m[order(m$Mortality.Rate,m$Hospital.Name),]
    d <- sorted_data[complete.cases(sorted_data),]
    sorted_list <- as.vector(d$Hospital.Name)
  }
  if(num == "best") {
    return(sorted_list[[1]])
  }
  else if(num == "worst") {
    return(sorted_list[[length(sorted_list)]])
  }
  else {
    return(sorted_list[[num]]) 
  }
}

