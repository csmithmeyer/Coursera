##Part 3

rankhospital <- function(state, outcome, num = "best") {
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  if(outcome=="heart attack"){
    outcome<-"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
  } else if (outcome == "heart failure"){
    outcome<-"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
  } else if (outcome == "pneumonia"){
    outcome<-"Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  } else{
    stop("invalid outcome")
  }
  
  data<-data[c("Hospital.Name","State",paste(outcome))]
  data[,3]<-as.numeric(data[,3])
  data<-na.omit(data)
  
  if(state %in% data[,2]==F){
    stop("invalid state")
  }
  data<-subset(data,State == state)
  data<-data[order(data[,3],data[,1]),]
  
  if(num=="best"){
    num<-1
  } else if(num=="worst"){
    num<-nrow(data)
  } else if (num>nrow(data)){
    NA
  }
  
  data[num,1]
}
rankhospital("MN","heart attack", 10)

rankhospital("NC", "heart attack", "worst")
rankhospital("WA", "heart attack", 7)
rankhospital("TX", "pneumonia", 10)
rankhospital("NY", "heart attack", 7)
