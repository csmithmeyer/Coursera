setwd("C:/Users/colby/Desktop/coursera/")
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)
outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[, 11])

##Part 2

best <- function(state, outcome) {
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
  data[1,1]
}

best("WA","pneumobnia")

best("SC", "heart attack")
best("NY", "pneumonia")
best("AK", "pneumonia")
