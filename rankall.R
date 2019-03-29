rankall <- function(outcome, num = "best") {
  
  #outcome<-"pneumonia"
  #num<-2
  
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
  
  states<-sort(unique(data[,2]))

  rankall<-data.frame(data[2,])
  colnames(rankall)<-c("Hosp","St","Outcome")
  for(i in 1:length(states)){
  #i<-1
  tempdata<-data
  tempdata<-subset(tempdata,State == states[i])
  tempdata<-tempdata[order(tempdata[,3],tempdata[,1]),]
  colnames(tempdata)<-c("Hosp","St","Outcome")
  if(num=="best"){
    num1<-1
  } else if(num=="worst"){
    num1<-nrow(tempdata)
  } else if (num>nrow(tempdata)){
   num1<-1
   tempdata[,1]<-NA
  } else{
  num1<-num
  }
  rankall[i,]<-tempdata[num1,]
  }
  rankall[,1:2]
  #i<-i+1
  }
rankall("heart attack", 20)
tail(rankall("pneumonia", "worst"), 3)
tail(rankall("heart failure"), 10)

data<-subset(data,State == "WY")
data<-data[order(data[,3],data[,1]),]

r <- rankall("heart attack", 4)
as.character(subset(r, St == "HI")$Hosp)

r <- rankall("pneumonia", "worst")
as.character(subset(r, St == "NJ")$Hosp)

r <- rankall("heart failure", 10)
as.character(subset(r, St == "NV")$Hosp)
