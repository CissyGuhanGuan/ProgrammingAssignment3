best <- function(state, outcome){
    mydata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    if(sum(mydata$State == state) == 0){
        stop("invalid state")
    }
    if(outcome == "heart attack"){
        colNum <- 11
    }
    else if(outcome == "heart failure"){
        colNum <- 17
    }
    else if(outcome == "pneumonia"){
        colNum <- 23
    }
    else{
        stop("invalid outcome")
    }
    mydata[,colNum] <- as.numeric(mydata[ ,colNum])
    remaining <- subset(mydata, mydata$State == state & !is.na(mydata[,colNum]))
    hosName <- NULL
    min <- 100
    
    for(i in seq_along(remaining$Hospital.Name)){
        if(remaining[i, colNum] < min){
            hosName <- remaining$Hospital.Name[i]
            min <- remaining[i, colNum]
        }
        else if(remaining[i, colNum] == min){
            if(remaining$Hospital.Name[i] < hosName){
                hosName <- remaining$Hospital.Name[i]
                min <- remaining[i, colNum]
            }
        }
    }
    hosName
}