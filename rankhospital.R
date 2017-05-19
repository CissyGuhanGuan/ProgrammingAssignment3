rankhospital <- function(state, outcome, num = "best"){
    setwd("~/Documents/workspace/ProgAssignment3-data")
    mydata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    if(sum(levels(as.factor(mydata$State)) == state) == 0){
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
    
    mydata[ ,colNum] <- as.numeric(mydata[ ,colNum])
    
    remaining <- subset(mydata, mydata$State == state & !is.na(mydata[ ,colNum]))
    
    if(num == "best"){
        ranking <- 1
    }
    else if(num == "worst"){
        ranking <- length(remaining$Hospital.Name)
    }
    else if(num >= 1 & num <= length(remaining)){
        ranking <- num
    }
    else{
        return("NA")
    }
    
    order <- order(remaining[ ,colNum][1:ranking])
    
    leagueTable <- list(name = character(ranking), performance = numeric(ranking))
    
    for(i in 1:ranking){
        leagueTable$name[i] = remaining$Hospital.Name[order[i]]
        leagueTable$performance[i] = remaining[ ,colNum][order[i]]
    }
    
    if(length(levels(as.factor(leagueTable$performance))) < ranking){
        for(i in 1:(ranking - 1)){
            if((leagueTable$performance[i] == leagueTable$performance[i+1]) & (leagueTable$name[i] > leagueTable$name[i+1])){
                testing <- leagueTable$name[i]
                leagueTable$name[i] <- leagueTable$name[i + 1]
                leagueTable$name[i + 1]<- testing
            }
        }
    }

    
    
    for(i in (ranking + 1):length(remaining$Hospital.Name)){
        if(i > length(remaining$Hospital.Name)){
            break
        }

        place <- ranking + 1
        while(remaining[ ,colNum][i] <= leagueTable$performance[place - 1]){
            place <- place - 1
 
            
            if(remaining[ ,colNum][i] == leagueTable$performance[place] & remaining$Hospital.Name[i] > leagueTable$name[place]){
               place <- place + 1
               break
            }
            if(place == 1){
                break
            }
        }

        if(place <= ranking){
            for(itr in ranking : place){
                if(itr == place){
                    leagueTable$performance[itr] <- remaining[ ,colNum][i]
                    leagueTable$name[itr] <- remaining$Hospital.Name[i]
                    break
                }
                leagueTable$performance[itr] <- leagueTable$performance[itr - 1]
                leagueTable$name[itr] <- leagueTable$name[itr - 1]
            }
        }
       
    }
    leagueTable$name[ranking]
    
}
