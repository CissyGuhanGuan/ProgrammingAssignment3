rankall <- function(outcome, num = "best"){
    ## read in data
    mydata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    ## test outcome validity, and nail down the column number
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
    
    ## transform rates into numeric type 
    mydata[ ,colNum] <- as.numeric(mydata[ ,colNum])
    
    ##subsetting, leaving only non-NA data and the desired columns
    valid<- subset(mydata, !is.na(mydata[ ,colNum]))[ ,c(2, 7, colNum)]
    
    ## declare the results ##
    rankAll <- data.frame(hospital = character(length(levels(as.factor(valid$State)))), state = character(length(levels(as.factor(valid$State)))), stringsAsFactors = FALSE)
    ## ran the ranking codes for each states  ##
    for(stateNum in seq_along(levels(as.factor(mydata$State)))){
        
        ## now we are at the following state
        state = levels(as.factor(mydata$State))[stateNum]
        
        ##subsetting, leaving only data for the current state
        remaining <- subset(valid, valid$State == state)
        
        ## figuring out the ranking number
        if(num == "best"){
            ## best means 1
            ranking <- 1
        }
        else if(num == "worst"){
            ## worst means last
            ranking <- length(remaining$Hospital.Name)
        }
        else if(num >= 1 & num <= length(remaining$Hospital.Name)){
            ## numbers mean numbers
            ranking <- num
        }
        else{
            ## out-numbered number gives out NA for the current state
            rankAll$hospital[stateNum] = "NA"
            rankAll$state[stateNum] = state
            next
        }
        
        ## we will assume ranking is five for comment purpose
        
        ## get the first five names ranked in order, and we will figure out the place for the rest names
        order <- order(remaining[1:ranking, 3])
        
        ## leagueTable for eventually give us the top 5 names, and we will get the 5th from here
        leagueTable <- data.frame(name = character(ranking), performance = numeric(ranking), stringsAsFactors = FALSE)
        
        ## get first 5 names ranked in leagueTable
        for(i in 1:ranking){
            leagueTable$name[i] = remaining[order[i], 1]
            leagueTable$performance[i] = remaining[order[i], 3]
        }
        
        ## sort out ties
        if(length(levels(as.factor(leagueTable$performance))) < ranking){
            for(i in 1:(ranking - 1)){
                if((leagueTable$performance[i] == leagueTable$performance[i + 1]) & (leagueTable$name[i] > leagueTable$name[i+1])){
                    testing <- leagueTable$name[i]
                    leagueTable$name[i] <- leagueTable$name[i + 1]
                    leagueTable$name[i + 1]<- testing
                }
            }
        }

        
        ## get the rest names in leagueTable
        for(i in (ranking + 1):length(remaining[ ,1])){
            if(i > length(remaining[ ,1])){
                rankAll$hospital[stateNum] <- leagueTable$name[ranking]
                rankAll$state[stateNum] <- state
                break
            }
            
            place <- ranking + 1
            while(remaining[i, 3] <= leagueTable$performance[place - 1]){
                place <- place - 1
            
                if(remaining[i, 3] == leagueTable$performance[place] & remaining[i, 1] > leagueTable$name[place]){
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
                        leagueTable$performance[itr] <- remaining[i, 3]
                        leagueTable$name[itr] <- remaining[i, 1]
                        break
                    }
                    leagueTable$performance[itr] <- leagueTable$performance[itr - 1]
                    leagueTable$name[itr] <- leagueTable$name[itr - 1]
                }
            }
            
        }
        rankAll$hospital[stateNum] <- leagueTable$name[ranking]
        rankAll$state[stateNum] <- state
        
    }
    rankAll
    
}