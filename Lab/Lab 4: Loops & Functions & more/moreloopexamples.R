# MORE LOOP EXAMPLES TO HELP WITH THE LOOPS LAB EXERCISE

# FIRST, Remember, indexing for dataframes: mydf[rows,columns]

#### Loop example 1 ####

# Here I create an empty dataframe and then fill it with the value of the cells based on their row and column names
(mydf<-data.frame(matrix(ncol = 5, nrow = 10))) #create an empty dataframe

names(mydf)<-letters[1:5] #set column names

mydf #look at new and updated dataframe

# First a loop within a loop:
# Fill in the cells of the dataframe with their column x row ID
for (i in 1:nrow(mydf)){
  for (j in 1:ncol(mydf)){
    mydf[i,j] <- paste(colnames(mydf)[j],row.names(mydf)[i],sep="")
  }
}

mydf #look at df

# Same as above but single loop: less efficient but also works
mydf.alt<-data.frame(matrix(ncol = 5, nrow = 10)) #create and empty dataframe
names(mydf.alt)<-letters[1:5] #set column names

for (i in 1:nrow(mydf.alt)){
    mydf.alt$a <- paste("a",row.names(mydf.alt)[i],sep="")
    mydf.alt$b <- paste("b",row.names(mydf.alt)[i],sep="")
    mydf.alt$c <- paste("c",row.names(mydf.alt)[i],sep="")
    mydf.alt$d <- paste("d",row.names(mydf.alt)[i],sep="")
    mydf.alt$e <- paste("e",row.names(mydf.alt)[i],sep="")
}

mydf.alt


#### Loop example 2 #### 

# Probability of seeing rabbits on 5, 15, 23 or 30 out of 35 walks if the overall probability of seeing a rabbit on a walk ranges from 0 to 1

(mydf2<-data.frame(matrix(ncol = 5, nrow = 11))) #create empty df

names(mydf2)<-c("probs",paste(c(5,15,23,30),"events",sep = "")) #set column names

mydf2 #look at the new empty df

mydf2[,1]<-seq(0,1,0.1) #fill in column 1

mydf2 #look at df

Nevents <- c(5,15,23,30) #vector of number of events

# Recall the arguments that the dbinom() function takes:
# dbinom(k, N, p)
# k = number of events
# N = number of trials
# p = probability of an event occurring

#Loop within a loop
for (i in 1:nrow(mydf2)){ #loop across all hypothesized probability values
  for(j in c(2:5)){ # j goes from 2 to 5 because we are estimating the likelihoods of some particular number of events (the column header) based on some hypothesized probability of an event occurring, but the columns where we want to input those values are columns 2 through 5. Column 1 has our models/hypotheses for they probability values.
    mydf2[i,j]<-dbinom(Nevents[j-1],35,mydf2$probs[i])
  }
}

mydf2

#Same result as above but single loop: less efficient but also works
mydf2.alt<-data.frame(matrix(ncol = 5, nrow = 11))
names(mydf2.alt)<-c("probs",paste(c(5,15,23,30),"events",sep = ""))
mydf2.alt[,1]<-seq(0,1,0.1)

for (i in 1:nrow(mydf2.alt)){
    mydf2.alt[i,2]<-dbinom(5,35,mydf2.alt$probs[i])
    mydf2.alt[i,3]<-dbinom(15,35,mydf2.alt$probs[i])
    mydf2.alt[i,4]<-dbinom(23,35,mydf2.alt$probs[i])
    mydf2.alt[i,5]<-dbinom(30,35,mydf2.alt$probs[i])
}
 
mydf2.alt



