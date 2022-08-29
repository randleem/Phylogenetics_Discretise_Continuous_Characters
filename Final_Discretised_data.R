myMatrix <- read.csv(file = "my matrix file path", header = TRUE)

discretiseFunction <- function(matrix){
  rowsLen <- length(matrix[, 1])
  colsLen <- length(matrix[1,])
  discretisedMatrix <-matrix(data=NA, nrow=rowsLen, ncol=colsLen)
  
  grandColIndex <- 1
  
  # Loop over the columns in the data
  for(col in matrix[1,]){
    print(col)
    #sort data and get standared dev
    colData<-matrix[,grandColIndex]
    sortedData <- sort(colData)
    length <- length(sortedData)
    
    #create temp matrix
    sortedMatrix <- matrix(data=NA, nrow=length, ncol=3)
    
    #Loop over ordered data and find the difference
    index <- 1
    for (num in sortedData){
      
      if(index < length){
        difference <- (num - sortedData[index+1])*-1
        print(difference)
        sortedMatrix[index, 1] <- num
        sortedMatrix[index, 2] <- difference
      }else{
        sortedMatrix[index, 1] <- num
        index <-1
        break
      }
      index <- index + 1
    }
    
    #Find the Standard Deviation of difference data
    differenceData <- sortedMatrix[1:length-1,2]
    standardDeviation <- 2* sd(differenceData)
    print(standardDeviation)
    # Loop over data and assign coding
    newIndex <- 1
    characterCoding <- 0
    sortedMatrix[1,3]<- 0
    
    for (row in sortedMatrix[1:length-1,2]){
      if(row > standardDeviation){
        characterCoding <- characterCoding+1
      }
      sortedMatrix[newIndex+1, 3] <- characterCoding
      newIndex <- newIndex + 1
      if(newIndex == length){
        newIndex <-1
      }
    }
    
    #put new coding into new data matrix
    indexTwo<-1
    for(data in colData){
      if(is.na(data)){
      }else{
        indexOfData<- which(sortedMatrix[,1]==data)
        codingValue <- sortedMatrix[(indexOfData[1]), 3]
        discretisedMatrix[indexTwo, grandColIndex] <- codingValue
      }
      indexTwo <- indexTwo+1
      if(indexTwo > rowsLen){
        indexTwo <- 1
      }
    }
    
    grandColIndex <- grandColIndex + 1
    if(grandColIndex > colsLen){
      grandColIndex <-1
    }
  }
  write.csv(discretisedMatrix,"Desktop/discretised_character_matrix", row.names = FALSE)
}

discretiseFunction(myMatrix)
