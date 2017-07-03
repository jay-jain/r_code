training <- read.csv('/home/jay/Desktop/trait_stevens_training.csv',head = TRUE, sep = ',', check.names = FALSE, stringsAsFactors = FALSE)

removeValues <- function (dataframe, proportion, rowWeight, colWeight){
    # Remove the first column so species names do not get removed
    alteredDataFrame <- dataframe[,-c(1)]
    totalValues = nrow(alteredDataFrame) * ncol(alteredDataFrame)
    print(totalValues)
    numberOfValuesToRemove = proportion * totalValues
    
    # If rowWeight and colWeight are BOTH not specificed, then BOTH rowWeight and colWeight are set to 1.0
    # Else if rowWeight is not specified, but colWeight is specified, then set rowWeight to 1.0
    # Else if colWeight is not specififed, but RowWeight is specified, then set colWeight to 1.0
    if (missing(rowWeight) & missing(colWeight)){
      rowWeight <- rep(1.0,83)
      colWeight <- rep(1.0,6)
    }else if(missing(rowWeight) & !missing(colWeight))
      rowWeight <- rep(1.0,83)
    else{
      colWeight <- rep(1.0,6)
    }
    
    # Create weightMatrix and fill in values based on rowWeight and colWeight
    #weightMatrix <- Matrix(data=NA, nrow = 83, ncol = 6)
    weightVector <- vector(mode = "double",length = 498)
    print(length(weightVector))

     i = 1
     for(j in 1:length(colWeight)){
        for(k in 1:length(rowWeight)){
          weightVector[i] <- rowWeight[k] * colWeight[j]
          i = i + 1  

        }
      }

    print(weightVector)
    
    matrix <- sample(x = 1:totalValues, size = numberOfValuesToRemove, replace = FALSE, prob = weightVector)
    
    #print(length(matrix))
    for(i in 1:length(matrix)){
      row <- row(dataframe)[matrix[i]]
      column <- col(dataframe)[matrix[i]] + 1 # Add one to column index to account for removing species column
      dataframe[row,column] <- NA
    }
    return (dataframe)
}

missingTraining <- removeValues(training,proportion = 0.25,colWeight = c(.25,.25,.25,.25,.25,.35))

View(missingTraining)

# Show number of NA's in each trait column
colSums(is.na(missingTraining[,2:7]))

