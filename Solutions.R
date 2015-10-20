Hello.World <- function(){
  #The classic "Hello World" program.
  #
  # Args:
  #   none
  # 
  # Returns:
  #   Prints to the console
  
  print("Hello World")
}

myTtest <- function(){
  #Reads ARCENE data from the UCI website, performs t-tests on each column, then orders them.
  #
  # Args: 
  #   none
  # 
  # Returns:
  #   The five columns with the lowest p-values.  Note, p-values are in the last row of the dataframe.
  
  ##Assumptions:
  #   two-sided ttest
  
  ##Download data
  repository <- "https://archive.ics.uci.edu/ml/machine-learning-databases/arcene/ARCENE/"
  message("Downloading ARCENE data files")
  eTimeStart <- proc.time()
  arceneData <- read.table(paste0(repository, "arcene_train.data"))
  arceneLabels <- read.table(paste0(repository, "arcene_train.labels"))
  eTimeStop <- round(proc.time() - eTimeStart, digits = 2)
  message(paste("ARCENE data files downloaded in ", eTimeStop[3], " seconds"))
  ##argsout <- list(arceneData, arceneLabels) #can be used for development/debugging
  ##return(argsout) #can be used for development/debugging
  
  ##Perform ttests by column
  message("performing t-tests on all columns")
  myPapply <- lapply(arceneData, function(x) t.test(x[arceneLabels==-1],x[arceneLabels==1])$p.value)
  resTable <- rbind(arceneData, myPapply)
  
  ##Use for validation of lapply function
  #myP <- array(0,dim=c(10,1))
  #for (i in 1:10){
  #  myP[i] <- t.test(arceneData[arceneLabels==-1,i],arceneData[arceneLabels==1,i])$p.value
  #}

  ##Sort the columns of results dataframe according to the pvalue which is found in the last row
  message("sorting columns in ascending order according to p-values")
  sResTable <- resTable[order(resTable[nrow(resTable),])]
  rownames(sResTable)[101] <- "p-values"
  
  #Return the five columns with the lowest p-value
  return(sResTable[,1:5]) 
}

myRegression <- function(){
  #Generates points from an equation of a line with added noise, then fits a least-squares regression.
  #
  # Args:
  #   none
  # 
  # Returns:
  #   Plots the randomly generated points and the linear fit
  
  x <- runif(10)  #generate random x values
  y <- x+runif(1)+runif(10)  #generate y values from equation of a line and add in random noise
  somePoints <- data.frame(x,y)
  
  ##Find the solution to the system
  sol <- lm(formula = y~x, data = somePoints)
  
  ##Plot the original points and the results
  symColor = "darkorange"
  lineColor = "grey"
  plot(somePoints, type = "p", pch = 19, col = symColor, bg = symColor)
  lines(x, sol$fitted.values, col = lineColor, lwd = 3)
  legend("topleft", cbind("random data", "least-squares regression"), 
         col = c(symColor, lineColor),
         lty = c(-1, 1),
         lwd = c(1,3),
         pch = c(19, NA)
         )
}