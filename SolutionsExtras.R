Hello.World2 <- function(language=""){
  ##This function is for learning about object-oriented programming in R.
  # It outputs the classic "Hello World" three different languages depending on the input
  # http://www.cyclismo.org/tutorial/R/s3Classes.html is a good tutorial on methods
  
  # Args:
  #   language: The desired output language: either Spanish, French, or English.
  #
  # Returns:
  #   Prints to the console in the specificied language.
  
  language = deparse(substitute(language))
  
  if (identical(language, "English")){
    class(language) <- append(class(language), "english")
  } else if (identical(language, "Spanish")){
    class(language) <- append(class(language), "spanish")    
  } else if (identical(language, "French")){
    class(language) <- append(class(language), "french")    
  } else {
    stop("Please input the desired language, English, Spanish, or French.",
                  "  Note: first letter must be capitalized; no quotation marks.")
  }
  UseMethod("Hello.World2",language)
}

Hello.World2.english <- function(language){
  print("Hello World")
}

Hello.World2.spanish <- function(language){
  print("Hola al Mundo")
}

Hello.World2.french <- function(language){
  print("Bonjour le Monde")
}

myRegression2 <- function(type="confidence"){
  ##This function generates random data points, then fits a least-squares regression and finds either
  # the 95% confidence interval (default), or the 95% prediction interval.  The interval type is 
  # determined by the function input string of either "confidence" or "predict".
  #
  # Args:
  #   type: the type of interval to generate. Confidence (default) or prediction.
  #
  # Returns:
  #   Plots random points, linear fit, and desired interval.
  
  ##Generate points
  x <- runif(10)  #generate random x values
  y <- x+runif(1)+runif(10)  #generate y values from equation of a line and add in random noise
  somePoints <- data.frame(x,y)
  
  ##Find the solution to the system
  sol <- lm(formula = y~x, data = somePoints)
  
  ##Estimate the confidence interval of the fit
  sortPoints <- somePoints[order(somePoints$x),]
  pred.w.clim <- predict(lm(sortPoints$y~sortPoints$x), interval = type)
  
  ###Plots 
  
  symColor = "darkorange"
  lineColor = "grey"
  intervalColor ="blue"
  
  ##Plot the original points and the results
  plot(somePoints, type = "p", pch = 19, col = symColor, bg = symColor)
  lines(x, sol$fitted.values, col = lineColor, lwd = 3)
  legend("topleft", cbind("random data", "least-squares regression"), 
         col = c(symColor, lineColor),
         lty = c(-1, 1),
         lwd = c(1,3),
         pch = c(19, NA)
        )
  
  ##Plot the fit and confidence intervals
  matplot(sortPoints$x, pred.w.clim,
          type="l", lty = c(1,2,2), col = c(lineColor, intervalColor, intervalColor), lwd = 3,
          ylab = "predicted y", xlab = "x")
  points(somePoints, type = "p", pch = 19, col = symColor, bg = symColor)
  legend("topleft", cbind("random points", "least-squares regression", paste("95%",type,"interval")), 
        col = c(symColor, lineColor, intervalColor),
        lty = c(-1,1,2),
        lwd = c(2,3,3),
        pch = c(19,NA,NA)
        )
}