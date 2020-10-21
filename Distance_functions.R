setwd("C:/Users/Timot/OneDrive/Bureau/mémoire")
mydata = read.csv("letter.csv")  # read csv file 

# INPUT:
#  - X a matrix of data points with the class variable excluded
#  - Y a vector of 1 new observation with the same structure as the matrix's rows
# OUTPUT:
# the nearest observation of the matrix to the new one

#y <- test$Selected_class[1,]
#x <- test$Without_class

Closest_Point <- function(y, x) {
  
  n <- nrow(x)
  sq_distances <- numeric(n)
  for (i in 1:n) {
    sq_distances[i] <- sum((y - x[i, ])^2)
  }
return( min(sq_distances) )
}


#test2 <- Closest_Point(y = test$Selected_class[1,], x = test$Without_class)
#test <- Closest_Point(y = trainingdata[56,], x = trainingdata[-c(56), ])