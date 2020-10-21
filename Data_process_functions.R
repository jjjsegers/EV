# INPUT:
# Feed a matrix with the class column at the last position, in  a matrix of n columns, the class column
# to exclude should be at the nth position
#  - a matrix of data points with the class variable TO BE excluded
# OUTPUT:
# List with
# - a matrix without the class variable chosen value
# - a matrix with all the class variable chosen values

#data <- mydata
#class_column <- 17
#class_excluded <- "Z"

remove_class <- function(data, class_column, class_excluded) {

   #Excluding the class
  
  #rename the class column
  colnames(data)[class_column] <- "class"
  
  #Factorize the class column if necessary
  data[, class_column] <- as.factor(data[, class_column])
   
   #Build the sub-frames
   
   #class excluded
   Piece <- data[ which(data$class==class_excluded),]
   Piece <- Piece[, 1:class_column-1]
   
   #other classes
   Build <- data[ which(data$class!=class_excluded),]
   Build <- Build[, 1:class_column-1]
   
   results <- list(Build, Piece)
   names(results) <- c("Without_class", "Selected_class")
   return(results)
}


#test <- remove_class(data = mydata, class_column = 17, class_excluded = "Z")
