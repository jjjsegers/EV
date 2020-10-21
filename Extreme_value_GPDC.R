require("fitdistrplus")
require("FNN")
require("weibullness")
require("bda")
require("GLDEX")
require("RANN")
require("evd")
#Inspired from https://rdrr.io/cran/evtclass/src/R/gpdcTrain.R
# From E. Vignotto

# Set Working Directory
wd <- "/Users/Timot/OneDrive/Bureau//mémoire"
setwd(wd)


# Source
source("Data_process_functions.r")
source("Distance_functions.r")

#data
mydata <- read.csv("letter.csv")  # read csv file 

#Process the data
processed_set <- remove_class(data = mydata, class_column = 17, class_excluded = "Z")



GPDC <- function(trainingdata, data_to_be_tested, max_neighbours, threshold_value) 
{
  
  res <- numeric()
  distances_base <- numeric()
  distances_test <- numeric()

  
  #Testing a new set of observations
  distances_test <- numeric()
  
  #init objects to store results
  #vector of distances
  distances_test <- numeric()
  #dataframe of results
  m <- matrix(0, ncol = length(q_values), nrow = nrow(data_to_be_tested))
  test_results <- data.frame(m)
  
  #Results dataframe
  for (i in 1:length(q_values)) {
    test_results[,i] <- quant_num[i]
  }
  
  
  #Distances in base set
  distances <- nn2(trainingdata,trainingdata,k=max_neighbours)
  distances <- distances$nn.dist
  
  #remove distances with same obs (itself)
  distances <- subset( distances, select = -1 )
  
  #Use thresold the k smallest distances values to estimate Epsi 
  threshold <- distances[,1]
  distances2 <- distances[,-1]
  
  
  R <- (distances2)/threshold
  
  #The mixture features
  shape <- apply(R, 1, function(x) mean(log(x[x != 0])))
  
  

  
  #Vignotto version
  #ball <- apply(cbind(shape,threshold),1,function(x)
  #  -evd::qgpd(q,x[2],x[1]*x[2],x[1]))
  #pshape <- p*shape
  
  
}



#test3 <- EVMachine()
#test_3 <- EVMachine(trainingdata = processed_set$Without_class, data_to_be_tested = processed_set$Selected_class, 
# boot_nb=10, q_values=c(0.90, 0.925, 0.95, 0.975, 0.98, 0.99)

# )
#max_neighbours <- 4
#threshold_value <- 2
#trainingdata <- processed_set$Without_class
#data_to_be_tested <- processed_set$Selected_class