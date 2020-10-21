require("fitdistrplus")
require("FNN")
require("weibullness")
require("bda")
require("GLDEX")
require("RANN")
require("evd")
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



EVMachine <- function(trainingdata, data_to_be_tested, boot_nb, q_values ) 
  {
  
  
  
  #Getting min distances for base matrix
  nearest_neighbours <- get.knn(data = trainingdata, k = 1, algorithm = "kd_tree" )
  min_distances <- nearest_neighbours$nn.dist
  
  #store base distances
  distances_base <- numeric()
  distances_base <- min_distances

  
  #Visu of the distribution
  vector_min_distances <- as.vector(as.numeric(min_distances))
  desc_weib <- descdist(vector_min_distances, boot = boot_nb)
  
  #removing zero distances 
  for (i in 1:length(vector_min_distances)) {
    if(vector_min_distances[i] == 0){vector_min_distances[i] <- NA}
  }
  vector_min_distances_omit <- na.omit(vector_min_distances)
  vector_min_distances_omit <- as.vector(as.numeric(vector_min_distances_omit))
  
  
  #Fitting the weibull(classical) to positive min Distances
  Weib <- fitdist(data=vector_min_distances_omit, distr="weibull")
  summary(Weib)
  plot_weibull <- plot(Weib)
  
  #Init. the quantile level of the weibull to test new obs
  #q_values <- c(0.90, 0.925, 0.95, 0.975, 0.98, 0.99)
  quant <- quantile(Weib, probs = q_values)
  quant <- quant$quantiles
  quant_num <- as.numeric(quant)
  
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
  
  
  #Distances for new obs + store
   distances1nn <- nn2(trainingdata,data_to_be_tested,k=1)
   distances1nn <- distances1nn$nn.dist
   
   distances_test <- distances1nn
    
  test_results[i,] <- test_results[i,] - distances_test[i]
  
  
  #Converting to boolean
  test_results[test_results<0] <- 0
  test_results[test_results>0] <- 1
  test_results[test_results==0] <- 1  

    
  results <- list(distances_base, distances_test, test_results, plot_weibull, summary(Weib), desc_weib)
  names(results) <- c("Distances in base set", "Tested distances", 
                      "Test results for each new obs", "plot of transformed weibull", "summary of transformed weibull",
                      "plot of base weibull")
  return(results)
  
  }



#test3 <- EVMachine()
test_3 <- EVMachine(trainingdata = processed_set$Without_class, data_to_be_tested = processed_set$Selected_class, 
                    boot_nb=10, q_values=c(0.90, 0.925, 0.95, 0.975, 0.98, 0.99)
                    
                    )

#trainingdata <- processed_set$Without_class
#data_to_be_tested <- processed_set$Selected_class