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
  
  #Distances in base set
  distances <- nn2(trainingdata,trainingdata,k=max_neighbours)
  distances_neg <- -(distances$nn.dist)
  
  #remove distances with same obs (itself)
  distances_neg <- subset( distances_neg, select = -1 )
  
  threshold <- distances_neg[,upper_order_nb+1]
  distances_neg <- distances_neg[,-(upper_order_nb+1)]

  R <- (distances_neg)/threshold


  shape <- mean(log(R[,1][R[,1] != 0])))

  
  

  
}


# )
#max_neighbours <- 4
#threshold_value <- 2
#trainingdata <- processed_set$Without_class
#data_to_be_tested <- processed_set$Selected_class
