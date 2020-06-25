## SOM Training for disposition
## Authors: Marius Boda and Peter Boda

### LOAD LIBRARIES - install with:
#install.packages(c("kohonen", "dummies", "ggplot2", "maptools", "sp", "reshape2", "rgeos", "rgdal"))
##setwd("~/Dropbox/Dropbox_Marius/MyProjects/Visual Triage/R.Projects/4. Visual Triage SOM/RStudio Project")

## ----------------------------------- LIBRARIES ------------------------------------

library(kohonen)
library(dummies)
library(ggplot2)
library(rgdal)
library(sp)
library(maptools)
library(reshape2)
library(rgeos)
library(rgdal)
library(dplyr)

## ------------------------------------ INFO -----------------------------------------

disposition_column = c(12)
# "disposition"
esi_column = c(2)
# "esi"
demographics_columns = c(3:9)
# "age" "gender" "ethnicity" "race" "lang" "religion" "maritalstatus"
social_columns = c(10,11)
# "employstatus" "insurance_status"
context_columns = c(13:16)
# "arrivalmode" "arrivalmonth" "arrivalday" "arrivalhour_bin"
history_columns = c(17,21:301)
totals_columns = c(18:20)
# "total_pmh" "total_meds" "total_cc"
vitals_columns = c(683:689)
# "triage_vital_hr" "triage_vital_sbp" "triage_vital_dbp" "triage_vital_rr" "triage_vital_o2" "triage_vital_o2_device" "triage_vital_temp" 
meds_columns = c(727:773)
pmh_columns = c(21:301)
cc_columns = c(776:975)

## ---------------------------------- CROSS VALIDATION ------------------------------------

cross_validation_indexes <- function(size_of_data= 10, K_folds= 1, verbose=TRUE) {
  #
  # Index generation for Cross-validation with K folds 
  # 
  # 1. Takes all indexes of the original data, size of data given in size_of_data
  # 2. Randomly splits into K folds 
  # 3. Each folds used once as a test set, the rest train set
  # 4. Returns the K lists of test + train set indexes
  #
  #
  
  # this wil be the output returning, K times the folds= test + train indexes
  all_folds <- vector("list", length = K_folds)
  
  all_indexes <- c(1:size_of_data)  
  size_of_fold <-  floor(size_of_data / K_folds)
  
  cat(sprintf("Number of folds: %d \n", K_folds))
  cat(sprintf("Size of data:    %d \n", size_of_data))
  cat(sprintf("Size of folds:   %d \n", size_of_fold))
  #cat(print(all_indexes))
  cat(sprintf("\n======================================= \n"))
  
  
  rest_of_indexes <- all_indexes
  
  for(i in 1:(K_folds-1)){
    #cat(sprintf("Fold number: %d \n", i))
    #cat(sprintf("================= \n"))
    
    #
    # select the reqired number of samples from the remaining size
    #
    fold_indexes <- sample(rest_of_indexes, size=size_of_fold, replace = F)
    
    rest_of_indexes <-  setdiff(rest_of_indexes, fold_indexes)
    
    #cat("test (", length(fold_indexes), "): ", sort(fold_indexes), "\n\n")
    #cat("rest (", length(rest_of_indexes), "):", rest_of_indexes, "\n\n")
    
    all_folds[[i]] <- list(test_set=fold_indexes, training_set=all_indexes[-fold_indexes])
    
    
  }
  
  # and the remaining is the last fold, since dividing all data with K might not be an integer
  fold_indexes <- rest_of_indexes
  rest_of_indexes <-  setdiff(rest_of_indexes, fold_indexes)
  #cat(sprintf("Fold number: %d \n", i+1))
  cat(sprintf("================= \n"))
  #cat("test (", length(fold_indexes), "): ", sort(fold_indexes), "\n\n")
  #cat("rest (", length(rest_of_indexes), "):", rest_of_indexes, "\n\n")
  
  all_folds[[i+1]] <- list(test_set=fold_indexes, training_set=all_indexes[-fold_indexes])
  
  # by default, we view the resulting set of folds
  # set to view=FALSE in the function call if not needed
  if (verbose) {
    View(all_folds)
  }
  
  
  return(all_folds)
}

## -------------------------------- PREPARE FUNCTIONS --------------------------------

prepare_training <- function(data, training_indexes, testing_indexes, input){
  
  er_disposition <- data$disposition
  
  data <- data %>% mutate_if(is.factor, as.numeric)
  
  training <- scale(data[training_indexes, input])
  
  
  test <- scale(data[testing_indexes, input],
                center = attr(training, "scaled:center"),
                scale = attr(training, "scaled:scale"))
  
  
  training_data <- list(inputs = training,
                        disposition = er_disposition[training_indexes])
  
  
  test_data <- list(inputs = test, disposition = er_disposition[testing_indexes])
  
  
  return(training_data)
}

prepare_testing <- function(data, training_indexes, testing_indexes, input){
  er_disposition <- data$disposition
  
  data <- data %>% mutate_if(is.factor, as.numeric)
  training <- scale(data[training_indexes, input])
  
  test <- scale(data[testing_indexes, input],
                center = attr(training, "scaled:center"),
                scale = attr(training, "scaled:scale"))
  
  training_data <- list(inputs = training,
                        disposition = er_disposition[training_indexes])
  
  
  test_data <- list(inputs = test, disposition = er_disposition[testing_indexes])
  
  return(test_data)
  
}

## ---------------------------- PRIMARY TRAINING FUNCTION ---------------------------------

training_som <- function(dataframe, 
                         size_of_data = 100, 
                         Epoch = 1,
                         K_folds = 1,
                         input = c(demographics_columns), 
                         output,
                         grid_x = 15,
                         grid_y = 15) {
  
  
  
  all_folds <- cross_validation_indexes(size_of_data, K_folds, verbose = FALSE)
  
  ## assigns return value of function to all_folds
  
  som_grid = somgrid(grid_x, grid_y, "hexagonal") 
  ## creates som grid
  
  for(e in 1:Epoch) {
    for(k in 1:K_folds){
      cat(sprintf("================= \n"))
      cat(sprintf("Epoch: %d \n", e))
      cat(sprintf("Fold: %d \n", k))
      training_indexes <- all_folds[[k]]$training_set
      testing_indexes <- all_folds[[k]]$test_set
      som_training <- prepare_training(first_thousand, training_indexes, testing_indexes, input)
      som_testing <- prepare_testing(first_thousand, training_indexes, testing_indexes, input)
      
      som_model <- supersom(som_training, grid = som_grid, maxNA.fraction = .5)
      
      #som.prediction <- predict(som_model, newdata = som_testing,
                                #trainingdata = som_training)
      #som.table <- table(er_disposition[testing_indexes], som.prediction$predictions[["disposition"]])
      
      som.prediction <- predict(som_model, newdata = som_testing,
                                whatmap = "inputs")
      som.table <- table(er_disposition[testing_indexes], som.prediction$predictions[["disposition"]])
      
      
      print(som.table)
      #som_model <- supersom(som_training, grid = som_grid)
      #som.prediction <- predict(som_model, newdata = som_testing)
      #som.table <- table(er_disposition[testing_indexes], som.prediction$predictions[["disposition"]])
      #print(som.table)
      som.sensitivity <- sensitivity(som.table)
      cat(sprintf(" \n"))
      cat(sprintf("Sensitivity: %.3f %s \n", som.sensitivity * 100, "%"))
      som.specificty <- specificity(som.table)
      cat(sprintf("Specificty: %.3f %s \n", som.specificty * 100, "%"))
      cat(sprintf(" \n"))
    }
  }
  #return(som.prediction$predictions[["disposition"]])
}



