## SOM creation and prediction for disposition
## Authors: Marius Boda and Peter Boda

### LOAD LIBRARIES - install with:
## install.packages(c("kohonen", "dummies", "ggplot2", "maptools", "sp", "reshape2", "rgeos", "rgdal", "dplyr", "caret", "e1071", "shiny"))

## ----------------------------------- LIBRARIES ------------------------------------

library(kohonen)
library(dummies)
library(ggplot2)
library(rgdal)
library(sp)
library(maptools)
library(reshape2)
library(rgeos)
library(dplyr)
library(caret)
library(e1071)
library(shiny)

## ------------------------------------ INFO -----------------------------------------

data <- df
data$total_pmh <- rowSums(data[,18:298])
data$total_meds <- rowSums(data[,724:771])
data$total_cc <- rowSums(data[,773:972])

dep_column = c(1)
esi_column = c(2)
demographics_columns = c(3:9)
disposition_column = c(12)
social_columns = c(10,11)
context_columns = c(13:16)
history_columns = c(18:298)
n_edvisits_column = c(299)
n_admissions_column = c(300)
n_last_columns = c(301:388)
n_min_columns = c(389:476)
n_max_columns = c(477:564)
n_median_columns = c(565:652)
n_last2_columns = c(653:661)
npos_columns = c(662:670)
count_columns = c(671:679)
vitals_columns = c(680: 686)
other_columns = c(687:723)
meds_columns = c(724:771)
n_surgeries_column = c(772)
cc_columns = c(773:972)
totals_columns = c(973:975)

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
  cat(sprintf(" \n"))
  #cat(print(all_indexes))
  # cat(sprintf("\n======================================= \n"))
  
  
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
  #cat(sprintf("================= \n"))
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

prepare <- function(data, indexes) {
  disposition <- data$disposition
  data <- data %>% mutate_if(is.factor, as.numeric)
  data <- scale(data)
  
  demographics <- data[indexes,demographics_columns]
  social <- data[indexes, social_columns]
  context <- data[indexes, context_columns]
  history <- data[indexes, history_columns]
  n_edvisits <- data[indexes, n_edvisits_column]
  n_admissions <- data[indexes, n_admissions_column]
  n_last <- data[indexes, n_last_columns]
  n_min <- data[indexes, n_min_columns]
  n_max <- data[indexes, n_max_columns]
  n_median <- data[indexes, n_median_columns]
  n_last2 <- data[indexes, n_last2_columns]
  npos <- data[indexes, npos_columns]
  count <- data[indexes, count_columns]
  vitals <- data[indexes, vitals_columns]
  other <- data[indexes, other_columns]
  meds <- data[indexes, meds_columns]
  n_surgeries <- data[indexes, n_surgeries_column]
  cc <- data[indexes, cc_columns]
  totals <- data[indexes, totals_columns]
  
  som_data <- list(demographics = demographics, 
                   history = history,
                   vitals = vitals,
                   cc = cc,
                   totals = totals, 
                   social = social,
                   context = context, 
                   n_edvisits = n_edvisits,
                   n_admissions = n_admissions,
                   n_last = n_last,
                   n_min = n_min,
                   n_max = n_max,
                   n_median = n_median,
                   n_last2 = n_last2,
                   npos = npos,
                   count = count,
                   other = other,
                   meds = meds,
                   n_surgeries = n_surgeries,
                   totals = totals,
                   disposition = disposition[indexes])
  return(som_data)
}


## ---------------------------- PRIMARY TRAINING FUNCTION ---------------------------------

training_som <- function(dataframe, 
                         size_of_data = 100, 
                         Epoch = 1,
                         K_folds = 1,
                         input = c("demographics", "vitals", "history"), 
                         grid_x = 5,
                         grid_y = 5) {
  
  all_folds <- cross_validation_indexes(size_of_data, K_folds, verbose = FALSE)
  
  dataframe <- dataframe[0:size_of_data,]
  
  som_grid = somgrid(grid_x, grid_y, "hexagonal") 
  
  for(e in 1:Epoch) {
    for(k in 1:K_folds){
      cat(sprintf(" \n"))
      cat(sprintf("=================================== \n"))
      cat(sprintf(" \n"))
      cat(sprintf("Epoch: %d \n", e))
      cat(sprintf("Fold: %d \n", k))
      cat(sprintf(" \n"))
      
      training_indexes <- all_folds[[k]]$training_set
      testing_indexes <- all_folds[[k]]$test_set
      
      som_training_data = prepare(dataframe, indexes = training_indexes)
      som_testing_data = prepare(dataframe, indexes = testing_indexes)
      
      som_model <- supersom(som_training_data, 
                            grid = som_grid, 
                            maxNA.fraction = .5, 
                            whatmap = input,
                            rlen = 1000)
      
      #View(som_model)
      
      som.prediction <- predict(som_model, newdata = som_testing_data)

      som.confusion <- confusionMatrix(data = som.prediction$predictions[["disposition"]], 
                                       reference = som_testing_data[["disposition"]])
      print(som.confusion$table)
      cat(sprintf(" \n"))
      
      na_count <- sum(is.na(som.prediction$predictions[["disposition"]]))
      cat(sprintf("Number of NA's: %i \n", na_count))
      
      
    }
  }
  
  # par(mfrow = c(1,2))
  som_model_plot <- plot(som_model, type = "mapping")
  # plot(som_model, type = "codes")

}








