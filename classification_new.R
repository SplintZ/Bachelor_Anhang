#example script to illustrate how the data in our thesis was created
#if run as is it will perform a dimensionality reduction using LDA followed by
#a test of the classification performance using QDA the results are stored in the predict_matrix variable
rm(list = ls())

#import libraries and config
library(openxlsx)   #Used to read the data from the excel file
library(FactoMineR) #Used to perform the PCA
library(MASS)       #used to perform LDA
library(kernlab)    #used to perform kernel based discriminant analysis
library(ggplot2)    #Used for drawing
library(Matrix)
source("paths.R")
source("functions.R")

##read data
data_og <- read_data(path_fi, path_fa)

##Standardize Data
data_std <- as.data.frame(scale(data_og[,(7:ncol(data_og))]))
data_diagnosis <- data_og[,1]

#output current configuration classification matrix (predict)
classes <- unique(data_diagnosis)
predict_matrix <- matrix(0, nrow=length(classes), ncol=length(classes))
rownames(predict_matrix) <- classes
colnames(predict_matrix) <- classes

for (i in 1:nrow(data_std)) {
  
  data <- data_std
  
  #perform kpca for kfda dimensionality reduction (to undo rerun standardization)
  #data <- transform_kfda(data[-i,], dim=31, kernel="tanhdot", kargs=list())
  
  #perform lda dimensionality reduction
  red_model <- lda(data[-i,],grouping=data_diagnosis[-i])
  red_data <- as.matrix(data) %*% red_model$scaling
  red_data <- as.data.frame(red_data)

  model <- perform_lda(red_data[-i,], grouping = data_diagnosis[-i])
  model$classes <- classes
  #classify with predict
  #class <- as.character(classify(data[-i,], model)$class)
  
  #classify with mahalanobis
  distance <- single_classify(red_data[i,], model, method="euclidean")
  pred_class <- classes[which(distance == min(distance))]
  
  predict_matrix[data_diagnosis[i], pred_class] <- predict_matrix[data_diagnosis[i], pred_class] + 1
}

#average distances
#dist_matrix <- dist_matrix / length(data)

#output average correct predictions
print((predict_matrix[1,1] + predict_matrix[2,2] + predict_matrix[3,3] + predict_matrix[4,4] + predict_matrix[5,5]) / sum(predict_matrix))
