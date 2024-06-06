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
data <- as.data.frame(scale(data_og[,(7:ncol(data_og))]))
data_diagnosis <- data_og[,1]

#perform kpca for kfda dimensionality reduction (to undo rerun standardization)
#data <- transform_kfda(data, dim=31, kernel="tanhdot", kargs=list())

#perform lda dimensionality reduction
full_model <- lda(data,grouping=data_diagnosis)
data <- as.matrix(data) %*% full_model$scaling
data <- as.data.frame(data)

#output current configuration classification matrix (predict)
classes <- unique(data_diagnosis)
full_model$classes <- classes
predict_matrix <- matrix(0, nrow=length(classes), ncol=length(classes))
dist_matrix <- matrix(0, nrow=length(classes), ncol=length(classes))
rownames(predict_matrix) <- classes
colnames(predict_matrix) <- classes
rownames(dist_matrix) <- classes
colnames(dist_matrix) <- classes

for (i in 1:1000) {
  set.seed(i)
  
  #create sample with 1 test for every group
  test_sample <- rep(TRUE, nrow(data))
  for (j in classes) {
    test_sample[sample(which(data_diagnosis == j), 1)] <- FALSE
  }
  
  class <- c()
  model <- perform_lda(data[test_sample,], grouping = data_diagnosis[test_sample])
  model$classes <- classes
  #classify with predict
  #class <- as.character(classify(data[!test_sample,], model)$class)
  
  #classify with mahalanobis
  distances <- classify(data[!test_sample,], model, method="euclidean")
  dist_matrix <- dist_matrix + distances
  for (j in 1:length(classes)) {
    class[j] <- classes[which(distances[j,] == min(distances[j,]), arr.ind=TRUE)]
  }

  for (j in 1:length(classes)) {
    current <- predict_matrix[data_diagnosis[!test_sample][j], class[j]]
    predict_matrix[data_diagnosis[!test_sample][j], class[j]] <- current + 1
  }
}

#average distances
dist_matrix <- dist_matrix / 1000

#output average correct predictions
print((predict_matrix[1,1] + predict_matrix[2,2] + predict_matrix[3,3] + predict_matrix[4,4] + predict_matrix[5,5]) / sum(predict_matrix))
