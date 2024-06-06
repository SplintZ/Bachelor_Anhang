##read old data
data_og <- read_data(path_fi, path_fa)
data_old <- as.data.frame(data_og[,(2:ncol(data_og))])
data_diagnosis <- data_og[,1]

#read new data
data_new <- read.xlsx(path_new, rowNames=TRUE, sheet=1, na.strings=c("n.d.","n.d. "))
data_new <- as.data.frame(data_new)
data_new <- data_new[, intersect(colnames(data_og), colnames(data_new))]

#perform kpca
kpca_data <- kpca(~., data=data_old, kernel="anovadot", kpar=list(sigma=0.005, degree=2), features=36)
data_old <- kpca_data@rotated
data_new <- predict(kpca_data, data_new)

#perform lda dimensionality reduction
full_model <- lda(data_old,grouping=data_diagnosis)
data_old <- as.matrix(data_old) %*% full_model$scaling
data_old <- as.data.frame(data_old)
data_new <- as.matrix(data_new) %*% full_model$scaling
data_new <- as.data.frame(data_new)

#create model
classes <- unique(data_diagnosis)
model <- perform_lda(data_old, grouping = data_diagnosis)
model$classes <- classes
predictions <- c()
dist_matrix <- matrix(0, nrow=nrow(data_new), ncol=length(classes))
colnames(dist_matrix) <- classes

#make predictions
for (i in 1:nrow(data_new)) {
  distance <- single_classify(data_new[i,], model, method="mahalanobis")
  #predictions[i] <- distance
  print(rownames(data_new)[i])
  print(distance)
  #predictions[i] <- classes[which(distance == min(distance))]
}

#get distances
for (i in 1:nrow(data_new)) {
  for (j in 1:length(classes)){
    dist_matrix[i,j] <- sqrt(sum((data_new[i,] - model$group_means[j,])^2))
  }
}

dist_matrix <- round(dist_matrix, digits = 2)

print(predictions)