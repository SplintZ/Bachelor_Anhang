#reads the excel-tables at path_fi and path_fa containing the data that is to be analysed
#processes NAs and removes unusable data and returns the cleaned up tables as a data frame
read_data <- function(path_fi, path_fa, use="Fi", missing="median", path_new="") {
  data_Fa <- read.xlsx(path_fa, rowNames=TRUE, sheet=1, na.strings=c("n.d.","n.d. "))
  data_Fi <- read.xlsx(path_fi, rowNames=TRUE, sheet=1, na.strings=c("n.d.","n.d. "))
  
  if (("Fa" %in% use) &  ("Fi" %in% use)){
    #Import Fa and Fi data.
    data_og <- cbind(data_Fa, data_Fi[,(7:ncol(data_Fi))])
    
  } else if("Fa" %in% use) {
    #Import Fa data
    data_og <- data_Fa
    
  } else if("Fi" %in% use) {
    #Import Fi data
    data_og <- data_Fi[,-2:-6]
    colnames(data_og)[1] <- "diagnosis"
  }

  ##process NA in data
  if (missing == "median") {
    #Change NA to median in group
    for (i in which(sapply(data_og, is.numeric)|sapply(data_og, is.logical))){
      for (j in which (is.na(data_og[,i]))){
        data_og[j,i] <- median(data_og[data_og[, "diagnosis"]==data_og[j,"diagnosis"],
                                       i], na.rm=TRUE)
      }
    }
  } else if(missing == "average"){
    #Change NA to average in group
    for (i in which(sapply(data_og, is.numeric)|sapply(data_og, is.logical))){
      for (j in which (is.na(data_og[,i]))){
        data_og[j,i] <- mean(data_og[data_og[, "diagnosis"]==data_og[j,"diagnosis"],
                                     i], na.rm=TRUE)
      }
    }
  }
  
  #Remove all Columns with 0 Variance
  low_var <- which(apply(data_og[,-1],2,var) <= 0.7) + 1
  data_og <- data_og[, -low_var]
  
  if (path_new != "") {
    data_new <- read.xlsx(path_new, rowNames=TRUE, sheet=1, na.strings=c("n.d.","n.d. "))[,-2:-3]
    data_new[1] <- "validation"
    colnames(data_new)[1] <- "diagnosis"
    data_og <- rbind(data_og, data_new[, intersect(colnames(data_new), colnames(data_og))])
  }
  
  return(data_og)
}

#performs a kpca with the given kernel and parameters returns the patients transformed into
#feature space
transform_kfda <- function(data, dim=15, kernel="rbfdot", kargs=list()) {
  kpca_data <- kpca(~., data=data, kernel=kernel, kpar=kargs, features=dim)
  data_transformed <- kpca_data@rotated
  data_transformed <- as.data.frame(scale(data_transformed))
  
  return(data_transformed)
}

#reduces the dimensions of the data with LDA for classification and builds a classification model
#returns a data frame with all needed information for classification if needed
perform_lda <- function(data, grouping=c()) {
  groups <- unique(grouping)
  
  model <- lda(data, grouping=grouping)
  model$transformed <- as.matrix(data) %*% as.matrix(model$scaling)
  
  group_means <- matrix(nrow=length(groups), ncol=ncol(data))
  rownames(group_means) <- groups
  group_cov <- list()
  
  for (i in groups) {
    group_data <- data[which(grouping==i),]
    group_means[i,] <- colMeans(group_data)
    group_cov[[i]] <- cov(group_data)
  }
  
  model$group_means <- group_means
  model$group_cov <- group_cov
  
  return(model)
}

perform_qda <- function(data, grouping=c()) {
  model <- qda(data, grouping=grouping)
  return(model)
}

single_classify <- function(sample, model, method="predict") {
  groups = model$classes
  result <- c()
  
  if (method == "predict") {
    result <- predict(model, sample)
    
  } else if (method == "mahalanobis") {
    for (i in groups) {
      result[i] <- mahalanobis(sample, model$group_means[i,], model$group_cov[[i]])
    }
    
  } else if (method == "euclidean") {
    for (i in groups) {
      result[i] <- sqrt(sum((sample - model$group_means[i,])^2))
    }
  }
  return(result)
}

#returns classification of sample with desired model and method
classify <- function(sample, model, method="predict") {
  if (method == "predict") {
    result <- predict(model, sample)
  } else if (method == "mahalanobis") {
    groups <- model$classes
    dist_matrix <- matrix(nrow=nrow(sample), ncol=length(groups))
    colnames(dist_matrix) <- groups
    rownames(dist_matrix) <- groups

    for (i in groups) {
      dist_matrix[,i] <- mahalanobis(sample, model$group_means[i,], model$group_cov[[i]])
    }
    
    result <- dist_matrix
  } else if (method == "euclidean") {
    groups <- model$classes
    dist_matrix <- matrix(nrow=nrow(sample), ncol=length(groups))
    colnames(dist_matrix) <- groups
    rownames(dist_matrix) <- groups

    for (i in groups) {
      for (j in 1:length(groups)){
        dist_matrix[j,i] <- sqrt(sum((sample[j,] - model$group_means[i,])^2))
      }
    }
    result <- dist_matrix
  }
  
  return(result)
}

#creates an array of points that are then classified with the given model
#removes all points that do not have a point of a different class bordering them
#used for dreating the classification boundaries in the plotly plots
boundary_points <- function(minx, maxx, miny, maxy, minz, maxz, stepsize, model) {
  points <- data.frame(x=double(), y=double(), z=double(), class=character())
  xstep <- length(seq(minx, maxx, stepsize))
  ystep <- length(seq(miny, maxy, stepsize))
  zstep <- length(seq(minz, maxz, stepsize))
  
  for (x in seq(minx, maxx, stepsize)) {
    for (y in seq(miny, maxy, stepsize)) {
      for (z in seq(minz, maxz, stepsize)) {
        class <- toString(predict(model, c(x,y,z))$class)
        points[nrow(points)+1,] <- list(x,y,z,class)
      }
    }
  }
  
  keep <- integer()
  for (i in 1:nrow(points)) {
    point <- points[i,]
    if (point$x==minx || point$x==maxx || point$y==miny || point$y==maxy || point$z==minz || point$z==maxz) {
      next
    }

    neighbors <- c()
    neighbors[1] <- points[i+ystep*zstep,4]
    neighbors[2] <- points[i-ystep*zstep,4]
    neighbors[3] <- points[i+zstep,4]
    neighbors[4] <- points[i-zstep,4]
    neighbors[5] <- points[i+1,4]
    neighbors[6] <- points[i-1,4]
    if (sum(which(neighbors != point$class)) > 0) {
      keep <- c(keep,i)
    }
  }
  
  return(points[keep,])
}
