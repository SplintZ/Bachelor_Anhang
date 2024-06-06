#script used for generating the excel tables used in the analysis of the pca and reprojection data
rm(list = ls())

#import libraries and config
library(openxlsx)   #Used to read the data from the excel file
library(FactoMineR) #Used to perform the PCA
source("paths.R")



#setup variables (changes here affect behavior of script)
replace_missing_with <- "median"
pca_dimensions <- 56
use_data <- "Fi"



#read data
txt <-vector()
data_Fa <- read.xlsx(path_fa, rowNames=TRUE, sheet=1, na.strings=c("n.d.","n.d. "))
data_Fi <- read.xlsx(path_fi, rowNames=TRUE, sheet=1, na.strings=c("n.d.","n.d. "))

if (("Fa" %in% use_data) &  ("Fi" %in% use_data)){
  #Import Fa and Fi data.
  data_og <- cbind(data_Fa, data_Fi[,(7:ncol(data_Fi))])
  txt <- c(txt,"Fa Fi")
  
} else if("Fa" %in% use_data) {
  #Import Fa data
  data_og <- data_Fa
  txt <- c(txt,"Fa")
  
} else if("Fi" %in% use_data) {
  #Import Fi data
  data_og <- data_Fi
  txt <- c(txt,"Fi")
}

colnames(data_og)[1:6] <- c("diagnosis", "BMI", "age", "duration", "num", "sex")
rm(data_Fi)
rm(data_Fa)



#process NA in data
if (replace_missing_with == "median"){
  #Change NA to median in group
  for (i in which(sapply(data_og, is.numeric)|sapply(data_og, is.logical))){
    for (j in which (is.na(data_og[,i]))){
      data_og[j,i] <- median(data_og[data_og[, "diagnosis"]==data_og[j,"diagnosis"],
                               i], na.rm=TRUE)
    }
  }
} else if(replace_missing_with == "average"){
  #Change NA to average in group
  for (i in which(sapply(data_og, is.numeric)|sapply(data_og, is.logical))){
    for (j in which (is.na(data_og[,i]))){
      data_og[j,i] <- mean(data_og[data_og[, "diagnosis"]==data_og[j,"diagnosis"],
                             i], na.rm=TRUE)
    }
  }
}

#Remove all Columns with 0 Variance
data_og <- cbind(data_og[,0:6], data_og[,which(apply(data_og,2,var) != 0)][, -c(1:3)])



#Standardize Data

#center data around 0
data_mat <- as.matrix(data_og[,(7:ncol(data_og))])
center <- colMeans(data_mat)
data_mat <- sweep(data_mat, 2, center)
#scale data to standard deviation 1
deviation <- function(v) {
  sqrt(sum(v^2) / max(1, length(v) - 1))
}
scale <- apply(data_mat, 2, deviation)
data_mat <- sweep(data_mat, 2, scale, "/")
#rebind scaled data to df
data <- cbind(data_og[,(0:6)], as.data.frame(data_mat))
rm(data_mat)



#Perform PCA
pca_data <- PCA(data[,7:ncol(data)], graph=FALSE, scale.unit = FALSE, ncp = pca_dimensions)

#get contribution data
contr_names <- pca_data$var$contrib
for (i in 1:56) {
  contr_names[i,] <- ifelse(contr_names[i,]> 100/56,rownames(contr_names)[i]," ")
}

contr_x <- pca_data$var$contrib
for (i in 1:56) {
  contr_x[,i] <- ifelse(contr_x[,i]> 100/56,"X"," ")
}

#add workbook for data export

#create and add contribution sheet
wb_pca <- createWorkbook()
addWorksheet(wb_pca, "Contribution")
writeData(wb_pca, "Contribution", pca_data$var$contrib[,1:11], startRow=1, startCol=1, rowNames = TRUE)
addWorksheet(wb_pca, "Contribution2")
writeData(wb_pca, "Contribution2", contr_x[,1:11], startRow=1, startCol=1, rowNames = TRUE)
conditionalFormatting(wb_pca, "Contribution",
                      cols = 1:11, rows = 1:57,
                      style = c("#e1ebbe", "#d14133"),
                      rule = c(0, 100/56),
                      type = "colourScale"
)
addWorksheet(wb_pca, "Contribution names")
writeData(wb_pca, "Contribution names", contr_names[,1:11], startRow=1, startCol=1, rowNames = TRUE)
saveWorkbook(wb_pca, file= paste(path_table,"contribution.xlsx"), overwrite=TRUE)


#Put PCA Coordinates and further corresponding patient information in data_transformed
data_transformed <- as.data.frame(pca_data[["ind"]][["coord"]])
data_transformed <- cbind(diagnosis=data$diagnosis, data_transformed)



#analyze pca
wb_patient <- createWorkbook()



#calculate averages
addWorksheet(wb_patient, "Average")

average_feature <- aggregate(data_transformed[,2:ncol(data_transformed)],
                     list(diagnosis = data_transformed$diagnosis), FUN=mean)
rownames(average_feature) <- average_feature$diagnosis
average_feature <- average_feature[,2:ncol(average_feature)]

writeData(wb_patient, "Average", average_feature, startRow=1, startCol=1, rowNames = TRUE)



#backproject and denormalize average patients
QMat <- as.matrix(pca_data$svd$V)
feature <- as.matrix(average_feature)
world <- feature %*% t(QMat)


#denormalise projection
addWorksheet(wb_patient, "Average Reprojected")

scale <- scale[names(scale) %in% colnames(data)]
reprojMat <- sweep(world, 2, scale, "*")
center <- center[names(center) %in% colnames(data)]
reprojMat <- sweep(reprojMat, 2, center, "+")
colnames(reprojMat) <- colnames(data[7:ncol(data)])
reprojMat <- apply(reprojMat, 2, round, digits=1)
rownames(reprojMat) <- rownames(average_feature)

writeData(wb_patient, "Average Reprojected", reprojMat, startRow=1, startCol=1, rowNames = TRUE)
writeData(wb_patient, "Average Reprojected", c("reprojected averages"), startRow=1)


#add average original non-projected data for comparison
average_world <- aggregate(data_og[,7:ncol(data_og)], list(diagnosis = data_og$diagnosis), FUN=mean)
rownames(average_world) <- average_world$diagnosis
average_world <- apply(average_world[,2:ncol(average_world)], 2, round, digits=1)
average_world <- average_world[rownames(reprojMat), ]

writeData(wb_patient, "Average Reprojected", average_world, startRow=8, startCol=1, rowNames = TRUE)
writeData(wb_patient, "Average Reprojected", c("original averages"), startRow=8)


#compare non-projected and reprojected data and add it
project_diff <- abs(reprojMat - average_world)

writeData(wb_patient, "Average Reprojected", project_diff, startRow=15, startCol=1, rowNames = TRUE)
writeData(wb_patient, "Average Reprojected", c("absolute difference"), startRow=15)


#compute variance
standarddevog <- aggregate(data_og[,7:ncol(data_og)], list(diagnosis = data_og$diagnosis), FUN=sd)
rownames(standarddevog) <- standarddevog$diagnosis
standarddevog <- apply(standarddevog[,2:ncol(standarddevog)], 2, round, digits=3)

writeData(wb_patient, "Average Reprojected", standarddevog, startRow=22, startCol=1, rowNames = TRUE)
writeData(wb_patient, "Average Reprojected", c("variance"), startRow=22)


#apply heatmap
conditionalFormatting(wb_patient, "Average Reprojected",
                      cols = 1:57, rows = 1:21,
                      style = c("#e1ebbe", "#d14133"),
                      rule = c(0, 5),
                      type = "colourScale"
)
conditionalFormatting(wb_patient, "Average Reprojected",
                      cols = 1:57, rows = 22:28,
                      style = c("#eef477", "#b48bc6"),
                      type = "colourScale"
)



#add Q Matrix (Eigenvectors) to see which muscles have most impact
addWorksheet(wb_patient, "Eigenvectors")
eigen <- t(QMat)
colnames(eigen) <- colnames(average_world)
writeData(wb_patient, "Eigenvectors", eigen, rowNames = TRUE)
conditionalFormatting(wb_patient, "Eigenvectors",
                      cols = 1:ncol(eigen)+1, rows = 1:nrow(eigen)+1,
                      style = c("#d14133", "#e1ebbe", "#d14133"),
                      rule = c(-0.3, 0, 0.3),
                      type = "colourScale"
)

#save results to excel
saveWorkbook(wb_patient, file= paste(path_table,pca_dimensions,"dimensions results.xlsx"), overwrite=TRUE)


