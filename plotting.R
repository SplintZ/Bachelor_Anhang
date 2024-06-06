#script used for visualizing the patients in feature space
#along with additional information like ellipses or classification boundaries
rm(list = ls())

#import libraries and config
library(openxlsx)   #Used to read the data from the excel file
library(FactoMineR) #Used to perform the PCA
library(MASS)       #used to perform LDA
library(kernlab)    #used to perform kernel based discriminant analysis
library(plotly)     #Used for 3D scatter plot
library(ggplot2)    #Used for drawing
library(htmlwidgets)#Used to save 3D plot
library(rgl)        #used for ellipses in plot
library(Matrix)
source("paths.R")
source("functions.R")



#setup variables (changes here affect behavior of script)
analysis_method <- "lda"    #lda, lda2, qda or kfda, or pca 
color_dict<-c("sIBM"="#1F78B4", "PM-Mito"="#33A02C", "OLM"="#FF7D00", 
              "IMNM"="#6A3D9A", "Normal"="#E78AC3", "Validation"="#FFDD00")

#read data
data_og <- read_data(path_fi, path_fa, path_new=path_new)
data <- data_og[data_og[,1] != "validation", -1]
data_validation = data_og[data_og[,1] == "validation", -1]
data_diagnosis <- data_og[data_og[,1] != "validation", 1]
full_diag <- c(data_diagnosis, rep("Validation", 5))

#standardize data
center = colMeans(data)
std = apply(data,2,sd)
data <- sweep(data, 2, center)
data <- sweep(data, 2, std, "/")
data_validation <- sweep(data_validation, 2, center)
data_validation <- sweep(data_validation, 2, std, "/")

#perform kpca
kpca_data <- kpca(~., data=data, kernel="anovadot", kpar=list(sigma=0.005, degree=2), features=34)
data <- kpca_data@rotated
data_validation <- predict(kpca_data, data_validation)

#perform LDA
plot_model <- lda(data, grouping=data_diagnosis)
data <- as.matrix(data) %*% as.matrix(plot_model$scaling)
data_validation <- as.matrix(data_validation) %*% as.matrix(plot_model$scaling)


#create boundary lines
model <- qda(data[,-4], grouping=data_diagnosis)
boundaries <- boundary_points(-9, 22, -8, 7, -4, 7, 1, model)
create_plot("validation anovadot lda", data_transformed=rbind(data, data_validation), boundaries = boundaries)

#test validation
test_model <- lda(data, grouping=data_diagnosis)
toString(predict(test_model, data_validation)$class)

create_plot <- function(name, data_transformed=data, diagnosis=full_diag, ellipses=FALSE, boundaries=c()) {
  data_transformed <- cbind(diagnosis = diagnosis, as.data.frame(data_transformed))
  
  data_transformed$diagnosis <- as.factor(as.character(data_transformed$diagnosis))
  plot_3d <- plot_ly(data_transformed, 
                     x= ~data_transformed[,2], 
                     y= ~data_transformed[,3], 
                     z= ~data_transformed[,4], 
                     type="scatter3d", 
                     mode="markers",
                     color = ~diagnosis,
                     colors = color_dict,
                     symbols = c("circle", "diamond", "circle"),
                     text = ~paste("Ziffer:", rownames(data_transformed)),
                     marker=list(size =5))%>%
    
    layout(scene=list(xaxis=list(title=colnames(data_transformed[2])), 
                      yaxis=list(title=colnames(data_transformed[3])),
                      zaxis=list(title=colnames(data_transformed[4]))))
  
  #Save plot without ellipses
  saveWidget(plot_3d, paste(c(path_graph, name, " 3D.html"), collapse=""))
  
  if(ellipses != FALSE) {
    for (i in levels(data_transformed$diagnosis)){
      data_group <- data_transformed[(data_transformed$diagnosis %in% i),]
      #Calculate ellipsoid
      ellipsoid <- ellipse3d(cov(cbind(x=data_group[,2], 
                                       y=data_group[,3], 
                                       z=data_group[,4])),
                             centre = c(mean(data_group[,2]), 
                                        mean(data_group[,3]), 
                                        mean(data_group[,4])),
                             level=ellipses)
      
      #add ellipsoid to plot
      plot_3d <- plot_3d %>% add_trace(x=ellipsoid$vb [1,],
                                       y=ellipsoid$vb [2,],
                                       z=ellipsoid$vb [3,],
                                       type="mesh3d",
                                       alphahull = 0,
                                       opacity = 0.01,
                                       facecolor = rep(color_dict[i], ncol(ellipsoid$ib)*2),
                                       autocolorscale=FALSE,
                                       cauto=FALSE)
    }
  
    #save the plot
    saveWidget(plot_3d, paste(c(path_graph, name, " 3D_Ellipses.html"), collapse=""))
  }
  
  if(length(boundaries) != 0) {
    plot_data <- boundaries
    plot_3d <- plot_3d %>% add_trace(x= plot_data[,1], 
                                     y= plot_data[,2], 
                                     z= plot_data[,3], 
                                     type="scatter3d",
                                     text = paste("Ziffer:", rownames(plot_data)),
                                     color = plot_data$class,
                                     colors = color_dict,
                                     opacity = 0.5,
                                     marker=list(size=3))
    #Save plot with boundaries
    saveWidget(plot_3d, paste(c(path_graph, name, " boundaries 3D.html"), collapse=""))
  }
}
