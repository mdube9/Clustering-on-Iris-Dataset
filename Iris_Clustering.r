#required packages for the script

package <- c('nnet','ggplot2','plyr','caretEnsemble','plotly','foreign','reshape2',
             'utils','class','e1071','randomForest','data.table','factoextra',
             'cluster','clustertend','seriation','NbClust','clValid')


#installing and loading necessary packages

for (x in package){
  if(require(x,character.only = TRUE,quietly=TRUE,warn.conflicts = FALSE)==FALSE){
    if(x!="factoextra"){
      install.packages(x)
      suppressPackageStartupMessages(suppressWarnings(suppressMessages(library(x,character.only = TRUE,
                                        quietly = TRUE,message=FALSE,
                                        verbose=FALSE,warn.conflicts = FALSE))))
    }
    else{
      devtools::install_github("kassambara/factoextra")
    }
  }
}
print("Successfully installed all the packages")

#Creating the formula
f=as.formula("Species ~ .")


#########################################Clustering#########################################
#Implementing clustering 
install.packages('factoextra')
devtools::install_github("kassambara/factoextra")
data(iris)
scaled_data=scale(iris[,-5])

#Checking clustering tendencies before clustering analysis
  set.seed(123)
  library('clustertend')
  hopkins(scaled_data, n = nrow(scaled_data)-1)

  #applying VAT now
  library("seriation")
  # scaled_data: ordered dissimilarity image
  df_dist <- dist(scaled_data) 
  dissplot(df_dist)
  
  #applying cluster tendency function 
  clustend <- get_clust_tendency(scaled_data, nrow(scaled_data)-1)
  clustend$hopkins_stat
  
  # Customize the plot
  clustend$plot + 
    scale_fill_gradient(low = "steelblue", high = "white")
  
#So there is non uniform tendencies in the plots, therefore clustering is a good option

#Determinig optimal number of clusters in the dataset
  library("factoextra")
  library("NbClust")
  fviz_nbclust(scaled_data, kmeans, method = "gap_stat")
  res.nbclust <- NbClust(scaled_data, distance = "euclidean",
                         min.nc = 2, max.nc = 10, 
                         method = "complete", index ="all") 
  factoextra::fviz_nbclust(res.nbclust) + theme_minimal()  
  
  
#Elbow method which take SSE of individual cluster 
  k.max <- 15 # Maximal number of clusters
  wss <- sapply(1:k.max, 
                function(k){kmeans(scaled_data, k, nstart=10 )$tot.withinss})
  plot(1:k.max, wss,
       type="b", pch = 19, frame = FALSE, 
       xlab="Number of clusters K",
       ylab="Total within-clusters sum of squares")
  abline(v = 3, lty =2)

  
#we now  want to apply average silhoutte for the algorithm
  library(cluster)
  k.max <- 15
  data <- iris.scaled
  sil <- rep(0, k.max)
  # Compute the average silhouette width for 
  # k = 2 to k = 15
  for(i in 2:k.max){
    km.res <- kmeans(scaled_data, centers = i, nstart = 30)
    ss <- silhouette(km.res$cluster, dist(data))
    sil[i] <- mean(ss[, 3])
  }
  # Plot the  average silhouette width
  plot(1:k.max, sil, type = "b", pch = 19, 
       frame = FALSE, xlab = "Number of clusters k")
  abline(v = which.max(sil), lty = 2)

#To find which clustering algorithm is best for the dataset
  
  library(clValid)
  # Compute clValid
  clmethods <- c("hierarchical","kmeans","pam")
  intern <- clValid(scaled_data, nClust = 2:6,
                    clMethods = clmethods, validation = "internal")
  # Summary
  summary(intern) 
  View(scaled_data)
  a<-hclust(scaled_data,method = "ward.D2")
  a_test=a$cluster  
  y=iris[,5]
  
  y=as.character(y,decreasing=FALSE)
  y
  y <- lapply(y, gsub, pattern = "virginica", replacement = "1", fixed = TRUE)
  View ( y[70:150])
  y<-as.numeric(y)

  sum (y!=a_test)
##########################################End Clustering####################################