install.packages("tidyverse")
# Set working directory
setwd("C:/Users/humbe/OneDrive/Documents/23. IDBE/08. Dissertation/10. Dissertation_analysis/02. AHC")
getwd()

#Load libraries
library("FactoMineR")
library("factoextra")
library("FactoInvestigate")
library("Factoshiny")
library("cluster")
library("dendextend")
library("tidyverse")
#Load data
Data<-read.csv("230107_Cat data.csv")

head(Data[, 1:6], 70)
mcadata<-head(Data[, 2:6], 70)
summary(Data)


# Agglomerative Nesting (Hierarchical Clustering)
res.dist <- dist(Data, method = "euclidean")

res.agnes <- agnes(x = Data, # data matrix
                   stand = TRUE, # Standardize the data
                   metric = "euclidean", # metric for distance matrix
                   method = "ward" # Linkage method
)
#Visualise dendogram
fviz_dend(res.agnes, cex = 0.6, k = 5)

# Cut tree into 4 groups# Cut the dendrogram at a certain height giving a specific number of clusters with cutree()
grp <- cutree(res.agnes, k=6)

# Append the cluster result to the dataset so that we can plot the clusters with different colors 
clustersdata<- Data %>% 
  mutate(cluster = grp) %>%
  relocate(cluster, .after =  LSOA11CD)

summary(clustersdata)

write.csv(clustersdata, "Clusterdata_230107_K6.csv")


# Filter cluster of choice too see the LSOA in that cluster
clustersdata %>% filter(cluster == 1)






# Print clusters. The clusters make sense
fviz_cluster(list(data = Data, cluster = clustersdata), 
             main = 'Complete Linkage, euclidean distance')


#Visualise banner of agnes
plot(res.agnes,which.plots = 2)


#Visualise cluster plot

fviz_cluster(list(data = Data , cluster = grp))

fviz_cluster( object = res.agnes,
              data = Data, # data used for clustering
              ellipse.type = "norm",
              geom = "point",
              palette = "jco",
              main = "",
              ggtheme = theme_minimal())

