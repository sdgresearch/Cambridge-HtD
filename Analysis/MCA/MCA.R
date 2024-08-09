# Firts, Install packages
install.packages(c("FactoMineR", "factoextra","Factoshiny"))
install.packages(c("Factoshiny","missMDA","FactoInvestigate"))
install.packages(c("plyr"))
install.packages("lpyr")
install.packages("MASS")
# Set working directory
getwd()
setwd("C:/Users/humbe/OneDrive/Documents/23. IDBE/08. Dissertation/10. Dissertation_analysis/01. MCA")
getwd()

# Load libraries
library("FactoMineR") 
library("factoextra")
library("FactoInvestigate")
library("Factoshiny")
library("plyr")
library("plyr")
library("MASS")
#Load data
Data<-read.csv("230107_Cat data.csv")

head(Data[, 2:6], 70)
mcadata<-head(Data[, 2:6], 70)
summary(mcadata)
summary(Data)

#Summary of variablesand categories

count(Data)

# count number of c in each column
ldply(Data, function(c) sum(c =="Very High"))
ldply(Data, function(c) sum(c =="High"))
ldply(Data, function(c) sum(c =="Medium"))
ldply(Data, function(c) sum(c =="Low"))
ldply(Data, function(c) sum(c =="Very low"))


#MCA R code
MCA(mcadata, ncp = 5, graph = TRUE)
res.mca <- MCA(mcadata, graph = TRUE)
print(res.mca)

#Visualization
#Eigen value
eig.val <- get_eigenvalue(res.mca)
fviz_screeplot(res.mca, addlabels = TRUE, ylim = c(0, 15))


# MCA_Biplot including variable and LSOA's
fviz_mca_biplot(res.mca, 
                repel = TRUE, 
                ggtheme = theme_minimal())

# Variables
var <- get_mca_var(res.mca)
# Coordinates
head(var$coord)
# Cos2: quality on the factore map
head(var$cos2)
# Contributions to the principal components
head(var$contrib)

#Correlation between variables and principal dimensions
fviz_mca_var(res.mca, choice = "mca.cor", 
             repel = TRUE, 
             ggtheme = theme_minimal())




#Quality of representation of variable categories
fviz_mca_var(res.mca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, 
             ggtheme = theme_minimal())



# Graphs individuals/LSOA s

#Plots: quality and contribution - INDIVIDUALS/LSOA'S
# Plot - individuals (LSOA's) - MCA
ind <- get_mca_ind(res.mca)

fviz_mca_ind(res.mca, col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE, # Avoid text overlapping (slow if many points)
             ggtheme = theme_minimal())

fviz_mca_ind(res.mca,
             repel = TRUE, # Avoid text overlapping (slow if many points)
             ggtheme = theme_minimal())

# Cos2 of LOSA's
fviz_cos2(res.mca, choice = "ind", axes = 1:2, top = 20)
# Contribution of LSOA's to the dimensions
fviz_contrib(res.mca, choice = "ind", axes = 1:2, top = 20)



