
# Load Libraries
library(tidyverse)  # Data manipulation and visualization
library(cluster)    # Clustering algorithms
library(factoextra) # Clustering visualization
library(NbClust)    # Optimal number of clusters
library(clValid)    # Cluster validation
library(corrplot)   # Correlation visualization
library(readxl)     # Reading Excel files
# Load the dataset
data <- read_excel("Travel_Review.xlsx")

# Examine the structure and summary of the data
str(data)
summary(data)

# Check for and impute missing values if necessary
data$Gardens[is.na(data$Gardens)] <- median(data$Gardens, na.rm = TRUE)

# Remove unnecessary columns
data <- data[,-which(names(data) == "UserID")]
# Principal Component Analysis (PCA)
data_pca <- prcomp(data, scale. = TRUE)
fviz_pca_var(data_pca, repel = TRUE)
# Scaling data for clustering
scaled_data <- scale(data)

# K-Means Clustering
set.seed(123)  # For reproducibility
kmeans_result <- kmeans(scaled_data, centers = optimal_clusters_kmeans, nstart = 25)
fviz_cluster(kmeans_result, data = scaled_data)

# PAM Clustering
pam_result <- pam(scaled_data, 5)
fviz_cluster(pam_result, data = scaled_data)
# Determine the optimal number of clusters for k-means
nb_kmeans <- NbClust(data = scaled_data, distance = "euclidean", method = "kmeans", min.nc = 2, max.nc = 10)
fviz_nbclust(nb_kmeans)
optimal_clusters_kmeans <- as.integer(nb_kmeans$Best.nc[1])

# Determine the optimal number of clusters for hierarchical clustering
nb_hc <- NbClust(data = scaled_data, distance = "euclidean", method = "ward.D2", min.nc = 2, max.nc = 10)
fviz_nbclust(nb_hc)
optimal_clusters_hc <- as.integer(nb_hc$Best.nc[1])
# Visualize the dendrogram for hierarchical clustering
hclus_result <- hclust(dist(scaled_data), method = "ward.D2")
fviz_dend(hclus_result, k = optimal_clusters_hc, rect = TRUE)
# Attach cluster membership to the original data
data_with_clusters <- cbind(data, KMeans_Cluster = kmeans_result$cluster, PAM_Cluster = pam_result$clustering)

# Exporting clustered data to a new Excel file
write.xlsx(data_with_clusters, "Clustered_Travelbiz_Data.xlsx")
