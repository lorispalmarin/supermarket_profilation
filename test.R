cluster_data2c <- cluster_data2 %>%
  mutate(education = ifelse(education == "basic", 1,
                            ifelse(education == "phd", 2, 
                                   ifelse(education == "graduation", 3,
                                          ifelse(education == "2n cycle", 4,
                                                 ifelse(education == "master", 5,
                                                        NA))))))

cluster_data2c <- cluster_data2c %>%
  mutate(marital_status = ifelse(marital_status == "divorced", 1,
                                 ifelse(marital_status == "single", 2, 
                                        ifelse(marital_status == "married", 3,
                                               ifelse(marital_status == "widow", 4,
                                                      NA)))))




##### cluster with quantitative variables
categoricals <- select(data, education, marital_status, acceptedcmp3:acceptedcmp2, response)
cluster_data2 <- cbind(clustering_datas, categoricals)
cluster_data2 <- na.omit(cluster_data2)
cluster_data2 <- dummy_cols(cluster_data2, select_columns = c("education", "marital_status"), remove_selected_columns = TRUE)

# Scaling numerical variables
numerical_columns <- c(1:16, 24:26)
cluster_data2[numerical_columns] <- scale(cluster_data2[numerical_columns])

# t-SNE
set.seed(42)
tsne_results <- Rtsne(as.matrix(cluster_data2), perplexity = 40, check_duplicates = FALSE)

# Plot t-SNE results
plot(tsne_results$Y, pch=20, main="t-SNE of Clustered Data")

##now clustering, i apply nbclust to original data to get n of clusters
fviz_nbclust(cluster_data2, kmeans, method="gap_stat")
fviz_nbclust(cluster_data2, kmeans, method = "wss")
fviz_nbclust(cluster_data2, kmeans, method = "silhouette")

#k-means to t-sne results
kmeans_tsne <- kmeans(tsne_results$Y, centers=4)
tsneY <- as.data.frame(tsne_results$Y)
fviz_cluster(kmeans_tsne, data=tsneY, geom = "point",
             stand = FALSE, ellipse = FALSE, main = "k-means Clusters")
