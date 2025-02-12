library(readr)
library(dplyr)
library(factoextra)
library(lubridate)
library(cluster)
library(tidyr)
library(fastDummies)
library(Rtsne)
library(pheatmap)
library(tibble)
library(ggplot2)
library(corrplot)

#data opening
data <- read.csv("data.csv")
nasproblem <- c(341, 457, 664, 1467, 1827, 1926)
data <- data[-nasproblem,]

########### DATA CLEANING

#Remove missing values
data <- na.omit(data)

#Lowercase for faster writing
data$Education <- tolower(data$Education)
data$Marital_Status <- tolower(data$Marital_Status)
data$Country <- tolower(data$Country)
names(data) <- tolower(names(data))

#Outliers handling
outliers_data <- subset(data, select=c("year_birth", "income", "kidhome", "teenhome", 
                                       "recency", "numdealspurchases", "numwebpurchases", 
                                       "numcatalogpurchases", "numstorepurchases", "numwebvisitsmonth"))
boxplot(scale(outliers_data))
data <- data[data$income <= 200000, ]
data <- data[data$year_birth >= 1930, ]
forboxplot <- subset(data, select=c("year_birth", "income", "kidhome", "teenhome", 
                                    "recency", "numdealspurchases", "numwebpurchases", 
                                    "numcatalogpurchases", "numstorepurchases", "numwebvisitsmonth"))
boxplot(scale(forboxplot))

# Removing meaningless values from marital status variable like "yolo" and "absurd

data <- subset(data, !(marital_status %in% c("yolo", "absurd", "alone", "together")))


#change data type from chr to date
data$dt_customer <- as.Date(data$dt_customer, format = "%Y-%m-%d")

summary(data)

###NEW VARIABLES
#computing age of customers
data$age <- 2024 - data$year_birth 
#computing total purchases
data$totalpurchases <- data$numwebpurchases + data$numcatalogpurchases + data$numstorepurchases
#computing total expenses
data$moneyspent <- data$mntwines + data$mntfruits + data$mntmeatproducts + data$mntfishproducts + data$mntsweetproducts + data$mntgoldprods
# years of fidelisation
data$dt_customer <- 2024 - year(data$dt_customer)

####### EDA

## single variable analysis boxplot
singlevar_data <- select(data, totalpurchases, mntwines, mntfruits, mntmeatproducts, 
                         mntfishproducts, mntsweetproducts, mntgoldprods, 
                         moneyspent)
singlevar_data <- gather(singlevar_data, key="variable")
singlevar_graph <- ggplot(singlevar_data)+
  geom_boxplot(aes(x=variable,y=value)) + 
  facet_wrap(~variable,ncol=8,scales="free") + 
  theme(strip.text.x = element_blank(),
        text = element_text(size=9))  

## distribution of age
hist(data$age, main = "Distribution of Age")

##correlation mtx and heatmap
corr_matrix <- cor(corr_data)
corr_heat <- corrplot(corr_matrix, method = "color", type = "upper", 
                      tl.col="black", tl.srt=45)






###### DOES COUNTRY REALLY MATTER?

datacountry <- dplyr::select(data, response, country)
datacountry$country <- as.factor(datacountry$country)

log_coun <- glm(response~country, data = datacountry, family = "binomial")
summary(log_coun)





####### CLUSTERING

#building dataframe
clustering_data <- data
#removing id and year of birth, excluding categorical variables
clustering_data <- dplyr::select(clustering_data, -id, -year_birth)
clustering_data <- dplyr::select(clustering_data, age, income, dt_customer, recency, kidhome, teenhome, mntwines, mntfruits, mntmeatproducts, 
                          mntfishproducts, mntsweetproducts, mntgoldprods, 
                          numdealspurchases, numwebpurchases, numcatalogpurchases,
                          numstorepurchases, numwebvisitsmonth, 
                          totalpurchases, moneyspent)

clustering_data$fish_ratio <- clustering_data$mntfishproducts / clustering_data$moneyspent
clustering_data$meat_ratio <- clustering_data$mntmeatproducts / clustering_data$moneyspent
clustering_data$sweet_ratio <- clustering_data$mntsweetproducts / clustering_data$moneyspent
clustering_data$gold_ratio <- clustering_data$mntgoldprods / clustering_data$moneyspent
clustering_data$fruit_ratio <- clustering_data$mntfruits / clustering_data$moneyspent
clustering_data$wine_ratio <- clustering_data$mntwines / clustering_data$moneyspent

clustering_data$web_ratio <- clustering_data$numwebpurchases / clustering_data$totalpurchases
clustering_data$catalog_ratio <- clustering_data$numcatalogpurchases / clustering_data$totalpurchases
clustering_data$store_ratio <- clustering_data$numstorepurchases / clustering_data$totalpurchases

clustering_datas <- select(clustering_data, -mntfishproducts, -mntmeatproducts,
                          - mntsweetproducts, -mntgoldprods, -numwebpurchases,
                          -numcatalogpurchases, -numstorepurchases, -mntwines, -mntfruits)
clustering_data <- na.omit(clustering_datas)



# need to scale? YES
summary(clustering_data)
boxplot(clustering_data)
clustering_datascaled <- scale(clustering_data)
summary(clustering_datascaled)
boxplot(clustering_datascaled)

# statistics to get number of clusters 
set.seed(123)
fviz_nbclust(clustering_datascaled, kmeans, method="gap_stat")
fviz_nbclust(clustering_datascaled, kmeans, method = "wss")
fviz_nbclust(clustering_datascaled, kmeans, method = "silhouette")

#apply k-means
set.seed(1)
clustering1 <- kmeans(clustering_datascaled, centers = 3 ) 
fviz_cluster(clustering1, clustering_datascaled)

#assign clusters back to the cluster data and compute means
clustering_data$Cluster <- clustering1$cluster
cluster1_means <- clustering_data %>% 
  group_by(Cluster) %>%
  summarise_all(mean)
write.csv(cluster1_means, file="clustering1means.csv")

silhouette_score_num <- mean(silhouette(clustering1$cluster, dist(clustering_data))[, "sil_width"])





##### cluster with quantitative variables
categoricals <- select(data, education, marital_status, acceptedcmp3:acceptedcmp2, response, complain)
cluster_data2 <- cbind(clustering_datas, categoricals)
cluster_data2 <- na.omit(cluster_data2)
cluster_data2$education <- as.factor(cluster_data2$education)
cluster_data2$marital_status <- as.factor(cluster_data2$marital_status)
cluster_data2$acceptedcmp1 <- as.factor(cluster_data2$acceptedcmp1)
cluster_data2$acceptedcmp2 <- as.factor(cluster_data2$acceptedcmp2)
cluster_data2$acceptedcmp3 <- as.factor(cluster_data2$acceptedcmp3)
cluster_data2$acceptedcmp4 <- as.factor(cluster_data2$acceptedcmp4)
cluster_data2$acceptedcmp5 <- as.factor(cluster_data2$acceptedcmp5)
cluster_data2$response <- as.factor(cluster_data2$response)
cluster_data2$complain <- as.factor(cluster_data2$complain)


# distance matrix using gower's distance to deal with numerical and categorical vars
dist_matrix <- daisy(cluster_data2, metric = "gower")

# t-SNE
set.seed(42)
tsne_results <- Rtsne(dist_matrix, perplexity = 40, check_duplicates = FALSE)

# Plot t-SNE results
plot(tsne_results$Y, pch=20, main="t-SNE of Clustered Data", xlab = "X1", ylab = "X2")

##now clustering, i apply nbclust to results of tsne to get n of clusters (3)
fviz_nbclust(tsne_results$Y, kmeans, method="gap_stat")
fviz_nbclust(tsne_results$Y, kmeans, method = "wss")
fviz_nbclust(tsne_results$Y, kmeans, method = "silhouette")

#k-means to t-sne results
kmeans_tsne <- kmeans(tsne_results$Y, centers=3)
tsneY <- as.data.frame(tsne_results$Y)
fviz_cluster(kmeans_tsne, data=tsneY, geom = "point",
             stand = FALSE, ellipse = FALSE, main = "k-means Clusters (t-SNE)")

#assign clusters back to the original data
cluster_data2$Cluster <- kmeans_tsne$cluster
k_tsne_means <- cluster_data2 %>% 
  group_by(Cluster) %>%
  summarise_all(mean) 

#silhouette score
silhouette_score_full <- mean(silhouette(kmeans_tsne$cluster, dist(tsneY))[, "sil_width"])


#computing means for general  
write.csv(k_tsne_means, file="k_tsne_means.csv")

general_means <- data %>%
  summarise_all(mean)
general_means <- dplyr::select(general_means, -1:-4, -21:-28)

general_means$fish_ratio <- general_means$mntfishproducts / general_means$moneyspent
general_means$meat_ratio <- general_means$mntmeatproducts / general_means$moneyspent
general_means$sweet_ratio <- general_means$mntsweetproducts / general_means$moneyspent
general_means$gold_ratio <- general_means$mntgoldprods / general_means$moneyspent
general_means$fruit_ratio <- general_means$mntfruits / general_means$moneyspent
general_means$wine_ratio <- general_means$mntwines / general_means$moneyspent

general_means$web_ratio <- general_means$numwebpurchases / general_means$totalpurchases
general_means$catalog_ratio <- general_means$numcatalogpurchases / general_means$totalpurchases
general_means$store_ratio <- general_means$numstorepurchases / general_means$totalpurchases

general_means <- dplyr::select(general_means, -6:-11, -13:-15)
write.csv(general_means, file="general_means.csv")




#### ANALYSIS OF CLUSTER

## boxplots for numerical variables

ggplot(cluster_data2, aes(x = factor(Cluster), y = cluster_data2$income, fill = factor(Cluster))) +
  geom_boxplot() +
  labs(title = paste("Distribution of income by cluster"),
       x = "Cluster",
       y = "Income") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3") +
  theme(legend.position = "none")

ggplot(cluster_data2, aes(x = factor(Cluster), y = cluster_data2$kidhome, fill = factor(Cluster))) +
  geom_boxplot() +
  labs(title = paste("Distribution of kids at home by cluster"),
       x = "Cluster",
       y = "Kidhome") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3") +
  theme(legend.position = "none")

ggplot(cluster_data2, aes(x = factor(Cluster), y = cluster_data2$teenhome, fill = factor(Cluster))) +
  geom_boxplot() +
  labs(title = paste("Distribution of teen at home by cluster"),
       x = "Cluster",
       y = "TeenHome") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3") +
  theme(legend.position = "none")

ggplot(cluster_data2, aes(x = factor(Cluster), y = cluster_data2$age, fill = factor(Cluster))) +
  geom_boxplot() +
  labs(title = paste("Distribution of age by cluster"),
       x = "Cluster",
       y = "Age") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3") +
  theme(legend.position = "none")

ggplot(cluster_data2, aes(x = factor(Cluster), y = cluster_data2$totalpurchases, fill = factor(Cluster))) +
  geom_boxplot() +
  labs(title = paste("Distribution of total purchases by cluster"),
       x = "Cluster",
       y = "Total purchases") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3") +
  theme(legend.position = "none")

ggplot(cluster_data2, aes(x = factor(Cluster), y = cluster_data2$moneyspent, fill = factor(Cluster))) +
  geom_boxplot() +
  labs(title = paste("Distribution of money spent by cluster"),
       x = "Cluster",
       y = "Money spent") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3") +
  theme(legend.position = "none")


## frequences of categoricals

education_freq <- with(cluster_data2, table(Cluster, education))
marital_freq <- with(cluster_data2, table(Cluster, marital_status))
response_freq <- with(cluster_data2, table(Cluster, response))

# 'education' graph
ggplot(cluster_data2, aes(x = education, fill = as.factor(Cluster))) +
  geom_bar(position = "dodge") +
  labs(x = "Education Level", y = "Frequency", fill = "Cluster") +
  theme_minimal()

# 'marital_status' graph
ggplot(cluster_data2, aes(x = marital_status, fill = as.factor(Cluster))) +
  geom_bar(position = "dodge") +
  labs(x = "Marital Status", y = "Frequency", fill = "Cluster") +
  theme_minimal()

#'response' graph
ggplot(cluster_data2, aes(x = response, fill = as.factor(Cluster))) +
  geom_bar(position = "dodge") +
  labs(x = "Response", y = "Frequency", fill = "Cluster") +
  theme_minimal()



######### CLUSTER VALIDATION
lmres <- glm(response ~ as.factor(Cluster), family="binomial", data = cluster_data2)
summary(lmres)


#dimension of each cluster
cluster_counts <- table((cluster_data2$Cluster))
