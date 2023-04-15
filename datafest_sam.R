prime <- read.csv("Prime.csv")

library(stringr)
library(dplyr)
library(stats)

### CLUSTERING
library(ggplot2)

clusterset <- prime[,c(36,37,38)]
k <- 3
clusters <- kmeans(clusterset, k)
clusters$cluster

pairs(clusterset)


# Create a scatterplot
ggplot(data = prime, aes(x = client_to_attorney_ratio, y = median_income, label=StateName)) +
  geom_point(aes(color = factor(clusters$cluster))) + geom_text(hjust=0, vjust=0)
  scale_color_discrete(name = "Cluster")

