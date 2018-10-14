# define the USArrests as the dataset
ds <- USArrests

# calculate average mur
der, assault, urban pop and rape in America
avg_murder <- mean(ds$Murder)
avg_assault <- mean(ds$Assault)
avg_up <- mean(ds$UrbanPop)
avg_rape <- mean(ds$Rape)

avg_murder
avg_assault
avg_up
avg_rape

# calculate the Euclidian matrix
dist_matrix <- dist(ds)

# PART A - cluster the states using hierarchical clustering w/ complete linkage
hclust.complete <- hclust(dist_matrix, method = "complete")

# PART B - plot the cluster and cut the dendogram that results in three distinct clusters
# here, at height=150, it results in three distinct clusters
plot(hclust.complete)
abline(h = 150, col = "red")

# get the states on each clusters
# States in Cluster-1: Alabama, Alaska, Arizona, California, Delaware, Florida, Illinois, Louisiana, Maryland, Michigan, Mississippi, Nevada, New Mexico, New York, North Carolina, South Carolina
# States in Cluster-2: Arkansas, Colorado, Georgia, Massachusetts, Missouri, New Jersey, Oklahoma, Oregon, Rhode Island, Tennessee, Texas, Virginia, Washington, Wyoming
# States in Cluster-3: Connecticut, Hawaii, Idaho, Indiana, Iowa, Kansas, Kentucy, Maine, Minnesota, Montana, Nebraska, New Hampshire, North Dakota, Ohio, Pennsylvania, South Dakota, Utah, Vermont, West Virginia, Wisconsin
cut.ds <- cutree(hclust.complete, h=150)

# which cluster corresponds to which state - 0 is false, 1 is true
table(rownames(ds), cut.ds)

# calculate the means in cluster 1
avg_murder_1 <- mean(ds$Murder[cut.ds == 1])
avg_assault_1 <- mean(ds$Assault[cut.ds == 1])
avg_up_1 <- mean(ds$UrbanPop[cut.ds == 1])
avg_rape_1 <- mean(ds$Rape[cut.ds == 1])

# spit out some information for cluster 1
paste("C1 has more murders than nation average? ", avg_murder < avg_murder_1)
paste("C1 has more assaults than nation average? ", avg_assault < avg_assault_1)
paste("C1 has more rapes than nation average? ", avg_rape < avg_rape_1)
paste("C1 has more urban pop than nation average? ", avg_up < avg_up_1)

# calculate the means in cluster 2
avg_murder_2 <- mean(ds$Murder[cut.ds == 2])
avg_assault_2 <- mean(ds$Assault[cut.ds == 2])
avg_up_2 <- mean(ds$UrbanPop[cut.ds == 2])
avg_rape_2 <- mean(ds$Rape[cut.ds == 2])

# spit out some information for cluster 2
paste("C2 has more murders than nation average? ", avg_murder < avg_murder_2)
paste("C2 has more assaults than nation average? ", avg_assault < avg_assault_2)
paste("C2 has more rapes than nation average? ", avg_rape < avg_rape_2)
paste("C2 has more urban pop than nation average? ", avg_up < avg_up_2)

# calculate the means in cluster 3
avg_murder_3 <- mean(ds$Murder[cut.ds == 3])
avg_assault_3 <- mean(ds$Assault[cut.ds == 3])
avg_up_3 <- mean(ds$UrbanPop[cut.ds == 3])
avg_rape_3 <- mean(ds$Rape[cut.ds == 3])

# spit out some information for cluster 2
paste("C3 has more murders than nation average? ", avg_murder < avg_murder_3)
paste("C3 has more assaults than nation average? ", avg_assault < avg_assault_3)
paste("C3 has more rapes than nation average? ", avg_rape < avg_rape_3)
paste("C3 has more urban pop than nation average? ", avg_up < avg_up_3)

# PART C - heirarchically cluster the states again, this time by scaling to have a sd=1
# scale the original dataset for all of the features have standard deviation=1
ds.scaled <- scale(ds)
colMeans(ds.scaled)
apply(ds.scaled, 2, sd)

# calculate the Euclidian matrix
dist_matrix_scaled <- dist(ds.scaled)

# cluster the new scaled dataset using hclust complete linkage
hclust.scaled_complete <- hclust(dist_matrix_scaled, method="complete")
plot(hclust.scaled_complete)
abline(h = 5, col = "red")

cut.ds_scaled <- cutree(hclust.scaled_complete, h=5)

cut.ds_scaled

# which cluster corresponds to which state - 0 is false, 1 is true
table(rownames(ds), cut.ds_scaled)

# calculate the means in cluster 1
avg_murder_1 <- mean(ds$Murder[cut.ds_scaled == 1])
avg_assault_1 <- mean(ds$Assault[cut.ds_scaled == 1])
avg_up_1 <- mean(ds$UrbanPop[cut.ds_scaled == 1])
avg_rape_1 <- mean(ds$Rape[cut.ds_scaled == 1])

# spit out some information for cluster 1
paste("C1 has more murders than nation average? ", avg_murder < avg_murder_1)
paste("C1 has more assaults than nation average? ", avg_assault < avg_assault_1)
paste("C1 has more rapes than nation average? ", avg_rape < avg_rape_1)
paste("C1 has more urban pop than nation average? ", avg_up < avg_up_1)

# calculate the means in cluster 2
avg_murder_2 <- mean(ds$Murder[cut.ds_scaled == 2])
avg_assault_2 <- mean(ds$Assault[cut.ds_scaled == 2])
avg_up_2 <- mean(ds$UrbanPop[cut.ds_scaled == 2])
avg_rape_2 <- mean(ds$Rape[cut.ds_scaled == 2])

# spit out some information for cluster 2
paste("C2 has more murders than nation average? ", avg_murder < avg_murder_2)
paste("C2 has more assaults than nation average? ", avg_assault < avg_assault_2)
paste("C2 has more rapes than nation average? ", avg_rape < avg_rape_2)
paste("C2 has more urban pop than nation average? ", avg_up < avg_up_2)
