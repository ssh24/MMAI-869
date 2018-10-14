# define the USArrests as the dataset
ds <- USArrests

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
cut.ds <- cutree(hclust.complete, k=max(c(1,2,3)))

# get murder information for each cluster
table(cut.ds, ds$Murder)
# get assault information for each cluster
table(cut.ds, ds$Assault)
# get urban pop information for each cluster
table(cut.ds, ds$UrbanPop)
# get rape information for each cluster
table(cut.ds, ds$Rape)

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
cutree(hclust.scaled_complete, h=5)
