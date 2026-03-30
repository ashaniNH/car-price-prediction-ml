attach(trainingModel)
attach(testingModel)
str(trainingModel)


######### Factor Analysis ################
 # Load necessary libraries
#install.packages("FactoMineR")
#install.packages("factoextra")
#install.packages("data.table")
# Load necessary libraries
library(FactoMineR)  # For performing FAMD
library(factoextra)  # For visualizing results
library(data.table)  # For data manipulation

# Assuming your dataset is stored in a variable called `trainingModel`

# Step 1: Perform FAMD analysis
res.famd <- FAMD(trainingModel, graph = FALSE)

# Step 2: Print FAMD summary
summary(res.famd)

# Step 3: Generate Scree Plot (Explained Variance)
fviz_screeplot(res.famd, addlabels = TRUE, ylim = c(0, 50))  # Adjust ylim as needed

# Step 4: Visualize Factor Map of Variables (Representation in Factor Space)
fviz_famd_var(res.famd, repel = TRUE)

# Step 5: Visualize Contribution of Variables to the First Dimension (Dim 1)
fviz_contrib(res.famd, "var", axes = 1)

# Step 6: Visualize Contribution of Variables to the Second Dimension (Dim 2)
fviz_contrib(res.famd, "var", axes = 2)

# Step 7: Factor Map with Gradient Colors Based on Cos2 Values (Quality of Representation)
fviz_famd_var(res.famd, "quanti.var", 
              col.var = "cos2",  # Color based on cos2 values
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),  # Color scale
              repel = TRUE)  # Avoid label overlap

# Step 8: Extract Contributions of Variables
dimdesc(res.famd)

# Step 9: Extract FAMD factor scores for clustering
famd_coords <- res.famd$ind$coord  # Coordinates of individuals in factor space

# Step 10: Determine Optimal Number of Clusters using Elbow Method
fviz_nbclust(famd_coords, kmeans, method = "wss") + 
  ggtitle("Elbow Method for Optimal Number of Clusters")

# Step 11: Determine Optimal Number of Clusters using Silhouette Method
fviz_nbclust(famd_coords, kmeans, method = "silhouette") + 
  ggtitle("Silhouette Method for Optimal Number of Clusters")

# Step 12: Perform K-Means Clustering (Choose k based on elbow or silhouette method)
set.seed(123)  # Ensure reproducibility
k <- 3  # Set the optimal number of clusters (adjust based on the elbow/silhouette plot)
kmeans_res <- kmeans(famd_coords, centers = k, nstart = 25)

# Step 13: Add cluster labels to the dataset
#trainingModel$cluster <- as.factor(kmeans_res$cluster)

# Step 14: Visualize Cluster Plot in FAMD Factor Space
fviz_cluster(kmeans_res, data = famd_coords, 
             geom = "point", ellipse.type = "convex",
             palette = "jco", repel = TRUE) +
  labs(title = "Cluster Plot Based on FAMD Results")

# Step 15: Inspect the clustered dataset
head(trainingModel)