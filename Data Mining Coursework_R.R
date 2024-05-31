library(readr)

file_path <- "C:/Users/am488/Downloads/SEM 2/DATA MINING AND MACHINE LEARNING/Coursework/Data_DMML.csv"
data <- read_csv(file_path)

head(data)
tail(data)

#Exploratory Data Analysis (EDA):
# Displaying the data structure
str(data)

# Summary statistics
summary(data)
dim(data)

correlation_matrix <- cor(data)
print(correlation_matrix)

# Summary of specific columns
summary(data$Numeric_Variable)
summary(data$Categorical_Variable)

# Check for duplicate rows in the entire dataset
duplicated_rows <- duplicated(data)

# Count the number of duplicate rows
num_duplicates <- sum(duplicated_rows)
print(paste("Number of duplicate rows:", num_duplicates))

# Show duplicate rows
duplicate_data <- data[duplicated_rows, ]
print(duplicate_data)

# Remove duplicate rows
unique_data <- unique(data)

# Print the number of rows before and after removing duplicates
print(paste("Number of rows before removing duplicates:", nrow(data)))
print(paste("Number of rows after removing duplicates:", nrow(unique_data)))

boxplot(data)

# Exclude "ID" from the dataset
data_subset <- data[, -1] 

# Shortened variable names excluding "ID"
variable_names <- c("InDegree", "OutDegree", "TotalPosts", "MeanWordCount", "LikeRate", 
                    "PercentQuestions", "PercentURLs", "MeanPostsPerThread", "InitiationRatio", 
                    "MeanPostsPerSubForum", "PercBiNeighbours", "AccountAge")



# Remove 'ID' from variable names
variable_names <- c("InDeg", "OutDeg", "TP", "MWC", "LR", "PQ", "PU", "MPPT", "IR", "MPST", "PBN", "AA")

# Define shortened variable names
variable_names_short <- c("InDeg", "OutDeg", "TP", "MWC", "LR", "PQ", "PU", "MPPT", "IR", "MPST", "PBN", "AA")

# Subset the data without 'ID'
data_subset <- data[, -1]

# Create a boxplot with improved layout, legends, and shortened variable names
boxplot(data_subset, 
        col = colors,
        border = "black", 
        notch = FALSE, 
        main = "Boxplot of Data", 
        xlab = "Variables", 
        ylab = "Values", 
        names = variable_names_short,  # Use shortened variable names on x-axis
        las = 2,  
        cex.axis = 0.8,  
        font.axis = 2,
        outline = FALSE,  # Remove outliers
        horizontal = TRUE,  # Horizontal orientation
        ylim = c(0, max(data_subset) * 1.1),  # Set y-axis limit
        mar = c(7, 5, 4, 2) + 0.1)  # Adjust margins

# Add legend outside the plot area
legend("topright", 
       legend = variable_names,  # Use full variable names in legend
       fill = colors, 
       title = "Variables",
       cex = 0.8,
       bg = "white")  # Set legend background color


library(ggplot2)
variables <- c("InDegree", "OutDegree", "TotalPosts", "MeanWordCount", "LikeRate", 
               "PercentQuestions", "PercentURLs", "MeanPostsPerThread", "InitiationRatio", 
               "MeanPostsPerSubForum", "PercBiNeighbours", "AccountAge")

# Create the pair plot
pairs(data[, variables], col = "blue")



library(ggplot2)
pairs(data[, c("InDegree", "OutDegree", "TotalPosts", "LikeRate", "PercentQuestions", "PercentURLs","MeanPostsPerThread", "InitiationRatio", 
               "MeanPostsPerSubForum", "PercBiNeighbours", "AccountAge")])

# Specify the variables
variables <- c("InDegree", "OutDegree", "TotalPosts", "MeanWordCount", "LikeRate", 
               "PercentQuestions", "PercentURLs", "MeanPostsPerThread", "InitiationRatio", 
               "MeanPostsPerSubForum", "PercBiNeighbours", "AccountAge")

# Create a pair plot
pair_plot <- ggplot(data, aes_string(x = variables[1], y = variables[2])) +
  geom_point(alpha = 0.5) +
  theme_minimal() +
  labs(x = variables[1], y = variables[2]) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Add more plots for other combinations of variables
for (i in 1:length(variables)) {
  for (j in 1:length(variables)) {
    if (i != j) {
      pair_plot <- pair_plot + 
        geom_point(data = data, aes_string(x = variables[i], y = variables[j]), alpha = 0.5)
    }
  }
}

# Print the pair plot
print(pair_plot)


# Load required libraries
library(ggplot2)

# Create a histogram with modified orientation and colors
histogram <- ggplot(data, aes(x = TotalPosts)) +
  geom_histogram(binwidth = 100, fill = "lightblue", color = "black") +  # Using dark blue for histogram bars
  labs(title = "Distribution of Total Posts", x = "Count", y = "Total Posts") +  # Axis labels
  theme_minimal() +
  theme(axis.text = element_text(color = "black"),  # Black color for axis text
        axis.title = element_text(color = "black"))  # Black color for axis titles

# Change the orientation of the histogram
histogram <- histogram + coord_flip()

# Print the histogram
print(histogram)


# Create a histogram for AccountAge
histogram_account_age <- ggplot(data, aes(x = AccountAge)) +
  geom_histogram(binwidth = 1, fill = "#87CEEB", color = "black", binrange = c(0, 120)) +  # Using light blue for histogram bars
  labs(title = "Distribution of Account Age", x = "Account Age", y = "Count") +  # Axis labels
  theme_minimal() +
  theme(axis.text = element_text(color = "black"),  # Black color for axis text
        axis.title = element_text(color = "black")) +  # Black color for axis titles
  scale_fill_manual(values = custom_colors)  # Apply custom color palette

summary(data$AccountAge)
print(histogram_account_age)


# Scatter plot for InDegree vs OutDegree with enhanced appearance
scatterplot_in_out_degree <- ggplot(data, aes(x = InDegree, y = OutDegree)) +
  geom_point(color = "lightblue", size = 1, stroke = 1) +  # Scatter plot points in light blue color with a stroke
  labs(title = "InDegree vs OutDegree", x = "InDegree", y = "OutDegree") +  # Axis labels
  theme_minimal() +
  theme(axis.text = element_text(color = "black"),  # Black color for axis text
        axis.title = element_text(color = "black"))  # Black color for axis titles

print(scatterplot_in_out_degree)

# Calculate correlation matrix
correlation_matrix <- cor(data)

print(correlation_matrix)

# Create a boxplot for LikeRate
boxplot_like_rate <- ggplot(data, aes(y = LikeRate)) +
  geom_boxplot(fill = "#87CEEB", color = "black") +
  labs(title = "Boxplot of LikeRate", y = "LikeRate") +
  theme_minimal() +
  theme(axis.text = element_text(color = "black"),
        axis.title = element_text(color = "black"))

print(boxplot_like_rate)

# Create a scatterplot matrix for key metrics
scatterplot_matrix <- ggpairs(data, columns = c("InDegree", "OutDegree", "TotalPosts", "MeanWordCount", "LikeRate"),
                              title = "Scatterplot Matrix of Key Metrics")

print(scatterplot_matrix)

# Remove the "ID" column from the dataframe
data <- data[, !(names(data) %in% c("ID"))]

# Calculate the correlation matrix
correlation_matrix <- cor(data)

library(corrplot)

# Create the heatmap
heatmap <- corrplot(correlation_matrix, method = "color", 
                    col = colorRampPalette(c("red", "yellow", "green"))(10),  # Color palette
                    addCoef.col = "black", # Add coefficient color
                    number.cex = 0.7, # Coefficient size
                    tl.col = "black", # Text label color
                    tl.srt = 45, # Text label rotation angle
                    diag = FALSE,)

print(heatmap)


# Unsupervised Model: Clustering
clustering_data <- data[, c("InDegree", "OutDegree", "TotalPosts", "MeanWordCount", "LikeRate", "PercentQuestions", "PercentURLs", "MeanPostsPerThread", "InitiationRatio", "MeanPostsPerSubForum", "PercBiNeighbours", "AccountAge")]

# Standardize the data 
scaled_data <- scale(clustering_data)
print(scaled_data)

wss <- numeric(10)  # Initialize vector to store within-cluster sum of squares (WCSS)

# Calculate WCSS for different values of k
for (i in 1:10) {
  kmeans_model <- kmeans(scaled_data, centers = i)
  wss[i] <- kmeans_model$tot.withinss
}

# Plot the elbow curve
elbow_df <- data.frame(k = 1:10, WCSS = wss)  # Data frame for plotting
elbow_plot <- ggplot(elbow_df, aes(x = k, y = WCSS)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "Elbow Method for K-means Clustering",
       x = "Number of Clusters (k)",
       y = "Within-Cluster Sum of Squares (WCSS)") +
  theme_minimal()

# Find the elbow point 
diff_wss <- c(0, diff(wss))  # Calculate differences in WCSS
elbow_point <- which(diff(diff_wss) == max(diff(diff_wss))) + 1  # Index of the elbow point

# Add a vertical line at the elbow point
elbow_plot <- elbow_plot +
  geom_vline(xintercept = elbow_point, linetype = "dashed", color = "green") +
  geom_point(data = elbow_df[elbow_point, ], aes(x = k, y = WCSS), color = "green", size = 3)

# Print the elbow plot
print(elbow_plot)

# Optimal number of clusters
optimal_clusters <- elbow_point

# Perform K-means clustering with optimal number of clusters
kmeans_model <- kmeans(scaled_data, centers = optimal_clusters)
print(kmeans_model)

# Plot the clusters
# Define pastel colors for clusters
cluster_colors <- c("#a6cee3", "#b2df8a", "#fb9a99")  # Pastel blue, green, pink

# Plot the clusters
plot(scaled_data, col = cluster_colors[kmeans_model$cluster], main = "K-means Clustering", pch = 20)
points(kmeans_model$centers, col = 1:3, pch = 8, cex = 2)

# Add legend
legend("topright", legend = paste("Cluster", 1:3), col = cluster_colors, pch = 20)


# Calculate silhouette scores
silhouette_scores <- silhouette(kmeans_model$cluster, dist(scaled_data))

# Print silhouette scores
print(silhouette_scores)

# Compute average silhouette width
avg_width <- mean(silhouette_scores[, "sil_width"])

# Print average silhouette width
cat("Average silhouette width:", avg_width, "\n")

# Create color palette for clusters
cluster_colors <- c("red", "blue", "green")

# Plot silhouette graph
  plot(silhouette_scores, col = cluster_colors[kmeans_model$cluster], border = NA,
       xlab = "Silhouette Score", ylab = "Cluster", main = "Silhouette Plot")

# Add legend
legend("topright", legend = 1:optimal_clusters, fill = cluster_colors, title = "Cluster", cex = 0.8)

# Add average silhouette width line
abline(h = avg_width, lty = 2, col = "black")

# Add labels
text(-0.2, 1:length(kmeans_model$cluster), kmeans_model$cluster, pos = 4, col = cluster_colors[kmeans_model$cluster])

# Plot silhouette graph
plot(silhouette_scores, col = cluster_colors[kmeans_model$cluster], border = NA,
     xlab = "Silhouette Score", ylab = "Cluster", main = "Silhouette Plot")

# Add legend
legend("topright", legend = 1:optimal_clusters, fill = cluster_colors, title = "Cluster", cex = 0.8)

# Add average silhouette width line
abline(h = avg_width, lty = 2, col = "black")

# Add labels
text(-0.2, 1:length(kmeans_model$cluster), kmeans_model$cluster, pos = 4, col = cluster_colors[kmeans_model$cluster])

# Add grid lines
grid()

# Adjust margins
par(mar=c(5, 4, 4, 8) + 0.1)

# Rotate y-axis labels for better readability
axis(2, at = 1:length(kmeans_model$cluster), labels = kmeans_model$cluster, las = 1)

# Load necessary library
library(factoextra)

# Compute K-means clustering
kmeans_model <- kmeans(scaled_data, centers = 3, nstart = 25)

# Extract cluster labels
cluster_labels <- kmeans_model$cluster

# Compute silhouette width
sil_width <- silhouette(cluster_labels, dist(scaled_data))

# Plot silhouette width
fviz_silhouette(sil_width, palette = c("blue", "red", "green"), border.color = NA)


# Compute average silhouette width
avg_width <- mean(silhouette_scores[, "sil_width"])

# Print average silhouette width
cat("Average silhouette width:", avg_width, "\n")



# Calculate the correlation between Clusters and the specified features
correlation_with_clusters <- cor(clustered_data)[, "Clusters"]

# Select the features for the visualization
features <- c("InDegree", "OutDegree", "TotalPosts", "MeanWordCount", 
              "LikeRate", "PercentQuestions", "PercentURLs", "MeanPostsPerThread", 
              "InitiationRatio", "MeanPostsPerSubForum", "PercBiNeighbours", "AccountAge")

# Filter the correlation values for the selected features
correlation_with_clusters <- correlation_with_clusters[features]

# Sort the correlation values in descending order
sorted_correlation <- sort(correlation_with_clusters, decreasing = TRUE)

# Plot the correlation values
barplot(sorted_correlation, main = "Correlation with Clusters", 
        xlab = "Features", ylab = "Correlation", col = "lightblue")


cluster_centers <- aggregate(. ~ cluster, data = cbind(scaled_data, cluster = as.factor(kmeans_model$cluster)), FUN = mean)

print(cluster_centers)

##Explore the centres
centers
library(ggplot2)


# Plot using ggplot
ggplot(centers_long, aes(x = Variable, y = Value, fill = as.factor(cluster))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Cluster Centers", x = "Variable", y = "Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

library(ggplot2)

# Convert the centers dataframe into long format
centers_long <- tidyr::pivot_longer(centers, cols = -c(cluster), names_to = "Variable", values_to = "Value")

# Plot using ggplot
ggplot(centers_long, aes(x = Value, y = Variable, fill = as.factor(cluster))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Center of variables for all three clusters", x = "Value", y = "Variable") +
  theme_minimal() +
  theme(axis.text.y = element_text(angle = 0, hjust = 1))


#SUPERVISED MODEL

library(randomForest)

# Define the target variable
target_variable <- kmeans_model$cluster

# Train Random Forest model
rf_model <- randomForest(x = scaled_data, y = as.factor(target_variable), ntree = 500)
print(rf_model)

# Generate predictions
rf_predictions <- predict(rf_model, scaled_data)

# Calculate accuracy
accuracy <- sum(rf_predictions == target_variable) / length(target_variable)

print(paste("Accuracy:", accuracy * 100, "%"))






# KNN
library(class)

k <- 3

# Train KNN model
knn_model <- knn(train = scaled_data, test = scaled_data, cl = target_variable, k = k)

# Calculate accuracy
accuracy <- sum(knn_model == target_variable) / length(target_variable)


print(paste("Accuracy:", accuracy * 100, "%"))

# Generate the confusion matrix
conf_matrix <- table(knn_model, target_variable)

print(conf_matrix)



# SVM
library(e1071)

# Train SVM model
svm_model <- svm(as.factor(target_variable) ~ ., data = scaled_data, kernel = "radial", cost = 1, gamma = 1)

# Generate predictions
svm_predictions <- predict(svm_model, scaled_data)

# Calculate accuracy
accuracy <- sum(svm_predictions == target_variable) / length(target_variable)

print(paste("Accuracy:", accuracy * 100, "%"))

library(caret)

# Create a confusion matrix
conf_matrix <- confusionMatrix(as.factor(svm_predictions), as.factor(target_variable))

print(conf_matrix)




