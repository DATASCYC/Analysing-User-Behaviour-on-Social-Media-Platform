# Load the dataset into R
listings <- read.csv("listings.csv")

# Display the structure of the dataset
str(listings)

# Display the first few rows of the dataset
head(listings)
tail(listings)


# Check for duplicate rows in the entire dataset
duplicate_rows <- listings[duplicated(listings), ]

# Count the number of duplicate rows
num_duplicates <- nrow(duplicate_rows)

# Show duplicate rows
print(duplicate_rows)

# Remove duplicate rows
listings_unique <- unique(listings)

# Print the number of rows before and after removing duplicates
num_rows_before <- nrow(listings)
num_rows_after <- nrow(listings_unique)

print(paste("Number of rows before removing duplicates:", num_rows_before))
print(paste("Number of rows after removing duplicates:", num_rows_after))



# Calculate missing values
missing_values <- colSums(is.na(listings))

# Print the number of missing values for each column
print(missing_values)

# Filter out columns with missing values
missing_values_filtered <- missing_values[missing_values > 0]

# Plot missing values for columns with missing values using ggplot2
# Convert missing_values_filtered into a data frame
missing_values_df <- data.frame(variable = names(missing_values_filtered),
                                missing_percentage = missing_values_filtered)

# Plot using ggplot
library(ggplot2)

ggplot(missing_values_df, aes(x = reorder(variable, -missing_percentage), y = missing_percentage)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Missing Values in Dataset",
       x = "Variables",
       y = "Percentage Missing") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Convert price to numeric format
listings$price <- as.numeric(gsub("[$,]", "", listings$price))

# Check the structure of the "price" column after conversion
str(listings$price)


# Step 1: Identify rows with missing price values
missing_price_indices <- which(is.na(listings$price))

# Step 2: Extract neighborhood information for those rows
missing_property_type <- listings$property_type[missing_price_indices]


# Step 3: Calculate the average price for each neighborhood, excluding missing values
property_type_avg_price <- tapply(listings$price[!is.na(listings$price)], listings$property_type[!is.na(listings$price)], mean)

# Step 4: Impute missing price values with the average price of their respective neighborhoods
for (i in 1:length(missing_price_indices)) {
  missing_index <- missing_price_indices[i]
  property_type <- missing_property_type[i]
  imputed_price <- property_type_avg_price [property_type]
  listings$price[missing_index] <-imputed_price
}




# Check for missing values in the "price" column
missing_price <- sum(is.na(listings$price))

# Print the number of missing values for "price"
print(paste("Number of missing values for 'price':", missing_price))


library(ggplot2)

# Create a data frame with price and its frequency
price_freq <- as.data.frame(table(listings$price))


# Rename columns for clarity
colnames(price_freq) <- c("Price", "Frequency")

# Create a scatterplot of price vs. frequency
scatterplot <- ggplot(price_freq, aes(x = Price, y = Frequency)) +
  geom_point(color = "#FF5733", alpha = 0.6, size = 2) + # Adjust colors, transparency, and point size
  labs(title = "Price vs. Frequency", x = "Price", y = "Frequency") +
  theme_minimal() + # Apply a minimal theme
  theme(plot.title = element_text(hjust = 0.5), # Center the title
        axis.text = element_text(size = 10),   # Adjust axis text size
        axis.title = element_text(size = 12))  # Adjust axis title size

# Display the scatterplot
scatterplot

# 1. Descriptive Statistics
summary(listings$price)  # Summary statistics

# 2. Outlier Detection
# Visualize the distribution of prices using a boxplot
boxplot(listings$price, main = "Boxplot of Prices")


# Explore Factors Influencing Pricing

#Neighbourhood Analysis
library(dplyr)
library(ggplot2)

# 1. Aggregate data by neighborhood
neighborhood_summary <- listings %>%
  group_by(neighbourhood_cleansed) %>%
  summarize(
    avg_price = mean(price, na.rm = TRUE),
    median_price = median(price, na.rm = TRUE),
    avg_minimum_nights = mean(minimum_nights, na.rm = TRUE),
    num_listings = n()
  ) %>%
  arrange(desc(avg_price))  # Arrange neighborhoods by average price

# Print the summary of aggregated data
print(neighborhood_summary)


# Group the data by the "neighbourhood_cleansed" variable and calculate the number of listings in each neighborhood
neighborhood_listings <- listings %>%
  group_by(neighbourhood_cleansed) %>%
  summarize(num_listings = n())

# Plot the graph for listings vs. neighbourhood cleansed
ggplot(neighborhood_listings, 
       aes(x = reorder(neighbourhood_cleansed, -num_listings), y = num_listings, fill = num_listings)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_viridis_c(option = "plasma", direction = 1, begin = 0.2, end = 0.8) +
  labs(title = "Number of Listings vs. Neighbourhood Cleansed",
       x = "Neighbourhood Cleansed",
       y = "Number of Listings") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip()

# Group the data by the "neighbourhood_cleansed" variable and calculate the average rating in each neighborhood
neighborhood_avg_rating <- listings %>%
  group_by(neighbourhood_cleansed) %>%
  summarize(avg_rating = mean(review_scores_rating, na.rm = TRUE)) %>%
  arrange(desc(avg_rating))  # Arrange neighborhoods from highest to lowest average rating

# Plot the line graph for neighbourhood cleansed vs. rating
ggplot(neighborhood_avg_rating, aes(x = reorder(neighbourhood_cleansed, avg_rating), y = avg_rating, group = 1)) +
  geom_line(color = "skyblue", size = 1) +
  geom_point(color = "darkblue", size = 3) +  # Add points for each neighborhood
  geom_text(aes(label = round(avg_rating, 2)), hjust = -0.2, vjust = 0.5, size = 3, color = "black") +  # Add text labels beside the points
  labs(title = "Neighbourhood Cleansed vs. Average Rating",
       x = "Neighbourhood Cleansed",
       y = "Average Rating") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip()





# Visualize average price by neighborhood
library(viridis)

# Visualize average price by neighborhood
ggplot(neighborhood_summary, aes(x = reorder(neighbourhood_cleansed, -avg_price), y = avg_price, fill = avg_price)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_viridis_c(option = "plasma", direction = 1, begin = 0.2, end = 0.8) +
  labs(title = "Average Price by Neighborhood",
       x = "Neighborhood",
       y = "Average Price") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip()


# Visualize average minimum nights by neighborhood cleansed
ggplot(neighborhood_summary, aes(x = reorder(neighbourhood_cleansed, -avg_minimum_nights), y = avg_minimum_nights, fill = avg_minimum_nights)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_viridis_c(option = "plasma", direction = 1, begin = 0.2, end = 0.8) +
  labs(title = "Average Minimum Nights by Neighborhood",
       x = "Neighborhood",
       y = "Average Minimum Nights") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip()


# Visualize number of listings by neighborhood
ggplot(neighborhood_summary, aes(x = reorder(neighbourhood_cleansed, -num_listings), y = num_listings, fill = num_listings)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_viridis_c(option = "plasma", direction = 1, begin = 0.2, end = 0.8) +
  labs(title = "Number of Listings by Neighborhood",
       x = "Neighborhood",
       y = "Number of Listings") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip()
 

# Room Type Analysis: Create a bar plot of room types
ggplot(listings, aes(x = room_type, fill = room_type)) +
  geom_bar() +
  labs(title = "Distribution of Room Types", x = "Room Type", y = "Frequency") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), # Center the title
        axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels for better readability


# Room Type vs Cleansed Neighbourhood
ggplot(listings, aes(x = neighbourhood_cleansed, fill = room_type)) +
  geom_bar(position = "dodge") +
  labs(title = "Room Type by Cleansed Neighbourhood", x = "Cleansed Neighbourhood", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_viridis_d(option = "plasma", begin = 0.2, end = 0.8)  # Adjust color scale if needed


# Calculate the frequency of room types by cleansed neighbourhood
room_type_counts <- listings %>%
  group_by(neighbourhood_cleansed, room_type) %>%
  summarise(count = n()) %>%
  arrange(neighbourhood_cleansed, desc(count))

# Reorder neighbourhoods based on the total counts of room types
neighbourhood_order <- room_type_counts %>%
  group_by(neighbourhood_cleansed) %>%
  summarise(total_count = sum(count)) %>%
  arrange(desc(total_count)) %>%
  pull(neighbourhood_cleansed)

# Create the bar plot
ggplot(room_type_counts, aes(x = factor(neighbourhood_cleansed, levels = neighbourhood_order), y = count, fill = room_type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(label = count), position = position_dodge(width = 0.7), vjust = -0.5, size = 3) +  # Add count labels
  labs(title = "Room Type Distribution by Neighbourhood", x = "Neighbourhood", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_viridis_d(option = "plasma", begin = 0.2, end = 0.8) +  # Adjust color scale if needed
  guides(fill = guide_legend(title = "Room Type"))  # Add legend title

# Load necessary libraries
library(ggplot2)

# Check the structure of the dataset
str(listings)



#Geospatial Visualisation
library(ggplot2)


# Check the structure of the selected columns to ensure they are numeric
str(listings[, c("price", "review_scores_rating", "minimum_nights")])

# Calculate correlation matrix
correlation_matrix <- cor(listings[, c("price", "review_scores_rating", "minimum_nights")], use = "pairwise.complete.obs")

# Print correlation matrix
print(correlation_matrix)

# Calculate median price by property type
mean_prices <- listings %>%
  group_by(property_type) %>%
  summarize(median_price = mean(price, na.rm = TRUE))

# Plot median price by property type with rotated x-axis labels, no legends, and full screen
ggplot(mean_prices, aes(x = property_type, y = median_price, fill = property_type)) +
  geom_bar(stat = "identity") +
  labs(title = "Mean Price by Property Type", x = "Property Type", y = "Mean Price") +
  scale_fill_viridis_d(option = "plasma", begin = 0.2, end = 0.8) +  # Adjust color scale
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
        axis.text.y = element_text(size = 8),  # Decrease the size of y-axis labels
        legend.position = "none") +  # Remove legends
  coord_flip() +  # Flip coordinates to make the graph full screen
  theme(
    plot.background = element_rect(fill = "white", color = NA),  # Set background color
    panel.background = element_rect(fill = "white", color = NA),  # Set panel background color
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    axis.line = element_line(color = "black")  # Set axis line color
  ) +
  labs(title = "Mean Price by Property Type") +
  theme(plot.title = element_text(hjust = 0.5))  # Center the title



# Calculate median price by room type
median_price_by_room <- listings %>%
  group_by(room_type) %>%
  summarise(median_price = median(price, na.rm = TRUE))

# Create a bar plot of median price by room type
ggplot(median_price_by_room, aes(x = room_type, y = median_price, fill = room_type)) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(title = "Median Price by Room Type", x = "Room Type", y = "Median Price") +
  scale_fill_viridis_d(option = "plasma", begin = 0.2, end = 0.8) +  # Adjust color scale
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
        legend.position = "none",  # Remove legend
        plot.title = element_text(hjust = 0.5))  # Center the title





# Filter out missing or out-of-range values
listings_filtered <- listings[complete.cases(listings$review_scores_rating, listings$price), ]


# Load necessary libraries
library(ggplot2)
library(viridis)
library(ggrepel)

# Filter out missing or out-of-range values
listings_filtered <- listings[complete.cases(listings$review_scores_rating, listings$neighbourhood_cleansed), ]

# Calculate the average review score for each cleansed neighborhood
neighborhood_avg_rating <- listings_filtered %>%
  group_by(neighbourhood_cleansed) %>%
  summarize(avg_rating = mean(review_scores_rating, na.rm = TRUE)) %>%
  arrange(desc(avg_rating))  # Arrange neighborhoods from highest to lowest average rating

# Add a lagged column for plotting lines
neighborhood_avg_rating <- neighborhood_avg_rating %>%
  mutate(prev_neighbourhood = lag(neighbourhood_cleansed),
         prev_avg_rating = lag(avg_rating))

# Create a scatterplot with a line and labels
ggplot(neighborhood_avg_rating, aes(x = reorder(neighbourhood_cleansed, avg_rating), y = avg_rating, label = round(avg_rating, 2))) +
  geom_point(color = viridis(20)[10], size = 3) +  # Scatterplot
  geom_line(aes(x = reorder(neighbourhood_cleansed, avg_rating), y = avg_rating, group = 1), color = viridis(20)[15], size = 1.2, alpha = 0.8) +  # Line plot
  geom_text_repel(size = 3) +  # Labels for each point with repelling
  scale_color_viridis_c(option = "plasma", direction = 1, begin = 0.2, end = 0.8) +  # Color scale
  labs(title = "Average Review Scores by Cleansed Neighborhood",
       x = "Cleansed Neighborhood",
       y = "Average Review Scores") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip()


# Load necessary libraries
library(ggplot2)
library(viridis)
library(ggrepel)

# Filter out missing or out-of-range values
listings_filtered <- listings[complete.cases(listings$review_scores_rating, listings$neighbourhood_cleansed), ]

# Calculate the average price for each cleansed neighborhood
neighborhood_avg_price <- listings_filtered %>%
  group_by(neighbourhood_cleansed) %>%
  summarize(avg_price = mean(price, na.rm = TRUE)) %>%
  arrange(desc(avg_price))  # Arrange neighborhoods from highest to lowest average price

# Plot the bar chart for average price by neighborhood with plasma and viridis aesthetics
ggplot(neighborhood_avg_price, aes(x = reorder(neighbourhood_cleansed, avg_price), y = avg_price, fill = avg_price)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_c(option = "plasma", direction = 1, begin = 0.2, end = 0.8) +  # Set fill color palette to plasma
  labs(title = "Average Price by Neighborhood",
       x = "Cleansed Neighborhood",
       y = "Average Price (USD)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip()  # Flip coordinates to make the graph more readable















# Relationship Between Price and Review Scores: Retired couples may be interested in finding accommodations that offer good value for money. Exploring the relationship between price and review scores can help them identify listings that provide a satisfactory experience at a reasonable price.

# Filter out missing values for price and review_scores_rating
filtered_data <- listings[complete.cases(listings$price, listings$review_scores_rating), ]

# Create a scatter plot of price vs. review scores with a smoother trend line
ggplot(filtered_data, aes(x = price, y = review_scores_rating, color = price)) +
  geom_point(alpha = 0.7, size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "skyblue", size = 1) +
  scale_color_viridis_c(option = "plasma", name = "Price", labels = scales::dollar, guide = guide_colorbar(reverse = TRUE)) +
  labs(title = "Relationship Between Price and Review Scores",
       x = "Price (USD)",
       y = "Review Scores") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        legend.position = "right",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12)) +
  guides(color = guide_colorbar(title.position = "top", title.hjust = 0.5, label.position = "right", nbin = 10))




library(ggplot2)

# Filter the dataset by price range
filtered_data <- listings %>%
  filter(price <= 150)

# Group the filtered data by neighborhood and count the number of listings
neighborhood_counts <- filtered_data %>%
  group_by(neighbourhood_cleansed) %>%
  summarise(listing_count = n())

# Sort the neighborhoods by listing count in descending order
sorted_neighborhoods <- neighborhood_counts %>%
  arrange(desc(listing_count))

library(viridis)



# Plot the top neighborhoods with the most listings in the price range using a different color palette
ggplot(sorted_neighborhoods, aes(x = reorder(neighbourhood_cleansed, listing_count), y = listing_count, fill = listing_count)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_viridis_c() +
  labs(x = "Neighborhood", y = "Number of Listings", title = "Top Neighborhoods with Most Listings (Price <= $150)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip()


# Define custom colors for each room type
room_colors <- c("Entire home/apt" = "#1f77b4",   # Blue for Entire home/apt
                 "Private room" = "#ff7f0e",      # Orange for Private room
                 "Shared room" = "#2ca02c",       # Green for Shared room
                 "Hotel room" = "#d62728")        # Red for Hotel room

# Filter the dataset by price range
filtered_data <- listings %>%
  filter(price <= 150)

# Plot the number of listings vs. room type with custom colors
ggplot(filtered_data, aes(x = n(), y = room_type, fill = room_type)) +
  geom_bar(stat = "identity") +  # Bar plot
  scale_fill_manual(values = room_colors) +  # Assign custom colors
  labs(x = "Number of Listings", y = "Room Type", title = "Number of Listings by Room Type") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 12))  # Adjust y-axis label size

# Filter the dataset by price range
filtered_data <- listings %>%
  filter(price <= 150)

# Calculate the count of listings by room type
room_counts <- filtered_data %>%
  group_by(room_type) %>%
  summarise(count = n())

# Plot the number of listings vs. room type
ggplot(room_counts, aes(x = count, y = room_type, fill = room_type)) +
  geom_bar(stat = "identity") +  # Bar plot
  labs(x = "Number of Listings", y = "Room Type", title = "Number of Listings by Room Type") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 12))  # Adjust y-axis label size

# Step 1: Filter the data for the top 3 popular neighborhoods
top_neighborhoods <- neighborhood_listings %>%
  tail(3)

# Step 2: Filter the data to include only the room types private, home/apt, and hotel
filtered_data <- listings %>%
  filter(neighbourhood_cleansed %in% top_neighborhoods$neighbourhood_cleansed &
           room_type %in% c("Private room", "Entire home/apt", "Hotel room"))

# Step 3: Group the data by neighborhood and room type, and calculate the average price
price_analysis <- filtered_data %>%
  group_by(neighbourhood_cleansed, room_type) %>%
  summarise(avg_price = mean(price, na.rm = TRUE))

# Step 4: Plot the bar graph
ggplot(price_analysis, aes(x = neighbourhood_cleansed, y = avg_price, fill = room_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Price Analysis of Accommodations by Room Type and Neighborhood",
       x = "Neighborhood",
       y = "Average Price",
       fill = "Room Type") +
  scale_fill_manual(values = c("Private room" = "#2ca02c", "Entire home/apt" = "red", "Hotel room" = "#6B40BF")) +
  theme_minimal()




# Define function to categorize property types
categorize_property_type <- function(property_type) {
  if (property_type %in% c("Apartment", "Condominium", "Loft")) {
    return("Apartment")
  } else if (property_type %in% c("House", "Townhouse")) {
    return("House")
  } else {
    return("Other")
  }
}

# Apply the function to categorize property types
listings$property_category <- sapply(listings$property_type, categorize_property_type)

# Define function to categorize room types
categorize_room_type <- function(room_type) {
  if (room_type %in% c("Entire home/apt")) {
    return("Entire home/apt")
  } else if (room_type %in% c("Private room")) {
    return("Private room")
  } else {
    return("Other")
  }
}

# Apply the function to categorize room types
listings$room_category <- sapply(listings$room_type, categorize_room_type)

library(ggplot2)

# Plot the graph
ggplot(listings, aes(x = property_category, fill = room_category)) +
  geom_bar(position = "fill") +
  labs(x = "Property Type", y = "Proportion", title = "Proportion of Room Types by Property Type") +
  scale_fill_manual(values = c("blue", "green", "red")) + # Customizing fill colors
  theme_minimal()










# Filter out missing or out-of-range values
listings_filtered <- listings[complete.cases(listings$review_scores_rating, listings$neighbourhood_cleansed), ]

# Calculate the average review score for each cleansed neighborhood
neighborhood_avg_rating <- listings_filtered %>%
  group_by(neighbourhood_cleansed) %>%
  summarize(avg_rating = mean(review_scores_rating, na.rm = TRUE)) %>%
  arrange(desc(avg_rating))  # Arrange neighborhoods from highest to lowest average rating

# Select the top 10 neighborhoods
top_neighborhoods <- head(neighborhood_avg_rating, 10)

# Create a line graph for average review scores by neighborhood
ggplot(top_neighborhoods, aes(x = reorder(neighbourhood_cleansed, avg_rating), y = avg_rating, group = 1)) +
  geom_line(color = "skyblue", size = 1.5) +  # Line plot
  geom_point(color = "darkblue", size = 3) +  # Points for each neighborhood
  geom_text(aes(label = round(avg_rating, 2)), hjust = -0.2, vjust = 0.5, size = 3, color = "black") +  # Labels for points
  labs(title = "Average Review Scores for Top 10 Neighborhoods",
       x = "Neighborhood",
       y = "Average Review Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip()
`
# Load required libraries
library(ggplot2)

# Create a dataframe with the provided data
neighborhoods <- c("IJburg - Zeeburgereiland", "Slotervaart", "De Pijp - Rivierenbuurt", 
                   "Zuid", "Watergraafsmeer", "Oud-Oost", "De Baarsjes - Oud-West", 
                   "Noord-West", "Westerpark", "Bos en Lommer")
avg_prices <- c(120, 110, 130, 140, 125, 135, 145, 115, 130, 120) # Replace with actual average prices

cost_of_living <- data.frame(neighbourhoods, avg_prices)

# Create a bar plot for the average price of listings by neighborhood
ggplot(cost_of_living, aes(x = reorder(neighbourhoods, avg_prices), y = avg_prices)) +
  geom_bar(stat = "identity", fill = "skyblue", width = 0.5) +  # Bar plot
  geom_text(aes(label = paste("$", avg_prices)), vjust = -0.3, size = 4, color = "black") +  # Labels for bars
  labs(title = "Comparative Cost of Living Across Top 10 Neighborhoods",
       x = "Neighborhood",
       y = "Average Price ($)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip()


library(dplyr)
library(ggplot2)

# Step 1: Filter the data to include only the top 10 neighborhoods based on the number of listings
top_10_neighborhoods <- neighborhood_listings %>%
  arrange(desc(num_listings)) %>%
  head(10) %>%
  select(neighbourhood_cleansed)

# Step 2: Filter the data to include only listings from these top 10 neighborhoods
filtered_data <- listings %>%
  filter(neighbourhood_cleansed %in% top_10_neighborhoods$neighbourhood_cleansed)

# Step 3: Group the filtered data by neighborhood and calculate the average review score and average price
neighborhood_summary <- filtered_data %>%
  group_by(neighbourhood_cleansed) %>%
  summarize(avg_review_score = mean(review_scores_rating, na.rm = TRUE),
            avg_price = mean(price, na.rm = TRUE))

# Step 4: Plot the average review score by neighborhood
ggplot(neighborhood_summary, aes(x = reorder(neighbourhood_cleansed, avg_review_score), y = avg_review_score)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Average Review Score by Neighborhood",
       x = "Neighborhood",
       y = "Average Review Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Sort the neighborhoods by the number of listings in descending order
top_neighborhoods <- neighborhood_listings %>%
  arrange(desc(num_listings)) %>%
  head(10)

# Display the top 10 neighborhoods
print(top_neighborhoods)

# Filter the data to include only listings from the top 10 neighborhoods
filtered_data <- listings %>%
  filter(neighbourhood_cleansed %in% top_neighborhoods$neighbourhood_cleansed)

# Calculate the average review score and average price for each neighborhood
neighborhood_summary <- filtered_data %>%
  group_by(neighbourhood_cleansed) %>%
  summarize(
    avg_review_score = mean(review_scores_rating, na.rm = TRUE),
    avg_price = mean(price, na.rm = TRUE)
  ) %>%
  arrange(desc(avg_review_score))  # Arrange neighborhoods by average review score

# Print the summary of average review score and average price for the top 10 neighborhoods
print(neighborhood_summary)


# Load necessary libraries
library(ggplot2)

# Visualize average review score and average price for the top 10 neighborhoods in a line graph
ggplot(neighborhood_summary, aes(x = reorder(neighbourhood_cleansed, avg_review_score), y = avg_review_score, group = 1)) +
  geom_line(color = "skyblue", size = 1.2) +  # Line plot for average review score
  geom_point(color = "darkblue", size = 3) +  # Add points for each neighborhood
  geom_text(aes(label = round(avg_review_score, 2)), hjust = -0.2, vjust = 0.5, size = 3, color = "black") +  # Add text labels beside the points
  geom_line(aes(x = reorder(neighbourhood_cleansed, avg_price), y = avg_price, group = 1), color = "orange", size = 1.2, linetype = "dashed") +  # Line plot for average price
  geom_point(aes(x = reorder(neighbourhood_cleansed, avg_price), y = avg_price), color = "darkorange", size = 3) +  # Add points for average price
  geom_text(aes(label = paste("$", round(avg_price, 2))), hjust = -0.2, vjust = 0.5, size = 3, color = "black") +  # Add text labels beside the points for average price
  labs(title = "Average Review Score and Average Price for Top 10 Neighborhoods",
       x = "Neighborhood",
       y = "Average Score / Price") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(sec.axis = sec_axis(~., name = "Average Price (USD)"))  # Add secondary y-axis for average price

# Load necessary libraries
library(ggplot2)

# Visualize average review score and average price for the top 10 neighborhoods in a bar graph
ggplot(neighborhood_summary, aes(x = reorder(neighbourhood_cleansed, avg_review_score), y = avg_review_score)) +
  geom_bar(stat = "identity", fill = "skyblue") +  # Bar plot for average review score
  geom_text(aes(label = round(avg_review_score, 2)), vjust = -0.5, size = 3, color = "black") +  # Add text labels above the bars for average review score
  geom_bar(aes(x = reorder(neighbourhood_cleansed, avg_price), y = avg_price), stat = "identity", fill = "orange") +  # Bar plot for average price
  geom_text(aes(label = paste("$", round(avg_price, 2))), vjust = -0.5, size = 3, color = "black") +  # Add text labels above the bars for average price
  labs(title = "Average Review Score and Average Price for Top 10 Neighborhoods",
       x = "Neighborhood",
       y = "Average Score / Price") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(sec.axis = sec_axis(~., name = "Average Price (USD)"))  # Add secondary y-axis for average price





# Load necessary libraries
library(dplyr)
library(ggplot2)

# Filter the dataset by price range
filtered_data <- listings %>%
  filter(price <= 150)

# Group the filtered data by neighborhood and count the number of listings
neighborhood_counts <- filtered_data %>%
  group_by(neighbourhood_cleansed) %>%
  summarise(listing_count = n())

# Sort the neighborhoods by listing count in descending order
sorted_neighborhoods <- neighborhood_counts %>%
  arrange(desc(listing_count))

# Extract the top 10 neighborhoods
top_10_neighborhoods <- sorted_neighborhoods %>%
  top_n(10)

# Join the top 10 neighborhoods with their average review scores
neighborhood_summary <- inner_join(top_10_neighborhoods, avg_review_scores, by = "neighbourhood_cleansed")

# Plot the top 10 neighborhoods with their review scores
ggplot(neighborhood_summary, aes(x = reorder(neighbourhood_cleansed, listing_count), y = avg_review_score)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = round(avg_review_score, 2)), vjust = -0.5, size = 3, color = "black") +
  labs(title = "Top 10 Neighborhoods by Listing Count (Price <= $150)",
       x = "Neighborhood",
       y = "Average Review Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Plot the line graph for listing count and average review scores
ggplot(neighborhood_summary, aes(x = reorder(neighbourhood_cleansed, listing_count))) +
  geom_line(aes(y = listing_count, color = "Listing Count")) +
  geom_point(aes(y = listing_count, color = "Listing Count")) +
  geom_line(aes(y = avg_review_score * 100, color = "Avg. Review Score")) +
  geom_point(aes(y = avg_review_score * 100, color = "Avg. Review Score")) +
  scale_y_continuous(sec.axis = sec_axis(~./100, name = "Average Review Score")) +
  labs(title = "Listing Count and Average Review Scores by Neighborhood",
       x = "Neighborhood",
       y = "Listing Count",
       color = "Metric") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot the line graph for listing count and average review scores
ggplot(neighborhood_summary, aes(x = reorder(neighbourhood_cleansed, listing_count))) +
  geom_line(aes(y = listing_count, color = "Listing Count"), size = 1.5) +
  geom_point(aes(y = listing_count, color = "Listing Count"), size = 3) +
  geom_line(aes(y = avg_review_score * 100, color = "Avg. Review Score"), size = 1.5, linetype = "dashed") +
  geom_point(aes(y = avg_review_score * 100, color = "Avg. Review Score"), size = 3) +
  scale_y_continuous(name = "Listing Count",
                     sec.axis = sec_axis(~./100, name = "Average Review Score")) +
  labs(title = "Listing Count and Average Review Scores by Neighborhood",
       x = "Neighborhood",
       color = "Metric") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))







# Load necessary libraries
library(dplyr)
library(ggplot2)

# Filter the dataset by price range
filtered_data <- listings %>%
  filter(price <= 150)

# Group the filtered data by neighborhood and count the number of listings
neighborhood_counts <- filtered_data %>%
  group_by(neighbourhood_cleansed) %>%
  summarise(listing_count = n())

# Sort the neighborhoods by listing count in descending order
sorted_neighborhoods <- neighborhood_counts %>%
  arrange(desc(listing_count))

# Extract the top 10 neighborhoods
top_10_neighborhoods <- sorted_neighborhoods %>%
  top_n(10)

# Join the top 10 neighborhoods with their average review scores
neighborhood_summary <- inner_join(top_10_neighborhoods, avg_review_scores, by = "neighbourhood_cleansed")

# Plot the line graph for listing count and average review scores
ggplot(neighborhood_summary, aes(x = reorder(neighbourhood_cleansed, listing_count))) +
  geom_line(aes(y = listing_count, group = 1, color = "Listing Count")) +
  geom_point(aes(y = listing_count, color = "Listing Count")) +
  geom_line(aes(y = avg_review_score * 100, group = 1, color = "Avg. Review Score")) +
  geom_point(aes(y = avg_review_score * 100, color = "Avg. Review Score")) +
  scale_y_continuous(sec.axis = sec_axis(~./100, name = "Average Review Score")) +
  labs(title = "Listing Count and Average Review Scores by Neighborhood",
       x = "Neighborhood",
       y = "Listing Count",
       color = "Metric") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))













# Filter the dataset by price range
filtered_data <- listings %>%
  filter(price <= 150)

# Group the filtered data by neighborhood and count the number of listings
neighborhood_counts <- filtered_data %>%
  group_by(neighbourhood_cleansed) %>%
  summarise(listing_count = n())

# Sort the neighborhoods by listing count in descending order
sorted_neighborhoods <- neighborhood_counts %>%
  arrange(desc(listing_count))

# Select the top 10 neighborhoods
top_10_neighborhoods <- sorted_neighborhoods %>%
  slice(1:10)

# Plot the top 10 neighborhoods based on listing count
ggplot(top_10_neighborhoods, aes(x = reorder(neighbourhood_cleansed, listing_count), y = listing_count)) +
  geom_bar(stat = "identity", fill = viridis(10)) +  # Using viridis color palette
  labs(title = "Top 10 Neighborhoods with Price $150 or Less",
       x = "Neighborhood",
       y = "Number of Listings") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))




# Group the data by neighborhood and calculate the average price
avg_price_per_neighborhood <- listings %>%
  group_by(neighbourhood_cleansed) %>%
  summarise(avg_price = mean(price, na.rm = TRUE))

# Print the average price per neighborhood
print(avg_price_per_neighborhood)

library(ggplot2)

# Plot the graph for average price per neighborhood
ggplot(avg_price_per_neighborhood, aes(x = reorder(neighbourhood_cleansed, avg_price), y = avg_price, fill = avg_price)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_c() +  # Use viridis color scale
  labs(title = "Average Price per Neighborhood",
       x = "Neighborhood",
       y = "Average Price") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip()



library(dplyr)
library(ggplot2)
library(viridis)
library(ggrepel)
library(geosphere)

# City center coordinates
city_center <- c(52.3676, 4.9041)

# Calculate distance between city center and each listing's coordinates
listings$distance_to_center <- distHaversine(listings[c("latitude", "longitude")], city_center)

# Group the listings by neighborhood and calculate the average distance to city center
neighborhood_avg_distance <- listings %>%
  group_by(neighbourhood_cleansed) %>%
  summarize(avg_distance = mean(distance_to_center))

# Sort the neighborhoods by average distance in ascending order
sorted_neighborhoods <- neighborhood_avg_distance %>%
  arrange(avg_distance)

# Get the top 10 neighborhoods by average distance
top_10_neighborhoods <- sorted_neighborhoods %>%
  head(10)

# Plot the average distances for the top 10 neighborhoods
ggplot(top_10_neighborhoods, aes(x = reorder(neighbourhood_cleansed, avg_distance), y = avg_distance, label = round(avg_distance, 2))) +
  geom_bar(stat = "identity", fill = viridis(10, option = "D"), color = "black") +
  geom_text_repel(aes(y = avg_distance + 1000), size = 3, color = "black") +
  labs(x = "Neighborhood", y = "Average Distance to City Center (meters)", title = "Average Distance to City Center for Top 10 Neighborhoods") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



library(dplyr)
library(ggplot2)
library(viridis)

# City center coordinates
city_center <- c(52.3676, 4.9041)

# Calculate distance between city center and each listing's coordinates
listings$distance_to_center <- distHaversine(listings[c("latitude", "longitude")], city_center)

# Filter listings within a distance range of 5 km from the city center
listings_within_range <- listings %>%
  filter(distance_to_center <= 1000)

# Group the filtered listings by neighborhood and count the number of listings
neighborhood_counts <- listings_within_range %>%
  group_by(neighbourhood_cleansed) %>%
  summarise(listing_count = n())

# Find the closest neighborhood to the city center
closest_neighborhood <- neighborhood_counts %>%
  arrange(listing_count) %>%
  slice(1)

# Plot the number of listings in the closest neighborhood within 5 km
ggplot(closest_neighborhood, aes(x = reorder(neighbourhood_cleansed, listing_count), y = listing_count)) +
  geom_bar(stat = "identity", fill = viridis(1, option = "D"), color = "black") +
  geom_text(aes(label = listing_count), vjust = -0.5, size = 4, color = "black") +
  labs(x = "Neighborhood", y = "Number of Listings", title = "Number of Listings in Closest Neighborhood within 5 km") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

