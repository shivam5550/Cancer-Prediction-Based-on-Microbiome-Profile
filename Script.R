file <- "bacteria_disease.txt"
data <- read.table(file, header = TRUE, sep = "\t")
View(data)
write.csv(data, "bacteria_disease.csv", row.names = FALSE)

data_csv <- read.csv("bacteria_disease.csv")

head(data_csv)


total_zeros <- sum(data_csv == 0, na.rm = TRUE)

non_microbiome_cols <- c("Gender", "Age", "grp")
non_numeric_data <- data_csv[, non_microbiome_cols]


total_zeros_microbiome <- sum(microbiome_data == 0, na.rm = TRUE)

microbiome_data <- data_csv[, !colnames(data_csv) %in% non_microbiome_cols]

index_col <- data_csv[, 1]


# Function to extract D4 level from the column name and validate it
extract_d4 <- function(colname) {

  parts <- str_extract_all(colname, "D_[0-5]__[A-Za-z0-9.]+(?=\\.D|$)")[[1]]
  print(colname)  #debug
  print(parts)    # debug

  if (length(parts) >= 5) {
    d4_level <- paste(parts[1:5], collapse = ".")
    print(d4_level)  
    return(d4_level)
  } else {
    print(paste("Invalid:", colname))  
    return(NA)
  }
}

library(stringr)
#Extracting D4 levels
d4_levels <- sapply(colnames(microbiome_data), extract_d4)

# Filter out columns without valid D4 levels
valid_cols <- !is.na(d4_levels)
microbiome_data <- microbiome_data[, valid_cols]
d4_levels <- d4_levels[valid_cols]



#unique D4 levels
unique_d4_levels <- unique(d4_levels)
unique_d4_levels <- unique_d4_levels[unique_d4_levels != "index.NA.NA.NA.NA"]


#Manually combining for a row:
example_d4 <- unique_d4_levels[10]
matching_cols <- grep(example_d4, colnames(data_csv), value = TRUE)
first_row_values <- data_csv[39, matching_cols]
sum_first_row <- sum(first_row_values, na.rm = TRUE)




combined_data <- data.frame(index = index_col)

#Combining D5 countrs into D4:
for (d4 in unique_d4_levels) {
  print(d4)
  matching_cols <- grep(d4, colnames(data_csv), value = TRUE)
  print(matching_cols)
  # Initialize a vector to store the combined values for the current D4 level
  combined_values <- numeric(nrow(data_csv))
  
  # Looping through each row and calculate the sum for the current D4 level
  for (i in 1:nrow(data_csv)) {
    row_values <- data_csv[i, matching_cols]
    combined_values[i] <- sum(row_values, na.rm = TRUE)
  }
  
  # Add the combined values to the new data frame
  combined_data[[d4]] <- combined_values
}

head(combined_data)
head(data_csv)

final_combined_data <- cbind(combined_data, data_csv %>% select(one_of(non_microbiome_cols)))

total_zeros_d4 <- sum(final_combined_data == 0, na.rm = TRUE)

#new dataset
combined_data_path <- "bacteria_disease_combined_d4.csv"
write.csv(final_combined_data, combined_data_path, row.names = FALSE)

library(dplyr)

non_microbiome <- c("grp", "Age", "Gender", "index")
non_microbiome2 <- c("index")
# Extracting only microbiome data from original dataset
original_microbiome_data <- data_csv %>% select(-one_of(non_microbiome))

# Extracting the combined microbiome data 
combined_microbiome_data <- combined_data %>% select(-one_of(non_microbiome2), -index)

# Function to verify if values in the first row add up correctly
verify_all_rows <- function(original_microbiome_data, combined_microbiome_data) {
  mismatches <- list()
  
  for (i in 1:nrow(original_microbiome_data)) {
    row_mismatches <- list()
    
    for (combined_col in colnames(combined_microbiome_data)) {
      # Extract the D4 level from the combined column
      d4_level <- combined_col
      
      # Find the original columns that match this D4 level
      matching_cols <- grep(d4_level, colnames(original_microbiome_data), value = TRUE)
      
      # Sum the matching columns in the original dataset for the current row
      if (length(matching_cols) > 0) {
        original_sum <- sum(as.numeric(original_microbiome_data[i, matching_cols, drop = FALSE]), na.rm = TRUE)
        combined_sum <- as.numeric(combined_microbiome_data[i, combined_col])
        
        # Compare the sums
        if (original_sum != combined_sum) {
          row_mismatches[[d4_level]] <- list(original_sum = original_sum, combined_sum = combined_sum)
        }
      }
    }
    
    if (length(row_mismatches) > 0) {
      mismatches[[i]] <- row_mismatches
    }
  }
  
  return(mismatches)
}

# Verify all rows
mismatches <- verify_all_rows(original_microbiome_data, combined_microbiome_data)

# Print the result
if (length(mismatches) == 0) {
  print("All values in all rows match correctly.")
} else {
  print("There are mismatches in some rows:")
  print(mismatches)
}



#Clustering:

  #D_0__Bacteria.D_1__Fusobacteria.D_2__Fusobacteriia.D_3__Fusobacteriales.D_4__Fusobacteriaceae,
  #D_0__Bacteria.D_1__Bacteroidetes.D_2__Bacteroidia.D_3__Bacteroidales.D_4__Bacteroidaceae,
  #D_0__Bacteria.D_1__Bacteroidetes.D_2__Bacteroidia.D_3__Bacteroidales.D_4__Porphyromonadaceae,
  #D_0__Bacteria.D_1__Firmicutes.D_2__Clostridia.D_3__Clostridiales.D_4__Lachnospiraceae,
  #D_0__Bacteria.D_1__Proteobacteria.D_2__Gammaproteobacteria.D_3__Enterobacteriales.D_4__Enterobacteriaceae,
  #D_0__Archaea.D_1__Euryarchaeota.D_2__Methanobacteria.D_3__Methanobacteriales.D_4__Methanobacteriaceae


temp_data <- final_combined_data %>%
  mutate(Blood_Found = ifelse(grepl("P$", index), "P", "N"))

head(temp_data[, c("index", "Blood_Found")])

# Create a box plot to visualize the distribution of ages within each Blood_Found group
ggplot(temp_data, aes(x = Blood_Found, y = Age, fill = Blood_Found)) +
  geom_boxplot() +
  labs(title = "Boxplot of Age Distribution by Blood Found in Stool (P/N)",
       x = "Blood Found (P/N)",
       y = "Age",
       fill = "Blood Found") +
  theme_minimal() +
  theme(legend.position = "none")


# Boxplot of Age by Blood Found and Gender
ggplot(temp_data, aes(x = Blood_Found, y = Age, fill = Gender)) +
  geom_boxplot() +
  labs(title = "Boxplot of Age Distribution by Blood Found in Feaces (P/N) and Gender",
       x = "Blood Found (P/N)",
       y = "Age",
       fill = "Gender") +
  theme_minimal()+
  theme(axis.text.x = element_text(size = 14),  # Increase x-axis text size
      axis.text.y = element_text(size = 14))


# Bar Plot of Group by Blood Found
ggplot(temp_data, aes(x = grp, fill = Blood_Found)) +
  geom_bar(position = "dodge") +
  labs(title = "Bar Plot of Group by Blood Found in Stool (P/N)",
       x = "Group",
       y = "Count",
       fill = "Blood Found") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Check for entries:
entries <- temp_data %>%
  filter(Blood_Found == "N" & grp == "low")


# Violin Plot of Age by Group and Blood Found
ggplot(temp_data, aes(x = grp, y = Age, fill = Blood_Found)) +
  geom_violin() +
  labs(title = "Violin Plot of Age by Group and Blood Found in Stool (P/N)",
       x = "Group",
       y = "Age",
       fill = "Blood Found") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Facet Grid of Boxplots by Group
ggplot(temp_data, aes(x = Blood_Found, y = Age, fill = Blood_Found)) +
  geom_boxplot() +
  facet_wrap(~ grp) +
  labs(title = "Facet Grid of Boxplots of Age by Group and Blood Found in Stool (P/N)",
       x = "Blood Found (P/N)",
       y = "Age",
       fill = "Blood Found") +
  theme_minimal() +
  theme(legend.position = "none")


#HANDLING ZEROES using CLR transformation:

data <- read.csv("bacteria_disease_combined_d4.csv")
options(scipen = 999)

install.packages("dplyr")
install.packages("tidyr")
install.packages("compositions")
library(dplyr)
library(tidyr)
library(compositions)

microbiome_cols <- grep("^D_", colnames(data), value = TRUE)
non_microbiome_cols <- c("index","Gender", "Age", "grp")


microbiome_data <- data[, microbiome_cols]

#Filtering features present in less than 10% of samples.
threshold <- 0.1 * nrow(microbiome_data)
filtered_microbiome_data <- microbiome_data[, colSums(microbiome_data > 0) >= threshold]

#Imputing zeroes with small constant value:

#On setting the alpha dynamically it is setting to zero only which means the minimum non zero value is very small,
#hence using fixed value instead.
#alpha <- 0.5 * min(filtered_microbiome_data[filtered_data > 0], na.rm = TRUE)


filtered_data <- cbind(data[, non_microbiome_cols], filtered_microbiome_data)

filtered_data <- format(filtered_data, scientific = FALSE)

#new dataset
filtered_data_path <- "filtered.csv"
write.csv(filtered_data, filtered_data_path, row.names = FALSE)






#HANDLING ZEROES (ATTEMPT TO IMPLEMENT ZERO INFLATED MODEL):
library(ggplot2)
library(dplyr)
library(gridExtra)



#checking distribution before transformation:
# Randomly select 10 columns from microbiome columns
set.seed(123)  # For reproducibility
random_columns <- sample(microbiome_cols, 4)
print(random_columns)


# Function to create density plots for multiple columns
plot_multiple_densities <- function(data, columns, ncol = 2, x_limits = NULL) {
  plots <- lapply(columns, function(column) {
    p <- ggplot(data, aes(x = !!sym(column))) +
      geom_density(fill = "green", alpha = 0.7) +
      theme_minimal()
    if (!is.null(x_limits)) {
      p <- p + xlim(x_limits)
    }
    p
  })
  grid.arrange(grobs = plots, ncol = ncol)
}

# Plot density plots for the selected random columns
plot_multiple_densities(filtered_data, random_columns, ncol = 2)

plot_multiple_histograms(filtered_data, random_columns, ncol = 2)

#libraries for ZINB:
library(pscl)
library(purrr)


microbiome_data <- filtered_data[, microbiome_cols]


zinb_data <- microbiome_data

# Function to check if a column has any zeros
has_zeros <- function(column) {
  any(column == 0)
}


# Filter microbiome columns to include only those with at least one zero
microbiome_cols_with_zeros <- microbiome_cols[sapply(zinb_data[, microbiome_cols], has_zeros)]




# Function to fit a Zero-Inflated Negative Binomial model for each taxon with other taxa as predictors
fit_zinb_model <- function(taxon, data, microbiome_cols) {
  predictors <- paste(setdiff(microbiome_cols, taxon), collapse = " + ")
  formula <- as.formula(paste(taxon, "~", predictors))
  zeroinfl(formula, data = data, dist = "negbin")
}

# Fit ZINB models for each taxon
zinb_models <- map(microbiome_cols_with_zeros, ~ fit_zinb_model(.x, zinb_data, microbiome_cols_with_zeros))

names(zinb_models) <- microbiome_cols_with_zeros


predict_zinb <- function(model, data) {
  predict(model, newdata = data, type = "response")
}


# Function to replace zeros with predicted values
replace_zeros <- function(original_data, predicted_data) {
  replaced_data <- original_data
  for (i in 1:ncol(original_data)) {
    zeros <- which(original_data[, i] == 0)
    replaced_data[zeros, i] <- predicted_data[zeros, i]
  }
  replaced_data
}



# Predict values and replace zeros in the microbiome data
predicted_data <- map_dfc(zinb_models, predict_zinb, data = filtered_data)

colnames(predicted_data) <- microbiome_cols

existing_non_microbiome_cols <- intersect(non_microbiome_cols, colnames(zinb_data))


# Replace zeros in the microbiome data with predicted values
microbiome_data_replaced <- zinb_data[, microbiome_cols]



# Function to safely replace zeros with predicted values
replace_zeros_safe <- function(original_col, predicted_col) {
  zeros <- which(original_col == 0)
  if (length(predicted_col) >= length(zeros)) {
    original_col[zeros] <- predicted_col[zeros]
  }
  return(original_col)
}



# Iterate over the microbiome columns that were used in the model fitting
for (col in microbiome_cols_with_zeros) {
  original_col <- zinb_data[[col]]
  predicted_col <- predicted_data[[col]]
  
  # Replace zeros in the original column with predicted values
  zeros <- which(original_col == 0)
  microbiome_data_replaced[[col]] <- replace_zeros_safe(original_col, predicted_col)
}

non_microbiome_cols <- c("index", "Gender", "Age", "grp")
zinb_data_replaced <- cbind(filtered_data[, non_microbiome_cols], microbiome_data_replaced)


# Function to plot density for a taxon before and after replacement
plot_density_comparison <- function(taxon, original_data, replaced_data) {
  p1 <- ggplot(original_data, aes_string(x = taxon)) +
    geom_density(fill = "lightblue") +
    ggtitle(paste("Original Data")) +
    theme_minimal()
  
  p2 <- ggplot(replaced_data, aes_string(x = taxon)) +
    geom_density(fill = "lightgreen") +
    ggtitle(paste("Replaced Data")) +
    theme_minimal()
  
  return(list(p1, p2))
}

# Select a few taxa for comparison
selected_taxa <- microbiome_cols[9:11]  # Adjust the number as needed

# Plot density graphs for selected taxa
density_plots <- lapply(selected_taxa, function(taxon) {
  plot_density_comparison(taxon, filtered_data, zinb_data_replaced)
})

# Flatten the list of plots
density_plots <- do.call(c, density_plots)

# Arrange the plots in a grid
grid.arrange(grobs = density_plots, ncol = 2)

zinb_data_path <- "after_zinb.csv"
write.csv(zinb_data_replaced, zinb_data_path, row.names = FALSE)


#TRAINING MODELS BEFORE IMPUTING ZEROES:
library(caret)
library(glmnet)
library(randomForest)
library(xgboost)

non_microbiome_cols <- c("index", "Gender", "Age", "grp")
microbiome_cols <- setdiff(colnames(filtered_data), non_microbiome_cols)
filtered_data[microbiome_cols] <- lapply(filtered_data[microbiome_cols], as.numeric)

temp_data <- filtered_data

temp_data <- temp_data %>%
  mutate(target = ifelse(grp %in% c("normal_colonoscopy", "negative", "other"), 0, 1))

temp_data <- subset(temp_data, select = -c(grp))
temp_data$target <- as.factor(temp_data$target)
temp_data$Gender <- as.factor(temp_data$Gender)
temp_data$Age <- as.numeric(temp_data$Age)













