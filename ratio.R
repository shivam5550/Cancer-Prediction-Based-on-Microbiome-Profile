library(dplyr)
non_microbiome_cols <- c("index", "grp", "Age", "Gender")


microbiome_data <- data[, microbiome_cols]
# Calculate relative abundance
relative_abundance_data <- microbiome_data

for (i in 1:nrow(microbiome_data)) {
  row_sum <- sum(microbiome_data[i, ])
  if (row_sum > 0) {
    relative_abundance_data[i, ] <- microbiome_data[i, ] / row_sum
  } else {
    relative_abundance_data[i, ] <- 0
  }
}



# Manually calculate relative abundance for the first row
first_row_counts <- microbiome_data[1, ]
first_row_sum <- sum(first_row_counts)
first_row_relative_abundance <- if (first_row_sum > 0) {
  first_row_counts / first_row_sum
} else {
  rep(0, length(first_row_counts))
}

# Display the manually calculated relative abundance
print("Manually calculated relative abundance for the first row:")
print(first_row_relative_abundance)

# Display the relative abundance from the transformed data
print("Relative abundance from the transformed data for the first row:")
print(relative_abundance_data[1, ])

# Check if they are equal
all.equal(first_row_relative_abundance, relative_abundance_data[1, ])


ratio_data <- cbind(data[, non_microbiome_cols], relative_abundance_data)


ratio_data_path <- "ratio.csv"
write.csv(ratio_data, ratio_data_path, row.names = FALSE)


library(ALDEx2)

test_data <- ratio_data
total_zeros_after_threshold <- sum(test_data == 0, na.rm = TRUE)
# Create a new label 'adenoma' by merging 'low', 'intermediate', and 'high'
test_data$grp <- with(test_data, ifelse(grp %in% c("low", "intermediate", "high"), "adenoma", grp))


# Extract metadata and microbiome data
metadata <- test_data[, c("index", "Age", "Gender", "grp")]
microbiome_data <- test_data[, !(names(test_data) %in% c("index", "Age", "Gender", "grp"))]


# Define the two groups you are comparing
group1 <- "cancer"
group2 <- "negative"

# Filter metadata and count data for the two groups
filtered_metadata <- metadata[metadata$grp %in% c(group1, group2), ]
filtered_counts <- microbiome_data[rownames(filtered_metadata), ]

filtered_counts <- as.data.frame(sapply(filtered_counts, as.numeric))

# Perform ALDEx2 analysis
conds <- as.factor(filtered_metadata$grp)

aldex_data <- aldex.clr(filtered_counts, conds, mc.samples = 128, denom = "all")


plot_multiple_histograms <- function(data, columns, ncol = 2, x_limits = NULL) {
  plots <- lapply(columns, function(column) {
    p <- ggplot(data, aes(x = !!sym(column))) +
      geom_histogram(bins = 30, fill = "blue", color = "black", alpha = 0.7) +
      theme_minimal() +
      xlab(column) +
      ylab("Frequency") +
      
    
    if (!is.null(x_limits)) {
      p <- p + coord_cartesian(xlim = x_limits)
    }
    
    p
  })
  
  grid.arrange(grobs = plots, ncol = ncol)
}

plot_multiple_histograms(test_data, random_columns, ncol = 2)
