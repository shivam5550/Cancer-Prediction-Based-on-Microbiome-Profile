data_csv <- read.csv("bacteria_disease.csv")

head(data_csv)

non_microbiome_cols <- c("Gender", "Age", "grp")
non_numeric_data <- data_csv[, non_microbiome_cols]

microbiome_data <- data_csv[, !colnames(data_csv) %in% non_microbiome_cols]

threshold <- 0.1 * nrow(microbiome_data)
threshold_microbiome_data <- microbiome_data[, colSums(microbiome_data > 0) >= threshold]

threshold_data <- cbind(threshold_microbiome_data, data_csv[, non_microbiome_cols])


total_zeros_after_threshold <- sum(threshold_data == 0, na.rm = TRUE)

#new dataset
data_path <- "threshold_original.csv"
write.csv(threshold_data, data_path, row.names = FALSE)
