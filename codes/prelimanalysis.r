# Composite Face Effect Analysis Script
# Load required libraries
library(tidyverse)

# Set working directory to where your CSV files are
setwd("/Users/annapusok/Desktop/data/")

# Read and combine all CSV files
file_list <- list.files(pattern = "*.csv")
all_data <- map_df(file_list, read_csv)

# Display basic info
cat("Total number of files:", length(file_list), "\n")
cat("Total number of rows:", nrow(all_data), "\n")
cat("Number of unique subjects:", n_distinct(all_data$subject_id), "\n")

# Filter main task trials only (exclude practice and other non-trial rows)
main_data <- all_data %>%
  filter(task == "main", !is.na(correct), !is.na(response_time_ms))

# Create alignment and correct_response variables
main_data <- main_data %>%
  mutate(
    alignment = case_when(
      condition %in% c("diff_align", "same_aligned") ~ "aligned",
      condition %in% c("diff_misalign", "same_misaligned") ~ "misaligned"
    ),
    correct_response = case_when(
      condition %in% c("diff_align", "diff_misalign") ~ "different",
      condition %in% c("same_aligned", "same_misaligned") ~ "same"
    )
  )

cat("\nMain trials after filtering:", nrow(main_data), "\n")

# ===== ACCURACY ANALYSIS =====
cat("\n========== ACCURACY ANALYSIS ==========\n")

# Calculate accuracy by subject and condition
accuracy_by_subject <- main_data %>%
  group_by(subject_id, alignment, correct_response) %>%
  summarise(
    accuracy = mean(correct, na.rm = TRUE),
    n_trials = n(),
    .groups = "drop"
  )

# Overall means and SEs
accuracy_means <- accuracy_by_subject %>%
  group_by(alignment, correct_response) %>%
  summarise(
    mean_accuracy = mean(accuracy),
    se_accuracy = sd(accuracy) / sqrt(n()),
    n_subjects = n(),
    .groups = "drop"
  )

print(accuracy_means)

# Statistical tests for accuracy
cat("\n--- Accuracy: Different Trials ---\n")
diff_accuracy <- accuracy_by_subject %>%
  filter(correct_response == "different") %>%
  pivot_wider(names_from = alignment, values_from = accuracy, id_cols = subject_id)

if(all(c("aligned", "misaligned") %in% names(diff_accuracy))) {
  diff_acc_test <- t.test(diff_accuracy$aligned, diff_accuracy$misaligned, paired = TRUE)
  print(diff_acc_test)
  cat("Effect size (Cohen's d):", 
      (mean(diff_accuracy$aligned) - mean(diff_accuracy$misaligned)) / 
        sd(diff_accuracy$aligned - diff_accuracy$misaligned), "\n")
}

cat("\n--- Accuracy: Same Trials ---\n")
same_accuracy <- accuracy_by_subject %>%
  filter(correct_response == "same") %>%
  pivot_wider(names_from = alignment, values_from = accuracy, id_cols = subject_id)

if(all(c("aligned", "misaligned") %in% names(same_accuracy))) {
  same_acc_test <- t.test(same_accuracy$aligned, same_accuracy$misaligned, paired = TRUE)
  print(same_acc_test)
  cat("Effect size (Cohen's d):", 
      (mean(same_accuracy$aligned) - mean(same_accuracy$misaligned)) / 
        sd(same_accuracy$aligned - same_accuracy$misaligned), "\n")
}



# Accuracy plot
accuracy_plot <- ggplot(accuracy_means, 
                        aes(x = alignment, y = mean_accuracy, fill = correct_response)) +
  geom_bar(stat = "identity", position = position_dodge(0.9), width = 0.7) +
  geom_errorbar(aes(ymin = mean_accuracy - se_accuracy, 
                    ymax = mean_accuracy + se_accuracy),
                width = 0.2, position = position_dodge(0.9)) +
  scale_fill_manual(values = c("different" = "#38b6ff", "same" = "#ff914d"),
                    labels = c("Different", "Same")) +
  labs(title = "Accuracy by Alignment and Response Type",
       y = "Accuracy (Proportion Correct)", 
       x = "Alignment Condition",
       fill = "Trial Type") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top") +
  ylim(0, 1)

print(accuracy_plot)
ggsave("accuracy_plot.png", width = 8, height = 6)


print(rt_plot)
ggsave("rt_plot.png", width = 8, height = 6)

# ===== EXPORT SUMMARY DATA =====
write_csv(accuracy_means, "accuracy_summary.csv")
write_csv(rt_means, "rt_summary.csv")
write_csv(accuracy_by_subject, "accuracy_by_subject.csv")
write_csv(rt_by_subject, "rt_by_subject.csv")

