####################################
################################
install.packages("ggridges")
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggridges)

# Load the data
file_path <- "/Users/mtofflemire/Mtofflemire Dropbox/Michael Tofflemire/Projects/syndiesel/scripts/results/config_1-results/config_1_0_10000_0.0000001_results/summary_output.txt"
df <- read.table(file_path, header = TRUE, sep = "\t")

# Event types for probabilities
event_types <- c("Prob_1_Event", "Prob_2_Events", "Prob_3_Events", "Prob_4_Events", "Prob_5_Events", "Prob_6_Events")

# Reshape the data into a long format
probabilities_long_df <- df %>%
  gather(key = "EventType", value = "Probability", all_of(event_types))

# Find the event type with the highest maximum probability
highest_prob_event <- probabilities_long_df %>%
  group_by(EventType) %>%
  summarize(max_prob = max(Probability)) %>%
  filter(max_prob == max(max_prob)) %>%
  pull(EventType)

# Create a new column to define colors for each event type
probabilities_long_df <- probabilities_long_df %>%
  mutate(Color = case_when(
    EventType == "Prob_1_Events" ~ "orange",
    EventType == highest_prob_event ~ "orange",
    TRUE ~ "gray"
  ))

# Create a ridgeline plot with the true event ("Prob_3_Events") in red and the highest in blue
plot_prob <- ggplot(probabilities_long_df, aes(x = Probability, y = EventType, fill = Color)) +
  geom_density_ridges(alpha = 0.7, scale = 10, rel_min_height = 0.01, color = "black", size = 0.25) +
  scale_fill_identity() +
  labs(
    title = "Ridgeline Plot of Probabilities for Each Event Type Across Runs",
    x = "Probability",
    y = "Event Type"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),      # remove ALL gridlines
    panel.border = element_rect(       # add full box around plotting area
      colour = "black", fill = NA, size = 1
    ),
    legend.position = "none"
  ) +
  coord_cartesian(xlim = c(-0.08, 1))

plot_prob

plot_prob
# Save the plot as a PNG file
ggsave("/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_3_160000_10000_0.00001_results/probability_plot.png", plot = plot_prob, width = 8, height = 8, dpi = 300)










# make sure EventType has a consistent order
probabilities_long_df$EventType <- factor(probabilities_long_df$EventType,
                                          levels = event_types)

plot_prob <- ggplot(probabilities_long_df,
                    aes(x = Probability, y = EventType, fill = Color)) +
  geom_density_ridges(alpha = 0.7,
                      scale = 10,
                      rel_min_height = 0.01,
                      color = "black",
                      size = 0.25) +
  # y-axis labels as 1–6 instead of "Prob_1_Event" etc.
  scale_y_discrete(
    breaks = event_types,
    labels = 1:length(event_types)
  ) +
  scale_fill_identity() +
  labs(
    title = "Ridgeline Plot of Probabilities for Each Event Type Across Runs",
    x = "Probability",
    y = "Event Type"
  ) +
  theme_minimal() +
  theme(
    # light gridlines
    panel.grid.major = element_line(color = "grey85", size = 0.3),
    panel.grid.minor = element_line(color = "grey92", size = 0.2),
    # box around plotting area
    panel.border = element_rect(color = "black", fill = NA, size = 0.8),
    legend.position = "none"
  ) +
  coord_cartesian(xlim = c(-0.08, 1))

plot_prob




















#########################
########################
library(tidyr)
library(dplyr)
library(ggplot2)
library(scales)  # For formatting axis labels

# Load the summary file
file_path <- "/Users/mtofflemire/Mtofflemire Dropbox/Michael Tofflemire/Projects/syndiesel/scripts/results/config_1-results/config_no-mig_1_0_10000_results/summary_output.txt"

df_new <- read.table(file_path, header = TRUE, sep = "\t")

# Gather the Mean divergence values into long format
df_new_long_mean <- df_new %>%
  gather(key = "PopPair", value = "MeanDivergence", 
         Mean_Divergence_PopA_PopB:Mean_Divergence_PopK_PopL) %>%
  select(Run, PopPair, MeanDivergence)

# Gather the True divergence values into long format
df_new_long_true <- df_new %>%
  gather(key = "PopPair_True", value = "TrueValue", 
         True_Divergence_PopA_PopB:True_Divergence_PopK_PopL) %>%
  mutate(PopPair = gsub("True_Divergence_", "Mean_Divergence_", PopPair_True)) %>%
  select(Run, PopPair, TrueValue)

# Merge both datasets based on matching population pairs and runs
df_combined_new <- inner_join(df_new_long_mean, df_new_long_true, by = c("Run", "PopPair"))

# Remove duplicates if necessary
df_combined_new_unique <- df_combined_new[!duplicated(df_combined_new[, c("TrueValue", "MeanDivergence")]), ]

# Divide the mean estimated values by 1e-8
df_combined_new_unique$MeanDivergence <- df_combined_new_unique$MeanDivergence / 1e-8

# Filter out rows where MeanDivergence is NA or negative
df_combined_new_unique <- df_combined_new_unique %>%
  filter(!is.na(MeanDivergence) & MeanDivergence >= 0)

# Convert TrueValue to a numeric variable
df_combined_new_unique$TrueValue <- as.numeric(as.character(df_combined_new_unique$TrueValue))

# Set the axis limit
axis_limit <- 350000

# Get unique true divergence values for vertical lines
unique_true_values <- unique(df_combined_new_unique$TrueValue)

# Assign the plot to a variable
my_plot <- ggplot(df_combined_new_unique, aes(x = TrueValue, y = MeanDivergence, group = PopPair, fill = PopPair)) +
  geom_violin(alpha = 0.7, trim = FALSE, scale = "area", adjust = 3, position = position_identity()) +
  geom_jitter(width = 0.4, alpha = 0.4, size = 3, color = "black") +
  geom_vline(xintercept = unique_true_values, linetype = "dotted", color = "blue", size = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  labs(x = "True Divergence", y = "Estimated Mean Divergence (x 10^-8)", 
       title = "True vs Estimated Divergence (Config 3: 10000_10000)") +
  theme_bw() +
  coord_fixed(ratio = 1) +
  scale_x_continuous(labels = scales::comma, limits = c(0, axis_limit)) +
  scale_y_continuous(labels = scales::comma, limits = c(0, axis_limit)) +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 16, face ="bold"),
    axis.text = element_text(size = 16, face="bold"),
    plot.title = element_text(size = 0, face = "bold")
  )
my_plot

# Save the plot as a PNG file
ggsave("/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_3_40000_10000_0.00001_results/my_plot.png", plot = my_plot, width = 8, height = 8, dpi = 300)




#For same divergence events across all population pairs. 
############################
#############################
# Load the necessary libraries
# Load the necessary libraries
library(tidyr)
library(dplyr)
library(ggplot2)
library(scales)  # For formatting axis labels

# Load the summary file
file_path <- "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001-syndiesel/scripts/results/config_1-results/config_1_0_10000_0.0000001_results/summary_output.txt"
df_new <- read.table(file_path, header = TRUE, sep = "\t")

# Gather the Mean divergence values into long format
df_new_long_mean <- df_new %>%
  gather(key = "PopPair", value = "MeanDivergence", 
         Mean_Divergence_PopA_PopB:Mean_Divergence_PopK_PopL) %>%
  select(Run, PopPair, MeanDivergence)

# Gather the True divergence values into long format
df_new_long_true <- df_new %>%
  gather(key = "PopPair_True", value = "TrueValue", 
         True_Divergence_PopA_PopB:True_Divergence_PopK_PopL) %>%
  mutate(PopPair = gsub("True_Divergence_", "Mean_Divergence_", PopPair_True)) %>%
  select(Run, PopPair, TrueValue)

# Merge both datasets based on matching population pairs and runs
df_combined_new <- inner_join(df_new_long_mean, df_new_long_true, by = c("Run", "PopPair"))

# Remove duplicates if necessary
df_combined_new_unique <- df_combined_new[!duplicated(df_combined_new[, c("TrueValue", "MeanDivergence")]), ]

# Divide the mean estimated values by 1e-8
df_combined_new_unique$MeanDivergence <- df_combined_new_unique$MeanDivergence / 1e-8

# Filter out rows where MeanDivergence is NA or negative
df_combined_new_unique <- df_combined_new_unique %>%
  filter(!is.na(MeanDivergence) & MeanDivergence >= 0)

# Convert TrueValue to a numeric variable
df_combined_new_unique$TrueValue <- as.numeric(as.character(df_combined_new_unique$TrueValue))

# Get unique true divergence values for vertical lines
unique_true_values <- unique(df_combined_new_unique$TrueValue)



# Create the plot with properly overlapping violin plots, formatted axis labels, and marked true divergence times
ggplot(df_combined_new_unique, aes(x = as.factor(TrueValue), y = MeanDivergence, fill = PopPair)) +
  geom_violin(alpha = 0.7, trim = FALSE, scale = "width", adjust = 0.5, position = position_dodge(width = 0)) +
  geom_jitter(aes(color = PopPair), alpha = 0.4, size = 1, position = position_dodge(width = 0.5)) +  # Color the points by PopPair
  geom_vline(xintercept = 10000, linetype = "dotted", color = "blue", size = 0.7) +
  geom_hline(yintercept = 10000, linetype = "dotted", color = "blue", size = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  labs(x = "True Divergence", y = "Estimated Mean Divergence", fill = "Population Pair", color = "Population Pair") +  # Add labels for the legend
  theme_bw() +
  scale_x_discrete(labels = "10,000") +  # Set the x-axis label to show the true divergence value
  scale_y_continuous(labels = comma, limits = c(0, 200000)) +
  theme(
    legend.position = "right",  # Position the legend on the right
    axis.title = element_text(size = 15, face = "bold"),  # Increase axis title size
    axis.text = element_text(size = 15, face = "bold"),  # Increase axis tick labels size
    plot.title = element_text(size = 15, face = "bold")  # Increase title size
  )





####################################
################################
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggridges)

# Load the data
file_path <- "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001-syndiesel/scripts/results/config_1_0_10000_results/summary_output.txt"
df <- read.table(file_path, header = TRUE, sep = "\t")

# Event types for probabilities
event_types <- c("Prob_1_Event", "Prob_2_Events", "Prob_3_Events", "Prob_4_Events", "Prob_5_Events", "Prob_6_Events")

# Reshape the data into a long format
probabilities_long_df <- df %>%
  gather(key = "EventType", value = "Probability", all_of(event_types))

# Find the event type with the highest maximum probability
highest_prob_event <- probabilities_long_df %>%
  group_by(EventType) %>%
  summarize(max_prob = max(Probability)) %>%
  filter(max_prob == max(max_prob)) %>%
  pull(EventType)

# Create a new column to define colors for each event type
probabilities_long_df <- probabilities_long_df %>%
  mutate(Color = case_when(
    EventType == "Prob_1_Events" ~ "orange",
    EventType == highest_prob_event ~ "orange",
    TRUE ~ "gray"
  ))

# Create a ridgeline plot with the true event ("Prob_3_Events") in red and the highest in blue
plot_prob <-ggplot(probabilities_long_df, aes(x = Probability, y = EventType, fill = Color)) +
  geom_density_ridges(alpha = 0.7, scale = 10, rel_min_height = 0.01, color = "black", size = 0.25) +
  scale_fill_identity() +  # Use the colors defined in the dataframe directly
  labs(title = "Ridgeline Plot of Probabilities for Each Event Type Across Runs", 
       x = "Probability", 
       y = "Event Type") +
  theme_minimal() +
  theme(
    legend.position = "none"  # Legend is removed, but gridlines are kept
  ) +
  coord_cartesian(xlim = c(-0.08, 1))  # Set x-axis limits from 0 to 1
plot_prob
# Save the plot as a PNG file
ggsave("/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_3_80000_10000_0.00001_results/probability_plot.png", plot = plot_prob, width = 8, height = 8, dpi = 300)
































# Updated Ridgeline Plot
plot_prob <- ggplot(probabilities_long_df, aes(x = Probability, y = EventType, fill = Color)) +
  geom_density_ridges(alpha = 0.7, scale = 10, rel_min_height = 0.01, color = "black", size = 0.25) +
  scale_fill_identity() +  # Use the colors defined in the dataframe directly
  labs(title = "Ridgeline Plot of Probabilities for Each Event Type Across Runs", 
       x = "Probability", 
       y = "Event Type") +
  theme_gray() +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 12),  # Adjust axis title size
    axis.text = element_text(size = 10),   # Adjust axis tick size
    panel.border = element_rect(color = "black", fill = NA, size = 1)  # Add black border
  ) +
  coord_cartesian(xlim = c(-0.08, 1))  # Set x-axis limits from 0 to 1
plot_prob

# Save the updated plot
ggsave("/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_3_160000_10000_0.00001_results/probability_plot.png", 
       plot = plot_prob, width = 8, height = 8, dpi = 300)



# Updated Violin Plot
my_plot <- ggplot(df_combined_new_unique, aes(x = TrueValue, y = MeanDivergence, group = PopPair, fill = PopPair)) +
  geom_violin(alpha = 0.7, trim = FALSE, scale = "area", adjust = 3, position = position_identity()) +
  geom_jitter(width = 0.4, alpha = 0.4, size = 1, color = "black") +
  geom_vline(xintercept = unique_true_values, linetype = "dotted", color = "blue", size = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  labs(x = "True Divergence", y = "Estimated Mean Divergence (x 10^-8)", 
       title = "True vs Estimated Divergence (Config 3: 10000_10000)") +
  theme_gray() +
  coord_cartesian(xlim = c(0, axis_limit), ylim = c(0, axis_limit)) +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 12),  # Adjust axis title size
    axis.text = element_text(size = 10),   # Adjust axis tick size
    panel.border = element_rect(color = "black", fill = NA, size = 1)  # Add black border
  )
my_plot

# Save the updated plot
ggsave("/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_3_80000_10000_results/violin_plot.png", 
       plot = my_plot, width = 8, height = 8, dpi = 300)



combined_plots <- my_plot / plot_prob

combined_plots
























# Convert TrueValue and MeanDivergence to numeric explicitly
df_combined_new_unique$TrueValue <- as.numeric(as.character(df_combined_new_unique$TrueValue))
df_combined_new_unique$MeanDivergence <- as.numeric(as.character(df_combined_new_unique$MeanDivergence))

# Filter out rows where TrueValue or MeanDivergence are NA, infinite, or negative
df_combined_new_unique <- df_combined_new_unique %>%
  filter(!is.na(TrueValue) & !is.na(MeanDivergence) &
           is.finite(TrueValue) & is.finite(MeanDivergence) & MeanDivergence >= 0)

# Verify that the data is clean
summary(df_combined_new_unique)
head(df_combined_new_unique)

# Plotting
ggplot(df_combined_new_unique, aes(x = TrueValue, y = MeanDivergence, fill = PopPair)) +
  geom_violin(alpha = 0.7, trim = FALSE, adjust = 1, scale = "width", width = 1.5) +  # Adjust width and smoothing for better visibility
  geom_jitter(alpha = 0.4, size = 3, color = "black", width = 0.2, height = 0) +  # Jitter points horizontally only
  geom_vline(xintercept = unique(df_combined_new_unique$TrueValue), linetype = "dotted", color = "blue", size = 0.7) +  # Vertical lines for true values
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +  # Add 1:1 line
  labs(x = "True Divergence", y = "Estimated Mean Divergence (x 10^-8)", 
       title = "True vs Estimated Divergence (Config 3: 10000_10000)") +
  theme_bw() +
  scale_x_continuous(labels = comma) +  # Use continuous x-axis
  scale_y_continuous(labels = comma, limits = c(0, axis_limit)) +  # Set y-axis limits
  theme(
    legend.position = "none",  # Remove the legend
    axis.title = element_text(size = 16, face = "bold"),  # Increase axis title size
    axis.text = element_text(size = 16, face = "bold"),  # Increase axis tick labels size
    plot.title = element_text(size = 18, face = "bold")  # Set title size
  )













# Load necessary libraries
library(ggplot2)
library(tidyr)

# Load the data from the file
df <- read.table("/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_3_10000_10000_results/summary_output.txt", 
                 header = TRUE, sep = "\t")  # Adjust the separator if needed

# Prepare the data in a long format for ggplot
df_long <- df %>%
  gather(key = "PopPair", value = "MeanDivergence", 
         Mean_Divergence_PopA_PopB:Mean_Divergence_PopK_PopL) %>%
  gather(key = "TrueDivergence", value = "TrueValue", 
         True_Divergence_PopA_PopB:True_Divergence_PopK_PopL)

# Filter to match the population pairs for correct plotting
df_long <- df_long[df_long$PopPair == gsub("Mean_Divergence_", "True_Divergence_", df_long$PopPair), ]

# Create the plot
ggplot(df_long, aes(x = TrueValue, y = MeanDivergence)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue", se = FALSE) + # Add a trend line
  labs(x = "True Divergence", y = "Estimated Mean Divergence", 
       title = "True vs Estimated Divergence for Population Pairs") +
  facet_wrap(~ PopPair, scales = "free") +
  theme_minimal()




# Check the column names to make sure they are as expected
colnames(df)



# Check column names for matching patterns
divergence_cols <- grep("Mean_Divergence|True_Divergence", colnames(df), value = TRUE)
print(divergence_cols)

# Gather the Mean and True Divergence columns into a long format
df_long <- df %>%
  gather(key = "DivergenceType", value = "DivergenceValue", Mean_Divergence_PopA_PopB:Mean_Divergence_PopK_PopL, True_Divergence_PopA_PopB:True_Divergence_PopK_PopL) %>%
  separate(DivergenceType, into = c("Type", "PopPair"), sep = "_Divergence_") %>%
  spread(key = "Type", value = "DivergenceValue")

# Now create the plot
ggplot(df_long, aes(x = True, y = Mean)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue", se = FALSE) +
  labs(x = "True Divergence", y = "Estimated Mean Divergence", 
       title = "True vs Estimated Divergence for Population Pairs") +
  facet_wrap(~ PopPair, scales = "free") +
  theme_minimal()







# Load necessary libraries
library(ggplot2)
library(tidyr)

# Load the data from the file
df <- read.table("/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_3_10000_10000_results/summary_output.txt", 
                 header = TRUE, sep = "\t")

# Gather the Mean and True Divergence columns into a long format
df_long <- df %>%
  gather(key = "PopPair", value = "MeanDivergence", 
         Mean_Divergence_PopA_PopB:Mean_Divergence_PopK_PopL) %>%
  gather(key = "TrueDivergence", value = "TrueValue", 
         True_Divergence_PopA_PopB:True_Divergence_PopK_PopL)

# Ensure the population pairs match correctly
df_long <- df_long[df_long$PopPair == gsub("Mean_Divergence_", "True_Divergence_", df_long$PopPair), ]

# Create the plot
ggplot(df_long, aes(x = TrueValue, y = MeanDivergence, color = PopPair)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + # Add a trend line without standard error shading
  labs(x = "True Divergence", y = "Estimated Mean Divergence", 
       title = "True vs Estimated Divergence for Population Pairs") +
  theme_minimal()





# Load necessary libraries
library(ggplot2)
library(tidyr)

# Load the data from the file
df <- read.table("/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_3_10000_10000_results/summary_output.txt", 
                 header = TRUE, sep = "\t")

# Check column names to ensure correct columns are selected
colnames(df)

# Gather all Mean and True Divergence columns into long format
df_long <- df %>%
  gather(key = "PopPair", value = "Divergence", Mean_Divergence_PopA_PopB:Mean_Divergence_PopK_PopL, 
         True_Divergence_PopA_PopB:True_Divergence_PopK_PopL)

# Create a column indicating whether it's a True or Estimated divergence
df_long <- df_long %>%
  separate(PopPair, into = c("Type", "PopPair"), sep = "_Divergence_") %>%
  spread(key = "Type", value = "Divergence")

# Now create the plot - all population pairs on the same plot
ggplot(df_long, aes(x = True, y = Mean, color = PopPair)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +  # Add trend line for each population pair
  labs(x = "True Divergence", y = "Estimated Mean Divergence", 
       title = "True vs Estimated Divergence for Population Pairs") +
  theme_minimal()








# Load necessary libraries
library(ggplot2)
library(tidyr)

# Load the data from the file
df <- read.table("/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_3_10000_10000_results/summary_output.txt", 
                 header = TRUE, sep = "\t")

# Gather the Mean and True Divergence columns into a long format
df_long <- df %>%
  gather(key = "PopPair", value = "MeanDivergence", 
         Mean_Divergence_PopA_PopB:Mean_Divergence_PopK_PopL) %>%
  gather(key = "TrueDivergence", value = "TrueValue", 
         True_Divergence_PopA_PopB:True_Divergence_PopK_PopL)

# Ensure the population pairs match correctly
df_long <- df_long[df_long$PopPair == gsub("Mean_Divergence_", "True_Divergence_", df_long$PopPair), ]

# Multiply the true values by 10e-8
df_long$TrueValue <- df_long$TrueValue * 1e-8

# Create the plot
ggplot(df_long, aes(x = TrueValue, y = MeanDivergence, color = PopPair)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + # Add a trend line without standard error shading
  labs(x = "True Divergence (x 10^-8)", y = "Estimated Mean Divergence", 
       title = "True vs Estimated Divergence for Population Pairs") +
  theme_minimal()






# Load necessary libraries
library(ggplot2)
library(tidyr)

# Load the data
df <- read.table("/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_3_10000_10000_results/summary_output.txt", 
                 header = TRUE, sep = "\t")

# Reshape the dataframe to gather all Mean and True Divergence columns
df_long <- df %>%
  gather(key = "PopPair", value = "MeanDivergence", 
         Mean_Divergence_PopA_PopB:Mean_Divergence_PopK_PopL) %>%
  gather(key = "TruePopPair", value = "TrueValue", 
         True_Divergence_PopA_PopB:True_Divergence_PopK_PopL)

# Ensure that the population pairs match correctly
df_long <- df_long[df_long$PopPair == gsub("Mean_Divergence_", "True_Divergence_", df_long$TruePopPair), ]

# Multiply the true divergence values by 10e-8
df_long$TrueValue <- df_long$TrueValue * 1e-8

# Create the plot, showing all population pairs on the same plot
ggplot(df_long, aes(x = TrueValue, y = MeanDivergence, color = PopPair)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +  # Add a trend line
  labs(x = "True Divergence (x 10^-8)", y = "Estimated Mean Divergence", 
       title = "True vs Estimated Divergence for Population Pairs") +
  theme_minimal()






# Load necessary libraries
library(tidyr)
library(dplyr)

# Load the data
df <- read.table("/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_3_10000_10000_results/summary_output.txt", 
                 header = TRUE, sep = "\t")

# Gather the Mean and True Divergence columns
df_long <- df %>%
  gather(key = "PopPair", value = "MeanDivergence", 
         Mean_Divergence_PopA_PopB:Mean_Divergence_PopK_PopL)

df_long_true <- df %>%
  gather(key = "PopPair", value = "TrueValue", 
         True_Divergence_PopA_PopB:True_Divergence_PopK_PopL)

# Combine the Mean and True data
df_combined <- df_long %>%
  left_join(df_long_true, by = c("PopPair" = "PopPair"))

# Inspect the combined data
head(df_combined)









# Load necessary libraries
library(ggplot2)
library(tidyr)
library(dplyr)

# Load the data
df <- read.table("/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_3_10000_10000_results/summary_output.txt", 
                 header = TRUE, sep = "\t")

# Gather mean divergence values
df_long_mean <- df %>%
  gather(key = "PopPair", value = "MeanDivergence", 
         Mean_Divergence_PopA_PopB:Mean_Divergence_PopK_PopL)

# Gather true divergence values
df_long_true <- df %>%
  gather(key = "PopPair", value = "TrueValue", 
         True_Divergence_PopA_PopB:True_Divergence_PopK_PopL)

# Fix the PopPair names for the true divergences to match those of mean divergences
df_long_true$PopPair <- gsub("True_Divergence_", "Mean_Divergence_", df_long_true$PopPair)

# Combine the two datasets by matching the PopPair column
df_combined <- inner_join(df_long_mean, df_long_true, by = "PopPair")

# Multiply the true values by 10^-8
df_combined$TrueValue <- df_combined$TrueValue * 1e-8

# Create the plot
ggplot(df_combined, aes(x = TrueValue, y = MeanDivergence, color = PopPair)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +  # Add trend lines without confidence shading
  labs(x = "True Divergence (x 10^-8)", y = "Estimated Mean Divergence", 
       title = "True vs Estimated Divergence for Population Pairs") +
  theme_minimal()






# Load necessary libraries
library(ggplot2)
library(tidyr)
library(dplyr)

# Load the data
df <- read.table("/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_3_10000_10000_results/summary_output.txt", 
                 header = TRUE, sep = "\t")

# Gather mean divergence values
df_long_mean <- df %>%
  gather(key = "PopPair", value = "MeanDivergence", 
         Mean_Divergence_PopA_PopB:Mean_Divergence_PopK_PopL)

# Gather true divergence values
df_long_true <- df %>%
  gather(key = "PopPair", value = "TrueValue", 
         True_Divergence_PopA_PopB:True_Divergence_PopK_PopL)

# Fix the PopPair names for the true divergences to match those of mean divergences
df_long_true$PopPair <- gsub("True_Divergence_", "Mean_Divergence_", df_long_true$PopPair)

# Combine the two datasets by matching the PopPair column
df_combined <- inner_join(df_long_mean, df_long_true, by = "PopPair")

# Multiply the true values by 10^-8
df_combined$TrueValue <- df_combined$TrueValue * 1e-8

# Create the plot
ggplot(df_combined, aes(x = TrueValue, y = MeanDivergence, color = PopPair)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +  # Add trend lines without confidence shading
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +  # Add 1:1 diagonal line
  labs(x = "True Divergence (x 10^-8)", y = "Estimated Mean Divergence", 
       title = "True vs Estimated Divergence for Population Pairs") +
  theme_minimal()







# Load necessary libraries
library(ggplot2)
library(tidyr)
library(dplyr)

# Load the data
df <- read.table("/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_3_10000_10000_results/summary_output.txt", 
                 header = TRUE, sep = "\t")

# Gather mean divergence values
df_long_mean <- df %>%
  gather(key = "PopPair", value = "MeanDivergence", 
         Mean_Divergence_PopA_PopB:Mean_Divergence_PopK_PopL)

# Gather true divergence values
df_long_true <- df %>%
  gather(key = "PopPair", value = "TrueValue", 
         True_Divergence_PopA_PopB:True_Divergence_PopK_PopL)

# Fix the PopPair names for the true divergences to match those of mean divergences
df_long_true$PopPair <- gsub("True_Divergence_", "Mean_Divergence_", df_long_true$PopPair)

# Combine the two datasets by matching the PopPair column
df_combined <- inner_join(df_long_mean, df_long_true, by = "PopPair")

# Divide the mean estimates by 10^-8 to scale them
df_combined$MeanDivergence <- df_combined$MeanDivergence / 1e-8

# Create the plot
ggplot(df_combined, aes(x = TrueValue, y = MeanDivergence, color = PopPair)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +  # Add trend lines without confidence shading
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +  # Add 1:1 diagonal line
  labs(x = "True Divergence", y = "Estimated Mean Divergence (x 10^-8)", 
       title = "True vs Estimated Divergence for Population Pairs") +
  theme_minimal()






# Load necessary libraries
library(ggplot2)
library(tidyr)
library(dplyr)

# Load the data
df <- read.table("/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_3_10000_10000_results/summary_output.txt", 
                 header = TRUE, sep = "\t")

# Gather mean divergence values
df_long_mean <- df %>%
  gather(key = "PopPair", value = "MeanDivergence", 
         Mean_Divergence_PopA_PopB:Mean_Divergence_PopK_PopL)

# Gather true divergence values
df_long_true <- df %>%
  gather(key = "PopPair", value = "TrueValue", 
         True_Divergence_PopA_PopB:True_Divergence_PopK_PopL)

# Fix the PopPair names for the true divergences to match those of mean divergences
df_long_true$PopPair <- gsub("True_Divergence_", "Mean_Divergence_", df_long_true$PopPair)

# Combine the two datasets by matching the PopPair column
df_combined <- inner_join(df_long_mean, df_long_true, by = "PopPair")

# Divide the mean estimates by 10^-8 to scale them
df_combined$MeanDivergence <- df_combined$MeanDivergence / 1e-8

# Create the plot
ggplot(df_combined, aes(x = TrueValue, y = MeanDivergence, color = PopPair)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +  # Add trend lines without confidence shading
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +  # Add 1:1 diagonal line
  labs(x = "True Divergence", y = "Estimated Mean Divergence (x 10^-8)", 
       title = "True vs Estimated Divergence for Population Pairs") +
  theme_minimal() +
  coord_fixed() +  # Ensures a 1:1 aspect ratio for the plot
  xlim(range(df_combined$TrueValue)) +  # Set x-axis limits to the range of the true values
  ylim(range(df_combined$MeanDivergence))  # Set y-axis limits to the range of the mean divergence






# Load necessary libraries
library(ggplot2)
library(tidyr)
library(dplyr)

# Load the data
df <- read.table("/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_3_10000_10000_results/summary_output.txt", 
                 header = TRUE, sep = "\t")

# Gather mean divergence values
df_long_mean <- df %>%
  gather(key = "PopPair", value = "MeanDivergence", 
         Mean_Divergence_PopA_PopB:Mean_Divergence_PopK_PopL)

# Gather true divergence values
df_long_true <- df %>%
  gather(key = "PopPair", value = "TrueValue", 
         True_Divergence_PopA_PopB:True_Divergence_PopK_PopL)

# Fix the PopPair names for the true divergences to match those of mean divergences
df_long_true$PopPair <- gsub("True_Divergence_", "Mean_Divergence_", df_long_true$PopPair)

# Combine the two datasets by matching the PopPair column
df_combined <- inner_join(df_long_mean, df_long_true, by = "PopPair")

# Divide the mean estimates by 10^-8 to scale them
df_combined$MeanDivergence <- df_combined$MeanDivergence / 1e-8

# Get the maximum limit to set the same range for both axes
max_limit <- max(max(df_combined$TrueValue), max(df_combined$MeanDivergence))

# Create the plot with a square aspect ratio
ggplot(df_combined, aes(x = TrueValue, y = MeanDivergence, color = PopPair)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +  # Add trend lines without confidence shading
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +  # Add 1:1 diagonal line
  labs(x = "True Divergence", y = "Estimated Mean Divergence (x 10^-8)", 
       title = "True vs Estimated Divergence for Population Pairs") +
  theme_bw() +
  coord_fixed(ratio = 1) +  # Force a 1:1 aspect ratio
  xlim(0, max_limit) +  # Set x-axis limits from 0 to max limit
  ylim(0, max_limit)  # Set y-axis limits from 0 to max limit



# Create the plot with a square aspect ratio and legend in the bottom-right
ggplot(df_combined, aes(x = TrueValue, y = MeanDivergence, color = PopPair)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +  # Add trend lines without confidence shading
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +  # Add 1:1 diagonal line
  labs(x = "True Divergence", y = "Estimated Mean Divergence (x 10^-8)", 
       title = "True vs Estimated Divergence for Population Pairs") +
  theme_bw() +
  coord_fixed(ratio = 1) +  # Force a 1:1 aspect ratio
  xlim(0, max_limit) +  # Set x-axis limits from 0 to max limit
  ylim(0, max_limit) +  # Set y-axis limits from 0 to max limit
  theme(legend.position = c(0.75, 0.2))  # Move the legend to the bottom-right corner



# Create the plot with a square aspect ratio, reduced legend size, and legend in the bottom-right
ggplot(df_combined, aes(x = TrueValue, y = MeanDivergence, color = PopPair)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +  # Add trend lines without confidence shading
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +  # Add 1:1 diagonal line
  labs(x = "True Divergence", y = "Estimated Mean Divergence (x 10^-8)", 
       title = "True vs Estimated Divergence for Population Pairs") +
  theme_bw() +
  coord_fixed(ratio = 1) +  # Force a 1:1 aspect ratio
  xlim(0, max_limit) +  # Set x-axis limits from 0 to max limit
  ylim(0, max_limit) +  # Set y-axis limits from 0 to max limit
  theme(legend.position = c(0.75, 0.2),  # Move the legend to the bottom-right corner
        legend.key.size = unit(0.2, 'cm'),  # Reduce the size of the legend key
        legend.text = element_text(size = 8),  # Reduce the size of the legend text
        legend.title = element_text(size = 9))  # Reduce the size of the legend title (if present)






# Create the plot with a square aspect ratio, reduced legend size, and transparent points
ggplot(df_combined, aes(x = TrueValue, y = MeanDivergence, color = PopPair)) +
  geom_point(alpha = 0.3) +  # Set alpha transparency for the points
  geom_smooth(method = "lm", se = FALSE) +  # Add trend lines without confidence shading
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +  # Add 1:1 diagonal line
  labs(x = "True Divergence", y = "Estimated Mean Divergence (x 10^-8)", 
       title = "True vs Estimated Divergence for Population Pairs") +
  theme_bw() +
  coord_fixed(ratio = 1) +  # Force a 1:1 aspect ratio
  xlim(0, max_limit) +  # Set x-axis limits from 0 to max limit
  ylim(0, max_limit) +  # Set y-axis limits from 0 to max limit
  theme(legend.position = c(0.75, 0.2),  # Move the legend to the bottom-right corner
        legend.key.size = unit(0.2, 'cm'),  # Reduce the size of the legend key
        legend.text = element_text(size = 8),  # Reduce the size of the legend text
        legend.title = element_text(size = 9))  # Reduce the size of the legend title (if present)




# Create the plot with a square aspect ratio, reduced legend size, and more transparent points
ggplot(df_combined, aes(x = TrueValue, y = MeanDivergence, color = PopPair)) +
  geom_point(alpha = 0.3, size = 2) +  # Set a stronger alpha transparency and slightly larger points
  geom_smooth(method = "lm", se = FALSE) +  # Add trend lines without confidence shading
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +  # Add 1:1 diagonal line
  labs(x = "True Divergence", y = "Estimated Mean Divergence (x 10^-8)", 
       title = "True vs Estimated Divergence for Population Pairs") +
  theme_bw() +
  coord_fixed(ratio = 1) +  # Force a 1:1 aspect ratio
  xlim(0, max_limit) +  # Set x-axis limits from 0 to max limit
  ylim(0, max_limit) +  # Set y-axis limits from 0 to max limit
  theme(legend.position = c(0.75, 0.2),  # Move the legend to the bottom-right corner
        legend.key.size = unit(0.2, 'cm'),  # Reduce the size of the legend key
        legend.text = element_text(size = 8),  # Reduce the size of the legend text
        legend.title = element_text(size = 9))  # Reduce the size of the legend title (if present)




# Create the plot with a square aspect ratio, reduced legend size, and transparent points
ggplot(df_combined, aes(x = TrueValue, y = MeanDivergence, color = PopPair)) +
  geom_point(aes(alpha = 0.4), size = 3) +  # Set alpha transparency for the points explicitly inside aes()
  geom_smooth(method = "lm", se = FALSE) +  # Add trend lines without confidence shading
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +  # Add 1:1 diagonal line
  labs(x = "True Divergence", y = "Estimated Mean Divergence (x 10^-8)", 
       title = "True vs Estimated Divergence for Population Pairs") +
  theme_bw() +
  coord_fixed(ratio = 1) +  # Force a 1:1 aspect ratio
  xlim(0, max_limit) +  # Set x-axis limits from 0 to max limit
  ylim(0, max_limit) +  # Set y-axis limits from 0 to max limit
  scale_alpha(range = c(0.3, 0.4), guide = "none") +  # Ensure transparency is applied and not overridden
  theme(legend.position = c(0.75, 0.2),  # Move the legend to the bottom-right corner
        legend.key.size = unit(0.2, 'cm'),  # Reduce the size of the legend key
        legend.text = element_text(size = 8),  # Reduce the size of the legend text
        legend.title = element_text(size = 9))  # Reduce the size of the legend title (if present)





# Create the plot with a square aspect ratio, reduced legend size, and transparent points
ggplot(df_combined, aes(x = TrueValue, y = MeanDivergence, color = PopPair)) +
  geom_point(alpha = 0.3, size = 6) +  # Set alpha transparency for the points directly here
  geom_smooth(method = "lm", se = FALSE) +  # Add trend lines without confidence shading
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +  # Add 1:1 diagonal line
  labs(x = "True Divergence", y = "Estimated Mean Divergence (x 10^-8)", 
       title = "True vs Estimated Divergence for Population Pairs") +
  theme_bw() +
  coord_fixed(ratio = 1) +  # Force a 1:1 aspect ratio
  xlim(0, max_limit) +  # Set x-axis limits from 0 to max limit
  ylim(0, max_limit) +  # Set y-axis limits from 0 to max limit
  theme(legend.position = c(0.75, 0.2),  # Move the legend to the bottom-right corner
        legend.key.size = unit(0.2, 'cm'),  # Reduce the size of the legend key
        legend.text = element_text(size = 8),  # Reduce the size of the legend text
        legend.title = element_text(size = 9))  # Reduce the size of the legend title (if present)





# Create the plot with a square aspect ratio, reduced legend size, and transparent points
ggplot(df_combined, aes(x = TrueValue, y = MeanDivergence, color = PopPair)) +
  geom_point(alpha = 0.2, size = 3) +  # Directly set alpha transparency for points
  geom_smooth(method = "lm", se = FALSE) +  # Add trend lines without confidence shading
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +  # Add 1:1 diagonal line
  labs(x = "True Divergence", y = "Estimated Mean Divergence (x 10^-8)", 
       title = "True vs Estimated Divergence for Population Pairs") +
  theme_bw() +
  coord_fixed(ratio = 1) +  # Force a 1:1 aspect ratio
  xlim(0, max_limit) +  # Set x-axis limits from 0 to max limit
  ylim(0, max_limit) +  # Set y-axis limits from 0 to max limit
  scale_color_manual(values = c("red", "blue", "green", "purple", "orange", "yellow")) +  # Set fixed colors to prevent overrides
  theme(legend.position = c(0.75, 0.2),  # Move the legend to the bottom-right corner
        legend.key.size = unit(0.2, 'cm'),  # Reduce the size of the legend key
        legend.text = element_text(size = 8),  # Reduce the size of the legend text
        legend.title = element_text(size = 9))  # Reduce the size of the legend title (if present)




# Create the plot with transparent points and a fixed square aspect ratio
ggplot(df_combined, aes(x = TrueValue, y = MeanDivergence, color = PopPair)) +
  geom_point(size = 3, alpha = 0.5) +  # Set alpha transparency for the points directly here
  geom_smooth(method = "lm", se = FALSE) +  # Add trend lines without confidence shading
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +  # Add 1:1 diagonal line
  labs(x = "True Divergence", y = "Estimated Mean Divergence (x 10^-8)", 
       title = "True vs Estimated Divergence for Population Pairs") +
  theme_bw() +
  coord_fixed(ratio = 1) +  # Force a 1:1 aspect ratio
  xlim(0, max_limit) +  # Set x-axis limits from 0 to max limit
  ylim(0, max_limit) +  # Set y-axis limits from 0 to max limit
  guides(alpha = "none") +  # Remove alpha from the legend
  theme(legend.position = c(0.75, 0.2),  # Move the legend to the bottom-right corner
        legend.key.size = unit(0.2, 'cm'),  # Reduce the size of the legend key
        legend.text = element_text(size = 8),  # Reduce the size of the legend text
        legend.title = element_text(size = 9))  # Reduce the size of the legend title (if present)



# Create the plot with a square aspect ratio, reduced legend size, and transparent points
ggplot(df_combined, aes(x = TrueValue, y = MeanDivergence, color = PopPair)) +
  geom_point(alpha = 0.3, size = 3) +  # Directly set alpha transparency for the points
  geom_smooth(method = "lm", se = FALSE) +  # Add trend lines without confidence shading
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +  # Add 1:1 diagonal line
  labs(x = "True Divergence", y = "Estimated Mean Divergence (x 10^-8)", 
       title = "True vs Estimated Divergence for Population Pairs") +
  theme_bw() +
  coord_fixed(ratio = 1) +  # Force a 1:1 aspect ratio
  xlim(0, max_limit) +  # Set x-axis limits from 0 to max limit
  ylim(0, max_limit) +  # Set y-axis limits from 0 to max limit
  theme(legend.position = c(0.75, 0.2),  # Move the legend to the bottom-right corner
        legend.key.size = unit(0.2, 'cm'),  # Reduce the size of the legend key
        legend.text = element_text(size = 8),  # Reduce the size of the legend text
        legend.title = element_text(size = 9))  # Reduce the size of the legend title (if present)



# Create a simple test plot to check if alpha works
ggplot(df_combined, aes(x = TrueValue, y = MeanDivergence)) +
  geom_point(color = "blue", alpha = 0.3, size = 3) +  # Set alpha and color explicitly
  labs(x = "True Divergence", y = "Estimated Mean Divergence (x 10^-8)", 
       title = "Test Plot: True vs Estimated Divergence") +
  theme_minimal()



# Save the plot as a PNG file to check if alpha transparency is working
png("test_alpha_plot.png", width = 800, height = 800)

# Create the plot with transparency
ggplot(df_combined, aes(x = TrueValue, y = MeanDivergence)) +
  geom_point(color = "blue", alpha = 0.3, size = 3) +  # Set alpha and color explicitly
  labs(x = "True Divergence", y = "Estimated Mean Divergence (x 10^-8)", 
       title = "Test Plot: True vs Estimated Divergence") +
  theme_minimal()

dev.off()  # Close the PNG device





# Save the plot as a PDF file to check if alpha transparency is working
pdf("test_alpha_plot.pdf", width = 8, height = 8)

# Create the plot with transparency
ggplot(df_combined, aes(x = TrueValue, y = MeanDivergence)) +
  geom_point(color = "blue", alpha = 0.3, size = 3) +  # Set alpha and color explicitly
  labs(x = "True Divergence", y = "Estimated Mean Divergence (x 10^-8)", 
       title = "Test Plot: True vs Estimated Divergence") +
  theme_minimal()

dev.off()  # Close the PDF device



# Check ggplot2 version
packageVersion("ggplot2")

# Update ggplot2 if it's not the latest version
install.packages("ggplot2")




install.packages("Cairo")



# Use Cairo to render the plot
library(Cairo)

# Save the plot using Cairo as the rendering device
CairoPNG("cairo_test_alpha_plot.png", width = 800, height = 800)

# Create the plot with transparency
ggplot(df_combined, aes(x = TrueValue, y = MeanDivergence)) +
  geom_point(color = "blue", alpha = 0.3, size = 3) +  # Set alpha transparency
  labs(x = "True Divergence", y = "Estimated Mean Divergence (x 10^-8)", 
       title = "Test Plot: True vs Estimated Divergence") +
  theme_minimal()

dev.off()  # Close the Cairo device



# Generate a simple dataset
test_data <- data.frame(
  x = rnorm(100),
  y = rnorm(100)
)

# Create a simple test plot with alpha transparency
ggplot(test_data, aes(x = x, y = y)) +
  geom_point(alpha = 0.3, size = 3, color = "blue") +
  labs(title = "Test Plot: Alpha Transparency Example") +
  theme_minimal()




# Inspect the structure of the combined dataframe
str(df_combined)

# Inspect the first few rows of the dataframe
head(df_combined)

# Check for NA values or extreme values in TrueValue and MeanDivergence
summary(df_combined)






# Create a simplified plot with just the core variables
ggplot(df_combined, aes(x = TrueValue, y = MeanDivergence)) +
  geom_point(color = "blue", alpha = 0.3, size = 3) +  # Set alpha transparency for points
  labs(x = "True Divergence", y = "Estimated Mean Divergence (x 10^-8)", 
       title = "Simplified Plot: True vs Estimated Divergence") +
  theme_minimal()



# Take a smaller subset of the data for testing transparency
df_subset <- df_combined[1:100, ]  # Take the first 100 rows

# Create the plot using a subset of the data
ggplot(df_subset, aes(x = TrueValue, y = MeanDivergence)) +
  geom_point(color = "red", alpha = 0.3, size = 3) +  # Apply transparency
  labs(x = "True Divergence", y = "Estimated Mean Divergence (x 10^-8)", 
       title = "Subset Test Plot: True vs Estimated Divergence") +
  theme_minimal()



# Check for summary statistics of key variables
summary(df_combined$TrueValue)
summary(df_combined$MeanDivergence)



# Ensure TrueValue and MeanDivergence are numeric
df_combined$TrueValue <- as.numeric(df_combined$TrueValue)
df_combined$MeanDivergence <- as.numeric(df_combined$MeanDivergence)

# Ensure PopPair is a factor
df_combined$PopPair <- as.factor(df_combined$PopPair)

# Plot again with this cleaned data
ggplot(df_combined, aes(x = TrueValue, y = MeanDivergence, color = PopPair)) +
  geom_point(alpha = 0.3, size = 3) +  # Set alpha transparency for points
  labs(x = "True Divergence", y = "Estimated Mean Divergence (x 10^-8)", 
       title = "True vs Estimated Divergence with Cleaned Data") +
  theme_minimal()




# Normalize the values of TrueValue and MeanDivergence
df_combined$TrueValue_scaled <- scale(df_combined$TrueValue)
df_combined$MeanDivergence_scaled <- scale(df_combined$MeanDivergence)

# Create a plot with scaled values
ggplot(df_combined, aes(x = TrueValue_scaled, y = MeanDivergence_scaled)) +
  geom_point(color = "blue", alpha = 0.3, size = 3) +  # Set alpha transparency for points
  labs(x = "Scaled True Divergence", y = "Scaled Estimated Mean Divergence", 
       title = "Plot with Scaled Values") +
  theme_minimal()




# Create test data
test_data <- data.frame(
  x = rnorm(100, mean = 10, sd = 2),
  y = rnorm(100, mean = 10, sd = 2)
)

# Create a combined plot: test data on the left, your data on the right
par(mfrow = c(1, 2))  # Set up a side-by-side plot layout

# Plot test data
plot(test_data$x, test_data$y, col = rgb(0, 0, 1, 0.3), pch = 19, 
     main = "Test Data with Alpha", xlab = "X", ylab = "Y")

# Plot your data
plot(df_combined$TrueValue, df_combined$MeanDivergence, col = rgb(1, 0, 0, 0.3), pch = 19, 
     main = "Your Data with Alpha", xlab = "True Divergence", ylab = "Mean Divergence")



# Check for duplicate values in TrueValue and MeanDivergence
sum(duplicated(df_combined$TrueValue))
sum(duplicated(df_combined$MeanDivergence))

# Check if there are constant values (no variation) in TrueValue or MeanDivergence
length(unique(df_combined$TrueValue))
length(unique(df_combined$MeanDivergence))




# Filter out duplicate rows based on TrueValue and MeanDivergence
df_unique <- df_combined[!duplicated(df_combined[, c("TrueValue", "MeanDivergence")]), ]




# Add jitter to spread the points out slightly
ggplot(df_combined, aes(x = TrueValue, y = MeanDivergence, color = PopPair)) +
  geom_point(alpha = 0.3, size = 3, position = position_jitter(width = 0.2, height = 0.2)) +  # Add jitter
  labs(x = "True Divergence", y = "Estimated Mean Divergence (x 10^-8)", 
       title = "True vs Estimated Divergence with Jitter") +
  theme_minimal()




# Add jitter to spread the points out slightly
ggplot(df_combined, aes(x = TrueValue, y = MeanDivergence, color = PopPair)) +
  geom_point(alpha = 0.3, size = 3, position = position_jitter(width = 0.2, height = 0.2)) +  # Add jitter
  labs(x = "True Divergence", y = "Estimated Mean Divergence (x 10^-8)", 
       title = "True vs Estimated Divergence with Jitter") +
  theme_minimal()







# Remove duplicates based on TrueValue and MeanDivergence
df_unique <- df_combined[!duplicated(df_combined[, c("TrueValue", "MeanDivergence")]), ]

# Plot the data after removing duplicates
ggplot(df_unique, aes(x = TrueValue, y = MeanDivergence, color = PopPair)) +
  geom_point(alpha = 0.3, size = 3) +  # Set alpha transparency for the points
  labs(x = "True Divergence", y = "Estimated Mean Divergence (x 10^-8)", 
       title = "True vs Estimated Divergence (No Duplicates)") +
  theme_minimal()







# Create the plot with a square aspect ratio, diagonal line, transparency, and no duplicates
ggplot(df_unique, aes(x = TrueValue, y = MeanDivergence, color = PopPair)) +
  geom_point(alpha = 0.3, size = 3) +  # Set alpha transparency and point size
  geom_smooth(method = "lm", se = FALSE) +  # Add trend lines without confidence shading
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +  # Add 1:1 diagonal line
  labs(x = "True Divergence", y = "Estimated Mean Divergence (x 10^-8)", 
       title = "True vs Estimated Divergence (No Duplicates)") +
  theme_bw() +
  coord_fixed(ratio = 1) +  # Force a 1:1 aspect ratio
  xlim(0, max(df_unique$TrueValue)) +  # Set x-axis limits to the range of TrueValue
  ylim(0, max(df_unique$MeanDivergence)) +  # Set y-axis limits to the range of MeanDivergence
  theme(legend.position = c(0.75, 0.2),  # Move the legend to the bottom-right corner
        legend.key.size = unit(0.2, 'cm'),  # Reduce the size of the legend key
        legend.text = element_text(size = 8),  # Reduce the size of the legend text
        legend.title = element_text(size = 9))  # Adjust the size of the legend title (if present)





# Determine the max limit to make both axes equal and square
max_limit <- max(c(max(df_unique$TrueValue), max(df_unique$MeanDivergence)))

# Create the square plot with diagonal line and no duplicates
ggplot(df_unique, aes(x = TrueValue, y = MeanDivergence, color = PopPair)) +
  geom_point(alpha = 0.3, size = 3) +  # Set alpha transparency and point size
  geom_smooth(method = "lm", se = FALSE) +  # Add trend lines without confidence shading
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +  # Add 1:1 diagonal line
  labs(x = "True Divergence", y = "Estimated Mean Divergence (x 10^-8)", 
       title = "True vs Estimated Divergence (No Duplicates)") +
  theme_bw() +
  coord_fixed(ratio = 1) +  # Ensure the aspect ratio is 1:1
  xlim(0, max_limit) +  # Set both x and y axis limits to the same max value
  ylim(0, max_limit) +  # Set both x and y axis limits to the same max value
  theme(legend.position = c(0.75, 0.2),  # Move the legend to the bottom-right corner
        legend.key.size = unit(0.2, 'cm'),  # Reduce the size of the legend key
        legend.text = element_text(size = 8),  # Reduce the size of the legend text
        legend.title = element_text(size = 9))  # Adjust the size of the legend title (if present)

             



             
             
             





# Load the new summary file
df_new <- read.table("/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_3_20000_10000_results/summary_output.txt", 
                     header = TRUE, sep = "\t")

# Preview the new data
head(df_new)




# Remove duplicates based on TrueValue and MeanDivergence
df_new_unique <- df_new[!duplicated(df_new[, c("TrueValue", "MeanDivergence")]), ]






library(tidyr)

# Gather the True and Mean divergence values into long format
df_new_long <- df_new %>%
  gather(key = "PopPair", value = "MeanDivergence", 
         Mean_Divergence_PopA_PopB:Mean_Divergence_PopK_PopL) %>%
  gather(key = "TruePopPair", value = "TrueValue", 
         True_Divergence_PopA_PopB:True_Divergence_PopK_PopL)

# Ensure that the pairs match between the true and mean values
df_new_long <- df_new_long[df_new_long$PopPair == gsub("True_Divergence_", "Mean_Divergence_", df_new_long$TruePopPair), ]



# Remove duplicates
df_new_unique <- df_new_long[!duplicated(df_new_long[, c("TrueValue", "MeanDivergence")]), ]



# Determine the max limit to make both axes equal and square
max_limit_new <- max(c(max(df_new_unique$TrueValue), max(df_new_unique$MeanDivergence)))

# Create the square plot for the new data
ggplot(df_new_unique, aes(x = TrueValue, y = MeanDivergence, color = PopPair)) +
  geom_point(alpha = 0.3, size = 3) +  # Set alpha transparency and point size
  geom_smooth(method = "lm", se = FALSE) +  # Add trend lines without confidence shading
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +  # Add 1:1 diagonal line
  labs(x = "True Divergence", y = "Estimated Mean Divergence (x 10^-8)", 
       title = "True vs Estimated Divergence (No Duplicates, New File)") +
  theme_bw() +
  coord_fixed(ratio = 1) +  # Ensure the aspect ratio is 1:1
  xlim(0, max_limit_new) +  # Set both x and y axis limits to the same max value
  ylim(0, max_limit_new) +  # Set both x and y axis limits to the same max value
  theme(legend.position = c(0.75, 0.2),  # Move the legend to the bottom-right corner
        legend.key.size = unit(0.2, 'cm'),  # Reduce the size of the legend key
        legend.text = element_text(size = 8),  # Reduce the size of the legend text
        legend.title = element_text(size = 9))  # Adjust the size of the legend title (if present)







library(tidyr)
library(dplyr)

# Gather the Mean divergence values into long format
df_new_long <- df_new %>%
  gather(key = "PopPair", value = "MeanDivergence", 
         Mean_Divergence_PopA_PopB:Mean_Divergence_PopK_PopL)

# Gather the True divergence values into long format
df_new_long_true <- df_new %>%
  gather(key = "PopPair_True", value = "TrueValue", 
         True_Divergence_PopA_PopB:True_Divergence_PopK_PopL)

# Ensure the PopPair names for True and Mean Divergence match correctly
df_new_long_true$PopPair_True <- gsub("True_Divergence_", "Mean_Divergence_", df_new_long_true$PopPair_True)

# Merge both datasets based on matching population pairs and runs
df_combined_new <- inner_join(df_new_long, df_new_long_true, by = c("Run", "PopPair" = "PopPair_True"))




# Remove duplicates if necessary
df_combined_new_unique <- df_combined_new[!duplicated(df_combined_new[, c("TrueValue", "MeanDivergence")]), ]




# Determine the max limit to make both axes equal and square
max_limit_new <- max(c(max(df_combined_new_unique$TrueValue), max(df_combined_new_unique$MeanDivergence)))

# Create the square plot for the new data
ggplot(df_combined_new_unique, aes(x = TrueValue, y = MeanDivergence, color = PopPair)) +
  geom_point(alpha = 0.3, size = 3) +  # Set alpha transparency and point size
  geom_smooth(method = "lm", se = FALSE) +  # Add trend lines without confidence shading
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +  # Add 1:1 diagonal line
  labs(x = "True Divergence", y = "Estimated Mean Divergence (x 10^-8)", 
       title = "True vs Estimated Divergence (No Duplicates, New File)") +
  theme_bw() +
  coord_fixed(ratio = 1) +  # Ensure the aspect ratio is 1:1
  xlim(0, max_limit_new) +  # Set both x and y axis limits to the same max value
  ylim(0, max_limit_new) +  # Set both x and y axis limits to the same max value
  theme(legend.position = c(0.75, 0.2),  # Move the legend to the bottom-right corner
        legend.key.size = unit(0.2, 'cm'),  # Reduce the size of the legend key
        legend.text = element_text(size = 8),  # Reduce the size of the legend text
        legend.title = element_text(size = 9))  # Adjust the size of the legend title (if present)




library(tidyr)
library(dplyr)

# Gather the Mean divergence values into long format
df_new_long_mean <- df_new %>%
  gather(key = "PopPair", value = "MeanDivergence", 
         Mean_Divergence_PopA_PopB:Mean_Divergence_PopK_PopL)

# Gather the True divergence values into long format
df_new_long_true <- df_new %>%
  gather(key = "PopPair_True", value = "TrueValue", 
         True_Divergence_PopA_PopB:True_Divergence_PopK_PopL)

# Ensure that the PopPair and PopPair_True names are properly aligned
df_new_long_true$PopPair_True <- gsub("True_Divergence_", "Mean_Divergence_", df_new_long_true$PopPair_True)



# Check the first few rows of the gathered Mean Divergence
head(df_new_long_mean)

# Check the first few rows of the gathered True Divergence
head(df_new_long_true)

                          





# Gather the Mean divergence values into long format, keeping only necessary columns
df_new_long_mean <- df_new %>%
  gather(key = "PopPair", value = "MeanDivergence", 
         Mean_Divergence_PopA_PopB:Mean_Divergence_PopK_PopL) %>%
  select(Run, PopPair, MeanDivergence)

# Gather the True divergence values into long format, keeping only necessary columns
df_new_long_true <- df_new %>%
  gather(key = "PopPair_True", value = "TrueValue", 
         True_Divergence_PopA_PopB:True_Divergence_PopK_PopL) %>%
  mutate(PopPair = gsub("True_Divergence_", "Mean_Divergence_", PopPair_True)) %>%
  select(Run, PopPair, TrueValue)




# Merge both datasets based on matching population pairs and runs
df_combined_new <- inner_join(df_new_long_mean, df_new_long_true, by = c("Run", "PopPair"))

# Check the first few rows after merging to ensure correctness
head(df_combined_new)




# Remove duplicates if necessary
df_combined_new_unique <- df_combined_new[!duplicated(df_combined_new[, c("TrueValue", "MeanDivergence")]), ]

# Determine the max limit to make both axes equal and square
max_limit_new <- max(c(max(df_combined_new_unique$TrueValue), max(df_combined_new_unique$MeanDivergence)))

# Create the square plot for the new data
ggplot(df_combined_new_unique, aes(x = TrueValue, y = MeanDivergence, color = PopPair)) +
  geom_point(alpha = 0.3, size = 3) +  # Set alpha transparency and point size
  geom_smooth(method = "lm", se = FALSE) +  # Add trend lines without confidence shading
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +  # Add 1:1 diagonal line
  labs(x = "True Divergence", y = "Estimated Mean Divergence (x 10^-8)", 
       title = "True vs Estimated Divergence (No Duplicates, New File)") +
  theme_bw() +
  coord_fixed(ratio = 1) +  # Ensure the aspect ratio is 1:1
  xlim(0, max_limit_new) +  # Set both x and y axis limits to the same max value
  ylim(0, max_limit_new) +  # Set both x and y axis limits to the same max value
  theme(legend.position = c(0.75, 0.2),  # Move the legend to the bottom-right corner
        legend.key.size = unit(0.2, 'cm'),  # Reduce the size of the legend key
        legend.text = element_text(size = 8),  # Reduce the size of the legend text
        legend.title = element_text(size = 9))  # Adjust the size of the legend title (if present)






# Divide the mean estimated values by 10e-8
df_combined_new_unique$MeanDivergence <- df_combined_new_unique$MeanDivergence / 10e-8

# Determine the max limit to make both axes equal and square
max_limit_new <- max(c(max(df_combined_new_unique$TrueValue), max(df_combined_new_unique$MeanDivergence)))

# Create the square plot for the new data
ggplot(df_combined_new_unique, aes(x = TrueValue, y = MeanDivergence, color = PopPair)) +
  geom_point(alpha = 0.3, size = 3) +  # Set alpha transparency and point size
  geom_smooth(method = "lm", se = FALSE) +  # Add trend lines without confidence shading
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +  # Add 1:1 diagonal line
  labs(x = "True Divergence", y = "Estimated Mean Divergence (x 10^-8)", 
       title = "True vs Estimated Divergence (No Duplicates, New File)") +
  theme_bw() +
  coord_fixed(ratio = 1) +  # Ensure the aspect ratio is 1:1
  xlim(0, max_limit_new) +  # Set both x and y axis limits to the same max value
  ylim(0, max_limit_new) +  # Set both x and y axis limits to the same max value
  theme(legend.position = c(0.75, 0.2),  # Move the legend to the bottom-right corner
        legend.key.size = unit(0.2, 'cm'),  # Reduce the size of the legend key
        legend.text = element_text(size = 8),  # Reduce the size of the legend text
        legend.title = element_text(size = 9))  # Adjust the size of the legend title (if present)



dev.off()

# Divide the mean estimated values by 10e-8
df_combined_new_unique$MeanDivergence <- df_combined_new_unique$MeanDivergence / 10e-8

# Determine the max limit to make both axes equal and square
max_limit_new <- max(c(max(df_combined_new_unique$TrueValue), max(df_combined_new_unique$MeanDivergence)))

# Create the square plot for the new data without the smooth line or diagonal line
ggplot(df_combined_new_unique, aes(x = TrueValue, y = MeanDivergence, color = PopPair)) +
  geom_point(alpha = 0.3, size = 3) +  # Set alpha transparency and point size
  labs(x = "True Divergence", y = "Estimated Mean Divergence (x 10^-8)", 
       title = "True vs Estimated Divergence (No Duplicates, New File)") +
  theme_bw() +
  coord_fixed(ratio = 1) +  # Ensure the aspect ratio is 1:1
  xlim(0, max_limit_new) +  # Set both x and y axis limits to the same max value
  ylim(0, max_limit_new) +  # Set both x and y axis limits to the same max value
  theme(legend.position = c(0.75, 0.2),  # Move the legend to the bottom-right corner
        legend.key.size = unit(0.2, 'cm'),  # Reduce the size of the legend key
        legend.text = element_text(size = 8),  # Reduce the size of the legend text
        legend.title = element_text(size = 9))  # Adjust the size of the legend title (if present)






# Divide the mean estimated values by 10e-8
df_combined_new_unique$MeanDivergence <- df_combined_new_unique$MeanDivergence / 10e-8

# Determine the max limit to make both axes equal and square
max_limit_new <- max(c(max(df_combined_new_unique$TrueValue), max(df_combined_new_unique$MeanDivergence)))

# Create the square plot for the new data with no legend
ggplot(df_combined_new_unique, aes(x = TrueValue, y = MeanDivergence, color = PopPair)) +
  geom_point(alpha = 0.3, size = 3) +  # Set alpha transparency and point size
  geom_smooth(method = "lm", se = FALSE) +  # Add trend lines without confidence shading
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +  # Add 1:1 diagonal line
  labs(x = "True Divergence", y = "Estimated Mean Divergence (x 10^-8)", 
       title = "True vs Estimated Divergence (No Duplicates, New File)") +
  theme_bw() +
  coord_fixed(ratio = 1) +  # Ensure the aspect ratio is 1:1
  xlim(0, max_limit_new) +  # Set both x and y axis limits to the same max value
  ylim(0, max_limit_new) +  # Set both x and y axis limits to the same max value
  theme(legend.position = "none")  # Remove the legend








# Load the summary file
df_new <- read.table("/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_3_10000_10000_results/summary_output.txt", 
                     header = TRUE, sep = "\t")

# Preview the data
head(df_new)




library(tidyr)
library(dplyr)

# Gather the Mean divergence values into long format
df_new_long_mean <- df_new %>%
  gather(key = "PopPair", value = "MeanDivergence", 
         Mean_Divergence_PopA_PopB:Mean_Divergence_PopK_PopL) %>%
  select(Run, PopPair, MeanDivergence)

# Gather the True divergence values into long format
df_new_long_true <- df_new %>%
  gather(key = "PopPair_True", value = "TrueValue", 
         True_Divergence_PopA_PopB:True_Divergence_PopK_PopL) %>%
  mutate(PopPair = gsub("True_Divergence_", "Mean_Divergence_", PopPair_True)) %>%
  select(Run, PopPair, TrueValue)



# Merge both datasets based on matching population pairs and runs
df_combined_new <- inner_join(df_new_long_mean, df_new_long_true, by = c("Run", "PopPair"))

# Preview the merged data
head(df_combined_new)



# Remove duplicates if necessary
df_combined_new_unique <- df_combined_new[!duplicated(df_combined_new[, c("TrueValue", "MeanDivergence")]), ]

# Divide the mean estimated values by 10e-8
df_combined_new_unique$MeanDivergence <- df_combined_new_unique$MeanDivergence / 10e-8



# Determine the max limit to make both axes equal and square
max_limit_new <- max(c(max(df_combined_new_unique$TrueValue), max(df_combined_new_unique$MeanDivergence)))

# Create the square plot for the new data without the legend
ggplot(df_combined_new_unique, aes(x = TrueValue, y = MeanDivergence, color = PopPair)) +
  geom_point(alpha = 0.3, size = 3) +  # Set alpha transparency and point size
  geom_smooth(method = "lm", se = FALSE) +  # Add trend lines without confidence shading
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +  # Add 1:1 diagonal line
  labs(x = "True Divergence", y = "Estimated Mean Divergence (x 10^-8)", 
       title = "True vs Estimated Divergence (No Duplicates, New File)") +
  theme_bw() +
  coord_fixed(ratio = 1) +  # Ensure the aspect ratio is 1:1
  xlim(0, max_limit_new) +  # Set both x and y axis limits to the same max value
  ylim(0, max_limit_new) +  # Set both x and y axis limits to the same max value
  theme(legend.position = "none")  # Remove the legend







# Load the summary file
df_new <- read.table("/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_3_20000_10000_results/summary_output.txt", 
                     header = TRUE, sep = "\t")

# Preview the data
head(df_new)

library(tidyr)
library(dplyr)

# Gather the Mean divergence values into long format
df_new_long_mean <- df_new %>%
  gather(key = "PopPair", value = "MeanDivergence", 
         Mean_Divergence_PopA_PopB:Mean_Divergence_PopK_PopL) %>%
  select(Run, PopPair, MeanDivergence)

# Gather the True divergence values into long format
df_new_long_true <- df_new %>%
  gather(key = "PopPair_True", value = "TrueValue", 
         True_Divergence_PopA_PopB:True_Divergence_PopK_PopL) %>%
  mutate(PopPair = gsub("True_Divergence_", "Mean_Divergence_", PopPair_True)) %>%
  select(Run, PopPair, TrueValue)


# Merge both datasets based on matching population pairs and runs
df_combined_new <- inner_join(df_new_long_mean, df_new_long_true, by = c("Run", "PopPair"))

# Preview the merged data
head(df_combined_new)




# Remove duplicates if necessary
df_combined_new_unique <- df_combined_new[!duplicated(df_combined_new[, c("TrueValue", "MeanDivergence")]), ]

# Divide the mean estimated values by 10e-8
df_combined_new_unique$MeanDivergence <- df_combined_new_unique$MeanDivergence / 10e-8


# Determine the max limit to make both axes equal and square
max_limit_new <- max(c(max(df_combined_new_unique$TrueValue), max(df_combined_new_unique$MeanDivergence)))

# Create the square plot for the new data without the legend
ggplot(df_combined_new_unique, aes(x = TrueValue, y = MeanDivergence, color = PopPair)) +
  geom_point(alpha = 0.3, size = 3) +  # Set alpha transparency and point size
  geom_smooth(method = "lm", se = FALSE) +  # Add trend lines without confidence shading
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +  # Add 1:1 diagonal line
  labs(x = "True Divergence", y = "Estimated Mean Divergence (x 10^-8)", 
       title = "True vs Estimated Divergence (No Duplicates, New File)") +
  theme_bw() +
  coord_fixed(ratio = 1) +  # Ensure the aspect ratio is 1:1
  xlim(0, max_limit_new) +  # Set both x and y axis limits to the same max value
  ylim(0, max_limit_new) +  # Set both x and y axis limits to the same max value
  theme(legend.position = "none")  # Remove the legend









library(tidyr)
library(dplyr)
library(ggplot2)

# Function to process each summary file and create the plot
process_summary_file <- function(file_path, plot_title) {
  # Load the summary file
  df_new <- read.table(file_path, header = TRUE, sep = "\t")
  
  # Gather the Mean divergence values into long format
  df_new_long_mean <- df_new %>%
    gather(key = "PopPair", value = "MeanDivergence", 
           Mean_Divergence_PopA_PopB:Mean_Divergence_PopK_PopL) %>%
    select(Run, PopPair, MeanDivergence)
  
  # Gather the True divergence values into long format
  df_new_long_true <- df_new %>%
    gather(key = "PopPair_True", value = "TrueValue", 
           True_Divergence_PopA_PopB:True_Divergence_PopK_PopL) %>%
    mutate(PopPair = gsub("True_Divergence_", "Mean_Divergence_", PopPair_True)) %>%
    select(Run, PopPair, TrueValue)
  
  # Merge both datasets based on matching population pairs and runs
  df_combined_new <- inner_join(df_new_long_mean, df_new_long_true, by = c("Run", "PopPair"))
  
  # Remove duplicates if necessary
  df_combined_new_unique <- df_combined_new[!duplicated(df_combined_new[, c("TrueValue", "MeanDivergence")]), ]
  
  # Divide the mean estimated values by 10e-8
  df_combined_new_unique$MeanDivergence <- df_combined_new_unique$MeanDivergence / 10e-8
  
  # Determine the max limit to make both axes equal and square
  max_limit_new <- max(c(max(df_combined_new_unique$TrueValue), max(df_combined_new_unique$MeanDivergence)))
  
  # Create the square plot for the new data without the legend
  ggplot(df_combined_new_unique, aes(x = TrueValue, y = MeanDivergence, color = PopPair)) +
    geom_point(alpha = 0.3, size = 3) +  # Set alpha transparency and point size
    geom_smooth(method = "lm", se = FALSE) +  # Add trend lines without confidence shading
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +  # Add 1:1 diagonal line
    labs(x = "True Divergence", y = "Estimated Mean Divergence (x 10^-8)", 
         title = plot_title) +
    theme_bw() +
    coord_fixed(ratio = 1) +  # Ensure the aspect ratio is 1:1
    xlim(0, max_limit_new) +  # Set both x and y axis limits to the same max value
    ylim(0, max_limit_new) +  # Set both x and y axis limits to the same max value
    theme(legend.position = "none")  # Remove the legend
}

# File paths
file_paths <- c(
  "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_3_10000_10000_results/summary_output.txt",
  "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_3_20000_10000_results/summary_output.txt",
  "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_3_40000_10000_results/summary_output.txt"
)

# Titles for the plots
plot_titles <- c(
  "True vs Estimated Divergence (Config 3: 10000_10000)",
  "True vs Estimated Divergence (Config 3: 20000_10000)",
  "True vs Estimated Divergence (Config 3: 40000_10000)"
)

# Process and plot each summary file
for (i in 1:length(file_paths)) {
  process_summary_file(file_paths[i], plot_titles[i])
}









library(tidyr)
library(dplyr)
library(ggplot2)

# Process the summary file and create the plot
file_path <- "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_3_40000_10000_results/summary_output.txt"

# Load the summary file
df_new <- read.table(file_path, header = TRUE, sep = "\t")

# Gather the Mean divergence values into long format
df_new_long_mean <- df_new %>%
  gather(key = "PopPair", value = "MeanDivergence", 
         Mean_Divergence_PopA_PopB:Mean_Divergence_PopK_PopL) %>%
  select(Run, PopPair, MeanDivergence)

# Gather the True divergence values into long format
df_new_long_true <- df_new %>%
  gather(key = "PopPair_True", value = "TrueValue", 
         True_Divergence_PopA_PopB:True_Divergence_PopK_PopL) %>%
  mutate(PopPair = gsub("True_Divergence_", "Mean_Divergence_", PopPair_True)) %>%
  select(Run, PopPair, TrueValue)

# Merge both datasets based on matching population pairs and runs
df_combined_new <- inner_join(df_new_long_mean, df_new_long_true, by = c("Run", "PopPair"))

# Remove duplicates if necessary
df_combined_new_unique <- df_combined_new[!duplicated(df_combined_new[, c("TrueValue", "MeanDivergence")]), ]

# Divide the mean estimated values by 10e-8
df_combined_new_unique$MeanDivergence <- df_combined_new_unique$MeanDivergence / 10e-8

# Determine the max limit to make both axes equal and square
max_limit_new <- max(c(max(df_combined_new_unique$TrueValue), max(df_combined_new_unique$MeanDivergence)))

# Create the square plot for the new data without the legend
ggplot(df_combined_new_unique, aes(x = TrueValue, y = MeanDivergence, color = PopPair)) +
  geom_point(alpha = 0.3, size = 3) +  # Set alpha transparency and point size
  geom_smooth(method = "lm", se = FALSE) +  # Add trend lines without confidence shading
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +  # Add 1:1 diagonal line
  labs(x = "True Divergence", y = "Estimated Mean Divergence (x 10^-8)", 
       title = "True vs Estimated Divergence (Config 3: 40000_10000)") +
  theme_bw() +
  coord_fixed(ratio = 1) +  # Ensure the aspect ratio is 1:1
  xlim(0, max_limit_new) +  # Set both x and y axis limits to the same max value
  ylim(0, max_limit_new) +  # Set both x and y axis limits to the same max value
  theme(legend.position = "none")  # Remove the legend












library(tidyr)
library(dplyr)
library(ggplot2)

# Process the summary file and create the plot
file_path <- "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_3_80000_10000_results/summary_output.txt"

# Load the summary file
df_new <- read.table(file_path, header = TRUE, sep = "\t")

# Gather the Mean divergence values into long format
df_new_long_mean <- df_new %>%
  gather(key = "PopPair", value = "MeanDivergence", 
         Mean_Divergence_PopA_PopB:Mean_Divergence_PopK_PopL) %>%
  select(Run, PopPair, MeanDivergence)

# Gather the True divergence values into long format
df_new_long_true <- df_new %>%
  gather(key = "PopPair_True", value = "TrueValue", 
         True_Divergence_PopA_PopB:True_Divergence_PopK_PopL) %>%
  mutate(PopPair = gsub("True_Divergence_", "Mean_Divergence_", PopPair_True)) %>%
  select(Run, PopPair, TrueValue)

# Merge both datasets based on matching population pairs and runs
df_combined_new <- inner_join(df_new_long_mean, df_new_long_true, by = c("Run", "PopPair"))

# Remove duplicates if necessary
df_combined_new_unique <- df_combined_new[!duplicated(df_combined_new[, c("TrueValue", "MeanDivergence")]), ]

# Divide the mean estimated values by 10e-8
df_combined_new_unique$MeanDivergence <- df_combined_new_unique$MeanDivergence / 10e-8

# Determine the max limit to make both axes equal and square
max_limit_new <- max(c(max(df_combined_new_unique$TrueValue), max(df_combined_new_unique$MeanDivergence)))

# Create the square plot for the new data without the legend
ggplot(df_combined_new_unique, aes(x = TrueValue, y = MeanDivergence, color = PopPair)) +
  geom_point(alpha = 0.3, size = 3) +  # Set alpha transparency and point size
  geom_smooth(method = "lm", se = FALSE) +  # Add trend lines without confidence shading
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +  # Add 1:1 diagonal line
  labs(x = "True Divergence", y = "Estimated Mean Divergence (x 10^-8)", 
       title = "True vs Estimated Divergence (Config 3: 80000_10000)") +
  theme_bw() +
  coord_fixed(ratio = 1) +  # Ensure the aspect ratio is 1:1
  xlim(0, max_limit_new) +  # Set both x and y axis limits to the same max value
  ylim(0, max_limit_new) +  # Set both x and y axis limits to the same max value
  theme(legend.position = "none")  # Remove the legend












library(tidyr)
library(dplyr)
library(ggplot2)

# Process the summary file and create the plot
file_path <- "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_3_160000_10000_results/summary_output.txt"

# Load the summary file
df_new <- read.table(file_path, header = TRUE, sep = "\t")

# Gather the Mean divergence values into long format
df_new_long_mean <- df_new %>%
  gather(key = "PopPair", value = "MeanDivergence", 
         Mean_Divergence_PopA_PopB:Mean_Divergence_PopK_PopL) %>%
  select(Run, PopPair, MeanDivergence)

# Gather the True divergence values into long format
df_new_long_true <- df_new %>%
  gather(key = "PopPair_True", value = "TrueValue", 
         True_Divergence_PopA_PopB:True_Divergence_PopK_PopL) %>%
  mutate(PopPair = gsub("True_Divergence_", "Mean_Divergence_", PopPair_True)) %>%
  select(Run, PopPair, TrueValue)

# Merge both datasets based on matching population pairs and runs
df_combined_new <- inner_join(df_new_long_mean, df_new_long_true, by = c("Run", "PopPair"))

# Remove duplicates if necessary
df_combined_new_unique <- df_combined_new[!duplicated(df_combined_new[, c("TrueValue", "MeanDivergence")]), ]

# Divide the mean estimated values by 10e-8
df_combined_new_unique$MeanDivergence <- df_combined_new_unique$MeanDivergence / 10e-8

# Determine the max limit to make both axes equal and square
max_limit_new <- max(c(max(df_combined_new_unique$TrueValue), max(df_combined_new_unique$MeanDivergence)))

# Create the square plot for the new data without the legend
ggplot(df_combined_new_unique, aes(x = TrueValue, y = MeanDivergence, color = PopPair)) +
  geom_point(alpha = 0.3, size = 3) +  # Set alpha transparency and point size
  geom_smooth(method = "lm", se = FALSE) +  # Add trend lines without confidence shading
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +  # Add 1:1 diagonal line
  labs(x = "True Divergence", y = "Estimated Mean Divergence (x 10^-8)", 
       title = "True vs Estimated Divergence (Config 3: 160000_10000)") +
  theme_bw() +
  coord_fixed(ratio = 1) +  # Ensure the aspect ratio is 1:1
  xlim(0, max_limit_new) +  # Set both x and y axis limits to the same max value
  ylim(0, max_limit_new) +  # Set both x and y axis limits to the same max value
  theme(legend.position = "none")  # Remove the legend

















library(tidyr)
library(dplyr)
library(ggplot2)

# Process the summary file and create the plot
file_path <- "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_3_10000_10000_results/summary_output.txt"

# Load the summary file
df_new <- read.table(file_path, header = TRUE, sep = "\t")

# Gather the Mean divergence values into long format
df_new_long_mean <- df_new %>%
  gather(key = "PopPair", value = "MeanDivergence", 
         Mean_Divergence_PopA_PopB:Mean_Divergence_PopK_PopL) %>%
  select(Run, PopPair, MeanDivergence)

# Gather the True divergence values into long format
df_new_long_true <- df_new %>%
  gather(key = "PopPair_True", value = "TrueValue", 
         True_Divergence_PopA_PopB:True_Divergence_PopK_PopL) %>%
  mutate(PopPair = gsub("True_Divergence_", "Mean_Divergence_", PopPair_True)) %>%
  select(Run, PopPair, TrueValue)

# Merge both datasets based on matching population pairs and runs
df_combined_new <- inner_join(df_new_long_mean, df_new_long_true, by = c("Run", "PopPair"))

# Remove duplicates if necessary
df_combined_new_unique <- df_combined_new[!duplicated(df_combined_new[, c("TrueValue", "MeanDivergence")]), ]

# Divide the mean estimated values by 10e-8
df_combined_new_unique$MeanDivergence <- df_combined_new_unique$MeanDivergence / 10e-8

# Determine the max limit to make both axes equal and square
max_limit_new <- max(c(max(df_combined_new_unique$TrueValue), max(df_combined_new_unique$MeanDivergence)))

# Create the plot with y-axis limited to 300,000
ggplot(df_combined_new_unique, aes(x = TrueValue, y = MeanDivergence, color = PopPair)) +
  geom_point(alpha = 0.3, size = 3) +  # Set alpha transparency and point size
  geom_smooth(method = "lm", se = FALSE) +  # Add trend lines without confidence shading
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +  # Add 1:1 diagonal line
  labs(x = "True Divergence", y = "Estimated Mean Divergence (x 10^-8)", 
       title = "True vs Estimated Divergence (Config 3: 10000_10000)") +
  theme_bw() +
  coord_fixed(ratio = 1) +  # Ensure the aspect ratio is 1:1
  xlim(0, max_limit_new) +  # Set x-axis limits based on data
  ylim(0, 300000) +  # Set y-axis limit to 300,000
  theme(legend.position = "none")  # Remove the legend










library(tidyr)
library(dplyr)
library(ggplot2)

# Process the summary file and create the plot
file_path <- "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_3_10000_10000_results/summary_output.txt"

# Load the summary file
df_new <- read.table(file_path, header = TRUE, sep = "\t")

# Gather the Mean divergence values into long format
df_new_long_mean <- df_new %>%
  gather(key = "PopPair", value = "MeanDivergence", 
         Mean_Divergence_PopA_PopB:Mean_Divergence_PopK_PopL) %>%
  select(Run, PopPair, MeanDivergence)

# Gather the True divergence values into long format
df_new_long_true <- df_new %>%
  gather(key = "PopPair_True", value = "TrueValue", 
         True_Divergence_PopA_PopB:True_Divergence_PopK_PopL) %>%
  mutate(PopPair = gsub("True_Divergence_", "Mean_Divergence_", PopPair_True)) %>%
  select(Run, PopPair, TrueValue)

# Merge both datasets based on matching population pairs and runs
df_combined_new <- inner_join(df_new_long_mean, df_new_long_true, by = c("Run", "PopPair"))

# Remove duplicates if necessary
df_combined_new_unique <- df_combined_new[!duplicated(df_combined_new[, c("TrueValue", "MeanDivergence")]), ]

# Divide the mean estimated values by 10e-8
df_combined_new_unique$MeanDivergence <- df_combined_new_unique$MeanDivergence / 10e-8

# Create the plot with both axes limited to 300,000
ggplot(df_combined_new_unique, aes(x = TrueValue, y = MeanDivergence, color = PopPair)) +
  geom_point(alpha = 0.3, size = 3) +  # Set alpha transparency and point size
  geom_smooth(method = "lm", se = FALSE) +  # Add trend lines without confidence shading
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +  # Add 1:1 diagonal line
  labs(x = "True Divergence", y = "Estimated Mean Divergence (x 10^-8)", 
       title = "True vs Estimated Divergence (Config 3: 10000_10000)") +
  theme_bw() +
  coord_fixed(ratio = 1) +  # Ensure the aspect ratio is 1:1
  xlim(0, 300000) +  # Set x-axis limit to 300,000
  ylim(0, 300000) +  # Set y-axis limit to 300,000
  theme(legend.position = "none")  # Remove the legend






library(tidyr)
library(dplyr)
library(ggplot2)

# Process the summary file and create the plot
file_path <- "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_3_20000_10000_results/summary_output.txt"

# Load the summary file
df_new <- read.table(file_path, header = TRUE, sep = "\t")

# Gather the Mean divergence values into long format
df_new_long_mean <- df_new %>%
  gather(key = "PopPair", value = "MeanDivergence", 
         Mean_Divergence_PopA_PopB:Mean_Divergence_PopK_PopL) %>%
  select(Run, PopPair, MeanDivergence)

# Gather the True divergence values into long format
df_new_long_true <- df_new %>%
  gather(key = "PopPair_True", value = "TrueValue", 
         True_Divergence_PopA_PopB:True_Divergence_PopK_PopL) %>%
  mutate(PopPair = gsub("True_Divergence_", "Mean_Divergence_", PopPair_True)) %>%
  select(Run, PopPair, TrueValue)

# Merge both datasets based on matching population pairs and runs
df_combined_new <- inner_join(df_new_long_mean, df_new_long_true, by = c("Run", "PopPair"))

# Remove duplicates if necessary
df_combined_new_unique <- df_combined_new[!duplicated(df_combined_new[, c("TrueValue", "MeanDivergence")]), ]

# Divide the mean estimated values by 10e-8
df_combined_new_unique$MeanDivergence <- df_combined_new_unique$MeanDivergence / 10e-8

# Create the plot with both axes limited to 300,000
ggplot(df_combined_new_unique, aes(x = TrueValue, y = MeanDivergence, color = PopPair)) +
  geom_point(alpha = 0.3, size = 3) +  # Set alpha transparency and point size
  geom_smooth(method = "lm", se = FALSE) +  # Add trend lines without confidence shading
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +  # Add 1:1 diagonal line
  labs(x = "True Divergence", y = "Estimated Mean Divergence (x 10^-8)", 
       title = "True vs Estimated Divergence (Config 3: 20000_10000)") +
  theme_bw() +
  coord_fixed(ratio = 1) +  # Ensure the aspect ratio is 1:1
  xlim(0, 300000) +  # Set x-axis limit to 300,000
  ylim(0, 300000) +  # Set y-axis limit to 300,000
  theme(legend.position = "none")  # Remove the legend











library(tidyr)
library(dplyr)
library(ggplot2)

# Process the summary file and create the plot
file_path <- "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_3_80000_10000_results/summary_output.txt"

# Load the summary file
df_new <- read.table(file_path, header = TRUE, sep = "\t")

# Gather the Mean divergence values into long format
df_new_long_mean <- df_new %>%
  gather(key = "PopPair", value = "MeanDivergence", 
         Mean_Divergence_PopA_PopB:Mean_Divergence_PopK_PopL) %>%
  select(Run, PopPair, MeanDivergence)

# Gather the True divergence values into long format
df_new_long_true <- df_new %>%
  gather(key = "PopPair_True", value = "TrueValue", 
         True_Divergence_PopA_PopB:True_Divergence_PopK_PopL) %>%
  mutate(PopPair = gsub("True_Divergence_", "Mean_Divergence_", PopPair_True)) %>%
  select(Run, PopPair, TrueValue)

# Merge both datasets based on matching population pairs and runs
df_combined_new <- inner_join(df_new_long_mean, df_new_long_true, by = c("Run", "PopPair"))

# Remove duplicates if necessary
df_combined_new_unique <- df_combined_new[!duplicated(df_combined_new[, c("TrueValue", "MeanDivergence")]), ]

# Divide the mean estimated values by 10e-8
df_combined_new_unique$MeanDivergence <- df_combined_new_unique$MeanDivergence / 10e-8

# Create the plot with both axes limited to 300,000
ggplot(df_combined_new_unique, aes(x = TrueValue, y = MeanDivergence, color = PopPair)) +
  geom_point(alpha = 0.3, size = 3) +  # Set alpha transparency and point size
  geom_smooth(method = "lm", se = FALSE) +  # Add trend lines without confidence shading
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +  # Add 1:1 diagonal line
  labs(x = "True Divergence", y = "Estimated Mean Divergence (x 10^-8)", 
       title = "True vs Estimated Divergence (Config 3: 80000_10000)") +
  theme_bw() +
  coord_fixed(ratio = 1) +  # Ensure the aspect ratio is 1:1
  xlim(0, 300000) +  # Set x-axis limit to 300,000
  ylim(0, 300000) +  # Set y-axis limit to 300,000
  theme(legend.position = "none")  # Remove the legend









library(tidyr)
library(dplyr)
library(ggplot2)

# Process the summary file and create the plot
file_path <- "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_3_40000_10000_results/summary_output.txt"

# Load the summary file
df_new <- read.table(file_path, header = TRUE, sep = "\t")

# Gather the Mean divergence values into long format
df_new_long_mean <- df_new %>%
  gather(key = "PopPair", value = "MeanDivergence", 
         Mean_Divergence_PopA_PopB:Mean_Divergence_PopK_PopL) %>%
  select(Run, PopPair, MeanDivergence)

# Gather the True divergence values into long format
df_new_long_true <- df_new %>%
  gather(key = "PopPair_True", value = "TrueValue", 
         True_Divergence_PopA_PopB:True_Divergence_PopK_PopL) %>%
  mutate(PopPair = gsub("True_Divergence_", "Mean_Divergence_", PopPair_True)) %>%
  select(Run, PopPair, TrueValue)

# Merge both datasets based on matching population pairs and runs
df_combined_new <- inner_join(df_new_long_mean, df_new_long_true, by = c("Run", "PopPair"))

# Remove duplicates if necessary
df_combined_new_unique <- df_combined_new[!duplicated(df_combined_new[, c("TrueValue", "MeanDivergence")]), ]

# Divide the mean estimated values by 10e-8
df_combined_new_unique$MeanDivergence <- df_combined_new_unique$MeanDivergence / 10e-8

# Create the plot with both axes limited to 300,000
ggplot(df_combined_new_unique, aes(x = TrueValue, y = MeanDivergence, color = PopPair)) +
  geom_point(alpha = 0.3, size = 3) +  # Set alpha transparency and point size
  geom_smooth(method = "lm", se = FALSE) +  # Add trend lines without confidence shading
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +  # Add 1:1 diagonal line
  labs(x = "True Divergence", y = "Estimated Mean Divergence (x 10^-8)", 
       title = "True vs Estimated Divergence (Config 3: 40000_10000)") +
  theme_bw() +
  coord_fixed(ratio = 1) +  # Ensure the aspect ratio is 1:1
  xlim(0, 300000) +  # Set x-axis limit to 300,000
  ylim(0, 300000) +  # Set y-axis limit to 300,000
  theme(legend.position = "none")  # Remove the legend











library(tidyr)
library(dplyr)
library(ggplot2)

# Process the summary file and create the plot
file_path <- "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_3_160000_10000_results/summary_output.txt"

# Load the summary file
df_new <- read.table(file_path, header = TRUE, sep = "\t")

# Gather the Mean divergence values into long format
df_new_long_mean <- df_new %>%
  gather(key = "PopPair", value = "MeanDivergence", 
         Mean_Divergence_PopA_PopB:Mean_Divergence_PopK_PopL) %>%
  select(Run, PopPair, MeanDivergence)

# Gather the True divergence values into long format
df_new_long_true <- df_new %>%
  gather(key = "PopPair_True", value = "TrueValue", 
         True_Divergence_PopA_PopB:True_Divergence_PopK_PopL) %>%
  mutate(PopPair = gsub("True_Divergence_", "Mean_Divergence_", PopPair_True)) %>%
  select(Run, PopPair, TrueValue)

# Merge both datasets based on matching population pairs and runs
df_combined_new <- inner_join(df_new_long_mean, df_new_long_true, by = c("Run", "PopPair"))

# Remove duplicates if necessary
df_combined_new_unique <- df_combined_new[!duplicated(df_combined_new[, c("TrueValue", "MeanDivergence")]), ]

# Divide the mean estimated values by 10e-8
df_combined_new_unique$MeanDivergence <- df_combined_new_unique$MeanDivergence / 10e-8

# Create the plot with both axes limited to 300,000
ggplot(df_combined_new_unique, aes(x = TrueValue, y = MeanDivergence, color = PopPair)) +
  geom_point(alpha = 0.3, size = 3) +  # Set alpha transparency and point size
  geom_smooth(method = "lm", se = FALSE) +  # Add trend lines without confidence shading
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +  # Add 1:1 diagonal line
  labs(x = "True Divergence", y = "Estimated Mean Divergence (x 10^-8)", 
       title = "True vs Estimated Divergence (Config 3: 160000_10000)") +
  theme_bw() +
  coord_fixed(ratio = 1) +  # Ensure the aspect ratio is 1:1
  xlim(0, 340000) +  # Set x-axis limit to 300,000
  ylim(0, 340000) +  # Set y-axis limit to 300,000
  theme(legend.position = "none")  # Remove the legend











library(tidyr)
library(dplyr)
library(ggplot2)

# Process the summary file and create the plot
file_path <- "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_6_10000_10000_results/summary_output.txt"

# Load the summary file
df_new <- read.table(file_path, header = TRUE, sep = "\t")

# Gather the Mean divergence values into long format
df_new_long_mean <- df_new %>%
  gather(key = "PopPair", value = "MeanDivergence", 
         Mean_Divergence_PopA_PopB:Mean_Divergence_PopK_PopL) %>%
  select(Run, PopPair, MeanDivergence)

# Gather the True divergence values into long format
df_new_long_true <- df_new %>%
  gather(key = "PopPair_True", value = "TrueValue", 
         True_Divergence_PopA_PopB:True_Divergence_PopK_PopL) %>%
  mutate(PopPair = gsub("True_Divergence_", "Mean_Divergence_", PopPair_True)) %>%
  select(Run, PopPair, TrueValue)

# Merge both datasets based on matching population pairs and runs
df_combined_new <- inner_join(df_new_long_mean, df_new_long_true, by = c("Run", "PopPair"))

# Remove duplicates if necessary
df_combined_new_unique <- df_combined_new[!duplicated(df_combined_new[, c("TrueValue", "MeanDivergence")]), ]

# Divide the mean estimated values by 10e-8
df_combined_new_unique$MeanDivergence <- df_combined_new_unique$MeanDivergence / 10e-8

# Create the plot with both axes limited to 300,000
ggplot(df_combined_new_unique, aes(x = TrueValue, y = MeanDivergence, color = PopPair)) +
  geom_point(alpha = 0.3, size = 3) +  # Set alpha transparency and point size
  geom_smooth(method = "lm", se = FALSE) +  # Add trend lines without confidence shading
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +  # Add 1:1 diagonal line
  labs(x = "True Divergence", y = "Estimated Mean Divergence (x 10^-8)", 
       title = "True vs Estimated Divergence (Config 6: 10000_10000)") +
  theme_bw() +
  coord_fixed(ratio = 1) +  # Ensure the aspect ratio is 1:1
  xlim(0, 900000) +  # Set x-axis limit to 300,000
  ylim(0, 900000) +  # Set y-axis limit to 300,000
  theme(legend.position = "none")  # Remove the legend










library(tidyr)
library(dplyr)
library(ggplot2)

# Process the summary file and create the plot
file_path <- "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_6_20000_10000_results/summary_output.txt"

# Load the summary file
df_new <- read.table(file_path, header = TRUE, sep = "\t")

# Gather the Mean divergence values into long format
df_new_long_mean <- df_new %>%
  gather(key = "PopPair", value = "MeanDivergence", 
         Mean_Divergence_PopA_PopB:Mean_Divergence_PopK_PopL) %>%
  select(Run, PopPair, MeanDivergence)

# Gather the True divergence values into long format
df_new_long_true <- df_new %>%
  gather(key = "PopPair_True", value = "TrueValue", 
         True_Divergence_PopA_PopB:True_Divergence_PopK_PopL) %>%
  mutate(PopPair = gsub("True_Divergence_", "Mean_Divergence_", PopPair_True)) %>%
  select(Run, PopPair, TrueValue)

# Merge both datasets based on matching population pairs and runs
df_combined_new <- inner_join(df_new_long_mean, df_new_long_true, by = c("Run", "PopPair"))

# Remove duplicates if necessary
df_combined_new_unique <- df_combined_new[!duplicated(df_combined_new[, c("TrueValue", "MeanDivergence")]), ]

# Divide the mean estimated values by 10e-8
df_combined_new_unique$MeanDivergence <- df_combined_new_unique$MeanDivergence / 10e-8

# Create the plot with both axes limited to 300,000
ggplot(df_combined_new_unique, aes(x = TrueValue, y = MeanDivergence, color = PopPair)) +
  geom_point(alpha = 0.3, size = 3) +  # Set alpha transparency and point size
  geom_smooth(method = "lm", se = FALSE) +  # Add trend lines without confidence shading
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +  # Add 1:1 diagonal line
  labs(x = "True Divergence", y = "Estimated Mean Divergence (x 10^-8)", 
       title = "True vs Estimated Divergence (Config 6: 20000_10000)") +
  theme_bw() +
  coord_fixed(ratio = 1) +  # Ensure the aspect ratio is 1:1
  xlim(0, 900000) +  # Set x-axis limit to 300,000
  ylim(0, 900000) +  # Set y-axis limit to 300,000
  theme(legend.position = "none")  # Remove the legend







library(tidyr)
library(dplyr)
library(ggplot2)

# Process the summary file and create the plot
file_path <- "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_6_40000_10000_results/summary_output.txt"

# Load the summary file
df_new <- read.table(file_path, header = TRUE, sep = "\t")

# Gather the Mean divergence values into long format
df_new_long_mean <- df_new %>%
  gather(key = "PopPair", value = "MeanDivergence", 
         Mean_Divergence_PopA_PopB:Mean_Divergence_PopK_PopL) %>%
  select(Run, PopPair, MeanDivergence)

# Gather the True divergence values into long format
df_new_long_true <- df_new %>%
  gather(key = "PopPair_True", value = "TrueValue", 
         True_Divergence_PopA_PopB:True_Divergence_PopK_PopL) %>%
  mutate(PopPair = gsub("True_Divergence_", "Mean_Divergence_", PopPair_True)) %>%
  select(Run, PopPair, TrueValue)

# Merge both datasets based on matching population pairs and runs
df_combined_new <- inner_join(df_new_long_mean, df_new_long_true, by = c("Run", "PopPair"))

# Remove duplicates if necessary
df_combined_new_unique <- df_combined_new[!duplicated(df_combined_new[, c("TrueValue", "MeanDivergence")]), ]

# Divide the mean estimated values by 10e-8
df_combined_new_unique$MeanDivergence <- df_combined_new_unique$MeanDivergence / 10e-8

# Create the plot with both axes limited to 300,000
ggplot(df_combined_new_unique, aes(x = TrueValue, y = MeanDivergence, color = PopPair)) +
  geom_point(alpha = 0.3, size = 3) +  # Set alpha transparency and point size
  geom_smooth(method = "lm", se = FALSE) +  # Add trend lines without confidence shading
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +  # Add 1:1 diagonal line
  labs(x = "True Divergence", y = "Estimated Mean Divergence (x 10^-8)", 
       title = "True vs Estimated Divergence (Config 6: 40000_10000)") +
  theme_bw() +
  coord_fixed(ratio = 1) +  # Ensure the aspect ratio is 1:1
  xlim(0, 900000) +  # Set x-axis limit to 300,000
  ylim(0, 900000) +  # Set y-axis limit to 300,000
  theme(legend.position = "none")  # Remove the legend










library(tidyr)
library(dplyr)
library(ggplot2)

# Process the summary file and create the plot
file_path <- "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_1_0_10000_results/summary_output.txt"

# Load the summary file
df_new <- read.table(file_path, header = TRUE, sep = "\t")

# Gather the Mean divergence values into long format
df_new_long_mean <- df_new %>%
  gather(key = "PopPair", value = "MeanDivergence", 
         Mean_Divergence_PopA_PopB:Mean_Divergence_PopK_PopL) %>%
  select(Run, PopPair, MeanDivergence)

# Gather the True divergence values into long format
df_new_long_true <- df_new %>%
  gather(key = "PopPair_True", value = "TrueValue", 
         True_Divergence_PopA_PopB:True_Divergence_PopK_PopL) %>%
  mutate(PopPair = gsub("True_Divergence_", "Mean_Divergence_", PopPair_True)) %>%
  select(Run, PopPair, TrueValue)

# Merge both datasets based on matching population pairs and runs
df_combined_new <- inner_join(df_new_long_mean, df_new_long_true, by = c("Run", "PopPair"))

# Remove duplicates if necessary
df_combined_new_unique <- df_combined_new[!duplicated(df_combined_new[, c("TrueValue", "MeanDivergence")]), ]

# Divide the mean estimated values by 10e-8
df_combined_new_unique$MeanDivergence <- df_combined_new_unique$MeanDivergence / 10e-8

# Create the plot with both axes limited to 300,000
ggplot(df_combined_new_unique, aes(x = TrueValue, y = MeanDivergence, color = PopPair)) +
  geom_point(alpha = 0.3, size = 3) +  # Set alpha transparency and point size
  geom_smooth(method = "lm", se = FALSE) +  # Add trend lines without confidence shading
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +  # Add 1:1 diagonal line
  labs(x = "True Divergence", y = "Estimated Mean Divergence (x 10^-8)", 
       title = "True vs Estimated Divergence (Config 6: 80000_10000)") +
  theme_bw() +
  coord_fixed(ratio = 1) +  # Ensure the aspect ratio is 1:1
  xlim(0, 20000) +  # Set x-axis limit to 300,000
  ylim(0, 20000) +  # Set y-axis limit to 300,000
  theme(legend.position = "none")  # Remove the legend










library(tidyr)
library(dplyr)
library(ggplot2)

# Process the summary file and create the plot
file_path <- "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_6_160000_10000_results/summary_output.txt"

# Load the summary file
df_new <- read.table(file_path, header = TRUE, sep = "\t")

# Gather the Mean divergence values into long format
df_new_long_mean <- df_new %>%
  gather(key = "PopPair", value = "MeanDivergence", 
         Mean_Divergence_PopA_PopB:Mean_Divergence_PopK_PopL) %>%
  select(Run, PopPair, MeanDivergence)

# Gather the True divergence values into long format
df_new_long_true <- df_new %>%
  gather(key = "PopPair_True", value = "TrueValue", 
         True_Divergence_PopA_PopB:True_Divergence_PopK_PopL) %>%
  mutate(PopPair = gsub("True_Divergence_", "Mean_Divergence_", PopPair_True)) %>%
  select(Run, PopPair, TrueValue)

# Merge both datasets based on matching population pairs and runs
df_combined_new <- inner_join(df_new_long_mean, df_new_long_true, by = c("Run", "PopPair"))

# Remove duplicates if necessary
df_combined_new_unique <- df_combined_new[!duplicated(df_combined_new[, c("TrueValue", "MeanDivergence")]), ]

# Divide the mean estimated values by 10e-8
df_combined_new_unique$MeanDivergence <- df_combined_new_unique$MeanDivergence / 10e-8

# Create the plot with both axes limited to 300,000
ggplot(df_combined_new_unique, aes(x = TrueValue, y = MeanDivergence, color = PopPair)) +
  geom_point(alpha = 0.3, size = 3) +  # Set alpha transparency and point size
  geom_smooth(method = "lm", se = FALSE) +  # Add trend lines without confidence shading
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +  # Add 1:1 diagonal line
  labs(x = "True Divergence", y = "Estimated Mean Divergence (x 10^-8)", 
       title = "True vs Estimated Divergence (Config 6: 160000_10000)") +
  theme_bw() +
  coord_fixed(ratio = 1) +  # Ensure the aspect ratio is 1:1
  xlim(0, 900000) +  # Set x-axis limit to 300,000
  ylim(0, 900000) +  # Set y-axis limit to 300,000
  theme(legend.position = "none")  # Remove the legend












library(tidyr)
library(dplyr)
library(ggplot2)

# Load the data
file_path <- "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_3_10000_10000_results/summary_output.txt"
df <- read.table(file_path, header = TRUE, sep = "\t")

# Gather the probability columns into long format
df_long <- df %>%
  gather(key = "EventType", value = "Probability", 
         Prob_1_Event:Prob_6_Events)

# Create a plot showing the relationship between the true number of events and the estimated probabilities
ggplot(df_long, aes(x = True_Number_Events, y = Probability, color = EventType)) +
  geom_point(alpha = 0.6, size = 3) +
  geom_jitter(width = 0.1, height = 0) +  # Jitter to spread out points
  labs(x = "True Number of Events", y = "Estimated Probability",
       title = "Estimated Probability of Shared Events vs True Number of Events") +
  theme_minimal() +
  theme(legend.position = "right")  # Adjust the legend position as needed






library(tidyr)
library(dplyr)

# Load the data
file_path <- "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_3_10000_10000_results/summary_output.txt"
df <- read.table(file_path, header = TRUE, sep = "\t")

# Create a function to calculate RMSE
calculate_rmse <- function(predicted, actual) {
  sqrt(mean((predicted - actual)^2))
}

# Calculate RMSE for each event type
event_types <- c("Prob_1_Event", "Prob_2_Events", "Prob_3_Events", "Prob_4_Events", "Prob_5_Events", "Prob_6_Events")
rmse_results <- sapply(event_types, function(event) {
  calculate_rmse(df[[event]], df$True_Number_Events)
})

# Create a data frame to display the RMSE results
rmse_df <- data.frame(EventType = event_types, RMSE = rmse_results)

# Print the RMSE results
print(rmse_df)






library(tidyr)
library(dplyr)
library(ggplot2)

# Load the data
file_path <- "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_3_10000_10000_results/summary_output.txt"
df <- read.table(file_path, header = TRUE, sep = "\t")

# Create a function to calculate RMSE
calculate_rmse <- function(predicted, actual) {
  sqrt(mean((predicted - actual)^2))
}

# Calculate RMSE for each event type
event_types <- c("Prob_1_Event", "Prob_2_Events", "Prob_3_Events", "Prob_4_Events", "Prob_5_Events", "Prob_6_Events")
rmse_results <- sapply(event_types, function(event) {
  calculate_rmse(df[[event]], df$True_Number_Events)
})

# Create a data frame to display the RMSE results
rmse_df <- data.frame(EventType = event_types, RMSE = rmse_results)

# Plot the RMSE results
ggplot(rmse_df, aes(x = EventType, y = RMSE)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "RMSE for Estimated Probabilities of Shared Events", 
       x = "Event Type", 
       y = "RMSE") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))








library(tidyr)
library(dplyr)
library(ggplot2)

# Load the data
file_path <- "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_3_10000_10000_results/summary_output.txt"
df <- read.table(file_path, header = TRUE, sep = "\t")

# Create a function to calculate RMSE
calculate_rmse <- function(predicted, actual) {
  sqrt(mean((predicted - actual)^2))
}

# Calculate RMSE for each event type
event_types <- c("Prob_1_Event", "Prob_2_Events", "Prob_3_Events", "Prob_4_Events", "Prob_5_Events", "Prob_6_Events")
rmse_results <- sapply(event_types, function(event) {
  calculate_rmse(df[[event]], df$True_Number_Events)
})

# Create a data frame to display the RMSE results
rmse_df <- data.frame(EventType = event_types, RMSE = rmse_results)

# Plot the RMSE results with thinner bars
ggplot(rmse_df, aes(x = EventType, y = RMSE)) +
  geom_bar(stat = "identity", fill = "skyblue", width = 0.9) +  # Adjust the width parameter for thinner bars
  labs(title = "RMSE for Estimated Probabilities of Shared Events", 
       x = "Event Type", 
       y = "RMSE") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))









library(tidyr)
library(dplyr)
library(ggplot2)

# Load the data
file_path <- "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_3_10000_10000_results/summary_output.txt"
df <- read.table(file_path, header = TRUE, sep = "\t")

# Create a function to calculate RMSE per run
calculate_rmse_per_run <- function(predicted, actual) {
  sqrt((predicted - actual)^2)
}

# Reshape the data to calculate RMSE per run and per event type
event_types <- c("Prob_1_Event", "Prob_2_Events", "Prob_3_Events", "Prob_4_Events", "Prob_5_Events", "Prob_6_Events")

# Create a long format data frame with RMSE values per run
rmse_long_df <- df %>%
  gather(key = "EventType", value = "Probability", all_of(event_types)) %>%
  mutate(RMSE = calculate_rmse_per_run(Probability, True_Number_Events))

# Plot the RMSE results per run using a boxplot to show the distribution
ggplot(rmse_long_df, aes(x = EventType, y = RMSE)) +
  geom_boxplot(fill = "skyblue") +
  labs(title = "RMSE for Estimated Probabilities of Shared Events Per Run", 
       x = "Event Type", 
       y = "RMSE (Per Run)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))









library(tidyr)
library(dplyr)
library(ggplot2)

# Load the data
file_path <- "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_3_20000_10000_results/summary_output.txt"
df <- read.table(file_path, header = TRUE, sep = "\t")

# Create a function to calculate RMSE per run
calculate_rmse_per_run <- function(predicted, actual) {
  sqrt((predicted - actual)^2)
}

# Reshape the data to calculate RMSE per run and per event type
event_types <- c("Prob_1_Event", "Prob_2_Events", "Prob_3_Events", "Prob_4_Events", "Prob_5_Events", "Prob_6_Events")

# Create a long format data frame with RMSE values per run
rmse_long_df <- df %>%
  gather(key = "EventType", value = "Probability", all_of(event_types)) %>%
  mutate(RMSE = calculate_rmse_per_run(Probability, True_Number_Events))

# Plot the RMSE results per run using a boxplot to show the distribution
ggplot(rmse_long_df, aes(x = EventType, y = RMSE)) +
  geom_boxplot(fill = "skyblue") +
  labs(title = "RMSE for Estimated Probabilities of Shared Events Per Run (Config 3: 20000_10000)", 
       x = "Event Type", 
       y = "RMSE (Per Run)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))







library(tidyr)
library(dplyr)
library(ggplot2)

# Load the data
file_path <- "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_3_40000_10000_results/summary_output.txt"
df <- read.table(file_path, header = TRUE, sep = "\t")

# Create a function to calculate RMSE per run
calculate_rmse_per_run <- function(predicted, actual) {
  sqrt((predicted - actual)^2)
}

# Reshape the data to calculate RMSE per run and per event type
event_types <- c("Prob_1_Event", "Prob_2_Events", "Prob_3_Events", "Prob_4_Events", "Prob_5_Events", "Prob_6_Events")

# Create a long format data frame with RMSE values per run
rmse_long_df <- df %>%
  gather(key = "EventType", value = "Probability", all_of(event_types)) %>%
  mutate(RMSE = calculate_rmse_per_run(Probability, True_Number_Events))

# Plot the RMSE results per run using a boxplot to show the distribution
ggplot(rmse_long_df, aes(x = EventType, y = RMSE)) +
  geom_boxplot(fill = "skyblue") +
  labs(title = "RMSE for Estimated Probabilities of Shared Events Per Run (Config 3: 40000_10000)", 
       x = "Event Type", 
       y = "RMSE (Per Run)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))















library(tidyr)
library(dplyr)
library(ggplot2)

# Load the data
file_path <- "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_3_80000_10000_results/summary_output.txt"
df <- read.table(file_path, header = TRUE, sep = "\t")

# Create a function to calculate RMSE per run
calculate_rmse_per_run <- function(predicted, actual) {
  sqrt((predicted - actual)^2)
}

# Reshape the data to calculate RMSE per run and per event type
event_types <- c("Prob_1_Event", "Prob_2_Events", "Prob_3_Events", "Prob_4_Events", "Prob_5_Events", "Prob_6_Events")

# Create a long format data frame with RMSE values per run
rmse_long_df <- df %>%
  gather(key = "EventType", value = "Probability", all_of(event_types)) %>%
  mutate(RMSE = calculate_rmse_per_run(Probability, True_Number_Events))

# Plot the RMSE results per run using a boxplot to show the distribution
ggplot(rmse_long_df, aes(x = EventType, y = RMSE)) +
  geom_boxplot(fill = "skyblue") +
  labs(title = "RMSE for Estimated Probabilities of Shared Events Per Run (Config 3: 80000_10000)", 
       x = "Event Type", 
       y = "RMSE (Per Run)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))







library(tidyr)
library(dplyr)
library(ggplot2)

# Load the data
file_path <- "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_3_160000_10000_results/summary_output.txt"
df <- read.table(file_path, header = TRUE, sep = "\t")

# Create a function to calculate RMSE per run
calculate_rmse_per_run <- function(predicted, actual) {
  sqrt((predicted - actual)^2)
}

# Reshape the data to calculate RMSE per run and per event type
event_types <- c("Prob_1_Event", "Prob_2_Events", "Prob_3_Events", "Prob_4_Events", "Prob_5_Events", "Prob_6_Events")

# Create a long format data frame with RMSE values per run
rmse_long_df <- df %>%
  gather(key = "EventType", value = "Probability", all_of(event_types)) %>%
  mutate(RMSE = calculate_rmse_per_run(Probability, True_Number_Events))

# Plot the RMSE results per run using a boxplot to show the distribution
ggplot(rmse_long_df, aes(x = EventType, y = RMSE)) +
  geom_boxplot(fill = "skyblue") +
  labs(title = "RMSE for Estimated Probabilities of Shared Events Per Run (Config 3: 160000_10000)", 
       x = "Event Type", 
       y = "RMSE (Per Run)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





library(tidyr)
library(dplyr)
library(ggplot2)

# Load the data
file_path <- "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_6_10000_10000_results/summary_output.txt"
df <- read.table(file_path, header = TRUE, sep = "\t")

# Create a function to calculate RMSE per run
calculate_rmse_per_run <- function(predicted, actual) {
  sqrt((predicted - actual)^2)
}

# Reshape the data to calculate RMSE per run and per event type
event_types <- c("Prob_1_Event", "Prob_2_Events", "Prob_3_Events", "Prob_4_Events", "Prob_5_Events", "Prob_6_Events")

# Create a long format data frame with RMSE values per run
rmse_long_df <- df %>%
  gather(key = "EventType", value = "Probability", all_of(event_types)) %>%
  mutate(RMSE = calculate_rmse_per_run(Probability, True_Number_Events))

# Plot the RMSE results per run using a boxplot to show the distribution
ggplot(rmse_long_df, aes(x = EventType, y = RMSE, fill = EventType == "Prob_6_Events")) +
  geom_boxplot() +
  scale_fill_manual(values = c("skyblue", "red")) +  # Set colors: skyblue for others, red for correct
  labs(title = "RMSE for Estimated Probabilities of Shared Events Per Run (Config 6: 10000_10000)", 
       x = "Event Type", 
       y = "RMSE (Per Run)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")  # Remove the legend for clarity







library(tidyr)
library(dplyr)
library(ggplot2)

# Load the data
file_path <- "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_6_20000_10000_results/summary_output.txt"
df <- read.table(file_path, header = TRUE, sep = "\t")

# Create a function to calculate RMSE per run
calculate_rmse_per_run <- function(predicted, actual) {
  sqrt((predicted - actual)^2)
}

# Reshape the data to calculate RMSE per run and per event type
event_types <- c("Prob_1_Event", "Prob_2_Events", "Prob_3_Events", "Prob_4_Events", "Prob_5_Events", "Prob_6_Events")

# Create a long format data frame with RMSE values per run
rmse_long_df <- df %>%
  gather(key = "EventType", value = "Probability", all_of(event_types)) %>%
  mutate(RMSE = calculate_rmse_per_run(Probability, True_Number_Events))

# Plot the RMSE results per run using a boxplot to show the distribution
ggplot(rmse_long_df, aes(x = EventType, y = RMSE, fill = EventType == "Prob_6_Events")) +
  geom_boxplot() +
  scale_fill_manual(values = c("skyblue", "red")) +  # Set colors: skyblue for others, red for correct
  labs(title = "RMSE for Estimated Probabilities of Shared Events Per Run (Config 6: 20000_10000)", 
       x = "Event Type", 
       y = "RMSE (Per Run)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")  # Remove the legend for clarity







library(tidyr)
library(dplyr)
library(ggplot2)

# Load the data
file_path <- "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_6_40000_10000_results/summary_output.txt"
df <- read.table(file_path, header = TRUE, sep = "\t")

# Create a function to calculate RMSE per run
calculate_rmse_per_run <- function(predicted, actual) {
  sqrt((predicted - actual)^2)
}

# Reshape the data to calculate RMSE per run and per event type
event_types <- c("Prob_1_Event", "Prob_2_Events", "Prob_3_Events", "Prob_4_Events", "Prob_5_Events", "Prob_6_Events")

# Create a long format data frame with RMSE values per run
rmse_long_df <- df %>%
  gather(key = "EventType", value = "Probability", all_of(event_types)) %>%
  mutate(RMSE = calculate_rmse_per_run(Probability, True_Number_Events))

# Plot the RMSE results per run using a boxplot to show the distribution
ggplot(rmse_long_df, aes(x = EventType, y = RMSE, fill = EventType == "Prob_6_Events")) +
  geom_boxplot() +
  scale_fill_manual(values = c("skyblue", "red")) +  # Set colors: skyblue for others, red for correct
  labs(title = "RMSE for Estimated Probabilities of Shared Events Per Run (Config 6: 40000_10000)", 
       x = "Event Type", 
       y = "RMSE (Per Run)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")  # Remove the legend for clarity

\






library(tidyr)
library(dplyr)
library(ggplot2)

# Load the data
file_path <- "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_6_80000_10000_results/summary_output.txt"
df <- read.table(file_path, header = TRUE, sep = "\t")

# Create a function to calculate RMSE per run
calculate_rmse_per_run <- function(predicted, actual) {
  sqrt((predicted - actual)^2)
}

# Reshape the data to calculate RMSE per run and per event type
event_types <- c("Prob_1_Event", "Prob_2_Events", "Prob_3_Events", "Prob_4_Events", "Prob_5_Events", "Prob_6_Events")

# Create a long format data frame with RMSE values per run
rmse_long_df <- df %>%
  gather(key = "EventType", value = "Probability", all_of(event_types)) %>%
  mutate(RMSE = calculate_rmse_per_run(Probability, True_Number_Events))

# Plot the RMSE results per run using a boxplot to show the distribution
ggplot(rmse_long_df, aes(x = EventType, y = RMSE, fill = EventType == "Prob_6_Events")) +
  geom_boxplot() +
  scale_fill_manual(values = c("skyblue", "red")) +  # Set colors: skyblue for others, red for correct
  labs(title = "RMSE for Estimated Probabilities of Shared Events Per Run (Config 6: 80000_10000)", 
       x = "Event Type", 
       y = "RMSE (Per Run)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")  # Remove the legend for clarity






library(tidyr)
library(dplyr)
library(ggplot2)

# Load the data
file_path <- "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_6_160000_10000_results/summary_output.txt"
df <- read.table(file_path, header = TRUE, sep = "\t")

# Create a function to calculate RMSE per run
calculate_rmse_per_run <- function(predicted, actual) {
  sqrt((predicted - actual)^2)
}

# Reshape the data to calculate RMSE per run and per event type
event_types <- c("Prob_1_Event", "Prob_2_Events", "Prob_3_Events", "Prob_4_Events", "Prob_5_Events", "Prob_6_Events")

# Create a long format data frame with RMSE values per run
rmse_long_df <- df %>%
  gather(key = "EventType", value = "Probability", all_of(event_types)) %>%
  mutate(RMSE = calculate_rmse_per_run(Probability, True_Number_Events))

# Plot the RMSE results per run using a boxplot to show the distribution
ggplot(rmse_long_df, aes(x = EventType, y = RMSE, fill = EventType == "Prob_6_Events")) +
  geom_boxplot() +
  scale_fill_manual(values = c("skyblue", "red")) +  # Set colors: skyblue for others, red for correct
  labs(title = "RMSE for Estimated Probabilities of Shared Events Per Run (Config 6: 160000_10000)", 
       x = "Event Type", 
       y = "RMSE (Per Run)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")  # Remove the legend for clarity









library(tidyr)
library(dplyr)
library(ggplot2)

# Load the data
file_path <- "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_3_10000_10000_results/summary_output.txt"
df <- read.table(file_path, header = TRUE, sep = "\t")

# Create a function to calculate RMSE per run
calculate_rmse_per_run <- function(predicted, actual) {
  sqrt((predicted - actual)^2)
}

# Reshape the data to calculate RMSE per run and per event type
event_types <- c("Prob_1_Event", "Prob_2_Events", "Prob_3_Events", "Prob_4_Events", "Prob_5_Events", "Prob_6_Events")

# Create a long format data frame with RMSE values per run
rmse_long_df <- df %>%
  gather(key = "EventType", value = "Probability", all_of(event_types)) %>%
  mutate(RMSE = calculate_rmse_per_run(Probability, True_Number_Events))

# Plot the RMSE results per run using a boxplot to show the distribution
ggplot(rmse_long_df, aes(x = EventType, y = RMSE, fill = EventType == "Prob_3_Events")) +
  geom_boxplot() +
  scale_fill_manual(values = c("skyblue", "red")) +  # Set colors: skyblue for others, red for correct
  labs(title = "RMSE for Estimated Probabilities of Shared Events Per Run (Config 3: 10000_10000)", 
       x = "Event Type", 
       y = "RMSE (Per Run)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")  # Remove the legend for clarity




library(tidyr)
library(dplyr)
library(ggplot2)

# Load the data
file_path <- "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_3_20000_10000_results/summary_output.txt"
df <- read.table(file_path, header = TRUE, sep = "\t")

# Create a function to calculate RMSE per run
calculate_rmse_per_run <- function(predicted, actual) {
  sqrt((predicted - actual)^2)
}

# Reshape the data to calculate RMSE per run and per event type
event_types <- c("Prob_1_Event", "Prob_2_Events", "Prob_3_Events", "Prob_4_Events", "Prob_5_Events", "Prob_6_Events")

# Create a long format data frame with RMSE values per run
rmse_long_df <- df %>%
  gather(key = "EventType", value = "Probability", all_of(event_types)) %>%
  mutate(RMSE = calculate_rmse_per_run(Probability, True_Number_Events))

# Plot the RMSE results per run using a boxplot to show the distribution
ggplot(rmse_long_df, aes(x = EventType, y = RMSE, fill = EventType == "Prob_3_Events")) +
  geom_boxplot() +
  scale_fill_manual(values = c("skyblue", "red")) +  # Set colors: skyblue for others, red for correct
  labs(title = "RMSE for Estimated Probabilities of Shared Events Per Run (Config 3: 20000_10000)", 
       x = "Event Type", 
       y = "RMSE (Per Run)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")  # Remove the legend for clarity








library(tidyr)
library(dplyr)
library(ggplot2)

# Load the data
file_path <- "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_3_40000_10000_results/summary_output.txt"
df <- read.table(file_path, header = TRUE, sep = "\t")

# Create a function to calculate RMSE per run
calculate_rmse_per_run <- function(predicted, actual) {
  sqrt((predicted - actual)^2)
}

# Reshape the data to calculate RMSE per run and per event type
event_types <- c("Prob_1_Event", "Prob_2_Events", "Prob_3_Events", "Prob_4_Events", "Prob_5_Events", "Prob_6_Events")

# Create a long format data frame with RMSE values per run
rmse_long_df <- df %>%
  gather(key = "EventType", value = "Probability", all_of(event_types)) %>%
  mutate(RMSE = calculate_rmse_per_run(Probability, True_Number_Events))

# Plot the RMSE results per run using a boxplot to show the distribution
ggplot(rmse_long_df, aes(x = EventType, y = RMSE, fill = EventType == "Prob_3_Events")) +
  geom_boxplot() +
  scale_fill_manual(values = c("skyblue", "red")) +  # Set colors: skyblue for others, red for correct
  labs(title = "RMSE for Estimated Probabilities of Shared Events Per Run (Config 3: 40000_10000)", 
       x = "Event Type", 
       y = "RMSE (Per Run)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")  # Remove the legend for clarity




library(tidyr)
library(dplyr)
library(ggplot2)

# Load the data
file_path <- "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_3_80000_10000_results/summary_output.txt"
df <- read.table(file_path, header = TRUE, sep = "\t")

# Create a function to calculate RMSE per run
calculate_rmse_per_run <- function(predicted, actual) {
  sqrt((predicted - actual)^2)
}

# Reshape the data to calculate RMSE per run and per event type
event_types <- c("Prob_1_Event", "Prob_2_Events", "Prob_3_Events", "Prob_4_Events", "Prob_5_Events", "Prob_6_Events")

# Create a long format data frame with RMSE values per run
rmse_long_df <- df %>%
  gather(key = "EventType", value = "Probability", all_of(event_types)) %>%
  mutate(RMSE = calculate_rmse_per_run(Probability, True_Number_Events))

# Plot the RMSE results per run using a boxplot to show the distribution
ggplot(rmse_long_df, aes(x = EventType, y = RMSE, fill = EventType == "Prob_3_Events")) +
  geom_boxplot() +
  scale_fill_manual(values = c("skyblue", "red")) +  # Set colors: skyblue for others, red for correct
  labs(title = "RMSE for Estimated Probabilities of Shared Events Per Run (Config 3: 80000_10000)", 
       x = "Event Type", 
       y = "RMSE (Per Run)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")  # Remove the legend for clarity






library(tidyr)
library(dplyr)
library(ggplot2)

# Load the data
file_path <- "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_1_0_10000_results/summary_output.txt"
df <- read.table(file_path, header = TRUE, sep = "\t")

# Create a function to calculate RMSE per run
calculate_rmse_per_run <- function(predicted, actual) {
  sqrt((predicted - actual)^2)
}

# Reshape the data to calculate RMSE per run and per event type
event_types <- c("Prob_1_Event", "Prob_2_Events", "Prob_3_Events", "Prob_4_Events", "Prob_5_Events", "Prob_6_Events")

# Create a long format data frame with RMSE values per run
rmse_long_df <- df %>%
  gather(key = "EventType", value = "Probability", all_of(event_types)) %>%
  mutate(RMSE = calculate_rmse_per_run(Probability, True_Number_Events))

# Plot the RMSE results per run using a boxplot to show the distribution
ggplot(rmse_long_df, aes(x = EventType, y = RMSE, fill = EventType == "Prob_3_Events")) +
  geom_boxplot() +
  scale_fill_manual(values = c("skyblue", "red")) +  # Set colors: skyblue for others, red for correct
  labs(title = "RMSE for Estimated Probabilities of Shared Events Per Run (Config 3: 160000_10000)", 
       x = "Event Type", 
       y = "RMSE (Per Run)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")  # Remove the legend for clarity















library(tidyr)
library(dplyr)
library(ggplot2)

# Load the summary file
file_path <- "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_1_0_10000_results/summary_output.txt"
df_new <- read.table(file_path, header = TRUE, sep = "\t")

# Gather the Mean divergence values into long format
df_new_long_mean <- df_new %>%
  gather(key = "PopPair", value = "MeanDivergence", 
         Mean_Divergence_PopA_PopB:Mean_Divergence_PopK_PopL) %>%
  select(Run, PopPair, MeanDivergence)

# Gather the True divergence values into long format
df_new_long_true <- df_new %>%
  gather(key = "PopPair_True", value = "TrueValue", 
         True_Divergence_PopA_PopB:True_Divergence_PopK_PopL) %>%
  mutate(PopPair = gsub("True_Divergence_", "Mean_Divergence_", PopPair_True)) %>%
  select(Run, PopPair, TrueValue)

# Merge both datasets based on matching population pairs and runs
df_combined_new <- inner_join(df_new_long_mean, df_new_long_true, by = c("Run", "PopPair"))

# Check for duplicates and remove if necessary
df_combined_new_unique <- df_combined_new[!duplicated(df_combined_new[, c("TrueValue", "MeanDivergence")]), ]

# Convert the mean estimated values to the same scale as the true values
df_combined_new_unique$MeanDivergence <- df_combined_new_unique$MeanDivergence * 1e8  # Adjust scale to match TrueValue

# Determine the max limit for the plot axes (can be customized)
max_limit <- max(c(max(df_combined_new_unique$TrueValue), max(df_combined_new_unique$MeanDivergence)))

# Create the plot with both axes limited
ggplot(df_combined_new_unique, aes(x = TrueValue, y = MeanDivergence, color = PopPair)) +
  geom_point(alpha = 0.3, size = 3) +  # Set alpha transparency and point size
  geom_smooth(method = "lm", se = FALSE) +  # Add trend lines without confidence shading
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +  # Add 1:1 diagonal line
  labs(x = "True Divergence", y = "Estimated Mean Divergence (x 10^-8)", 
       title = "True vs Estimated Divergence (Config 1: 0_10000)") +
  theme_bw() +
  coord_fixed(ratio = 1) +  # Ensure the aspect ratio is 1:1
  xlim(0, max_limit) +  # Set x-axis limit
  ylim(0, max_limit) +  # Set y-axis limit
  theme(legend.position = "none")  # Remove the legend










library(tidyr)
library(dplyr)
library(ggplot2)

# Load the summary file
file_path <- "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_3_10000_10000_results/summary_output.txt"
df_new <- read.table(file_path, header = TRUE, sep = "\t")

# Gather the Mean divergence values into long format
df_new_long_mean <- df_new %>%
  gather(key = "PopPair", value = "MeanDivergence", 
         Mean_Divergence_PopA_PopB:Mean_Divergence_PopK_PopL) %>%
  select(Run, PopPair, MeanDivergence)

# Gather the True divergence values into long format
df_new_long_true <- df_new %>%
  gather(key = "PopPair_True", value = "TrueValue", 
         True_Divergence_PopA_PopB:True_Divergence_PopK_PopL) %>%
  mutate(PopPair = gsub("True_Divergence_", "Mean_Divergence_", PopPair_True)) %>%
  select(Run, PopPair, TrueValue)

# Merge both datasets based on matching population pairs and runs
df_combined_new <- inner_join(df_new_long_mean, df_new_long_true, by = c("Run", "PopPair"))

# Remove duplicates if necessary
df_combined_new_unique <- df_combined_new[!duplicated(df_combined_new[, c("TrueValue", "MeanDivergence")]), ]

# Divide the mean estimated values by 1e-8
df_combined_new_unique$MeanDivergence <- df_combined_new_unique$MeanDivergence / 1e-8

# Determine the max limit for the plot axes (can be customized)
max_limit <- max(c(max(df_combined_new_unique$TrueValue), max(df_combined_new_unique$MeanDivergence)))

# Create the plot with both axes limited
ggplot(df_combined_new_unique, aes(x = TrueValue, y = MeanDivergence, color = PopPair)) +
  geom_point(alpha = 0.3, size = 3) +  # Set alpha transparency and point size
  geom_smooth(method = "lm", se = FALSE) +  # Add trend lines without confidence shading
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +  # Add 1:1 diagonal line
  labs(x = "True Divergence", y = "Estimated Mean Divergence (x 10^-8)", 
       title = "True vs Estimated Divergence (Config 3: 10000_10000)") +
  theme_bw() +
  coord_fixed(ratio = 1) +  # Ensure the aspect ratio is 1:1
  xlim(0, max_limit) +  # Set x-axis limit
  ylim(0, max_limit) +  # Set y-axis limit
  theme(legend.position = "none")  # Remove the legend











library(tidyr)
library(dplyr)
library(ggplot2)

# Load the summary file
file_path <- "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_3_20000_10000_results/summary_output.txt"
df_new <- read.table(file_path, header = TRUE, sep = "\t")

# Gather the Mean divergence values into long format
df_new_long_mean <- df_new %>%
  gather(key = "PopPair", value = "MeanDivergence", 
         Mean_Divergence_PopA_PopB:Mean_Divergence_PopK_PopL) %>%
  select(Run, PopPair, MeanDivergence)

# Gather the True divergence values into long format
df_new_long_true <- df_new %>%
  gather(key = "PopPair_True", value = "TrueValue", 
         True_Divergence_PopA_PopB:True_Divergence_PopK_PopL) %>%
  mutate(PopPair = gsub("True_Divergence_", "Mean_Divergence_", PopPair_True)) %>%
  select(Run, PopPair, TrueValue)

# Merge both datasets based on matching population pairs and runs
df_combined_new <- inner_join(df_new_long_mean, df_new_long_true, by = c("Run", "PopPair"))

# Remove duplicates if necessary
df_combined_new_unique <- df_combined_new[!duplicated(df_combined_new[, c("TrueValue", "MeanDivergence")]), ]

# Divide the mean estimated values by 1e-8
df_combined_new_unique$MeanDivergence <- df_combined_new_unique$MeanDivergence / 1e-8

# Determine the max limit for the plot axes (can be customized)
max_limit <- max(c(max(df_combined_new_unique$TrueValue), max(df_combined_new_unique$MeanDivergence)))

# Create the plot with both axes limited
ggplot(df_combined_new_unique, aes(x = TrueValue, y = MeanDivergence, color = PopPair)) +
  geom_point(alpha = 0.3, size = 3) +  # Set alpha transparency and point size
  geom_smooth(method = "lm", se = FALSE) +  # Add trend lines without confidence shading
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +  # Add 1:1 diagonal line
  labs(x = "True Divergence", y = "Estimated Mean Divergence (x 10^-8)", 
       title = "True vs Estimated Divergence (Config 3: 20000_10000)") +
  theme_bw() +
  coord_fixed(ratio = 1) +  # Ensure the aspect ratio is 1:1
  xlim(0, max_limit) +  # Set x-axis limit
  ylim(0, max_limit) +  # Set y-axis limit
  theme(legend.position = "none")  # Remove the legend






library(tidyr)
library(dplyr)
library(ggplot2)

# Load the summary file
file_path <- "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_3_40000_10000_results/summary_output.txt"
df_new <- read.table(file_path, header = TRUE, sep = "\t")

# Gather the Mean divergence values into long format
df_new_long_mean <- df_new %>%
  gather(key = "PopPair", value = "MeanDivergence", 
         Mean_Divergence_PopA_PopB:Mean_Divergence_PopK_PopL) %>%
  select(Run, PopPair, MeanDivergence)

# Gather the True divergence values into long format
df_new_long_true <- df_new %>%
  gather(key = "PopPair_True", value = "TrueValue", 
         True_Divergence_PopA_PopB:True_Divergence_PopK_PopL) %>%
  mutate(PopPair = gsub("True_Divergence_", "Mean_Divergence_", PopPair_True)) %>%
  select(Run, PopPair, TrueValue)

# Merge both datasets based on matching population pairs and runs
df_combined_new <- inner_join(df_new_long_mean, df_new_long_true, by = c("Run", "PopPair"))

# Remove duplicates if necessary
df_combined_new_unique <- df_combined_new[!duplicated(df_combined_new[, c("TrueValue", "MeanDivergence")]), ]

# Divide the mean estimated values by 1e-8
df_combined_new_unique$MeanDivergence <- df_combined_new_unique$MeanDivergence / 1e-8

# Determine the max limit for the plot axes (can be customized)
max_limit <- max(c(max(df_combined_new_unique$TrueValue), max(df_combined_new_unique$MeanDivergence)))

# Create the plot with both axes limited
ggplot(df_combined_new_unique, aes(x = TrueValue, y = MeanDivergence, color = PopPair)) +
  geom_point(alpha = 0.3, size = 3) +  # Set alpha transparency and point size
  geom_smooth(method = "lm", se = FALSE) +  # Add trend lines without confidence shading
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +  # Add 1:1 diagonal line
  labs(x = "True Divergence", y = "Estimated Mean Divergence (x 10^-8)", 
       title = "True vs Estimated Divergence (Config 3: 40000_10000)") +
  theme_bw() +
  coord_fixed(ratio = 1) +  # Ensure the aspect ratio is 1:1
  xlim(0, max_limit) +  # Set x-axis limit
  ylim(0, max_limit) +  # Set y-axis limit
  theme(legend.position = "none")  # Remove the legend










library(tidyr)
library(dplyr)
library(ggplot2)

# Load the summary file
file_path <- "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_3_10000_10000_results/summary_output.txt"
df_new <- read.table(file_path, header = TRUE, sep = "\t")

# Gather the Mean divergence values into long format
df_new_long_mean <- df_new %>%
  gather(key = "PopPair", value = "MeanDivergence", 
         Mean_Divergence_PopA_PopB:Mean_Divergence_PopK_PopL) %>%
  select(Run, PopPair, MeanDivergence)

# Gather the True divergence values into long format
df_new_long_true <- df_new %>%
  gather(key = "PopPair_True", value = "TrueValue", 
         True_Divergence_PopA_PopB:True_Divergence_PopK_PopL) %>%
  mutate(PopPair = gsub("True_Divergence_", "Mean_Divergence_", PopPair_True)) %>%
  select(Run, PopPair, TrueValue)

# Merge both datasets based on matching population pairs and runs
df_combined_new <- inner_join(df_new_long_mean, df_new_long_true, by = c("Run", "PopPair"))

# Remove duplicates if necessary
df_combined_new_unique <- df_combined_new[!duplicated(df_combined_new[, c("TrueValue", "MeanDivergence")]), ]

# Divide the mean estimated values by 1e-8
df_combined_new_unique$MeanDivergence <- df_combined_new_unique$MeanDivergence / 1e-8

# Determine the max limit for the plot axes (can be customized)
max_limit <- max(c(max(df_combined_new_unique$TrueValue), max(df_combined_new_unique$MeanDivergence)))

# Create the plot with both axes limited
ggplot(df_combined_new_unique, aes(x = TrueValue, y = MeanDivergence, color = PopPair)) +
  geom_point(alpha = 0.3, size = 3) +  # Set alpha transparency and point size
  geom_smooth(method = "lm", se = FALSE) +  # Add trend lines without confidence shading
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +  # Add 1:1 diagonal line
  labs(x = "True Divergence", y = "Estimated Mean Divergence (x 10^-8)", 
       title = "True vs Estimated Divergence (Config 3: 80000_10000)") +
  theme_bw() +
  coord_fixed(ratio = 1) +  # Ensure the aspect ratio is 1:1
  xlim(0, max_limit) +  # Set x-axis limit
  ylim(0, max_limit) +  # Set y-axis limit
  theme(legend.position = "none")  # Remove the legend






library(tidyr)
library(dplyr)
library(ggplot2)

# Load the summary file
file_path <- "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_3_160000_10000_results/summary_output.txt"
df_new <- read.table(file_path, header = TRUE, sep = "\t")

# Gather the Mean divergence values into long format
df_new_long_mean <- df_new %>%
  gather(key = "PopPair", value = "MeanDivergence", 
         Mean_Divergence_PopA_PopB:Mean_Divergence_PopK_PopL) %>%
  select(Run, PopPair, MeanDivergence)

# Gather the True divergence values into long format
df_new_long_true <- df_new %>%
  gather(key = "PopPair_True", value = "TrueValue", 
         True_Divergence_PopA_PopB:True_Divergence_PopK_PopL) %>%
  mutate(PopPair = gsub("True_Divergence_", "Mean_Divergence_", PopPair_True)) %>%
  select(Run, PopPair, TrueValue)

# Merge both datasets based on matching population pairs and runs
df_combined_new <- inner_join(df_new_long_mean, df_new_long_true, by = c("Run", "PopPair"))

# Remove duplicates if necessary
df_combined_new_unique <- df_combined_new[!duplicated(df_combined_new[, c("TrueValue", "MeanDivergence")]), ]

# Divide the mean estimated values by 1e-8
df_combined_new_unique$MeanDivergence <- df_combined_new_unique$MeanDivergence / 1e-8

# Determine the max limit for the plot axes (can be customized)
max_limit <- max(c(max(df_combined_new_unique$TrueValue), max(df_combined_new_unique$MeanDivergence)))

# Create the plot with both axes limited
ggplot(df_combined_new_unique, aes(x = TrueValue, y = MeanDivergence, color = PopPair)) +
  geom_point(alpha = 0.3, size = 3) +  # Set alpha transparency and point size
  geom_smooth(method = "lm", se = FALSE) +  # Add trend lines without confidence shading
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +  # Add 1:1 diagonal line
  labs(x = "True Divergence", y = "Estimated Mean Divergence (x 10^-8)", 
       title = "True vs Estimated Divergence (Config 3: 160000_10000)") +
  theme_bw() +
  coord_fixed(ratio = 1) +  # Ensure the aspect ratio is 1:1
  xlim(0, max_limit) +  # Set x-axis limit
  ylim(0, max_limit) +  # Set y-axis limit
  theme(legend.position = "none")  # Remove the legend











\




library(tidyr)
library(dplyr)
library(ggplot2)

# Load the summary file
file_path <- "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_6_10000_10000_results/summary_output.txt"
df_new <- read.table(file_path, header = TRUE, sep = "\t")

# Gather the Mean divergence values into long format
df_new_long_mean <- df_new %>%
  gather(key = "PopPair", value = "MeanDivergence", 
         Mean_Divergence_PopA_PopB:Mean_Divergence_PopK_PopL) %>%
  select(Run, PopPair, MeanDivergence)

# Gather the True divergence values into long format
df_new_long_true <- df_new %>%
  gather(key = "PopPair_True", value = "TrueValue", 
         True_Divergence_PopA_PopB:True_Divergence_PopK_PopL) %>%
  mutate(PopPair = gsub("True_Divergence_", "Mean_Divergence_", PopPair_True)) %>%
  select(Run, PopPair, TrueValue)

# Merge both datasets based on matching population pairs and runs
df_combined_new <- inner_join(df_new_long_mean, df_new_long_true, by = c("Run", "PopPair"))

# Remove duplicates if necessary
df_combined_new_unique <- df_combined_new[!duplicated(df_combined_new[, c("TrueValue", "MeanDivergence")]), ]

# Divide the mean estimated values by 1e-8
df_combined_new_unique$MeanDivergence <- df_combined_new_unique$MeanDivergence / 1e-8

# Determine the max limit for the plot axes (can be customized)
max_limit <- max(c(max(df_combined_new_unique$TrueValue), max(df_combined_new_unique$MeanDivergence)))

# Create the plot with both axes limited
ggplot(df_combined_new_unique, aes(x = TrueValue, y = MeanDivergence, color = PopPair)) +
  geom_point(alpha = 0.3, size = 3) +  # Set alpha transparency and point size
  geom_smooth(method = "lm", se = FALSE) +  # Add trend lines without confidence shading
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +  # Add 1:1 diagonal line
  labs(x = "True Divergence", y = "Estimated Mean Divergence (x 10^-8)", 
       title = "True vs Estimated Divergence (Config 6: 10000_10000)") +
  theme_bw() +
  coord_fixed(ratio = 1) +  # Ensure the aspect ratio is 1:1
  xlim(0, max_limit) +  # Set x-axis limit
  ylim(0, max_limit) +  # Set y-axis limit
  theme(legend.position = "none")  # Remove the legend











library(tidyr)
library(dplyr)
library(ggplot2)

# Load the summary file
file_path <- "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_6_20000_10000_results/summary_output.txt"
df_new <- read.table(file_path, header = TRUE, sep = "\t")

# Gather the Mean divergence values into long format
df_new_long_mean <- df_new %>%
  gather(key = "PopPair", value = "MeanDivergence", 
         Mean_Divergence_PopA_PopB:Mean_Divergence_PopK_PopL) %>%
  select(Run, PopPair, MeanDivergence)

# Gather the True divergence values into long format
df_new_long_true <- df_new %>%
  gather(key = "PopPair_True", value = "TrueValue", 
         True_Divergence_PopA_PopB:True_Divergence_PopK_PopL) %>%
  mutate(PopPair = gsub("True_Divergence_", "Mean_Divergence_", PopPair_True)) %>%
  select(Run, PopPair, TrueValue)

# Merge both datasets based on matching population pairs and runs
df_combined_new <- inner_join(df_new_long_mean, df_new_long_true, by = c("Run", "PopPair"))

# Remove duplicates if necessary
df_combined_new_unique <- df_combined_new[!duplicated(df_combined_new[, c("TrueValue", "MeanDivergence")]), ]

# Divide the mean estimated values by 1e-8
df_combined_new_unique$MeanDivergence <- df_combined_new_unique$MeanDivergence / 1e-8

# Determine the max limit for the plot axes (can be customized)
max_limit <- max(c(max(df_combined_new_unique$TrueValue), max(df_combined_new_unique$MeanDivergence)))

# Create the plot with both axes limited
ggplot(df_combined_new_unique, aes(x = TrueValue, y = MeanDivergence, color = PopPair)) +
  geom_point(alpha = 0.3, size = 3) +  # Set alpha transparency and point size
  geom_smooth(method = "lm", se = FALSE) +  # Add trend lines without confidence shading
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +  # Add 1:1 diagonal line
  labs(x = "True Divergence", y = "Estimated Mean Divergence (x 10^-8)", 
       title = "True vs Estimated Divergence (Config 6: 20000_10000)") +
  theme_bw() +
  coord_fixed(ratio = 1) +  # Ensure the aspect ratio is 1:1
  xlim(0, max_limit) +  # Set x-axis limit
  ylim(0, max_limit) +  # Set y-axis limit
  theme(legend.position = "none")  # Remove the legend









library(tidyr)
library(dplyr)
library(ggplot2)

# Load the summary file
file_path <- "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_6_40000_10000_results/summary_output.txt"
df_new <- read.table(file_path, header = TRUE, sep = "\t")

# Gather the Mean divergence values into long format
df_new_long_mean <- df_new %>%
  gather(key = "PopPair", value = "MeanDivergence", 
         Mean_Divergence_PopA_PopB:Mean_Divergence_PopK_PopL) %>%
  select(Run, PopPair, MeanDivergence)

# Gather the True divergence values into long format
df_new_long_true <- df_new %>%
  gather(key = "PopPair_True", value = "TrueValue", 
         True_Divergence_PopA_PopB:True_Divergence_PopK_PopL) %>%
  mutate(PopPair = gsub("True_Divergence_", "Mean_Divergence_", PopPair_True)) %>%
  select(Run, PopPair, TrueValue)

# Merge both datasets based on matching population pairs and runs
df_combined_new <- inner_join(df_new_long_mean, df_new_long_true, by = c("Run", "PopPair"))

# Remove duplicates if necessary
df_combined_new_unique <- df_combined_new[!duplicated(df_combined_new[, c("TrueValue", "MeanDivergence")]), ]

# Divide the mean estimated values by 1e-8
df_combined_new_unique$MeanDivergence <- df_combined_new_unique$MeanDivergence / 1e-8

# Determine the max limit for the plot axes (can be customized)
max_limit <- max(c(max(df_combined_new_unique$TrueValue), max(df_combined_new_unique$MeanDivergence)))

# Create the plot with both axes limited
ggplot(df_combined_new_unique, aes(x = TrueValue, y = MeanDivergence, color = PopPair)) +
  geom_point(alpha = 0.3, size = 3) +  # Set alpha transparency and point size
  geom_smooth(method = "lm", se = FALSE) +  # Add trend lines without confidence shading
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +  # Add 1:1 diagonal line
  labs(x = "True Divergence", y = "Estimated Mean Divergence (x 10^-8)", 
       title = "True vs Estimated Divergence (Config 6: 40000_10000)") +
  theme_bw() +
  coord_fixed(ratio = 1) +  # Ensure the aspect ratio is 1:1
  xlim(0, max_limit) +  # Set x-axis limit
  ylim(0, max_limit) +  # Set y-axis limit
  theme(legend.position = "none")  # Remove the legend







library(tidyr)
library(dplyr)
library(ggplot2)

# Load the summary file
file_path <- "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_6_80000_10000_results/summary_output.txt"
df_new <- read.table(file_path, header = TRUE, sep = "\t")

# Gather the Mean divergence values into long format
df_new_long_mean <- df_new %>%
  gather(key = "PopPair", value = "MeanDivergence", 
         Mean_Divergence_PopA_PopB:Mean_Divergence_PopK_PopL) %>%
  select(Run, PopPair, MeanDivergence)

# Gather the True divergence values into long format
df_new_long_true <- df_new %>%
  gather(key = "PopPair_True", value = "TrueValue", 
         True_Divergence_PopA_PopB:True_Divergence_PopK_PopL) %>%
  mutate(PopPair = gsub("True_Divergence_", "Mean_Divergence_", PopPair_True)) %>%
  select(Run, PopPair, TrueValue)

# Merge both datasets based on matching population pairs and runs
df_combined_new <- inner_join(df_new_long_mean, df_new_long_true, by = c("Run", "PopPair"))

# Remove duplicates if necessary
df_combined_new_unique <- df_combined_new[!duplicated(df_combined_new[, c("TrueValue", "MeanDivergence")]), ]

# Divide the mean estimated values by 1e-8
df_combined_new_unique$MeanDivergence <- df_combined_new_unique$MeanDivergence / 1e-8

# Determine the max limit for the plot axes (can be customized)
max_limit <- max(c(max(df_combined_new_unique$TrueValue), max(df_combined_new_unique$MeanDivergence)))

# Create the plot with both axes limited
ggplot(df_combined_new_unique, aes(x = TrueValue, y = MeanDivergence, color = PopPair)) +
  geom_point(alpha = 0.3, size = 3) +  # Set alpha transparency and point size
  geom_smooth(method = "lm", se = FALSE) +  # Add trend lines without confidence shading
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +  # Add 1:1 diagonal line
  labs(x = "True Divergence", y = "Estimated Mean Divergence (x 10^-8)", 
       title = "True vs Estimated Divergence (Config 6: 80000_10000)") +
  theme_bw() +
  coord_fixed(ratio = 1) +  # Ensure the aspect ratio is 1:1
  xlim(0, max_limit) +  # Set x-axis limit
  ylim(0, max_limit) +  # Set y-axis limit
  theme(legend.position = "none")  # Remove the legend








library(tidyr)
library(dplyr)
library(ggplot2)

# Load the summary file
file_path <- "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_6_160000_10000_results/summary_output.txt"
df_new <- read.table(file_path, header = TRUE, sep = "\t")

# Gather the Mean divergence values into long format
df_new_long_mean <- df_new %>%
  gather(key = "PopPair", value = "MeanDivergence", 
         Mean_Divergence_PopA_PopB:Mean_Divergence_PopK_PopL) %>%
  select(Run, PopPair, MeanDivergence)

# Gather the True divergence values into long format
df_new_long_true <- df_new %>%
  gather(key = "PopPair_True", value = "TrueValue", 
         True_Divergence_PopA_PopB:True_Divergence_PopK_PopL) %>%
  mutate(PopPair = gsub("True_Divergence_", "Mean_Divergence_", PopPair_True)) %>%
  select(Run, PopPair, TrueValue)

# Merge both datasets based on matching population pairs and runs
df_combined_new <- inner_join(df_new_long_mean, df_new_long_true, by = c("Run", "PopPair"))

# Remove duplicates if necessary
df_combined_new_unique <- df_combined_new[!duplicated(df_combined_new[, c("TrueValue", "MeanDivergence")]), ]

# Divide the mean estimated values by 1e-8
df_combined_new_unique$MeanDivergence <- df_combined_new_unique$MeanDivergence / 1e-8

# Determine the max limit for the plot axes (can be customized)
max_limit <- max(c(max(df_combined_new_unique$TrueValue), max(df_combined_new_unique$MeanDivergence)))

# Create the plot with both axes limited
ggplot(df_combined_new_unique, aes(x = TrueValue, y = MeanDivergence, color = PopPair)) +
  geom_point(alpha = 0.3, size = 3) +  # Set alpha transparency and point size
  geom_smooth(method = "lm", se = FALSE) +  # Add trend lines without confidence shading
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +  # Add 1:1 diagonal line
  labs(x = "True Divergence", y = "Estimated Mean Divergence (x 10^-8)", 
       title = "True vs Estimated Divergence (Config 6: 160000_10000)") +
  theme_bw() +
  coord_fixed(ratio = 1) +  # Ensure the aspect ratio is 1:1
  xlim(0, max_limit) +  # Set x-axis limit
  ylim(0, max_limit) +  # Set y-axis limit
  theme(legend.position = "none")  # Remove the legend












library(tidyr)
library(dplyr)
library(ggplot2)

# Load the data
file_path <- "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_1_0_10000_results/summary_output.txt"
df <- read.table(file_path, header = TRUE, sep = "\t")

# Event types for probabilities
event_types <- c("Prob_1_Event", "Prob_2_Events", "Prob_3_Events", "Prob_4_Events", "Prob_5_Events", "Prob_6_Events")

# Reshape the data into a long format
probabilities_long_df <- df %>%
  gather(key = "EventType", value = "Probability", all_of(event_types))

# Create a boxplot to show the distribution of probabilities for each event type across all runs
ggplot(probabilities_long_df, aes(x = EventType, y = Probability, fill = EventType)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set3") +  # Use a color palette for differentiation
  labs(title = "Distribution of Probabilities for Each Event Type Across Runs", 
       x = "Event Type", 
       y = "Probability") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "none")  # Remove the legend for clarity










library(tidyr)
library(dplyr)
library(ggplot2)

# Load the data
file_path <- "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_1_0_10000_results/summary_output.txt"
df <- read.table(file_path, header = TRUE, sep = "\t")

# Event types for probabilities
event_types <- c("Prob_1_Event", "Prob_2_Events", "Prob_3_Events", "Prob_4_Events", "Prob_5_Events", "Prob_6_Events")

# Reshape the data into a long format
probabilities_long_df <- df %>%
  gather(key = "EventType", value = "Probability", all_of(event_types))

# Create density plots to show the distribution of probabilities for each event type across all runs
ggplot(probabilities_long_df, aes(x = Probability, fill = EventType, color = EventType)) +
  geom_density(alpha = 0.4, adjust = 1) +  # Density plot with transparency
  scale_fill_brewer(palette = "Set2") +  # Use a color palette for differentiation
  scale_color_brewer(palette = "Set2") +  # Matching colors for lines
  labs(title = "Density Distribution of Probabilities for Each Event Type", 
       x = "Probability", 
       y = "Density") +
  theme_minimal() +
  theme(legend.position = "right")







library(tidyr)
library(dplyr)
library(ggplot2)

# Load the data
file_path <- "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_1_0_10000_results/summary_output.txt"
df <- read.table(file_path, header = TRUE, sep = "\t")

# Event types for probabilities
event_types <- c("Prob_1_Event", "Prob_2_Events", "Prob_3_Events", "Prob_4_Events", "Prob_5_Events", "Prob_6_Events")

# Reshape the data into a long format
probabilities_long_df <- df %>%
  gather(key = "EventType", value = "Probability", all_of(event_types))

# Create a violin plot to show the distribution of probabilities for each event type across all runs
ggplot(probabilities_long_df, aes(x = EventType, y = Probability, fill = EventType)) +
  geom_violin(trim = FALSE, alpha = 0.7) +  # Violin plot without trimming tails
  geom_boxplot(width = 0.1, outlier.shape = NA, alpha = 0.4) +  # Overlay a boxplot for summary stats
  scale_fill_brewer(palette = "Set3") +  # Use a color palette for differentiation
  labs(title = "Distribution of Probabilities for Each Event Type Across Runs", 
       x = "Event Type", 
       y = "Probability") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "none")  # Remove the legend for clarity








library(tidyr)
library(dplyr)
library(ggplot2)

# Load the data
file_path <- "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_1_0_10000_results/summary_output.txt"
df <- read.table(file_path, header = TRUE, sep = "\t")

# Event types for probabilities
event_types <- c("Prob_1_Event", "Prob_2_Events", "Prob_3_Events", "Prob_4_Events", "Prob_5_Events", "Prob_6_Events")

# Reshape the data into a long format
probabilities_long_df <- df %>%
  gather(key = "EventType", value = "Probability", all_of(event_types))

# Create a violin plot to show the distribution of probabilities for each event type across all runs
ggplot(probabilities_long_df, aes(x = EventType, y = Probability, fill = EventType)) +
  geom_violin(trim = FALSE, alpha = 0.7) +  # Violin plot without trimming tails
  scale_fill_brewer(palette = "Set3") +  # Use a color palette for differentiation
  labs(title = "Distribution of Probabilities for Each Event Type Across Runs", 
       x = "Event Type", 
       y = "Probability") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "none")  # Remove the legend for clarity








library(tidyr)
library(dplyr)
library(ggplot2)

# Load the data
file_path <- "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_1_0_10000_results/summary_output.txt"
df <- read.table(file_path, header = TRUE, sep = "\t")

# Event types for probabilities
event_types <- c("Prob_1_Event", "Prob_2_Events", "Prob_3_Events", "Prob_4_Events", "Prob_5_Events", "Prob_6_Events")

# Reshape the data into a long format
probabilities_long_df <- df %>%
  gather(key = "EventType", value = "Probability", all_of(event_types))

# Create a violin plot to show the distribution of probabilities for each event type across all runs
ggplot(probabilities_long_df, aes(x = EventType, y = Probability, fill = EventType)) +
  geom_violin(trim = FALSE, scale = "width", adjust = 1.5, width = 0.8, alpha = 0.7) +  # Adjust width and scale
  scale_fill_brewer(palette = "Set3") +  # Use a color palette for differentiation
  labs(title = "Distribution of Probabilities for Each Event Type Across Runs", 
       x = "Event Type", 
       y = "Probability") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "none")  # Remove the legend for clarity










install.packages("ggside")
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggside)  # Load the ggside package for half violin plots

# Load the data
file_path <- "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_1_0_10000_results/summary_output.txt"
df <- read.table(file_path, header = TRUE, sep = "\t")

# Event types for probabilities
event_types <- c("Prob_1_Event", "Prob_2_Events", "Prob_3_Events", "Prob_4_Events", "Prob_5_Events", "Prob_6_Events")

# Reshape the data into a long format
probabilities_long_df <- df %>%
  gather(key = "EventType", value = "Probability", all_of(event_types))

# Create a half violin plot to show the distribution of probabilities for each event type across all runs
ggplot(probabilities_long_df, aes(x = EventType, y = Probability, fill = EventType)) +
  geom_half_violin(trim = FALSE, side = "l", adjust = 1.5, alpha = 0.7) +  # Half violin plot, left side
  scale_fill_brewer(palette = "Set3") +  # Use a color palette for differentiation
  labs(title = "Distribution of Probabilities for Each Event Type Across Runs", 
       x = "Event Type", 
       y = "Probability") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "none")  # Remove the legend for clarity










library(tidyr)
library(dplyr)
library(ggplot2)

# Load the data
file_path <- "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_1_0_10000_results/summary_output.txt"
df <- read.table(file_path, header = TRUE, sep = "\t")

# Event types for probabilities
event_types <- c("Prob_1_Event", "Prob_2_Events", "Prob_3_Events", "Prob_4_Events", "Prob_5_Events", "Prob_6_Events")

# Reshape the data into a long format
probabilities_long_df <- df %>%
  gather(key = "EventType", value = "Probability", all_of(event_types))

# Create a half violin plot to show the distribution of probabilities for each event type across all runs
ggplot(probabilities_long_df, aes(x = EventType, y = Probability, fill = EventType)) +
  geom_violin(trim = FALSE, adjust = 1.5, alpha = 0.7) +  # Full violin plot
  coord_flip() +  # Flip coordinates to make it vertical
  scale_fill_brewer(palette = "Set3") +  # Use a color palette for differentiation
  labs(title = "Distribution of Probabilities for Each Event Type Across Runs", 
       x = "Event Type", 
       y = "Probability") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "none") +
  xlim(c(-0.5, NA))  # Clip to half violin








library(tidyr)
library(dplyr)
library(ggplot2)

# Load the data
file_path <- "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_1_0_10000_results/summary_output.txt"
df <- read.table(file_path, header = TRUE, sep = "\t")

# Event types for probabilities
event_types <- c("Prob_1_Event", "Prob_2_Events", "Prob_3_Events", "Prob_4_Events", "Prob_5_Events", "Prob_6_Events")

# Reshape the data into a long format
probabilities_long_df <- df %>%
  gather(key = "EventType", value = "Probability", all_of(event_types))










library(tidyr)
library(dplyr)
library(ggplot2)

# Load the data
file_path <- "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_1_0_10000_results/summary_output.txt"
df <- read.table(file_path, header = TRUE, sep = "\t")

# Event types for probabilities
event_types <- c("Prob_1_Event", "Prob_2_Events", "Prob_3_Events", "Prob_4_Events", "Prob_5_Events", "Prob_6_Events")

# Reshape the data into a long format
probabilities_long_df <- df %>%
  gather(key = "EventType", value = "Probability", all_of(event_types))

# Create a half violin plot using width adjustment and coordinate clipping
ggplot(probabilities_long_df, aes(x = EventType, y = Probability, fill = EventType)) +
  geom_violin(trim = FALSE, adjust = 1.5, alpha = 0.7, scale = "width") +
  scale_fill_brewer(palette = "Set3") +  # Use a color palette for differentiation
  labs(title = "Distribution of Probabilities for Each Event Type Across Runs", 
       x = "Event Type", 
       y = "Probability") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "none") +
  coord_cartesian(ylim = c(0, 1), xlim = c(0.5, length(event_types) + 0.5))  # Clip to show half of the violin








install.packages("ggridges")

library(tidyr)
library(dplyr)
library(ggplot2)
library(ggridges)

# Load the data
file_path <- "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_1_0_10000_results/summary_output.txt"
df <- read.table(file_path, header = TRUE, sep = "\t")

# Event types for probabilities
event_types <- c("Prob_1_Event", "Prob_2_Events", "Prob_3_Events", "Prob_4_Events", "Prob_5_Events", "Prob_6_Events")

# Reshape the data into a long format
probabilities_long_df <- df %>%
  gather(key = "EventType", value = "Probability", all_of(event_types))

# Create a ridgeline plot to show the distribution of probabilities for each event type across all runs
ggplot(probabilities_long_df, aes(x = Probability, y = EventType, fill = EventType)) +
  geom_density_ridges(alpha = 0.7, scale = 2) +
  scale_fill_brewer(palette = "Set3") +  # Use a color palette for differentiation
  labs(title = "Ridgeline Plot of Probabilities for Each Event Type Across Runs", 
       x = "Probability", 
       y = "Event Type") +
  theme_minimal() +
  theme(legend.position = "none")








library(tidyr)
library(dplyr)
library(ggplot2)
library(ggridges)

# Load the data
file_path <- "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_1_0_10000_results/summary_output.txt"
df <- read.table(file_path, header = TRUE, sep = "\t")

# Event types for probabilities
event_types <- c("Prob_1_Event", "Prob_2_Events", "Prob_3_Events", "Prob_4_Events", "Prob_5_Events", "Prob_6_Events")

# Reshape the data into a long format
probabilities_long_df <- df %>%
  gather(key = "EventType", value = "Probability", all_of(event_types))

# Create a ridgeline plot with adjusted settings for better visibility
ggplot(probabilities_long_df, aes(x = Probability, y = EventType, fill = EventType)) +
  geom_density_ridges(alpha = 0.8, scale = 10, rel_min_height = 0.01, color = "black", size = 0.25) +
  scale_fill_brewer(palette = "Set3") +  # Use a color palette for differentiation
  labs(title = "Ridgeline Plot of Probabilities for Each Event Type Across Runs", 
       x = "Probability", 
       y = "Event Type") +
  theme_minimal() +
  theme(legend.position = "none")














library(tidyr)
library(dplyr)
library(ggplot2)
library(ggridges)

# Load the data
file_path <- "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_3_10000_10000_results/summary_output.txt"
df <- read.table(file_path, header = TRUE, sep = "\t")

# Event types for probabilities
event_types <- c("Prob_1_Event", "Prob_2_Events", "Prob_3_Events", "Prob_4_Events", "Prob_5_Events", "Prob_6_Events")

# Reshape the data into a long format
probabilities_long_df <- df %>%
  gather(key = "EventType", value = "Probability", all_of(event_types))

# Create a ridgeline plot with adjusted settings for better visibility
ggplot(probabilities_long_df, aes(x = Probability, y = EventType, fill = EventType)) +
  geom_density_ridges(alpha = 0.8, scale = 10, rel_min_height = 0.01, color = "black", size = 0.25) +
  scale_fill_brewer(palette = "Set3") +  # Use a color palette for differentiation
  labs(title = "Ridgeline Plot of Probabilities for Each Event Type Across Runs", 
       x = "Probability", 
       y = "Event Type") +
  theme_minimal() +
  theme(legend.position = "none")








library(tidyr)
library(dplyr)
library(ggplot2)
library(ggridges)

# Load the data
file_path <- "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_3_10000_10000_results/summary_output.txt"
df <- read.table(file_path, header = TRUE, sep = "\t")

# Event types for probabilities
event_types <- c("Prob_1_Event", "Prob_2_Events", "Prob_3_Events", "Prob_4_Events", "Prob_5_Events", "Prob_6_Events")

# Reshape the data into a long format
probabilities_long_df <- df %>%
  gather(key = "EventType", value = "Probability", all_of(event_types))

# Create a ridgeline plot with only "Prob_3_Events" highlighted in a different color
ggplot(probabilities_long_df, aes(x = Probability, y = EventType, fill = EventType == "Prob_3_Events")) +
  geom_density_ridges(alpha = 0.8, scale = 10, rel_min_height = 0.01, color = "black", size = 0.25) +
  scale_fill_manual(values = c("gray", "red")) +  # Gray for others, red for "Prob_3_Events"
  labs(title = "Ridgeline Plot of Probabilities for Each Event Type Across Runs", 
       x = "Probability", 
       y = "Event Type") +
  theme_minimal() +
  theme(legend.position = "none")







library(tidyr)
library(dplyr)
library(ggplot2)
library(ggridges)

# Load the data
file_path <- "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_3_10000_10000_results/summary_output.txt"
df <- read.table(file_path, header = TRUE, sep = "\t")

# Event types for probabilities
event_types <- c("Prob_1_Event", "Prob_2_Events", "Prob_3_Events", "Prob_4_Events", "Prob_5_Events", "Prob_6_Events")

# Reshape the data into a long format
probabilities_long_df <- df %>%
  gather(key = "EventType", value = "Probability", all_of(event_types))

# Create a ridgeline plot with only "Prob_3_Events" highlighted in a different color and no gridlines
ggplot(probabilities_long_df, aes(x = Probability, y = EventType, fill = EventType == "Prob_3_Events")) +
  geom_density_ridges(alpha = 0.8, scale = 10, rel_min_height = 0.01, color = "black", size = 0.25) +
  scale_fill_manual(values = c("gray", "red")) +  # Gray for others, red for "Prob_3_Events"
  labs(title = "Ridgeline Plot of Probabilities for Each Event Type Across Runs", 
       x = "Probability", 
       y = "Event Type") +
  theme_classic() +
  theme(
    legend.position = "none",  # Remove the legend
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank()   # Remove minor gridlines
  )






library(tidyr)
library(dplyr)
library(ggplot2)
library(ggridges)

# Load the data
file_path <- "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_3_20000_10000_results/summary_output.txt"
df <- read.table(file_path, header = TRUE, sep = "\t")

# Event types for probabilities
event_types <- c("Prob_1_Event", "Prob_2_Events", "Prob_3_Events", "Prob_4_Events", "Prob_5_Events", "Prob_6_Events")

# Reshape the data into a long format
probabilities_long_df <- df %>%
  gather(key = "EventType", value = "Probability", all_of(event_types))

# Create a ridgeline plot with only "Prob_3_Events" highlighted in a different color and no gridlines
ggplot(probabilities_long_df, aes(x = Probability, y = EventType, fill = EventType == "Prob_3_Events")) +
  geom_density_ridges(alpha = 0.8, scale = 10, rel_min_height = 0.01, color = "black", size = 0.25) +
  scale_fill_manual(values = c("gray", "red")) +  # Gray for others, red for "Prob_3_Events"
  labs(title = "Ridgeline Plot of Probabilities for Each Event Type Across Runs", 
       x = "Probability", 
       y = "Event Type") +
  theme_classic() +
  theme(
    legend.position = "none",  # Remove the legend
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank()   # Remove minor gridlines
  )








library(tidyr)
library(dplyr)
library(ggplot2)
library(ggridges)

# Load the data
file_path <- "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_3_40000_10000_results/summary_output.txt"
df <- read.table(file_path, header = TRUE, sep = "\t")

# Event types for probabilities
event_types <- c("Prob_1_Event", "Prob_2_Events", "Prob_3_Events", "Prob_4_Events", "Prob_5_Events", "Prob_6_Events")

# Reshape the data into a long format
probabilities_long_df <- df %>%
  gather(key = "EventType", value = "Probability", all_of(event_types))

# Create a ridgeline plot with only "Prob_3_Events" highlighted in a different color and no gridlines
ggplot(probabilities_long_df, aes(x = Probability, y = EventType, fill = EventType == "Prob_3_Events")) +
  geom_density_ridges(alpha = 0.8, scale = 10, rel_min_height = 0.01, color = "black", size = 0.25) +
  scale_fill_manual(values = c("gray", "red")) +  # Gray for others, red for "Prob_3_Events"
  labs(title = "Ridgeline Plot of Probabilities for Each Event Type Across Runs", 
       x = "Probability", 
       y = "Event Type") +
  theme_classic() +
  theme(
    legend.position = "none",  # Remove the legend
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank()   # Remove minor gridlines
  )







library(tidyr)
library(dplyr)
library(ggplot2)
library(ggridges)

# Load the data
file_path <- "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_3_80000_10000_results/summary_output.txt"
df <- read.table(file_path, header = TRUE, sep = "\t")

# Event types for probabilities
event_types <- c("Prob_1_Event", "Prob_2_Events", "Prob_3_Events", "Prob_4_Events", "Prob_5_Events", "Prob_6_Events")

# Reshape the data into a long format
probabilities_long_df <- df %>%
  gather(key = "EventType", value = "Probability", all_of(event_types))

# Create a ridgeline plot with only "Prob_3_Events" highlighted in a different color and no gridlines
ggplot(probabilities_long_df, aes(x = Probability, y = EventType, fill = EventType == "Prob_3_Events")) +
  geom_density_ridges(alpha = 0.8, scale = 10, rel_min_height = 0.01, color = "black", size = 0.25) +
  scale_fill_manual(values = c("gray90", "red")) +  # Gray for others, red for "Prob_3_Events"
  labs(title = "Ridgeline Plot of Probabilities for Each Event Type Across Runs", 
       x = "Probability", 
       y = "Event Type") +
  theme_bw() +
  theme(
    legend.position = "none" # Remove the legend
  # Remove minor gridlines
  )










library(tidyr)
library(dplyr)
library(ggplot2)
library(ggridges)

# Load the data
file_path <- "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_3_10000_10000_results/summary_output.txt"
df <- read.table(file_path, header = TRUE, sep = "\t")

# Event types for probabilities
event_types <- c("Prob_1_Event", "Prob_2_Events", "Prob_3_Events", "Prob_4_Events", "Prob_5_Events", "Prob_6_Events")

# Reshape the data into a long format
probabilities_long_df <- df %>%
  gather(key = "EventType", value = "Probability", all_of(event_types))

# Create a ridgeline plot with only "Prob_3_Events" highlighted in a different color
ggplot(probabilities_long_df, aes(x = Probability, y = EventType, fill = EventType == "Prob_3_Events")) +
  geom_density_ridges(alpha = 0.8, scale = 10, rel_min_height = 0.01, color = "black", size = 0.25) +
  scale_fill_manual(values = c("gray", "red")) +  # Gray for others, red for "Prob_3_Events"
  labs(title = "Ridgeline Plot of Probabilities for Each Event Type Across Runs", 
       x = "Probability", 
       y = "Event Type") +
  theme_minimal() +
  theme(
    legend.position = "none"
  )







library(tidyr)
library(dplyr)
library(ggplot2)
library(ggridges)

# Load the data
file_path <- "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_3_10000_10000_results/summary_output.txt"
df <- read.table(file_path, header = TRUE, sep = "\t")

# Event types for probabilities
event_types <- c("Prob_1_Event", "Prob_2_Events", "Prob_3_Events", "Prob_4_Events", "Prob_5_Events", "Prob_6_Events")

# Reshape the data into a long format
probabilities_long_df <- df %>%
  gather(key = "EventType", value = "Probability", all_of(event_types))

# Create a ridgeline plot with only "Prob_3_Events" highlighted in a different color
ggplot(probabilities_long_df, aes(x = Probability, y = EventType, fill = EventType == "Prob_3_Events")) +
  geom_density_ridges(alpha = 0.8, scale = 10, rel_min_height = 0.01, color = "black", size = 0.25) +
  scale_fill_manual(values = c("gray", "red")) +  # Gray for others, red for "Prob_3_Events"
  labs(title = "Ridgeline Plot of Probabilities for Each Event Type Across Runs", 
       x = "Probability", 
       y = "Event Type") +
  theme_minimal() +
  theme(
    legend.position = "none"  # Remove the legend
  )









library(tidyr)
library(dplyr)
library(ggplot2)
library(ggridges)

# Load the data
file_path <- "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_3_160000_10000_results/summary_output.txt"
df <- read.table(file_path, header = TRUE, sep = "\t")

# Event types for probabilities
event_types <- c("Prob_1_Event", "Prob_2_Events", "Prob_3_Events", "Prob_4_Events", "Prob_5_Events", "Prob_6_Events")

# Reshape the data into a long format
probabilities_long_df <- df %>%
  gather(key = "EventType", value = "Probability", all_of(event_types))

# Create a ridgeline plot with only "Prob_3_Events" highlighted in a different color
ggplot(probabilities_long_df, aes(x = Probability, y = EventType, fill = EventType == "Prob_3_Events")) +
  geom_density_ridges(alpha = 0.8, scale = 10, rel_min_height = 0.01, color = "black", size = 0.25) +
  scale_fill_manual(values = c("gray", "red")) +  # Gray for others, red for "Prob_3_Events"
  labs(title = "Ridgeline Plot of Probabilities for Each Event Type Across Runs", 
       x = "Probability", 
       y = "Event Type") +
  theme_minimal() +
  theme(
    legend.position = "none"  # Legend is removed, but gridlines are kept
  )







library(tidyr)
library(dplyr)
library(ggplot2)
library(ggridges)

# Load the data
file_path <- "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_3_10000_10000_results/summary_output.txt"
df <- read.table(file_path, header = TRUE, sep = "\t")

# Event types for probabilities
event_types <- c("Prob_1_Event", "Prob_2_Events", "Prob_3_Events", "Prob_4_Events", "Prob_5_Events", "Prob_6_Events")

# Reshape the data into a long format
probabilities_long_df <- df %>%
  gather(key = "EventType", value = "Probability", all_of(event_types))

# Create a ridgeline plot with only "Prob_3_Events" highlighted in a different color
ggplot(probabilities_long_df, aes(x = Probability, y = EventType, fill = EventType == "Prob_3_Events")) +
  geom_density_ridges(alpha = 0.8, scale = 10, rel_min_height = 0.01, color = "black", size = 0.25) +
  scale_fill_manual(values = c("gray", "red")) +  # Gray for others, red for "Prob_3_Events"
  labs(title = "Ridgeline Plot of Probabilities for Each Event Type Across Runs", 
       x = "Probability", 
       y = "Event Type") +
  theme_minimal() +
  theme(
    legend.position = "none"  # Legend is removed, but gridlines are kept
  ) +
  coord_cartesian(xlim = c(-0.09, 1))  # Set x-axis limits from 0 to 1







library(tidyr)
library(dplyr)
library(ggplot2)
library(ggridges)

# Load the data
file_path <- "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_3_10000_10000_results/summary_output.txt"
df <- read.table(file_path, header = TRUE, sep = "\t")

# Event types for probabilities
event_types <- c("Prob_1_Event", "Prob_2_Events", "Prob_3_Events", "Prob_4_Events", "Prob_5_Events", "Prob_6_Events")

# Reshape the data into a long format
probabilities_long_df <- df %>%
  gather(key = "EventType", value = "Probability", all_of(event_types))

# Find the event type with the highest maximum probability
highest_prob_event <- probabilities_long_df %>%
  group_by(EventType) %>%
  summarize(max_prob = max(Probability)) %>%
  filter(max_prob == max(max_prob)) %>%
  pull(EventType)

# Create a ridgeline plot with the event type with the highest max probability highlighted in blue
ggplot(probabilities_long_df, aes(x = Probability, y = EventType, fill = EventType == highest_prob_event)) +
  geom_density_ridges(alpha = 0.8, scale = 10, rel_min_height = 0.01, color = "black", size = 0.25) +
  scale_fill_manual(values = c("gray", "lightblue")) +  # Gray for others, blue for the highest probability event
  labs(title = "Ridgeline Plot of Probabilities for Each Event Type Across Runs", 
       x = "Probability", 
       y = "Event Type") +
  theme_minimal() +
  theme(
    legend.position = "none"  # Legend is removed, but gridlines are kept
  ) +
  coord_cartesian(xlim = c(0, 1))  # Set x-axis limits from 0 to 1








library(tidyr)
library(dplyr)
library(ggplot2)
library(ggridges)

# Load the data
file_path <- "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_3_10000_10000_results/summary_output.txt"
df <- read.table(file_path, header = TRUE, sep = "\t")

# Event types for probabilities
event_types <- c("Prob_1_Event", "Prob_2_Events", "Prob_3_Events", "Prob_4_Events", "Prob_5_Events", "Prob_6_Events")

# Reshape the data into a long format
probabilities_long_df <- df %>%
  gather(key = "EventType", value = "Probability", all_of(event_types))

# Find the event type with the highest maximum probability
highest_prob_event <- probabilities_long_df %>%
  group_by(EventType) %>%
  summarize(max_prob = max(Probability)) %>%
  filter(max_prob == max(max_prob)) %>%
  pull(EventType)

# Create a ridgeline plot with the true event ("Prob_3_Events") in red and the highest in blue
ggplot(probabilities_long_df, aes(x = Probability, y = EventType, fill = EventType)) +
  geom_density_ridges(alpha = 0.8, scale = 10, rel_min_height = 0.01, color = "black", size = 0.25) +
  scale_fill_manual(values = c("Prob_3_Events" = "red", 
                               highest_prob_event = "blue", 
                               "gray")) +  # Gray for others
                                 labs(title = "Ridgeline Plot of Probabilities for Each Event Type Across Runs", 
                                      x = "Probability", 
                                      y = "Event Type") +
  theme_minimal() +
  theme(
    legend.position = "none"  # Legend is removed, but gridlines are kept
  ) +
  coord_cartesian(xlim = c(0, 1))  # Set x-axis limits from 0 to 1





####################################
################################
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggridges)

# Load the data
file_path <- "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_6_160000_10000_results/summary_output.txt"
df <- read.table(file_path, header = TRUE, sep = "\t")

# Event types for probabilities
event_types <- c("Prob_1_Event", "Prob_2_Events", "Prob_3_Events", "Prob_4_Events", "Prob_5_Events", "Prob_6_Events")

# Reshape the data into a long format
probabilities_long_df <- df %>%
  gather(key = "EventType", value = "Probability", all_of(event_types))

# Find the event type with the highest maximum probability
highest_prob_event <- probabilities_long_df %>%
  group_by(EventType) %>%
  summarize(max_prob = max(Probability)) %>%
  filter(max_prob == max(max_prob)) %>%
  pull(EventType)

# Create a new column to define colors for each event type
probabilities_long_df <- probabilities_long_df %>%
  mutate(Color = case_when(
    EventType == "Prob_6_Events" ~ "orange",
    EventType == highest_prob_event ~ "lightblue",
    TRUE ~ "gray"
  ))

# Create a ridgeline plot with the true event ("Prob_3_Events") in red and the highest in blue
ggplot(probabilities_long_df, aes(x = Probability, y = EventType, fill = Color)) +
  geom_density_ridges(alpha = 0.7, scale = 10, rel_min_height = 0.01, color = "black", size = 0.25) +
  scale_fill_identity() +  # Use the colors defined in the dataframe directly
  labs(title = "Ridgeline Plot of Probabilities for Each Event Type Across Runs", 
       x = "Probability", 
       y = "Event Type") +
  theme_minimal() +
  theme(
    legend.position = "none"  # Legend is removed, but gridlines are kept
  ) +
  coord_cartesian(xlim = c(-0.08, 1))  # Set x-axis limits from 0 to 1




#########################
########################
library(tidyr)
library(dplyr)
library(ggplot2)
library(scales)  # For formatting axis labels

# Load the summary file
file_path <- "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_6_160000_10000_results/summary_output.txt"

df_new <- read.table(file_path, header = TRUE, sep = "\t")

# Gather the Mean divergence values into long format
df_new_long_mean <- df_new %>%
  gather(key = "PopPair", value = "MeanDivergence", 
         Mean_Divergence_PopA_PopB:Mean_Divergence_PopK_PopL) %>%
  select(Run, PopPair, MeanDivergence)

# Gather the True divergence values into long format
df_new_long_true <- df_new %>%
  gather(key = "PopPair_True", value = "TrueValue", 
         True_Divergence_PopA_PopB:True_Divergence_PopK_PopL) %>%
  mutate(PopPair = gsub("True_Divergence_", "Mean_Divergence_", PopPair_True)) %>%
  select(Run, PopPair, TrueValue)

# Merge both datasets based on matching population pairs and runs
df_combined_new <- inner_join(df_new_long_mean, df_new_long_true, by = c("Run", "PopPair"))

# Remove duplicates if necessary
df_combined_new_unique <- df_combined_new[!duplicated(df_combined_new[, c("TrueValue", "MeanDivergence")]), ]

# Divide the mean estimated values by 1e-8
df_combined_new_unique$MeanDivergence <- df_combined_new_unique$MeanDivergence / 1e-8

# Filter out rows where MeanDivergence is NA or negative
df_combined_new_unique <- df_combined_new_unique %>%
  filter(!is.na(MeanDivergence) & MeanDivergence >= 0)

# Convert TrueValue to a numeric variable
df_combined_new_unique$TrueValue <- as.numeric(as.character(df_combined_new_unique$TrueValue))

# Set the axis limit
axis_limit <- 900000

# Get unique true divergence values for vertical lines
unique_true_values <- unique(df_combined_new_unique$TrueValue)

# Create the plot with adjusted violin plots
ggplot(df_combined_new_unique, aes(x = TrueValue, y = MeanDivergence, group = PopPair, fill = PopPair)) +
  geom_violin(alpha = 0.7, trim = FALSE, scale = "area", adjust = 3, position = position_identity()) +  # Make violins wider
  geom_jitter(width = 0.4, alpha = 0.4, size = 3, color = "black") +  # Add jittered points for visibility
  geom_vline(xintercept = unique_true_values, linetype = "dotted", color = "blue", size = 0.7) +  # Add vertical lines for true divergence times
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +  # Add 1:1 diagonal line
  labs(x = "True Divergence", y = "Estimated Mean Divergence (x 10^-8)", 
       title = "True vs Estimated Divergence (Config 3: 10000_10000)") +
  theme_bw() +
  coord_fixed(ratio = 1) +  # Ensure the aspect ratio is 1:1
  scale_x_continuous(labels = comma, limits = c(0, axis_limit)) +  # Set x-axis limit
  scale_y_continuous(labels = comma, limits = c(0, axis_limit)) +  # Set y-axis limit to match x-axis
  theme(
    legend.position = "none",  # Remove the legend
    axis.title = element_text(size = 16, face ="bold"),  # Increase axis title size
    axis.text = element_text(size = 16, face="bold"),  # Increase axis tick labels size
    plot.title = element_text(size = 0, face = "bold")  # Increase title size
  )












library(tidyr)
library(dplyr)
library(ggplot2)

# Load the summary file
file_path <- "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_6_80000_10000_results/summary_output.txt"
df_new <- read.table(file_path, header = TRUE, sep = "\t")

# Gather the Mean divergence values into long format
df_new_long_mean <- df_new %>%
  gather(key = "PopPair", value = "MeanDivergence", 
         Mean_Divergence_PopA_PopB:Mean_Divergence_PopK_PopL) %>%
  select(Run, PopPair, MeanDivergence)

# Gather the True divergence values into long format
df_new_long_true <- df_new %>%
  gather(key = "PopPair_True", value = "TrueValue", 
         True_Divergence_PopA_PopB:True_Divergence_PopK_PopL) %>%
  mutate(PopPair = gsub("True_Divergence_", "Mean_Divergence_", PopPair_True)) %>%
  select(Run, PopPair, TrueValue)

# Merge both datasets based on matching population pairs and runs
df_combined_new <- inner_join(df_new_long_mean, df_new_long_true, by = c("Run", "PopPair"))

# Remove duplicates if necessary
df_combined_new_unique <- df_combined_new[!duplicated(df_combined_new[, c("TrueValue", "MeanDivergence")]), ]

# Divide the mean estimated values by 1e-8
df_combined_new_unique$MeanDivergence <- df_combined_new_unique$MeanDivergence / 1e-8

# Determine the max limit for the plot axes
max_limit <- max(c(max(df_combined_new_unique$TrueValue), max(df_combined_new_unique$MeanDivergence)))

# Create the plot with violin plots where the x-axis corresponds to TrueValue
ggplot(df_combined_new_unique, aes(x = TrueValue, y = MeanDivergence, group = TrueValue)) +
  geom_violin(fill = "skyblue", alpha = 0.5, trim = FALSE) +  # Create violin plots
  geom_jitter(width = 0.2, alpha = 0.4, size = 1, color = "black") +  # Add jittered points for visibility
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +  # Add 1:1 diagonal line
  labs(x = "True Divergence", y = "Estimated Mean Divergence (x 10^-8)", 
       title = "True vs Estimated Divergence (Violin Plot, Config 6: 80000_10000)") +
  theme_bw() +
  coord_fixed(ratio = 1) +  # Ensure the aspect ratio is 1:1
  xlim(0, max_limit) +  # Set x-axis limit
  ylim(0, max_limit) +  # Set y-axis limit
  theme(legend.position = "none")  # Remove the legend











library(tidyr)
library(dplyr)
library(ggplot2)

# Load the summary file
file_path <- "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_6_80000_10000_results/summary_output.txt"
df_new <- read.table(file_path, header = TRUE, sep = "\t")

# Gather the Mean divergence values into long format
df_new_long_mean <- df_new %>%
  gather(key = "PopPair", value = "MeanDivergence", 
         Mean_Divergence_PopA_PopB:Mean_Divergence_PopK_PopL) %>%
  select(Run, PopPair, MeanDivergence)

# Gather the True divergence values into long format
df_new_long_true <- df_new %>%
  gather(key = "PopPair_True", value = "TrueValue", 
         True_Divergence_PopA_PopB:True_Divergence_PopK_PopL) %>%
  mutate(PopPair = gsub("True_Divergence_", "Mean_Divergence_", PopPair_True)) %>%
  select(Run, PopPair, TrueValue)

# Merge both datasets based on matching population pairs and runs
df_combined_new <- inner_join(df_new_long_mean, df_new_long_true, by = c("Run", "PopPair"))

# Remove duplicates if necessary
df_combined_new_unique <- df_combined_new[!duplicated(df_combined_new[, c("TrueValue", "MeanDivergence")]), ]

# Divide the mean estimated values by 1e-8
df_combined_new_unique$MeanDivergence <- df_combined_new_unique$MeanDivergence / 1e-8

# Determine the max limit for the plot axes
max_limit <- max(c(max(df_combined_new_unique$TrueValue), max(df_combined_new_unique$MeanDivergence)))

# Create the plot with wider violin plots
ggplot(df_combined_new_unique, aes(x = TrueValue, y = MeanDivergence, group = TrueValue)) +
  geom_violin(fill = "skyblue", alpha = 0.5, trim = FALSE, scale = "width") +  # Wider violin plots
  geom_jitter(width = 0.2, alpha = 0.4, size = 1, color = "black") +  # Add jittered points for visibility
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +  # Add 1:1 diagonal line
  labs(x = "True Divergence", y = "Estimated Mean Divergence (x 10^-8)", 
       title = "True vs Estimated Divergence (Violin Plot, Config 6: 80000_10000)") +
  theme_bw() +
  coord_fixed(ratio = 1) +  # Ensure the aspect ratio is 1:1
  xlim(0, max_limit) +  # Set x-axis limit
  ylim(0, max_limit) +  # Set y-axis limit
  theme(legend.position = "none")  # Remove the legend










library(tidyr)
library(dplyr)
library(ggplot2)

# Load the summary file
file_path <- "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_6_160000_10000_results/summary_output.txt"
df_new <- read.table(file_path, header = TRUE, sep = "\t")

# Gather the Mean divergence values into long format
df_new_long_mean <- df_new %>%
  gather(key = "PopPair", value = "MeanDivergence", 
         Mean_Divergence_PopA_PopB:Mean_Divergence_PopK_PopL) %>%
  select(Run, PopPair, MeanDivergence)

# Gather the True divergence values into long format
df_new_long_true <- df_new %>%
  gather(key = "PopPair_True", value = "TrueValue", 
         True_Divergence_PopA_PopB:True_Divergence_PopK_PopL) %>%
  mutate(PopPair = gsub("True_Divergence_", "Mean_Divergence_", PopPair_True)) %>%
  select(Run, PopPair, TrueValue)

# Merge both datasets based on matching population pairs and runs
df_combined_new <- inner_join(df_new_long_mean, df_new_long_true, by = c("Run", "PopPair"))

# Remove duplicates if necessary
df_combined_new_unique <- df_combined_new[!duplicated(df_combined_new[, c("TrueValue", "MeanDivergence")]), ]

# Divide the mean estimated values by 1e-8
df_combined_new_unique$MeanDivergence <- df_combined_new_unique$MeanDivergence / 1e-8

# Determine the max limit for the plot axes
max_limit <- max(c(max(df_combined_new_unique$TrueValue), max(df_combined_new_unique$MeanDivergence)))

# Create the plot with different colors for each violin plot
ggplot(df_combined_new_unique, aes(x = TrueValue, y = MeanDivergence, group = TrueValue, fill = PopPair)) +
  geom_violin(alpha = 0.7, trim = FALSE, scale = "width") +  # Wider violin plots with unique colors
  geom_jitter(width = 0.2, alpha = 0.4, size = 1, color = "black") +  # Add jittered points for visibility
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +  # Add 1:1 diagonal line
  labs(x = "True Divergence", y = "Estimated Mean Divergence (x 10^-8)", 
       title = "True vs Estimated Divergence (Violin Plot, Config 6: 80000_10000)") +
  theme_bw() +
  coord_fixed(ratio = 1) +  # Ensure the aspect ratio is 1:1
  xlim(0, max_limit) +  # Set x-axis limit
  ylim(0, max_limit) +  # Set y-axis limit
  theme(legend.position = "none")  # Remove the legend







library(tidyr)
library(dplyr)
library(ggplot2)
library(see)

# Load the summary file
file_path <- "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_6_80000_10000_results/summary_output.txt"
df_new <- read.table(file_path, header = TRUE, sep = "\t")

# Gather the Mean divergence values into long format
df_new_long_mean <- df_new %>%
  gather(key = "PopPair", value = "MeanDivergence", 
         Mean_Divergence_PopA_PopB:Mean_Divergence_PopK_PopL) %>%
  select(Run, PopPair, MeanDivergence)

# Gather the True divergence values into long format
df_new_long_true <- df_new %>%
  gather(key = "PopPair_True", value = "TrueValue", 
         True_Divergence_PopA_PopB:True_Divergence_PopK_PopL) %>%
  mutate(PopPair = gsub("True_Divergence_", "Mean_Divergence_", PopPair_True)) %>%
  select(Run, PopPair, TrueValue)

# Merge both datasets based on matching population pairs and runs
df_combined_new <- inner_join(df_new_long_mean, df_new_long_true, by = c("Run", "PopPair"))

# Remove duplicates if necessary
df_combined_new_unique <- df_combined_new[!duplicated(df_combined_new[, c("TrueValue", "MeanDivergence")]), ]

# Divide the mean estimated values by 1e-8
df_combined_new_unique$MeanDivergence <- df_combined_new_unique$MeanDivergence / 1e-8

# Determine the max limit for the plot axes
max_limit <- max(c(max(df_combined_new_unique$TrueValue), max(df_combined_new_unique$MeanDivergence)))

# Create the plot with half violin plots
ggplot(df_combined_new_unique, aes(x = TrueValue, y = MeanDivergence, fill = PopPair)) +
  geom_half_violin(alpha = 0.7, trim = FALSE, scale = "width", side = "r") +  # Right side half violin plots
  geom_jitter(width = 0.2, alpha = 0.4, size = 1, color = "black") +  # Add jittered points for visibility
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +  # Add 1:1 diagonal line
  labs(x = "True Divergence", y = "Estimated Mean Divergence (x 10^-8)", 
       title = "True vs Estimated Divergence (Half Violin Plot, Config 6: 80000_10000)") +
  theme_bw() +
  coord_fixed(ratio = 1) +  # Ensure the aspect ratio is 1:1
  xlim(0, max_limit) +  # Set x-axis limit
  ylim(0, max_limit) +  # Set y-axis limit
  theme(legend.position = "none")  # Remove the legend








library(tidyr)
library(dplyr)
library(ggplot2)

# Load the summary file
file_path <- "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_3_10000_10000_results/summary_output.txt"
df_new <- read.table(file_path, header = TRUE, sep = "\t")

# Gather the Mean divergence values into long format
df_new_long_mean <- df_new %>%
  gather(key = "PopPair", value = "MeanDivergence", 
         Mean_Divergence_PopA_PopB:Mean_Divergence_PopK_PopL) %>%
  select(Run, PopPair, MeanDivergence)

# Gather the True divergence values into long format
df_new_long_true <- df_new %>%
  gather(key = "PopPair_True", value = "TrueValue", 
         True_Divergence_PopA_PopB:True_Divergence_PopK_PopL) %>%
  mutate(PopPair = gsub("True_Divergence_", "Mean_Divergence_", PopPair_True)) %>%
  select(Run, PopPair, TrueValue)

# Merge both datasets based on matching population pairs and runs
df_combined_new <- inner_join(df_new_long_mean, df_new_long_true, by = c("Run", "PopPair"))

# Remove duplicates if necessary
df_combined_new_unique <- df_combined_new[!duplicated(df_combined_new[, c("TrueValue", "MeanDivergence")]), ]

# Divide the mean estimated values by 1e-8
df_combined_new_unique$MeanDivergence <- df_combined_new_unique$MeanDivergence / 1e-8

# Determine the max limit for the plot axes
max_limit <- max(c(max(df_combined_new_unique$TrueValue), max(df_combined_new_unique$MeanDivergence)))

# Create the plot with full violin plots
ggplot(df_combined_new_unique, aes(x = TrueValue, y = MeanDivergence, fill = PopPair)) +
  geom_violin(alpha = 0.7, trim = FALSE, scale = "width") +  # Full violin plots with unique colors for each pair
  geom_jitter(width = 0.2, alpha = 0.4, size = 1, color = "black") +  # Add jittered points for visibility
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +  # Add 1:1 diagonal line
  labs(x = "True Divergence", y = "Estimated Mean Divergence (x 10^-8)", 
       title = "True vs Estimated Divergence (Violin Plot, Config 3: 10000_10000)") +
  theme_bw() +
  coord_fixed(ratio = 1) +  # Ensure the aspect ratio is 1:1
  xlim(0, max_limit) +  # Set x-axis limit
  ylim(0, max_limit) +  # Set y-axis limit
  theme(legend.position = "none")  # Remove the legend










library(tidyr)
library(dplyr)
library(ggplot2)

# Load the summary file
file_path <- "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_3_10000_10000_results/summary_output.txt"
df_new <- read.table(file_path, header = TRUE, sep = "\t")

# Gather the Mean divergence values into long format
df_new_long_mean <- df_new %>%
  gather(key = "PopPair", value = "MeanDivergence", 
         Mean_Divergence_PopA_PopB:Mean_Divergence_PopK_PopL) %>%
  select(Run, PopPair, MeanDivergence)

# Gather the True divergence values into long format
df_new_long_true <- df_new %>%
  gather(key = "PopPair_True", value = "TrueValue", 
         True_Divergence_PopA_PopB:True_Divergence_PopK_PopL) %>%
  mutate(PopPair = gsub("True_Divergence_", "Mean_Divergence_", PopPair_True)) %>%
  select(Run, PopPair, TrueValue)

# Merge both datasets based on matching population pairs and runs
df_combined_new <- inner_join(df_new_long_mean, df_new_long_true, by = c("Run", "PopPair"))

# Remove duplicates if necessary
df_combined_new_unique <- df_combined_new[!duplicated(df_combined_new[, c("TrueValue", "MeanDivergence")]), ]

# Divide the mean estimated values by 1e-8
df_combined_new_unique$MeanDivergence <- df_combined_new_unique$MeanDivergence / 1e-8

# Determine the max limit for the plot axes
max_limit <- max(c(max(df_combined_new_unique$TrueValue), max(df_combined_new_unique$MeanDivergence)))

# Create the plot with overlapping violin plots
ggplot(df_combined_new_unique, aes(x = factor(TrueValue), y = MeanDivergence, fill = PopPair)) +
  geom_violin(alpha = 0.7, trim = FALSE, scale = "width", position = position_identity()) +  # Allow overlapping violins
  geom_jitter(width = 0.2, alpha = 0.4, size = 1, color = "black") +  # Add jittered points for visibility
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +  # Add 1:1 diagonal line
  labs(x = "True Divergence", y = "Estimated Mean Divergence (x 10^-8)", 
       title = "True vs Estimated Divergence (Violin Plot, Config 3: 10000_10000)") +
  theme_bw() +
  coord_fixed(ratio = 1) +  # Ensure the aspect ratio is 1:1
  ylim(0, max_limit) +  # Set y-axis limit
  theme(legend.position = "none")  # Remove the legend








library(tidyr)
library(dplyr)
library(ggplot2)

# Load the summary file
file_path <- "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_3_10000_10000_results/summary_output.txt"
df_new <- read.table(file_path, header = TRUE, sep = "\t")

# Gather the Mean divergence values into long format
df_new_long_mean <- df_new %>%
  gather(key = "PopPair", value = "MeanDivergence", 
         Mean_Divergence_PopA_PopB:Mean_Divergence_PopK_PopL) %>%
  select(Run, PopPair, MeanDivergence)

# Gather the True divergence values into long format
df_new_long_true <- df_new %>%
  gather(key = "PopPair_True", value = "TrueValue", 
         True_Divergence_PopA_PopB:True_Divergence_PopK_PopL) %>%
  mutate(PopPair = gsub("True_Divergence_", "Mean_Divergence_", PopPair_True)) %>%
  select(Run, PopPair, TrueValue)

# Merge both datasets based on matching population pairs and runs
df_combined_new <- inner_join(df_new_long_mean, df_new_long_true, by = c("Run", "PopPair"))

# Remove duplicates if necessary
df_combined_new_unique <- df_combined_new[!duplicated(df_combined_new[, c("TrueValue", "MeanDivergence")]), ]

# Divide the mean estimated values by 1e-8
df_combined_new_unique$MeanDivergence <- df_combined_new_unique$MeanDivergence / 1e-8

# Determine the max limit for the plot axes
max_limit <- max(c(max(df_combined_new_unique$TrueValue), max(df_combined_new_unique$MeanDivergence)))

# Create the plot with overlapping violin plots
ggplot(df_combined_new_unique, aes(x = as.factor(TrueValue), y = MeanDivergence, fill = PopPair)) +
  geom_violin(alpha = 0.7, trim = FALSE, scale = "area", position = position_dodge(width = 0.5)) +  # Full violins that overlap correctly
  geom_jitter(width = 0.2, alpha = 0.4, size = 1, color = "black") +  # Add jittered points for visibility
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +  # Add 1:1 diagonal line
  labs(x = "True Divergence", y = "Estimated Mean Divergence (x 10^-8)", 
       title = "True vs Estimated Divergence (Violin Plot, Config 3: 10000_10000)") +
  theme_bw() +
  coord_fixed(ratio = 1) +  # Ensure the aspect ratio is 1:1
  ylim(0, max_limit) +  # Set y-axis limit
  theme(legend.position = "none")  # Remove the legend









library(tidyr)
library(dplyr)
library(ggplot2)

# Load the summary file
file_path <- "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_3_10000_10000_results/summary_output.txt"
df_new <- read.table(file_path, header = TRUE, sep = "\t")

# Gather the Mean divergence values into long format
df_new_long_mean <- df_new %>%
  gather(key = "PopPair", value = "MeanDivergence", 
         Mean_Divergence_PopA_PopB:Mean_Divergence_PopK_PopL) %>%
  select(Run, PopPair, MeanDivergence)

# Gather the True divergence values into long format
df_new_long_true <- df_new %>%
  gather(key = "PopPair_True", value = "TrueValue", 
         True_Divergence_PopA_PopB:True_Divergence_PopK_PopL) %>%
  mutate(PopPair = gsub("True_Divergence_", "Mean_Divergence_", PopPair_True)) %>%
  select(Run, PopPair, TrueValue)

# Merge both datasets based on matching population pairs and runs
df_combined_new <- inner_join(df_new_long_mean, df_new_long_true, by = c("Run", "PopPair"))

# Remove duplicates if necessary
df_combined_new_unique <- df_combined_new[!duplicated(df_combined_new[, c("TrueValue", "MeanDivergence")]), ]

# Divide the mean estimated values by 1e-8
df_combined_new_unique$MeanDivergence <- df_combined_new_unique$MeanDivergence / 1e-8

# Determine the max limit for the plot axes
max_limit <- max(c(max(df_combined_new_unique$TrueValue), max(df_combined_new_unique$MeanDivergence)))

# Create the plot with properly overlapping violin plots
ggplot(df_combined_new_unique, aes(x = TrueValue, y = MeanDivergence, group = PopPair, fill = PopPair)) +
  geom_violin(alpha = 0.7, trim = FALSE, scale = "width") +  # Full violins without dodging
  geom_jitter(width = 0.2, alpha = 0.4, size = 1, color = "black") +  # Add jittered points for visibility
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +  # Add 1:1 diagonal line
  labs(x = "True Divergence", y = "Estimated Mean Divergence (x 10^-8)", 
       title = "True vs Estimated Divergence (Violin Plot, Config 3: 10000_10000)") +
  theme_bw() +
  coord_fixed(ratio = 1) +  # Ensure the aspect ratio is 1:1
  xlim(0, max_limit) +  # Set x-axis limit
  ylim(0, max_limit) +  # Set y-axis limit
  theme(legend.position = "none")  # Remove the legend









library(tidyr)
library(dplyr)
library(ggplot2)
library(ggridges)

# Load the summary file
file_path <- "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_3_10000_10000_results/summary_output.txt"
df_new <- read.table(file_path, header = TRUE, sep = "\t")

# Gather the Mean divergence values into long format
df_new_long_mean <- df_new %>%
  gather(key = "PopPair", value = "MeanDivergence", 
         Mean_Divergence_PopA_PopB:Mean_Divergence_PopK_PopL) %>%
  select(Run, PopPair, MeanDivergence)

# Gather the True divergence values into long format
df_new_long_true <- df_new %>%
  gather(key = "PopPair_True", value = "TrueValue", 
         True_Divergence_PopA_PopB:True_Divergence_PopK_PopL) %>%
  mutate(PopPair = gsub("True_Divergence_", "Mean_Divergence_", PopPair_True)) %>%
  select(Run, PopPair, TrueValue)

# Merge both datasets based on matching population pairs and runs
df_combined_new <- inner_join(df_new_long_mean, df_new_long_true, by = c("Run", "PopPair"))

# Remove duplicates if necessary
df_combined_new_unique <- df_combined_new[!duplicated(df_combined_new[, c("TrueValue", "MeanDivergence")]), ]

# Divide the mean estimated values by 1e-8
df_combined_new_unique$MeanDivergence <- df_combined_new_unique$MeanDivergence / 1e-8

# Create the ridgeline plot
ggplot(df_combined_new_unique, aes(x = MeanDivergence, y = as.factor(TrueValue), fill = PopPair)) +
  geom_density_ridges(scale = 1, alpha = 0.7, rel_min_height = 0.01) +
  labs(x = "Estimated Mean Divergence (x 10^-8)", y = "True Divergence", 
       title = "Ridgeline Plot of Estimated vs. True Divergence") +
  theme_minimal() +
  theme(legend.position = "none")









library(tidyr)
library(dplyr)
library(ggplot2)
library(ggridges)

# Load the summary file
file_path <- "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_3_10000_10000_results/summary_output.txt"
df_new <- read.table(file_path, header = TRUE, sep = "\t")

# Gather the Mean divergence values into long format
df_new_long_mean <- df_new %>%
  gather(key = "PopPair", value = "MeanDivergence", 
         Mean_Divergence_PopA_PopB:Mean_Divergence_PopK_PopL) %>%
  select(Run, PopPair, MeanDivergence)

# Gather the True divergence values into long format
df_new_long_true <- df_new %>%
  gather(key = "PopPair_True", value = "TrueValue", 
         True_Divergence_PopA_PopB:True_Divergence_PopK_PopL) %>%
  mutate(PopPair = gsub("True_Divergence_", "Mean_Divergence_", PopPair_True)) %>%
  select(Run, PopPair, TrueValue)

# Merge both datasets based on matching population pairs and runs
df_combined_new <- inner_join(df_new_long_mean, df_new_long_true, by = c("Run", "PopPair"))

# Remove duplicates if necessary
df_combined_new_unique <- df_combined_new[!duplicated(df_combined_new[, c("TrueValue", "MeanDivergence")]), ]

# Divide the mean estimated values by 1e-8
df_combined_new_unique$MeanDivergence <- df_combined_new_unique$MeanDivergence / 1e-8

# Create the ridgeline plot with TrueValue on the x-axis
ggplot(df_combined_new_unique, aes(x = TrueValue, y = MeanDivergence, fill = PopPair)) +
  geom_density_ridges(scale = 1, alpha = 0.7, rel_min_height = 0.01) +
  labs(x = "True Divergence", y = "Estimated Mean Divergence (x 10^-8)", 
       title = "Ridgeline Plot of Estimated vs. True Divergence") +
  theme_minimal() +
  theme(legend.position = "none")








library(tidyr)
library(dplyr)
library(ggplot2)
library(ggridges)

# Load the summary file
file_path <- "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_3_10000_10000_results/summary_output.txt"
df_new <- read.table(file_path, header = TRUE, sep = "\t")

# Gather the Mean divergence values into long format
df_new_long_mean <- df_new %>%
  gather(key = "PopPair", value = "MeanDivergence", 
         Mean_Divergence_PopA_PopB:Mean_Divergence_PopK_PopL) %>%
  select(Run, PopPair, MeanDivergence)

# Gather the True divergence values into long format
df_new_long_true <- df_new %>%
  gather(key = "PopPair_True", value = "TrueValue", 
         True_Divergence_PopA_PopB:True_Divergence_PopK_PopL) %>%
  mutate(PopPair = gsub("True_Divergence_", "Mean_Divergence_", PopPair_True)) %>%
  select(Run, PopPair, TrueValue)

# Merge both datasets based on matching population pairs and runs
df_combined_new <- inner_join(df_new_long_mean, df_new_long_true, by = c("Run", "PopPair"))

# Remove duplicates if necessary
df_combined_new_unique <- df_combined_new[!duplicated(df_combined_new[, c("TrueValue", "MeanDivergence")]), ]

# Divide the mean estimated values by 1e-8
df_combined_new_unique$MeanDivergence <- df_combined_new_unique$MeanDivergence / 1e-8

# Create the ridgeline plot with MeanDivergence as y-axis and TrueValue as x-axis
ggplot(df_combined_new_unique, aes(x = TrueValue, y = as.factor(PopPair), height = MeanDivergence, fill = PopPair, group = PopPair)) +
  geom_density_ridges(stat = "identity", scale = 0.9, alpha = 0.7) +
  labs(x = "True Divergence", y = "Population Pair", 
       title = "Ridgeline Plot of Estimated vs. True Divergence") +
  theme_minimal() +
  theme(legend.position = "none")











library(tidyr)
library(dplyr)
library(ggplot2)

# Load the summary file
file_path <- "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_3_10000_10000_results/summary_output.txt"
df_new <- read.table(file_path, header = TRUE, sep = "\t")

# Gather the Mean divergence values into long format
df_new_long_mean <- df_new %>%
  gather(key = "PopPair", value = "MeanDivergence", 
         Mean_Divergence_PopA_PopB:Mean_Divergence_PopK_PopL) %>%
  select(Run, PopPair, MeanDivergence)

# Gather the True divergence values into long format
df_new_long_true <- df_new %>%
  gather(key = "PopPair_True", value = "TrueValue", 
         True_Divergence_PopA_PopB:True_Divergence_PopK_PopL) %>%
  mutate(PopPair = gsub("True_Divergence_", "Mean_Divergence_", PopPair_True)) %>%
  select(Run, PopPair, TrueValue)

# Merge both datasets based on matching population pairs and runs
df_combined_new <- inner_join(df_new_long_mean, df_new_long_true, by = c("Run", "PopPair"))

# Remove duplicates if necessary
df_combined_new_unique <- df_combined_new[!duplicated(df_combined_new[, c("TrueValue", "MeanDivergence")]), ]

# Divide the mean estimated values by 1e-8
df_combined_new_unique$MeanDivergence <- df_combined_new_unique$MeanDivergence / 1e-8

# Determine the max limit for the plot axes
max_limit <- max(c(max(df_combined_new_unique$TrueValue), max(df_combined_new_unique$MeanDivergence)))

# Create the plot with properly overlapping violin plots
ggplot(df_combined_new_unique, aes(x = as.factor(TrueValue), y = MeanDivergence, fill = PopPair)) +
  geom_violin(alpha = 0.7, trim = FALSE, scale = "width", position = position_identity()) +  # Ensure overlap
  geom_jitter(width = 0.1, alpha = 0.4, size = 1, color = "black") +  # Add jittered points for visibility
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +  # Add 1:1 diagonal line
  labs(x = "True Divergence", y = "Estimated Mean Divergence (x 10^-8)", 
       title = "True vs Estimated Divergence (Violin Plot, Config 3: 10000_10000)") +
  theme_bw() +
  coord_fixed(ratio = 1) +  # Ensure the aspect ratio is 1:1
  ylim(0, max_limit) +  # Set y-axis limit
  theme(legend.position = "none")  # Remove the legend








library(tidyr)
library(dplyr)
library(ggplot2)

# Load the summary file
file_path <- "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_3_10000_10000_results/summary_output.txt"
df_new <- read.table(file_path, header = TRUE, sep = "\t")

# Gather the Mean divergence values into long format
df_new_long_mean <- df_new %>%
  gather(key = "PopPair", value = "MeanDivergence", 
         Mean_Divergence_PopA_PopB:Mean_Divergence_PopK_PopL) %>%
  select(Run, PopPair, MeanDivergence)

# Gather the True divergence values into long format
df_new_long_true <- df_new %>%
  gather(key = "PopPair_True", value = "TrueValue", 
         True_Divergence_PopA_PopB:True_Divergence_PopK_PopL) %>%
  mutate(PopPair = gsub("True_Divergence_", "Mean_Divergence_", PopPair_True)) %>%
  select(Run, PopPair, TrueValue)

# Merge both datasets based on matching population pairs and runs
df_combined_new <- inner_join(df_new_long_mean, df_new_long_true, by = c("Run", "PopPair"))

# Remove duplicates if necessary
df_combined_new_unique <- df_combined_new[!duplicated(df_combined_new[, c("TrueValue", "MeanDivergence")]), ]

# Divide the mean estimated values by 1e-8
df_combined_new_unique$MeanDivergence <- df_combined_new_unique$MeanDivergence / 1e-8

# Filter out rows where MeanDivergence is NA or outside the expected range (e.g., below 0)
df_combined_new_unique <- df_combined_new_unique %>%
  filter(!is.na(MeanDivergence) & MeanDivergence >= 0)

# Determine the max limit for the plot axes
max_limit <- max(c(max(df_combined_new_unique$TrueValue), max(df_combined_new_unique$MeanDivergence)))

# Create the plot with properly overlapping violin plots
ggplot(df_combined_new_unique, aes(x = as.factor(TrueValue), y = MeanDivergence, fill = PopPair)) +
  geom_violin(alpha = 0.7, trim = FALSE, scale = "width", position = position_identity()) +  # Ensure overlap
  geom_jitter(width = 0.1, alpha = 0.4, size = 1, color = "black") +  # Add jittered points for visibility
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +  # Add 1:1 diagonal line
  labs(x = "True Divergence", y = "Estimated Mean Divergence (x 10^-8)", 
       title = "True vs Estimated Divergence (Violin Plot, Config 3: 10000_10000)") +
  theme_bw() +
  coord_fixed(ratio = 1) +  # Ensure the aspect ratio is 1:1
  ylim(0, max_limit) +  # Set y-axis limit
  theme(legend.position = "none")  # Remove the legend















library(tidyr)
library(dplyr)
library(ggplot2)

# Load the summary file
file_path <- "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_3_10000_10000_results/summary_output.txt"
df_new <- read.table(file_path, header = TRUE, sep = "\t")

# Gather the Mean divergence values into long format
df_new_long_mean <- df_new %>%
  gather(key = "PopPair", value = "MeanDivergence", 
         Mean_Divergence_PopA_PopB:Mean_Divergence_PopK_PopL) %>%
  select(Run, PopPair, MeanDivergence)

# Gather the True divergence values into long format
df_new_long_true <- df_new %>%
  gather(key = "PopPair_True", value = "TrueValue", 
         True_Divergence_PopA_PopB:True_Divergence_PopK_PopL) %>%
  mutate(PopPair = gsub("True_Divergence_", "Mean_Divergence_", PopPair_True)) %>%
  select(Run, PopPair, TrueValue)

# Merge both datasets based on matching population pairs and runs
df_combined_new <- inner_join(df_new_long_mean, df_new_long_true, by = c("Run", "PopPair"))

# Remove duplicates if necessary
df_combined_new_unique <- df_combined_new[!duplicated(df_combined_new[, c("TrueValue", "MeanDivergence")]), ]

# Divide the mean estimated values by 1e-8
df_combined_new_unique$MeanDivergence <- df_combined_new_unique$MeanDivergence / 1e-8

# Filter out rows where MeanDivergence is NA, below zero, or excessively high
df_combined_new_unique <- df_combined_new_unique %>%
  filter(!is.na(MeanDivergence) & MeanDivergence >= 0 & MeanDivergence <= max(TrueValue) * 10)

# Determine the max limit for the plot axes
max_limit <- max(c(df_combined_new_unique$TrueValue, df_combined_new_unique$MeanDivergence))

# Create the plot with properly overlapping violin plots
ggplot(df_combined_new_unique, aes(x = as.factor(TrueValue), y = MeanDivergence, fill = PopPair)) +
  geom_violin(alpha = 0.7, trim = FALSE, scale = "width", position = position_identity()) +  # Ensure overlap
  geom_jitter(width = 0.1, alpha = 0.4, size = 1, color = "black") +  # Add jittered points for visibility
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +  # Add 1:1 diagonal line
  labs(x = "True Divergence", y = "Estimated Mean Divergence (x 10^-8)", 
       title = "True vs Estimated Divergence (Violin Plot, Config 3: 10000_10000)") +
  theme_bw() +
  coord_fixed(ratio = 1) +  # Ensure the aspect ratio is 1:1
  ylim(0, max_limit) +  # Set y-axis limit
  theme(legend.position = "none")  # Remove the legend














library(tidyr)
library(dplyr)
library(ggplot2)

# Load the summary file
file_path <- "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_3_10000_10000_results/summary_output.txt"
df_new <- read.table(file_path, header = TRUE, sep = "\t")

# Gather the Mean divergence values into long format
df_new_long_mean <- df_new %>%
  gather(key = "PopPair", value = "MeanDivergence", 
         Mean_Divergence_PopA_PopB:Mean_Divergence_PopK_PopL) %>%
  select(Run, PopPair, MeanDivergence)

# Gather the True divergence values into long format
df_new_long_true <- df_new %>%
  gather(key = "PopPair_True", value = "TrueValue", 
         True_Divergence_PopA_PopB:True_Divergence_PopK_PopL) %>%
  mutate(PopPair = gsub("True_Divergence_", "Mean_Divergence_", PopPair_True)) %>%
  select(Run, PopPair, TrueValue)

# Merge both datasets based on matching population pairs and runs
df_combined_new <- inner_join(df_new_long_mean, df_new_long_true, by = c("Run", "PopPair"))

# Remove duplicates if necessary
df_combined_new_unique <- df_combined_new[!duplicated(df_combined_new[, c("TrueValue", "MeanDivergence")]), ]

# Divide the mean estimated values by 1e-8
df_combined_new_unique$MeanDivergence <- df_combined_new_unique$MeanDivergence / 1e-8

# Filter out rows where MeanDivergence is NA, below zero, or excessively high
df_combined_new_unique <- df_combined_new_unique %>%
  filter(!is.na(MeanDivergence) & MeanDivergence >= 0 & MeanDivergence <= max(TrueValue) * 10)

# Determine the max limit for the plot axes
max_limit <- max(c(df_combined_new_unique$TrueValue, df_combined_new_unique$MeanDivergence))

# Create the plot with properly overlapping violin plots
ggplot(df_combined_new_unique, aes(x = as.factor(TrueValue), y = MeanDivergence, fill = PopPair)) +
  geom_violin(alpha = 0.7, trim = FALSE, scale = "width", position = position_identity()) +  # Ensure overlap
  geom_jitter(width = 0.1, alpha = 0.4, size = 1, color = "black") +  # Add jittered points for visibility
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +  # Add 1:1 diagonal line
  labs(x = "True Divergence", y = "Estimated Mean Divergence (x 10^-8)", 
       title = "True vs Estimated Divergence (Violin Plot, Config 3: 10000_10000)") +
  theme_bw() +
  coord_fixed(ratio = 1) +  # Ensure the aspect ratio is 1:1
  ylim(0, max_limit) +  # Set y-axis limit
  theme(legend.position = "none")  # Remove the legend












library(tidyr)
library(dplyr)
library(ggplot2)

# Load the summary file
file_path <- "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_3_10000_10000_results/summary_output.txt"
df_new <- read.table(file_path, header = TRUE, sep = "\t")

# Gather the Mean divergence values into long format
df_new_long_mean <- df_new %>%
  gather(key = "PopPair", value = "MeanDivergence", 
         Mean_Divergence_PopA_PopB:Mean_Divergence_PopK_PopL) %>%
  select(Run, PopPair, MeanDivergence)

# Gather the True divergence values into long format
df_new_long_true <- df_new %>%
  gather(key = "PopPair_True", value = "TrueValue", 
         True_Divergence_PopA_PopB:True_Divergence_PopK_PopL) %>%
  mutate(PopPair = gsub("True_Divergence_", "Mean_Divergence_", PopPair_True)) %>%
  select(Run, PopPair, TrueValue)

# Merge both datasets based on matching population pairs and runs
df_combined_new <- inner_join(df_new_long_mean, df_new_long_true, by = c("Run", "PopPair"))

# Remove duplicates if necessary
df_combined_new_unique <- df_combined_new[!duplicated(df_combined_new[, c("TrueValue", "MeanDivergence")]), ]

# Divide the mean estimated values by 1e-8
df_combined_new_unique$MeanDivergence <- df_combined_new_unique$MeanDivergence / 1e-8

# Filter out rows where MeanDivergence is NA or negative
df_combined_new_unique <- df_combined_new_unique %>%
  filter(!is.na(MeanDivergence) & MeanDivergence >= 0)

# Find the maximum divergence values
max_true_value <- max(df_combined_new_unique$TrueValue)
max_mean_divergence <- max(df_combined_new_unique$MeanDivergence)

# Determine the max limit for the plot axes
max_limit <- max(max_true_value, max_mean_divergence)

# Create the plot with properly overlapping violin plots
ggplot(df_combined_new_unique, aes(x = as.factor(TrueValue), y = MeanDivergence, fill = PopPair)) +
  geom_violin(alpha = 0.7, trim = FALSE, scale = "width", position = position_identity()) +  # Ensure overlap
  geom_jitter(width = 0.1, alpha = 0.4, size = 1, color = "black") +  # Add jittered points for visibility
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +  # Add 1:1 diagonal line
  labs(x = "True Divergence", y = "Estimated Mean Divergence (x 10^-8)", 
       title = "True vs Estimated Divergence (Violin Plot, Config 3: 10000_10000)") +
  theme_bw() +
  coord_fixed(ratio = 1) +  # Ensure the aspect ratio is 1:1
  xlim(0, max_limit) +  # Set x-axis limit to match max limit
  ylim(0, max_limit) +  # Set y-axis limit to match max limit
  theme(legend.position = "none")  # Remove the legend










library(tidyr)
library(dplyr)
library(ggplot2)

# Load the summary file
file_path <- "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_3_10000_10000_results/summary_output.txt"
df_new <- read.table(file_path, header = TRUE, sep = "\t")

# Gather the Mean divergence values into long format
df_new_long_mean <- df_new %>%
  gather(key = "PopPair", value = "MeanDivergence", 
         Mean_Divergence_PopA_PopB:Mean_Divergence_PopK_PopL) %>%
  select(Run, PopPair, MeanDivergence)

# Gather the True divergence values into long format
df_new_long_true <- df_new %>%
  gather(key = "PopPair_True", value = "TrueValue", 
         True_Divergence_PopA_PopB:True_Divergence_PopK_PopL) %>%
  mutate(PopPair = gsub("True_Divergence_", "Mean_Divergence_", PopPair_True)) %>%
  select(Run, PopPair, TrueValue)

# Merge both datasets based on matching population pairs and runs
df_combined_new <- inner_join(df_new_long_mean, df_new_long_true, by = c("Run", "PopPair"))

# Remove duplicates if necessary
df_combined_new_unique <- df_combined_new[!duplicated(df_combined_new[, c("TrueValue", "MeanDivergence")]), ]

# Divide the mean estimated values by 1e-8
df_combined_new_unique$MeanDivergence <- df_combined_new_unique$MeanDivergence / 1e-8

# Filter out rows where MeanDivergence is NA or negative
df_combined_new_unique <- df_combined_new_unique %>%
  filter(!is.na(MeanDivergence) & MeanDivergence >= 0)

# Convert TrueValue to a numeric variable
df_combined_new_unique$TrueValue <- as.numeric(as.character(df_combined_new_unique$TrueValue))

# Determine the max limit for the plot axes
max_limit <- max(c(df_combined_new_unique$TrueValue, df_combined_new_unique$MeanDivergence))

# Create the plot with properly overlapping violin plots
ggplot(df_combined_new_unique, aes(x = TrueValue, y = MeanDivergence, fill = PopPair)) +
  geom_violin(alpha = 0.7, trim = FALSE, scale = "width", position = position_identity()) +  # Ensure overlap
  geom_jitter(width = 0.1, alpha = 0.4, size = 1, color = "black") +  # Add jittered points for visibility
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +  # Add 1:1 diagonal line
  labs(x = "True Divergence", y = "Estimated Mean Divergence (x 10^-8)", 
       title = "True vs Estimated Divergence (Violin Plot, Config 3: 10000_10000)") +
  theme_bw() +
  coord_fixed(ratio = 1) +  # Ensure the aspect ratio is 1:1
  xlim(0, max_limit) +  # Set x-axis limit to match max limit
  ylim(0, max_limit) +  # Set y-axis limit to match max limit
  theme(legend.position = "none")  # Remove the legend











library(tidyr)
library(dplyr)
library(ggplot2)
library(scales)  # For formatting axis labels

# Load the summary file
file_path <- "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_3_10000_10000_results/summary_output.txt"
df_new <- read.table(file_path, header = TRUE, sep = "\t")

# Gather the Mean divergence values into long format
df_new_long_mean <- df_new %>%
  gather(key = "PopPair", value = "MeanDivergence", 
         Mean_Divergence_PopA_PopB:Mean_Divergence_PopK_PopL) %>%
  select(Run, PopPair, MeanDivergence)

# Gather the True divergence values into long format
df_new_long_true <- df_new %>%
  gather(key = "PopPair_True", value = "TrueValue", 
         True_Divergence_PopA_PopB:True_Divergence_PopK_PopL) %>%
  mutate(PopPair = gsub("True_Divergence_", "Mean_Divergence_", PopPair_True)) %>%
  select(Run, PopPair, TrueValue)

# Merge both datasets based on matching population pairs and runs
df_combined_new <- inner_join(df_new_long_mean, df_new_long_true, by = c("Run", "PopPair"))

# Remove duplicates if necessary
df_combined_new_unique <- df_combined_new[!duplicated(df_combined_new[, c("TrueValue", "MeanDivergence")]), ]

# Divide the mean estimated values by 1e-8
df_combined_new_unique$MeanDivergence <- df_combined_new_unique$MeanDivergence / 1e-8

# Filter out rows where MeanDivergence is NA or negative
df_combined_new_unique <- df_combined_new_unique %>%
  filter(!is.na(MeanDivergence) & MeanDivergence >= 0)

# Convert TrueValue to a numeric variable
df_combined_new_unique$TrueValue <- as.numeric(as.character(df_combined_new_unique$TrueValue))

# Determine the max limit for the plot axes
max_limit <- max(c(df_combined_new_unique$TrueValue, df_combined_new_unique$MeanDivergence))

# Create the plot with properly overlapping violin plots and formatted axis labels
ggplot(df_combined_new_unique, aes(x = TrueValue, y = MeanDivergence, fill = PopPair)) +
  geom_violin(alpha = 0.7, trim = FALSE, scale = "width", position = position_identity()) +  # Ensure overlap
  geom_jitter(width = 0.1, alpha = 0.4, size = 1, color = "black") +  # Add jittered points for visibility
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +  # Add 1:1 diagonal line
  labs(x = "True Divergence", y = "Estimated Mean Divergence (x 10^-8)", 
       title = "True vs Estimated Divergence (Violin Plot, Config 3: 10000_10000)") +
  theme_bw() +
  coord_fixed(ratio = 1) +  # Ensure the aspect ratio is 1:1
  scale_x_continuous(labels = comma, limits = c(0, max_limit)) +  # Set x-axis with whole numbers
  scale_y_continuous(labels = comma, limits = c(0, max_limit)) +  # Set y-axis with whole numbers
  theme(legend.position = "none")  # Remove the legend











library(tidyr)
library(dplyr)
library(ggplot2)
library(scales)  # For formatting axis labels

# Load the summary file
file_path <- "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_1_0_10000_results/summary_output.txt"
df_new <- read.table(file_path, header = TRUE, sep = "\t")

# Gather the Mean divergence values into long format
df_new_long_mean <- df_new %>%
  gather(key = "PopPair", value = "MeanDivergence", 
         Mean_Divergence_PopA_PopB:Mean_Divergence_PopK_PopL) %>%
  select(Run, PopPair, MeanDivergence)

# Gather the True divergence values into long format
df_new_long_true <- df_new %>%
  gather(key = "PopPair_True", value = "TrueValue", 
         True_Divergence_PopA_PopB:True_Divergence_PopK_PopL) %>%
  mutate(PopPair = gsub("True_Divergence_", "Mean_Divergence_", PopPair_True)) %>%
  select(Run, PopPair, TrueValue)

# Merge both datasets based on matching population pairs and runs
df_combined_new <- inner_join(df_new_long_mean, df_new_long_true, by = c("Run", "PopPair"))

# Remove duplicates if necessary
df_combined_new_unique <- df_combined_new[!duplicated(df_combined_new[, c("TrueValue", "MeanDivergence")]), ]

# Divide the mean estimated values by 1e-8
df_combined_new_unique$MeanDivergence <- df_combined_new_unique$MeanDivergence / 1e-8

# Filter out rows where MeanDivergence is NA or negative
df_combined_new_unique <- df_combined_new_unique %>%
  filter(!is.na(MeanDivergence) & MeanDivergence >= 0)

# Convert TrueValue to a numeric variable
df_combined_new_unique$TrueValue <- as.numeric(as.character(df_combined_new_unique$TrueValue))

# Determine the max limit for the plot axes
max_limit <- max(c(df_combined_new_unique$TrueValue, df_combined_new_unique$MeanDivergence))

# Get unique true divergence values for vertical lines
unique_true_values <- unique(df_combined_new_unique$TrueValue)

# Create the plot with properly overlapping violin plots, formatted axis labels, and marked true divergence times
ggplot(df_combined_new_unique, aes(x = TrueValue, y = MeanDivergence, fill = PopPair)) +
  geom_violin(alpha = 0.7, trim = FALSE, scale = "width", position = position_identity()) +  # Ensure overlap
  geom_jitter(width = 0.1, alpha = 0.4, size = 1, color = "black") +  # Add jittered points for visibility
  geom_vline(xintercept = unique_true_values, linetype = "dotted", color = "blue", size = 0.7) +  # Add vertical lines for true divergence times
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +  # Add 1:1 diagonal line
  labs(x = "True Divergence", y = "Estimated Mean Divergence (x 10^-8)", 
       title = "True vs Estimated Divergence (Violin Plot, Config 3: 10000_10000)") +
  theme_bw() +
  coord_fixed(ratio = 1) +  # Ensure the aspect ratio is 1:1
  scale_x_continuous(labels = comma, limits = c(0, max_limit)) +  # Set x-axis with whole numbers
  scale_y_continuous(labels = comma, limits = c(0, max_limit)) +  # Set y-axis with whole numbers
  theme(legend.position = "none")  # Remove the legend


















library(tidyr)
library(dplyr)
library(ggplot2)
library(scales)  # For formatting axis labels

# Load the summary file
file_path <- "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_1_0_10000_results/summary_output.txt"
df_new <- read.table(file_path, header = TRUE, sep = "\t")

# Gather the Mean divergence values into long format
df_new_long_mean <- df_new %>%
  gather(key = "PopPair", value = "MeanDivergence", 
         Mean_Divergence_PopA_PopB:Mean_Divergence_PopK_PopL) %>%
  select(Run, PopPair, MeanDivergence)

# Gather the True divergence values into long format
df_new_long_true <- df_new %>%
  gather(key = "PopPair_True", value = "TrueValue", 
         True_Divergence_PopA_PopB:True_Divergence_PopK_PopL) %>%
  mutate(PopPair = gsub("True_Divergence_", "Mean_Divergence_", PopPair_True)) %>%
  select(Run, PopPair, TrueValue)

# Merge both datasets based on matching population pairs and runs
df_combined_new <- inner_join(df_new_long_mean, df_new_long_true, by = c("Run", "PopPair"))

# Remove duplicates if necessary
df_combined_new_unique <- df_combined_new[!duplicated(df_combined_new[, c("TrueValue", "MeanDivergence")]), ]

# Divide the mean estimated values by 1e-8
df_combined_new_unique$MeanDivergence <- df_combined_new_unique$MeanDivergence / 1e-8

# Filter out rows where MeanDivergence is NA or negative
df_combined_new_unique <- df_combined_new_unique %>%
  filter(!is.na(MeanDivergence) & MeanDivergence >= 0)

# Convert TrueValue to a numeric variable
df_combined_new_unique$TrueValue <- as.numeric(as.character(df_combined_new_unique$TrueValue))

# Determine the max limit for the plot axes
max_limit <- max(c(df_combined_new_unique$TrueValue, df_combined_new_unique$MeanDivergence))

# Get unique true divergence values for vertical lines
unique_true_values <- unique(df_combined_new_unique$TrueValue)

# Create the plot with properly overlapping violin plots, formatted axis labels, and marked true divergence times
ggplot(df_combined_new_unique, aes(x = TrueValue, y = MeanDivergence, fill = PopPair)) +
  geom_violin(alpha = 0.7, trim = FALSE, scale = "width", position = position_identity()) +  # Ensure overlap
  geom_jitter(width = 0.1, alpha = 0.4, size = 1, color = "black") +  # Add jittered points for visibility
  geom_vline(xintercept = unique_true_values, linetype = "dotted", color = "blue", size = 0.7) +  # Add vertical lines for true divergence times
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +  # Add 1:1 diagonal line
  labs(x = "True Divergence", y = "Estimated Mean Divergence", 
       title = "True vs Estimated Divergence (Violin Plot, Config 3: 10000_10000)") +
  theme_bw() +
  coord_fixed(ratio = 1) +  # Ensure the aspect ratio is 1:1
  scale_x_continuous(labels = comma, limits = c(0, max_limit)) +  # Set x-axis with whole numbers
  scale_y_continuous(labels = comma, limits = c(0, max_limit)) +  # Set y-axis with whole numbers
  theme(
    legend.position = "none",  # Remove the legend
    axis.title = element_text(size = 15, face="bold"),  # Increase axis title size
    axis.text = element_text(size = 15, face="bold"),  # Increase axis tick labels size
    plot.title = element_text(size = 0, face = "bold")  # Increase title size
  )




#For same divergence events across all population pairs. 
############################
#############################
library(tidyr)
library(dplyr)
library(ggplot2)
library(scales)  # For formatting axis labels

# Load the summary file
file_path <- "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_1_0_10000_results/summary_output.txt"
df_new <- read.table(file_path, header = TRUE, sep = "\t")

# Gather the Mean divergence values into long format
df_new_long_mean <- df_new %>%
  gather(key = "PopPair", value = "MeanDivergence", 
         Mean_Divergence_PopA_PopB:Mean_Divergence_PopK_PopL) %>%
  select(Run, PopPair, MeanDivergence)

# Gather the True divergence values into long format
df_new_long_true <- df_new %>%
  gather(key = "PopPair_True", value = "TrueValue", 
         True_Divergence_PopA_PopB:True_Divergence_PopK_PopL) %>%
  mutate(PopPair = gsub("True_Divergence_", "Mean_Divergence_", PopPair_True)) %>%
  select(Run, PopPair, TrueValue)

# Merge both datasets based on matching population pairs and runs
df_combined_new <- inner_join(df_new_long_mean, df_new_long_true, by = c("Run", "PopPair"))

# Remove duplicates if necessary
df_combined_new_unique <- df_combined_new[!duplicated(df_combined_new[, c("TrueValue", "MeanDivergence")]), ]

# Divide the mean estimated values by 1e-8
df_combined_new_unique$MeanDivergence <- df_combined_new_unique$MeanDivergence / 1e-8

# Filter out rows where MeanDivergence is NA or negative
df_combined_new_unique <- df_combined_new_unique %>%
  filter(!is.na(MeanDivergence) & MeanDivergence >= 0)

# Convert TrueValue to a numeric variable
df_combined_new_unique$TrueValue <- as.numeric(as.character(df_combined_new_unique$TrueValue))

# Get unique true divergence values for vertical lines
unique_true_values <- unique(df_combined_new_unique$TrueValue)

# Create the plot with properly overlapping violin plots, formatted axis labels, and marked true divergence times
ggplot(df_combined_new_unique, aes(x = TrueValue, y = MeanDivergence, fill = PopPair)) +
  geom_violin(alpha = 0.7, trim = FALSE, scale = "width", position = position_identity()) +  # Ensure overlap
  geom_jitter(width = 0.1, alpha = 0.4, size = 1, color = "black") +  # Add jittered points for visibility
  geom_vline(xintercept = unique_true_values, linetype = "dotted", color = "blue", size = 0.7) +  # Add vertical lines for true divergence times
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +  # Add 1:1 diagonal line
  labs(x = "True Divergence", y = "Estimated Mean Divergence", 
       title = "True vs Estimated Divergence (Violin Plot, Config 3: 10000_10000)") +
  theme_bw() +
  coord_fixed(ratio = 1) +  # Ensure the aspect ratio is 1:1
  scale_x_continuous(labels = comma, limits = c(0, 20000)) +  # Set x-axis limit to 0-20,000
  scale_y_continuous(labels = comma, limits = c(0, 20000)) +  # Set y-axis limit to 0-20,000
  theme(
    legend.position = "none",  # Remove the legend
    axis.title = element_text(size = 15, face="bold"),  # Increase axis title size
    axis.text = element_text(size = 15, face="bold"),  # Increase axis tick labels size
    plot.title = element_text(size = 15, face = "bold")  # Increase title size
  )


ggplot(df_combined_new_unique, aes(x = factor(PopPair), y = MeanDivergence, fill = PopPair)) +
  geom_violin(alpha = 0.7, trim = FALSE, scale = "width", adjust = 0.5, position = position_dodge(width = 0.8)) +
  geom_jitter(alpha = 0.4, size = 1, color = "black", position = position_dodge(width = 0.8)) +
  geom_vline(xintercept = unique_true_values, linetype = "dotted", color = "blue", size = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  labs(x = "Population Pair", y = "Estimated Mean Divergence", 
       title = "True vs Estimated Divergence (Violin Plot, Config 3: 10000_10000)") +
  theme_bw() +
  scale_y_continuous(labels = comma, limits = c(0, 20000)) +
  theme(
    legend.position = "none",  # Remove the legend
    axis.title = element_text(size = 15, face = "bold"),  # Increase axis title size
    axis.text = element_text(size = 15, face = "bold"),  # Increase axis tick labels size
    plot.title = element_text(size = 15, face = "bold")  # Increase title size
  )


ggplot(df_combined_new_unique, aes(x = as.factor(TrueValue), y = MeanDivergence, fill = PopPair)) +
  geom_violin(alpha = 0.7, trim = FALSE, scale = "width", adjust = 0.5, position = position_dodge(width = 0)) +
  geom_jitter(alpha = 0.4, size = 1, color = "black", position = position_dodge(width = 0.5)) +
  geom_vline(xintercept = 10000, linetype = "dotted", color = "blue", size = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  labs(x = "True Divergence", y = "Estimated Mean Divergence", 
       title = "True vs Estimated Divergence (Aligned Violin Plot)") +
  theme_bw() +
  scale_x_discrete(labels = "10,000") +  # Set the x-axis label to show the true divergence value
  scale_y_continuous(labels = comma, limits = c(0, 20000)) +
  theme(
    legend.position = "none",  # Remove the legend
    axis.title = element_text(size = 15, face = "bold"),  # Increase axis title size
    axis.text = element_text(size = 15, face = "bold"),  # Increase axis tick labels size
    plot.title = element_text(size = 15, face = "bold")  # Increase title size
  )




library(tidyr)
library(dplyr)
library(ggplot2)
library(scales)  # For formatting axis labels

# Load the summary file
file_path <- "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_1_0_10000_results/summary_output.txt"
df_new <- read.table(file_path, header = TRUE, sep = "\t")

# Gather the Mean divergence values into long format
df_new_long_mean <- df_new %>%
  gather(key = "PopPair", value = "MeanDivergence", 
         Mean_Divergence_PopA_PopB:Mean_Divergence_PopK_PopL) %>%
  select(Run, PopPair, MeanDivergence)

# Gather the True divergence values into long format
df_new_long_true <- df_new %>%
  gather(key = "PopPair_True", value = "TrueValue", 
         True_Divergence_PopA_PopB:True_Divergence_PopK_PopL) %>%
  mutate(PopPair = gsub("True_Divergence_", "Mean_Divergence_", PopPair_True)) %>%
  select(Run, PopPair, TrueValue)

# Merge both datasets based on matching population pairs and runs
df_combined_new <- inner_join(df_new_long_mean, df_new_long_true, by = c("Run", "PopPair"))

# Remove duplicates if necessary
df_combined_new_unique <- df_combined_new[!duplicated(df_combined_new[, c("TrueValue", "MeanDivergence")]), ]

# Divide the mean estimated values by 1e-8
df_combined_new_unique$MeanDivergence <- df_combined_new_unique$MeanDivergence / 1e-8

# Filter out rows where MeanDivergence is NA or negative
df_combined_new_unique <- df_combined_new_unique %>%
  filter(!is.na(MeanDivergence) & MeanDivergence >= 0)

# Convert TrueValue to a numeric variable
df_combined_new_unique$TrueValue <- as.numeric(as.character(df_combined_new_unique$TrueValue))

# Set the x-axis limit
x_max_limit <- 50000

# Get unique true divergence values for vertical lines
unique_true_values <- unique(df_combined_new_unique$TrueValue)

# Create the plot with properly overlapping violin plots, formatted axis labels, and marked true divergence times
ggplot(df_combined_new_unique, aes(x = TrueValue, y = MeanDivergence, fill = PopPair)) +
  geom_violin(alpha = 0.7, trim = FALSE, scale = "width", position = position_identity()) +  # Ensure overlap
  geom_jitter(width = 0.1, alpha = 0.4, size = 1, color = "black") +  # Add jittered points for visibility
  geom_vline(xintercept = unique_true_values, linetype = "dotted", color = "blue", size = 0.7) +  # Add vertical lines for true divergence times
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +  # Add 1:1 diagonal line
  labs(x = "True Divergence", y = "Estimated Mean Divergence (x 10^-8)", 
       title = "True vs Estimated Divergence (Violin Plot, Config 3: 10000_10000)") +
  theme_bw() +
  coord_fixed(ratio = 1) +  # Ensure the aspect ratio is 1:1
  scale_x_continuous(labels = comma, limits = c(0, x_max_limit)) +  # Set x-axis with whole numbers
  scale_y_continuous(labels = comma, limits = c(0, max_limit)) +  # Set y-axis with whole numbers
  theme(
    legend.position = "none",  # Remove the legend
    axis.title = element_text(size = 14),  # Increase axis title size
    axis.text = element_text(size = 12),  # Increase axis tick labels size
    plot.title = element_text(size = 16, face = "bold")  # Increase title size
  )













library(tidyr)
library(dplyr)
library(ggplot2)
library(scales)  # For formatting axis labels

# Load the summary file
file_path <- "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_1_0_10000_results/summary_output.txt"
df_new <- read.table(file_path, header = TRUE, sep = "\t")

# Gather the Mean divergence values into long format
df_new_long_mean <- df_new %>%
  gather(key = "PopPair", value = "MeanDivergence", 
         Mean_Divergence_PopA_PopB:Mean_Divergence_PopK_PopL) %>%
  select(Run, PopPair, MeanDivergence)

# Gather the True divergence values into long format
df_new_long_true <- df_new %>%
  gather(key = "PopPair_True", value = "TrueValue", 
         True_Divergence_PopA_PopB:True_Divergence_PopK_PopL) %>%
  mutate(PopPair = gsub("True_Divergence_", "Mean_Divergence_", PopPair_True)) %>%
  select(Run, PopPair, TrueValue)

# Merge both datasets based on matching population pairs and runs
df_combined_new <- inner_join(df_new_long_mean, df_new_long_true, by = c("Run", "PopPair"))

# Remove duplicates if necessary
df_combined_new_unique <- df_combined_new[!duplicated(df_combined_new[, c("TrueValue", "MeanDivergence")]), ]

# Divide the mean estimated values by 1e-8
df_combined_new_unique$MeanDivergence <- df_combined_new_unique$MeanDivergence / 1e-8

# Filter out rows where MeanDivergence is NA or negative
df_combined_new_unique <- df_combined_new_unique %>%
  filter(!is.na(MeanDivergence) & MeanDivergence >= 0)

# Convert TrueValue to a numeric variable
df_combined_new_unique$TrueValue <- as.numeric(as.character(df_combined_new_unique$TrueValue))

# Set the axis limit
axis_limit <- 20000

# Get unique true divergence values for vertical lines
unique_true_values <- unique(df_combined_new_unique$TrueValue)

# Create the plot with properly overlapping violin plots, formatted axis labels, and marked true divergence times
ggplot(df_combined_new_unique, aes(x = TrueValue, y = MeanDivergence, fill = PopPair)) +
  geom_violin(alpha = 0.7, trim = FALSE, scale = "width", position = position_identity()) +  # Ensure overlap
  geom_jitter(width = 0.05, alpha = 0.4, size = 0.5, color = "black") +  # Add jittered points for visibility
  geom_vline(xintercept = unique_true_values, linetype = "dotted", color = "blue", size = 0.7) +  # Add vertical lines for true divergence times
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +  # Add 1:1 diagonal line
  labs(x = "True Divergence", y = "Estimated Mean Divergence (x 10^-8)", 
       title = "True vs Estimated Divergence (Violin Plot, Config 3: 10000_10000)") +
  theme_bw() +
  coord_fixed(ratio = 1) +  # Ensure the aspect ratio is 1:1
  scale_x_continuous(labels = comma, limits = c(0, axis_limit)) +  # Set x-axis limit
  scale_y_continuous(labels = comma, limits = c(0, axis_limit)) +  # Set y-axis limit to match x-axis
  theme(
    legend.position = "none",  # Remove the legend
    axis.title = element_text(size = 16, face = "bold"),  # Increase axis title size
    axis.text = element_text(size = 16, face = "bold"),  # Increase axis tick labels size
    plot.title = element_text(size = 0, face = "bold")  # Increase title size
  )













library(tidyr)
library(dplyr)
library(ggplot2)
library(scales)  # For formatting axis labels

# Load the summary file
file_path <- "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_3_80000_10000_results/summary_output.txt"
df_new <- read.table(file_path, header = TRUE, sep = "\t")

# Gather the Mean divergence values into long format
df_new_long_mean <- df_new %>%
  gather(key = "PopPair", value = "MeanDivergence", 
         Mean_Divergence_PopA_PopB:Mean_Divergence_PopK_PopL) %>%
  select(Run, PopPair, MeanDivergence)

# Gather the True divergence values into long format
df_new_long_true <- df_new %>%
  gather(key = "PopPair_True", value = "TrueValue", 
         True_Divergence_PopA_PopB:True_Divergence_PopK_PopL) %>%
  mutate(PopPair = gsub("True_Divergence_", "Mean_Divergence_", PopPair_True)) %>%
  select(Run, PopPair, TrueValue)

# Merge both datasets based on matching population pairs and runs
df_combined_new <- inner_join(df_new_long_mean, df_new_long_true, by = c("Run", "PopPair"))

# Remove duplicates if necessary
df_combined_new_unique <- df_combined_new[!duplicated(df_combined_new[, c("TrueValue", "MeanDivergence")]), ]

# Divide the mean estimated values by 1e-8
df_combined_new_unique$MeanDivergence <- df_combined_new_unique$MeanDivergence / 1e-8

# Filter out rows where MeanDivergence is NA or negative
df_combined_new_unique <- df_combined_new_unique %>%
  filter(!is.na(MeanDivergence) & MeanDivergence >= 0)

# Convert TrueValue to a numeric variable
df_combined_new_unique$TrueValue <- as.numeric(as.character(df_combined_new_unique$TrueValue))

# Set the axis limit
axis_limit <- 350000

# Get unique true divergence values for vertical lines
unique_true_values <- unique(df_combined_new_unique$TrueValue)

# Create the plot with thinned violin plots
ggplot(df_combined_new_unique, aes(x = TrueValue, y = MeanDivergence, fill = PopPair)) +
  geom_violin(alpha = 0.7, trim = FALSE, scale = "count", width = 0.6, position = position_identity()) +  # Thinned violins
  geom_jitter(width = 0.1, alpha = 0.4, size = 0.5, color = "black") +  # Add jittered points for visibility
  geom_vline(xintercept = unique_true_values, linetype = "dotted", color = "blue", size = 0.7) +  # Add vertical lines for true divergence times
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +  # Add 1:1 diagonal line
  labs(x = "True Divergence", y = "Estimated Mean Divergence (x 10^-8)", 
       title = "True vs Estimated Divergence (Thinned Violin Plot, Config 3: 10000_10000)") +
  theme_bw() +
  coord_fixed(ratio = 1) +  # Ensure the aspect ratio is 1:1
  scale_x_continuous(labels = comma, limits = c(0, axis_limit)) +  # Set x-axis limit
  scale_y_continuous(labels = comma, limits = c(0, axis_limit)) +  # Set y-axis limit to match x-axis
  theme(
    legend.position = "none",  # Remove the legend
    axis.title = element_text(size = 14),  # Increase axis title size
    axis.text = element_text(size = 12),  # Increase axis tick labels size
    plot.title = element_text(size = 16, face = "bold")  # Increase title size
  )











library(tidyr)
library(dplyr)
library(ggplot2)
library(scales)  # For formatting axis labels

# Load the summary file
file_path <- "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_3_80000_10000_results/summary_output.txt"
df_new <- read.table(file_path, header = TRUE, sep = "\t")

# Gather the Mean divergence values into long format
df_new_long_mean <- df_new %>%
  gather(key = "PopPair", value = "MeanDivergence", 
         Mean_Divergence_PopA_PopB:Mean_Divergence_PopK_PopL) %>%
  select(Run, PopPair, MeanDivergence)

# Gather the True divergence values into long format
df_new_long_true <- df_new %>%
  gather(key = "PopPair_True", value = "TrueValue", 
         True_Divergence_PopA_PopB:True_Divergence_PopK_PopL) %>%
  mutate(PopPair = gsub("True_Divergence_", "Mean_Divergence_", PopPair_True)) %>%
  select(Run, PopPair, TrueValue)

# Merge both datasets based on matching population pairs and runs
df_combined_new <- inner_join(df_new_long_mean, df_new_long_true, by = c("Run", "PopPair"))

# Remove duplicates if necessary
df_combined_new_unique <- df_combined_new[!duplicated(df_combined_new[, c("TrueValue", "MeanDivergence")]), ]

# Divide the mean estimated values by 1e-8
df_combined_new_unique$MeanDivergence <- df_combined_new_unique$MeanDivergence / 1e-8

# Filter out rows where MeanDivergence is NA or negative
df_combined_new_unique <- df_combined_new_unique %>%
  filter(!is.na(MeanDivergence) & MeanDivergence >= 0)

# Convert TrueValue to a numeric variable
df_combined_new_unique$TrueValue <- as.numeric(as.character(df_combined_new_unique$TrueValue))

# Set the axis limit
axis_limit <- 350000

# Get unique true divergence values for vertical lines
unique_true_values <- unique(df_combined_new_unique$TrueValue)

# Create the plot with adjusted violin plots
ggplot(df_combined_new_unique, aes(x = TrueValue, y = MeanDivergence, fill = PopPair)) +
  geom_violin(alpha = 0.7, trim = FALSE,  width = 10, position = position_identity()) +  # Adjusted width for violins
  geom_jitter(width = 0.1, alpha = 0.4, size = 1, color = "black") +  # Add jittered points for visibility
  geom_vline(xintercept = unique_true_values, linetype = "dotted", color = "blue", size = 0.7) +  # Add vertical lines for true divergence times
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +  # Add 1:1 diagonal line
  labs(x = "True Divergence", y = "Estimated Mean Divergence (x 10^-8)", 
       title = "True vs Estimated Divergence (Config 3: 10000_10000)") +
  theme_bw() +
  coord_fixed(ratio = 1) +  # Ensure the aspect ratio is 1:1
  scale_x_continuous(labels = comma, limits = c(0, axis_limit)) +  # Set x-axis limit
  scale_y_continuous(labels = comma, limits = c(0, axis_limit)) +  # Set y-axis limit to match x-axis
  theme(
    legend.position = "none",  # Remove the legend
    axis.title = element_text(size = 14),  # Increase axis title size
    axis.text = element_text(size = 12),  # Increase axis tick labels size
    plot.title = element_text(size = 16, face = "bold")  # Increase title size
  )















#########################
########################
library(tidyr)
library(dplyr)
library(ggplot2)
library(scales)  # For formatting axis labels

# Load the summary file
file_path <- "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_6_160000_10000_results/summary_output.txt"

df_new <- read.table(file_path, header = TRUE, sep = "\t")

# Gather the Mean divergence values into long format
df_new_long_mean <- df_new %>%
  gather(key = "PopPair", value = "MeanDivergence", 
         Mean_Divergence_PopA_PopB:Mean_Divergence_PopK_PopL) %>%
  select(Run, PopPair, MeanDivergence)

# Gather the True divergence values into long format
df_new_long_true <- df_new %>%
  gather(key = "PopPair_True", value = "TrueValue", 
         True_Divergence_PopA_PopB:True_Divergence_PopK_PopL) %>%
  mutate(PopPair = gsub("True_Divergence_", "Mean_Divergence_", PopPair_True)) %>%
  select(Run, PopPair, TrueValue)

# Merge both datasets based on matching population pairs and runs
df_combined_new <- inner_join(df_new_long_mean, df_new_long_true, by = c("Run", "PopPair"))

# Remove duplicates if necessary
df_combined_new_unique <- df_combined_new[!duplicated(df_combined_new[, c("TrueValue", "MeanDivergence")]), ]

# Divide the mean estimated values by 1e-8
df_combined_new_unique$MeanDivergence <- df_combined_new_unique$MeanDivergence / 1e-8

# Filter out rows where MeanDivergence is NA or negative
df_combined_new_unique <- df_combined_new_unique %>%
  filter(!is.na(MeanDivergence) & MeanDivergence >= 0)

# Convert TrueValue to a numeric variable
df_combined_new_unique$TrueValue <- as.numeric(as.character(df_combined_new_unique$TrueValue))

# Set the axis limit
axis_limit <- 900000

# Get unique true divergence values for vertical lines
unique_true_values <- unique(df_combined_new_unique$TrueValue)

# Create the plot with adjusted violin plots
ggplot(df_combined_new_unique, aes(x = TrueValue, y = MeanDivergence, group = PopPair, fill = PopPair)) +
  geom_violin(alpha = 0.7, trim = FALSE, scale = "area", adjust = 3, position = position_identity()) +  # Make violins wider
  geom_jitter(width = 0.4, alpha = 0.4, size = 3, color = "black") +  # Add jittered points for visibility
  geom_vline(xintercept = unique_true_values, linetype = "dotted", color = "blue", size = 0.7) +  # Add vertical lines for true divergence times
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +  # Add 1:1 diagonal line
  labs(x = "True Divergence", y = "Estimated Mean Divergence (x 10^-8)", 
       title = "True vs Estimated Divergence (Config 3: 10000_10000)") +
  theme_bw() +
  coord_fixed(ratio = 1) +  # Ensure the aspect ratio is 1:1
  scale_x_continuous(labels = comma, limits = c(0, axis_limit)) +  # Set x-axis limit
  scale_y_continuous(labels = comma, limits = c(0, axis_limit)) +  # Set y-axis limit to match x-axis
  theme(
    legend.position = "none",  # Remove the legend
    axis.title = element_text(size = 16, face ="bold"),  # Increase axis title size
    axis.text = element_text(size = 16, face="bold"),  # Increase axis tick labels size
    plot.title = element_text(size = 0, face = "bold")  # Increase title size
  )












# Load the necessary libraries
library(tidyr)
library(dplyr)
library(ggplot2)
library(scales)  # For formatting axis labels

# Load the summary file
file_path <- "/Users/mtofflemire/Mtofflemire Dropbox/Michael Tofflemire/Projects/syndiesel/scripts/results/config_1-results/config_no-mig_1_0_10000_results/summary_output.txt"
df_new <- read.table(file_path, header = TRUE, sep = "\t")

# Gather the Mean divergence values into long format
df_new_long_mean <- df_new %>%
  gather(key = "PopPair", value = "MeanDivergence", 
         Mean_Divergence_PopA_PopB:Mean_Divergence_PopK_PopL) %>%
  select(Run, PopPair, MeanDivergence)

# Gather the True divergence values into long format
df_new_long_true <- df_new %>%
  gather(key = "PopPair_True", value = "TrueValue", 
         True_Divergence_PopA_PopB:True_Divergence_PopK_PopL) %>%
  mutate(PopPair = gsub("True_Divergence_", "Mean_Divergence_", PopPair_True)) %>%
  select(Run, PopPair, TrueValue)

# Merge both datasets based on matching population pairs and runs
df_combined_new <- inner_join(df_new_long_mean, df_new_long_true, by = c("Run", "PopPair"))

# Remove duplicates if necessary
df_combined_new_unique <- df_combined_new[!duplicated(df_combined_new[, c("TrueValue", "MeanDivergence")]), ]

# Divide the mean estimated values by 1e-8
df_combined_new_unique$MeanDivergence <- df_combined_new_unique$MeanDivergence / 1e-8

# Filter out rows where MeanDivergence is NA or negative
df_combined_new_unique <- df_combined_new_unique %>%
  filter(!is.na(MeanDivergence) & MeanDivergence >= 0)

# Convert TrueValue to a numeric variable
df_combined_new_unique$TrueValue <- as.numeric(as.character(df_combined_new_unique$TrueValue))

# Set the x-axis value to a continuous numeric value
ggplot(df_combined_new_unique, aes(x = TrueValue, y = MeanDivergence, fill = PopPair)) +
  geom_violin(alpha = 0.7, trim = FALSE, scale = "width", adjust = 0.5, position = position_identity()) +
  geom_jitter(alpha = 0.4, size = 1, color = "black", position = position_jitter(width = 0.1)) +
  geom_vline(xintercept = 10000, linetype = "dotted", color = "blue", size = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  labs(x = "True Divergence", y = "Estimated Mean Divergence", 
       title = "True vs Estimated Divergence (Aligned Violin Plot)") +
  theme_bw() +
  scale_x_continuous(limits = c(0, 50000), breaks = c(10000, 50000), labels = c("10,000", "50,000")) +  # Set x-axis limits and labels
  scale_y_continuous(labels = comma, limits = c(0, 50000)) +  # Set y-axis limits
  theme(
    legend.position = "none",  # Remove the legend
    axis.title = element_text(size = 15, face = "bold"),  # Increase axis title size
    axis.text = element_text(size = 15, face = "bold"),  # Increase axis tick labels size
    plot.title = element_text(size = 15, face = "bold")  # Increase title size
  )













library(tidyr)
library(dplyr)
library(ggplot2)
library(ggridges)

# Load the data
file_path <- "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_1_0_160000_results/summary_output.txt"
df <- read.table(file_path, header = TRUE, sep = "\t")

# Event types for probabilities
event_types <- c("Prob_1_Event", "Prob_2_Events", "Prob_3_Events", "Prob_4_Events", "Prob_5_Events", "Prob_6_Events")

# Reshape the data into a long format
probabilities_long_df <- df %>%
  gather(key = "EventType", value = "Probability", all_of(event_types))

# Find the event type with the highest maximum probability
highest_prob_event <- probabilities_long_df %>%
  group_by(EventType) %>%
  summarize(max_prob = max(Probability)) %>%
  filter(max_prob == max(max_prob)) %>%
  pull(EventType)

# Create a new column to define fill categories for each event type
probabilities_long_df <- probabilities_long_df %>%
  mutate(FillCategory = case_when(
    EventType == "Prob_3_Events" ~ "True Event (Prob_3_Events)",
    EventType == highest_prob_event ~ "Highest Probability Event",
    TRUE ~ "Other Events"
  ))

# Ensure FillCategory is treated as a factor
probabilities_long_df$FillCategory <- factor(probabilities_long_df$FillCategory, 
                                             levels = c("True Event (Prob_3_Events)", 
                                                        "Highest Probability Event", 
                                                        "Other Events"))

# Create the ridgeline plot
ggplot(probabilities_long_df, aes(x = Probability, y = EventType, fill = FillCategory)) +
  geom_density_ridges(alpha = 0.7, scale = 1, rel_min_height = 0.01, color = "black") +
  scale_fill_manual(values = c(
    "True Event (Prob_3_Events)" = "orange",
    "Highest Probability Event" = "lightblue",
    "Other Events" = "gray"
  )) +
  labs(title = "Ridgeline Plot of Probabilities for Each Event Type Across Runs",
       x = "Probability",
       y = "Event Type",
       fill = "Event Category") +  # Add a label for the legend
  theme_minimal() +
  theme(
    legend.position = "right"  # Position the legend on the right
  ) +
  coord_cartesian(xlim = c(0, 1))  # Set x-axis limits from 0 to 1




































# Load the necessary libraries
library(tidyr)
library(dplyr)
library(ggplot2)
library(scales)  # For formatting axis labels

# Load the summary file
file_path <- "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_1_0_10000_0.01_results/summary_output.txt"
df_new <- read.table(file_path, header = TRUE, sep = "\t")

# Step 1: Check for missing values
missing_values <- sum(is.na(df_new))
print(paste("Total missing values in the data:", missing_values))

# Gather the Mean divergence values into long format
df_new_long_mean <- df_new %>%
  gather(key = "PopPair", value = "MeanDivergence", 
         Mean_Divergence_PopA_PopB:Mean_Divergence_PopK_PopL) %>%
  select(Run, PopPair, MeanDivergence)

# Gather the True divergence values into long format
df_new_long_true <- df_new %>%
  gather(key = "PopPair_True", value = "TrueValue", 
         True_Divergence_PopA_PopB:True_Divergence_PopK_PopL) %>%
  mutate(PopPair = gsub("True_Divergence_", "Mean_Divergence_", PopPair_True)) %>%
  select(Run, PopPair, TrueValue)

# Merge both datasets based on matching population pairs and runs
df_combined_new <- inner_join(df_new_long_mean, df_new_long_true, by = c("Run", "PopPair"))

# Step 2: Check the number of rows per population pair
pair_counts <- df_combined_new %>%
  group_by(PopPair) %>%
  summarise(count = n())
print(pair_counts)

# Step 3: Remove duplicates if necessary
df_combined_new_unique <- df_combined_new[!duplicated(df_combined_new[, c("TrueValue", "MeanDivergence")]), ]

# Step 4: Divide the mean estimated values by 1e-8
df_combined_new_unique$MeanDivergence <- df_combined_new_unique$MeanDivergence / 1e-8

# Step 5: Filter out rows where MeanDivergence is NA or negative
# Note: Comment out this line if it removes rows inconsistently across pairs
df_combined_new_unique <- df_combined_new_unique %>%
  filter(!is.na(MeanDivergence) & MeanDivergence >= 0)

# Step 6: Convert TrueValue to a numeric variable
df_combined_new_unique$TrueValue <- as.numeric(as.character(df_combined_new_unique$TrueValue))

# Step 7: Get unique true divergence values for vertical lines
unique_true_values <- unique(df_combined_new_unique$TrueValue)

# Step 8: Create the plot with properly overlapping violin plots, formatted axis labels, and marked true divergence times
ggplot(df_combined_new_unique, aes(x = as.factor(TrueValue), y = MeanDivergence, fill = PopPair)) +
  geom_violin(alpha = 0.7, trim = FALSE, scale = "width", adjust = 0.5, position = position_dodge(width = 0)) +
  geom_jitter(alpha = 0.4, size = 1, color = "black", position = position_dodge(width = 0.5)) +
  geom_vline(xintercept = 10000, linetype = "dotted", color = "blue", size = 0.7) +
  geom_hline(yintercept = 10000, linetype = "dotted", color = "blue", size = 0.7) +  # Add horizontal dotted line at y = 10,000
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  labs(x = "True Divergence", y = "Estimated Mean Divergence") +
  theme_bw() +
  scale_x_discrete(labels = "10,000") +  # Set the x-axis label to show the true divergence value
  scale_y_continuous(labels = comma, limits = c(0, 50000)) +  # Increase y-axis limit to 50,000
  theme(
    legend.position = "none",  # Remove the legend
    axis.title = element_text(size = 15, face = "bold"),  # Increase axis title size
    axis.text = element_text(size = 15, face = "bold"),  # Increase axis tick labels size
    plot.title = element_text(size = 15, face = "bold")  # Increase title size
  )











# Gather the Mean divergence values into long format
df_new_long_mean <- df_new %>%
  gather(key = "PopPair", value = "MeanDivergence", 
         Mean_Divergence_PopA_PopB:Mean_Divergence_PopK_PopL) %>%
  select(Run, PopPair, MeanDivergence)

# Gather the True divergence values into long format
df_new_long_true <- df_new %>%
  gather(key = "PopPair_True", value = "TrueValue", 
         True_Divergence_PopA_PopB:True_Divergence_PopK_PopL) %>%
  mutate(PopPair = gsub("True_Divergence_", "Mean_Divergence_", PopPair_True)) %>%
  select(Run, PopPair, TrueValue)

# Merge both datasets without any deduplication or filtering
df_combined_no_filter <- inner_join(df_new_long_mean, df_new_long_true, by = c("Run", "PopPair"))

# Filter data for the 5th and 6th population pairs only
df_combined_filtered <- df_combined_no_filter %>%
  filter(PopPair %in% c("Mean_Divergence_PopI_PopJ", "Mean_Divergence_PopK_PopL"))

# Plot only the 5th and 6th population pairs
ggplot(df_combined_filtered, aes(x = as.factor(TrueValue), y = MeanDivergence, fill = PopPair)) +
  geom_violin(alpha = 0.7, trim = FALSE, scale = "width", adjust = 0.5, position = position_dodge(width = 0)) +
  geom_jitter(alpha = 0.4, size = 1, color = "black", position = position_dodge(width = 0.5)) +
  geom_vline(xintercept = 10000, linetype = "dotted", color = "blue", size = 0.7) +
  geom_hline(yintercept = 10000, linetype = "dotted", color = "blue", size = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  labs(x = "True Divergence", y = "Estimated Mean Divergence") +
  theme_bw() +
  scale_x_discrete(labels = "10,000") +
  scale_y_continuous(labels = comma, limits = c(0, 50000)) +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 15, face = "bold"),
    axis.text = element_text(size = 15, face = "bold"),
    plot.title = element_text(size = 15, face = "bold")
  )








# Step 1: Check the gathered Mean Divergence data
print("Mean Divergence - Long Format")
df_new_long_mean %>%
  filter(PopPair %in% c("Mean_Divergence_PopI_PopJ", "Mean_Divergence_PopK_PopL")) %>%
  print()

# Step 2: Check the gathered True Divergence data
print("True Divergence - Long Format")
df_new_long_true %>%
  filter(PopPair %in% c("Mean_Divergence_PopI_PopJ", "Mean_Divergence_PopK_PopL")) %>%
  print()

# Step 3: Check the merged dataset to ensure it has 30 points per population pair
print("Merged Data for 5th and 6th Population Pairs")
df_combined_no_filter %>%
  filter(PopPair %in% c("Mean_Divergence_PopI_PopJ", "Mean_Divergence_PopK_PopL")) %>%
  print()






# Ensure MeanDivergence is numeric
df_combined_filtered <- df_combined_no_filter %>%
  filter(PopPair %in% c("Mean_Divergence_PopI_PopJ", "Mean_Divergence_PopK_PopL")) %>%
  mutate(MeanDivergence = as.numeric(MeanDivergence) * 1e8)  # Scale up by 1e8 for visibility

# Plot only the 5th and 6th population pairs with scaled MeanDivergence
ggplot(df_combined_filtered, aes(x = as.factor(TrueValue), y = MeanDivergence, fill = PopPair)) +
  geom_violin(alpha = 0.7, trim = FALSE, scale = "width", adjust = 0.5, position = position_dodge(width = 0)) +
  geom_jitter(alpha = 0.4, size = 1, color = "black", position = position_dodge(width = 0.5)) +
  geom_vline(xintercept = 10000, linetype = "dotted", color = "blue", size = 0.7) +
  geom_hline(yintercept = 10000, linetype = "dotted", color = "blue", size = 0.7) +  # Scale y-intercept
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  labs(x = "True Divergence", y = "Estimated Mean Divergence (scaled by 1e8)") +
  theme_bw() +
  scale_x_discrete(labels = "10,000") +
  scale_y_continuous(labels = comma) +  # Auto-adjust y-axis
  theme(
    legend.position = "none",
    axis.title = element_text(size = 15, face = "bold"),
    axis.text = element_text(size = 15, face = "bold"),
    plot.title = element_text(size = 15, face = "bold")
  )










# Load the necessary libraries
library(tidyr)
library(dplyr)
library(ggplot2)
library(scales)  # For formatting axis labels

# Load the summary file
file_path <- "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001_syndiesel/scripts/results/config_1_0_160000_0.0001_results/summary_output.txt"
df_new <- read.table(file_path, header = TRUE, sep = "\t")

# Gather the Mean divergence values into long format
df_new_long_mean <- df_new %>%
  gather(key = "PopPair", value = "MeanDivergence", 
         Mean_Divergence_PopA_PopB:Mean_Divergence_PopK_PopL) %>%
  select(Run, PopPair, MeanDivergence)

# Gather the True divergence values into long format
df_new_long_true <- df_new %>%
  gather(key = "PopPair_True", value = "TrueValue", 
         True_Divergence_PopA_PopB:True_Divergence_PopK_PopL) %>%
  mutate(PopPair = gsub("True_Divergence_", "Mean_Divergence_", PopPair_True)) %>%
  select(Run, PopPair, TrueValue)

# Merge both datasets based on matching population pairs and runs
df_combined_new <- inner_join(df_new_long_mean, df_new_long_true, by = c("Run", "PopPair"))

# Scale MeanDivergence for clearer visualization
df_combined_new$MeanDivergence <- df_combined_new$MeanDivergence * 1e8  # Scale up by 1e8

# Convert TrueValue to numeric if necessary
df_combined_new$TrueValue <- as.numeric(as.character(df_combined_new$TrueValue))

# Define unique true divergence values for vertical lines
unique_true_values <- unique(df_combined_new$TrueValue)

# Plot all population pairs with properly scaled MeanDivergence
ggplot(df_combined_new, aes(x = as.factor(TrueValue), y = MeanDivergence, fill = PopPair)) +
  geom_violin(alpha = 0.7, trim = FALSE, scale = "width", adjust = 0.5, position = position_dodge(width = 0)) +
  geom_jitter(aes(color = PopPair), alpha = 0.4, size = 1, position = position_dodge(width = 0.5)) +  # Color the points by PopPair
  geom_vline(xintercept = 160000, linetype = "dotted", color = "blue", size = 0.7) +  # Update x-intercept for 80,000
  geom_hline(yintercept = 160000, linetype = "dotted", color = "blue", size = 0.7) +  # Update y-intercept with scaling
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  labs(x = "True Divergence", y = "Estimated Mean Divergence (scaled by 1e8)", fill = "Population Pair", color = "Population Pair") +
  theme_bw() +
  scale_x_discrete(labels = unique_true_values) +  # Dynamically set x-axis labels based on unique TrueValue
  scale_y_continuous(labels = comma, limits = c(0, 40000)) +  # Adjust y-axis limit based on scaling
  theme(
    legend.position = "right",  # Position the legend on the right
    axis.title = element_text(size = 15, face = "bold"),
    axis.text = element_text(size = 15, face = "bold"),
    plot.title = element_text(size = 15, face = "bold")
  )


















































# Load required libraries
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggridges)
library(patchwork)  # For combining plots

# Load the data
file_path <- "/Users/mtofflemire/Mtofflemire Dropbox/Michael Tofflemire/Projects/syndiesel/scripts/results/config_1-results/config_no-mig_1_0_10000_results/summary_output.txt"
df <- read.table(file_path, header = TRUE, sep = "\t")

### First Plot: Violin Plot ###
# Gather the Mean divergence values into long format
df_new_long_mean <- df %>%
  gather(key = "PopPair", value = "MeanDivergence", 
         Mean_Divergence_PopA_PopB:Mean_Divergence_PopK_PopL) %>%
  select(Run, PopPair, MeanDivergence)

# Gather the True divergence values into long format
df_new_long_true <- df %>%
  gather(key = "PopPair_True", value = "TrueValue", 
         True_Divergence_PopA_PopB:True_Divergence_PopK_PopL) %>%
  mutate(PopPair = gsub("True_Divergence_", "Mean_Divergence_", PopPair_True)) %>%
  select(Run, PopPair, TrueValue)

# Merge both datasets
df_combined_new <- inner_join(df_new_long_mean, df_new_long_true, by = c("Run", "PopPair"))

# Remove duplicates if necessary
df_combined_new_unique <- df_combined_new[!duplicated(df_combined_new[, c("TrueValue", "MeanDivergence")]), ]

# Divide the mean estimated values by 1e-8
df_combined_new_unique$MeanDivergence <- df_combined_new_unique$MeanDivergence / 1e-8

# Filter out rows where MeanDivergence is NA or negative
df_combined_new_unique <- df_combined_new_unique %>%
  filter(!is.na(MeanDivergence) & MeanDivergence >= 0)

# Convert TrueValue to numeric
df_combined_new_unique$TrueValue <- as.numeric(as.character(df_combined_new_unique$TrueValue))

# Create the violin plot
plot_violin <- ggplot(df_combined_new_unique, aes(x = as.factor(TrueValue), y = MeanDivergence, fill = PopPair)) +
  geom_violin(alpha = 0.7, trim = FALSE, scale = "width", adjust = 0.5, position = position_dodge(width = 0)) +
  geom_hline(yintercept = 80000, linetype = "dashed", color = "blue", size = 1, inherit.aes = FALSE) +
  geom_jitter(aes(color = PopPair), alpha = 0.4, size = 1, position = position_dodge(width = 0.5)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  labs(
       x = "True Divergence", 
       y = "Estimated Mean Divergence", 
       fill = "Population Pair", 
       color = "Population Pair") +
  theme_gray() +
  scale_y_continuous(labels = scales::comma, limits = c(0, 350000), expand = c(0, 0)) +  # Remove default padding
  theme(
    legend.position = "none",
    legend.title = element_text(size = 7),
    legend.text = element_text(size = 6),
    legend.key.size = unit(0.3, "cm"),
    legend.box = "vertical",
    legend.box.spacing = unit(0.1, "cm"),
    axis.title.y = element_text(size = 12, margin = margin(r = 0)),  # Bring Y-axis label closer
    axis.text.y = element_text(size = 10, margin = margin(r = 0)),    # Keep numbers close to axis
    axis.title.x = element_text(size = 12),
    axis.text.x = element_text(size = 10),
    plot.margin = margin(10, 10, 10, 10),  # Adjust plot margins
    panel.grid.major = element_line(color = "white"),  # Keep major gridlines
    panel.grid.minor = element_line(color = "white"),  # Keep minor gridlines
    panel.border =  = element_line(color = "black")  # Add x and y axis lines
  )

plot_violin


### Second Plot: Ridgeline Plot ###
# Event types for probabilities
event_types <- c("Prob_1_Event", "Prob_2_Events", "Prob_3_Events", "Prob_4_Events", "Prob_5_Events", "Prob_6_Events")

# Reshape the data into a long format
probabilities_long_df <- df %>%
  gather(key = "EventType", value = "Probability", all_of(event_types))

# Find the event type with the highest maximum probability
highest_prob_event <- probabilities_long_df %>%
  group_by(EventType) %>%
  summarize(max_prob = max(Probability)) %>%
  filter(max_prob == max(max_prob)) %>%
  pull(EventType)

# Create a new column to define colors for each event type
probabilities_long_df <- probabilities_long_df %>%
  mutate(Color = case_when(
    EventType == "Prob_1_Events" ~ "orange",
    EventType == highest_prob_event ~ "orange",
    TRUE ~ "gray"
  ))

# Create the ridgeline plot
plot_ridge <- ggplot(probabilities_long_df, aes(x = Probability, y = EventType, fill = Color)) +
  geom_density_ridges(alpha = 0.7, scale = 10, rel_min_height = 0.01, color = "black", size = 0.25) +
  scale_fill_identity() +
  labs(
    x = "Probability", 
    y = "Event Type") +
  theme_gray() +
  theme(
    legend.position = "none",
    axis.title.y = element_text(size = 12, margin = margin(r = 1)),  # Bring y-axis title closer
    axis.text.y = element_text(size = 10, margin = margin(r = -5)),  # Bring y-axis numbers closer
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    plot.margin = margin(10, 5, 10, 5),  # Reduce plot margins
    panel.grid.major = element_line(color = "white"),  # Keep major gridlines
    panel.grid.minor = element_line(color = "white"),  # Keep minor gridlines
    axis.line = element_line(color = "black")  # Add x and y axis lines
  ) +
  coord_cartesian(xlim = c(-0.08, 1))



plot_ridge

# Combine Plots
combined_plot <- plot_violin / plot_ridge
combined_plot









# Save Combined Plot
ggsave("/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001-syndiesel/scripts/results/config_1/config_no-mig_1_0_80000_results/combined_plot.png", 
       plot = combined_plot, width =4, height = 6.5, dpi = 300)
























# Load required libraries
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggridges)
library(patchwork)  # For combining plots

# Load the data
file_path <- "/Users/mtofflemire/Mtofflemire Dropbox/Michael Tofflemire/Projects/syndiesel/scripts/results/config_1-results/config_no-mig_1_0_10000_results/summary_output.txt"
df <- read.table(file_path, header = TRUE, sep = "\t")

### First Plot: Violin Plot ###
# Gather the Mean divergence values into long format
df_new_long_mean <- df %>%
  gather(key = "PopPair", value = "MeanDivergence", 
         Mean_Divergence_PopA_PopB:Mean_Divergence_PopK_PopL) %>%
  select(Run, PopPair, MeanDivergence)

# Gather the True divergence values into long format
df_new_long_true <- df %>%
  gather(key = "PopPair_True", value = "TrueValue", 
         True_Divergence_PopA_PopB:True_Divergence_PopK_PopL) %>%
  mutate(PopPair = gsub("True_Divergence_", "Mean_Divergence_", PopPair_True)) %>%
  select(Run, PopPair, TrueValue)

# Merge both datasets
df_combined_new <- inner_join(df_new_long_mean, df_new_long_true, by = c("Run", "PopPair"))

# Remove duplicates if necessary
df_combined_new_unique <- df_combined_new[!duplicated(df_combined_new[, c("TrueValue", "MeanDivergence")]), ]

# Divide the mean estimated values by 1e-8
df_combined_new_unique$MeanDivergence <- df_combined_new_unique$MeanDivergence / 1e-8

# Filter out rows where MeanDivergence is NA or negative
df_combined_new_unique <- df_combined_new_unique %>%
  filter(!is.na(MeanDivergence) & MeanDivergence >= 0)

# Convert TrueValue to numeric
df_combined_new_unique$TrueValue <- as.numeric(as.character(df_combined_new_unique$TrueValue))

# Violin Plot
# Violin Plot
plot_violin <- ggplot(df_combined_new_unique, aes(x = as.factor(TrueValue), y = MeanDivergence, fill = PopPair)) +
  geom_violin(alpha = 0.7, trim = FALSE, scale = "width", adjust = 0.5, position = position_dodge(width = 0)) +
  geom_hline(yintercept = 160000, linetype = "dashed", color = "blue", size = 1, inherit.aes = FALSE) +
  geom_jitter(aes(color = PopPair), alpha = 0.4, size = 1, position = position_dodge(width = 0.5)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  labs(
    x = "True Divergence", 
    y = "Estimated Mean Divergence", 
    fill = "Population Pair", 
    color = "Population Pair") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 350000), expand = c(0, 0)) +  # Remove default padding
  theme_gray() +
  theme(
    legend.position = "none",
    legend.title = element_text(size = 7),
    legend.text = element_text(size = 6),
    legend.key.size = unit(0.3, "cm"),
    axis.title.y = element_text(size = 12, margin = margin(r = 0)),  # Y-axis label size
    axis.text.y = element_text(size = 14, margin = margin(r = 0)),   # Larger Y-axis numbers
    axis.title.x = element_text(size = 12),                         # X-axis label size
    axis.text.x = element_text(size = 14),                          # Larger X-axis numbers
    plot.margin = margin(10, 10, 10, 10),  # Adjust plot margins
    panel.grid.major = element_blank(),  # No gridlines
    panel.grid.minor = element_blank(),  # No gridlines
    panel.border = element_rect(color = "black", fill = NA, size = 1)  # Add black border
  )


### Second Plot: Ridgeline Plot ###
# Event types for probabilities
event_types <- c("Prob_1_Event", "Prob_2_Events", "Prob_3_Events", "Prob_4_Events", "Prob_5_Events", "Prob_6_Events")

# Reshape the data into a long format
probabilities_long_df <- df %>%
  gather(key = "EventType", value = "Probability", all_of(event_types))

# Find the event type with the highest maximum probability
highest_prob_event <- probabilities_long_df %>%
  group_by(EventType) %>%
  summarize(max_prob = max(Probability)) %>%
  filter(max_prob == max(max_prob)) %>%
  pull(EventType)

# Create a new column to define colors for each event type
probabilities_long_df <- probabilities_long_df %>%
  mutate(Color = case_when(
    EventType == "Prob_1_Events" ~ "orange",
    EventType == highest_prob_event ~ "orange",
    TRUE ~ "gray"
  ))


# Ridgeline Plot
plot_ridge <- ggplot(probabilities_long_df, aes(x = Probability, y = EventType, fill = Color)) +
  geom_density_ridges(alpha = 0.7, scale = 10, rel_min_height = 0.01, color = "black", size = 0.25) +
  scale_fill_identity() +
  labs(
    x = "Probability", 
    y = "Event Type") +
  theme_gray() +
  theme(
    legend.position = "none",
    axis.title.y = element_text(size = 12, margin = margin(r = 1)),  # Y-axis label size
    axis.text.y = element_text(size = 14, margin = margin(r = 0)),   # Larger Y-axis numbers
    axis.title.x = element_text(size = 12),                         # X-axis label size
    axis.text.x = element_text(size = 14),                          # Larger X-axis numbers
    plot.margin = margin(10, 5, 10, 5),  # Reduce plot margins
    panel.grid.major = element_blank(),  # No gridlines
    panel.grid.minor = element_blank(),  # No gridlines
    panel.border = element_rect(color = "black", fill = NA, size = 1)  # Add black border
  ) +
  coord_cartesian(xlim = c(-0.08, 1))


# Combine plots
combined_plot <- plot_violin / plot_ridge
combined_plot

# Save combined plot
#ggsave("/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001-syndiesel/scripts/results/config_1/config_no-mig_1_0_80000_results/combined_plot.png", 
       plot = combined_plot, width = 4, height = 6.5, dpi = 300)




























# Install the patchwork package if not already installed
if (!requireNamespace("patchwork", quietly = TRUE)) {
  install.packages("patchwork")
}

library(patchwork)

# Combine the plots using patchwork
combined_plot <- my_plot / plot_prob

# Display the combined plot
combined_plot



####################################
########### Libraries ##############
####################################
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggridges)
library(scales)  # For formatting axis labels

####################################
########### Ridgeline Plot #########
####################################

# Load the data
file_path <- "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001-syndiesel/scripts/results/config_6-results/config_6_160000_10000_0.0000001_results/summary_output.txt"
df <- read.table(file_path, header = TRUE, sep = "\t")
df
# Event types for probabilities
event_types <- c("Prob_1_Event", "Prob_2_Events", "Prob_3_Events", "Prob_4_Events", "Prob_5_Events", "Prob_6_Events")

# Reshape the data into a long format
probabilities_long_df <- df %>%
  gather(key = "EventType", value = "Probability", all_of(event_types))

# Find the event type with the highest maximum probability
highest_prob_event <- probabilities_long_df %>%
  group_by(EventType) %>%
  summarize(max_prob = max(Probability)) %>%
  filter(max_prob == max(max_prob)) %>%
  pull(EventType)

# Create a new column to define colors for each event type
probabilities_long_df <- probabilities_long_df %>%
  mutate(Color = case_when(
    EventType == "Prob_6_Events" ~ "lightblue",
    EventType == highest_prob_event ~ "orange",
    TRUE ~ "gray"
  ))

# Create a ridgeline plot
plot_prob <- ggplot(probabilities_long_df, aes(x = Probability, y = EventType, fill = Color)) +
  geom_density_ridges(alpha = 0.7, scale = 10, rel_min_height = 0.01, color = "black", size = 0.25) +
  scale_fill_identity() +  # Use the colors defined in the dataframe directly
  labs(
       x = "Probability", 
       y = "Event Type") +
  theme_gray() +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 12),  # Adjust axis title size
    axis.text = element_text(size = 10),   # Adjust axis tick size
    panel.border = element_rect(color = "black", fill = NA, size = 1)  # Add black border
  ) +
  coord_cartesian(xlim = c(-0.08, 1))  # Set x-axis limits
plot_prob


####################################
########### Violin Plot ############
####################################

# Load the summary file
file_path <- "/Users/mtofflemire/Mtofflemire Dropbox/Michael Tofflemire/Projects/syndiesel/scripts/results/config_1-results/config_no-mig_1_0_10000_results/summary_output.txt"

df_new <- read.table(file_path, header = TRUE, sep = "\t")

# Gather the Mean divergence values into long format
df_new_long_mean <- df_new %>%
  gather(key = "PopPair", value = "MeanDivergence", 
         Mean_Divergence_PopA_PopB:Mean_Divergence_PopK_PopL) %>%
  select(Run, PopPair, MeanDivergence)

# Gather the True divergence values into long format
df_new_long_true <- df_new %>%
  gather(key = "PopPair_True", value = "TrueValue", 
         True_Divergence_PopA_PopB:True_Divergence_PopK_PopL) %>%
  mutate(PopPair = gsub("True_Divergence_", "Mean_Divergence_", PopPair_True)) %>%
  select(Run, PopPair, TrueValue)

# Merge both datasets based on matching population pairs and runs
df_combined_new <- inner_join(df_new_long_mean, df_new_long_true, by = c("Run", "PopPair"))

# Remove duplicates if necessary
df_combined_new_unique <- df_combined_new[!duplicated(df_combined_new[, c("TrueValue", "MeanDivergence")]), ]

# Divide the mean estimated values by 1e-8
df_combined_new_unique$MeanDivergence <- df_combined_new_unique$MeanDivergence / 1e-8

# Filter out rows where MeanDivergence is NA or negative
df_combined_new_unique <- df_combined_new_unique %>%
  filter(!is.na(MeanDivergence) & MeanDivergence >= 0)

# Convert TrueValue to a numeric variable
df_combined_new_unique$TrueValue <- as.numeric(as.character(df_combined_new_unique$TrueValue))

# Set the axis limit
axis_limit <- 900000

# Get unique true divergence values for vertical lines
unique_true_values <- unique(df_combined_new_unique$TrueValue)

# Create the violin plot
my_plot <- ggplot(df_combined_new_unique, aes(x = TrueValue, y = MeanDivergence, group = PopPair, fill = PopPair)) +
  geom_violin(alpha = 0.7, trim = FALSE, scale = "area", adjust = 3, position = position_identity()) +
  geom_jitter(width = 0.4, alpha = 0.4, size = 1, color = "black") +
  geom_vline(xintercept = unique_true_values, linetype = "dotted", color = "blue", size = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  labs(x = "True Divergence", y = "Estimated Mean Divergence (x 10^-8)") +
  theme_gray() +
  coord_cartesian(xlim = c(0, axis_limit), ylim = c(0, axis_limit)) +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 12),  # Adjust axis title size
    axis.text = element_text(size = 10),   # Adjust axis tick size
    panel.border = element_rect(color = "black", fill = NA, size = 1)  # Add black border
  )
my_plot





# Combine plots
combined_plot <- my_plot / plot_prob
combined_plot





# Specify the file path and file name
output_file <- "/Users/michaeltofflemire/Library/CloudStorage/Dropbox/sites/storage/local/projects/20241001-syndiesel/scripts/results/config_6_160000_10000_0.0000001_results/combined_plot.png"  # Replace with your desired file path

# Save the plot with high resolution
ggsave(
  filename = output_file,
  plot = combined_plot,  # The plot to save
  device = "png",        # Save as PNG
  width = 5,            # Width in inches (adjust as needed)
  height = 8,            # Height in inches (adjust as needed)
  dpi = 300              # Resolution in dots per inch (higher value = better quality)
)

# Confirmation message
cat("Plot saved successfully to", output_file, "\n")





































####################################
################################
install.packages("ggridges")
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggridges)

# Load the data
file_path <- "/Users/mtofflemire/Mtofflemire Dropbox/Michael Tofflemire/Projects/syndiesel/scripts/results/config_3-results/config_3_no-mig_3_10000_10000_results/summary_output.txt"
df <- read.table(file_path, header = TRUE, sep = "\t")

# Event types
event_types <- c("Prob_1_Event", "Prob_2_Events", "Prob_3_Events",
                 "Prob_4_Events", "Prob_5_Events", "Prob_6_Events")

# Long format
probabilities_long_df <- df %>%
  gather(key = "EventType", value = "Probability", all_of(event_types))

# Highest-probability event
highest_prob_event <- probabilities_long_df %>%
  group_by(EventType) %>%
  summarize(max_prob = max(Probability)) %>%
  filter(max_prob == max(max_prob)) %>%
  pull(EventType)

# Colors  (ONLY CHANGE IS ADDING Prob_3_Events = "green")
probabilities_long_df <- probabilities_long_df %>%
  mutate(Color = case_when(
    EventType == "Prob_3_Events" ~ "green",          # TRUE number of events → green
    EventType == "Prob_1_Events" ~ "hotpink",
    EventType == highest_prob_event ~ "hotpink",
    TRUE ~ "gray"
  ))

probabilities_long_df$EventType <- factor(probabilities_long_df$EventType,
                                          levels = event_types)

# ---- Plot ----
plot_prob <- ggplot(probabilities_long_df,
                    aes(x = Probability, y = EventType, fill = Color)) +
  geom_density_ridges(
    alpha = 0.7,
    scale = 10,
    rel_min_height = 0.01,
    color = "black",
    size = 0.25
  ) +
  scale_y_discrete(
    breaks = event_types,
    labels = 1:length(event_types)
  ) +
  scale_fill_identity() +
  labs(
    x = "Probability",
    y = "Event Type"
  ) +
  theme_minimal() +
  theme(
    # Horizontal gridlines ONLY
    panel.grid.major.y = element_line(color = "grey80", size = 0.3),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    
    # Border
    panel.border = element_rect(color = "black", fill = NA, size = 0.8),
    
    legend.position = "none"
  ) +
  coord_cartesian(xlim = c(-0.08, 1))

plot_prob














####################################
################################
install.packages("ggridges")
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggridges)

# Load the data
file_path <- "/Users/mtofflemire/Mtofflemire Dropbox/Michael Tofflemire/Projects/syndiesel/scripts/results/config_6-results/config_no-mig_6_160000_10000_results/summary_output.txt"
df <- read.table(file_path, header = TRUE, sep = "\t")

# Event types
event_types <- c("Prob_1_Event", "Prob_2_Events", "Prob_3_Events",
                 "Prob_4_Events", "Prob_5_Events", "Prob_6_Events")

# Long format
probabilities_long_df <- df %>%
  gather(key = "EventType", value = "Probability", all_of(event_types))

# --- Event with highest *mean* probability ---
highest_mean_event <- probabilities_long_df %>%
  group_by(EventType) %>%
  summarize(mean_prob = mean(Probability, na.rm = TRUE)) %>%
  filter(mean_prob == max(mean_prob)) %>%
  pull(EventType)

# Colors
probabilities_long_df <- probabilities_long_df %>%
  mutate(Color = case_when(
    EventType == "Prob_6_Events" ~ "green",          # true number of events
    EventType == highest_mean_event ~ "hotpink",# highest mean probability
    TRUE ~ "gray"
  ))

probabilities_long_df$EventType <- factor(probabilities_long_df$EventType,
                                          levels = event_types)

# ---- Plot ----
plot_prob <- ggplot(probabilities_long_df,
                    aes(x = Probability, y = EventType, fill = Color)) +
  geom_density_ridges(
    alpha = 0.7,
    scale = 10,
    rel_min_height = 0.01,
    color = "black",
    size = 0.25
  ) +
  scale_y_discrete(
    breaks = event_types,
    labels = 1:length(event_types)
  ) +
  scale_fill_identity() +
  labs(
    x = "Probability",
    y = "Event Type"
  ) +
  theme_minimal() +
  theme(
    # Horizontal gridlines ONLY
    panel.grid.major.y = element_line(color = "grey80", size = 0.3),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    
    # Border
    panel.border = element_rect(color = "black", fill = NA, size = 0.8),
    
    legend.position = "none"
  ) +
  coord_cartesian(xlim = c(-0.08, 1))

plot_prob





























####################################
################################
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggridges)

# Load the data
file_path <- "/Users/mtofflemire/Mtofflemire Dropbox/Michael Tofflemire/Projects/syndiesel/scripts/results/config_3-results/config_3_no-mig_3_20000_10000_results/summary_output.txt"
df <- read.table(file_path, header = TRUE, sep = "\t")

# Event types
event_types <- c("Prob_1_Event", "Prob_2_Events", "Prob_3_Events",
                 "Prob_4_Events", "Prob_5_Events", "Prob_6_Events")

# Long format
probabilities_long_df <- df %>%
  gather(key = "EventType", value = "Probability", all_of(event_types))

# --- Highest mean probability event ---
highest_mean_event <- probabilities_long_df %>%
  group_by(EventType) %>%
  summarize(mean_prob = mean(Probability, na.rm = TRUE)) %>%
  filter(mean_prob == max(mean_prob)) %>%
  pull(EventType)

# Create a categorical fill variable
probabilities_long_df <- probabilities_long_df %>%
  mutate(FillGroup = case_when(
    EventType == "Prob_3_Events" ~ "True # Events",
    EventType == highest_mean_event ~ "Highest Mean Probability",
    TRUE ~ "Other Event Types"
  ))

# Set factor order for legend
probabilities_long_df$FillGroup <- factor(
  probabilities_long_df$FillGroup,
  levels = c("True # Events", "Highest Mean Probability", "Other Event Types")
)

probabilities_long_df$EventType <- factor(probabilities_long_df$EventType,
                                          levels = event_types)

# ---- Plot ----
plot_prob <- ggplot(probabilities_long_df,
                    aes(x = Probability, y = EventType, fill = FillGroup)) +
  geom_density_ridges(
    alpha = 0.7,
    scale = 10,
    rel_min_height = 0.01,
    color = "black",
    size = 0.25
  ) +
  scale_y_discrete(
    breaks = event_types,
    labels = 1:length(event_types)
  ) +
  scale_fill_manual(
    name = "Event Highlight",
    values = c(
      "True # Events" = "green",
      "Highest Mean Probability" = "hotpink",
      "Other Event Types" = "gray"
    )
  ) +
  labs(
    x = "Probability",
    y = "Event Type"
  ) +
  theme_minimal() +
  theme(
    # Horizontal gridlines ONLY
    panel.grid.major.y = element_line(color = "grey80", size = 0.3),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    
    # Border
    panel.border = element_rect(color = "black", fill = NA, size = 0.8),
    
    legend.position = "right"
  ) +
  coord_cartesian(xlim = c(-0.08, 1))

plot_prob


























####################################
################################
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggridges)

# Load data
file_path <- '/Users/michaeltofflemire/Mtofflemire Dropbox/Michael Tofflemire/Projects/syndiesel/scripts/results/config_1-results/config_1_0_10000_0.0000001_results/summary_output.txt'
df <- read.table(file_path, header = TRUE, sep = "\t")

# Event types
event_types <- c("Prob_1_Event", "Prob_2_Events", "Prob_3_Events",
                 "Prob_4_Events", "Prob_5_Events", "Prob_6_Events")

# Long format
probabilities_long_df <- df %>%
  gather(key = "EventType", value = "Probability", all_of(event_types))

# True event
true_event <- "Prob_1_Event"

# Highest *mean* probability
highest_mean_event <- probabilities_long_df %>%
  group_by(EventType) %>%
  summarise(mean_prob = mean(Probability, na.rm = TRUE)) %>%
  filter(mean_prob == max(mean_prob)) %>%
  pull(EventType)

# Fill groups with overlap logic
probabilities_long_df <- probabilities_long_df %>%
  mutate(FillGroup = case_when(
    EventType == true_event & EventType == highest_mean_event ~ "True + Highest",
    EventType == true_event ~ "True Only",
    EventType == highest_mean_event ~ "Highest Mean Only",
    TRUE ~ "Other"
  ))

# Factor order
probabilities_long_df$EventType <- factor(probabilities_long_df$EventType, levels = event_types)

probabilities_long_df$FillGroup <- factor(
  probabilities_long_df$FillGroup,
  levels = c("True + Highest", "True Only", "Highest Mean Only", "Other")
)

# Colors
fill_colors <- c(
  "True + Highest"    = "purple4",     # unified color for overlap
  "True Only"         = "green",
  "Highest Mean Only" = "hotpink",
  "Other"             = "gray"
)

# ---- Plot ----
plot_prob <- ggplot(probabilities_long_df,
                    aes(x = Probability, y = EventType, fill = FillGroup)) +
  geom_density_ridges(
    alpha = 0.7,
    scale = 10,
    rel_min_height = 0.01,
    color = "black",
    size = 0.25
  ) +
  scale_y_discrete(
    breaks = event_types,
    labels = 1:length(event_types)
  ) +
  scale_fill_manual(
    values = fill_colors
  ) +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
    # Horizontal gridlines ONLY
    panel.grid.major.y = element_line(color = "grey80", size = 0.3),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    
    # Border
    panel.border = element_rect(color = "black", fill = NA, size = 0.8),
    
    # REMOVE LEGEND
    legend.position = "none"
  ) +
  coord_cartesian(xlim = c(-0.08, 1))

plot_prob























library(ggplot2)
library(cowplot)
library(grid)

# Dummy data to generate legend
dummy_df <- data.frame(
  FillGroup = factor(
    c("True # events = Highest mean probability",
      "True # events",
      "Highest mean probability",
      "Other events"),
    levels = c("True # events = Highest mean probability",
               "True # events",
               "Highest mean probability",
               "Other events")
  ),
  x = 1,
  y = 1
)

# Colors – names MUST match levels in FillGroup exactly
fill_colors <- c(
  "True # events = Highest mean probability" = "purple4",
  "True # events"                            = "green",
  "Highest mean probability"                 = "hotpink",
  "Other events"                             = "gray"
)

# Dummy plot to create a legend of color squares
dummy_plot <- ggplot(dummy_df, aes(x = x, y = y, fill = FillGroup)) +
  geom_tile(width = 0.8, height = 0.8) +      # square color boxes
  scale_fill_manual(
    name = "Key",
    values = fill_colors
  ) +
  guides(
    fill = guide_legend(nrow = 1)
  ) +
  theme_void() +
  theme(
    legend.position   = "bottom",
    legend.direction  = "horizontal",
    legend.title      = element_text(size = 12, face = "bold"),
    legend.text       = element_text(size = 11)
  )

# Extract only the legend
legend_only <- get_legend(dummy_plot)

# Print legend only
grid.newpage()
grid.draw(legend_only)
































####################################
########### Violin Plot ############
####################################

library(tidyr)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyr)
library(dplyr)
library(ggplot2)
library(scales)

# Load the summary file
file_path <- '/Users/michaeltofflemire/Mtofflemire Dropbox/Michael Tofflemire/Projects/syndiesel/scripts/results/config_6-results/config_mig-1e-2_6_160000_10000_0.01_results/summary_output.txt'
df_new <- read.table(file_path, header = TRUE, sep = "\t")

# Gather the Mean divergence values into long format
df_new_long_mean <- df_new %>%
  gather(key = "PopPair", value = "MeanDivergence",
         Mean_Divergence_PopA_PopB:Mean_Divergence_PopK_PopL) %>%
  select(Run, PopPair, MeanDivergence)

# Gather the True divergence values into long format
df_new_long_true <- df_new %>%
  gather(key = "PopPair_True", value = "TrueValue",
         True_Divergence_PopA_PopB:True_Divergence_PopK_PopL) %>%
  mutate(PopPair = gsub("True_Divergence_", "Mean_Divergence_", PopPair_True)) %>%
  select(Run, PopPair, TrueValue)

# Merge both datasets
df_combined_new <- inner_join(df_new_long_mean, df_new_long_true,
                              by = c("Run", "PopPair"))

# Remove duplicates
df_combined_new_unique <- df_combined_new[!duplicated(df_combined_new[, c("TrueValue", "MeanDivergence")]), ]

# Divide mean estimates by 1e-8
df_combined_new_unique$MeanDivergence <- df_combined_new_unique$MeanDivergence / 1e-8

# Filter out invalid rows
df_combined_new_unique <- df_combined_new_unique %>%
  filter(!is.na(MeanDivergence) & MeanDivergence >= 0)

# Convert TrueValue to numeric
df_combined_new_unique$TrueValue <- as.numeric(as.character(df_combined_new_unique$TrueValue))

# Axis limit
axis_limit <- 900000

# Unique true divergence values
unique_true_values <- unique(df_combined_new_unique$TrueValue)

# Build the plot
my_plot <- ggplot(df_combined_new_unique,
                  aes(x = TrueValue, y = MeanDivergence, group = PopPair)) +
  geom_violin(fill = "gray70", 
              color = NA,          # <<< REMOVES VIOLIN OUTLINE
              alpha = 1, 
              trim = FALSE, 
              scale = "area",
              adjust = 3, 
              position = position_identity()) +
  geom_jitter(width = 0.4, alpha = 1, size = 1, color = "black") +
  geom_vline(xintercept = unique_true_values, linetype = "dotted",
             color = "gray20", size = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",
              color = "black") +
  labs(
    x = NULL,
    y = NULL
  ) +
  coord_cartesian(xlim = c(0, axis_limit),
                  ylim = c(0, axis_limit)) +
  
  # Short 10K / 50K / 100K format
  scale_x_continuous(labels = label_number(scale_cut = cut_short_scale())) +
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
  
  theme_bw() +
  theme(
    legend.position = "none",
    axis.text = element_text(size = 10),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
  )

my_plot


