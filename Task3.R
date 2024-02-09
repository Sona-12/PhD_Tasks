# Parse command line arguments
args <- commandArgs(trailingOnly = TRUE)

# Check if the correct number of arguments is provided
if (length(args) != 2) {
  stop("Usage: Rscript Task3.R <gene_info_input_file> <output_file>")
}

library(readr)
library(ggplot2)
library(dplyr)

df <- read.delim(gzfile(args[1]), header = TRUE)

# Extract rows 2, 3, and 5 into subdf
subdf <- df[, c(3,7)]
# head(subdf)

# Define the order of chromosome levels
chromosome_order <- c(1:22, "X", "Y", "MT", "Un")

# Filter out rows with ambiguous chromosome values and reorder chromosome levels
filtered_data <- subdf %>%
  filter(!grepl("\\|", chromosome) & chromosome != "-") %>%
  mutate(Chromosome = factor(chromosome, levels = chromosome_order))


unique_chromosomes <- unique(filtered_data$chromosome)
print(unique_chromosomes)

# Count the number of genes 
gene_counts <- filtered_data %>%
  group_by(chromosome) %>%
  summarize(GeneCount = n())

# Create the plot
plot <- ggplot(gene_counts, aes(x = factor(chromosome, levels = chromosome_order), y = GeneCount)) +
  geom_bar(stat = "identity", fill = "grey14") +
  labs(title = "Number of Genes in each Chromosome", x = "Chromosomes", y = "Gene Count") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", color = "black"),
    axis.title.x = element_text(face = "bold", color = "black"),
    axis.title.y = element_text(face = "bold", color = "black")
  )

ggsave(paste0(args[2], ".pdf"), plot, width = 8, height = 6)