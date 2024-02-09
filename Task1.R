# Parse command line arguments
args <- commandArgs(trailingOnly = TRUE)

# Check if the correct number of arguments is provided
if (length(args) != 3) {
  stop("Usage: Rscript Task1.R <gene_info_input_file> <gmt_input_file> <output_file>")
}

# Load required libraries
library(tidyr)
library(dplyr)
library(tidyverse)
library(readr)

df = read.delim(gzfile(args[1]), header = T)

# Extract rows 2, 3, and 5 into subdf
subdf <- df[, c(2,3,5)]
# head(subdf)

# Function to split synonyms and create new rows
split_synonyms <- function(data) {
  data %>%
    mutate(Synonyms = strsplit(Synonyms, "\\|")) %>%
    unnest(Synonyms) %>%
    arrange(GeneID)
}

table2 <- split_synonyms(subdf)

# Separate GeneID and Symbol_Synonyms into different data frames
geneid_df <- data.frame(GeneID = table2$GeneID)
symbol_synonyms_df <- data.frame(Symbol_Synonyms = c(table2$Symbol, table2$Synonyms))

# Combine the two data frames by rows
output_data <- cbind(geneid_df, symbol_synonyms_df)

# Sort unique rows
sorted_df <- output_data %>%
  distinct() %>%
  arrange(GeneID, Symbol_Synonyms)

# head(sorted_df)

df2 = read.delim(args[2], sep ="\t", header = F)

# Extract columns 3 till the end of the df
cols_to_replace <- names(df2)[3:ncol(df2)]

# Replace values using indexing and saving file
df2[cols_to_replace] <- sapply(df2[cols_to_replace], function(x) sorted_df$GeneID[match(x, sorted_df$Symbol_Synonyms)])
write.csv(df2, (paste0(args[3],".gmt")), row.names = FALSE)