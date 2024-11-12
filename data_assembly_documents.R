#Reading in Initial Data Frame
setwd("~/Privacy Research")
initial_dataframe <- read.csv("Data - Data.csv")

#Creating a Data Frame to Read PDF/Word Documents
# Load necessary libraries
library(pdftools)
library(readtext)
library(dplyr)
library(tidyverse)

# Function to read a PDF file and extract text
read_pdf_text <- function(file_path) {
  pdf_text(file_path) %>%
    paste(collapse = "\n")
}

# Function to read a Word file and extract text
read_word_text <- function(file_path) {
  readtext(file_path)$text
}

# Specify the folder containing the files
folder_path <- "downloads"

# List all PDF and Word files in the folder
file_list <- list.files(folder_path, pattern = "\\.(pdf|docx?)$", full.names = TRUE, ignore.case = TRUE)

# Initialize an empty data frame
file_contents <- data.frame(file_name = character(), text = character(), stringsAsFactors = FALSE)

# Loop through each file and read the contents
for (file in file_list) {
  # Extract the file name
  file_name <- basename(file)
  
  # Read the file based on its extension
  if (grepl("\\.pdf$", file, ignore.case = TRUE)) {
    file_text <- read_pdf_text(file)
  } else if (grepl("\\.docx?$", file, ignore.case = TRUE)) {
    file_text <- read_word_text(file)
  } else {
    next
  }
  
  # Add the file name and text to the data frame
  file_contents <- file_contents %>%
    add_row(file_name = file_name, text = file_text)
}

file_contents$Document.ID <- substr(file_contents$file_name, 1, 18)
full_dataframe <- merge(initial_dataframe, file_contents, by = "Document.ID")

initial_ids <- full_dataframe$Document.ID
subset_dataframe_base <- initial_dataframe[!initial_dataframe$Document.ID %in% initial_ids, ]

subset_dataframe_base$file_name <- NA_character_
subset_dataframe_base$text <- NA_character_

final_dataframe_1 <- rbind(full_dataframe, subset_dataframe_base)
final_dataframe_1$doc_number <- substr(final_dataframe_1$Document.ID, 15, 18)
final_dataframe_1 <- final_dataframe_1 %>%
  arrange(doc_number) %>%
  select(1, 10, 41, 42, 44, 46, 47, 53, 61, 62)

df <- final_dataframe_1

unique_text_check <- df %>%
  group_by(Document.ID) %>%
  summarise(all_unique = n_distinct(text) == n())

non_unique_texts <- unique_text_check %>%
  filter(!all_unique)

print(non_unique_texts)

filtered_df <- df %>%
  filter(Document.ID %in% non_unique_texts$Document.ID) %>%
  group_by(Document.ID) %>%
  slice(1) %>%
  ungroup()

filtered_df_2 <- df %>%
  filter(!Document.ID %in% non_unique_texts$Document.ID)

filtered_df_final <- rbind(filtered_df, filtered_df_2)

saveRDS(filtered_df_final, "filtered_df_final.RDS")
df <- filtered_df_final %>%
  group_by(Document.ID) %>%
  mutate(row_num = row_number()) %>%
  ungroup()

# Spread the data so each Document.ID has one row with multiple text columns
df_wide <- df %>%
  pivot_wider(id_cols = Document.ID, names_from = row_num, values_from = text, names_prefix = "text_")

df_2 <- df %>%
  select(1:8)
df_2 <- df_2[!duplicated(df_2),]

final_df_1 <- merge(df_wide, df_2, by = "Document.ID")
final_df <- final_df_1[, c("Document.ID", "Title", "First.Name", "Last.Name", "State.Province", "Country", "Organization.Name", "Comment", 
                           "text_1", "text_2", "text_3", "text_4", "text_5", "text_6", "text_7", "text_8","text_9", "text_10", "text_11",
                           "text_12", "text_13", "text_14", "text_15", "text_16", "text_17", "text_18", "text_19")]

saveRDS(final_df, "final_dataframe.RDS")
write.csv(final_df, "final_dataframe.csv")
