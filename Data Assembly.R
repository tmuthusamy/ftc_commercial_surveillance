#Reading in Initial Data Frame
setwd("~/Privacy Research")
initial_dataframe <- read.csv("Data - Data.csv")

#Creating a Data Frame to Read PDF/Word Documents
# Load necessary libraries
library(pdftools)
library(readtext)
library(dplyr)

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

# Print the resulting data frame
print(file_contents)

file_contents$Document.ID <- substr(file_contents$file_name, 1, 18)
full_dataframe <- merge(initial_dataframe, file_contents, by = "Document.ID")
#removed files that didn't have attachments--figure out how to add them back in!