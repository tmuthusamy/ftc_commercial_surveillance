library(httr)
library(jsonlite)
library(dplyr)

get_image_description <- function(image_path, api_key, api_url) {
  image_data <- upload_file(image_path)
  response <- POST(
    api_url,
    add_headers(Authorization = paste("Bearer", api_key)),
    body = list(file = image_data),
    encode = "multipart"
  )
  
  if (status_code(response) == 200) {
    response_content <- content(response, as = "text")
    response_json <- fromJSON(response_content)
    return(response_json$description)
  } else {
    warning(paste("Error:", status_code(response), "for file:", image_path))
    return(NA)
  }
}

# Replace with your actual API key and endpoint URL
api_key <- "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"
api_url <- "https://api.openai.com/v1/chat/completions"

# Path to the folder containing images
image_folder <- "image_attachments/"

# List all image files in the folder
image_files <- list.files(image_folder, full.names = TRUE)

# Initialize an empty data frame to store results
results <- data.frame(
  image_path = character(),
  description = character(),
  stringsAsFactors = FALSE
)

# Loop over each image file and get descriptions
for (image_file in image_files) {
  description <- get_image_description(image_file, api_key, api_url)
  results <- rbind(results, data.frame(image_path = image_file, description = description, stringsAsFactors = FALSE))
}




