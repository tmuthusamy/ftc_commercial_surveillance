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
