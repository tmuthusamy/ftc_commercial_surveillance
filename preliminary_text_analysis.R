df1 = readRDS("final_dataframe_1.rds")

#Subsetting comments under two sentences
library(stringr)
library(tidyverse)
count_sentences <- function(words) {
  sentences <- unlist(str_split(words, "(?<=[.!?])\\s+"))
  return(length(sentences))
}

df1$comment_sentence_count <- sapply(df1$Comment, count_sentences)
df1$text_sentence_count <- sapply(df1$text, count_sentences)
df1$sentence_number <- case_when(
  df1$comment_sentence_count >= 2 ~ "Important",
  df1$comment_sentence_count >= 2 ~ "Important",
  TRUE ~ "Trivial"
)

df2 = df1 %>%
  filter(df1$sentence_number == "Important")

#Analyzing organization names
organizations <- unique(df1$Organization.Name[df1$Organization.Name != ""])
organizations
