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
  df1$text_sentence_count >= 2 ~ "Important",
  TRUE ~ "Trivial"
)

df2 = df1 %>%
  filter(df1$sentence_number == "Important")

hist(df1$comment_sentence_count, breaks = 50)
hist(df1$text_sentence_count)


#Analyzing organization names
organizations <- unique(df1$Organization.Name[df1$Organization.Name != ""])
write.csv(organizations, "organizations.csv")

#Trying to get at occupations
specific_word <- "As a"
extracted_sentences_1 <- df1 %>%
  filter(str_detect(Comment, specific_word)) %>%
  mutate(extracted = str_extract(Comment, paste0("([^.]*\\b", specific_word, "\\b[^.]*\\.)"))) %>%
  select(extracted)

specific_word <- "I am a"
extracted_sentences_2 <- df1 %>%
  filter(str_detect(Comment, specific_word)) %>%
  mutate(extracted = str_extract(Comment, paste0("([^.]*\\b", specific_word, "\\b[^.]*\\.)"))) %>%
  select(extracted)

specific_word <- "As a"
extracted_sentences_3 <- df1 %>%
  filter(str_detect(text, specific_word)) %>%
  mutate(extracted = str_extract(text, paste0("([^.]*\\b", specific_word, "\\b[^.]*\\.)"))) %>%
  select(extracted)

specific_word <- "I am a"
extracted_sentences_4 <- df1 %>%
  filter(str_detect(text, specific_word)) %>%
  mutate(extracted = str_extract(text, paste0("([^.]*\\b", specific_word, "\\b[^.]*\\.)"))) %>%
  select(extracted)

prelim_occupation_df <-
  do.call(rbind, list(extracted_sentences_1, extracted_sentences_2, 
                      extracted_sentences_3, extracted_sentences_4))
write.csv(prelim_occupation_df, "occupations.csv")
