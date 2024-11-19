df1 = readRDS("final_dataframe.RDS")

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
  select(Document.ID, extracted)

specific_word <- "I am a"
extracted_sentences_2 <- df1 %>%
  filter(str_detect(Comment, specific_word)) %>%
  mutate(extracted = str_extract(Comment, paste0("([^.]*\\b", specific_word, "\\b[^.]*\\.)"))) %>%
  select(Document.ID, extracted)

specific_word <- "As a"
extracted_sentences_3 <- df1 %>%
  pivot_longer(cols = starts_with("text"), names_to = "text_column", values_to = "all_text") %>%
  filter(str_detect(Comment, specific_word)) %>%
  mutate(extracted = str_extract(all_text, paste0("([^.]*\\b", specific_word, "\\b[^.]*\\.)"))) %>%
  mutate(extracted = str_squish(str_trim(extracted))) %>%
  select(Document.ID, extracted) %>%
  distinct()

specific_word <- "I am a"
extracted_sentences_4 <- df1 %>%
  pivot_longer(cols = starts_with("text"), names_to = "text_column", values_to = "all_text") %>%
  filter(str_detect(Comment, specific_word)) %>%
  mutate(extracted = str_extract(all_text, paste0("([^.]*\\b", specific_word, "\\b[^.]*\\.)"))) %>%
  mutate(extracted = str_squish(str_trim(extracted))) %>%
  select(Document.ID, extracted) %>%
  distinct()

prelim_occupation_df_1 <-
  do.call(rbind, list(extracted_sentences_1, extracted_sentences_2, extracted_sentences_3, extracted_sentences_4)) %>%
  distinct()
write.csv(prelim_occupation_df_1, "occupations.csv")


#ANALYSIS OF FIVE KEY AREAS

#Civil Society
specific_words <- c("advocate", "civil society", "human rights defender")
regex_pattern <- paste0("\\b(", paste(specific_words, collapse = "|"), ")\\b")
civil_society_1 <- df1 %>%
  pivot_longer(cols = starts_with("text"), names_to = "text_column", values_to = "all_text") %>%
  filter(str_detect(all_text, regex_pattern)) %>% 
  mutate(extracted = str_extract(all_text, paste0("([^.]*", regex_pattern, "[^.]*\\.)"))) %>%
  mutate(extracted = str_squish(str_trim(extracted))) %>%
  select(Document.ID, extracted) %>%
  distinct()
civil_society_2 <- df1 %>%
  filter(str_detect(Comment, regex_pattern)) %>%
  mutate(extracted = str_extract(Comment, paste0("([^.]*\\b", regex_pattern, "\\b[^.]*\\.)"))) %>%
  select(Document.ID, extracted) %>%
  distinct()
civil_society <- rbind(civil_society_1, civil_society_2) %>%
  distinct()

df1 <- df1 %>%
  rowwise() %>%
  mutate(civil_society = any(str_detect(c_across(8:27), regex_pattern))) %>%
  ungroup()


#Tech Companies
specific_words <- c("engineer", "manager", "CEO", "CTO", "programmer")
regex_pattern <- paste0("\\b(", paste(specific_words, collapse = "|"), ")\\b")
tech_companies_1 <- df1 %>%
  pivot_longer(cols = starts_with("text"), names_to = "text_column", values_to = "all_text") %>%
  filter(str_detect(all_text, regex_pattern)) %>% 
  mutate(extracted = str_extract(all_text, paste0("([^.]*", regex_pattern, "[^.]*\\.)"))) %>%
  mutate(extracted = str_squish(str_trim(extracted))) %>%
  select(Document.ID, extracted) %>%
  distinct()
tech_companies_2 <- df1 %>%
  filter(str_detect(Comment, regex_pattern)) %>%
  mutate(extracted = str_extract(Comment, paste0("([^.]*\\b", regex_pattern, "\\b[^.]*\\.)"))) %>%
  select(Document.ID, extracted) %>%
  distinct()
tech_companies <- rbind(tech_companies_1, tech_companies_2) %>%
  distinct()

df1 <- df1 %>%
  rowwise() %>%
  mutate(tech_companies = any(str_detect(c_across(8:27), regex_pattern))) %>%
  ungroup()

#Consumers
specific_words <- c("individual", "student", "disabled", "parent", "consumer", "user", "customer")
regex_pattern <- paste0("\\b(", paste(specific_words, collapse = "|"), ")\\b")
consumers_1 <- df1 %>%
  pivot_longer(cols = starts_with("text"), names_to = "text_column", values_to = "all_text") %>%
  filter(str_detect(all_text, regex_pattern)) %>% 
  mutate(extracted = str_extract(all_text, paste0("([^.]*", regex_pattern, "[^.]*\\.)"))) %>%
  mutate(extracted = str_squish(str_trim(extracted))) %>%
  select(Document.ID, extracted) %>%
  distinct()
consumers_2 <- df1 %>%
  filter(str_detect(Comment, regex_pattern)) %>%
  mutate(extracted = str_extract(Comment, paste0("([^.]*\\b", regex_pattern, "\\b[^.]*\\.)"))) %>%
  select(Document.ID, extracted) %>%
  distinct()
consumers <- rbind(consumers_1, consumers_2) %>%
  distinct()

df1 <- df1 %>%
  rowwise() %>%
  mutate(consumers = any(str_detect(c_across(8:27), regex_pattern))) %>%
  ungroup()


#Academics
specific_words <- c("an academic", "professor", "researcher")
regex_pattern <- paste0("\\b(", paste(specific_words, collapse = "|"), ")\\b")
academics_1 <- df1 %>%
  pivot_longer(cols = starts_with("text"), names_to = "text_column", values_to = "all_text") %>%
  filter(str_detect(all_text, regex_pattern)) %>% 
  mutate(extracted = str_extract(all_text, paste0("([^.]*", regex_pattern, "[^.]*\\.)"))) %>%
  mutate(extracted = str_squish(str_trim(extracted))) %>%
  select(Document.ID, extracted) %>%
  distinct()
academics_2 <- df1 %>%
  filter(str_detect(Comment, regex_pattern)) %>%
  mutate(extracted = str_extract(Comment, paste0("([^.]*\\b", regex_pattern, "\\b[^.]*\\.)"))) %>%
  select(Document.ID, extracted) %>%
  distinct()
academics <- rbind(academics_1, academics_2) %>%
  distinct()

df1 <- df1 %>%
  rowwise() %>%
  mutate(academics = any(str_detect(c_across(8:27), regex_pattern))) %>%
  ungroup()

#Federal Government
specific_words <- c("agency", "government employee", "civil servant")
regex_pattern <- paste0("\\b(", paste(specific_words, collapse = "|"), ")\\b")
government_1 <- df1 %>%
  pivot_longer(cols = starts_with("text"), names_to = "text_column", values_to = "all_text") %>%
  filter(str_detect(all_text, regex_pattern)) %>% 
  mutate(extracted = str_extract(all_text, paste0("([^.]*", regex_pattern, "[^.]*\\.)"))) %>%
  mutate(extracted = str_squish(str_trim(extracted))) %>%
  select(Document.ID, extracted) %>%
  distinct()
government_2 <- df1 %>%
  filter(str_detect(Comment, regex_pattern)) %>%
  mutate(extracted = str_extract(Comment, paste0("([^.]*\\b", regex_pattern, "\\b[^.]*\\.)"))) %>%
  select(Document.ID, extracted) %>%
  distinct()
government <- rbind(government_1, government_2) %>%
  distinct()

df1 <- df1 %>%
  rowwise() %>%
  mutate(government = any(str_detect(c_across(8:27), regex_pattern))) %>%
  ungroup()

prelim_occupation_df_2 <- do.call(rbind, list(civil_society, tech_companies, consumers, 
                                              academics, government)) %>%
  distinct()

prelim_occupation_df <- rbind(prelim_occupation_df_1, prelim_occupation_df_2) %>%
  distinct() %>%
  na.omit() %>%
  arrange(Document.ID)
write.xlsx(prelim_occupation_df, "occupations.xlsx")
write.csv(df1, "final_dataframe.csv")

df_truncated <- df1 %>%
  select(1:8, 29:33)
library(openxlsx)
write.xlsx(df_truncated, file = "final_dataframe.xlsx")
