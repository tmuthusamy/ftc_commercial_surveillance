final_df = readRDS("final_dataframe.RDS")

library(tidytext)
library(tidyverse)
library(textdata)

#Sentiment Analysis on Comments
afinn_sentiments <- get_sentiments("afinn")

calculate_sentiment <- function(data, text_column) {
  data %>%
    unnest_tokens(word, !!sym(text_column)) %>%
    inner_join(afinn_sentiments, by = "word") %>%
    group_by(Document.ID) %>%
    summarise(avg_sentiment = mean(value, na.rm = TRUE)) %>%
    rename(!!paste0("avg_sentiment_", text_column) := avg_sentiment)
}

sentiment_text1 <- calculate_sentiment(final_df, "Comment")
sentiment_text2 <- calculate_sentiment(final_df, "text_1")
sentiment_text3 <- calculate_sentiment(final_df, "text_2")
sentiment_text4 <- calculate_sentiment(final_df, "text_3")
sentiment_text5 <- calculate_sentiment(final_df, "text_4")
sentiment_text6 <- calculate_sentiment(final_df, "text_5")
sentiment_text7 <- calculate_sentiment(final_df, "text_6")
sentiment_text8 <- calculate_sentiment(final_df, "text_7")
sentiment_text9 <- calculate_sentiment(final_df, "text_8")
sentiment_text10 <- calculate_sentiment(final_df, "text_9")
sentiment_text11 <- calculate_sentiment(final_df, "text_10")
sentiment_text12 <- calculate_sentiment(final_df, "text_11")
sentiment_text13 <- calculate_sentiment(final_df, "text_12")
sentiment_text14 <- calculate_sentiment(final_df, "text_13")
sentiment_text15 <- calculate_sentiment(final_df, "text_14")
sentiment_text16 <- calculate_sentiment(final_df, "text_15")
sentiment_text17 <- calculate_sentiment(final_df, "text_16")
sentiment_text18 <- calculate_sentiment(final_df, "text_17")
sentiment_text19 <- calculate_sentiment(final_df, "text_18")
sentiment_text20 <- calculate_sentiment(final_df, "text_19")

# Combine sentiment scores back to the original data frame
df_with_sentiment <- final_df %>%
  left_join(sentiment_text1, by = "Document.ID") %>%
  left_join(sentiment_text2, by = "Document.ID") %>%
  left_join(sentiment_text3, by = "Document.ID") %>%
  left_join(sentiment_text4, by = "Document.ID")%>%
  left_join(sentiment_text5, by = "Document.ID") %>%
  left_join(sentiment_text6, by = "Document.ID")%>%
  left_join(sentiment_text7, by = "Document.ID") %>%
  left_join(sentiment_text8, by = "Document.ID")%>%
  left_join(sentiment_text9, by = "Document.ID") %>%
  left_join(sentiment_text10, by = "Document.ID")%>%
  left_join(sentiment_text11, by = "Document.ID") %>%
  left_join(sentiment_text12, by = "Document.ID")%>%
  left_join(sentiment_text13, by = "Document.ID") %>%
  left_join(sentiment_text14, by = "Document.ID")%>%
  left_join(sentiment_text15, by = "Document.ID") %>%
  left_join(sentiment_text16, by = "Document.ID")%>%
  left_join(sentiment_text17, by = "Document.ID") %>%
  left_join(sentiment_text18, by = "Document.ID")%>%
  left_join(sentiment_text19, by = "Document.ID") %>%
  left_join(sentiment_text20, by = "Document.ID")
df_with_sentiment$average_sentiment <- rowMeans(df_with_sentiment[,28:47], na.rm = TRUE)

df_with_sentiment <- df_with_sentiment %>%
  select(1:8, 28, 48)
mean(df_with_sentiment$average_sentiment, na.rm = TRUE)


#Topic Modeling





