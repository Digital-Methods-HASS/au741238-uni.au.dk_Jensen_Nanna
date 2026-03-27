library(tidyverse)
library(here)

# For text mining:

library(pdftools)
library(tidytext)
library(textdata)
library(ggwordcloud)

#Get the pdf dokument, got.pdf = got_path
got_path <- here("data","got.pdf")
got_text <- pdf_text(got_path)
got_text

#Wrangling split-lines
got_df <- data.frame(got_text) %>%
  mutate(text_full = str_split(got_text, pattern = '\n')) %>%
  unnest(text_full) %>%
  mutate(text_full = str_trim(text_full))

got_df$text_full


#Individual words, tokenize
got_tokens <- got_df %>%
  unnest_tokens(word, text_full)

got_tokens


#Count the words
got_wc <- got_tokens %>%
  count(word) %>%
  arrange(-n)

got_wc

#removestopwords

?stop_words

got_stop <- got_tokens %>%
  anti_join(stop_words) %>%
  select(-got_text)

#check counts again
got_swc <- got_stop %>%
  count(word) %>%
  arrange(-n)
got_swc

#filter non text
got_no_numeric <- got_stop %>%
  filter(is.na(as.numeric(word)))

#2000 unique words
length(unique(got_no_numeric$word))

#top 100
got_top100 <- got_no_numeric %>%
  count(word) %>%
  arrange(-n) %>%
  head(100)
got_top100

#cloud
got_cloud <- ggplot(data = got_top100, aes(label = word)) +
  geom_text_wordcloud() +
  theme_minimal()

got_cloud

#starshape
ggplot(data = got_top100, aes(label = word, size = n)) +
  geom_text_wordcloud_area(aes(color = n), shape = "star") +
  scale_size_area(max_size = 15) +
  scale_color_gradientn(colors = c("darkgreen","blue","red")) +
  theme_minimal()
ggsave("star.png", width =18)

#negative and positive words with "afinn"
get_sentiments(lexicon = "afinn")

#only positive words with "afinn"
afinn_pos <- get_sentiments("afinn") %>%
  filter(value %in% c(3,4,5))
afinn_pos

#bing positive or negative
get_sentiments(lexicon = "bing")

#nrc
get_sentiments(lexicon = "nrc")

#analysis with afinn (bind words in `ipcc_stop` to `afinn` lexicon)
got_afinn <- got_stop %>%
  inner_join(get_sentiments("afinn"))
got_afinn

#find some counts (by sentiment ranking)
got_afinn_hist <- got_afinn %>%
  count(value)

#plot in a histogram
ggplot(data = got_afinn_hist, aes(x = value, y = n)) +
  geom_col(aes(fill = value)) +
  theme_bw()

#focus on 3
got_afinn3 <- got_afinn %>%
  filter(value ==3)
got_afinn3

#Checking 3 positive unique words)
unique(got_afinn3$word)

#plot
got_afinn3_n <- got_afinn3 %>%
  count(word, sort = TRUE) %>%
  mutate(word = fct_reorder(factor(word), n))

ggplot(data = got_afinn3_n, aes(x = word, y = n)) +
  geom_col() +
  coord_flip() +
  theme_bw()

#summarize "afinn" + median
got_summary <- got_afinn %>%
  summarize(
    mean_score = mean(value),
    median_score = median(value)
  )
got_summary

#non-stopwordslist + nrc
got_nrc <- got_stop %>%
  inner_join(get_sentiments("nrc"))

#check exclusion
got_exclude <- got_stop %>%
  anti_join(get_sentiments("nrc"))
got_exclude

# Count to find the most excluded
got_exclude_n <- got_exclude %>%
  count(word, sort = TRUE)

head(got_exclude_n)

#Count bing
got_nrc_n <- got_nrc %>%
  count(sentiment, sort = TRUE)

#plot them
ggplot(data = got_nrc_n, aes(x = sentiment, y = n)) +
  geom_col(aes(fill = sentiment))+
  theme_bw()

#count by sentiment *and* word, then facet
got_nrc_n5 <- got_nrc %>%
  count(word,sentiment, sort = TRUE) %>%
  group_by(sentiment) %>%
  top_n(5) %>%
  ungroup()

#show it
got_nrc_gg <- ggplot(data = got_nrc_n5, aes(x = reorder(word,n), y = n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, ncol = 2, scales = "free") +
  coord_flip() +
  theme_minimal() +
  labs(x = "Word", y = "count")
got_nrc_gg

#save it
ggsave(plot = got_nrc_gg,
       here("figures","got_nrc_sentiment.png"),
       height = 8,
       width = 5)
#focus on the word "lord"
lord<- get_sentiments(lexicon = "nrc") %>%
  filter(word == "lord")
lord
