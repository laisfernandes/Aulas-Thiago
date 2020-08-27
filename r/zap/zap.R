# Carregando os pacotes
library("tidyverse")
library("ggplot2")
library("readr")
library("rwhatsapp")
library("dplyr")
library("ggplot2"); theme_set(theme_minimal())
library("lubridate")

library('stringr')
library('tidytext')

library("stopwords")
to_remove <- c(stopwords(language = "de"),
               "media",
               "omitted",
               "ref",
               "dass",
               "schon",
               "mal",
               "android.s.wt")

install.packages("stopwords")
install.packages("tidytext")
install.packages("rwhatsapp")

#history <- system.file("extdata", "/Users/thiagorolemberg/Documents/professor/impacta/ling-ds-engenharia/r/zap/conversas.txt", package = "rwhatsapp")

history <- system.file("extdata", "sample.txt", package = "rwhatsapp")
chat <- rwa_read(history)


chat <- rwa_read("/Users/thiagorolemberg/Documents/professor/impacta/ling-ds-engenharia/r/zap/conversas.txt") %>% 
  filter(!is.na(author)) # remove messages without author


chat %>%
  mutate(day = date(time)) %>%
  count(day) %>%
  ggplot(aes(x = day, y = n)) +
  geom_bar(stat = "identity") +
  ylab("") + xlab("") +
  ggtitle("Messages per day")


chat %>%
  mutate(day = date(time)) %>%
  count(author) %>%
  ggplot(aes(x = reorder(author, n), y = n)) +
  geom_bar(stat = "identity") +
  ylab("") + xlab("") +
  coord_flip() +
  ggtitle("Number of messages")


library("tidyr")
chat %>%
  unnest(emoji) %>%
  count(author, emoji, sort = TRUE) %>%
  group_by(author) %>%
  top_n(n = 6, n) %>%
  ggplot(aes(x = reorder(emoji, n), y = n, fill = author)) +
  geom_col(show.legend = FALSE) +
  ylab("") +
  xlab("") +
  coord_flip() +
  facet_wrap(~author, ncol = 2, scales = "free_y")  +
  ggtitle("Most often used emojis")



chat %>%
  unnest_tokens(input = text,
                output = word) %>%
  count(author, word, sort = TRUE) %>%
  group_by(author) %>%
  top_n(n = 6, n) %>%
  ggplot(aes(x = reorder_within(word, n, author), y = n, fill = author)) +
  geom_col(show.legend = FALSE) +
  ylab("") +
  xlab("") +
  coord_flip() +
  facet_wrap(~author, ncol = 2, scales = "free_y") +
  scale_x_reordered() +
  ggtitle("Most often used words")


chat %>%
  unnest_tokens(input = text,
                output = word) %>%
  filter(!word %in% to_remove) %>%
  group_by(author) %>%
  summarise(lex_diversity = n_distinct(word)) %>%
  arrange(desc(lex_diversity)) %>%
  ggplot(aes(x = reorder(author, lex_diversity),
             y = lex_diversity,
             fill = author)) +
  geom_col(show.legend = FALSE) +
  scale_y_continuous(expand = (mult = c(0, 0, 0, 500))) +
  geom_text(aes(label = scales::comma(lex_diversity)), hjust = -0.1) +
  ylab("unique words") +
  xlab("") +
  ggtitle("Lexical Diversity") +
  coord_flip()

chat %>%
  unnest_tokens(input = text,
                output = word) %>%
  select(word, author) %>%
  filter(!word %in% to_remove) %>%
  mutate(word = gsub(".com", "", word)) %>%
  mutate(word = gsub("^gag", "9gag", word)) %>%
  count(author, word, sort = TRUE) %>%
  bind_tf_idf(term = word, document = author, n = n) %>%
  filter(n > 10) %>%
  group_by(author) %>%
  top_n(n = 6, tf_idf) %>%
  ggplot(aes(x = reorder_within(word, n, author), y = n, fill = author)) +
  geom_col(show.legend = FALSE) +
  ylab("") +
  xlab("") +
  coord_flip() +
  facet_wrap(~author, ncol = 2, scales = "free_y") +
  scale_x_reordered() +
  ggtitle("Important words using tf–idf by author")


o_words <- chat %>%
  unnest_tokens(input = text,
                output = word) %>%
  filter(author != "Thiago") %>% 
  count(word, sort = TRUE) 

chat %>%
  unnest_tokens(input = text,
                output = word) %>%
  filter(author == "Thiago") %>% 
  count(word, sort = TRUE) %>% 
  filter(!word %in% o_words$word) %>% # only select words nobody else uses
  top_n(n = 6, n) %>%
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_col(show.legend = FALSE) +
  ylab("") + xlab("") +
  coord_flip() +
  ggtitle("Unique words of Thiago")