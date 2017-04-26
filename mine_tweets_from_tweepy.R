library(tidyverse)
library(tidytext)
library(lubridate)
library(stringr)
library(jsonlite)


# tweet archive source (for inclusion in plot titles)
source_text <- ''
  
# import tweets from a single CSV file
csv_file_name <- ''
tweets <- read_csv(csv_file_name, col_types = 'cccTccccciccliiccccc')


# import tweets from two CSV files (search and stream)
search_file <- ''
stream_file <- ''
tweets <- read_csv(search_file, col_types = 'cccTccccciccliiccccc') %>%
  full_join(read_csv(stream_file, col_types = 'cccTccccciccliiccccc')) %>%
  unique()
  



# functions to extract URLs from JSON entities_str cell
get_expanded_url <- function(cell) {
  if (!is.na(cell)) {
    try(url <- fromJSON(gsub("'", '"', cell)))
    if (exists('url') & length(url$url) > 0) {
      return(url$expanded_url)
    } else {
      return(NA)
    }
  } else {
    return(NA)
  }
}

# function to extract domains from URLs
extract_domain <- function(url) {
  return(gsub('www.', '', unlist(strsplit(unlist(strsplit(as.character(url), '//'))[2], '/'))[1]))
}

# extract urls from tweets
tweets_with_urls <- tweets %>%
  mutate(url = sapply(entities_urls, get_expanded_url)) %>%
  filter(!is.na(url)) %>%
  unnest()

# count URLs frequency of occurrence
url_list <- tweets_with_urls %>%
  group_by(url) %>%
  summarize(count = n()) %>%
  select(url, count) %>%
  # filter(!grepl('https://t.co/', url),
  #        !grepl('https://twitter.com/', url)) %>%
  arrange(desc(count))


# count the frequency of a domain's occurrence
domain_list <- url_list %>%
  mutate(domain = mapply(extract_domain, url)) %>%
  group_by(domain) %>%
  summarize(domain_count = sum(count)) %>%
  arrange(desc(domain_count))

# plot the most common domains in the corpus
min_date <- tweets %>%
  select(created_at) %>%
  mutate(date = ymd(substring(created_at, 1, 10))) %>%
  .$date %>%
  min()

domain_list %>%
  filter(domain != 'twitter.com') %>%
  mutate(domain = reorder(domain, domain_count)) %>%
  ggplot(aes(domain, domain_count, fill = domain)) +
  geom_bar(stat = 'identity') +
  xlab(NULL) +
  ylab(paste('domain count (since ', 
             min_date,
             ')', sep = '')) +
  ggtitle(paste('Most common domains (other than twitter.com) in tweets containing', source_text)) +
  theme(legend.position="none") +
  coord_flip()

# write domain info to files
write_csv(domain_list, '')
write_csv(url_list, '')



# extract words from tweets (excepting RTs), make tidy
reg_words <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"
tidy_tweets <- tweets %>% 
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https", "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = reg_words) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "[a-z]"))

tidy_tweets %>%
  count(word, sort=TRUE) %>%
  filter(
         # substr(word, 1, 1) != '#', # omit hashtags
         # substr(word, 1, 1) != '@', # omit Twitter handles
         n > 20
         ) %>% # only most common words
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = word)) +
  geom_bar(stat = 'identity') +
  xlab(NULL) +
  ylab(paste('Word count (since ', 
             min_date,
             ')', sep = '')) +
  ggtitle(paste('Most common words in tweets containing', source_text)) +
  theme(legend.position="none") +
  coord_flip()

# bigrams
tidy_bigrams <- tweets %>% 
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https", "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = reg_words) %>%
  mutate(next_word = lead(word)) %>%
  filter(!word %in% stop_words$word, # remove stop words
         !next_word %in% stop_words$word, # remove stop words
         substr(word, 1, 1) != '@', # remove user handles to protect privacy
         substr(next_word, 1, 1) != '@', # remove user handles to protect privacy
         substr(word, 1, 1) != '#', # remove hashtags
         substr(next_word, 1, 1) != '#',
         str_detect(word, "[a-z]"), # remove words containing ony numbers or symbols
         str_detect(next_word, "[a-z]")) %>% # remove words containing ony numbers or symbols
  filter(id_str == lead(id_str)) %>% # needed to ensure bigrams to cross from one tweet into the next
  unite(bigram, word, next_word, sep = ' ')

tidy_bigrams %>%
  count(bigram, sort=TRUE) %>%
  filter(n >= 5) %>%
  mutate(bigram = reorder(bigram, n)) %>%
  ggplot(aes(bigram, n, fill = bigram)) +
  geom_bar(stat = 'identity') +
  xlab(NULL) +
  ylab(paste('bigram count (since ', 
             min_date,
             ')', 
             sep = '')) +
  ggtitle(paste('Most common bigrams in tweets containing', source_text, '(excluding hashtags and handles)')) +
  theme(legend.position="none") +
  coord_flip()

