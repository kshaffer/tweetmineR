library(tidyverse)
library(tidytext)
library(lubridate)
library(stringr)
library(httr)

# import tweets from TAGS archive
google_sheet_csv <- '' #insert URL of published CSV file from TAGS archive Google sheet
tweets <- read_csv(google_sheet_csv,
                   col_types = 'ccccccccccccciiccc') %>%
  mutate(date = mdy(paste(substring(created_at, 5, 10), substring(created_at, 27 ,30))))
  
source_text <- '#americafirst'
minimum_occurrences <- 5 # minimum number of occurrences to include in output

# extract URLs from tweets and count frequency
reg <- "([^A-Za-z_\\d#/@']|'(?![A-Za-z_\\d#/@]))"
urls_temp <- tweets %>% 
  unnest_tokens(word, text, token = "regex", pattern = reg, to_lower = FALSE) %>%
  mutate(word = str_replace_all(word, "https|//t|http|&amp;|&lt;|&gt;", ""),
         word = str_replace_all(word, "co/", "https://t.co/")) %>%
  select(word) %>%
  filter(grepl('https://t.co/', word, fixed = TRUE)) %>%
  count(word, sort=TRUE) %>%
  mutate(word = reorder(word, n)) 

# find target and status code of redirected URL
# limited to URLs occurring a minimum number of times (set above)
# excludes tweets as sources
#
# this queries each URL in the table and will take some time,
# especially for longer archives
# 
# TO DO: save already queried URLs locally, then query new ones
# and merge the results

urls_common <- urls_temp %>%
  filter(n >= minimum_occurrences) %>% 
  mutate(source_url = as.character(word)) %>%
  select(source_url, count = n) 

url <- t(sapply(urls_common$source_url, GET)) %>%
  as_tibble() %>%
  select(url, status_code)

url_list <- cbind(urls_common, unnest(url)) %>%
  as_tibble() %>%
  select(url, count, status_code) %>%
  filter(status_code != 404,
         url != 'https://t.co/',
         !grepl('https://twitter.com/', url))

# extract domains from URLs
extract_domain <- function(url) {
  return(gsub('www.', '', unlist(strsplit(unlist(strsplit(as.character(url), '//'))[2], '/'))[1]))
}

# count the frequency of a domain's occurrence in the most frequent URL list
domain_list <- url_list %>%
  mutate(domain = mapply(extract_domain, url)) %>%
  group_by(domain) %>%
  summarize(domain_count = sum(count)) %>%
  arrange(desc(domain_count))

# plot the most common domains in the corpus
domain_list %>%
  mutate(domain = reorder(domain, domain_count)) %>%
  ggplot(aes(domain, domain_count, fill = domain)) +
  geom_bar(stat = 'identity') +
  xlab(NULL) +
  ylab('URL occurrences (since 2017-02-12)') +
  ggtitle(paste('Most common domains in tweets containing', source_text)) +
  theme(legend.position="none") +
  coord_flip()

# write domain info to files
write_csv(domain_list, 'aggregate_data_dumps/af_domains.csv')
write_csv(url_list, 'aggregate_data_dumps/af_urls.csv')

# extract words from tweets, make tidy
reg_words <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"
tidy_tweets <- tweets %>% 
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https", "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = reg_words) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "[a-z]"))

# plot the most common words in the corpus
min_date <- tidy_tweets %>%
  mutate(date = mdy(paste(substring(created_at, 5, 10), substring(created_at, 27 ,30)))) %>%
  select(date) %>%
  .$date %>%
  min()
  
tidy_tweets %>%
  count(word, sort=TRUE) %>%
  filter(substr(word, 1, 1) != '#', # omit hashtags
         substr(word, 1, 1) != '@', # omit Twitter handles
         n > 80) %>% # only most common words
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
  filter(n >= 30) %>%
  mutate(bigram = reorder(bigram, n)) %>%
  ggplot(aes(bigram, n, fill = bigram)) +
  geom_bar(stat = 'identity') +
  xlab(NULL) +
  ylab(paste('bigram count (since ', 
             min_date,
             ')', sep = '')) +
  ggtitle(paste('Most common bigrams in tweets containing', source_text)) +
  theme(legend.position="none") +
  coord_flip()

