library(tidyverse)
library(tidytext)
library(lubridate)
library(stringr)
library(httr)

# import tweets from TAGS archive
google_sheet_csv <- '' #insert URL of published CSV file from TAGS archive Google sheet
tweets <- read_csv(google_sheet_csv)
source_text <- '#whitegenocide'
minimum_occurrences <- 10 # minimum number of occurrences to include in output

# extract URLs from tweets and count frequency
reg <- "([^A-Za-z_\\d#@/']|'(?![A-Za-z_\\d#@/]))"
urls_temp <- tweets %>% 
  unnest_tokens(word, text, token = "regex", pattern = reg, to_lower = FALSE) %>%
  mutate(word = str_replace_all(word, "https|//t|http|&amp;|&lt;|&gt;", ""),
         word = str_replace_all(word, "co/", "https://t.co/")) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "[a-z]")) %>%
  select(word) %>%
  filter(grepl('https://t.co/', word, fixed = TRUE)) %>%
  count(word, sort=TRUE) %>%
  #filter(n > 5) %>%
  mutate(word = reorder(word, n)) 

# find target and status code of redirected URL
# limited to URLs occurring 10 or more times
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

# plot the most common URLs in the corpus
url_list %>%
  ggplot(aes(url, count, fill = url)) +
  geom_bar(stat = 'identity') +
  xlab(NULL) +
  ylab('URL occurrences (since 2017-02-12)') +
  ggtitle(paste('Most common URLs in tweets containing', source_text)) +
  theme(legend.position="none") +
  coord_flip()

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

# extract words from tweets, make tidy
reg_words <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"
tidy_tweets <- tweets %>% 
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https", "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = reg_words) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "[a-z]"))


# plot the most common words in the corpus
tidy_tweets %>%
  count(word, sort=TRUE) %>%
  filter(n > 200) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = word)) +
  geom_bar(stat = 'identity') +
  xlab(NULL) +
  ylab('Word count (since 2017-02-12)') +
  ggtitle(paste('Most common words in tweets containing', source_text)) +
  theme(legend.position="none") +
  coord_flip()

