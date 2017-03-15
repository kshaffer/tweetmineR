library(tidyverse)
library(tidytext)
library(lubridate)
library(stringr)
library(httr)
library(rvest)
library(jsonlite)
library(scales)

# import tweets from a single TAGS archive
google_sheet_csv <- '' #insert file path of local download or URL of published CSV file from TAGS archive Google sheet
tweets <- read_csv(google_sheet_csv,
                   col_types = 'ccccccccccccciiccc') %>%
  mutate(date = mdy(paste(substring(created_at, 5, 10), substring(created_at, 27 ,30))))
  
source_text <- '#maga'


# import tweets from multiple TAGS archives and assemble a single tweet database
tweets <- read_csv('maga_tweetsMar345.csv', col_types = 'ccccccccccccciiccc') %>%
            mutate(source = '#maga',
                   pol = 'right') %>%
  full_join(read_csv('resist_tweetsMar345.csv', col_types = 'ccccccccccccciiccc') %>%
              mutate(source = '#resist',
                     pol = 'left')) %>%
  full_join(read_csv('americafirst_tweetsMar3ff.csv', col_types = 'ccccccccccccciiccc') %>%
              mutate(source = '#americafirst',
                     pol = 'right')) %>%
  full_join(read_csv('pizzagate_tweetsMar3ff.csv', col_types = 'ccccccccccccciiccc') %>%
              mutate(source = '#pizzagate',
                     pol = 'right')) %>%
  full_join(read_csv('trumprussia_tweetsMar345.csv', col_types = 'ccccccccccccciiccc') %>%
              mutate(source = '#trumprussia',
                     pol = 'left')) %>%
  full_join(read_csv('blacklivesmatter_tweetsMar3ff.csv', col_types = 'ccccccccccccciiccc') %>%
              mutate(source = '#blacklivesmatter',
                     pol = 'left')) %>%
  full_join(read_csv('impeachtrump_tweetsMar3ff.csv', col_types = 'ccccccccccccciiccc') %>%
              mutate(source = '#impeachtrump',
                     pol = 'left')) %>%
  full_join(read_csv('whitegenocide_tweetsMar3ff.csv', col_types = 'ccccccccccccciiccc') %>%
              mutate(source = '#whitegenocide',
                     pol = 'right'))


# functions to extract URLs from JSON entities_str cell
get_url <- function(cell) {
  if (!is.na(cell)) {
    try(urls <- fromJSON(cell)$urls)
    if (exists('urls') & length(urls) > 0) {
      return(unlist(urls$url))
    } else {
      return(NA)
    }
  } else {
    return(NA)
  }
}

get_expanded_url <- function(cell) {
  if (!is.na(cell)) {
    try(urls <- fromJSON(cell)$urls)
    if (exists('urls') & length(urls) > 0) {
      return(unlist(urls$expanded_url))
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
  mutate(linked_url_expanded = sapply(entities_str, get_expanded_url),
         linked_url_tco = sapply(entities_str, get_url))

# whole corpus url list
urls <- unlist(tweets_with_urls$linked_url_expanded)

# whole corpus, tagged by source and pol
urls <- tweets_with_urls %>%
  filter(source == '#maga') %>%
  select(linked_url_expanded) %>%
  unlist() %>%
  as_tibble() %>%
  mutate(source = '#maga',
         pol = 'right') %>%
  full_join(tweets_with_urls %>%
              filter(source == '#americafirst') %>%
              select(linked_url_expanded) %>%
              unlist() %>%
              as_tibble() %>%
              mutate(source = '#americafirst',
                     pol = 'right')) %>%
  full_join(tweets_with_urls %>%
              filter(source == '#pizzagate') %>%
              select(linked_url_expanded) %>%
              unlist() %>%
              as_tibble() %>%
              mutate(source = '#pizzagate',
                     pol = 'right')) %>%
  full_join(tweets_with_urls %>%
              filter(source == '#whitegenocide') %>%
              select(linked_url_expanded) %>%
              unlist() %>%
              as_tibble() %>%
              mutate(source = '#whitegenocide',
                     pol = 'right')) %>%
  full_join(tweets_with_urls %>%
              filter(source == '#resist') %>%
              select(linked_url_expanded) %>%
              unlist() %>%
              as_tibble() %>%
              mutate(source = '#resist',
                     pol = 'left')) %>%
  full_join(tweets_with_urls %>%
              filter(source == '#trumprussia') %>%
              select(linked_url_expanded) %>%
              unlist() %>%
              as_tibble() %>%
              mutate(source = '#trumprussia',
                     pol = 'left')) %>%
  full_join(tweets_with_urls %>%
              filter(source == '#impeachtrump') %>%
              select(linked_url_expanded) %>%
              unlist() %>%
              as_tibble() %>%
              mutate(source = '#impeachtrump',
                     pol = 'left')) %>%
  full_join(tweets_with_urls %>%
              filter(source == '#blacklivesmatter') %>%
              select(linked_url_expanded) %>%
              unlist() %>%
              as_tibble() %>%
              mutate(source = '#blacklivesmatter',
                     pol = 'left')) %>%
  mutate(url = value) %>%
  select(url, source, pol)

# count URLs frequency of occurrence
url_list <- urls %>%
  filter(pol == 'right') %>%
  group_by(url) %>%
  summarize(count = n()) %>%
  select(url, count) %>%
  filter(!grepl('https://t.co/', url),
         !grepl('https://twitter.com/', url),
         url != 'NA') %>%
  arrange(desc(count))


# count the frequency of a domain's occurrence
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
  ylab(paste('domain count (since ', 
             min_date,
             ')', sep = '')) +
  ggtitle(paste('Most common domains in tweets containing', source_text)) +
  theme(legend.position="none") +
  coord_flip()

# write domain info to files
write_csv(domain_list, 'right_domains.csv')
write_csv(url_list, 'right_urls.csv')

write_csv(urls, 'all_urls.csv')

# 2D visualization of common websites
url_share_by_pol <- urls %>%
  mutate(domain = mapply(extract_domain, url)) %>%
  group_by(domain, pol) %>%
  summarize(count = n()) %>%
  spread(pol, count, fill = 0) %>%
  mutate(left_total = nrow(urls %>% filter(pol == 'left')),
         right_total = nrow(urls %>% filter(pol == 'right')),
         left_share = left/left_total,
         right_share = right/right_total)

write_csv(url_share_by_pol, 'url_share_by_pol.csv')

url_share_by_pol %>% 
  ggplot(aes(x = right_share,
             y = left_share,
             color = abs(right_share - left_share))) +
  geom_jitter() +
  geom_text(aes(label = domain), check_overlap = TRUE, vjust = 1.5) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  scale_x_log10() +
  scale_y_log10() +
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
  xlab('Share of right-wing hashtag tweets (log10)') +
  ylab('Share of left-wing hashtag tweets (log10)') +
  ggtitle('Relative prominence of domains in right- and left-wing hashtags\nright-wing: #maga, #americafirst, #pizzagate, #whitegenocide\nleft-wing: #resist, #impeachtrump, #trumprussia, #blacklivesmatter') +
  theme(legend.position='none')


# compare most left-/right-slanted of domains
site_ratios <- url_share_by_pol %>%
  filter(!is.na(domain)) %>%
  mutate_each(funs((. + 1) / sum(. + 1)), -domain) %>%
  mutate(right_over_left = log(right_share/left_share)) %>%
  arrange(desc(right_over_left))

site_ratios %>% 
  arrange(desc(abs(right_over_left)))

site_ratios %>%
  group_by(right_over_left < 0) %>%
  top_n(15, abs(right_over_left)) %>%
  ungroup() %>%
  mutate(domain = reorder(domain, right_over_left)) %>%
  ggplot(aes(domain, right_over_left, fill = right_over_left < 0)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  ylab("Log odds ratio (Right-wing/Left-wing)") +
  ggtitle('Domains most characteristic of left- or right-wing hashtags on Twitter in early March\nright-wing: #maga, #americafirst, #pizzagate, #whitegenocide\nleft-wing: #resist, #impeachtrump, #trumprussia, #blacklivesmatter\n') +
  scale_fill_discrete(name = "", labels = c("Right-wing", "Left-wing"))


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
         n > 300) %>% # only most common words
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
  filter(n >= 100) %>%
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






# extract URLs from tweet texts and count frequency 
# (alternative mode that takes a long time, but follows URL shorteners to target URLs)

minimum_occurrences <- 2 # minimum number of occurrences to include in output

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

get_redirected_url <- function(url) {
  print(url)
  try(url_to_return <- GET(url)) 
  if (exists('url_to_return')) {
    return(url_to_return)
  } else {
    return(list(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))
  }
}

urls_common <- urls_temp %>%
  filter(n >= minimum_occurrences) %>% 
  mutate(source_url = as.character(word)) %>%
  select(source_url, count = n) 

url <- t(sapply(urls_common$source_url, get_redirected_url)) %>%
  as_tibble() %>%
  select(url, status_code)

url_list <- cbind(urls_common, url) %>%
  as_tibble() %>%
  select(url, count, status_code) %>%
  filter(status_code != 404,
         url != 'https://t.co/',
         !grepl('https://twitter.com/', url))
