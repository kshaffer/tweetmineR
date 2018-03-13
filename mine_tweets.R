library(tidyverse)
library(tidytext)
library(lubridate)
library(stringr)


source_folder <- 'data/'
files <- list.files(str_c(source_folder, 'sources/'))

tweets_raw <- tibble()

for(file in files) {
  print(file)
  tweets_raw <- bind_rows(tweets_raw,
                      read_csv(str_c(source_folder, 'sources/', file), col_types = 'ccccTccccciccliicccccc') %>%
                        select(id_str,
                               user_id_str,
                               created_at,
                               user_screen_name,
                               user_created_at,
                               user_statuses_count,
                               text,
                               entities_urls,
                               retweeted_status_id))
}

tweets <- tweets_raw %>%
  group_by(id_str,
           user_id_str,
           created_at,
           user_screen_name,
           user_created_at,
           text,
           entities_urls,
           retweeted_status_id) %>%
  summarize(user_statuses_count = max(user_statuses_count)) %>%
  ungroup()

write_csv(tweets, str_c(source_folder, 'tweets.csv'))

# high-/low-volume account threshold (this archive)
threshold <- ifelse((length(tweets$id_str) / 10000) > 1,
                    length(tweets$id_str) / 10000,
                    2)
# threshold <- 12
write_csv(as_tibble(threshold), str_c(source_folder, 'threshold.csv'))
threshold_all_time <- 100000 # high-/low-volume account threshold (all-time)

# frequency of tweets over time
tweets %>%
  mutate(time_floor = floor_date(created_at, unit = "1 hour")) %>%
  group_by(time_floor) %>%
  summarize(tweet_count = n()) %>%
  write_csv(str_c(source_folder, 'tweets_per_hour.csv'))


# most retweeted
tweets %>%
  filter(str_detect(text, '^RT')) %>%
  count(text, sort = TRUE) %>%
  slice(1) %>%
  write_csv(str_c(source_folder, 'most_retweeted_tweet.csv'))

tweets %>%
  filter(created_at > max(created_at) - days(1)) %>%
  filter(str_detect(text, '^RT')) %>%
  count(text, sort = TRUE) %>%
  slice(1) %>%
  write_csv(str_c(source_folder, 'most_retweeted_tweet_24h.csv'))

tweets %>%
  filter(str_detect(text, '^RT')) %>%
  count(text, sort = TRUE) %>%
  filter(n > 1) %>%
  write_csv(str_c(source_folder, 'most_retweeted_tweet_all.csv'))

tweets %>%
  filter(created_at > max(created_at) - days(1)) %>%
  filter(str_detect(text, '^RT')) %>%
  count(text, sort = TRUE) %>%
  filter(n > 1) %>%
  write_csv(str_c(source_folder, 'most_retweeted_tweet_all_24h.csv'))

extract_retweeted_user <- function(text) {
  return(str_split(str_replace(text, '^RT ', ''), ':')[[1]][1])
}

tweets %>%
  select(text) %>%
  filter(str_detect(text, '^RT')) %>%
  mutate(retweeted_user = sapply(text, extract_retweeted_user)) %>%
  select(retweeted_user) %>%
  count(retweeted_user, sort = TRUE) %>%
  write_csv(str_c(source_folder, 'most_retweeted_users.csv'))

tweets %>%
  filter(created_at > max(created_at) - days(1)) %>%
  select(text) %>%
  filter(str_detect(text, '^RT')) %>%
  mutate(retweeted_user = sapply(text, extract_retweeted_user)) %>%
  select(retweeted_user) %>%
  count(retweeted_user, sort = TRUE) %>%
  write_csv(str_c(source_folder, 'most_retweeted_users_24h.csv'))


# account summaries
accounts_meta <- tweets %>%
  select(user_screen_name, user_created_at, user_statuses_count) %>%
  mutate(account_created = floor_date(as_date(str_c(mdy(str_c(substring(user_created_at, 5, 11), substring(user_created_at, 27, 30))), substring(user_created_at, 11, 19))), unit = "1 day")) %>%
  select(-user_created_at) %>%
  group_by(user_screen_name, account_created) %>%
  summarize(statuses = max(user_statuses_count)) %>%
  ungroup()

accounts_meta %>%
  write_csv(str_c(source_folder, 'accounts.csv'))



# find most prolific accounts (with RTs)

tweets %>%
  count(user_screen_name, sort=TRUE) %>%
  write_csv(str_c(source_folder, 'user_count.csv'))

tweets %>%
  filter(created_at > max(created_at) - days(1)) %>%
  count(user_screen_name, sort=TRUE) %>%
  write_csv(str_c(source_folder, 'user_count_24h.csv'))



# extract words from tweets (with RTs), make tidy

reg_words <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"
tidy_tweets <- tweets %>%
  # select(id_str, user_screen_name, created_at, text) %>%
  select(created_at, text) %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https", "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = reg_words) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "[a-z]"))

write_csv(tidy_tweets, str_c(source_folder, 'tidy_tweets.csv'))


# Most mentioned accounts (with RTs)

tidy_tweets %>%
  count(word, sort=TRUE) %>%
  filter(substr(word, 1, 1) == '@') %>% # include only Twitter handles
  mutate(word = reorder(word, n)) %>%
  write_csv(str_c(source_folder, 'mention_count.csv'))

tidy_tweets %>%
  filter(created_at > max(created_at) - days(1)) %>%
  count(word, sort=TRUE) %>%
  filter(substr(word, 1, 1) == '@') %>% # include only Twitter handles
  mutate(word = reorder(word, n)) %>%
  write_csv(str_c(source_folder, 'mention_count_24h.csv'))



# Most used hashtags (with RTs)

tidy_tweets %>%
  count(word, sort=TRUE) %>%
  filter(substr(word, 1, 1) == '#') %>% # include only Twitter handles
  mutate(word = reorder(word, n)) %>%
  write_csv(str_c(source_folder, 'hashtag_count.csv'))

tidy_tweets %>%
  filter(created_at > max(created_at) - days(1)) %>%
  count(word, sort=TRUE) %>%
  filter(substr(word, 1, 1) == '#') %>% # include only Twitter handles
  mutate(word = reorder(word, n)) %>%
  write_csv(str_c(source_folder, 'hashtag_count_24h.csv'))

rm(tidy_tweets)
gc()


# functions to extract URLs from JSON entities_str cell

get_expanded_url <- function(cell) {
  if (!is.na(cell) & str_detect(cell, 'url')) {
    url <- cell %>%
      str_replace_all("\\[\\{'url': 'htt(\\S)* 'expanded_url': '", "") %>%
      str_replace_all("', 'display_url': '(\\S)*', .*$", "")
    #print(url)
    return(url)
  } else {
    return(NA)
  }
}

extract_domain <- function(url) {
  return(gsub('www.', '', unlist(strsplit(unlist(strsplit(as.character(url), '//'))[2], '/'))[1]))
}

# extract urls from tweets & count frequency of occurrence
url_list <- tweets %>%
  select(entities_urls) %>%
  mutate(url = sapply(entities_urls, get_expanded_url)) %>%
  filter(!is.na(url)) %>%
  # unnest() %>%
  count(url, sort = TRUE) %>%
  select(url, count = n) %>%
  filter(!grepl('https://t.co/', url),
         !grepl('https://twitter.com/', url))

url_list %>%
  write_csv(str_c(source_folder, 'url_list.csv'))


# count the frequency of a domain's occurrence
url_list %>%
  mutate(domain = mapply(extract_domain, url)) %>%
  group_by(domain) %>%
  summarize(domain_count = sum(count)) %>%
  arrange(desc(domain_count)) %>%
  write_csv(str_c(source_folder, 'domain_list.csv'))

rm(url_list)
gc()


# ditto, last 24h
url_list_24h <- tweets %>%
  select(entities_urls, created_at) %>%
  filter(created_at > max(created_at) - days(1)) %>%
  mutate(url = sapply(entities_urls, get_expanded_url)) %>%
  filter(!is.na(url)) %>%
  # unnest() %>%
  count(url, sort = TRUE) %>%
  select(url, count = n) %>%
  filter(!grepl('https://t.co/', url),
         !grepl('https://twitter.com/', url))

url_list_24h %>%
  write_csv(str_c(source_folder, 'url_list_24h.csv'))

url_list_24h %>%
  mutate(domain = mapply(extract_domain, url)) %>%
  group_by(domain) %>%
  summarize(domain_count = sum(count)) %>%
  arrange(desc(domain_count)) %>%
  write_csv(str_c(source_folder, 'domain_list_24h.csv'))

rm(url_list_24h)
gc()


# bigrams & trigrams

tidy_bigrams <- tweets %>%
  filter(!str_detect(text, '^RT')) %>%
  select(id_str, user_screen_name, created_at, text) %>%
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

write_csv(tidy_bigrams, str_c(source_folder, 'tidy_bigrams.csv'))

tidy_bigrams %>%
  count(bigram, sort = TRUE) %>%
  write_csv(str_c(source_folder, 'bigrams.csv'))

tidy_bigrams %>%
  filter(created_at > max(created_at) - days(1)) %>%
  count(bigram, sort = TRUE) %>%
  write_csv(str_c(source_folder, 'bigrams_24h.csv'))

rm(tidy_bigrams)
gc()


tidy_trigrams <- tweets %>%
  filter(!str_detect(text, '^RT')) %>%
  select(id_str, user_screen_name, created_at, text) %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https", "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = reg_words) %>%
  mutate(next_word = lead(word),
         next_next_word = lead(next_word)) %>%
  filter(!word %in% stop_words$word, # remove stop words
         !next_word %in% stop_words$word, # remove stop words
         !next_next_word %in% stop_words$word, # remove stop words
         substr(word, 1, 1) != '@', # remove user handles to protect privacy
         substr(next_word, 1, 1) != '@', # remove user handles to protect privacy
         substr(next_next_word, 1, 1) != '@', # remove user handles to protect privacy
         substr(word, 1, 1) != '#', # remove hashtags
         substr(next_word, 1, 1) != '#',
         substr(next_next_word, 1, 1) != '#',
         str_detect(word, "[a-z]"), # remove words containing ony numbers or symbols
         str_detect(next_word, "[a-z]"), # remove words containing ony numbers or symbols
         str_detect(next_next_word, "[a-z]")) %>% # remove words containing ony numbers or symbols
  filter(id_str == lead(id_str) & id_str == lead(id_str, n=2)) %>% # needed to ensure bigrams to cross from one tweet into the next
  unite(trigram, word, next_word, next_next_word, sep = ' ')

write_csv(tidy_trigrams, str_c(source_folder, 'tidy_trigrams.csv'))

tidy_trigrams %>%
  count(trigram, sort = TRUE) %>%
  write_csv(str_c(source_folder, 'trigrams.csv'))

tidy_trigrams %>%
  filter(created_at > max(created_at) - days(1)) %>%
  count(trigram, sort = TRUE) %>%
  write_csv(str_c(source_folder, 'trigrams_24h.csv'))

rm(tidy_trigrams)
gc()


# compare low & high volume tweets (this archive)
compare_tweets <-
  bind_rows(tweets %>%
              inner_join(tweets %>%
                           count(user_screen_name, sort=TRUE) %>%
                           filter(n >= threshold) %>%
                           select(user_screen_name) %>%
                           mutate(volume = 'high')),
            tweets %>%
              inner_join(tweets %>%
                           count(user_screen_name, sort=TRUE) %>%
                           filter(n < threshold) %>%
                           select(user_screen_name) %>%
                           mutate(volume = 'low'))
  )


compare_tweets %>%
  mutate(url = sapply(entities_urls, get_expanded_url)) %>%
  filter(!is.na(url)) %>%
  # unnest() %>%
  count(url, volume) %>%
  select(url, volume, count = n) %>%
  filter(!grepl('https://t.co/', url),
         !grepl('https://twitter.com/', url)) %>%
  mutate(domain = mapply(extract_domain, url)) %>%
  group_by(domain, volume) %>%
  summarize(domain_count = sum(count)) %>%
  arrange(desc(domain_count)) %>%
  spread(volume, domain_count, fill = 0) %>%
  ungroup() %>%
  mutate_each(funs((. + 1) / sum(. + 1)), -domain) %>%
  mutate(logratio = log(high / low)) %>%
  arrange(desc(logratio)) %>%
  select(domain, high, low, logratio) %>%
  write_csv(str_c(source_folder, 'domain_ratios.csv'))

rm(compare_tweets)
gc()


# compare low & high volume tweets (all time)
compare_tweets_all_time <-
  bind_rows(tweets %>%
              inner_join(accounts_meta %>%
                           filter(statuses >= threshold_all_time) %>%
                           select(user_screen_name) %>%
                           mutate(volume = 'high')),
            tweets %>%
              inner_join(accounts_meta %>%
                           filter(statuses < threshold_all_time) %>%
                           select(user_screen_name) %>%
                           mutate(volume = 'low'))
  )


compare_tweets_all_time %>%
  mutate(url = sapply(entities_urls, get_expanded_url)) %>%
  filter(!is.na(url)) %>%
  # unnest() %>%
  count(url, volume) %>%
  select(url, volume, count = n) %>%
  filter(!grepl('https://t.co/', url),
         !grepl('https://twitter.com/', url)) %>%
  mutate(domain = mapply(extract_domain, url)) %>%
  group_by(domain, volume) %>%
  summarize(domain_count = sum(count)) %>%
  arrange(desc(domain_count)) %>%
  spread(volume, domain_count, fill = 0) %>%
  ungroup() %>%
  mutate_each(funs((. + 1) / sum(. + 1)), -domain) %>%
  mutate(logratio = log(high / low)) %>%
  arrange(desc(logratio)) %>%
  select(domain, high, low, logratio) %>%
  write_csv(str_c(source_folder, 'domain_ratios_all_time.csv'))

rm(compare_tweets_all_time)
gc()



# compare recent and established accounts
compare_recent_established <- tweets %>%
  inner_join(accounts_meta %>%
               filter(account_created >= max(account_created) - days(31)) %>%
               select(user_screen_name) %>%
               mutate(recency = 'recent') %>%
               bind_rows(accounts_meta %>%
                           filter(account_created < max(account_created) - days(31)) %>%
                           select(user_screen_name) %>%
                           mutate(recency = 'established')))


compare_recent_established %>%
  mutate(url = sapply(entities_urls, get_expanded_url)) %>%
  filter(!is.na(url)) %>%
  # unnest() %>%
  count(url, recency) %>%
  filter(!grepl('https://t.co/', url),
         !grepl('https://twitter.com/', url)) %>%
  mutate(domain = mapply(extract_domain, url)) %>%
  group_by(domain, recency) %>%
  summarize(domain_count = sum(n)) %>%
  arrange(desc(domain_count)) %>%
  spread(recency, domain_count, fill = 0) %>%
  ungroup() %>%
  mutate_each(funs((. + 1) / sum(. + 1)), -domain) %>%
  mutate(logratio = log(recent / established)) %>%
  arrange(desc(logratio)) %>%
  select(domain, recent, established, logratio) %>%
  write_csv(str_c(source_folder, 'compare_recent_established.csv'))
