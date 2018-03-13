library(shiny)
library(tidyverse)
library(stringr)
library(lubridate)
library(scales)

source_folder <- '../data/'
threshold <- read_csv(str_c(source_folder, 'threshold.csv'))$value
threshold_all_time <- 100000 # high-/low-volume account threshold (all-time)

accounts <- read_csv(str_c(source_folder, 'accounts.csv')) %>%
  arrange(desc(statuses))
bigram_count <- read_csv(str_c(source_folder, 'bigrams.csv'))
bigram_count_24h <- read_csv(str_c(source_folder, 'bigrams_24h.csv'))
compare_recent_established <- read_csv(str_c(source_folder, 'compare_recent_established.csv'))
domain_ratios <- read_csv(str_c(source_folder, 'domain_ratios.csv'))
domain_ratios_all_time <- read_csv(str_c(source_folder, 'domain_ratios_all_time.csv'))
domain_list <- read_csv(str_c(source_folder, 'domain_list.csv'))
domain_list_24h <- read_csv(str_c(source_folder, 'domain_list_24h.csv'))
hashtag_count <- read_csv(str_c(source_folder, 'hashtag_count.csv'))
hashtag_count_24h <- read_csv(str_c(source_folder, 'hashtag_count_24h.csv'))
mention_count <- read_csv(str_c(source_folder, 'mention_count.csv'))
mention_count_24h <- read_csv(str_c(source_folder, 'mention_count_24h.csv'))
most_retweeted_tweet <- read_csv(str_c(source_folder, 'most_retweeted_tweet.csv'))
most_retweeted_tweet_24h <- read_csv(str_c(source_folder, 'most_retweeted_tweet_24h.csv'))
most_retweeted_tweet_all <- read_csv(str_c(source_folder, 'most_retweeted_tweet_all.csv'))
most_retweeted_tweet_all_24h <- read_csv(str_c(source_folder, 'most_retweeted_tweet_all_24h.csv'))
most_retweeted_users <- read_csv(str_c(source_folder, 'most_retweeted_users.csv'))
most_retweeted_users_24h <- read_csv(str_c(source_folder, 'most_retweeted_users_24h.csv'))
trigram_count <- read_csv(str_c(source_folder, 'trigrams.csv'))
trigram_count_24h <- read_csv(str_c(source_folder, 'trigrams_24h.csv'))
tweets_per_hour <- read_csv(str_c(source_folder, 'tweets_per_hour.csv'))
url_list <- read_csv(str_c(source_folder, 'url_list.csv'))
url_list_24h <- read_csv(str_c(source_folder, 'url_list_24h.csv'))
user_count <- read_csv(str_c(source_folder, 'user_count.csv'))
user_count_24h <- read_csv(str_c(source_folder, 'user_count_24h.csv'))

shinyServer(function(input, output) {

  output$total_tweets <- renderText(str_c(sum(tweets_per_hour$tweet_count),
                                          ' tweets from ',
                                          str_sub(min(tweets_per_hour$time_floor), 1, 10),
                                          ' to ',
                                          str_sub(max(tweets_per_hour$time_floor), 1, 10),
                                          '.'))

  output$tweets_per_hour_plot <- renderPlot({
    tweets_per_hour %>%
      ggplot(aes(time_floor, tweet_count)) +
      geom_col(fill = 'dodgerblue4') +
      xlab('Date/time (UTC)') +
      ylab('Number of tweets (per hour)') +
      ggtitle('Frequency of tweets')

  })

  output$most_rt_all <- renderUI({HTML(str_c(most_retweeted_tweet$text %>%
                                               str_replace('^RT ', ''),
                                             '<br/><br/>retweeted ',
                                             most_retweeted_tweet$n,
                                             ' times'))})

  output$most_rt_24h <- renderUI({HTML(str_c(most_retweeted_tweet_24h$text %>%
                                               str_replace('^RT ', ''),
                                             '<br/><br/>retweeted ',
                                             most_retweeted_tweet_24h$n,
                                             ' times'))})

  output$most_rt_html <- renderUI({HTML(
    str_c('<b>Most retweeted in the last 24 hours:</b><br/>',
          # '<blockquote class="twitter-tweet" data-lang="en"><p lang="en" dir="ltr">',
          # most_retweeted_tweet_24h$text %>%
          #   str_replace('^RT.*?: ', ''),
          # '</blockquote><script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>',

          most_retweeted_tweet_24h$text %>%
            str_replace('^RT ', ''),
          '<br/><br/>retweeted ',
          most_retweeted_tweet_24h$n,
          ' times',
          '<br/><br/>',
          '<b>Most retweeted since ',
          str_sub(min(tweets_per_hour$time_floor), 1, 10),
          ':</b><br/>',
          most_retweeted_tweet$text %>%
            str_replace('^RT ', ''),
          '<br/><br/>retweeted ',
          most_retweeted_tweet$n,
          ' times')
  )})

  output$most_rt_table <- renderDataTable({
    most_retweeted_tweet_all
  })

  output$most_rt_table_24h <- renderDataTable({
    most_retweeted_tweet_all_24h
  })

  output$user_count_table <- renderDataTable({
    user_count
  })

  output$user_count_plot <- renderPlot({
    user_count[1:20,] %>%
      mutate(user_screen_name = reorder(user_screen_name, n)) %>%
      ggplot(aes(user_screen_name, n, fill = user_screen_name)) +
      geom_col() +
      xlab('User handle') +
      ylab('Number of tweets') +
      ggtitle('Most prolific accounts (including RTs)') +
      coord_flip() +
      scale_fill_hue(h = c(15, 265)) +
      theme(legend.position = 'none')
  })

  output$mention_count_table <- renderDataTable({
    mention_count
  })

  output$mention_count_plot <- renderPlot({
    mention_count[1:20,] %>%
      mutate(word = reorder(word, n)) %>%
      ggplot(aes(word, n, fill = word)) +
      geom_col() +
      xlab('User handle') +
      ylab('Number of mentions') +
      ggtitle('Most mentioned and retweeted accounts') +
      coord_flip() +
      scale_fill_hue(h = c(15, 265)) +
      theme(legend.position = 'none')
  })

  output$hashtag_count_table <- renderDataTable({
    hashtag_count
  })

  output$hashtag_count_plot <- renderPlot({
    hashtag_count[1:20,] %>%
      mutate(word = reorder(word, n)) %>%
      ggplot(aes(word, n, fill = word)) +
      geom_col() +
      xlab('Hashtag') +
      ylab('Number of mentions') +
      ggtitle('Most mentioned hashtags (including RTs)') +
      coord_flip() +
      scale_fill_hue(h = c(15, 265)) +
      theme(legend.position = 'none')
  })

  output$user_count_table_24h <- renderDataTable({
    user_count_24h
  })

  output$user_count_plot_24h <- renderPlot({
    user_count_24h[1:20,] %>%
      mutate(user_screen_name = reorder(user_screen_name, n)) %>%
      ggplot(aes(user_screen_name, n, fill = user_screen_name)) +
      geom_col() +
      xlab('User handle') +
      ylab('Number of tweets') +
      ggtitle('Most prolific accounts (including RTs)') +
      coord_flip() +
      scale_fill_hue(h = c(15, 265)) +
      theme(legend.position = 'none')
  })

  output$mention_count_table_24h <- renderDataTable({
    mention_count_24h
  })

  output$mention_count_plot_24h <- renderPlot({
    mention_count_24h[1:20,] %>%
      mutate(word = reorder(word, n)) %>%
      ggplot(aes(word, n, fill = word)) +
      geom_col() +
      xlab('User handle') +
      ylab('Number of mentions') +
      ggtitle('Most mentioned and retweeted accounts') +
      coord_flip() +
      scale_fill_hue(h = c(15, 265)) +
      theme(legend.position = 'none')
  })

  output$hashtag_count_table_24h <- renderDataTable({
    hashtag_count_24h
  })

  output$hashtag_count_plot_24h <- renderPlot({
    hashtag_count_24h[1:20,] %>%
      mutate(word = reorder(word, n)) %>%
      ggplot(aes(word, n, fill = word)) +
      geom_col() +
      xlab('Hashtag') +
      ylab('Number of mentions') +
      ggtitle('Most mentioned hashtags (including RTs)') +
      coord_flip() +
      scale_fill_hue(h = c(15, 265)) +
      theme(legend.position = 'none')
  })

  output$domain_count_table <- renderDataTable({
    domain_list
  })

  output$domain_count_plot <- renderPlot({
    domain_list[1:20,] %>%
      mutate(domain = reorder(domain, domain_count)) %>%
      ggplot(aes(domain, domain_count, fill = domain)) +
      geom_col() +
      xlab('Domain') +
      ylab('Number of tweets linking to domain') +
      ggtitle('Most shared domains (including RTs)') +
      coord_flip() +
      scale_fill_hue(h = c(15, 265)) +
      theme(legend.position = 'none')
  })

  output$url_count_table <- renderDataTable({
    url_list
  })

  output$domain_count_table_24h <- renderDataTable({
    domain_list_24h
  })

  output$domain_count_plot_24h <- renderPlot({
    domain_list_24h[1:20,] %>%
      mutate(domain = reorder(domain, domain_count)) %>%
      ggplot(aes(domain, domain_count, fill = domain)) +
      geom_col() +
      xlab('Domain') +
      ylab('Number of tweets linking to domain') +
      ggtitle('Most shared domains, last 24 hours (counting RTs)') +
      coord_flip() +
      scale_fill_hue(h = c(15, 265)) +
      theme(legend.position = 'none')
  })

  output$url_count_table_24h <- renderDataTable({
    url_list_24h
  })

  output$bigram_count_table <- renderDataTable({
    bigram_count
  })

  output$bigram_count_plot <- renderPlot({
    bigram_count[1:20,] %>%
      mutate(bigram = reorder(bigram, n)) %>%
      ggplot(aes(bigram, n, fill = bigram)) +
      geom_col() +
      xlab('Bigram') +
      ylab('Number of occurrences') +
      ggtitle('Most common bigrams (excluding RTs)') +
      coord_flip() +
      scale_fill_hue(h = c(15, 265)) +
      theme(legend.position = 'none')
  })

  output$trigram_count_table <- renderDataTable({
    trigram_count
  })

  output$trigram_count_plot <- renderPlot({
    trigram_count[1:20,] %>%
      mutate(trigram = reorder(trigram, n)) %>%
      ggplot(aes(trigram, n, fill = trigram)) +
      geom_col() +
      xlab('trigram') +
      ylab('Number of occurrences') +
      ggtitle('Most common trigrams (excluding RTs)') +
      coord_flip() +
      scale_fill_hue(h = c(15, 265)) +
      theme(legend.position = 'none')
  })

  output$bigram_count_table_24h <- renderDataTable({
    bigram_count_24h
  })

  output$bigram_count_plot_24h <- renderPlot({
    bigram_count_24h[1:20,] %>%
      mutate(bigram = reorder(bigram, n)) %>%
      ggplot(aes(bigram, n, fill = bigram)) +
      geom_col() +
      xlab('Bigram') +
      ylab('Number of occurrences') +
      ggtitle('Most common bigrams (excluding RTs)') +
      coord_flip() +
      scale_fill_hue(h = c(15, 265)) +
      theme(legend.position = 'none')
  })

  output$trigram_count_table_24h <- renderDataTable({
    trigram_count_24h
  })

  output$trigram_count_plot_24h <- renderPlot({
    trigram_count_24h[1:20,] %>%
      mutate(trigram = reorder(trigram, n)) %>%
      ggplot(aes(trigram, n, fill = trigram)) +
      geom_col() +
      xlab('trigram') +
      ylab('Number of occurrences') +
      ggtitle('Most common trigrams (excluding RTs)') +
      coord_flip() +
      scale_fill_hue(h = c(15, 265)) +
      theme(legend.position = 'none')
  })

  output$user_statuses_table <- renderDataTable({
    accounts
  })

  output$user_statuses_plot <- renderPlot({
    accounts[1:20,] %>%
      mutate(user_screen_name = reorder(user_screen_name, statuses)) %>%
      ggplot(aes(user_screen_name, statuses, fill = user_screen_name)) +
      geom_col() +
      xlab('User') +
      ylab('Total account tweets') +
      ggtitle('Most prolific accounts in archive') +
      coord_flip() +
      scale_fill_hue(h = c(15, 265)) +
      theme(legend.position = 'none')
  })

  output$user_creation_table <- renderDataTable({
    accounts
  })

  output$user_creation_plot <- renderPlot({
    accounts %>%
      # filter(statuses > 100000,
      #        statuses < 500000) %>%
      ggplot(aes(account_created, statuses)) +
      geom_jitter(alpha = 0.1) +
      xlab('Date account created') +
      ylab('Total account tweets') +
      ggtitle('Searching for accounts created on the same date with the same number of tweets')

  })

  output$user_creation_bar_plot <- renderPlot({
    accounts %>%
      ggplot(aes(account_created)) +
      geom_bar(color = 'dodgerblue4') +
      xlab('Date account created') +
      ylab('Total accounts created on date') +
      ggtitle('Dates on which participating accounts were created')

  })

  output$domain_ratios_table <- renderDataTable({
    domain_ratios %>%
      inner_join(domain_list[1:100,]) %>%
      group_by(logratio < 0) %>%
      top_n(15, abs(logratio)) %>%
      ungroup() %>%
      mutate(domain = reorder(domain, logratio))
  })

  output$domain_ratios_plot <- renderPlot({
    domain_ratios %>%
      inner_join(domain_list[1:100,]) %>%
      group_by(logratio < 0) %>%
      top_n(15, abs(logratio)) %>%
      ungroup() %>%
      mutate(domain = reorder(domain, logratio)) %>%
      ggplot(aes(domain, logratio, fill = logratio < 0)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      ylab("log odds ratio (high-volume/low-volume)") +
      scale_fill_discrete(name = "", labels = c("high-volume", "low-volume")) +
      ggtitle(str_c('Most characteristic domains in tweets and retweets\n(from the 100 most shared domains).\nHigh-volume accounts have ',
                    threshold,
                    ' or more tweets in corpus.\nLow-volume accounts have less than ',
                    threshold,
                    ' tweets.'))
  })

  output$domain_ratio_2d_table <- renderDataTable({
    domain_ratios %>%
      inner_join(domain_list[1:50,])
  })

  output$domain_ratio_2d_plot <- renderPlot({
    domain_ratios %>%
      inner_join(domain_list[1:50,]) %>%
      ggplot(aes(x = low, y = high)) +
      geom_abline(color = "gray40", lty = 2) +
      #geom_jitter(alpha = 0.1, size = 1.5, width = 0.1, height = 0.1) +
      geom_point(alpha = 0.1) +
      geom_text(aes(label = domain), nudge_x = 0.03, nudge_y = 0.03) +
      scale_x_log10(labels = percent_format()) +
      scale_y_log10(labels = percent_format()) +
      scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
      theme(legend.position="none") +
      labs(y = str_c('high-volume accts (',
                     threshold,
                     ' or more tweets)'),
           x = str_c('low-volume accts (less than ',
                     threshold,
                     ' tweets)')) +
      ggtitle('50 domains most shared in tweets and retweets')
  })

  output$domain_ratios_table_all_time <- renderDataTable({
    domain_ratios_all_time %>%
      inner_join(domain_list[1:100,]) %>%
      group_by(logratio < 0) %>%
      top_n(15, abs(logratio)) %>%
      ungroup() %>%
      mutate(domain = reorder(domain, logratio))
  })

  output$domain_ratios_plot_all_time <- renderPlot({
    domain_ratios_all_time %>%
      inner_join(domain_list[1:100,]) %>%
      group_by(logratio < 0) %>%
      top_n(15, abs(logratio)) %>%
      ungroup() %>%
      mutate(domain = reorder(domain, logratio)) %>%
      ggplot(aes(domain, logratio, fill = logratio < 0)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      ylab("log odds ratio (high-volume/low-volume)") +
      scale_fill_discrete(name = "", labels = c("high-volume", "low-volume")) +
      ggtitle(str_c('Most characteristic domains in tweets and retweets (from the 100 most shared domains).\nHigh-volume accounts have ',
                    as.character(threshold_all_time),
                    ' or more tweets in account history.\nLow-volume accounts have less than ',
                    as.character(threshold_all_time),
                    ' tweets.'))
  })

  output$domain_ratio_2d_table_all_time <- renderDataTable({
    domain_ratios_all_time %>%
      inner_join(domain_list[1:50,])
  })

  output$domain_ratio_2d_plot_all_time <- renderPlot({
    domain_ratios_all_time %>%
      inner_join(domain_list[1:50,]) %>%
      ggplot(aes(x = low, y = high)) +
      geom_abline(color = "gray40", lty = 2) +
      #geom_jitter(alpha = 0.1, size = 1.5, width = 0.1, height = 0.1) +
      geom_point(alpha = 0.1) +
      geom_text(aes(label = domain), nudge_x = 0.03, nudge_y = 0.03) +
      scale_x_log10(labels = percent_format()) +
      scale_y_log10(labels = percent_format()) +
      scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
      theme(legend.position="none") +
      labs(y = str_c('high-volume accts (',
                     as.character(threshold_all_time),
                     ' or more tweets)'),
           x = str_c('low-volume accts (less than ',
                     as.character(threshold_all_time),
                     ' tweets)')) +
      ggtitle('50 domains most shared in tweets and retweets')
  })

  output$recency_ratios_table <- renderDataTable({
    compare_recent_established %>%
      inner_join(domain_list[1:100,]) %>%
      group_by(logratio < 0) %>%
      top_n(15, abs(logratio)) %>%
      ungroup() %>%
      mutate(domain = reorder(domain, logratio))
  })

  output$recency_ratios_plot <- renderPlot({
    compare_recent_established %>%
      inner_join(domain_list[1:100,]) %>%
      group_by(logratio < 0) %>%
      top_n(15, abs(logratio)) %>%
      ungroup() %>%
      mutate(domain = reorder(domain, logratio)) %>%
      ggplot(aes(domain, logratio, fill = logratio < 0)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      ylab("log odds ratio (recently created/established accounts)") +
      scale_fill_discrete(name = "", labels = c("recently created", "established")) +
      ggtitle(str_c('Most characteristic domains in tweets and retweets (from the 100 most shared domains).\n',
                    'Recent accounts were created in the last month.\n',
                    'Established accounts were created more than a month ago.'))
  })

  output$recency_ratio_2d_table <- renderDataTable({
    compare_recent_established %>%
      inner_join(domain_list[1:50,])
  })

  output$recency_ratio_2d_plot <- renderPlot({
    compare_recent_established %>%
      inner_join(domain_list[1:50,]) %>%
      ggplot(aes(x = established, y = recent)) +
      geom_abline(color = "gray40", lty = 2) +
      #geom_jitter(alpha = 0.1, size = 1.5, width = 0.1, height = 0.1) +
      geom_point(alpha = 0.1) +
      geom_text(aes(label = domain), nudge_x = 0.03, nudge_y = 0.03) +
      scale_x_log10(labels = percent_format()) +
      scale_y_log10(labels = percent_format()) +
      # scale_color_gradient(limits = c(0, 0.001), established = "darkslategray4", recent = "gray75") +
      theme(legend.position="none") +
      labs(y = 'Accounts created in the past month',
           x = 'Accounts created more than a month ago') +
      ggtitle('50 domains most shared in tweets and retweets')
  })

})
