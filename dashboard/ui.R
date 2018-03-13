library(shiny)

shinyUI(fluidPage(

  # Application title
  fluidRow(
    h1(strong('Social data analysis'), align = 'center'),
    h4(textOutput('total_tweets'), align = 'center')
  ),

  fluidRow(
    h2('Tweets per hour', align = 'center')
  ),

  fluidRow(
    column(8,

           # Tweets per hour
           tabsetPanel(
             tabPanel('Plot', plotOutput('tweets_per_hour_plot'))
           )
    ),
    column(4,
           tabsetPanel(
             tabPanel('Top RT', p(htmlOutput('most_rt_html'))),
             tabPanel('RTs (last 24h)', dataTableOutput('most_rt_table_24h')),
             tabPanel('RTs (whole archive)', dataTableOutput('most_rt_table'))
           )
    )
  ),

  fluidRow(
    h2('Most shared content', align = 'center')
  ),

  fluidRow(
    column(6,

           # Most linked domains/URLs - last 24 hours
           h3('Last 24 hours'),
           tabsetPanel(
             tabPanel('Plot', plotOutput('domain_count_plot_24h')),
             tabPanel('Domains', dataTableOutput('domain_count_table_24h')),
             tabPanel('URLs', dataTableOutput('url_count_table_24h'))
           )
    ),
    column(6,

           # Most linked domains/URLs - entire archive
           h3('Entire archive'),
           tabsetPanel(
             tabPanel('Plot', plotOutput('domain_count_plot')),
             tabPanel('Domains', dataTableOutput('domain_count_table')),
             tabPanel('URLs', dataTableOutput('url_count_table'))
           )
    )
  ),

  fluidRow(
    h3('comparing recently created accounts (less than one month old) with more established accounts', align = 'center')
  ),

  fluidRow(
    column(6,

           # Compare recency - tf-idf (all time)
           tabsetPanel(
             tabPanel('Plot', plotOutput('recency_ratios_plot')),
             tabPanel('Table', dataTableOutput('recency_ratios_table'))
           )
    ),
    column(6,

           # Compare recency - 2d (all time)
           tabsetPanel(
             tabPanel('Plot', plotOutput('recency_ratio_2d_plot')),
             tabPanel('Table', dataTableOutput('recency_ratio_2d_table'))
           )
    )
  ),

  fluidRow(
    h2('High-volume vs. low-volume accounts', align = 'center'),
    h4('comparing accounts with the most/least tweets in this archive', align = 'center')
  ),

  fluidRow(
    column(6,

           # Compare domains high-/low-volume accounts - tf-idf (this archive)
           tabsetPanel(
             tabPanel('Plot', plotOutput('domain_ratios_plot')),
             tabPanel('Table', dataTableOutput('domain_ratios_table'))
           )
    ),
    column(6,

           # Compare domains high-/low-volume accounts - 2d (this archive)
           tabsetPanel(
             tabPanel('Plot', plotOutput('domain_ratio_2d_plot')),
             tabPanel('Table', dataTableOutput('domain_ratio_2d_table'))
           )
    )
  ),

  fluidRow(
    h2('High-volume vs. low-volume accounts', align = 'center'),
    h4('comparing accounts with the most/least tweets in their entire account histories', align = 'center')
  ),

  fluidRow(
    column(6,

           # Compare domains high-/low-volume accounts - tf-idf (all time)
           tabsetPanel(
             tabPanel('Plot', plotOutput('domain_ratios_plot_all_time')),
             tabPanel('Table', dataTableOutput('domain_ratios_table_all_time'))
           )
    ),
    column(6,

           # Compare domains high-/low-volume accounts - 2d (all time)
           tabsetPanel(
             tabPanel('Plot', plotOutput('domain_ratio_2d_plot_all_time')),
             tabPanel('Table', dataTableOutput('domain_ratio_2d_table_all_time'))
           )
    )
  ),

  fluidRow(
    h2('Bigrams and trigrams', align = 'center'),
    h3('(tweets and replies only)', align = 'center')
  ),

  fluidRow(
    column(6,

           # Last 24 hours
           h3('Last 24 hours'),
           tabsetPanel(
             tabPanel('Bigrams (plot)', plotOutput('bigram_count_plot_24h')),
             tabPanel('Bigrams (table)', dataTableOutput('bigram_count_table_24h')),
             tabPanel('Trigrams (plot)', plotOutput('trigram_count_plot_24h')),
             tabPanel('Trigrams (table)', dataTableOutput('trigram_count_table_24h'))
           )
    ),
    column(6,

           # whole archive
           h3('Entire archive'),
           tabsetPanel(
             tabPanel('Bigrams (plot)', plotOutput('bigram_count_plot')),
             tabPanel('Bigrams (table)', dataTableOutput('bigram_count_table')),
             tabPanel('Trigrams (plot)', plotOutput('trigram_count_plot')),
             tabPanel('Trigrams (table)', dataTableOutput('trigram_count_table'))
           )
    )
  ),

  fluidRow(
    h2('Users and hashtags', align = 'center')
  ),

  fluidRow(
    h3('Last 24 hours', align = 'center')
  ),

  fluidRow(
    column(4,

           # Tweets per user
           tabsetPanel(
             tabPanel('Plot', plotOutput('user_count_plot_24h')),
             tabPanel('Table', dataTableOutput('user_count_table_24h'))
           )
    ),
    column(4,
           # Most mentioned users
           tabsetPanel(
             tabPanel('Plot', plotOutput('mention_count_plot_24h')),
             tabPanel('Table', dataTableOutput('mention_count_table_24h'))
           )
    ),
    column(4,
           # Most mentioned hashtags
           tabsetPanel(
             tabPanel('Plot', plotOutput('hashtag_count_plot_24h')),
             tabPanel('Table', dataTableOutput('hashtag_count_table_24h'))
           )
    )
  ),

  fluidRow(
    h3('Entire archive', align = 'center')
  ),

  fluidRow(
    column(4,

           # Tweets per user
           tabsetPanel(
             tabPanel('Plot', plotOutput('user_count_plot')),
             tabPanel('Table', dataTableOutput('user_count_table'))
           )
    ),
    column(4,
           # Most mentioned users
           tabsetPanel(
             tabPanel('Plot', plotOutput('mention_count_plot')),
             tabPanel('Table', dataTableOutput('mention_count_table'))
           )
    ),
    column(4,
           # Most mentioned hashtags
           tabsetPanel(
             tabPanel('Plot', plotOutput('hashtag_count_plot')),
             tabPanel('Table', dataTableOutput('hashtag_count_table'))
           )
    )
  ),

  fluidRow(
    h2('Account metadata', align = 'center')
  ),

  fluidRow(
    column(4,

           # Most prolific accounts (overall)
           tabsetPanel(
             tabPanel('Plot', plotOutput('user_statuses_plot')),
             tabPanel('Table', dataTableOutput('user_statuses_table'))
           )
    ),
    column(8,

           # Account creation date and total tweets
           tabsetPanel(
             tabPanel('Creation dates', plotOutput('user_creation_bar_plot')),
             tabPanel('Botnet clusters', plotOutput('user_creation_plot')),
             tabPanel('Table', dataTableOutput('user_creation_table'))
           )
    )
  )
))
