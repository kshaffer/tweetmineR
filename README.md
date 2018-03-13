# tweetmineR

Collect tweets with Python, parse them with R/TidyVerse, and generate a Shiny dashboard for high-level analysis and visualization.

For dashboard examples, see:

- [March for our Lives dashboard](https://thedisinformer.com/mfol/)  
- [115th US Congress dashboard](https://thedisinformer.com/congress/)  
- [Virginia 2017 election dashboard](https://kshaffer.shinyapps.io/shiny-social/)

See my blog post, ["Mining Twitter data with R, TidyText, and TAGS"](http://pushpullfork.com/2017/02/mining-twitter-data-tidy-text-tags/), for analysis examples (from an older version).

## Instructions (Python & Tweepy)

### Streaming

```twitter_stream.py``` is a Python script that uses Tweepy to fetch a live stream of tweets based on a search string. Register for a Twitter developer account and add your authentication details to ```twitter_authentication.py```. To start, either edit the following lines with your search query and output file name

```search_query = ['Twitter','@twitter', '#ilovehashtags']
filename = 'data/sources/stream-' + str(datetime.datetime.now()).replace(' ', '_').split('.')[0] + '.csv'```

Then run the script locally, or on a virtual private server for extended collection periods (using `nohup` to allow the process to continue after you close the connection).

Press CTRL-C to stop the script (on your local computer). Use `pgrep` and `kill` to stop a process running with `nohup` on your virtual private server.

### Searching Twitter history

```twitter_search.py``` works similarly to ```twitter_stream.py```, except it searches backwards in time, as far as the Twitter API will allow. It functions the same way as ```twitter_stream.py```: add your Twitter developer credentials to ```twitter_authentication.py```, and then run from the command line with the same command as above. Used in conjunction with ```twitter_stream.py```, it should collect as many tweets as possible, going both backwards and forwards in time, given a particular search query. Following [this example](https://www.karambelkar.info/2015/01/how-to-use-twitters-search-rest-api-most-effectively./), it uses AppAuthHandler in order to increase the maximum tweets downloaded per 15 minutes. It is set to pause when the API times out, so you can continue indefinitely until Twitter's historical limits kick in, potentially returning millions of tweets, depending on the popularity of the search terms.

### Analyzing Python/Tweepy results with R

```mine_tweets.R``` is an R script containing code to parse the output from the Python/Tweepy scripts. It will generate a number of files in the `data` folder that summarize the most used words, bigrams, trigrams, hashtags; domains and URLs linked most frequently; most prolific accounts; most retweeted tweets; etc.

The `dashboard` folder contains code for a Shiny dashboard that will take the output of `mine_tweets.R` and create a dashboard like the ones linked above for high-level analysis of trends in the collected tweets. It can be run locally, or deployed to a [Shiny Server](https://www.rstudio.com/products/shiny/shiny-server/).

Happy mining!
