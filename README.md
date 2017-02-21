# tweetmineR
Mine and analyze tweets in R using TidyVerse tools

To use this script, first generate a [TAGS archive](https://tags.hawksey.info/) of the tweets you want to mine. Then publish a CSV version of the ```Archive``` page to the web.

In ```mine_tweets.R```, add the url for that CSV file to the line: 

    google_sheet_csv <- ''

Then you can run the script in R studio chunk-by-chunk. 

This script will tell you the following:

- What are the most common words in a Twitter archive?  
- What are the most common URLs posted in a Twitter archive?  
- What are the root domains of the most common URLs in that archive?

The script will follow redirects from the ```https://t.co/``` URL shortener to find the source URL and domain. Because this process takes a long time, it will only run on URLs that occur with a frequency above a threshold set at the beginning of the script (```minimum_occurrences```; default is 10).

More features and details coming soon...

Happy mining!