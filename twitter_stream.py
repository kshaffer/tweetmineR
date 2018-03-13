import tweepy
import csv
import sys
import datetime
import time
import logging
from twitter_authentication import *

logger = logging.getLogger(__name__)

search_query = ['Twitter','@twitter', '#ilovehashtags']
filename = 'data/sources/stream-' + str(datetime.datetime.now()).replace(' ', '_').split('.')[0] + '.csv'

# auth & api handlers
auth = tweepy.OAuthHandler(consumer_key, consumer_secret)
auth.set_access_token(access_key, access_secret)
api = tweepy.API(auth)

"""
print('Authenticated as %s' % api.me().screen_name)
"""

# create output file and add header
with open(filename, 'w') as csvfile:
    writer = csv.writer(csvfile, delimiter=',', quotechar='"', quoting=csv.QUOTE_MINIMAL)
    header = ['id_str','user_id_str','in_reply_to_status_id_str','in_reply_to_user_id_str','created_at','in_reply_to_screen_name','source','user_name','user_screen_name','user_created_at','user_statuses_count','user_description','user_location','user_verified','user_followers_count','user_friends_count','user_url','text','entities_hashtags','entities_urls','entities_user_mentions', 'retweeted_status_id']
    writer.writerow(header)

# function for adding data to csv file
def write_csv(row_data, filename):
    with open(filename, 'a') as csvfile:
        writer = csv.writer(csvfile, delimiter=',', quotechar='"', quoting=csv.QUOTE_MINIMAL)
        writer.writerow(row_data)

#override tweepy.StreamListener to add logic to on_status
class MyStreamListener(tweepy.StreamListener):

    def on_status(self, status):
        print(status.text)
        if status.text[0:3] == 'RT ' and hasattr(status, 'retweeted_status'):
            try:
                retweeted_status_text = status.retweeted_status.extended_tweet['full_text']
            except:
                retweeted_status_text = status.text
                retweeted_status_id = status.retweeted_status.id_str
            try:
                write_csv([status.id_str, status.user._json['id_str'], status.in_reply_to_status_id_str, status.in_reply_to_user_id_str, status.created_at, status.in_reply_to_screen_name, status.source, status.user._json['name'], status.user._json['screen_name'], status.user._json['created_at'], status.user._json['statuses_count'], status.user._json['description'], status.user._json['location'], status.user._json['verified'], status.user._json['followers_count'],  status.user._json['friends_count'], status.user._json['url'], retweeted_status_text, status.entities['hashtags'], status.entities['urls'], status.entities['user_mentions'], retweeted_status_id], filename)
            except Exception as e:
                print(e)
        else:
            try:
                write_csv([status.id_str, status.user._json['id_str'], status.in_reply_to_status_id_str, status.in_reply_to_user_id_str, status.created_at, status.in_reply_to_screen_name, status.source, status.user._json['name'], status.user._json['screen_name'], status.user._json['created_at'], status.user._json['statuses_count'], status.user._json['description'], status.user._json['location'], status.user._json['verified'], status.user._json['followers_count'],  status.user._json['friends_count'], status.user._json['url'], status.extended_tweet['full_text'], status.entities['hashtags'], status.entities['urls'], status.entities['user_mentions'], ''], filename)
            except:
                try:
                    write_csv([status.id_str, status.user._json['id_str'], status.in_reply_to_status_id_str, status.in_reply_to_user_id_str, status.created_at, status.in_reply_to_screen_name, status.source, status.user._json['name'], status.user._json['screen_name'], status.user._json['created_at'], status.user._json['statuses_count'], status.user._json['description'], status.user._json['location'], status.user._json['verified'], status.user._json['followers_count'],  status.user._json['friends_count'], status.user._json['url'], status.text, status.entities['hashtags'], status.entities['urls'], status.entities['user_mentions'], ''], filename)
                except Exception as e:
                    print(e)

        # all parameters possible here:
        # https://dev.twitter.com/overview/api/tweets

myStreamListener = MyStreamListener()
myStream = tweepy.Stream(auth = api.auth, listener=myStreamListener)


# generate stream by search term
while True:
    logger.info("Starting stream tracking topics:\n{}".format(search_query))
    try:
        myStream.filter(track=search_query)
    except Exception as e:
        # Network error or stream failing behind
        # https://github.com/tweepy/tweepy/issues/448
        # prevent stream from crashing & attempt to recover
        logger.info(e)
        print(e)
        continue
