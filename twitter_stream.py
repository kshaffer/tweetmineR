import tweepy
import csv
import sys
from twitter_authentication import *

if len(sys.argv) >= 2:
    search_query = sys.argv[1]
    filename = sys.argv[2]
else:
    search_query = ['fox']
    filename = 'fox_test.csv'

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
    header = ['id_str','in_reply_to_status_id_str','in_reply_to_user_id_str','created_at','in_reply_to_screen_name','source','user_name','user_screen_name','user_created_at','user_statuses_count','user_description','user_location','user_verified','user_followers_count','user_friends_count','user_url','text','entities_hashtags','entities_urls','entities_user_mentions']
    writer.writerow(header)

# function for adding data to csv file
def write_csv(row_data, filename):
    with open(filename, 'a') as csvfile:
        writer = csv.writer(csvfile, delimiter=',', quotechar='"', quoting=csv.QUOTE_MINIMAL)
        writer.writerow(row_data)
            
#override tweepy.StreamListener to add logic to on_status
class MyStreamListener(tweepy.StreamListener):

    def on_status(self, status):
        write_csv([status.id_str, status.in_reply_to_status_id_str, status.in_reply_to_user_id_str, status.created_at, status.in_reply_to_screen_name, status.source, status.user._json['name'], status.user._json['screen_name'], status.user._json['created_at'], status.user._json['statuses_count'], status.user._json['description'], status.user._json['location'], status.user._json['verified'], status.user._json['followers_count'],  status.user._json['friends_count'], status.user._json['url'], status.text, status.entities['hashtags'], status.entities['urls'], status.entities['user_mentions']], filename)

        # all parameters possible here:
        # https://dev.twitter.com/overview/api/tweets

myStreamListener = MyStreamListener()
myStream = tweepy.Stream(auth = api.auth, listener=myStreamListener)


# generate stream by search term
myStream.filter(track=search_query)

