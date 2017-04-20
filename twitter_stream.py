import tweepy

# authentication
consumer_key = ''
consumer_secret = ''
access_key = ''
access_secret = ''

# auth & api handlers
auth = tweepy.OAuthHandler(consumer_key, consumer_secret)
auth.set_access_token(access_key, access_secret)
api = tweepy.API(auth)

"""
print('Authenticated as %s' % api.me().screen_name)
"""

#override tweepy.StreamListener to add logic to on_status
class MyStreamListener(tweepy.StreamListener):

    def on_status(self, status):
        print(status.id_str,status.in_reply_to_status_id_str,status.in_reply_to_user_id_str,str(status.created_at),status.in_reply_to_screen_name,status.source,status.user._json['name'],status.user._json['screen_name'],status.user._json['created_at'],status.user._json['statuses_count'],status.user._json['description'],status.user._json['location'],status.user._json['verified'],status.user._json['followers_count'],status.user._json['friends_count'],status.user._json['url'],status.text,status.entities['hashtags'],status.entities['urls'],status.entities['user_mentions'])

        # all parameters possible here:
        # https://dev.twitter.com/overview/api/tweets

myStreamListener = MyStreamListener()
myStream = tweepy.Stream(auth = api.auth, listener=myStreamListener)

# print csv header
print('id_str,in_reply_to_user_id_str,in_reply_to_user_id_str,created_at,in_reply_to_screen_name,source,user_name,user_screen_name,user_created_at,user_statuses_count,user_description,user_location,user_verified,user_followers_count,user_friends_count,user_url,text,entities_hashtags,entities_urls,entities_user_mentions')

# generate stream by search term
myStream.filter(track=['Brexit'])
