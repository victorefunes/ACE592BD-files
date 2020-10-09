#Import the necessary methods/packages from tweepy library
from tweepy.streaming import StreamListener
from tweepy import OAuthHandler
from tweepy import Stream
import time
import json
import csv

import os
path = os.path.join(os.path.expanduser('~'), "my_labs/outputs/Lab 2")

class StdOutListener(StreamListener):

    def __init__(self, time_limit=40):
        # start_time: is the start running time of the program
        self.start_time = time.time()
        # limit is the the assigned running time of the program, its default value is 40
        self.limit = time_limit
        # csvfile is the file to store the data
        self.csvfile = open(path + '/Twitter_Data.csv', 'w')
        #self.csvwriter = csv.writer(csvfile)
        
        super(StdOutListener, self).__init__()
        
    
    # The on_data function will keep crawling the twitter posts one by one, and store them into the data parameter
    # There are only two ways to stop the program inside:
    #                The on_data function returns false
    #                The on_error function catches a error
    def on_data(self, data):
        #  check if the program exceeded the time_limit of running time
        if (time.time() - self.start_time) < self.limit:
            print(round(time.time() - self.start_time))
            # convert data to json format
            d = json.loads(data)
            
            # check whether the current twitter post has coordinates
            geo_flag = 0
            try:
                lon = d['coordinates']['coordinates'][0]
                lat = d['coordinates']['coordinates'][1]
                geo_flag = 1
            except:
                print('no coordinates')

            # We only want to see the twitter posts with coordinates
            if( geo_flag == 1 ):
                text = d['text']
                creat_time = d['created_at']
                print( 'catch data with coordinates' )
                # use str() to convert double/float to string
                #self.csvwriter.writerow( [creat_time, str(lon), str(lat), text] )
                self.csvfile.write( creat_time + "," + str(lon) + "," + str(lat) + "," + text.rstrip() + '\n');
                return True
        # If time exceeded, stop the program.
        else: 
            self.csvfile.close()
            return False

    # If an error occured, stop the program
    def on_error(self, status):
        print(status)
		
if __name__ == '__main__':

    # This handles Twitter authentification and the connection to the Twitter Streaming API
    l = StdOutListener(3600)
    auth = OAuthHandler(consumer_key, consumer_secret)
    auth.set_access_token(access_token, access_token_secret)
    stream = Stream(auth, l)

    # A comma-separated list of longitude,latitude pairs specifying a set of bounding boxes to filter Tweets by
    # e.g. [-122.75,36.8,-121.75,37.8] indicates the area of San Francisco
    # NY Area
    stream.filter(locations=[-74.26,40.48,-73.70,40.92], encoding="utf-8", track="dog")
	
with open(os.path.join(os.path.expanduser('~'), "my_labs/outputs/Lab 2/Twitter_Data.csv")) as csvfile:
    # the csv file is seperated by commas, and the text of the tweet is the third entry in each row
    # we just have to make sure that the csv contains this information
    readcsv = csv.reader(csvfile, delimiter=',')
    for row in readcsv:
        if(len(row) > 3):
            print(row[3], '\n')	