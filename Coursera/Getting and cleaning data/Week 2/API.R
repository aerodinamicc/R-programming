#API
library(httr)
#Accessing twitter from R
myapp = oauth_app("twitter", key="consumerkey", secret="consumersecret")  #start the auth process
sig =sign_oauth1.0(myapp, token = "ur token here", token_secret = "urTokenSecret") #used when you try to get the data
homeTL = GET("https://api.twitter.com/1.1/statuses/home_timeline,json", sig) #thats the url that corresponds to the twitter API(1.1 ver);

#Converting the json objects
json1 =content(homeTL) #the following 3 basically use the jsonLite package to reformat the data
json2 =jsonLite::fromJSON(toJSON(json1))
json2[1,1:4]

#What url to use
#Go to the documentation of the twitter site - Resource URL
#Doc for the latest version of the API
# https://dev.twitter.com/docs/api/1.1/overview


