#Authenticate.R
library(twitteR)
#api_key<- "xx"
#api_secret<- "xx"
#access_token<- "xx"
#access_token_secret<- "xx"
#setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)


api_key <- "izRunXa7pXEAwSjUML1n99Ef0"
api_secret <- "vwTTcuFOxeqM9VhEiiDJdLtVHXVdGXxyAbKEnow7kvqzww88JI"
access_token <- "1950002257-WG5hnJYTK7wYezHQUbMXnAsIqOffhzfFAoL5NlE"
access_token_secret <- "4OdiqIR2W4kdJbcnEME2BmkhYnpQvj36QIQaAnNDYR20H"

setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)


install.packages('ggmap')
library(ggmap)
geocode('Indianapolis')
