library("twitteR")
library("tm")



#to get your consumerKey and consumerSecret see the twitteR documentation for instructions
consumer_key <- '4DUI703bNL7eK5C7dzvX9gfx4'
consumer_secret <- 'PY8x4vwVC0qHAy7z3d8WWtDlQj7kpqG6106Xoy8oFMikITzrnJ'
access_token <- '705229770267693057-2wTb9S7qeRqzPoM1ihwKqViXkqbCqs4'
access_secret <- 'jO9czwMJrSZdNmqEp51krdBeZTIchZEEb6cyiBZ9ON4tz'
setup_twitter_oauth(consumer_key,
                    consumer_secret,
                    access_token,
                    access_secret)

r_stats<- searchTwitter("#ControlAlZika",n=500)
r_stats_text <- sapply(r_stats, function(x) x$getText())

write.csv(r_stats_text, 'controlalzika.csv')



