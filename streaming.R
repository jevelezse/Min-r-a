library(streamgraph)
library(dplyr)
library(babynames)
library(DT)

ggplot2::movies %>%
  select(year, Action, Animation, Comedy, Drama, Documentary, Romance, Short) %>%
  tidyr::gather(genre, value, -year) %>%
  group_by(year, genre) %>%
  tally(wt=value) -> dat

streamgraph(dat, "genre", "n", "year", interactive=TRUE) %>%
  sg_fill_brewer("PuOr")

data<- read.csv("/home/jennifer/Desktop/Mineria/total6.csv", stringsAsFactors=FALSE)
data$date <- as.Date(data$date, format="%m/%d/%y")

streamgraph(data, interactive=TRUE) %>% sg_colors("PuOr")
