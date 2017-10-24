devtools::install_github("hrbrmstr/streamgraph")
library(streamgraph)
library(dplyr)
library(babynames)
library(DT)

data <- read.csv("/home/jennifer/Desktop/Mineria/total6.csv", stringsAsFactors=FALSE)
data$date <- as.Date(data$date, format="%m/%d/%y")

streamgraph(data, interactive=TRUE) %>% sg_fill_brewer("PuOr")

data <- read.csv("/home/jennifer/Desktop/Mineria/total.csv", stringsAsFactors=FALSE)
data$date <- as.Date(data$date, format="%m/%d/%y")

head(data)

streamgraph(data, interactive=TRUE) %>% sg_colors("Reds")
