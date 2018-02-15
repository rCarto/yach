library(pageviews)

# list countries wikipedia names
start = as.Date('2017-01-01')
end = as.Date("2017-12-31")

wiki <- article_pageviews(project = "en.wikipedia",
                       article = "North_Korea",
                       granularity = "daily",
                       start = start,
                       end = end,
                       user_type = c("user"),
                       platform = c("all"))


wiki$date <- as.Date(wiki$date)
save(wiki,file = "data/wiki.RData")

