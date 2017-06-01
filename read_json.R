library(jsonlite)


hadley_orgs <- fromJSON("https://api.github.com/users/hadley/orgs")
hadley_repos <- fromJSON("https://api.github.com/users/hadley/repos")
gg_commits <- fromJSON("https://api.github.com/repos/hadley/ggplot2/commits")
gg_issues <- fromJSON("https://api.github.com/repos/hadley/ggplot2/issues")

url = "http://api.datausa.io/api/?show=geo&sumlevel=state&required=avg_wage"

t <- fromJSON(url)


url <- "http://api.datausa.io/api/logic/?show=geo&sumlevel=all"
tp <- fromJSON(url)


inc <- "https://api.datausa.io/api/csv/?sort=desc&show=geo&required=income%2Cincome_moe&sumlevel=tract&year=all"
inK <- "https://api.datausa.io/api/?sort=desc&show=geo&required=income%2Cincome_moe&sumlevel=county&year=all&where=geo%3A%5E05000US20"
income <- fromJSON(inc)
income2 <- read.csv(inc)

library(tidyverse)
x <- as.tibble(income$data)

total <- x %>% group_by(V2) %>% summarise( total = n())


api <- "http://db.datausa.io/api/csv/?show=geo&sumlevel=msa&required=pop&year=latest"
dat <- read.csv( api )
head( dat )

income2$income <- as.integer(income2$income)
income2    %>% arrange(desc(income) ) %>% head()
