## load mwk
library(mwk)

## from maps package
cities <- maps::us.cities[-6]

## rename
names(cities)[c(1:3, 5)] <- c("city", "state", "size", "lng")

## remove state from city
cities$city <- sub("\\s+[A-Z]{2}$", "", cities$city)

## recalc size to be on 12.5-25 scale
cities$size <- with(cities, sqrt(size / wmax(size, 2)))
cities$size <- with(cities, (size * 12.5) + 12.5)
cities$size[cities$size > 25] <- 25

## create id variable
cities$id <- seq_len(nrow(cities))

## repos id to front and make tibble
cities <- repos_front(cities, id)

## function to format coords for searches
city2coords <- function(lat, lng, size) {
  paste0(round(lng, 4), ",", round(lat, 4), ",", "25mi")
}

## create coords variable
cities$coords <- with(cities, city2coords(lng, lat, size))

## select top two cities from each state
top2cities <- cities %>%
  group_by(state) %>%
  arrange(state, desc(size)) %>%
  mutate(keep = TRUE,
    keep = if (length(keep) > 2) c(TRUE, TRUE, rep(FALSE, length(keep) - 2)) else keep) %>%
  filter(keep) %>%
  select(-keep) %>%
  ungroup()

## get biggest cities not in top2cities
bigcities <- cities %>%
  filter(!id %in% top2cities$id) %>%
  arrange(desc(size)) %>%
  slice(1:80)

## combine
cities <- combine_rows(top2cities, bigcities)
