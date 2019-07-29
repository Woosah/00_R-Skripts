library(readr)
library(dplyr)
library(stringr)
library(purrr)
library(memoise)
library(osrm)
library(geosphere)
library(ggplot2)
library(gganimate)

# Inpsired by https://www.reddit.com/r/dataisbeautiful/comments/7pwnub/optimal_routes_from_the_geographic_center_of_the/.

city <- read_csv("https://github.com/datawookie/data-diaspora/blob/master/spatial/city-country.csv?raw=true", comment = "#", na = "") %>%
  select(-iso2, -country) %>%
  mutate(id = as.character(id))
country <- read_csv("https://github.com/datawookie/data-diaspora/raw/master/spatial/country-continent.csv", comment = "#", na = "") %>%
  select(-iso2)

city <- city %>%
  inner_join(country, by = "iso3") %>%
  filter(
    continent == "Europe",
    !iso3 %in% c("GBR", "IRL", "MLT", "FRO", "IMN", "CYP"),
    !str_detect(country, "^Iceland"),
    !admin_name %in% c("Canary Islands", "Madeira", "Azores", "Kamchatskiy Kray", "Sakhalinskaya Oblast’",
                       "Chukotskiy Avtonomnyy Okrug", "Sardegna", "Corsica", "Balearic Islands", "Kríti",
                       "Sicilia"),
    !(city %in% c("Oktyabrskiy", "Noginsk", "Melilla"))
  )

rome <- city %>% filter(city == "Rome") %>% select(id, lng, lat)
rest <- city %>% filter(city != "Rome") %>% select(id, lng, lat)

SINK = tempfile()

find_route = memoise(function(id, lng, lat) {
  message(id)
  while (TRUE) {
    # Capture output so that can identify "no route" error.
    #
    sinkfile <- file(SINK, open = "wt")
    #
    sink(sinkfile, type = "message")
    route = possibly(osrmRoute, otherwise = NULL)(src = c(id, lng, lat), dst = rome)
    sink(type = "message")
    #
    close(sinkfile)
    
    cat("  Pausing... ")
    Sys.sleep(rpois(1, 15))
    cat("done!\n")
    
    # Check for "no route".
    #
    if (readLines(SINK) %>% str_detect("NoRoute") %>% any()) {
      return(NULL)
    }
    
    # Check for distance >= 10000 km.
    #
    if (readLines(SINK) %>% str_detect("Total distance between all coordinates cannot exceed 10000 km") %>% any()) {
      return(NULL)
    }
    
    if (is.null(route)) {
      cat("  Waiting to retry... ")
      Sys.sleep(600)
      cat("done!\n")
    } else {
      return(route %>% mutate(id = id))
    }
  }
})

routes <- pmap(rest, find_route) %>% bind_rows()

# That took a while, so let's store the results just in case.
#
saveRDS(routes, "rome-routes.rds")

# DISTANCES -----------------------------------------------------------------------------------------------------------

routes <- routes %>%
  group_by(id) %>%
  mutate(
    frac = row_number() / n(),
    step = row_number()
  )

# MAP -----------------------------------------------------------------------------------------------------------------

p <- ggplot(routes) +
  geom_polygon(data = map_data('world'), aes(x=long, y=lat, group=group), fill = "grey90") +
  geom_path(aes(x = lon, y = lat, group = id), lwd = 1, alpha = 0.125, colour = "#3498db") +
  geom_point(data = rome, aes(x = lng, y = lat), pch = 21, size = 5, fill = "black", alpha = 0.3) +
  # geom_point(data = rest, aes(x = lng, y = lat), alpha = 0.3) +
  coord_cartesian(xlim = c(-10, 165), ylim = c(35, 75)) +
  theme_void() +
  NULL

p
ggsave("all-roads-lead-to-rome.png", width = 16, height = 9)

p +
  transition_reveal(step)
