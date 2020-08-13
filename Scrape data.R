#--------------------LOAD PACKAGES--------------
packages <- c("ggplot2", 'cowplot', 'lubridate', 'rvest',"dplyr", "viridis", "tidyverse")

# To install packages for the first time use the function below:
# lapply(packages, function(x) {install.packages(x)}) 

# If already installed, use this function
sapply(packages, library, character.only=T)

#------------------SCRAPE COVID PROJECTIONS-------------
covid <- "https://covid19-projections.com/#view-projections"
covid <- read_html(covid)

#Scrap tables
covidtable <- covid %>%
  html_nodes("table") %>%
  html_table(fill = TRUE)

#Isolate regional tables
covideu <- covidtable[[9]]
covidworld <- covidtable[[10]]
covidus <- covidtable[[8]][1,]
colnames(covidus)[1] <- c("Country")
colnames(covideu)[1] <- c("Country")
colnames(covidworld)[1] <- c("Country")

#Merge tables
covidproj <- merge(covideu, covidworld, all=T)
covidproj <- merge(covidproj, covidus, all=T)
covidproj$Country <- countrycode(covidproj$Country, origin = 'country.name', destination = 'iso3c', nomatch = NULL)

#Convert to numeric
covidproj$`Additional Deaths (% of Current Deaths)` <- gsub("%", "", covidproj$`Additional Deaths (% of Current Deaths)`)
covidproj[c("Current Deaths", "Projected Deaths - Mean", "Projected Deaths / 1M", 
            "Additional Deaths - Mean", "Additional Deaths (% of Current Deaths)", 
            "Projected Deaths - 2.5th Percentile", "Projected Deaths - 97.5th Percentile")] <- lapply(covidproj[c("Current Deaths", "Projected Deaths - Mean", "Projected Deaths / 1M", 
                                                                                                                  "Additional Deaths - Mean", "Additional Deaths (% of Current Deaths)", 
                                                                                                                  "Projected Deaths - 2.5th Percentile", "Projected Deaths - 97.5th Percentile")] ,
                                                                                                      function(xx) {
                                                                                                        gsub(",","",xx)
                                                                                                      })
covidproj[c("Current Deaths", "Projected Deaths - Mean", "Projected Deaths / 1M", 
            "Additional Deaths - Mean", "Additional Deaths (% of Current Deaths)", 
            "Projected Deaths - 2.5th Percentile", "Projected Deaths - 97.5th Percentile")] <- lapply(covidproj[c("Current Deaths", "Projected Deaths - Mean", "Projected Deaths / 1M", 
                                                                                                                  "Additional Deaths - Mean", "Additional Deaths (% of Current Deaths)", 
                                                                                                                  "Projected Deaths - 2.5th Percentile", "Projected Deaths - 97.5th Percentile")] ,
                                                                                                      function(xx) {
                                                                                                        as.numeric(as.character(xx))
                                                                                                      })

#Write csv
write.csv(covidproj, "covidproj.csv")

#--------------------------DRAW MAPS--------------------------------------
world <- map_data("world")
world <- world %>%
  dplyr::rename(Country = region) %>%
  dplyr::mutate(Country = countrycode(Country, origin = 'country.name', destination = 'iso3c'))

#Join datasets
worldmap <- inner_join(world, covidproj, by="Country")

#Plot world map
plain <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank(),
  plot.title = element_text(hjust = 0.5),
  panel.background = element_rect(fill = "#2C3E4F", colour = "#2C3E4F"),
  plot.background = element_rect(fill = "#2C3E4F", colour = "#2C3E4F") , 
  legend.background = element_rect(fill = "#2C3E4F", colour = "#2C3E4F"),
  text = element_text(colour = "lightgrey")
)

worldcovid <- ggplot(data = worldmap, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = `Projected Deaths / 1M`)) +
  scale_fill_distiller(palette ="Reds", direction = 1) + # or direction=1
  ggtitle("COVID deaths (projected deaths per 1M by Nov 2020)") +
  plain

worldcovid

#Joint maps
#Plot world map
plain <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank(),
  plot.title = element_text(hjust = 0.5),
  panel.background = element_rect(fill = "lightgrey", colour = "lightgrey"),
  plot.background = element_rect(fill = "lightgrey", colour = "lightgrey") , 
  legend.background = element_rect(fill = "lightgrey", colour = "lightgrey"),
  text = element_text(colour = "black")
)

one <- ggplot(data = worldmap, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = log(`Current Deaths`+1))) +
  scale_fill_viridis(direction=-1, option="D") + # or direction=1
  ggtitle("COVID current deaths (log)") +
  plain +
  labs(fill = "Deaths (log)")

three <- ggplot(data = worldmap, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = `Additional Deaths (% of Current Deaths)`)) +
  scale_fill_viridis(direction=-1, option="D") + # or direction=1
  ggtitle("Additional COVID Deaths by Nov 2020 (as % of Current Deaths)") +
  plain +
  labs(fill = "Additional Deaths (%)")

two <- ggplot(data = worldmap, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = `Projected Deaths / 1M`)) +
  scale_fill_viridis(direction=-1, option="D") + # or direction=1
  ggtitle("COVID Projected Deaths by Nov 2020 (per 1M)") +
  plain

cowplot::plot_grid(one,two,three, align= "v", ncol=1)
