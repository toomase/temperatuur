# weather radials tüüpi graafik Tallinna temperatuuri kohta aasta jooksul
# aluseks blogipostitus: http://jkunst.com/r/how-to-weather-radials/

library(stationaRy)
library(dplyr)
library(highcharter)
library(lubridate)
library(ggplot2)
library(viridis)
library(stringr)
library("htmlwidgets")

# funktsioon pärib valitud aasta kohta iga päeva max, min ja keskmise temperatuuri
# ja formatib tabeli graafiku jaoks sobivaks
temperatuurid <- function(aasta = 2015){
    temp_raw <- get_isd_stations() %>%
        select_isd_station(name = "tallinn") %>%
        get_isd_station_data(startyear = aasta, endyear = aasta) %>%
        select(time, temp) %>%
        mutate(kp = as.Date(time)) %>%
        group_by(kp) %>%
        summarise(min_temp = round(min(temp, na.rm = TRUE), 0),
                  max_temp = round(max(temp, na.rm = TRUE), 0),
                  mean_temp = round(mean(temp, na.rm = TRUE), 0)) %>%
        mutate(id = seq(nrow(.)), 
               tmstmp = datetime_to_timestamp(kp),
               month = month(ymd(kp)), 
               color = colorize_vector(mean_temp, "C"),
               y = max_temp - min_temp, 
               max = max_temp, 
               min = min_temp) %>% 
        select(x = tmstmp, y, high = max_temp, low = min_temp, name = kp,
               color, keskmine = mean_temp, max, min)
    
    return(temp_raw)
}

# valitud aastate temperatuuri andmed
temp_2015 <- temperatuurid(aasta = 2015)

# tooltip jaoks andmed
x <- c("min", "keskmine", "max")
y <- sprintf("{point.%s}", tolower(x))
tltip <- tooltip_table(x, y)

# graafik
graafik_temp_2015 <- highchart() %>% 
    # graafik y telje max ja min väärtusteg (high ja low)
    hc_chart(type = "columnrange", polar = TRUE) %>%
    # ära kuva legendi
    hc_plotOptions(series = list(showInLegend = FALSE)) %>%
    # x-telje kujundamine
    hc_xAxis(gridLineWidth = 0.5, type = "datetime",
             tickInterval = 30 * 24 * 3600 * 1000,
             labels = list(format = "{value: %b}")) %>%
    # y-telje max ja min väärtused ja kuva esimene ja viimane label
    hc_yAxis(max = 20, min = -20, labels = list(format = "{value} C°"),
             tickInterval = 10, showFirstLabel = TRUE, showLastLabel = TRUE) %>%
    # andmete lisamine data frame-st
    hc_add_series_df(data = temp_2015) %>% 
    # minimalistlik taust
    hc_add_theme(hc_theme_smpl()) %>% 
    # tooltip seadistamine
    hc_tooltip(useHTML = TRUE,
        headerFormat = as.character(tags$small("{point.x:%d %B, %Y}")),
        pointFormat = tltip) %>%
    # pealkiri
    hc_title(text = "Temperatuurid Tallinnas 2015.a", align = "center")

# salvesta graafik, et see iframe abil blogisse lisada
saveWidget(graafik_temp_2015, file="temp_graafik_2015.html", 
           selfcontained = TRUE)