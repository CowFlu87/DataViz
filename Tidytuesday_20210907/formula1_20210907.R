library(tidyverse)
library(lubridate)
library(ggtext)
library(showtext)


# Adding fonts and reading the data --------------------------
font_add_google("Karla", "Karla")
showtext_auto()

# Data extraction ---------------------------------------------
lap_times <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/lap_times.csv')
races <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/races.csv')

# Getting the fastest lap of Monza -----------------------------
monza <- races %>% 
  filter(
    circuitId == 14
  ) %>% 
  select(c(raceId, year)) %>% 
  left_join(
    lap_times %>% 
      group_by(
        raceId
      ) %>% 
      summarise(
        best_lap = min(milliseconds)
      ) %>% 
      ungroup(),
    by= "raceId"
  ) %>% 
  filter(
    !is.na(best_lap)
    )

# Plot time! ---------------------------------------------------

theme_update(panel.background   = element_rect(fill='#F8F8F8', color='#F8F8F8'),
             plot.background    = element_rect(fill='#F8F8F8', color= '#F8F8F8'),
             panel.border       = element_rect(fill = NA, color = NA),
             panel.grid.major.x = element_blank(),
             panel.grid.major.y = element_line(color = '#585d5f',
                                               size = 0.4
             ),
             panel.grid.minor.y  = element_blank(),
             panel.grid.minor.x  = element_blank(),
             axis.ticks.y        = element_blank(),
             axis.ticks.length.x = unit(6, "pt"),
             axis.text           = element_text(size=20),
             plot.margin         = unit(c(1,1,1.5,1.2),"cm"))


monza %>% 
  ggplot(aes(x=year, y=best_lap))+
  geom_rect(xmin  = 2002.5,
            xmax  = 2005.5,
            ymin  = 79000,
            ymax  = 84000,
            alpha = .01,
            fill  = "#FF2800")+
  annotate("text", 
           x          = 2008, 
           y          = 78000, 
           label      = "The record-breaker 3-year period",
           color      = "#525752",
           family     = "Karla",
           size       = 6.2, 
           lineheight = .9) +
  geom_line(color = "#525752",
            size  = 1.7)+
  geom_point(size   = 4,
             shape  = 21,
             color  = "#525752",
             fill   = "#F8F8F8",
             stroke = 2) +
  scale_x_continuous(breaks = seq(1996, 2021, 2))+
  scale_y_continuous(breaks = seq(75000, 90000, 5000),
                     labels = function(x) seconds_to_period(x/1000),
                     expand = c(0, 0))+
  coord_cartesian(ylim = c(75000, 95000),
                  xlim = c(1996, 2021))+
  labs(x  = "",
       y  = "",
       title = "Are the Formula 1 cars getting faster?",
       subtitle = "Fastest lap during the Italian Gran Prix across the years.<br>
       <i>Note</i>: During the 2003-2005, there were established records<br>such us the fastest race (2003),
       the race lap record (2004)<br> and the highest speed ever peaked (2005) on track",
       caption = "source: <b>TidyTuesday</b>") +
  theme(plot.title = element_markdown(face   = "bold",
                                      size   = 30,
                                      color  = "#FF2800",
                                      family = "Karla"),
        plot.subtitle = element_markdown(size   = 20,
                                         color  = "#525752",
                                         family = "Karla"),
        plot.caption = element_markdown(size   = 15,
                                        face   = "italic",
                                        color  = "#525752",
                                        family = "Karla"),
        axis.text = element_text(color  = "#525752",
                                 family = "Karla"),
        axis.text.y = element_text(hjust = -.5))


