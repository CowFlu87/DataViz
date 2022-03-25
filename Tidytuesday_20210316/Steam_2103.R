library(tidyverse)
library(ggtext)
library(showtext)


# Adding fonts and reading the data -------------------
font_add_google("Zilla Slab", "Zilla Slab")
showtext_auto()



steam <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-16/games.csv')

# Cleaning the data a little bit ---------------------------------------
CSGO <- steam %>% filter(gamename == 'Counter-Strike: Global Offensive') %>% 
  mutate(number_month = factor(month, levels=c('January',
                                               'February',
                                               'March',
                                               'April',
                                               'May',
                                               'June',
                                               'July',
                                               'August',
                                               'September',
                                               'October',
                                               'November',
                                               'December')),
         number_month = as.numeric(number_month),
         date         = paste0(year, '-', number_month, '-01'),
         date         = as.Date(date))
  
# Updating the graphic theme -----------------------------------------------
theme_update(panel.background   = element_rect(fill='#fcf3cf', color='#fcf3cf'),
             plot.background    = element_rect(fill='#fcf3cf', color= '#fcf3cf'),
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

# Plotting! ------------------------------------------
# Getting the peak of the time series
max_val <- max(CSGO$peak)

# Getting the lowest value in 2010
june18 <- CSGO %>% 
  filter(date == as.Date('2018-06-01')) %>% 
  pull(peak)

CSGO %>% 
  ggplot(aes(x=date, y=peak))+
  # Adding first the points then the line for overlapping purposes.
  geom_point(aes(x = as.Date('2020-04-01'), y = max_val),
             shape = 21,
             size  = 9,
             color = '#D2B4DE',
             fill  = '#D2B4DE')+
  geom_point(aes(x = as.Date('2020-04-01'), y = max_val),
             shape = 21,
             size  = 5,
             color = '#8E44AD',
             fill  = '#8E44AD',
             alpha =.5)+
  geom_line(size = 1.7) +
  geom_point(aes(x = as.Date('2018-06-01'), y = june18),
             shape = 21,
             size  = 9,
             color = '#D2B4DE',
             fill  = '#D2B4DE')+
  geom_point(aes(x = as.Date('2018-06-01'), y = june18),
             shape = 21,
             size  = 5,
             color = '#8E44AD',
             fill  = '#8E44AD',
             alpha = .5)+
  geom_line(size = 1.7) +
  scale_y_continuous(breaks = seq(0, 15e5, 5e5),
                     limits = c(0, 17.5e5),
                     labels = function(x) format(x/1000,
                                                big.mark = ".",
                                                decimal.mark = ",",
                                                scientific = FALSE),
                     expand = c(0, 0))+
  annotate("text", 
           x          = as.Date('2019-04-01'), 
           y          = 13.5e5, 
           label      = "Covid \noutbreak",
           family     = "Zilla Slab",
           size       = 6.8, 
           lineheight = .9) +
  annotate("text", 
           x          = as.Date('2017-03-01'), 
           y          = 0.23e6, 
           label      = "Something \nhappened here",
           family     = "Zilla Slab",
           size       = 6.8, 
           lineheight = .9) +
   geom_curve(aes(x    = as.Date('2019-10-01'), 
                  xend = as.Date('2020-04-01'),
                  y    = max_val+1.2e5, 
                  yend = max_val + 0.8e5),
             curvature = -.4,
             size      =1.1,
             color     = '#585d5f',
             arrow     = arrow(type   = 'open',
                               length = unit(0.3, 'cm'),
                               angle  = 45))+
  geom_curve(aes(x    = as.Date('2018-01-01'),
                 xend = as.Date('2018-06-01'),
                 y    = june18-15e4,
                 yend = june18 -11e4),
             curvature = .4,
             size      = 1.1,
             color     = '#585d5f',
             arrow     = arrow(type   = 'open',
                               length = unit(0.3, 'cm'),
                               angle  = 45))+
  labs(x        = NULL, 
       y        = NULL,
       title    = "Counter Strike: Global Offensive",
       subtitle = "Highest number<b>*</b> of players <br>across the months",
       caption  = "<b>*</b>Data shown in thousands <br><b>Source:</b> TidyTuesday")+
  theme(plot.title          = element_text(family = 'Zilla Slab',
                                           size   = 40,
                                           face   = 'bold',
                                           hjust  = .0,
                                           vjust  = .1),
        plot.subtitle       = element_markdown(family = 'Zilla Slab',
                                               size   = 30,
                                               hjust  = .0,
                                               vjust  = .1),
        plot.title.position = "plot",
        plot.caption        = element_markdown(family = 'Zilla Slab',
                                               size   = 20,
                                               face   = 'italic',
                                               margin = margin(t = 20)))


