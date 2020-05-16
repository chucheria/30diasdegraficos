library(readr)
library(dplyr)
library(ggplot2)


### CONSTANTS
DATA_FOLDER <- here::here('data')
FILE <- 'obesity-cleaned.csv'
FIGURES_FOLDER <- here::here('figures')
FIGURE <- 'day_1.png'


### READ
raw <- read_csv(file.path(DATA_FOLDER, FILE))


### WRANGLING
data <- raw %>% 
  janitor::clean_names() %>% 
  filter(sex != 'Both sexes', country == 'Spain') %>% 
  mutate(obesity_percent = gsub( " .*$", "", obesity_percent)) %>% 
  mutate(obesity_percent = as.numeric(obesity_percent))


### PLOT
gg <- ggplot(data, aes(x = year, y = obesity_percent)) +
  geom_bar(stat = 'identity', aes(fill = sex), position='dodge', width=0.6) + 
  theme_minimal() +
  theme(legend.position = 'bottom',
        legend.title = element_blank(),
        legend.margin = margin(t = 0.5, unit = 'cm'),
        text=element_text(size = 14, family = "Share"),
        panel.grid.minor.y = element_blank(),
        plot.margin = unit(c(1,0.5,0.5,0.5), "cm"),
        axis.title.x=element_text(vjust = -3, size = 11),
        axis.title.y=element_text(angle = 90, vjust = 4, size = 11),
        plot.title=element_text(size = 16, vjust = 6)) +
  labs(title = 'Evolution of obesity in Spain', 
       caption = 'Data: WHO',
       x = 'Year',
       y = 'Obesity percentage') +
  scale_fill_manual(values=c("#ffa69e", "#5e6472"))


### SAVE
ggsave(file.path(FIGURES_FOLDER, FIGURE), gg, device = 'png')
