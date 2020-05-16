library(readr)
library(dplyr)
library(ggplot2)
library(ggrepel)


### CONSTANTS
DATA_FOLDER <- here::here('data')
FILE <- 'obesity-cleaned.csv'
FIGURES_FOLDER <- here::here('figures')
FIGURE <- 'day_2.png'


### READ
raw <- read_csv(file.path(DATA_FOLDER, FILE)) %>% 
  janitor::clean_names()


### WRANGLING
data <- raw %>% 
  filter(sex == 'Both sexes') %>% 
  mutate(obesity_percent = gsub( " .*$", "", obesity_percent)) %>% 
  mutate(obesity_percent = as.numeric(obesity_percent))

summary <- data %>% 
  filter(country != 'United States of America') %>% 
  group_by(year) %>% 
  summarise(obesity_percent = round(mean(obesity_percent, na.rm = T), 1)) %>% 
  mutate(country = 'Rest of the world') %>% 
  bind_rows(data %>% 
              filter(country == 'United States of America') %>% 
              select(year, obesity_percent, country))


### PLOT
gg <- ggplot(
    data = summary, 
    aes(x = year, y = obesity_percent)
  ) +
  geom_line(
    aes(color = country), 
    size = 1.5
  ) +
  geom_text_repel(
    data = subset(summary, year == 2000),
    aes(label = country), 
    size = 3, 
    segment.color = NA,
    hjust = 1.2,
    vjust = -0.5,
    family = 'Lato'
  ) +
  geom_segment(
    aes(
      x = 1975, 
      xend = 1975,
      y = summary %>% 
        filter(country == 'Rest of the world', year == 1975) %>% 
        .$obesity_percent + 1,
      yend = summary %>% 
        filter(country == 'United States of America', year == 1975) %>% 
        .$obesity_percent - 1
    ),
    arrow = arrow(length = unit(0.2, "cm")),
    color = '#7f7f7f'
  ) +
  geom_text_repel(
    data = subset(summary, year == 1975)[1,],
    label = 'Diff 5.4',
    size = 3, 
    segment.color = NA,
    hjust = -0.1,
    vjust = -1.1,
    family = 'Lato',
    color = '#7f7f7f'
  ) +
  geom_segment(
    aes(
      x = 2016, 
      xend = 2016,
      y = summary %>% 
        filter(country == 'Rest of the world', year == 2016) %>% 
        .$obesity_percent + 1,
      yend = summary %>% 
        filter(country == 'United States of America', year == 2016) %>% 
        .$obesity_percent - 1
    ),
    arrow = arrow(length = unit(0.2, "cm")),
    color = '#7f7f7f'
  ) +
  geom_text_repel(
    data = subset(summary, year == 2016)[1,],
    label = 'Diff 16.3',
    size = 3, 
    segment.color = NA,
    hjust = 1.1,
    vjust = -1.1,
    family = 'Lato',
    color = '#7f7f7f'
  ) +
  theme_minimal() +
  scale_color_manual(
    values=c("#84dcc6", "#ed6a5a")
  ) +
  theme(
    legend.position = 'none',
    legend.margin = margin(t = 0.5, unit = 'cm'),
    text = element_text(size = 14, family = "Lato"),
    panel.grid.minor.y = element_blank(),
    plot.margin = unit(c(1,0.5,0.5,0.5), "cm"),
    axis.title.x = element_text(vjust = -3, size = 11),
    axis.title.y = element_text(angle = 90, vjust = 4, size = 11),
    plot.title = element_text(size = 16, vjust = 6, family = "Share"),
    plot.subtitle = element_text(size = 13, colour = '#7f7f7f', vjust = 6, family = "Share"),
    plot.caption = element_text(family = 'Share')
  ) +
  scale_y_continuous(
    limits = c(0, 40)
  ) +
  labs(
    title = 'Obesity growth over the years',
    subtitle = 'USA vs Rest of the World',
    caption = 'Data: WHO',
    x = 'Year',
    y = 'Obesity percentage'
  )
gg

### SAVE
ggsave(file.path(FIGURES_FOLDER, FIGURE), gg, device = 'png')

