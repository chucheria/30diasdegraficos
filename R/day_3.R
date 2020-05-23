library(readr)
library(dplyr)
library(ggplot2)

options(scipen=999)


### CONSTANTS
DATA_FOLDER <- here::here('data')
FILE <- 'SO_survey_results_public.csv'
FIGURES_FOLDER <- here::here('figures')
FIGURE <- 'day_3.png'


### READ
raw <- read_csv(file.path(DATA_FOLDER, FILE)) %>% 
  janitor::clean_names()

genders <- c('Woman', 'Man', 'Non-binary, genderqueer, or gender non-conforming')
employment <- c('Employed full-time')
quant <- quantile(raw$comp_total, na.rm = T)

set.seed(42)
### WRANGLING
data <- raw %>% 
  filter(employment %in% employment, age < 70, age > 18) %>% 
  tidyr::drop_na(gender, comp_total, age) %>% 
  filter(comp_total < quant[4], comp_total > quant[2]) %>% 
  mutate(minority = ifelse(grepl('^Woman', gender), T, F)) %>% 
  select(minority, comp_total, age) %>% 
  group_by(minority, age) %>% 
  sample_n(100, replace = TRUE) %>%
  ungroup()


### PLOT
gg <- ggplot(data, aes(y = comp_total, x = age)) +
  geom_point(
    alpha = 0.5, 
    aes(color = minority)
  ) +
  theme_minimal() +
  theme(
    legend.position = 'bottom',
    legend.margin = margin(t = 0.3, unit = 'cm'),
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    text = element_text(size = 14, family = "Lato"),
    panel.grid.minor.y = element_blank(),
    plot.margin = unit(c(1,0.5,0.5,0.5), "cm"),
    axis.title.x = element_text(vjust = -3, size = 11),
    axis.title.y = element_text(angle = 90, vjust = 4, size = 11),
    plot.title = element_text(size = 18, vjust = 6, family = "Share"),
    plot.subtitle = element_text(size = 14, colour = '#7f7f7f', vjust = 6, 
                                 family = "Share"),
    plot.caption = element_text(family = 'Share')
  ) +
  labs(
    title = 'Developers: salary vs age',
    subtitle = 'Sample of 100 of each population',
    caption = 'Data: Stack Overflow Developers Survey',
    x = 'Age',
    y = '$ Annual Salary'
  ) +
  scale_y_continuous(
    labels = scales::comma
  ) +
  scale_color_manual(labels = c("Men", "Minority gender"), values = c("#5e6472", "#ffa69e"))

### SAVE
ggsave(file.path(FIGURES_FOLDER, FIGURE), gg, device = 'png')


  