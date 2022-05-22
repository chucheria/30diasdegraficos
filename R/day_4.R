library(readr)
library(dplyr)
library(ggplot2)
library(purrr)

options(scipen=999)


### CONSTANTS
DATA_FOLDER <- here::here('data')
FILE <- 'SO_survey_results_public_2019.csv'
FIGURES_FOLDER <- here::here('figures')
FIGURE <- 'day_4.png'


### READ
raw <- read_csv(file.path(DATA_FOLDER, FILE)) %>% 
  janitor::clean_names()

employment <- c('Employed full-time')
hobbyist <- c('No')
quant <- quantile(raw$comp_total, na.rm = T)
not_dev_types <- c('Student')

### WRANGLING
# Get only most represented dev types
data <- raw %>% 
  filter(employment %in% employment, age < 70, age > 18, 
         hobbyist %in% hobbyist) %>%
  tidyr::drop_na(gender, comp_total, age, dev_type) %>% 
  filter(comp_total < quant[4], comp_total > quant[2]) %>% 
  select(age, gender, dev_type, comp_total) %>% 
  tidyr::separate_rows(gender, sep = ";") %>% 
  tidyr::separate_rows(dev_type, sep = ";") %>% 
  filter(!(dev_type %in% not_dev_types)) %>% 
  mutate(dev_type = factor(dev_type)) %>% 
  filter(dev_type %in% forcats::fct_count(data$dev_type, sort = T)[1:10, 1])
  
  
data %>% group_by(gender, age, dev_type) %>% 
  summarise(median_compensation = mean(comp_total), n = n()) %>% 
  ungroup() %>% 
  mutate(gender = if_else(
    gender == "Non-binary, genderqueer, or gender non-conforming",
    "NB - Q - NC", gender))

medias <- data %>% 
  group_by(gender, dev_type) %>% 
  summarise(mean = mean(median_compensation))


### PLOT
gg <- ggplot(data, aes(y = median_compensation, x = age)) +
  geom_line() +
  geom_hline(data = medias, aes(yintercept = mean)) +
  theme_minimal() +
  facet_grid(dev_type ~ gender) +
  theme(
    strip.text.y = element_text(angle = 0),
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
    caption = 'Data: Stack Overflow Developers Survey',
    x = 'Age',
    y = '$ Annual Salary'
  ) +
  scale_y_continuous(
    labels = scales::comma
  )

gg

### SAVE
ggsave(file.path(FIGURES_FOLDER, FIGURE), gg, device = 'png')


