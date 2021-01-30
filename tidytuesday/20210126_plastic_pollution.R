library(tidyverse)
library(countrycode)
library(tidytext)

theme_set(theme_bw())

plastics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-26/plastics.csv') %>% 
  pivot_longer(cols = c(empty:pvc), names_to = 'category') %>% 
  filter(parent_company != 'Grand Total') %>% 
  mutate(parent_company = case_when(
    parent_company %in% c('null', 'NULL', 'Unbranded', '') ~ 'N/A',
    TRUE ~ parent_company
  ))

regional_company_summary <- plastics %>% 
  mutate(
    region = countrycode(country, 'country.name',  'un.regionsub.name', warn = FALSE),
    continent = countrycode(country, 'country.name', 'continent', warn = FALSE)
  ) %>% 
  filter(parent_company != 'N/A' & !is.na(region)) %>% 
  group_by(continent, region, company = parent_company) %>% 
  summarize(value = sum(value, na.rm = TRUE)) %>% 
  filter(value != 0) %>% 
  arrange(desc(value)) %>% 
  group_by(continent, region) %>% 
  mutate(rank = row_number(-value)) %>% 
  filter(rank <= 10) %>% 
  arrange(continent, region, desc(value))
  

regional_company_summary %>% 
  mutate(company = str_replace_all(company, 'Company|Corporation', 'C.')) %>% 
  mutate(company = str_replace_all(company, 'Incorporated', 'Inc.')) %>% 
  mutate(company = str_replace_all(company, 'Federation', 'F.')) %>% 
  arrange(continent, region) %>% 
  mutate(region = as_factor(str_wrap(region, 10))) %>% 
  mutate(company_plot = reorder_within(company, value, region)) %>% 
  ggplot(aes(x = company_plot, y = value)) + 
  geom_col(aes(fill = continent), position = position_dodge(preserve = 'single'), width = 0.8) + 
  facet_wrap(vars(region), scales = 'free_y', ncol = 2, dir = 'v', strip.position = 'right') + 
  coord_flip() + 
  scale_y_log10(labels = scales::label_number_si()) + 
  scale_x_reordered() + 
  scale_fill_viridis_d(drop = FALSE) + 
  theme(
    legend.position = 'none',
    strip.text.y = element_text(angle = 0),
    strip.background = element_rect(size = 6, fill = 'lightgrey', color = 'lightgrey')
  ) + 
  labs(
    x = NULL, y = NULL,
    title = 'Top companies by pieces of collected plastic pollution',
    subtitle = 'Collected, counted and categorized by BFFP volunteers',
    caption = 'Source: Break Free from Plastic\n courtesy of Sarah Sauve, via #tidytuesday'
  )

ggsave(
  './tidytuesday/20210126_plastic_pollution_plot.png',
  width = 16,
  height = 16
)
