library(tidyverse)

poll <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-31/poll.csv')
reputation <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-31/reputation.csv')

ggplot(reputation, aes(x = score)) + 
  geom_density(aes(color = name))

reputation |> 
  group_by(industry) |> 
  summarize(n = n_distinct(company)) |> 
  arrange(desc(n))

relative_reputation_by_industry <- reputation |> 
  mutate(industry = fct_lump(industry, 5)) |> 
  group_by(industry, name) |> 
  summarise(mean_score = mean(score), n_company = n(), .groups = "drop") |> 
  group_by(name) |> 
  mutate(global_mean = weighted.mean(mean_score, n_company)) |> 
  mutate(rel_score = mean_score / global_mean)

relative_reputation_by_industry |> 
  ggplot(aes(x = name, y = rel_score, col = industry)) +
  geom_point() + 
  geom_hline(yintercept = 1, linetype = 'dashed') + 
  scale_y_continuous(labels = scales::percent) +
  coord_flip() + 
  labs(y = "relative score", x = "category", title = "Automotive industry is the leader")
