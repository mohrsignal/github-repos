# Workflow

# Load libraries and source functions
library(tidyverse)
source('~/R/github/github-repos/R/read.R')
source('~/R/github/github-repos/R/wrangle.R')
source('~/R/github/github-repos/R/visualize.R')

# con <- create_connection()
# 
# r_repos <- get_language_repos(con, "R")
#
# r_repos %>% saveRDS("data/r_repos.RDS)
r_repos <- readRDS("data/r_repos.RDS")

r_languages_count <-
  r_repos %>% 
  unnest() %>% 
  count(name)

r_cooccurence_hist <-
  r_languages_count %>% 
  filter(name != "R") %>% 
  ggplot() +
  geom_histogram(aes(x = n), binwidth = 100,
                 fill = rgb(136, 189, 230, max = 255)) + 
  scale_x_continuous(expand = c(0, 0),
                     breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "co-occurences with R",
       y = "repos",
       title = "Distribution of language co-occurences with R",
       subtitle = "binwidth = 100 repos")  +
  theme_minimal() +
  theme(panel.background = element_blank(),
        axis.line = element_line(color = rgb(140, 140, 140, max = 255)),
        strip.text = element_blank(),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 11, angle = 0),
        axis.text = element_text(size = 11),
        axis.ticks = element_blank()) +
  coord_cartesian(xlim = c(0, 11500))

r_cooccurence_hist2 <-
  r_coccurence_hist +
  geom_vline(xintercept = 800,
             color = "black",
             linetype = "dashed") +
  coord_cartesian(xlim = c(0, 11500), ylim = c(0, 35)) +
  labs(title = "Zoom: distribution of language co-occurences with R",
       subtitle = "binwidth = 100 repos; y-axis truncated at 35 counts")

r_cooccurence_dotplot <-
  r_languages_count %>% 
  filter(name != "R") %>% 
  filter(n > 800) %>%
  dotplot(x = name, y = n,
          title = "Top languages co-occurring with R",
          xlab = "language",
          ylab = "co-occurrence with R / occurrence in full dataset")

r_repo_count <- r_repos %>% nrow()

r_freq_dotplot2 <-
  r_languages_count %>% 
  filter(name != "R") %>% 
  filter(n > 800) %>%
  dotplot(x = name, y = n / r_repo_count,
          title = "Frequency of top languages co-occurring with R",
          xlab = "language",
          ylab = "co-occurrence with R / total R repos")

language_count <- readRDS("data/languages_count.RDS")

r_coocur <- readRDS("data/r_cooccurences.RDS")

r_freq_count <-
  r_languages_count %>%   
  filter(name != "R") %>% 
  filter(n > 800) %>% 
  rename(r_count = n) %>% 
  left_join(language_count,
            by = "name") %>% 
  mutate(freq = r_count/n)

r_freq_dotplot <-
  r_freq_count %>% 
  dotplot(x = name, y = freq,
          title = "Proportion of R co-occurences by language",
          xlab = "language",
          ylab = "co-occurrence with R / occurrence in full dataset")

r_freq_dotplot_no_rebol <-
  r_freq_count %>% 
  filter(name != "Rebol") %>% 
  dotplot(x = name, y = freq,
          title = "Proportion of R co-occurences by language",
          xlab = "language",
          ylab = "co-occurrence with R / occurrence in full dataset")