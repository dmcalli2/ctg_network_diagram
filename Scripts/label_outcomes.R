library(tidytext)
library(tidyverse)
outcome_analyses <- readRDS("Scratch_data/outcome_analyses_diabets_trials.Rds")
designs <- readRDS("Scratch_data/designs_diabets_trials.Rds")
outcomes <- readRDS("Scratch_data/outcomes_diabets_trials.Rds")
design_outcomes <- readRDS("Scratch_data/design_outcomes_diabets_trials.Rds")


text <- design_outcomes$measure %>%  unique()

gram_i <- map(2:8, ~
                design_outcomes %>% 
                distinct(measure) %>% 
                mutate(indx = seq_along(measure)) %>% 
                group_by(measure) %>% 
                unnest_tokens(mygram, input = measure, token = "ngrams", n = .x) %>% 
                count(mygram, sort = TRUE) %>% 
                mutate(m = n/sum(n)) %>% 
                ungroup())

gram_i <- bind_rows(gram_i, .id = "ngrams")
gram_i <- gram_i %>% 
  mutate(ngrams = parse_number(ngrams) + 1) %>% 
  arrange(desc(n)) %>% 
  filter(n > 10)
