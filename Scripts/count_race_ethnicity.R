# Count race and ethnicity categories
library(tidyverse)
library(stringr)

all_tbls <- readRDS("../Trial_identify/clinical_trials_august_2017/Output_data/all_aact_tables.Rds")
race_ethnic <- rapply(all_tbls, function(x) any(str_detect(x, "^race|Race")))
race_ethnic[race_ethnic & !is.na(race_ethnic)]

race_ethnic <- all_tbls$baseline_measurements %>% 
  filter(str_detect(title, "^Race|race")) %>% 
  group_by(nct_id, classification) %>% 
  summarise(n = sum(param_value_num)) %>% 
  ungroup()

race_ethnic_slct <- race_ethnic %>% 
  group_by(nct_id) %>% 
  mutate(proportion = n/sum(n)) %>% 
  filter(proportion >= 0)

race_ethnic_count <- race_ethnic_slct %>% 
  group_by(classification) %>% 
  count() %>% 
  arrange(classification) %>% 
  ungroup() %>% 
  mutate(x = 1:nrow(.))

race_ethnic_count <- race_ethnic_count %>% 
  mutate(classification_lc = classification %>% str_to_lower()) %>% 
  mutate(white = str_detect(classification_lc, "caucasian|white|europe"),
         black_or_african_american = str_detect(classification_lc, "black|african amer"),
         asian = str_detect(classification_lc, "asian"),
         hispanic_y_n = str_detect(classification_lc, "hispan|latin"),
         american_indian_or_alaska_native = str_detect(classification_lc, "native|india") &
           str_detect(classification_lc, "america|alaska"),
         native_hawaiian_or_other_pacific_islanders = str_detect(classification_lc, "pacific|native hawai"))

race_ethnic_count_other <- race_ethnic_count %>% 
  filter(!white, !black_or_african_american, !asian, !hispanic_y_n, !american_indian_or_alaska_native, !native_hawaiian_or_other_pacific_islanders)


# write_csv(race_ethnic_count_other, "Scratch_data/race_ethnicity_counts.csv")
race_ethnic_count_other <- read_csv("../Trial_identify/clinical_trials_august_2017/Data/reviewED_race_ethnicity_counts.csv")

race_ethnic_final <- race_ethnic_count %>% 
  anti_join(race_ethnic_count_other) %>% 
  select(classification, white:native_hawaiian_or_other_pacific_islanders) %>% 
  gather(key = "assigned_category", value = "tf", -classification) %>% 
  filter(tf) %>% 
  select(-tf)

race_ethnic_final <- bind_rows(race_ethnic_final, race_ethnic_count_other) %>% 
  distinct()

race_ethnic_classified <- race_ethnic %>% 
  inner_join(race_ethnic_final) %>% 
  filter(!assigned_category %in% c("hispanic_y_n", "Ethnicity"))

race_ethnic_classified_smry <- race_ethnic_classified %>% 
  mutate(assigned_category = if_else(assigned_category %in% c("Unknown","NoRollUp", "Other"),
                                     "Unknown, unable to collapse or other",
                                     assigned_category)) %>% 
  group_by(nct_id, assigned_category) %>% 
  summarise(n = sum(n)) %>% 
  ungroup() %>% 
  group_by(nct_id) %>% 
  mutate(prop = round(100*n/sum(n))) %>% 
  ungroup() %>% 
  mutate(race_category = str_replace_all(assigned_category, "_", " ") %>% 
           tools::toTitleCase())

all_trials <- (sum(!duplicated(all_tbls$studies$nct_id)))
trials_with_race_results <- (sum(!duplicated(race_ethnic_classified_smry$nct_id)))

plot1 <- ggplot(race_ethnic_classified_smry,
                aes(x = race_category, y = prop)) +
  geom_boxplot() +
  scale_x_discrete(paste("OMB race categories in",trials_with_race_results, " of ", all_trials, " trials")) +
  scale_y_continuous("Percentage of participants") +
  coord_flip()
tiff("Figures/boxplot_race_categories.tiff", width = 7, height =5, unit = "in", res = 300, compression = "lzw")
plot1
dev.off()
