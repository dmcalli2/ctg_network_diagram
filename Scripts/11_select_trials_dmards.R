dmard <- readRDS(file = "Scratch_data/dmards.Rds")
load(file = "Scratch_data/data_for_network.Rdata")

dmard_unk <- dmard %>% 
  filter(title %in% c("Selective immunosuppressants",
                      "Other immunosuppressants",
                      "Interleukin inhibitors")) %>% 
  distinct(atc_7, name)

write_csv(dmard_unk, "Scratch_data/review_mechanisms_dmards.csv")

# Looked-up RXNORM via RXNAV and got MOA and disease uses from class views 9th Jan 2018
# All drugs had an MOA. MoAs come from NDF-RT database NDFRT (National Drug File - Reference Terminology)
# All were found in RXNORM
dmard_moa <- read_csv("Data/reviewED_mechanisms_dmards.csv")
dmard_moa <- dmard_moa %>% 
  select(name, mechanism, note)
dmard <- dmard %>% 
  left_join(dmard_moa, by = "name")

write_csv(dmard, path = "scratch_data/review_discuss_rheumatologist.csv")
# Reviewed with Dr Lucy McGeogh, consultant rheumatologist.
# Agreed with NDF-RT classifications for DMARDS in list of repository drugs# Shoudl ask to review other drugs also