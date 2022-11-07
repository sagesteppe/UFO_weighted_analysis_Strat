library(tidyverse)

setwd('/media/sagesteppe/ExternalHD/UFO_Weights_for_stratification')
p2d <- 'data/raw'
list.files(path = p2d)

plot_status <- read.csv(file.path(p2d, list.files(path = p2d, pattern = "summary.csv")))  %>% 
  mutate(across(.cols = everything(), ~ str_to_lower(.))) %>% 
  
  # humans are humans and typos are an expression of humanity. also  add a 
  # factor levels which we arrange by and slice the top from . 
  mutate(
    Plot.Status = str_trim(Plot.Status, side = "both"),
    Plot.Status = str_replace(Plot.Status, 'rejected', 'not_sampled'),
    Plot.Status = str_replace(Plot.Status, " ", "_"),
    psF = factor(Plot.Status, levels = c('sampled', 'not_sampled'))
    ) %>%  
  
  # some numbers are missing leading zereos
  separate(Plot.ID, into = c('stratum', 'id'), sep = '-') %>% 
  mutate(id = str_pad(id, 3, pad = "0")) %>% 
  
  # group up - and take that 'sampled' from the top - if present
  unite('Plot.ID', stratum:id, sep = "-", remove = F) %>% 
  group_by(Plot.ID) %>% 
  arrange(psF) 

# this is 
status_check <- plot_status %>% # we want to ensure all of these plots are returned as sampled. 
  mutate(grp_n = n()) %>% 
  filter(grp_n >= 2, Plot.Status == 'sampled')

plot_status <- plot_status %>% 
  slice_head(n = 1) %>% 
  ungroup()

inner_join(plot_status, status_check, by = 'Plot.ID') # take a look for yourself, all good. 

plot_summary <- plot_status %>% 
  group_by(stratum, Plot.Status) %>% 
  summarise(n = n())  %>% 
  ungroup(Plot.Status) %>% 
  mutate(total = sum(n)) %>% 
  pivot_longer(n:total, values_to = 'Number') %>% 
  mutate(Plot.Status = if_else(name == 'total', 'total', Plot.Status)) %>% 
  select(-name) %>% 
  group_by(stratum, Plot.Status) %>% 
  sample_n(1)

rm(plot_status, status_check)

# format table for printing
plot_summary <- plot_summary %>% 
  pivot_wider(values_from = Number, names_from = Plot.Status) %>% 
  mutate(stratum = str_to_upper(stratum)) %>% 
  mutate(across(.cols = everything(), ~ replace_na(.x, 0))) %>% 
  rename_with(., ~ str_to_sentence(.))

# DERIVE THE VALUES FOR BOTH TOTAL AREA AND AREAS IN ACRES

r <- read.csv(file.path(p2d, 'UFO_strata_areas.csv')) %>% 
  mutate(ProportionalArea = Cells/sum(Cells)) %>% 
  select(Stratum, Code, ProportionalArea, Acres)

deSS <- data.frame( # desired sample size per stratum over live of sample design
  'Stratum' = c("AS", "GR", "MC", "MMS", "OT" , "PJ", "PP", "RI", "SD" , "SS" ), 
  'DesiredSS' = c(5, 5, 15, 25, 5, 25, 5, 15, 75, 80),
  'OrigWGT' = c(12932, 17867, 5113, 11934, 16685, 70970, 14716, 15227, 7911, 13376)
)

OriginalWeights <- left_join(plot_summary, r, by = c('Stratum' = 'Code')) %>% 
  left_join(., deSS, by = 'Stratum') %>% 
  select(StratumName = Stratum.y, Stratum, Total,  NotSampled = Not_sampled, Sampled, 
          DesiredSS, PropArea = ProportionalArea, Acres)

write.csv(OriginalWeights, './data/processed/OriginalWeightsSampleDesign.csv', row.names = F)

rm(r, plot_summary, deSS)


$$ n/N $$

library(spsurvey)
adjwgt()

wgtcat
wgt <- runif(50)
wgtcat <- rep(c("A", "B"), c(30, 20))
framesize <- c(A = 15, B = 10)
sites <- rep(rep(c(TRUE, FALSE), c(9, 1)), 5)

adjwgt(wgt, wgtcat, framesize, sites)


# Weights under plots re-classified into realized stratum

pps <- '/media/sagesteppe/ExternalHD/plot_post_stratification/data'

classified_Vplots <- read.csv(
  file.path(pps, 'processed/UFO_Veg_monitoring_CLASSIFIED.csv')) %>% 
  drop_na() %>% 
  sf::st_as_sf(coords = c(x = 'Longitude',  y = 'Latitude' ), crs = 4326)%>% 
  sf::st_transform(26912) %>% 
  sf::st_buffer(55)

plot_status <- read.csv(file.path(p2d, list.files(path = p2d, pattern = "summary.csv")))[307:409,] 
classified_Vplots2022 <- read.csv( # but this includes rejected plots... 
  file.path(pps, 'processed', 'UFO_2022_AIM_CLASSIFIED.csv')) 

pps <- '/media/sagesteppe/ExternalHD/aimDB/data/raw/AIM_Sample_Design'
a <- sf::st_read(file.path(pps, 'AIM_Design_Stratification.shp'), quiet = T) %>% 
  sf::st_transform(26912) %>% 
  sf::st_buffer(55)

ggplot() +
  geom_sf(data = a, aes(color = STRATUM))

d <- sf::st_join(classified_Vplots, a, left = F)

