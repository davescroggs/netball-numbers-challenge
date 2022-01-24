library(tidyverse)

SNOC <- superNetballersOfChristmas <- read_csv(
  "datasets/vol5/superNetballersOfChristmas.csv",
  col_types = cols(
    feedWithAttempt = col_number(),
    deflectionWithGain = col_number(),
    deflectionWithNoGain = col_number(),
    deflections = col_number(),
    turnovers = col_number(),
    generalPlayTurnovers = col_number(),
    netPoints = col_number()))

SNOC_start_pos <- SNOC %>% 
  filter(startingPositionCode != "-") %>%
  mutate(gen_pos = case_when(
    startingPositionCode %in% c("GA","GS") ~ "Goals",
    startingPositionCode %in% c("WA","C") ~ "Mids",
    TRUE ~ "Other"
  )) %>% 
  count(year,playerName,squadName,gen_pos) %>% 
  arrange(year,playerName,-n) %>% 
  group_by(year,playerName) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(-n)


# Goalers -----------------------------------------------------------------

# 2018 Fever 901 J.Fowler, N.Medhurst 551 405
# 2nd most goals by goalers in SSN
# Pre-supershot


SNOC_goals <- SNOC %>% 
  left_join(SNOC_start_pos) %>% 
  filter(gen_pos == "Goals") %>% 
    group_by(year,squadName) %>% 
  summarise_at(c(4,7:26),~sum(.x,na.rm = T)) %>%
  inner_join(SNOC %>% 
              left_join(SNOC_start_pos) %>%
               filter(gen_pos == "Goals") %>% 
              group_by(year,squadName) %>% 
              summarise(players = unique(playerName) %>% paste(sep = "",collapse = ", "))) %>% 
  mutate(turnovers = if_else(turnovers == 0,generalPlayTurnovers,turnovers),
         goalpct = goals/goalAttempts)

{SNOC_goals %>% 
    mutate(squad = paste(year,players,sep = " - ")) %>% 
  ggplot(aes(goals,feeds,group = squad)) +
  geom_point()} %>% 
  plotly::ggplotly()

SNOC %>% 
  filter(str_detect(playerName,"J.Fowler|Medhur")) %>% 
  group_by(year) %>% 
  summarise(goals = sum(goals)) %>% 
  arrange(goals)


# Attacking Mids --------------------------------------------------------------------


SNOC_mids <- SNOC %>% 
  left_join(SNOC_start_pos) %>% 
  filter(gen_pos == "Mids") %>% 
  group_by(year,squadName) %>% 
  summarise_at(c(4,7:26),~sum(.x,na.rm = T)) %>%
  inner_join(SNOC %>% 
               left_join(SNOC_start_pos) %>%
               filter(gen_pos == "Mids") %>% 
               group_by(year,squadName) %>% 
               summarise(players = unique(playerName) %>% paste(sep = "",collapse = ", "))) %>% 
  mutate(turnovers = if_else(turnovers == 0,generalPlayTurnovers,turnovers),
         deflections = if_else(deflections == 0, deflectionWithGain + deflectionWithNoGain,deflections)) %>% 
  ungroup()

# gains, feeds, turnovers, deflections, intercepts, penalties

SNOC_mids %>%
  select(year,squadName,players,gain,feeds,turnovers,deflections,intercepts,penalties) %>% 
  pivot_longer(cols = gain:penalties,names_to = "metric",values_to = "value") %>% 
  group_by(year,metric) %>% 
  mutate(mean = mean(value),
         difference = value - mean) %>% 
  pivot_wider(names_from = metric,values_from = c(value,mean,difference)) %>% 
  select(year,squadName,players,contains("differ")) %>% View()
  

SNOC %>% 
  ungroup() %>% 
  count(year, wt = goals)

SNOC %>% 
  filter(year == 2020) %>% 
  count(round)
