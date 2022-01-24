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
  count(year,playerName,squadName,startingPositionCode) %>% 
  arrange(year,playerName,-n) %>% 
  group_by(year,playerName) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(-n)
  
SNOC_grpd <- SNOC %>% 
group_by(playerName,year,squadName) %>% 
  summarise_at(c(3,6:25),~sum(.x,na.rm = T)) %>% 
  mutate(turnovers = coalesce(turnovers,generalPlayTurnovers)) %>% 
  left_join(SNOC_start_pos)
  

SNOC_grpd %>% 
  filter(startingPositionCode == "GS") %>%
  select(playerName,goalAssists,goalAttempts,goalMisses,goals,minutesPlayed,rebounds,turnovers)

SNOC %>% 
  select(-startingPositionCode) %>% 
  left_join(SNOC_start_pos) %>% 
  filter(startingPositionCode == "GS") %>% 
  group_by(playerName,year) %>% 
  summarise(meanGoals = mean(goals,na.rm = T),
            goals = sum(goals,na.rm = T),
            goalAttempts = sum(goalAttempts,na.rm = T),
            pct = goals/goalAttempts) %>% 
  arrange(-goals)

SNOC_grpd %>% 
  filter(startingPositionCode == "GS") %>%
  ggplot(aes(minutesPlayed,goals)) +
  geom_point() +
  ggrepel::geom_label_repel(data = SNOC_grpd %>% 
               filter(startingPositionCode == "GS") %>% arrange(-goals) %>% head(5),aes(label = playerName,col = factor(year)))


SNOC_grpd %>% 
  filter(startingPositionCode %in% c("GS","GA","WA")) %>% 
  group_by(squadName,year) %>% 
  summarise_at(c("goalAssists","goalAttempts","goalMisses","goals","rebounds","turnovers"),sum) %>% 
  inner_join(
    SNOC_grpd %>% 
      filter(startingPositionCode %in% c("GS","GA","WA")) %>% 
      group_by(squadName,year) %>% 
      summarise(players = paste(playerName,collapse = ", "))
  ) %>% View()
