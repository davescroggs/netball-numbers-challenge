
library(tidyverse)
library(lubridate)
library(rtweet)
get_token()
options(scipen = 999)

get_tweets <- get_timelines(c("MelbourneVixens","GIANTS_Netball","collingwoodsn"),n = 250)

tweet_lookup <- get_tweets %>% 
  mutate(hashtags = map_chr(hashtags,~unlist(.x) %>% paste(collapse = "-")),
         created_at = with_tz(created_at,tz = "Australia/Melbourne")) %>% 
  filter(is.na(reply_to_user_id),!is_quote,!is_retweet,created_at > "2021-09-12" & created_at < "2021-09-25",str_detect(hashtags,"WitnessFearless|SSNSignings|StandUnited") | status_id == 1439001071976796160) %>%
  mutate(across(c(favourites_count, retweet_count, quote_count, reply_count),~replace_na(.x, replace = 0))) %>% 
  transmute(
    id = status_id,
    date = created_at,
    username = paste("@", screen_name, sep = ""),
    content = text,
    interactions = favorite_count + retweet_count + quote_count + reply_count
  ) 

team_accounts = c("@NSWSwifts","@AdelaideTBirds","@sc_lightning","@FirebirdsQld","@WestCoastFever","@collingwoodsn","@GIANTS_Netball","@MelbourneVixens")

tweets <- read_csv("datasets/vol3/somethingAboutRiddlesAndFruit.csv") %>% 
  filter(username %in% (str_sub(team_accounts,2))) %>%
  mutate(date = case_when(
    username == "AdelaideTBirds" ~ with_tz(date,tz = "Australia/Adelaide"),
    username == "WestCoastFever" ~ with_tz(date,tz = "Australia/Perth"),
    TRUE ~ with_tz(date,tz = "Australia/Sydney"))) %>% 
  mutate(interactions = likeCount + retweetCount + replyCount + quoteCount,
         username = paste("@",username,sep = ""),
         id = as.character(id),
         username = factor(username, levels = team_accounts,ordered = T)) %>% 
  select(id,date,username,content,interactions) %>% 
  bind_rows(tweet_lookup) %>% 
  mutate(plot_loc = dense_rank(username),
         date = date + minutes(30))

annotations_1 <- tweets %>% 
  filter(id %in% c(1438424877107613696,1440496208288436224,1439001071976796160)) %>% 
  mutate(x = date + days(5),
         xend = date + hours(3),
         y = plot_loc + 0.5,
         yend = plot_loc + 0.15,
         text = c(" Harry Styles signs on",
                  " Wholesome Kim Rav tweet",
                  " One and done - Magpie's full\n team announcement"))

annotations_2 <- tweets %>% 
  filter(id %in% c(1445644860988411904)) %>% 
  mutate(x = date - days(3),
         xend = date - hours(12),
         y = plot_loc + 0.9,
         yend = plot_loc + 0.1,
         text = c("Jemma Mi Mi signing "))


tweets %>% 
  arrange(username) %>% 
  ggplot(aes(x = date,y = username, label = content)) +
  geom_point(aes(size = interactions)) +
  geom_curve(
    aes(x = x, y = y, xend = xend, yend = yend),
    data = annotations_1,
    arrow = arrow(length = unit(0.01, "npc"))) + 
  geom_text(data = annotations_1,
            aes(x = x,y = y,label = text),hjust = 0,vjust = 0.75,
            family = "Courier") +
  geom_segment(
    aes(x = x, y = y, xend = xend, yend = yend),
    data = annotations_2,
    arrow = arrow(length = unit(0.01, "npc"))) +
  geom_text(data = annotations_2,
            aes(x = x,y = y,label = text),hjust = 1, family = "Courier") +
  labs(title = "Timing of SSN club signing announcements on twitter",
       subtitle = "Size of eat dot represents the number of interactions (likes + retweents + replies) for each post.\nBigger dot = more interations",
    x = "Date",
       y = "Team Twitter Account",
       size = "Number of\ninteractions",
    caption = "Data via @aaronfox and twitter") +
  theme(panel.background = element_rect(fill = "#FFE7BA",colour = "black"),
        plot.title = element_text(hjust = 0.5),
        axis.text.y = element_text(size = 11),
        axis.text.x = element_text(size = 11),
        plot.subtitle = element_text(hjust = 0.5))

tweets %>% 
  mutate(dow = wday(date,label = TRUE,week_start = 1)) %>%
  count(dow,username) %>% 
  ggplot(aes(x = dow,y = username,fill = n)) +
  geom_tile() +
  geom_text(aes(label = n),colour = "white", family = "Courier") +
  labs(title = "Day of the week announcements were made",
       x = "Day",
       y = "Team Twitter Account",
       fill = "Number of\ntweets",
       caption = "Data via @aaronfox and twitter") +
  theme(panel.background = element_rect(fill = "#FFE7BA"),
        plot.title = element_text(hjust = 0.5),
        axis.text.y = element_text(size = 11),
        axis.text.x = element_text(size = 11),
        panel.grid.major = element_line(colour = "black"))

tweets %>% 
  mutate(tod = hour(date)) %>% 
  count(tod,username) %>% 
  ggplot(aes(x = tod,y = username,fill = n)) +
  geom_tile() + 
  geom_text(aes(label = n),colour = "white", family = "Courier") +
  theme_minimal() +
  scale_x_continuous(breaks = 8:24) +
  scale_fill_continuous(breaks = 1:8) +
  labs(title = "Time of day announcements were made",
       x = "Time (24 hr)",
       y = "Team Twitter Account",
       fill = "Number of\ntweets",
       caption = "Data via @aaronfox and twitter") +
  theme(panel.background = element_rect(fill = "#FFE7BA"),
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_line(colour = "black"))




