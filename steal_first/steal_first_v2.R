library(tidyverse)

## setwd("R_Baseball")

parse_retrosheet_pbp(2015)
parse_retrosheet_pbp(2016)
parse_retrosheet_pbp(2017)
parse_retrosheet_pbp(2018)

fields <- read_csv("master/data/fields.csv")
all2015 <- read_csv("download.folder/unzipped/all2015.csv",
                    col_names = pull(fields, Header))
all2016 <- read_csv("download.folder/unzipped/all2016.csv",
                    col_names = pull(fields, Header))
all2017 <- read_csv("download.folder/unzipped/all2017.csv",
                    col_names = pull(fields, Header))
all2018 <- read_csv("download.folder/unzipped/all2018.csv",
                    col_names = pull(fields, Header))

## Calculating Safe Rate --------------------------------------------------

all_15_18 <- rbind(all2015, all2016, all2017, all2018) %>%
  filter(BAT_EVENT_FL == TRUE) %>% mutate(year = substr(GAME_ID, 4, 7))

strikeout <- all_15_18 %>% filter(EVENT_CD == 3)

strikeout$BAT_PLAY_TX[strikeout$BAT_PLAY_TX == 2000] <- NA

reached <- strikeout %>% filter(BAT_DEST_ID != 0)
thrown <- strikeout %>% filter(BAT_PLAY_TX != 'NA' & BAT_PLAY_TX != 2)

reach_rate <- reached %>% group_by(year) %>% summarize(reach = n())
out_rate <- thrown %>% group_by(year) %>% summarize(outs = n())

safe_rate <- left_join(reach_rate, out_rate, by = 'year') %>%
  mutate(reach_rate = reach / (reach + outs))

steals <- read_csv("steal_first/steals.csv", col_names = TRUE)
steals <- steals %>% mutate(SB_rate = SB / (SB + CS))

safe_rate
steals

## Importing Statcast Pitch Data ------------------------------------------

two_outs15 <- read_csv("steal_first/k_2outs_15.csv", col_names = TRUE)
two_outs16 <- read_csv("steal_first/k_2outs_16.csv", col_names = TRUE)
two_outs17 <- read_csv("steal_first/k_2outs_17.csv", col_names = TRUE)
two_outs18 <- read_csv("steal_first/k_2outs_18.csv", col_names = TRUE)

other_outs15 <- read_csv("steal_first/k_other_15.csv", col_names = TRUE)
other_outs16 <- read_csv("steal_first/k_other_16.csv", col_names = TRUE)
other_outs17 <- read_csv("steal_first/k_other_17.csv", col_names = TRUE)
other_outs18 <- read_csv("steal_first/k_other_18.csv", col_names = TRUE)

blocked15 <- rbind(two_outs15, other_outs15)
blocked16 <- rbind(two_outs16, other_outs16)
blocked17 <- rbind(two_outs17, other_outs17)
blocked18 <- rbind(two_outs18, other_outs18)

blocked <- rbind(blocked15, blocked16, blocked17, blocked18) %>%
  mutate(year = substr(game_date, 1, 4), des_len = nchar(des))

called_2outs_15 <- read_csv("steal_first/called_2outs_15.csv", col_names = TRUE)
called_2outs_16 <- read_csv("steal_first/called_2outs_16.csv", col_names = TRUE)
called_2outs_17 <- read_csv("steal_first/called_2outs_17.csv", col_names = TRUE)
called_2outs_18 <- read_csv("steal_first/called_2outs_18.csv", col_names = TRUE)

called_other_15 <- read_csv("steal_first/called_other_15.csv", col_names = TRUE)
called_other_16 <- read_csv("steal_first/called_other_16.csv", col_names = TRUE)
called_other_17 <- read_csv("steal_first/called_other_17.csv", col_names = TRUE)
called_other_18 <- read_csv("steal_first/called_other_18.csv", col_names = TRUE)

called15 <- rbind(called_2outs_15, called_other_15)
called16 <- rbind(called_2outs_16, called_other_16)
called17 <- rbind(called_2outs_17, called_other_17)
called18 <- rbind(called_2outs_18, called_other_18)

called <- rbind(called15, called16, called17, called18) %>%
  mutate(year = substr(game_date, 1, 4), des_len = nchar(des))

swing_2outs_15 <- read_csv("steal_first/swing_2outs_15.csv", col_names = TRUE)
swing_2outs_16 <- read_csv("steal_first/swing_2outs_16.csv", col_names = TRUE)
swing_2outs_17 <- read_csv("steal_first/swing_2outs_17.csv", col_names = TRUE)
swing_2outs_18 <- read_csv("steal_first/swing_2outs_18.csv", col_names = TRUE)

swing_other_15 <- read_csv("steal_first/swing_other_15.csv", col_names = TRUE)
swing_other_16 <- read_csv("steal_first/swing_other_16.csv", col_names = TRUE)
swing_other_17 <- read_csv("steal_first/swing_other_17.csv", col_names = TRUE)
swing_other_18 <- read_csv("steal_first/swing_other_18.csv", col_names = TRUE)

swing15 <- rbind(swing_2outs_15, swing_other_15)
swing16 <- rbind(swing_2outs_16, swing_other_16)
swing17 <- rbind(swing_2outs_17, swing_other_17)
swing18 <- rbind(swing_2outs_18, swing_other_18)

swing <- rbind(swing15, swing16, swing17, swing18) %>%
  mutate(year = substr(game_date, 1, 4), des_len = nchar(des))

## Calculating Run Rate ---------------------------------------------------

called_ran <- called %>% filter(des_len > 80)
swing_ran <- swing %>% filter(des_len > 80)

blocked <- rbind(blocked, called_ran, swing_ran)
ran_first <- rbind(reached, thrown)

run_rate <- blocked %>% group_by(year) %>% summarize(opportunities = n())
ran_rate <- ran_first %>% group_by(year) %>% summarize(ran_base = n())

run_rate <- left_join(run_rate, ran_rate, by = 'year') %>%
  mutate(run_rate = ran_base / opportunities)

run_rate <- left_join(run_rate, reach_rate, by = 'year') %>%
  mutate(reach_rate = reach / opportunities)

run_rate

## Importing Statcast Sprint Speed ----------------------------------------

sprint_15 <- read_csv("steal_first/sprint_speed_15.csv", col_names = TRUE)
sprint_16 <- read_csv("steal_first/sprint_speed_16.csv", col_names = TRUE)
sprint_17 <- read_csv("steal_first/sprint_speed_17.csv", col_names = TRUE)
sprint_18 <- read_csv("steal_first/sprint_speed_18.csv", col_names = TRUE)

sprint_15 <- sprint_15 %>% mutate(year = '2015')
sprint_16 <- sprint_16 %>% mutate(year = '2016')
sprint_17 <- sprint_17 %>% mutate(year = '2017')
sprint_18 <- sprint_18 %>% mutate(year = '2018')

sprint <- rbind(sprint_15, sprint_16, sprint_17, sprint_18)

ID_MAP <- read_csv("ID_master.csv", col_names = TRUE)
mlb_retro <- ID_MAP %>% select(mlb_id, retro_id)

## Joining tables with Sprint Speed ---------------------------------------

tagged <- blocked %>% filter(des_len < 50)
tagged <- left_join(tagged, sprint, by = c("batter" = "player_id", "year"))
tagged_speed <- tagged %>% group_by(year) %>%
  summarize(tag_speed = mean(sprint_speed, na.rm = TRUE))

ran_first <- left_join(ran_first, mlb_retro, by = c("BAT_ID" = "retro_id"))
ran_first <- left_join(ran_first, sprint, by = c("mlb_id" = "player_id", "year"))
ran_speed <- ran_first %>% group_by(year) %>%
  summarize(ran_speed = mean(sprint_speed, na.rm = TRUE))

thrown <- left_join(thrown, mlb_retro, by = c("BAT_ID" = "retro_id"))
thrown <- left_join(thrown, sprint, by = c("mlb_id" = "player_id", "year"))
thrown_speed <- thrown %>% group_by(year) %>%
  summarize(out_speed = mean(sprint_speed, na.rm = TRUE))

reached <- left_join(reached, mlb_retro, by = c("BAT_ID" = "retro_id"))
reached <- left_join(reached, sprint, by = c("mlb_id" = "player_id", "year"))
reached_speed <- reached %>% group_by(year) %>%
  summarize(safe_speed = mean(sprint_speed, na.rm = TRUE))

speed_table <- left_join(tagged_speed, ran_speed, by = 'year')
speed_table <- left_join(speed_table, thrown_speed, by = 'year')
speed_table <- left_join(speed_table, reached_speed, by = 'year')

speed_table

## Using Home-to-First Time -----------------------------------------------
## No 2015/2016 Data Available

tagged %>% group_by(year) %>%
  summarize(time = mean(hp_to_1b, na.rm = TRUE))

ran_first %>% group_by(year) %>%
  summarize(time = mean(hp_to_1b, na.rm = TRUE))

thrown %>% group_by(year) %>%
  summarize(time = mean(hp_to_1b, na.rm = TRUE))

reached %>% group_by(year) %>%
  summarize(time = mean(hp_to_1b, na.rm = TRUE))
