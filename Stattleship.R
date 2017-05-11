## Stattleship xCase
## Patrick O'Malley
## 3/23/17

## This is the R script used to retrieve, manipulate, analyze, and visualize 
## the data from Stattleship.

## Install packages ------------------------------------------------------------
## Package required to access Stattleship data
install.packages("devtools")
library(devtools)
## Package for dates and times
install.packages("chron")
library(chron)
## Make connection to Stattleship
devtools::install_github("stattleship/stattleship-r")  
library(stattleshipR) 
# set_token('') the temporary token is expired

## Additional packages
## Package for data manipulation
install.packages("dplyr")
library(dplyr)
## Data visualization
install.packages("ggplot2")
library(ggplot2)
## Correlation plots
install.packages("corrplot")
library(corrplot)
## 3D scatter plot package
install.packages("scatterplot3d")
library(scatterplot3d)
## additional 3d plotting package
install.packages("rgl")
library(rgl)
## for excel workbooks
install.packages("xlsx")
library(xlsx)
## more colors
install.packages("RColorBrewer")
library(RColorBrewer)
## Normality testing package
install.packages("nortest")
library(nortest)
## Distribution test plotting
install.packages("fitdistrplus")
library(fitdistrplus)

## Test script -----------------------------------------------------------------
## Test script from Stattleship to confirm data downloaded okay
league <- "nba"  
sport  <- "basketball"  
ep     <- "players"  
q_body <- list()

players <- ss_get_result(sport = sport, league = league, ep = ep, query = q_body,
                         version = 1, walk = FALSE)  
players_df <- do.call("rbind", lapply(players, function(x) x$players)) 

head(players_df)
str(players_df)
table(players_df$sport)

## Access baseball data --------------------------------------------------------

## Retrieve Player Data
sport  <- "baseball"
league <- "mlb"
ep     <- "players"
q_body <- list() 

players <- ss_get_result(sport = sport, league = league, ep = ep, 
                         query = q_body, version = 1, walk = TRUE)

## Review structure of player data from stattleship 
names(players[[1]])
str(players[[1]])

## Separate the data frames in the player list
players_df <- do.call("rbind", lapply(players, function(x) x$players))
players_df <- unique(players_df)
positions_df <- do.call("rbind", lapply(players, 
                                        function(x) x$playing_positions))
positions_df <- unique(positions_df)
teams_df <- do.call("rbind", lapply(players, function(x) x$teams))
teams_df <- unique(teams_df)

## Rename id fields
colnames(players_df)[1] <- "player_id"
colnames(teams_df)[1] <- "team_id"
colnames(positions_df)[1] <- "position_id"

## Check structure of new data frames
str(players_df)

str(teams_df)
names(teams_df)
table(teams_df$name)

names(positions_df)
table(positions_df$name)

## Retrieve Game Logs
sport  <- "baseball"
league <- "mlb"
ep     <- "game_logs"
q_body <- list(season_id = "mlb-2016", interval_type = "regularseason") 

game_logs <- ss_get_result(sport = sport, league = league, ep = ep, 
                           query = q_body, version = 1, walk = TRUE)

## Review structure of game log data from stattleship 
names(game_logs[[1]])
str(game_logs[[1]])

## Separate the data frames in the game log list
game_logs_df <- do.call("rbind", lapply(game_logs, function(x) x$game_logs)) 
game_logs_df <- unique(game_logs_df)
games_df <- do.call("rbind", lapply(game_logs, function(x) x$games))
games_df <- unique(games_df)

## Rename id fields
colnames(games_df)[1] <- "game_id"
colnames(game_logs_df)[1] <- "game_log_id"

## Check structure of new data frames
str(games_df)
names(games_df)
names(game_logs_df)
head(games_df$started_at)

count(games_df[which(games_df$home_team_id == 
                               "2863bb0d-90f6-4ae2-93ca-e89cda3255ee" |
                       games_df$away_team_id == 
                       "2863bb0d-90f6-4ae2-93ca-e89cda3255ee"), ])
## 190 total games for a single team is more than should be in regular season 

## Get team game logs
ep <- "team_game_logs"
q_body <- list(season_id = "mlb-2016", interval_type = "regularseason") 

team_game_logs <- ss_get_result(sport = sport, league = league, ep = ep, 
                           query = q_body, version = 1, walk = TRUE)

## Review structure of team game log data from stattleship 
names(team_game_logs[[1]])
str(team_game_logs[[1]])

## Separate the data frames in the game log list
team_game_logs_df <- do.call("rbind", lapply(team_game_logs, 
                                             function(x) x$team_game_logs)) 
team_game_logs_df <- unique(team_game_logs_df)

## Rename id field
colnames(team_game_logs_df)[1] <- "team_game_log_id"

## Check structure of new data frames
str(team_game_logs_df)
names(team_game_logs_df)


## Exploring data --------------------------------------------------------------
## Merge player and game log data
game_logs_player <- merge(players_df, game_logs_df, by = 'player_id') 

## Explore resulting data frame
game_logs_player <- tbl_df(game_logs_player)
game_logs_player
## Remove some of the unnecessary variables
game_logs_player2 <- select(game_logs_player, -(created_at.x:updated_at.x), 
                            -(created_at.y:updated_at.y), -(team_id.y))

## Add team names to data frame
team_names <- select(teams_df, team_id, team_name = name)
head(team_names)
names(game_logs_player2)

game_logs_player3 <- game_logs_player2 %>%  
        left_join(team_names, by = c("team_id.x" = "team_id"))
names(game_logs_player3)
head(select(game_logs_player3, team_id.x, team_name))

game_logs_player3 <- game_logs_player3 %>%  
        left_join(team_names, by = c("opponent_id" = "team_id"))
head(select(game_logs_player3, opponent_id, team_name.y))
names(game_logs_player3)
game_logs_player3 <- rename(game_logs_player3, team_name = team_name.x, 
                            opponent_name = team_name.y)
head(select(game_logs_player3, opponent_name, team_name))

## Remove excess data frames for game logs
rm(game_logs_df, game_logs_player, game_logs_player2)
game_logs_player3
str(game_logs_player3)

## Review players df
players_df <- tbl_df(players_df)
players_df <- select(players_df, -(created_at:updated_at), -(league_id))
players_df

## Only include game logs where a player actually played the game,
table(game_logs_player3$game_played)
game_logs_player4 <- game_logs_player3 %>% 
        filter(game_played == TRUE)

## Remove inactive players
table(game_logs_player4$active)
game_logs_player5 <- game_logs_player4 %>% 
        filter(active == TRUE)

## Remove extra data frames
rm(game_logs_player3, game_logs_player4)

## Review postion data ---------------------------------------------------------
table(game_logs_player5$position_abbreviation)
names(game_logs_player5)

## Pitchers (P), (SP), (RP)
## innings_pitched -- neutral   
## balks -- bad
## balls_thrown -- neutral-bad
## batters_faced -- neutral
## earned_run_average -- lower is better
## pitcher_fielding_errors -- bad
## fly_ball_outs -- good
## ground_ball_outs -- good
## losses -- bad
## no_decisions -- neutral
## outs_pitched -- good
## pickoffs -- good
## pitcher_caught_stealing -- good
## pitcher_earned_runs -- bad
## pitcher_games_played -- neutral
## pitcher_games_started -- neutral
## pitcher_hit_by_pitch -- bad
## pitcher_hits -- bad
## pitcher_home_runs -- bad
## pitcher_intentional_walks -- neutral
## pitcher_runs -- bad
## pitcher_sacrifice_flys -- neutral-bad
## pitcher_sacrifice_hits -- neutral-bad
## pitcher_stolen_bases -- neutral-bad
## pitcher_strikeouts -- good
## pitcher_walks -- bad
## pitches_thrown -- neutral
## shutouts -- very good
## strikes_thrown -- good
## strike_percentage -- high is good
## whip -- lower is better
## wild_pitches -- bad
## wins -- good

## Relief Pitcher (RP)
## blown_saves -- bad
## saves -- good
## holds -- good
## inherited_runners -- neutral
## inherited_runners_scored -- bad
## inherited runner_scoring_percentage -- low is good

## Starting Pitcher (SP)
## complete_games -- neutral
## quality_starts -- good
## starting_pitches_thrown -- neutral

## Catchers (C)
## catcher_interference -- is a bad thing
## catcher_stealers_allowed -- bad thing
## catcher_stealers_caught -- good thing
## passed_balls -- bad thing

## Hitters (DH) and others?
## extra_base_hits -- good thing
## doubles -- good thing
## at_bats -- neutral
## batting_average -- higher is better
## grounded_into_double_plays -- bad thing
## hit_by_pitch -- good thing
## hits -- good thing
## home_runs -- very good thing
## on_base_percentage -- high is good
## on_base_plus_slugging -- high is good
## runs -- good
## runs_batted_in -- good
## sacrifice_fly -- neutral-good
## sacrifice_hit -- neutral-good
## singles -- good
## slugging percentage -- good
## stolen_bases -- good
## strikeouts -- bad
## total_bases -- good
## triples -- very good
## two_out_rbi
## walks -- neutral

## Runners
## caught_stealing -- bad thing

## Fielders
## fielding_errors -- bad

## Outfield (OF)
## outfield_assists -- very good thing

## There are a lot of pitching and hitting stats but almost nothing for fielding

## Review what data is available for fielding
## First look at only fielding players
table(game_logs_player5$position_abbreviation)
select(positions_df, position_id, abbreviation, name)

fielders <- filter(game_logs_player5, position_abbreviation %in% 
                   c("1B", "2B", "3B", "CF", "IF", "LF", "RF", "OF", "SS"))
table(fielders$position_abbreviation)

## Look at fielding errors for first basemen
fielders %>% 
        filter(position_abbreviation %in% "1B") %>% 
        group_by(player_id) %>% 
        summarise(errors = sum(fielding_errors, na.rm = TRUE))

first_basemen <- filter(fielders, position_abbreviation %in% "1B")
table(first_basemen$fielding_errors)
## The fielding error rate is very small with errors by first basemen occuring
## in only 4% of games

## Calculate errors per game for infielders
infield <- filter(fielders, position_abbreviation %in% c("1B", "2B", "3B", "SS", "IF"))
infield_errors <- infield %>% 
        select(player_id, first_name, last_name, fielding_errors) %>% 
        group_by(player_id, first_name, last_name) %>% 
        summarise(errors = sum(fielding_errors, na.rm = TRUE), games_played = n()) %>% 
        mutate(err_per_gm = errors / games_played) %>% 
        filter(games_played > 50)
## Filtered for over 50 games played since the error rate is so low, it may not 
## show up for players who have less than 50 appearances

qplot(infield_errors$games_played)        
qplot(infield_errors$err_per_gm, main = "Infield Errors Per Game")
mean_in_epg <- mean(infield_errors$err_per_gm) ## mean IF epg is 0.060
stddev_in_epg <- sd(infield_errors$err_per_gm) ## sd IF epg is 0.034

## Now check the statistics for outfielders
outfield <- filter(fielders, position_abbreviation %in% c("RF", "CF", "LF", "OF"))
outfield_errors <- outfield %>% 
        select(player_id, first_name, last_name, fielding_errors) %>% 
        group_by(player_id, first_name, last_name) %>% 
        summarise(errors = sum(fielding_errors, na.rm = TRUE), games_played = n()) %>% 
        mutate(err_per_gm = errors / games_played) %>% 
        filter(games_played > 50) 

qplot(outfield_errors$games_played)        
qplot(outfield_errors$err_per_gm, main = "Outfield Errors Per Game")
mean_out_epg <- mean(outfield_errors$err_per_gm) ## mean OF epg is 0.023
stddev_out_epg <- sd(outfield_errors$err_per_gm) ## sd OF epg is 0.018
## Mean and standard deviation are different for infield and outfield with 
## infield errors rates higher than those of outfielders
## Perform a T-test to determine if the difference is significant
t.test(infield_errors$err_per_gm, outfield_errors$err_per_gm)
## The P-value is 2.2e-16 so we can reject the null hypothesis and say the means
## are different.  So we will use the infield average for infield positions and 
## the outfield average for the outfield positions for normalizing

## Create error metrics for fielders -------------------------------------------

## Add column for errors per game to infield and outfield players
outfield_errors <- outfield %>% 
        select(player_id, first_name, last_name, fielding_errors) %>% 
        group_by(player_id, first_name, last_name) %>% 
        summarise(errors = sum(fielding_errors, na.rm = TRUE), games_played = n()) %>% 
        mutate(err_per_gm = errors / games_played) %>% 
        mutate(err_pg_std = ((err_per_gm - mean_out_epg) / stddev_out_epg))

## Set the error rate for players with less than 50 games to the mean so they
## don't have spuriously low or high rates
for (i in 1:nrow(outfield_errors)) {
        if (outfield_errors$games_played[[i]] <= 50) {
                outfield_errors$err_pg_std[[i]] <- 0
        }
}

summary(outfield_errors$err_pg_std)
sd(outfield_errors$err_pg_std)
qplot(outfield_errors$err_pg_std, main = "Standardized Errors Per Game for Outfielders")        

## Infielders
infield_errors <- infield %>% 
        select(player_id, first_name, last_name, fielding_errors) %>% 
        group_by(player_id, first_name, last_name) %>% 
        summarise(errors = sum(fielding_errors, na.rm = TRUE), games_played = n()) %>% 
        mutate(err_per_gm = errors / games_played) %>% 
        mutate(err_pg_std = ((err_per_gm - mean_in_epg) / stddev_in_epg))

for (i in 1:nrow(infield_errors)) {
        if (infield_errors$games_played[[i]] <= 50) {
                infield_errors$err_pg_std[[i]] <- 0
        }
}

infield_errors

summary(infield_errors$err_pg_std)
sd(infield_errors$err_pg_std)
qplot(infield_errors$err_pg_std, main = "Standardized Errors Per Game for Infielders") 

## Create hitting metric for players -------------------------------------------
## Metric will be based on on-base-plus-slugging percentages

position_OPS <- game_logs_player5 %>% 
        select(player_id, first_name, last_name, position_abbreviation, on_base_plus_slugging) %>% 
        group_by(position_abbreviation) %>% 
        #group_by(player_id, first_name, last_name) %>% 
        summarise(tot_OPS = sum(on_base_plus_slugging, na.rm = TRUE), games_played = n()) %>% 
        mutate(avg_OPS = tot_OPS / games_played) %>% 
        arrange(desc(avg_OPS))
position_OPS
## Average per position will be used to determine weighting in final position metric

## Add OPS metric for each position that it we be used for as a metric
## Positions OPS will be used for are DH, 3B, 1B, RF, 2B, SS, CF, LF, C, IF, OF
DH_OPS <- game_logs_player5 %>% 
        select(player_id, first_name, last_name, position_abbreviation, on_base_plus_slugging) %>% 
        group_by(player_id, first_name, last_name, position_abbreviation) %>% 
        summarise(tot_OPS = sum(on_base_plus_slugging, na.rm = TRUE), games_played = n()) %>% 
        mutate(avg_OPS = tot_OPS / games_played) %>% 
        filter(games_played > 5 & position_abbreviation %in% c("DH")) %>% 
        arrange(desc(avg_OPS))
DH_OPS
summary(DH_OPS$avg_OPS)
mean_DH_OPS <- mean(DH_OPS$avg_OPS) ## mean DH OPS is 0.778
stddev_DH_OPS <- sd(DH_OPS$avg_OPS) ## sd of DH OPS is 0.089

qplot(DH_OPS$avg_OPS, main = "OPS Per Game for Designated Hitters")

## Standardize and add players with low games played back in and set OPS value to mean
DH_OPS <- game_logs_player5 %>% 
        select(player_id, first_name, last_name, position_abbreviation, on_base_plus_slugging) %>% 
        group_by(player_id, first_name, last_name, position_abbreviation) %>% 
        summarise(tot_OPS = sum(on_base_plus_slugging, na.rm = TRUE), games_played = n()) %>% 
        mutate(avg_OPS = tot_OPS / games_played) %>% 
        mutate(avg_OPS_std = ((avg_OPS - mean_DH_OPS) / stddev_DH_OPS)) %>% 
        filter(position_abbreviation %in% c("DH")) %>% 
        arrange(desc(avg_OPS))
DH_OPS
for (i in 1:nrow(DH_OPS)) {
        if (DH_OPS$games_played[[i]] <= 5) {
                DH_OPS$avg_OPS_std[[i]] <- 0
        }
}
qplot(DH_OPS$avg_OPS_std, main = "Standardized OPS Per Game for Designated Hitters")

## Functions to be created to complete this task for remaining positions
OPS_calc <- function(pos_abbr) {
  ops_df <- game_logs_player5 %>% 
                select(player_id, first_name, last_name, position_abbreviation, 
                       on_base_plus_slugging) %>% 
                group_by(player_id, first_name, last_name, position_abbreviation) %>% 
                summarise(tot_OPS = sum(on_base_plus_slugging, na.rm = TRUE), 
                          games_played = n()) %>% 
                mutate(avg_OPS = tot_OPS / games_played) %>% 
                filter(games_played > 20 & position_abbreviation %in% c(pos_abbr)) %>% 
                arrange(desc(avg_OPS))
        
        return(ops_df)
}

OPS_std <- function(pos_abbr, mean_ops, std_ops) {
  ops_std_df <- game_logs_player5 %>% 
                select(player_id, first_name, last_name, position_abbreviation, 
                       on_base_plus_slugging) %>% 
                group_by(player_id, first_name, last_name, position_abbreviation) %>% 
                summarise(tot_OPS = sum(on_base_plus_slugging, na.rm = TRUE), 
                          games_played = n()) %>% 
                mutate(avg_OPS = tot_OPS / games_played) %>% 
                mutate(avg_OPS_std = ((avg_OPS - mean_ops) / std_ops)) %>% 
                filter(position_abbreviation %in% c(pos_abbr)) %>% 
                arrange(desc(avg_OPS))
        
        for (i in 1:nrow(ops_std_df)) {
                if (ops_std_df$games_played[[i]] <= 20) {
                        ops_std_df$avg_OPS_std[[i]] <- 0
                }
        }
  return(ops_std_df)
}

## Use functions for remaining positions
## First Basemen
first_B_OPS <- OPS_calc("1B")
summary(first_B_OPS$avg_OPS)
qplot(first_B_OPS$avg_OPS, main = "OPS Per Game for First Basemen")
mean_1B_OPS <- mean(first_B_OPS$avg_OPS) ## mean 1B OPS is 0.714
stddev_1B_OPS <- sd(first_B_OPS$avg_OPS) ## sd of 1B OPS is 0.151

first_B_OPS <- OPS_std("1B", mean_1B_OPS, stddev_1B_OPS)

first_B_OPS
summary(first_B_OPS$avg_OPS_std)
qplot(first_B_OPS$avg_OPS_std, main = "Standardized OPS Per Game for First Basemen")

## Second Basemen
second_B_OPS <- OPS_calc("2B")
summary(second_B_OPS$avg_OPS)
qplot(second_B_OPS$avg_OPS, main = "OPS Per Game for Second Basemen")
mean_2B_OPS <- mean(second_B_OPS$avg_OPS) ## mean 2B OPS is 0.672
stddev_2B_OPS <- sd(second_B_OPS$avg_OPS) ## sd of 2B OPS is 0.134

second_B_OPS <- OPS_std("2B", mean_2B_OPS, stddev_2B_OPS)

second_B_OPS
summary(second_B_OPS$avg_OPS_std)
qplot(second_B_OPS$avg_OPS_std, main = "Standardized OPS Per Game for Second Basemen")

## Third Basemen
third_B_OPS <- OPS_calc("3B")
summary(third_B_OPS$avg_OPS)
qplot(third_B_OPS$avg_OPS, main = "OPS Per Game for Third Basemen")
mean_3B_OPS <- mean(third_B_OPS$avg_OPS) ## mean 3B OPS is 0.722
stddev_3B_OPS <- sd(third_B_OPS$avg_OPS) ## sd of 3B OPS is 0.143

third_B_OPS <- OPS_std("3B", mean_3B_OPS, stddev_3B_OPS)

third_B_OPS
summary(third_B_OPS$avg_OPS_std)
qplot(third_B_OPS$avg_OPS_std, main = "Standardized OPS Per Game for Third Basemen")

## Short Stops
SS_OPS <- OPS_calc("SS")
summary(SS_OPS$avg_OPS)
qplot(SS_OPS$avg_OPS, main = "OPS Per Game for Short Stops")
mean_SS_OPS <- mean(SS_OPS$avg_OPS) ## mean SS OPS is 0.646
stddev_SS_OPS <- sd(SS_OPS$avg_OPS) ## sd of SS OPS is 0.164

SS_OPS <- OPS_std("SS", mean_SS_OPS, stddev_SS_OPS)

SS_OPS
summary(SS_OPS$avg_OPS_std)
qplot(SS_OPS$avg_OPS_std, main = "Standardized OPS Per Game for Short Stops")

## Infield
IF_OPS <- OPS_calc("IF")
summary(IF_OPS$avg_OPS)
qplot(IF_OPS$avg_OPS, main = "OPS Per Game for Infielders")
mean_IF_OPS <- mean(IF_OPS$avg_OPS) ## mean IF OPS is 0.584
stddev_IF_OPS <- sd(IF_OPS$avg_OPS) ## sd of IF OPS is 0.166

IF_OPS <- OPS_std("IF", mean_IF_OPS, stddev_IF_OPS)

IF_OPS
summary(IF_OPS$avg_OPS_std)
qplot(IF_OPS$avg_OPS_std, main = "Standardized OPS Per Game for Infielders")

## Outfield
OF_OPS <- OPS_calc("OF")
summary(OF_OPS$avg_OPS)
qplot(OF_OPS$avg_OPS, main = "OPS Per Game for Outfielders")
mean_OF_OPS <- mean(OF_OPS$avg_OPS) ## mean OF OPS is 0.599
stddev_OF_OPS <- sd(OF_OPS$avg_OPS) ## sd of OF OPS is 0.129

OF_OPS <- OPS_std("OF", mean_OF_OPS, stddev_OF_OPS)

OF_OPS
summary(OF_OPS$avg_OPS_std)
qplot(OF_OPS$avg_OPS_std, main = "Standardized OPS Per Game for Outfielders")

## Right Fielders
RF_OPS <- OPS_calc("RF")
summary(RF_OPS$avg_OPS)
qplot(RF_OPS$avg_OPS, main = "OPS Per Game for Right Fielders")
mean_RF_OPS <- mean(RF_OPS$avg_OPS) ## mean RF OPS is 0.735
stddev_RF_OPS <- sd(RF_OPS$avg_OPS) ## sd of RF OPS is 0.119

RF_OPS <- OPS_std("RF", mean_RF_OPS, stddev_RF_OPS)

RF_OPS
summary(RF_OPS$avg_OPS_std)
qplot(RF_OPS$avg_OPS_std, main = "Standardized OPS Per Game for Right Fielders")

## Center Fielders
CF_OPS <- OPS_calc("CF")
summary(CF_OPS$avg_OPS)
qplot(CF_OPS$avg_OPS, main = "OPS Per Game for Center Fielders")
mean_CF_OPS <- mean(CF_OPS$avg_OPS) ## mean CF OPS is 0.658
stddev_CF_OPS <- sd(CF_OPS$avg_OPS) ## sd of CF OPS is 0.122

CF_OPS <- OPS_std("CF", mean_CF_OPS, stddev_CF_OPS)

CF_OPS
summary(CF_OPS$avg_OPS_std)
qplot(CF_OPS$avg_OPS_std, main = "Standardized OPS Per Game for Center Fielders")

## Left Fielders
LF_OPS <- OPS_calc("LF")
summary(LF_OPS$avg_OPS)
qplot(LF_OPS$avg_OPS, main = "OPS Per Game for Left Fielders")
mean_LF_OPS <- mean(LF_OPS$avg_OPS) ## mean LF OPS is 0.661
stddev_LF_OPS <- sd(LF_OPS$avg_OPS) ## sd of LF OPS is 0.128

LF_OPS <- OPS_std("LF", mean_LF_OPS, stddev_LF_OPS)

LF_OPS
summary(LF_OPS$avg_OPS_std)
qplot(LF_OPS$avg_OPS_std, main = "Standardized OPS Per Game for Left Fielders")

## Catchers
C_OPS <- OPS_calc("C")
summary(C_OPS$avg_OPS)
qplot(C_OPS$avg_OPS, main = "OPS Per Game for Catchers")
mean_C_OPS <- mean(C_OPS$avg_OPS) ## mean C OPS is 0.633
stddev_C_OPS <- sd(C_OPS$avg_OPS) ## sd of C OPS is 0.157

C_OPS <- OPS_std("C", mean_C_OPS, stddev_C_OPS)

C_OPS
summary(C_OPS$avg_OPS_std)
qplot(C_OPS$avg_OPS_std, main = "Standardized OPS Per Game for Catchers")

## Calculate additional catcher statistics -------------------------------------
## catcher_stealers_allowed -- bad thing
## catcher_stealers_caught -- good thing

## Catcher Stealers Allowed
C_CSA <- game_logs_player5 %>% 
        select(player_id, first_name, last_name, position_abbreviation, catcher_stealers_allowed) %>% 
        group_by(player_id, first_name, last_name, position_abbreviation) %>% 
        summarise(tot_CSA = sum(catcher_stealers_allowed, na.rm = TRUE), games_played = n()) %>% 
        mutate(avg_CSA = tot_CSA / games_played) %>% 
        filter(games_played > 50 & position_abbreviation %in% c("C")) %>% 
        arrange(avg_CSA)
C_CSA
summary(C_CSA$avg_CSA)
mean_C_CSA <- mean(C_CSA$avg_CSA) ## mean C CSA is 0.229
stddev_C_CSA <- sd(C_CSA$avg_CSA) ## sd of C CSA is 0.116

qplot(C_CSA$avg_CSA, main = "Stealers Allowed Per Game for Catchers")

## Standardize and add players with low games played back in and set CSA value to mean
C_CSA <- game_logs_player5 %>% 
        select(player_id, first_name, last_name, position_abbreviation, catcher_stealers_allowed) %>% 
        group_by(player_id, first_name, last_name, position_abbreviation) %>% 
        summarise(tot_CSA = sum(catcher_stealers_allowed, na.rm = TRUE), games_played = n()) %>% 
        mutate(avg_CSA = tot_CSA / games_played) %>% 
        mutate(avg_CSA_std = ((avg_CSA - mean_C_CSA) / stddev_C_CSA)) %>% 
        filter(position_abbreviation %in% c("C")) %>% 
        arrange(avg_CSA)
C_CSA
for (i in 1:nrow(C_CSA)) {
        if (C_CSA$games_played[[i]] <= 50) {
                C_CSA$avg_CSA_std[[i]] <- 0
        }
}
qplot(C_CSA$avg_CSA_std, main = "Standardized Stealers Allowed Per Game for Catchers")

## Catcher Stealers Caught
C_CSC <- game_logs_player5 %>% 
        select(player_id, first_name, last_name, position_abbreviation, catcher_stealers_caught) %>% 
        group_by(player_id, first_name, last_name, position_abbreviation) %>% 
        summarise(tot_CSC = sum(catcher_stealers_caught, na.rm = TRUE), games_played = n()) %>% 
        mutate(avg_CSC = tot_CSC / games_played) %>% 
        filter(games_played > 50 & position_abbreviation %in% c("C")) %>% 
        arrange(desc(avg_CSC))
C_CSC
summary(C_CSC$avg_CSC)
mean_C_CSC <- mean(C_CSC$avg_CSC) ## mean C CSC is 0.100
stddev_C_CSC <- sd(C_CSC$avg_CSC) ## sd of C CSC is 0.052

qplot(C_CSC$avg_CSC, main = "Stealers Caught Per Game for Catchers")

## Standardize and add players with low games played back in and set CSC value to mean
C_CSC <- game_logs_player5 %>% 
        select(player_id, first_name, last_name, position_abbreviation, catcher_stealers_caught) %>% 
        group_by(player_id, first_name, last_name, position_abbreviation) %>% 
        summarise(tot_CSC = sum(catcher_stealers_caught, na.rm = TRUE), games_played = n()) %>% 
        mutate(avg_CSC = tot_CSC / games_played) %>% 
        mutate(avg_CSC_std = ((avg_CSC - mean_C_CSC) / stddev_C_CSC)) %>% 
        filter(position_abbreviation %in% c("C")) %>% 
        arrange(desc(avg_CSC))
C_CSC
for (i in 1:nrow(C_CSC)) {
        if (C_CSC$games_played[[i]] <= 50) {
                C_CSC$avg_CSC_std[[i]] <- 0
        }
}
qplot(C_CSC$avg_CSC_std, main = "Standardized Stealers Caught Per Game for Catchers")

## Determine pitcher metrics ---------------------------------------------------
## The first metric which will be calculated for all pitchers is K+ 
## (K's + fly outs + ground outs) / batters faced

pitcher_kplus <- game_logs_player5 %>% 
        select(player_id, first_name, last_name, position_abbreviation, 
               batters_faced, pitcher_strikeouts, ground_ball_outs, fly_ball_outs) %>% 
        group_by(player_id, first_name, last_name, position_abbreviation) %>% 
        summarise(tot_bf = sum(batters_faced, na.rm = TRUE), 
                  tot_k = sum(pitcher_strikeouts, na.rm = TRUE),
                  tot_gbo = sum(ground_ball_outs, na.rm = TRUE),
                  tot_fbo = sum(fly_ball_outs, na.rm = TRUE),
                  games_played = n()) %>% 
        mutate(kplus = (tot_k + tot_gbo + tot_fbo) / tot_bf) %>% 
        filter(games_played > 20 & position_abbreviation %in% c("P", "SP", "RP")) %>% 
        arrange(desc(kplus))

## Remove the row with NA value
pitcher_kplus <- pitcher_kplus[-which(is.nan(pitcher_kplus$kplus)), ]
pitcher_kplus

summary(pitcher_kplus$kplus)
summary(pitcher_kplus$kplus[which(pitcher_kplus$position_abbreviation == "SP")])
summary(pitcher_kplus$kplus[which(pitcher_kplus$position_abbreviation == "RP")])
summary(pitcher_kplus$kplus[which(pitcher_kplus$position_abbreviation == "P")])


mean_kplus <- mean(pitcher_kplus$kplus) ## mean K+ is 0.576
stddev_kplus <- sd(pitcher_kplus$kplus) ## sd of K+ is 0.044

qplot(pitcher_kplus$kplus, main = "Pitcher K+")

## Standardize and add players with low games played back in and set K+ value to mean
pitcher_kplus <- game_logs_player5 %>% 
        select(player_id, first_name, last_name, position_abbreviation, 
               batters_faced, pitcher_strikeouts, ground_ball_outs, fly_ball_outs) %>%
        group_by(player_id, first_name, last_name, position_abbreviation) %>% 
        summarise(tot_bf = sum(batters_faced, na.rm = TRUE), 
                  tot_k = sum(pitcher_strikeouts, na.rm = TRUE),
                  tot_gbo = sum(ground_ball_outs, na.rm = TRUE),
                  tot_fbo = sum(fly_ball_outs, na.rm = TRUE),
                  games_played = n()) %>% 
        mutate(kplus = (tot_k + tot_gbo + tot_fbo) / tot_bf) %>% 
        mutate(kplus_std = ((kplus - mean_kplus) / stddev_kplus)) %>% 
        filter(position_abbreviation %in% c("P", "RP", "SP")) %>% 
        arrange(desc(kplus))
pitcher_kplus
pitcher_kplus <- pitcher_kplus[!is.nan(pitcher_kplus$kplus), ] 
for (i in 1:nrow(pitcher_kplus)) {
        if (pitcher_kplus$games_played[[i]] <= 20) {
                pitcher_kplus$kplus_std[[i]] <- 0
        }
}
qplot(pitcher_kplus$kplus_std, main = "Standardized Pitcher K+")

## Look at quality starts rates for starting pitchers
SP_QS <- game_logs_player5 %>% 
        select(player_id, first_name, last_name, position_abbreviation, quality_starts) %>% 
        group_by(player_id, first_name, last_name, position_abbreviation) %>% 
        summarise(tot_qs = sum(quality_starts, na.rm = TRUE), games_played = n()) %>% 
        mutate(qs_rate = tot_qs / games_played) %>% 
        filter(games_played > 10 & position_abbreviation %in% c("SP")) %>% 
        arrange(desc(qs_rate))

summary(SP_QS$qs_rate)
qplot(SP_QS$qs_rate, main = "Starting Pitcher Quality Start Rate")
mean_SP_QS <- mean(SP_QS$qs_rate) ## Mean SP quality start rate is 0.379
stddev_SP_QS <- sd(SP_QS$qs_rate) ## SD for SP quality start rate is 0.193

## Standardize
SP_QS <- game_logs_player5 %>% 
        select(player_id, first_name, last_name, position_abbreviation, quality_starts) %>% 
        group_by(player_id, first_name, last_name, position_abbreviation) %>% 
        summarise(tot_qs = sum(quality_starts, na.rm = TRUE), games_played = n()) %>% 
        mutate(qs_rate = tot_qs / games_played) %>% 
        mutate(qsr_std = (qs_rate - mean_SP_QS) / stddev_SP_QS) %>% 
        filter(position_abbreviation %in% c("SP")) %>% 
        arrange(desc(qs_rate))
SP_QS
for (i in 1:nrow(SP_QS)) {
        if (SP_QS$games_played[[i]] <= 10) {
                SP_QS$qsr_std[[i]] <- 0
        }
}
summary(SP_QS$qsr_std)
qplot(SP_QS$qsr_std, main = "Standardized Starting Pitcher Quality Start Rate")


## Now calculate quality starts for other pitchers
P_QS <- game_logs_player5 %>% 
        select(player_id, first_name, last_name, position_abbreviation, quality_starts) %>% 
        group_by(player_id, first_name, last_name, position_abbreviation) %>% 
        summarise(tot_qs = sum(quality_starts, na.rm = TRUE), games_played = n()) %>% 
        mutate(qs_rate = tot_qs / games_played) %>% 
        filter(games_played > 5 & position_abbreviation %in% c("P")) %>% 
        arrange(desc(qs_rate))
summary(P_QS$qs_rate)
P_QS
mean_P_QS <- mean(P_QS$qs_rate) ## Mean pitcher quality start rate is 0.039
stddev_P_QS <- sd(P_QS$qs_rate) ## sd of pitcher QSR is 0.093
qplot(P_QS$qs_rate, main = "Pitcher Quality Start Rate")

## Standardize
P_QS <- game_logs_player5 %>% 
        select(player_id, first_name, last_name, position_abbreviation, quality_starts) %>% 
        group_by(player_id, first_name, last_name, position_abbreviation) %>% 
        summarise(tot_qs = sum(quality_starts, na.rm = TRUE), games_played = n()) %>% 
        mutate(qs_rate = tot_qs / games_played) %>% 
        mutate(qsr_std = (qs_rate - mean_P_QS) / stddev_P_QS) %>% 
        filter(position_abbreviation %in% c("P")) %>% 
        arrange(desc(qs_rate))
P_QS
for (i in 1:nrow(P_QS)) {
        if (P_QS$games_played[[i]] <= 10) {
                P_QS$qsr_std[[i]] <- 0
        }
}
summary(P_QS$qsr_std)
qplot(P_QS$qsr_std, main = "Standardized Pitcher Quality Start Rate")

## Create save metric for Relief Pitchers
RP_SP <- game_logs_player5 %>% 
        select(player_id, first_name, last_name, position_abbreviation, saves, blown_saves) %>% 
        group_by(player_id, first_name, last_name, position_abbreviation) %>% 
        summarise(tot_saves = sum(saves, na.rm = TRUE), 
                  tot_save_opp = sum(blown_saves, na.rm = TRUE) + 
                          sum(saves, na.rm = TRUE), games_played = n()) %>% 
        mutate(save_perc = tot_saves / tot_save_opp) %>% 
        filter(tot_save_opp > 10 & position_abbreviation %in% c("RP")) %>% 
        arrange(desc(save_perc))
summary(RP_SP$save_perc)
RP_SP
mean_RP_SP <- mean(RP_SP$save_perc) ## Mean relief pitcher save percent is 0.798
stddev_RP_SP <- sd(RP_SP$save_perc) ## sd of relief pitcher save pecent is 0.129
qplot(RP_SP$save_perc, main = "Relief Pitcher Save Percent")

## Standardize
RP_SP <- game_logs_player5 %>% 
        select(player_id, first_name, last_name, position_abbreviation, saves, blown_saves) %>% 
        group_by(player_id, first_name, last_name, position_abbreviation) %>% 
       summarise(tot_saves = sum(saves, na.rm = TRUE), 
                  tot_save_opp = sum(blown_saves, na.rm = TRUE) + 
                          sum(saves, na.rm = TRUE), games_played = n()) %>%  
        mutate(save_perc = tot_saves / tot_save_opp) %>%  
        mutate(save_perc_std = (save_perc - mean_RP_SP) / stddev_RP_SP) %>% 
        filter(position_abbreviation %in% c("RP")) %>% 
        arrange(desc(save_perc))
RP_SP
for (i in 1:nrow(RP_SP)) {
        if (RP_SP$tot_save_opp[[i]] <= 10) {
                RP_SP$save_perc_std[[i]] <- 0
        }
}
summary(RP_SP$save_perc_std)
qplot(RP_SP$save_perc_std, main = "Standardized Relief Pitcher Save Percent")

## Calculate two out RBI stats for DH's
DH_torbi <- game_logs_player5 %>% 
        select(player_id, first_name, last_name, position_abbreviation, two_out_rbi) %>% 
        group_by(player_id, first_name, last_name, position_abbreviation) %>% 
        summarise(tot_torbi = sum(two_out_rbi, na.rm = TRUE), 
                  games_played = n()) %>% 
        mutate(torbi_rate = tot_torbi / games_played) %>% 
        filter(games_played > 5 & position_abbreviation %in% c("DH")) %>% 
        arrange(desc(tot_torbi))
summary(DH_torbi$torbi_rate)
DH_torbi
mean_DH_torbi <- mean(DH_torbi$torbi_rate) ## Mean relief pitcher save percent is 0.135
stddev_DH_torbi <- sd(DH_torbi$torbi_rate) ## sd of relief pitcher save pecent is 0.029
qplot(DH_torbi$torbi_rate, main = "Designated Hitter Two Out RBI Rate")

## Standardize
DH_torbi <- game_logs_player5 %>% 
        select(player_id, first_name, last_name, position_abbreviation, two_out_rbi) %>% 
        group_by(player_id, first_name, last_name, position_abbreviation) %>% 
        summarise(tot_torbi = sum(two_out_rbi, na.rm = TRUE), 
                  games_played = n()) %>% 
        mutate(torbi_rate = tot_torbi / games_played) %>%
        mutate(torbi_std = (torbi_rate - mean_DH_torbi) / stddev_DH_torbi) %>% 
        filter(position_abbreviation %in% c("DH")) %>% 
        arrange(desc(torbi_rate))
DH_torbi
for (i in 1:nrow(DH_torbi)) {
        if (DH_torbi$games_played[[i]] <= 5) {
                DH_torbi$torbi_std[[i]] <- 0
        }
}
summary(DH_torbi$torbi_std)
qplot(DH_torbi$torbi_std, main = "Standardized Designated Hitter Two Out RBI Rate")

## Calculate outfield assists per game -----------------------------------------
game_logs_player5 %>% select(player_id, outfield_assists) %>% 
        filter(outfield_assists > 0 & player_id == "a07220d4-96f4-42f6-a00c-ca83f9dd0265") %>% 
        arrange(desc(outfield_assists))
## The outfield assist columns appears to be a total of the assists on the year
## so the max value will be used and averaged across games played


outfield_oa <- game_logs_player5 %>% 
        select(player_id, first_name, last_name, position_abbreviation, outfield_assists) %>% 
        group_by(player_id, first_name, last_name, position_abbreviation) %>% 
        summarise(tot_oa = max(outfield_assists, na.rm = TRUE), 
                  games_played = n()) %>% 
        mutate(oa_rate = tot_oa / games_played) %>% 
        filter(games_played > 50 & position_abbreviation %in% c("OF", "CF", "RF", "LF")) %>% 
        arrange(desc(oa_rate))

summary(outfield_oa$oa_rate)
summary(outfield_oa$oa_rate[which(outfield_oa$position_abbreviation == "OF")])
summary(outfield_oa$oa_rate[which(outfield_oa$position_abbreviation == "CF")])
summary(outfield_oa$oa_rate[which(outfield_oa$position_abbreviation == "RF")])
summary(outfield_oa$oa_rate[which(outfield_oa$position_abbreviation == "LF")])
qplot(outfield_oa$oa_rate, main = "Outfield Assist Rate")
## histogram is still highly skewed right even after removing players that have
## played few games so the median will be used during standardization rather than
## the mean. medians for each position in the outfield are close so they will be
## calculated together
mean_outfield_oa <- mean(outfield_oa$oa_rate) ## Mean outfield assist rate is 0.017
stddev_outfield_oa <- sd(outfield_oa$oa_rate) ## sd of outfield assist rate is 0.018
median_outfield_oa <- median(outfield_oa$oa_rate) ## Median outfield assist rate is 0.010

outfield_oa <- game_logs_player5 %>% 
        select(player_id, first_name, last_name, position_abbreviation, outfield_assists) %>% 
        group_by(player_id, first_name, last_name, position_abbreviation) %>% 
        summarise(tot_oa = max(outfield_assists, na.rm = TRUE), 
                  games_played = n()) %>% 
        mutate(oa_rate = tot_oa / games_played) %>% 
        mutate(oa_std = (oa_rate - mean_outfield_oa) / stddev_outfield_oa) %>% 
        filter(position_abbreviation %in% c("OF", "CF", "RF", "LF")) %>% 
        arrange(desc(oa_rate))
outfield_oa

std_med_oa <- (median_outfield_oa - mean_outfield_oa) / stddev_outfield_oa

for (i in 1:nrow(outfield_oa)) {
        if (outfield_oa$games_played[[i]] <= 50) {
                outfield_oa$oa_std[[i]] <- std_med_oa
        }
}

qplot(outfield_oa$oa_std, main = "Standardized Outfield Assist Rate")
outfield_oa[which.max(outfield_oa$oa_std), ]
## Composite score for each position -------------------------------------------
## Metrics for each position will be combined to form a single score

## SP Metric -------------------------------------------------------------------
## Metrics to combine are K+ and quality start rate

## First filter out all unneccesary columns 
SP_metric <- pitcher_kplus %>% 
        ungroup() %>%
        filter(position_abbreviation %in% "SP") %>% 
        select(player_id, first_name, last_name, kplus_std) %>% 
        arrange(kplus_std)
SP_metric
summary(SP_metric$kplus_std)

## Filter columns for QSR as well
temp <- SP_QS %>% 
          ungroup() %>% 
          select(player_id, qsr_std)
        
SP_metric <- SP_metric %>% left_join(temp)

## Review relation between two metrics
SP_lm <- lm(SP_metric$kplus_std ~ SP_metric$qsr_std)
SP_coeff <- coefficients(SP_lm)
plot(SP_metric$kplus_std, SP_metric$qsr_std, main = "QSR vs K+")
abline(SP_coeff)
SP_corr <- cor(SP_metric$kplus_std, SP_metric$qsr_std)
## relationship is positive with R = 0.447

ggplot(data = SP_metric, aes(x = kplus_std, y = qsr_std)) + geom_point() + 
        geom_smooth(method = "lm") + ggtitle("Pitcher Stats Relationship")
## Create normalized single score
SP_metric <- SP_metric %>% 
        mutate(SP_metric = .6 * kplus_std + .4 * qsr_std) %>% 
        arrange(desc(SP_metric))

max_SP_metric <- max(SP_metric$SP_metric)
min_SP_metric <- min(SP_metric$SP_metric)

SP_metric <- SP_metric %>% 
        mutate(SP_metric_norm = (abs(min_SP_metric) + SP_metric) / 
                       (abs(min_SP_metric) + max_SP_metric)) %>% 
        arrange(desc(SP_metric_norm))
## View results
SP_metric[1:10, ]
SP_metric[11:20, ]

summary(SP_metric$SP_metric_norm)
qplot(SP_metric$SP_metric_norm, main = "Normalized Starting Pitcher Metric", 
      xlab = "SP Metric")

## RP Metric -------------------------------------------------------------------
## Metrics to combine are K+ and save percent

## First filter out all unneccesary columns 
RP_metric <- pitcher_kplus %>% 
        ungroup() %>%
        filter(position_abbreviation %in% "RP") %>% 
        select(player_id, first_name, last_name, kplus_std) %>% 
        arrange(desc(kplus_std))
RP_metric
summary(RP_metric$kplus_std)

## Filter columns for save percent as well
temp <- RP_SP %>% 
          ungroup() %>% 
          select(player_id, save_perc_std)
        
RP_metric <- RP_metric %>% left_join(temp)

## Review relation between two metrics
RP_lm <- lm(RP_metric$kplus_std ~ RP_metric$save_perc_std)
RP_coeff <- coefficients(RP_lm)
plot(RP_metric$kplus_std, RP_metric$save_perc_std, main = "Save Percent vs K+")
abline(RP_coeff)
RP_corr <- cor(RP_metric$kplus_std, RP_metric$save_perc_std)
## relationship is weakly positive with R = 0.086

## Create normalized single score
RP_metric <- RP_metric %>% 
        mutate(RP_metric = .5 * kplus_std + .5 * save_perc_std) %>% 
        arrange(desc(RP_metric))

max_RP_metric <- max(RP_metric$RP_metric)
min_RP_metric <- min(RP_metric$RP_metric)

RP_metric <- RP_metric %>% 
        mutate(RP_metric_norm = (abs(min_RP_metric) + RP_metric) / 
                       (abs(min_RP_metric) + max_RP_metric)) %>% 
        arrange(desc(RP_metric_norm))
## View results
RP_metric[1:10, ]
RP_metric[11:20, ]

summary(RP_metric$RP_metric_norm)
qplot(RP_metric$RP_metric_norm, main = "Normalized Relief Pitcher Metric", 
      xlab = "RP Metric")

## P Metric --------------------------------------------------------------------
## Metrics to combine are K+ and quality start rate

## First filter out all unneccesary columns 
P_metric <- pitcher_kplus %>% 
        ungroup() %>%
        filter(position_abbreviation %in% "P") %>% 
        select(player_id, first_name, last_name, kplus_std) %>% 
        arrange(desc(kplus_std))
P_metric
summary(P_metric$kplus_std)

## Filter columns for QSR as well
temp <- P_QS %>% 
          ungroup() %>% 
          select(player_id, qsr_std)
        
P_metric <- P_metric %>% left_join(temp)

## Review relation between two metrics
P_lm <- lm(P_metric$kplus_std ~ P_metric$qsr_std)
P_coeff <- coefficients(P_lm)
plot(P_metric$kplus_std, P_metric$qsr_std, main = "QSR vs K+")
abline(P_coeff)
P_corr <- cor(P_metric$kplus_std, P_metric$qsr_std)
## relationship is weakly negative with R = -0.076

## Create normalized single score
P_metric <- P_metric %>% 
        mutate(P_metric = .6 * kplus_std + .4 * qsr_std) %>% 
        arrange(desc(P_metric))

max_P_metric <- max(P_metric$P_metric)
min_P_metric <- min(P_metric$P_metric)

P_metric <- P_metric %>% 
        mutate(P_metric_norm = (abs(min_P_metric) + P_metric) / 
                       (abs(min_P_metric) + max_P_metric)) %>% 
        arrange(desc(P_metric_norm))
## View results
P_metric[1:10, ]
P_metric[11:20, ]

summary(P_metric$P_metric_norm)
qplot(P_metric$P_metric_norm, main = "Normalized Pitcher Metric", 
      xlab = "P Metric")

## C Metric --------------------------------------------------------------------
## Metrics to combine are OPS, catcher steals alowed, and catcher steals caught

## First filter out all unneccesary columns 
C_metric <- C_OPS %>% 
        ungroup() %>%
        select(player_id, first_name, last_name, avg_OPS_std) %>% 
        arrange(desc(avg_OPS_std))
C_metric
summary(C_metric$avg_OPS_std)

## Filter columns for CSA as well
temp <- C_CSA %>% 
          ungroup() %>% 
          select(player_id, avg_CSA_std)
        
C_metric <- C_metric %>% left_join(temp)

## Filter columns for CSC as well
temp <- C_CSC %>% 
          ungroup() %>% 
          select(player_id, avg_CSC_std)
        
C_metric <- C_metric %>% left_join(temp)

## Review relation between two metrics
C_lm <- lm(data = C_metric, avg_OPS_std ~ avg_CSA_std + avg_CSC_std)
C_coeff <- coefficients(C_lm)
Cplot <- scatterplot3d(C_metric$avg_CSA_std, C_metric$avg_CSC_std, C_metric$avg_OPS_std,
              main = "Catcher Metrics Plot")
Cplot$plane3d(C_lm, col = "red")
plot3d(C_metric$avg_CSA_std, C_metric$avg_CSC_std, C_metric$avg_OPS_std)

C_corr <- cor(C_metric[ , c(4:6)])
corrplot(C_corr, type = "upper", method = "number")
    
## Create normalized single score
C_metric <- C_metric %>% 
        mutate(C_metric = .3 * avg_OPS_std - .35 * avg_CSA_std + .35 * avg_CSC_std) %>% 
        arrange(desc(C_metric))

max_C_metric <- max(C_metric$C_metric)
min_C_metric <- min(C_metric$C_metric)

C_metric <- C_metric %>% 
        mutate(C_metric_norm = (abs(min_C_metric) + C_metric) / 
                       (abs(min_C_metric) + max_C_metric)) %>% 
        arrange(desc(C_metric_norm))
## View results
C_metric[1:10, ]
C_metric[11:20, ]

summary(C_metric$C_metric_norm)
qplot(C_metric$C_metric_norm, main = "Normalized Catcher Metric", 
      xlab = "C Metric")

## 1B Metric -------------------------------------------------------------------
## Metrics to combine are OPS, and error rate

## First filter out all unneccesary columns 
first_B_metric <- first_B_OPS %>% 
        ungroup() %>%
        select(player_id, first_name, last_name, avg_OPS_std) %>% 
        arrange(desc(avg_OPS_std))
first_B_metric
summary(first_B_metric$avg_OPS_std)

## Filter columns for error rate as well
temp <- infield_errors %>% 
          ungroup() %>% 
          select(player_id, err_pg_std)
        
first_B_metric <- first_B_metric %>% left_join(temp)

## Review relation between two metrics
first_B_lm <- lm(data = first_B_metric, avg_OPS_std ~ err_pg_std)
first_B_coeff <- coefficients(first_B_lm)
plot(first_B_metric$avg_OPS_std, first_B_metric$err_pg_std, main = "OPS vs EPG")
abline(first_B_coeff)
first_B_corr <- cor(first_B_metric$avg_OPS_std, first_B_metric$err_pg_std)
## correlation is very weak with R = 0.004
    
## Create normalized single score
first_B_metric <- first_B_metric %>% 
        mutate(first_B_metric = .65 * avg_OPS_std - .35 * err_pg_std) %>% 
        arrange(desc(first_B_metric))

max_1B_metric <- max(first_B_metric$first_B_metric)
min_1B_metric <- min(first_B_metric$first_B_metric)

first_B_metric <- first_B_metric %>% 
        mutate(first_B_metric_norm = (abs(min_1B_metric) + first_B_metric) / 
                       (abs(min_1B_metric) + max_1B_metric)) %>% 
        arrange(desc(first_B_metric_norm))
## View results
first_B_metric[1:10, ]
first_B_metric[11:20, ]

summary(first_B_metric$first_B_metric_norm)
qplot(first_B_metric$first_B_metric_norm, main = "Normalized First Basemen Metric", 
      xlab = "First Basemen Metric")

## 2B Metric -------------------------------------------------------------------
## Metrics to combine are OPS, and error rate

## First filter out all unneccesary columns 
second_B_metric <- second_B_OPS %>% 
        ungroup() %>%
        select(player_id, first_name, last_name, avg_OPS_std) %>% 
        arrange(desc(avg_OPS_std))
second_B_metric
summary(second_B_metric$avg_OPS_std)

## Filter columns for error rate as well
temp <- infield_errors %>% 
          ungroup() %>% 
          select(player_id, err_pg_std)
        
second_B_metric <- second_B_metric %>% left_join(temp)

## Review relation between two metrics
second_B_lm <- lm(data = second_B_metric, avg_OPS_std ~ err_pg_std)
second_B_coeff <- coefficients(second_B_lm)
plot(second_B_metric$avg_OPS_std, second_B_metric$err_pg_std, main = "OPS vs EPG")
abline(second_B_coeff)
second_B_corr <- cor(second_B_metric$avg_OPS_std, second_B_metric$err_pg_std)
## correlation is weak with R = 0.184
    
## Create normalized single score
second_B_metric <- second_B_metric %>% 
        mutate(second_B_metric = .55 * avg_OPS_std - .45 * err_pg_std) %>% 
        arrange(desc(second_B_metric))

max_2B_metric <- max(second_B_metric$second_B_metric)
min_2B_metric <- min(second_B_metric$second_B_metric)

second_B_metric <- second_B_metric %>% 
        mutate(second_B_metric_norm = (abs(min_2B_metric) + second_B_metric) / 
                       (abs(min_2B_metric) + max_2B_metric)) %>% 
        arrange(desc(second_B_metric_norm))
## View results
second_B_metric[1:10, ]
second_B_metric[11:20, ]

summary(second_B_metric$second_B_metric_norm)
qplot(second_B_metric$second_B_metric_norm, main = "Normalized Second Basemen Metric", 
      xlab = "Second Basemen Metric")

## 3B Metric -------------------------------------------------------------------
## Metrics to combine are OPS, and error rate

## First filter out all unneccesary columns 
third_B_metric <- third_B_OPS %>% 
        ungroup() %>%
        select(player_id, first_name, last_name, avg_OPS_std) %>% 
        arrange(desc(avg_OPS_std))
third_B_metric
summary(third_B_metric$avg_OPS_std)

## Filter columns for error rate as well
temp <- infield_errors %>% 
          ungroup() %>% 
          select(player_id, err_pg_std)
        
third_B_metric <- third_B_metric %>% left_join(temp)

## Review relation between two metrics
third_B_lm <- lm(data = third_B_metric, avg_OPS_std ~ err_pg_std)
third_B_coeff <- coefficients(third_B_lm)
plot(third_B_metric$avg_OPS_std, third_B_metric$err_pg_std, main = "OPS vs EPG")
abline(third_B_coeff)
third_B_corr <- cor(third_B_metric$avg_OPS_std, third_B_metric$err_pg_std)
## correlation is weak with R = 0.296
    
## Create normalized single score
third_B_metric <- third_B_metric %>% 
        mutate(third_B_metric = .7 * avg_OPS_std - .3 * err_pg_std) %>% 
        arrange(desc(third_B_metric))

max_3B_metric <- max(third_B_metric$third_B_metric)
min_3B_metric <- min(third_B_metric$third_B_metric)

third_B_metric <- third_B_metric %>% 
        mutate(third_B_metric_norm = (abs(min_3B_metric) + third_B_metric) / 
                       (abs(min_3B_metric) + max_3B_metric)) %>% 
        arrange(desc(third_B_metric_norm))
## View results
third_B_metric[1:10, ]
third_B_metric[11:20, ]

summary(third_B_metric$third_B_metric_norm)
qplot(third_B_metric$third_B_metric_norm, main = "Normalized Third Basemen Metric", 
      xlab = "Third Basemen Metric")

## SS Metric -------------------------------------------------------------------
## Metrics to combine are OPS, and error rate

## First filter out all unneccesary columns 
SS_metric <- SS_OPS %>% 
        ungroup() %>%
        select(player_id, first_name, last_name, avg_OPS_std) %>% 
        arrange(desc(avg_OPS_std))
SS_metric
summary(SS_metric$avg_OPS_std)

## Filter columns for error rate as well
temp <- infield_errors %>% 
          ungroup() %>% 
          select(player_id, err_pg_std)
        
SS_metric <- SS_metric %>% left_join(temp)

## Review relation between two metrics
SS_lm <- lm(data = SS_metric, avg_OPS_std ~ err_pg_std)
SS_coeff <- coefficients(SS_lm)
plot(SS_metric$avg_OPS_std, SS_metric$err_pg_std, main = "OPS vs EPG")
abline(SS_coeff)
SS_corr <- cor(SS_metric$avg_OPS_std, SS_metric$err_pg_std)
## correlation is weak with R = 0.342
    
## Create normalized single score
SS_metric <- SS_metric %>% 
        mutate(SS_metric = .55 * avg_OPS_std - .45 * err_pg_std) %>% 
        arrange(desc(SS_metric))

max_SS_metric <- max(SS_metric$SS_metric)
min_SS_metric <- min(SS_metric$SS_metric)

SS_metric <- SS_metric %>% 
        mutate(SS_metric_norm = (abs(min_SS_metric) + SS_metric) / 
                       (abs(min_SS_metric) + max_SS_metric)) %>% 
        arrange(desc(SS_metric_norm))
## View results
SS_metric[1:10, ]
SS_metric[11:20, ]

summary(SS_metric$SS_metric_norm)
qplot(SS_metric$SS_metric_norm, main = "Normalized Short Stop Metric", 
      xlab = "Short Stop Metric")

## IF Metric -------------------------------------------------------------------
## Metrics to combine are OPS, and error rate

## First filter out all unneccesary columns 
IF_metric <- IF_OPS %>% 
        ungroup() %>%
        select(player_id, first_name, last_name, avg_OPS_std) %>% 
        arrange(desc(avg_OPS_std))
IF_metric
summary(IF_metric$avg_OPS_std)

## Filter columns for error rate as well
temp <- infield_errors %>% 
          ungroup() %>% 
          select(player_id, err_pg_std)
        
IF_metric <- IF_metric %>% left_join(temp)

## Review relation between two metrics
IF_lm <- lm(data = IF_metric, avg_OPS_std ~ err_pg_std)
IF_coeff <- coefficients(IF_lm)
plot(IF_metric$avg_OPS_std, IF_metric$err_pg_std, main = "OPS vs EPG")
abline(IF_coeff)
IF_corr <- cor(IF_metric$avg_OPS_std, IF_metric$err_pg_std)
## correlation is weak with R = 0.346
    
## Create normalized single score
IF_metric <- IF_metric %>% 
        mutate(IF_metric = .55 * avg_OPS_std - .45 * err_pg_std) %>% 
        arrange(desc(IF_metric))

max_IF_metric <- max(IF_metric$IF_metric)
min_IF_metric <- min(IF_metric$IF_metric)

IF_metric <- IF_metric %>% 
        mutate(IF_metric_norm = (abs(min_IF_metric) + IF_metric) / 
                       (abs(min_IF_metric) + max_IF_metric)) %>% 
        arrange(desc(IF_metric_norm))
## View results
IF_metric[1:10, ]
IF_metric[11:20, ]

summary(IF_metric$IF_metric_norm)
qplot(IF_metric$IF_metric_norm, main = "Normalized Infield Metric", 
      xlab = "Infield Metric")

## DH Metric -------------------------------------------------------------------
## Metrics to combine are OPS, and two out RBI per game

## First filter out all unneccesary columns 
DH_metric <- DH_OPS %>% 
        ungroup() %>%
        select(player_id, first_name, last_name, avg_OPS_std) %>% 
        arrange(desc(avg_OPS_std))
DH_metric
summary(DH_metric$avg_OPS_std)

## Filter columns for error rate as well
temp <- DH_torbi %>% 
          ungroup() %>% 
          select(player_id, torbi_std)
        
DH_metric <- DH_metric %>% left_join(temp)

## Review relation between two metrics
DH_lm <- lm(data = DH_metric, avg_OPS_std ~ torbi_std)
DH_coeff <- coefficients(DH_lm)
plot(DH_metric$avg_OPS_std, DH_metric$torbi_std, main = "OPS vs TORBI")
abline(DH_coeff)
DH_corr <- cor(DH_metric$avg_OPS_std, DH_metric$torbi_std)
## correlation is very weak with R = -0.032
    
## Create normalized single score
DH_metric <- DH_metric %>% 
        mutate(DH_metric = .7 * avg_OPS_std + .3 * torbi_std) %>% 
        arrange(desc(DH_metric))

max_DH_metric <- max(DH_metric$DH_metric)
min_DH_metric <- min(DH_metric$DH_metric)

DH_metric <- DH_metric %>% 
        mutate(DH_metric_norm = (abs(min_DH_metric) + DH_metric) / 
                       (abs(min_DH_metric) + max_DH_metric)) %>% 
        arrange(desc(DH_metric_norm))
## View results
DH_metric[1:10, ]
DH_metric[11:20, ]

summary(DH_metric$DH_metric_norm)
qplot(DH_metric$DH_metric_norm, main = "Normalized Designated Hitter Metric", 
      xlab = "Designated Hitter Metric")

## LF Metric --------------------------------------------------------------------
## Metrics to combine are OPS, error rate, and outfield assist rate

## First filter out all unneccesary columns 
LF_metric <- LF_OPS %>% 
        ungroup() %>%
        select(player_id, first_name, last_name, avg_OPS_std) %>% 
        arrange(desc(avg_OPS_std))
LF_metric
summary(LF_metric$avg_OPS_std)

## Filter columns for error rate as well
temp <- outfield_errors %>% 
          ungroup() %>% 
          select(player_id, err_pg_std)
        
LF_metric <- LF_metric %>% left_join(temp)

## Filter columns for outfield assist as well
temp <- outfield_oa %>% 
          ungroup() %>% 
          select(player_id, oa_std)
        
LF_metric <- LF_metric %>% left_join(temp)

## Review relation between two metrics
LF_lm <- lm(data = LF_metric, avg_OPS_std ~ err_pg_std + oa_std)
LF_coeff <- coefficients(LF_lm)
LFplot <- scatterplot3d(LF_metric$avg_OPS_std, LF_metric$err_pg_std, LF_metric$oa_std,
              main = "Left Fielder Metrics Plot")
LFplot$plane3d(LF_lm, col = "red")
plot3d(LF_metric$avg_OPS_std, LF_metric$err_pg_std, LF_metric$oa_std)

LF_corr <- cor(LF_metric[ , c(4:6)])
corrplot(LF_corr, type = "upper", method = "number")
    
## Create normalized single score
LF_metric <- LF_metric %>% 
        mutate(LF_metric = .5 * avg_OPS_std - .3 * err_pg_std + .2 * oa_std) %>% 
        arrange(desc(LF_metric))

max_LF_metric <- max(LF_metric$LF_metric)
min_LF_metric <- min(LF_metric$LF_metric)

LF_metric <- LF_metric %>% 
        mutate(LF_metric_norm = (abs(min_LF_metric) + LF_metric) / 
                       (abs(min_LF_metric) + max_LF_metric)) %>% 
        arrange(desc(LF_metric_norm))
## View results
LF_metric[1:10, ]
LF_metric[11:20, ]

summary(LF_metric$LF_metric_norm)
qplot(LF_metric$LF_metric_norm, main = "Normalized Left Fielder Metric", 
      xlab = "LF Metric")

## CF Metric --------------------------------------------------------------------
## Metrics to combine are OPS, error rate, and outfield assist rate

## First filter out all unneccesary columns 
CF_metric <- CF_OPS %>% 
        ungroup() %>%
        select(player_id, first_name, last_name, avg_OPS_std) %>% 
        arrange(desc(avg_OPS_std))
CF_metric
summary(CF_metric$avg_OPS_std)

## Filter columns for error rate as well
temp <- outfield_errors %>% 
          ungroup() %>% 
          select(player_id, err_pg_std)
        
CF_metric <- CF_metric %>% left_join(temp)

## Filter columns for outfield assist as well
temp <- outfield_oa %>% 
          ungroup() %>% 
          select(player_id, oa_std)
        
CF_metric <- CF_metric %>% left_join(temp)

## Review relation between two metrics
CF_lm <- lm(data = CF_metric, avg_OPS_std ~ err_pg_std + oa_std)
CF_coeff <- coefficients(CF_lm)
CFplot <- scatterplot3d(CF_metric$avg_OPS_std, CF_metric$err_pg_std, CF_metric$oa_std,
              main = "Center Fielder Metrics Plot")
CFplot$plane3d(CF_lm, col = "red")
plot3d(CF_metric$avg_OPS_std, CF_metric$err_pg_std, CF_metric$oa_std)

CF_corr <- cor(CF_metric[ , c(4:6)])
corrplot(CF_corr, type = "upper", method = "number")
    
## Create normalized single score
CF_metric <- CF_metric %>% 
        mutate(CF_metric = .5 * avg_OPS_std - .3 * err_pg_std + .2 * oa_std) %>% 
        arrange(desc(CF_metric))

max_CF_metric <- max(CF_metric$CF_metric)
min_CF_metric <- min(CF_metric$CF_metric)

CF_metric <- CF_metric %>% 
        mutate(CF_metric_norm = (abs(min_CF_metric) + CF_metric) / 
                       (abs(min_CF_metric) + max_CF_metric)) %>% 
        arrange(desc(CF_metric_norm))
## View results
CF_metric[1:10, ]
CF_metric[11:20, ]

summary(CF_metric$CF_metric_norm)
qplot(CF_metric$CF_metric_norm, main = "Normalized Center Fielder Metric", 
      xlab = "CF Metric")

## RF Metric --------------------------------------------------------------------
## Metrics to combine are OPS, error rate, and outfield assist rate

## First filter out all unneccesary columns 
RF_metric <- RF_OPS %>% 
        ungroup() %>%
        select(player_id, first_name, last_name, avg_OPS_std) %>% 
        arrange(desc(avg_OPS_std))
RF_metric
summary(RF_metric$avg_OPS_std)

## Filter columns for error rate as well
temp <- outfield_errors %>% 
          ungroup() %>% 
          select(player_id, err_pg_std)
        
RF_metric <- RF_metric %>% left_join(temp)

## Filter columns for outfield assist as well
temp <- outfield_oa %>% 
          ungroup() %>% 
          select(player_id, oa_std)
        
RF_metric <- RF_metric %>% left_join(temp)

## Review relation between two metrics
RF_lm <- lm(data = RF_metric, avg_OPS_std ~ err_pg_std + oa_std)
RF_coeff <- coefficients(RF_lm)
RFplot <- scatterplot3d(RF_metric$avg_OPS_std, RF_metric$err_pg_std, RF_metric$oa_std,
              main = "Right Fielder Metrics Plot")
RFplot$plane3d(RF_lm, col = "red")
plot3d(RF_metric$avg_OPS_std, RF_metric$err_pg_std, RF_metric$oa_std)

RF_corr <- cor(RF_metric[ , c(4:6)])
corrplot(RF_corr, type = "upper", method = "number")
    
## Create normalized single score
RF_metric <- RF_metric %>% 
        mutate(RF_metric = .5 * avg_OPS_std - .3 * err_pg_std + .2 * oa_std) %>% 
        arrange(desc(RF_metric))

max_RF_metric <- max(RF_metric$RF_metric)
min_RF_metric <- min(RF_metric$RF_metric)

RF_metric <- RF_metric %>% 
        mutate(RF_metric_norm = (abs(min_RF_metric) + RF_metric) / 
                       (abs(min_RF_metric) + max_RF_metric)) %>% 
        arrange(desc(RF_metric_norm))
## View results
RF_metric[1:10, ]
RF_metric[11:20, ]

summary(RF_metric$RF_metric_norm)
qplot(RF_metric$RF_metric_norm, main = "Normalized Right Fielder Metric", 
      xlab = "RF Metric")

## OF Metric --------------------------------------------------------------------
## Metrics to combine are OPS, error rate, and outfield assist rate

## First filter out all unneccesary columns 
OF_metric <- OF_OPS %>% 
        ungroup() %>%
        select(player_id, first_name, last_name, avg_OPS_std) %>% 
        arrange(desc(avg_OPS_std))
OF_metric
summary(OF_metric$avg_OPS_std)

## Filter columns for error rate as well
temp <- outfield_errors %>% 
          ungroup() %>% 
          select(player_id, err_pg_std)
        
OF_metric <- OF_metric %>% left_join(temp)

## Filter columns for outfield assist as well
temp <- outfield_oa %>% 
          ungroup() %>% 
          select(player_id, oa_std)
        
OF_metric <- OF_metric %>% left_join(temp)

## Review relation between two metrics
OF_lm <- lm(data = OF_metric, avg_OPS_std ~ err_pg_std + oa_std)
OF_coeff <- coefficients(OF_lm)
OFplot <- scatterplot3d(OF_metric$avg_OPS_std, OF_metric$err_pg_std, OF_metric$oa_std,
              main = "Out Fielder Metrics Plot")
OFplot$plane3d(OF_lm, col = "red")
plot3d(OF_metric$avg_OPS_std, OF_metric$err_pg_std, OF_metric$oa_std)

OF_corr <- cor(OF_metric[ , c(4:6)])
corrplot(OF_corr, type = "upper", method = "number")
    
## Create normalized single score
OF_metric <- OF_metric %>% 
        mutate(OF_metric = .5 * avg_OPS_std - .3 * err_pg_std + .2 * oa_std) %>% 
        arrange(desc(OF_metric))

max_OF_metric <- max(OF_metric$OF_metric)
min_OF_metric <- min(OF_metric$OF_metric)

OF_metric <- OF_metric %>% 
        mutate(OF_metric_norm = (abs(min_OF_metric) + OF_metric) / 
                       (abs(min_OF_metric) + max_OF_metric)) %>% 
        arrange(desc(OF_metric_norm))
## View results
OF_metric[1:10, ]
OF_metric[11:20, ]

summary(OF_metric$OF_metric_norm)
qplot(OF_metric$OF_metric_norm, main = "Normalized Out Fielder Metric", 
      xlab = "OF Metric")

## Compare at team level -------------------------------------------------------
## Add players into team data frame to compare complete rosters against eachother

str(teams_df)
## create new df for rosters
team_roster <- select(teams_df, team_id, location, nickname)
team_roster[, c("C", "1B", "2B", "3B", "SS", "OF1", "OF2", "OF3", "OF4", "OF5", 
                "2B/SS", "1B/3B", "UTIL", "P1", "P2", "P3", "P4", "P5", "P6", 
                "P7", "P8", "P9")] = NA                    
head(team_roster)                      
## Join individual position dataframes into sinle player data frame                      
## use which.max function to fill each spot in teams_roster df
player_pool <- select(players_df, player_id, team_id, first_name, last_name, position_abbreviation)
## Add each metric to the player pool
## SP
temp <- select(SP_metric, player_id, SP_metric_norm)
player_pool <- player_pool %>% left_join(temp) 
summary(player_pool$SP_metric_norm)
table(player_pool$SP_metric_norm)
## RP
temp <- select(RP_metric, player_id, RP_metric_norm)
player_pool <- player_pool %>% left_join(temp) 
summary(player_pool$RP_metric_norm)
table(player_pool$RP_metric_norm)
## P
temp <- select(P_metric, player_id, P_metric_norm)
player_pool <- player_pool %>% left_join(temp) 
## c
temp <- select(C_metric, player_id, C_metric_norm)
player_pool <- player_pool %>% left_join(temp)
## first_B_metric
temp <- select(first_B_metric, player_id, first_B_metric_norm)
player_pool <- player_pool %>% left_join(temp)
## second_B_metric
temp <- select(second_B_metric, player_id, second_B_metric_norm)
player_pool <- player_pool %>% left_join(temp)
## third_B_metric
temp <- select(third_B_metric, player_id, third_B_metric_norm)
player_pool <- player_pool %>% left_join(temp)
## SS_metric
temp <- select(SS_metric, player_id, SS_metric_norm)
player_pool <- player_pool %>% left_join(temp)
## IF_metric
temp <- select(IF_metric, player_id, IF_metric_norm)
player_pool <- player_pool %>% left_join(temp)
## LF_metric
temp <- select(LF_metric, player_id, LF_metric_norm)
player_pool <- player_pool %>% left_join(temp)
## CF_metric
temp <- select(CF_metric, player_id, CF_metric_norm)
player_pool <- player_pool %>% left_join(temp)
## RF_metric
temp <- select(RF_metric, player_id, RF_metric_norm)
player_pool <- player_pool %>% left_join(temp)
## OF_metric
temp <- select(OF_metric, player_id, OF_metric_norm)
player_pool <- player_pool %>% left_join(temp)
## DH_metric
temp <- select(DH_metric, player_id, DH_metric_norm)
player_pool <- player_pool %>% left_join(temp)

## Create single metric column for each player
player_pool <- player_pool %>%
   replace(is.na(.), 0) %>%
   mutate(metric = rowSums(.[6:19]))

qplot(player_pool$metric[which(player_pool$metric > 0)], main = "All Player Metric Distribution")

## Populate each position for each team with the player with the highest metric
## Reorder team_roster by team_id first
team_roster <- arrange(team_roster, team_id)

## P1
P1 <- player_pool %>% 
        filter(position_abbreviation == "SP") %>% 
        group_by(team_id) %>% 
        slice(which.max(metric)) %>% 
        arrange(team_id)

team_roster$P1 <- P1$metric
## Remove players that have been used already from pool
player_pool <- player_pool[!(player_pool$player_id %in% P1$player_id), ]

## P2
P2 <- player_pool %>% 
        filter(position_abbreviation == "SP") %>% 
        group_by(team_id) %>% 
        slice(which.max(metric)) %>% 
        arrange(team_id)

team_roster$P2 <- P2$metric
## Remove players that have been used already from pool
player_pool <- player_pool[!(player_pool$player_id %in% P2$player_id), ]

## P3
P3 <- player_pool %>% 
        filter(position_abbreviation == "SP") %>% 
        group_by(team_id) %>% 
        slice(which.max(metric)) %>% 
        arrange(team_id)

team_roster$P3 <- P3$metric
## Remove players that have been used already from pool
player_pool <- player_pool[!(player_pool$player_id %in% P3$player_id), ]

## P4
P4 <- player_pool %>% 
        filter(position_abbreviation == "SP") %>% 
        group_by(team_id) %>% 
        slice(which.max(metric)) %>% 
        arrange(team_id)

team_roster$P4 <- P4$metric
## Remove players that have been used already from pool
player_pool <- player_pool[!(player_pool$player_id %in% P4$player_id), ]

## P5
P5 <- player_pool %>% 
        filter(position_abbreviation %in% c("SP", "RP", "P")) %>% 
        group_by(team_id) %>% 
        slice(which.max(metric)) %>% 
        arrange(team_id)

team_roster$P5 <- P5$metric
## Remove players that have been used already from pool
player_pool <- player_pool[!(player_pool$player_id %in% P5$player_id), ]

## P6
P6 <- player_pool %>% 
        filter(position_abbreviation %in% c("SP", "RP", "P")) %>% 
        group_by(team_id) %>% 
        slice(which.max(metric)) %>% 
        arrange(team_id)

team_roster$P6 <- P6$metric
## Remove players that have been used already from pool
player_pool <- player_pool[!(player_pool$player_id %in% P6$player_id), ]

## P7
P7 <- player_pool %>% 
        filter(position_abbreviation %in% c("SP", "RP", "P")) %>% 
        group_by(team_id) %>% 
        slice(which.max(metric)) %>% 
        arrange(team_id)

team_roster$P7 <- P7$metric
## Remove players that have been used already from pool
player_pool <- player_pool[!(player_pool$player_id %in% P7$player_id), ]

## P8
P8 <- player_pool %>% 
        filter(position_abbreviation %in% c("SP", "RP", "P")) %>% 
        group_by(team_id) %>% 
        slice(which.max(metric)) %>% 
        arrange(team_id)

team_roster$P8 <- P8$metric
## Remove players that have been used already from pool
player_pool <- player_pool[!(player_pool$player_id %in% P8$player_id), ]

## P9
P9 <- player_pool %>% 
        filter(position_abbreviation %in% c("SP", "RP", "P")) %>% 
        group_by(team_id) %>% 
        slice(which.max(metric)) %>% 
        arrange(team_id)

team_roster$P9 <- P9$metric
## Remove players that have been used already from pool
player_pool <- player_pool[!(player_pool$player_id %in% P9$player_id), ]

## C
C <- player_pool %>% 
        filter(position_abbreviation %in% c("C")) %>% 
        group_by(team_id) %>% 
        slice(which.max(metric)) %>% 
        arrange(team_id)

team_roster$C <- C$metric
## Remove players that have been used already from pool
player_pool <- player_pool[!(player_pool$player_id %in% C$player_id), ]

## first_B
first_B <- player_pool %>% 
        filter(position_abbreviation %in% c("1B")) %>% 
        group_by(team_id) %>% 
        slice(which.max(metric)) %>% 
        arrange(team_id)

team_roster$`1B` <- first_B$metric
## Remove players that have been used already from pool
player_pool <- player_pool[!(player_pool$player_id %in% first_B$player_id), ]

## second_B
second_B <- player_pool %>% 
        filter(position_abbreviation %in% c("2B")) %>% 
        group_by(team_id) %>% 
        slice(which.max(metric)) %>% 
        arrange(team_id)

team_roster$`2B` <- second_B$metric
## Remove players that have been used already from pool
player_pool <- player_pool[!(player_pool$player_id %in% second_B$player_id), ]
                      
## third_B
third_B <- player_pool %>% 
        filter(position_abbreviation %in% c("3B")) %>% 
        group_by(team_id) %>% 
        slice(which.max(metric)) %>% 
        arrange(team_id)

team_roster$`3B` <- third_B$metric
## Remove players that have been used already from pool
player_pool <- player_pool[!(player_pool$player_id %in% third_B$player_id), ] 

## SS
SS <- player_pool %>% 
        filter(position_abbreviation %in% c("SS")) %>% 
        group_by(team_id) %>% 
        slice(which.max(metric)) %>% 
        arrange(team_id)

team_roster$SS <- SS$metric
## Remove players that have been used already from pool
player_pool <- player_pool[!(player_pool$player_id %in% SS$player_id), ] 

## OF1
OF1 <- player_pool %>% 
        filter(position_abbreviation %in% c("LF")) %>% 
        group_by(team_id) %>% 
        slice(which.max(metric)) %>% 
        arrange(team_id)

team_roster$OF1 <- OF1$metric
## Remove players that have been used already from pool
player_pool <- player_pool[!(player_pool$player_id %in% OF1$player_id), ] 

## OF2
OF2 <- player_pool %>% 
        filter(position_abbreviation %in% c("CF")) %>% 
        group_by(team_id) %>% 
        slice(which.max(metric)) %>% 
        arrange(team_id)

team_roster$OF2 <- OF2$metric
## Remove players that have been used already from pool
player_pool <- player_pool[!(player_pool$player_id %in% OF2$player_id), ] 

## OF3
OF3 <- player_pool %>% 
        filter(position_abbreviation %in% c("RF")) %>% 
        group_by(team_id) %>% 
        slice(which.max(metric)) %>% 
        arrange(team_id)

team_roster$OF3 <- OF3$metric
## Remove players that have been used already from pool
player_pool <- player_pool[!(player_pool$player_id %in% OF3$player_id), ] 

## OF4
OF4 <- player_pool %>% 
        filter(position_abbreviation %in% c("RF", "LF", "CF", "OF")) %>% 
        group_by(team_id) %>% 
        slice(which.max(metric)) %>% 
        arrange(team_id)

team_roster$OF4 <- OF4$metric
## Remove players that have been used already from pool
player_pool <- player_pool[!(player_pool$player_id %in% OF4$player_id), ] 

## OF5
OF5 <- player_pool %>% 
        filter(position_abbreviation %in% c("RF", "LF", "CF", "OF")) %>% 
        group_by(team_id) %>% 
        slice(which.max(metric)) %>% 
        arrange(team_id)

team_roster$OF5 <- OF5$metric
## Remove players that have been used already from pool
player_pool <- player_pool[!(player_pool$player_id %in% OF5$player_id), ] 

## 2B/SS
SS_2B <- player_pool %>% 
        filter(position_abbreviation %in% c("2B", "SS")) %>% 
        group_by(team_id) %>% 
        slice(which.max(metric)) %>% 
        arrange(team_id)

team_roster$`2B/SS` <- SS_2B$metric
## Remove players that have been used already from pool
player_pool <- player_pool[!(player_pool$player_id %in% SS_2B$player_id), ] 

## 1B/3B
first_third_B <- player_pool %>% 
        filter(position_abbreviation %in% c("1B", "3B")) %>% 
        group_by(team_id) %>% 
        slice(which.max(metric)) %>% 
        arrange(team_id)

team_roster$`1B/3B` <- first_third_B$metric
## Remove players that have been used already from pool
player_pool <- player_pool[!(player_pool$player_id %in% first_third_B$player_id), ] 

## UTIL
UTIL <- player_pool %>% 
        filter(!(position_abbreviation %in% c("P", "SP", "RP"))) %>% 
        group_by(team_id) %>% 
        slice(which.max(metric)) %>% 
        arrange(team_id)

team_roster$UTIL <- UTIL$metric
## Remove players that have been used already from pool
player_pool <- player_pool[!(player_pool$player_id %in% UTIL$player_id), ] 

## Create single team metric
team_roster <- team_roster %>% mutate(team_metric = rowSums(.[4:25]) / 22)
summary(team_roster$team_metric)
qplot(team_roster$team_metric, breaks = seq(.4,.6,.02), main = "Team Metric Distribution")

## Create dataframe for team rankings
team_rankings <- team_roster %>% 
        arrange(desc(team_metric)) %>% 
        select(location, nickname, team_metric)
team_rankings <- mutate(team_rankings, Rank = seq.int(nrow(team_rankings)))

## Compare with actual 2016 data -----------------------------------------------
## Bring in actual 2016 rankings for comparison
actual_2016_ranks <- read.xlsx("Actual_2016_MLB_Rankings.xlsx", sheetIndex = 1)
str(actual_2016_ranks)
str(team_rankings)
actual_2016_ranks$nickname <- as.character(actual_2016_ranks$nickname)

team_rankings <- team_rankings %>% left_join(actual_2016_ranks) %>% 
        mutate(diff = Rank - actual_2016_rank, abs_diff = abs(Rank - actual_2016_rank))
team_rankings

avg_rank_diff <- sum(team_rankings$abs_diff) / 30
team_rankings$diff <- as.factor(team_rankings$diff)
team_rankings$actual_2016_rank <- as.integer(team_rankings$actual_2016_rank)

## Visualize team expected rank vs actaul rank
colourCount = length(unique(team_rankings$diff))

getPalette = colorRampPalette(c("red", "black", "green"))


ggplot(team_rankings, aes(x=actual_2016_rank, y=1, label=nickname, colour=diff)) + 
        geom_text(angle = 45, size = 7) + 
        scale_color_manual(values = getPalette(colourCount)) +
        theme(legend.position = "bottom", axis.text.y = element_blank(), 
              axis.ticks.y = element_blank()) + 
        guides(color = guide_legend(nrow = 1)) +
        ggtitle("MLB 2016 Team Standings Per Win/Loss Percent") +
        labs(x = "Actual 2016 Standings", y = "") +
        scale_x_discrete(limits = seq(1:30)) + coord_cartesian(xlim = c(0,31))

## Review results

## Test team metric for normality
ad.test(team_roster$team_metric)
## p-value is 0.3882 so the normal distribution is a fine approximation
descdist(team_roster$team_metric)
## Cullen and Frey graph shows it could be better approximated by lognormal dist
## becasue there is some skewness to the dist

## Results for Orioles
orioles_score <- team_rankings$team_metric[which(team_rankings$nickname == "Orioles")]
orioles_score

## Compare Orioles and league leaders Cubs
t.test(team_roster[30, 4:25], team_roster[9, 4:25])

## Compare Brewers and league leaders Cubs
t.test(team_roster[30, 4:25], team_roster[24, 4:25])

team_roster$team_metric
max_team_metric <- max(team_roster$team_metric)
min_team_metric <- min(team_roster$team_metric)
 
test <- team_roster %>% 
        mutate(team_metric_norm = (team_metric - min_team_metric) / 
                       (max_team_metric - min_team_metric)) %>% 
        arrange(desc(team_metric_norm))

ggplot(test, aes(x=team_metric_norm)) + geom_histogram(bins = 10)





