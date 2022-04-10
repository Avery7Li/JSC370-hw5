library(data.table)
library(dplyr)
library(leaflet)
library(ggplot2)
library(gridExtra)
library(grid)
library(gtable)
library(mgcv)
library(kableExtra)
library("GGally")
library(AICcmodavg)
library(stringr)
library(plotly)

# Helper functions
# Slice from end of the string
slice_back <- function(s, n) {
  sub = substr(s, nchar(s)-n+1, nchar(s))
  return(sub)
}
# Mode
fmode <- function(x) unique(x)[which.max(table(x))]

# Position order constant
position_levels <- c("Opposite Spiker", "Outside Hitter", "Middle Blocker",
                     "Setter", "Libero")

# Read in the player and team data
scorers <- data.table::fread("data/best-scorers.csv")
spikers <- data.table::fread("data/best-spikers.csv")
diggers <- data.table::fread("data/best-diggers.csv")
setters <- data.table::fread("data/best-setters.csv")
receivers <- data.table::fread("data/best-receivers.csv")
blockers <- data.table::fread("data/best-blockers.csv")

bio <- data.table::fread("data/player_bio.csv")
team_rank <- data.table::fread("data/team_rank.csv")
matches <- data.table::fread("data/round_robin.csv")

## bio
# Rename ambiguous columns before merging
setnames(bio, 'spike', 'spike_height')
setnames(bio, 'block', 'block_height')
setnames(bio, 'total', 'total_selections')

# Deal with outliers
bio[spike_height < 240]
nrow(bio[spike_height >= block_height]) / nrow(bio)
bio[, spike_height := fifelse(spike_height < 150, block_height, spike_height)]

# Only one player has a universal position, replace with the most occurred position
bio[position == 'Universal']
bio[, position := fifelse(position == 'Universal', fmode(position), position)]

# Make position name consistent
bio[, position := fifelse(position == 'Middle blocker', 
                          'Middle Blocker', 
                          fifelse(position == 'Opposite spiker',
                                  'Opposite Spiker', position))]

## Create numerical variables - bmi, world_game, age
bio[, bmi := round(weight / (height/100)**2, 1)]
bio[, world_selection := world_championships + olympic_games]
# Take the last four char from birth date and convert to int as birth year
bio[, birth_year := strtoi(slice_back(birthdate, 4))]
# Calculate age
bio[, age:= 2019 - birth_year]
# Create a indicator for captain: name has '\nc' at the end
bio[, is_captain := fifelse(slice_back(name, 2) == '\nc', 1, 0)]
# Remove char from end of name
bio[, name := gsub("\nc", "", name)]
# Keep only relevant columns

## scorers
# Rename columns
setnames(scorers, 'total', 'total_score')
setnames(scorers, 'rank', 'score_rank')
# Drop column that's not interesting
scorers <- scorers[, !"shirtnumber"]

# Merge player scores with bio
# look for association between players' individual scores and their ability
players <- merge(x = bio[, .(name, team, position, age, height, weight, bmi,
                             spike_height, block_height,
                             total_selections, is_captain)], 
                 y = scorers, 
                 all.x = T, all.y = F, 
                 by=c("name", 'team'))

## Best players
# Rename
setnames(diggers, 'average_per_set', 'digs_per_set')
setnames(setters, 'average_per_set', 'sets_per_set')
setnames(spikers, 'success_%', 'attack_success_rate')
# Merge with player dataset
players <- merge(x = players, 
                 y = diggers[, .(name, digs_per_set)],
                 all.x = T, all.y = F, 
                 by="name")
players <- merge(x = players, 
                 y = setters[, .(name, sets_per_set)], 
                 all.x = T, all.y = F, 
                 by="name")
players <- merge(x = players, 
                 y = spikers[, .(name, attack_success_rate)], 
                 all.x = T, all.y = F, 
                 by="name")
# Check for NAs
players[is.na(digs_per_set) & is.na(sets_per_set) & 
          is.na(attack_success_rate) & is.na(total_score), .N]
players <- players[!(is.na(digs_per_set) & is.na(sets_per_set) & 
                       is.na(attack_success_rate) & is.na(total_score))]
players[is.na(total_score), .N]   # 43

# Impute missing values
players <- players %>%
  group_by(position) %>%
  mutate(
    total_score = coalesce(total_score, round(mean(total_score, na.rm=TRUE))),
    digs_per_set = coalesce(digs_per_set, round(mean(digs_per_set, na.rm=TRUE))),
    sets_per_set = coalesce(sets_per_set, round(mean(sets_per_set, na.rm=TRUE)))
    #attack_success_rate = coalesce(attack_success_rate,
    #                               round(mean(attack_success_rate, na.rm=TRUE)))
  )
# Get team full name
players <- merge(x=players,
                 y=team_rank[, .(team, team_full)], by='team')
players <-data.table(players)

## Team
# Rename team rank
setnames(team_rank, 'rank', 'team_rank')
# Merge team info together with player info
team_players <- merge(x=team_rank[, .(team, team_rank, match_win, 
                                      set_ratio, team_full)], 
                      y=players[, .(name, team, age, position, height, 
                                    total_selections, total_score,
                                    digs_per_set, sets_per_set, 
                                    attack_success_rate)],
                      
                      all.x = T, all.y = T, 
                      by='team')


## Top players
player_rank <- rbind(spikers[, .(name, rank, skill = "Attack", value=attack_success_rate)],
                  setters[, .(name, rank, skill = "Set", value=sets_per_set)],
                  diggers[, .(name, rank, skill = "Dig", value=digs_per_set)],
                  #receivers[, .(name, rank, skill = "Receive", value=`efficiency_%`)],
                  blockers[, .(name, rank, skill = "Block", value=average_per_set)])
player_rank <- merge(x=player_rank, 
                  y=bio[, .(team, name, position, is_captain)], 
                  all.x=T, all.y=F, by='name')
player_rank <- merge(x=player_rank, 
                  y=team_rank[, .(team_rank, team_full, team)], 
                  all.x=T, all.y=F, by='team')

## Round robin match result
matches[, team_home:=sapply(strsplit(as.character(matches$teams),'-'), "[", 1)]
matches[, team_away:=sapply(strsplit(as.character(matches$teams),'-'), "[", 2)]
matches[, set_home:=sapply(strsplit(as.character(matches$sets),'-'), "[", 1)]
matches[, set_away:=sapply(strsplit(as.character(matches$sets),'-'), "[", 2)]
# Calculate the number of sets won by each team in a match
matches<-transform(matches, set_home = as.numeric(set_home), 
                   set_away = as.numeric(set_away))
matches[, set_home_win:=set_home-set_away]
matches[, set_away_win:=set_away-set_home]

match_each_team <- rbind(matches[, .(team=team_home, opponent=team_away, 
                                     sets_win=set_home_win, sets=sets, date=date)],
                         matches[, .(team=team_away, opponent=team_home, 
                                     sets_win=set_away_win, sets=sets, date=date)])
# Order based on team name and date
match_each_team<-match_each_team[order(team, date)]
# Add a column of index for each team
match_each_team$index <- rep(1:15, times=16)

## Teams in order
team_abr_in_order <- team_rank[, team]
team_in_order <- team_rank[, team_full]

