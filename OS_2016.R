## Finding undervalued QBs and receivers based on outliers to the historical
## realtionships between QB and receiver ADP

library(data.table)
library(dplyr)
library(ggplot2)
library(readr)
library(stringr)

# Use Armchair Analysis data and historical ADP scraped from myfantasyleague.com
# Build historical trendline of QB/receiver ADP relationship based on past data

#Get Armchair Analysis data for rushing yards & RB receiving adjustments
offense <- read_csv("~/Desktop/AA/aa_2000_2015/csv/OFFENSE.csv")

#Add position to Armchair Analysis data
pos <- player %>% select(player, pos1)

off <- left_join(offense, pos, by="player")

# Need to adjust receiver ADP for RB receiving & QB rushing
# Get RB receiving data for the past 5 years

rb_rec <- off %>% 
    filter(year %in% 2010:2015) %>% 
    select(-(c(player, uid, gid, posd))) %>% 
    group_by(year, team, pos1) %>% 
    summarise_each(funs(sum))

rb <- rb_rec %>% 
    filter(pos1 == "RB") %>% 
    # calculate RB fantasy points from receiving
    mutate(rbfp = recy/20 + tdrec*4) %>% 
    select(year, team, rbfp)

wr_te <- rb_rec %>% 
    filter(pos1 %in% c("TE", "WR")) %>% 
    group_by(team, year) %>% 
    select(-pos1) %>% 
    summarise_each(funs(sum)) %>% 
    # total wr & te fantasy points from receiving
    mutate(wrtefp = recy/20 + tdrec*4) %>% 
    select(year, team, wrtefp)

rb_rec <- left_join(wr_te, rb, by=c("year", "team"))

rb_rec <- rb_rec %>% 
    # percentage of receiving FPs to RB of total
    mutate(perrb = rbfp/(wrtefp + rbfp), year1 = year + 1) %>% 
    select(year1, team, perrb) %>%
    setnames("year1", "year")
    
#Get QB rushing data for the past 5 years

qb_rush <- off %>% 
    filter(year %in% 2010:2015) %>% 
    select(-(c(player, uid, gid, posd))) %>% 
    group_by(year, team, pos1) %>% 
    summarise_each(funs(sum)) %>% 
    filter(pos1 == "QB") %>% 
    # calculate QB fantasy points from rushing
    mutate(qbrufp = ry/10 + tdr*6, qbpafp = py/20 + tdp*4 - ints*2, 
           qbruper = qbrufp/(qbrufp + qbpafp), year1 = year + 1) %>%
    ungroup() %>%
    select(year1, team, qbruper) %>%
    setnames("year1", "year")

adj <- left_join(qb_rush, rb_rec, by = c("year", "team"))

#Change team abbreviations to match MFL10 ADP Data

adj$team <- adj$team %>% 
    str_replace_all(fixed("GB"), "GBP") %>% 
    str_replace_all(fixed("KC"), "KCC") %>% 
    str_replace_all(fixed("NE"), "NEP") %>% 
    str_replace_all(fixed("NO"), "NOS") %>% 
    str_replace_all(fixed("SD"), "SDC") %>% 
    str_replace_all(fixed("SF"), "SFO") %>% 
    str_replace_all(fixed("TB"), "TBB")

#Get adp data for last five years (stored as adp_ranks.csv)

adp_ranks <- read_csv("adp_ranks_2015.csv")

qb_wr_te <- adp_ranks %>% 
            filter(year %in% 2011:2015, pos %in% c("QB", "WR", "TE")) %>% 
            arrange(desc(year), team, avg_pick)

#Separate by qb and receivers
qb <- filter(qb_wr_te, pos == "QB", team != "FA")

wr_te <- filter(qb_wr_te, pos %in% c("WR", "TE"), team != "FA", avg_pick <= 240)

#Rank QBs in order to only use ADP data for No.1 QB on each team
qb <- qb %>% 
    group_by(year, team) %>%
    mutate(rank = rank(avg_pick)) %>%
    filter(rank == 1) %>% 
    group_by(team, year) %>% 
    summarise(qb_avg = mean(avg_pick))

#Calculate receiver value by subtracting WR & TE ADPs from 240, 
# then sum by team and year

wr_te <- wr_te %>% 
    mutate(value = 240 - avg_pick) %>% 
    group_by(team, year) %>% 
    summarise(value = sum(value))

team <- left_join(qb, wr_te, by = c("year", "team")) %>%
    setnames("team", "team") %>%
    left_join(adj, by = c("year", "team")) %>%
    ungroup() %>%
    mutate(adj_value = (value/(1 - perrb))/(1 - qbruper))

#ADP data from the Best Ball ADP App (change name to most recent file)
total_draft <- read_csv("~/Downloads/best_ball_adp_rotoviz (43).csv")

# Transform name, pos, etc into same format as ADP data
total_draft$NAME <- (str_replace_all(total_draft$NAME, fixed(" (R)"), ""))

total_draft$NAME <- (str_replace_all(total_draft$NAME, fixed("*"), ""))

x <- strsplit(total_draft$NAME, ", ")
x <- do.call(rbind, x)
colnames(x) <- c("last", "first")
total_draft <- cbind(total_draft, x)

total_draft$first <- as.character(total_draft$first)

total_draft$pos <- str_sub(total_draft$first, -3, -1)

str_sub(total_draft$first, -3, -1) <- ""

total_draft$first <- str_trim(total_draft$first, side = "both")

total_draft$team <- str_sub(total_draft$first, -3, -1)

str_sub(total_draft$first, -3, -1) <- ""

total_draft$first <- str_trim(total_draft$first, side = "both")

total_draft$pos <- str_trim(total_draft$pos, side = "both")

total_draft$team <- str_trim(total_draft$team, side = "both")

## Combining First & Last Name
total_draft$name <- paste(total_draft$first, total_draft$last, sep=" ")

adp_2016 <- total_draft %>%  
    group_by(team, name, pos, ADP) %>% 
    summarise(avg_pick = mean(ADP)) %>% 
    arrange(team, avg_pick) %>%
    ungroup()

## Adding ADPs for teams without a drafted QB (2016 no ADP for CLE, RAM, DEN)

adp_2016[242,] <- c("CLE", "CLE QB", "QB", 240, 240)
adp_2016[243,] <- c("RAM", "STL QB", "QB", 240, 240)
adp_2016[244,] <- c("DEN", "DEN QB", "QB", 240, 240)

adp_2016$ADP <- as.numeric(adp_2016$ADP)
adp_2016$avg_pick <- as.numeric(adp_2016$avg_pick)

# Separate 2016 ADP for QB and receivers

qb_wr_te <- adp_2016 %>% 
    filter(pos %in% c("QB", "WR", "TE")) %>% 
    arrange(team, avg_pick)

qb <- filter(qb_wr_te, pos == "QB")

#Set Drafted minimum filter to screen out infrequently drafted WRs & TEs
wr_te <- qb_wr_te %>%
    filter(pos %in% c("WR","TE"), team != "FA", avg_pick <= 240) %>% 
    # Cut TE weighting in 1/3 versus WR
    mutate(value = ifelse(pos == "WR", 240 - avg_pick, (240 - avg_pick)/3))

#Rank QB to only use top QB per team
qb <- qb %>% 
    group_by(team) %>% 
    mutate(rank = rank(avg_pick, ties.method = "first"))

# Total receiver ADP values combined
wr_te <- wr_te %>% 
    group_by(team) %>% 
    summarise(value = sum(value))

# ADP for QB
qb <- qb %>% 
    filter(rank == 1) %>% 
    group_by(team, name) %>% 
    summarise(qb_avg = mean(avg_pick))

# Combine QB and receiver ADPs
team_16 <- left_join(qb, wr_te, by = "team")

team_16$year <- 2016

adj$team <- str_replace_all(adj$team, fixed("STL"), "RAM")

# Combine MFL10 ADP and historical ADP
team_16 <- left_join(team_16, adj, by = c("year", "team"))

#Gross up receiver value calculation to account for previous year's QB rushing & passing to RBs
team_16 <- team_16 %>% 
    mutate(adj_value = (value/(1 - perrb))/(1 - qbruper))

# Change names back

team_16$team <- team_16$team %>% 
    str_replace_all(fixed("GBP"), "GB") %>% 
    str_replace_all(fixed("KCC"), "KC") %>% 
    str_replace_all(fixed("NEP"), "NE") %>% 
    str_replace_all(fixed("NOS"), "NO") %>% 
    str_replace_all(fixed("SDC"), "SD") %>% 
    str_replace_all(fixed("SFO"), "SF") %>% 
    str_replace_all(fixed("TBB"), "TB")

# Graph QB/Receiver relationship with trendline
qb_rec_plot <- ggplot(team_16, aes(qb_avg, adj_value, label = team))

qb_rec_plot +  
    geom_point(color = "blue3", alpha = 1) + 
    geom_smooth(color = "blue3", linetype = 4, method = lm) + 
    fte_theme() + labs(x="Quarterback ADP", 
                       y="Combined Receiver Value (Based on ADP)",
                       title="QB/Receiver ADP Relationship (2016)") +
    geom_text(size = 4, hjust = -0.15, angle = 45) +
    coord_cartesian(ylim = c(0,1050))

ggsave("qb_rec_adp_2016.png")

#Linear regression & opportunity score calculations
lm_team_16 <- lm(adj_value ~ qb_avg, data = team_16)

team_16$est<-predict(lm_team_16,newdata=team_16)

team_16 <- mutate(team_16, os = est - adj_value)

#Reorder team names by opportunity score for graph
team_16$team2 <- reorder(team_16$team, desc(team_16$os))

os_16_plot <- ggplot(team_16, aes(team, os))
os_16_plot + geom_bar(aes(x=team2), stat = "identity", fill = "blue3") + 
    labs(x=NULL, y=NULL, title="Rookie Wide Receiver Opportunity Scores (Pre-Draft 2016)") + 
    scale_x_discrete(breaks=NULL) + 
    geom_text(aes(label=team2), size = 3.4, vjust = ifelse(team_16$os >= 0, -0.3, 1.3)) +
    fte_theme()

ggsave("rook_wr_os_2016_3.png")

