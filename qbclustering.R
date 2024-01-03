library(nflreadr)
library(nflfastR)
library(dplyr)
library(factoextra)
library(tidyverse)
library(cluster)
library(gt)
library(gtExtras)
library(nflplotR)

pass_pfr <- load_pfr_advstats(2019:2023, "pass", "season")
rush_pfr <- load_pfr_advstats(2019:2023, "rush", "season")
pass_ngs <- load_nextgen_stats(2019:2023, "passing")

pass_1 <- pass_pfr %>%
  filter(pass_attempts >= 250)

pass_1$player[which(pass_1$player == "Gardner Minshew II")] <- "Gardner Minshew"

pass_2 <- pass_ngs %>%
  filter(week == 0)

pass_stats <- left_join(pass_1, pass_2, by = c("season", "player"="player_display_name")) %>%
  mutate(rpo_pct = rpo_pass_att/pass_attempts, pa_pct = pa_pass_att/pass_attempts) %>%
  select(season, player, team, pass_attempts, pocket_time, rpo_pct, pa_pct, time = avg_time_to_throw, aggressiveness, air_yds_sticks = avg_air_yards_to_sticks)

rush_pfr$player[which(rush_pfr$player == "CJ Stroud")] <- "C.J. Stroud"
rush_pfr$player[which(rush_pfr$player == "Aidan OConnell")] <- "Aidan O'Connell"

rush_stats <- rush_pfr %>%
  select(season, player, rush_attempts = att)

stats <- left_join(pass_stats, rush_stats, by = c("season", "player"))

stats <- stats %>% filter(!is.na(pocket_time))

scaled <- scale(stats[,c(4:11)])

fviz_nbclust(scaled, kmeans, method = "silhouette", k.max=15)

set.seed(0)
kmeans_qbs <- kmeans(scaled, centers = 4, nstart = 25, iter.max = 20)
kmeans_centers <- as.data.frame(kmeans_qbs$centers)

kmeans_centers$cluster <- c('Cluster 1', 'Cluster 2', 'Cluster 3', 'Cluster 4')

kmeans_centers <- kmeans_centers %>%
  pivot_longer(!cluster, names_to = 'statname', values_to = 'statvalue')

kmeans_centers %>%
  ggplot(aes(x=statname, y=statvalue, color=cluster)) +
  geom_point() +
  facet_wrap(~ cluster, ncol = 2) +
  labs(x = "Statistic Predictor", y = "Scaled Statistical Value Center Per Cluster",
       title = "Cluster Compositions for NFL QBs (250+ Pass Attempts)") +
  theme(legend.position = "none", strip.text = element_text(face='bold'),
        axis.text.x = element_text(angle=90, size=8), 
        panel.grid.minor = element_blank(), plot.title = element_text(size=14, hjust=0.5, face="bold"), axis.title = element_text(face="bold"))

pca_qbs <- prcomp(scaled)
pca_summary <- summary(pca_qbs)

twopcas <- as.data.frame(pca_qbs$x[,1:2])
twopcas$cluster <- as.factor(kmeans_qbs$cluster)
variance_1 <- 100 *round(pca_summary$importance[2,1], 4) 
variance_2 <- 100 *round(pca_summary$importance[2,2], 4) 

twopcas %>%
  ggplot(aes(x=PC1, y=PC2, color= cluster)) + 
  geom_point(alpha=0.3) + 
  stat_ellipse(level=(2/3)) + 
  labs(x = paste0('PC1 (Accounts for ', variance_1, '% of Variance)'), 
       y = paste0('PC2 (Accounts for ', variance_2, '% of Variance)'), 
       title = 'K-Means Cluster Differences for NFL QBs in the 2022 Season (100+ Pass Attempts)') +
  theme(plot.title = element_text(size=14, hjust=0.5, face="bold"), axis.title = element_text(face="bold"))

name_cluster <- data.frame(player = stats$player, season = stats$season, team = stats$team, cluster=kmeans_qbs$cluster)

name_cluster <- name_cluster %>%
  mutate(adj = ifelse(cluster == 1, "Quick/Short Passer", ifelse(cluster == 2, "Dual-Threat", ifelse(cluster == 3, "Aggressive/Low-Usage", "Composed/High-Usage"))))

unique_players <- unique(name_cluster$player)

total_data <- expand.grid(player = unique_players, season = 2019:2023)

data <- left_join(total_data, name_cluster, by = c("season", "player"))

data$adj[which(is.na(data$adj))] <- "DNP/DNQ"
data$adj[which(data$player == "Baker Mayfield" & data$season == 2022)] <- "LACK OF DATA" # said to avoid confusion (also dobbs did not have any qualified seasons before this year so his table is not included anyway)

data <- data %>%
  mutate(adj = as.factor(adj))

logos <- teams_colors_logos %>%
  select(team = team_abbr, team_logo_espn)

data <- left_join(data, logos, by = "team")

for (qb in unique_players) {
  indiv_data <- data %>%
    filter(player == qb) %>%
    select(-player, -cluster, -team)
  
  table <- indiv_data %>% gt() %>%
    gt_img_rows(columns = team_logo_espn) %>%
    gt_nfl_headshots() %>%
    gt_theme_538() %>%
    cols_align(
      align = "center",
      columns = c(season, team_logo_espn, adj)
    ) %>%
    data_color(
      columns = adj,
      method = "factor",
      palette = "viridis"
    ) %>%
    cols_label(
      season = md("**Season**"),
      team_logo_espn = md("**Team**"),
      adj = md("**QB Style**")
    ) %>%
    tab_header(
      title = md(paste0("**", qb, "**")),
      subtitle = "QB Style From 2019 to 2023 Using K-Means Clustering"
    ) 
  
  gtsave(table, file.path(subfolder_path, paste0(qb, ".png")))
}
