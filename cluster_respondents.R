# ---- setup

library(cluster)
library(factoextra)
library(proxy)
library(d3heatmap)

# read in data -------------

source("read_in_clean_data.R")
source("utility_functions.R")

# look at similarity between vars --------

measures_df <- responses[,which(colnames(responses) == "1_official_statement") : ncol(responses)]

# here, vars are actually not treated as ordinal (because of t()), but does not matter too much
gower_dist_vars <- daisy(as.data.frame(t(measures_df)))
gower_sim_vars <- abs(as.matrix(gower_dist_vars)-1)
diag(gower_sim_vars) <- NA
d3heatmap(gower_sim_vars)

# look at similarity between respondents --------

gower_dist_respondents <- daisy(measures_df)
gower_sim_respondents <- abs(as.matrix(gower_dist_respondents)-1)
diag(gower_sim_respondents) <- NA
d3heatmap(gower_sim_respondents,
          labRow = rep("",nrow(gower_sim_respondents)),
          labCol = rep("",ncol(gower_sim_respondents)))

hist(gower_sim_respondents)

# ======= k-medoids ========

# ------- silhouettes and wss ---------

distmat <- as.matrix(gower_dist_respondents)

# slightly faster pam function
pam1 <- function(x,k) list(cluster = pam(as.dist(x),k, diss = T, cluster.only=TRUE))

fviz_nbclust(x = as.matrix(distmat),
             FUNcluster = pam1,
             method = "silhouette", diss = distmat,
             k.max = 15)

fviz_nbclust(x = as.matrix(distmat),
             FUNcluster = pam1,
             method = "wss", diss = distmat,
             k.max = 15)

# ---------- gap stat -------------

# takes longer to run
gapstat <- clusGap(x = as.matrix(distmat),
                   FUNcluster = pam1,
                   B = 100, K.max = 6) # 500 for proper

fviz_gap_stat(gapstat, maxSE = list(method = "firstSEmax"), linecolor = "red")
fviz_gap_stat(gapstat, maxSE = list(method = "Tibs2001SEmax")) #most recent stat
fviz_gap_stat(gapstat, maxSE = list(method = "firstmax"), linecolor = "green")

# lets go with 3 -----

cluster_solution <- pam(x = distmat, 3,diss = TRUE)

responses$cluster <- factor(cluster_solution$clustering)

# look at clusters -----

table(responses$cluster, responses$position)

# visualize in another small multiples plot

# smby_cluster multiples for instrument preferences
plot_list_by_cluster <- lapply(colnames(responses)[!(colnames(responses) %in% 
                                                       c("position","cluster","ever_avoid"))],
                        plot_var_dist_reduced, fill_var = "cluster")

plots_by_cluster <- 
  cowplot::plot_grid(plotlist = plot_list_by_cluster,
                     nrow = round(sqrt(length(plot_list_by_cluster))),
                     ncol = round(sqrt(length(plot_list_by_cluster))))
cowplot::plot_grid(plots_by_cluster, color_legend, rel_heights = c(5,0.5), ncol = 1)

position_plot_by_cluster <- plot_var_dist("position",fill_var = "cluster", percentage = TRUE)
position_plot_by_cluster <- position_plot_by_cluster + ggtitle("Cluster distribution \n among positions", subtitle = "")
position_plot_by_cluster
ggsave("Viz_outputs/position_by_cluster.png")

by_cluster_plot <-
  cowplot::plot_grid(plotlist = plot_list_by_cluster,
                     nrow = round(sqrt(length(plot_list_by_cluster))),
                     ncol = round(sqrt(length(plot_list_by_cluster))))
by_cluster_with_dist <- 
  cowplot::plot_grid(by_cluster_plot, position_plot_by_cluster, rel_widths = c(5,1), ncol = 2)
by_cluster_with_dist
ggsave("Viz_outputs/var_dist_by_cluster.png", width = 18, height = 8)
