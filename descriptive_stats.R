
# setup ----

source("read_in_clean_data.R")
source("utility_functions.R")

library(plotly)
library(ggplot2)
library(GGally)

# first overview stats ----

# NA distribution across vars

na_dist <- data.frame(var = colnames(responses),
                      na_count = colSums(is.na(responses)))

ggplot(na_dist,aes(x = var, y = na_count)) +
  geom_point() +
  geom_segment(aes(x=var, 
                   xend=var, 
                   y=0, 
                   yend=na_count)) + 
  labs(title="NA distribution", 
       subtitle="Missing answers among variable") + 
  theme(axis.text.x = element_text(angle=90, vjust=0.6, hjust = 1))
ggsave("Viz_outputs/na_dist.png")

table(responses$position)

table(responses$ever_avoid)

colnames(responses)

responses$`9a_internal_cap`

# small multiples for instrument preferences
plot_list_instruments <- lapply(colnames(responses)[which(colnames(responses) == "1_official_statement") : ncol(responses)],
                    plot_var_dist_reduced)

position_plot <- lapply("position",plot_var_dist)

color_legend <- cowplot::get_legend(position_plot[[1]] + 
                                      guides(color = guide_legend(nrow = 1)) +
                                      theme(legend.position = "bottom"))

instruments_plot <-
  cowplot::plot_grid(plotlist = plot_list_instruments,
                     nrow = round(sqrt(length(plot_list_instruments))) + 1,
                     ncol = round(sqrt(length(plot_list_instruments))))
cowplot::plot_grid(instruments_plot, color_legend, rel_heights = c(5,0.5), ncol = 1)
ggsave("Viz_outputs/instrument_prefs_small_multiples.png", width = 18, height = 8)

# small multiples of all vars (except ever avoid)

plot_list_all <- lapply(colnames(responses)[!(colnames(responses) %in% c("ever_avoid"))],
                          plot_var_dist_reduced)

plots_all <- 
  cowplot::plot_grid(plotlist = plot_list_all,
                     nrow = round(sqrt(length(plot_list_all))),
                     ncol = round(sqrt(length(plot_list_all))))
cowplot::plot_grid(plots_all, color_legend, rel_heights = c(5,0.5), ncol = 1)

ggsave("Viz_outputs/all_all_small_multiples.png", width = 18, height = 8)

# differences between answer distributions among positions ----

measures_df <- responses[,which(colnames(responses) == "1_official_statement") : ncol(responses)]
measures_df$positions <- responses$position
table(measures_df$positions)
# remove BA students and others to only have largest groups
measures_df <- measures_df[!(measures_df$positions %in% c("Bachelor / Master Student","Other / Keine der Bezeichnungen trifft zu")),]

# get medians per group
get_ordered_median <- function(x){
  x <- x[!(is.na(x))]
  levs <- levels(x)
  m <- median(as.integer(x))
  if(floor(m) != m)
  {warning("Median is between two values; using the first one")
    m <- floor(m)}
  ordered(m, labels = levs, levels = seq_along(levs))
  }

overall_medians <- unlist(lapply(measures_df[,colnames(measures_df) != "positions"],get_ordered_median))

overall_medians_df <- data.frame(var = names(overall_medians),
                         medians = overall_medians,
                         group = "all")

medians_per_group <- 
  lapply(unique(measures_df$positions), function(position){
    df <- measures_df[measures_df$positions == position,]
    df <- df[,colnames(df) != "positions"]
    data.frame(var = names(unlist(lapply(df,
                                   get_ordered_median))),
               medians = unlist(lapply(df,
                                       get_ordered_median)),
               group = position)
  })

medians_df <- do.call("rbind",medians_per_group)
medians_df$var <- rename_based_on_codebook(medians_df$var,var_codebook,
                         "var_code","var_short_text")
medians_plot <- 
  ggplot(medians_df, aes(x = var, y = medians, group = group, color = group)) + 
    geom_line(size = 2, alpha = 0.6,position = position_dodge(width = 0.5)) + 
    scale_color_brewer(palette = "Dark2", type = "div") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle=45, vjust=1, hjust = 1))
medians_plot

ggsave("Viz_outputs/medians_per_group.png", width = 18, height = 8)

overall_medians_df$var <- rename_based_on_codebook(overall_medians_df$var,var_codebook,
                                           "var_code","var_short_text")
overall_medians_plot <- 
  ggplot(overall_medians_df, aes(x = var, y = overall_medians, group = group, color = group)) + 
  geom_line(size = 2, alpha = 0.6,position = position_dodge(width = 0.5)) + 
  scale_color_brewer(palette = "Dark2", type = "div") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust = 1))
overall_medians_plot

ggsave("Viz_outputs/overall_medians.png", width = 18, height = 8)

# comments distribution ----

comments <- responses_with_comments[,grepl(pattern = "comment",colnames(responses_with_comments))]

colSums(!(is.na(comments)))
comments_per_person <- data.frame(ncomments = rowSums(!(is.na(comments))),
                                  position = responses_with_comments$position)

ggplot(comments_per_person, aes(y = ncomments, x = position, color = position)) +
  geom_boxplot(alpha = 0.8) + geom_jitter(width = 0.2, height = 0.2, alpha = 0.7) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust = 1)) + 
  ylab("Number of comments per person")

ggsave("Viz_outputs/comments_distribution.png", width = 12, height = 12)
