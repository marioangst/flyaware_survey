
# setup ----

source("read_in_clean_data.R")
source("utility_functions.R")

library(plotly)
library(ggplot2)
library(GGally)
library(grid)
library(gridExtra)

# first overview stats ----

# NA distribution across vars

na_dist <- data.frame(var = colnames(responses),
                      na_count = colSums(is.na(responses)))
na_dist$var <- rename_based_on_codebook(na_dist$var, var_codebook,
                                        "var_code","var_short_text")

na_dist_plot <- ggplot(na_dist,aes(x = var, y = na_count)) +
  geom_point(color = ifelse(na_dist$na_count == 0, "red","black")) +
  geom_segment(aes(x=var, 
                   xend=var, 
                   y=0, 
                   yend=na_count)) + 
  labs(title="Nonresponse distribution", 
       subtitle="Distribution of nonresponse across variables") + 
  xlab("Variable") + ylab("Nonresponse count") +
  theme(axis.text.x = element_text(angle=90, vjust=0.6, hjust = 1))

na_dist_plot

ggsave("Viz_outputs/na_dist.png")

table(responses$position)

table(responses$ever_avoid)

colnames(responses)

responses$`9a_internal_cap`

# small multiples for instrument preferences

plot_list_instruments <- lapply(colnames(responses)[which(colnames(responses) == "1_official_statement") : 
                                                      ncol(responses)],
                                plot_var_dist_reduced,
                                y_limit = max_n)

example_df <- data.frame(var = factor( 
                           sample(c("Strongly \n agree",
                                 "Agree",
                                 "Neither agree \n nor disagree",
                                 "Disagree",
                                 "Strongly \n disagree"),
                                 nrow(responses),
                                 replace = TRUE),ordered = TRUE, 
                           levels = c("Strongly \n agree",
                                      "Agree",
                                      "Neither agree \n nor disagree",
                                      "Disagree",
                                      "Strongly \n disagree")),
                         position = responses$position)

reading_example  <- 
  ggplot(example_df, aes(x = var)) + geom_bar(fill = "gray") + ylim(0,max_n) +
  labs(title=paste("Reading example")) + xlab("") + ylab("") +
  geom_text(data = data.frame(levels = levels(example_df$var)),
            mapping =  aes(x = c(1:5)), 
            label = levels(example_df$var), y = 100, 
            angle = 90, size = 14,
            lineheight = 0.9) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_text(size = 16),
        title = element_text(size = 23),
        legend.position = "none")
reading_example

ggsave("Viz_outputs/reading_example.png", dpi = 300, width = 13, height = 7)

example_plot  <- 
  ggplot(example_df, aes(x = var)) + geom_bar(fill = "gray") + ylim(0,max_n) +
  labs(title=paste("Reading example")) + xlab("") + ylab("") +
  geom_text(data = data.frame(levels = levels(example_df$var)),
            mapping =  aes(x = c(1:5)), 
            label = levels(example_df$var), y = 100, 
            angle = 90, size = 3,
            lineheight = 0.9) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position = "none",
        title = element_text(size = 6.5, face = "bold"))
example_plot

plot_list_instruments[["example"]] <- example_plot

position_plot <- lapply("position",plot_var_dist, y_limit = NA)

color_legend <- cowplot::get_legend(position_plot[[1]] + 
                                      guides(color = guide_legend(nrow = 1)) +
                                      theme(legend.position = "bottom"))
grid.newpage()
grid.draw(color_legend)

ggsave("Viz_outputs/legend.png",plot = grid.draw(color_legend), width = 12, 
       height = 2, dpi = 300)

instruments_plot <-
  cowplot::plot_grid(plotlist = plot_list_instruments,
                     nrow = round(sqrt(length(plot_list_instruments))) + 1,
                     ncol = round(sqrt(length(plot_list_instruments))))
cowplot::plot_grid(instruments_plot, color_legend, rel_heights = c(5,0.5), ncol = 1)

ggsave("Viz_outputs/instrument_prefs_small_multiples.png", width = 16, height = 12, dpi = 300)

# small multiples of all vars (except ever avoid)

plot_list_all <- lapply(colnames(responses)[!(colnames(responses) %in% c("ever_avoid"))],
                          plot_var_dist_reduced, y_limit = max_n, titlesize = 6.5)
plot_list_all[["example_plot"]] <- example_plot

plots_all <- 
  cowplot::plot_grid(plotlist = plot_list_all,
                     nrow = round(sqrt(length(plot_list_all))),
                     ncol = round(sqrt(length(plot_list_all))))

cowplot::plot_grid(plots_all, color_legend, rel_heights = c(5,0.5), ncol = 1)

ggsave("Viz_outputs/all_all_small_multiples.png",  width = 11.6, height = 8, dpi = 300)
ggsave("Viz_outputs/all_all_small_multiples.pdf",  width = 11.6, height = 8, dpi = 300)

# positions

p <- plot_var_dist("position", plot_legend = FALSE)
p <- p + ggtitle("Respondent position", subtitle = "") +
  scale_x_discrete(labels = c("Admin / IT \n Non-scientific", "BA/MA student", "Senior scientist \n Group leader \n Directorate",
                              "Other","PhD Student", "Postdoc", "Research assistant \n Technician")) +
  theme(axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12),
        axis.title = element_text(size = 12),
        title = element_text(size = 18),
        legend.position = "none")

ggsave("Viz_outputs/position_dist.png",  width = 11.6, height = 8, dpi = 300)

table(responses$position)

# differences between answer distributions among positions ----

measures_df <- responses[,which(colnames(responses) == "1_official_statement") : ncol(responses)]
measures_df$positions <- responses$position
table(measures_df$positions)
# remove BA students and others to only have largest groups
measures_df <- measures_df[!(measures_df$positions %in% c("Bachelor / Master Student","Other / Keine der Bezeichnungen trifft zu")),]

# plots of all vars separately

for (var in colnames(responses)){
  plot_var_dist(var,y_limit = max_n,plot_xlabs = FALSE, plot_legend = FALSE)
  ggsave(filename = paste("Viz_outputs/per_var_viz/",var,".png"), dpi = 300)
}

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


# assessment by flyaware group ----

ggplot(internal_assessment, aes(x = acceptance, y = impact, color = group)) +
  geom_jitter(width = 0.2, height = 0.2, alpha = 0.5, size = internal_assessment$cost * 15) + 
  xlab("Acceptance") + ylab("Expected impact") + 
  ggtitle("Categorization of measures", subtitle = "Size of dots by expected cost") +
  scale_y_continuous(breaks = c(1,2,3), labels = c("low","medium","high")) +
  labs(color = "Group") +
  scale_color_brewer(palette = "Dark2", type = "div") +
  theme_minimal() +
  theme(axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=16),
        legend.text = element_text(size=16),
        legend.title = element_text(size=16),
        axis.title = element_text(size = 22),
        title = element_text(size = 26)) +
  guides(colour = guide_legend(override.aes = list(size=10)))

ggsave("Viz_outputs/measure_categorization.png", dpi = 300, width = 13, height = 7)

# comments distribution ----

comments <- responses_with_comments[,grepl(pattern = "comment",colnames(responses_with_comments))]

colSums(!(is.na(comments)))
comments_per_person <- data.frame(ncomments = rowSums(!(is.na(comments))),
                                  position = responses_with_comments$position)

ggplot(comments_per_person, aes(y = ncomments, x = position, color = position)) +
  geom_boxplot(alpha = 0.8, color = "black") + geom_jitter(width = 0.2, height = 0.2, alpha = 0.7, size = 4) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust = 1)) + 
  ggtitle("Comments distribution across positions") +
  xlab(" ") +
  ylab("Number of comments per person") +
  scale_x_discrete(labels = c("Admin / IT \n Non-scientific", "BA/MA student", "Senior scientist \n Group leader \n Directorate",
                              "Other","PhD Student", "Postdoc", "Research assistant \n Technician")) +
  theme(axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12),
        axis.title = element_text(size = 12),
        title = element_text(size = 18),
        legend.position = "none")

ggsave("Viz_outputs/comments_distribution.png", dpi = 300, width = 13, height = 7)
