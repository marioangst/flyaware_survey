
# setup ----

source("read_in_clean_data.R")

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


table(responses$position)

table(responses$ever_avoid)

colnames(responses)

responses$`9a_internal_cap`

# the relatively complicated reformating of the variable name is necessary 
# due to the number in the variable name

plot_var_dist <-
  function(var, fill_var = "position"){
    ggplot(responses, aes_string(paste("`", as.character(var), "`", sep=""))) +
      geom_bar(aes_string(fill = fill_var)) + 
      labs(title=paste(var," distribution"), 
           subtitle=paste("Colored by ",fill_var)) + 
      theme(axis.text.x = element_text(angle=90, vjust=0.6, hjust = 1))
  }

# test
plot_var_dist(colnames(responses)[8])

# for small multiples
plot_var_dist_reduced <-
  function(var, fill_var = "position"){
    # remove NAs
    responses <- responses[!(is.na(responses[[var]])),]
    ggplot(responses, aes_string(paste("`", as.character(var), "`", sep=""))) +
      geom_bar(aes_string(fill = fill_var)) +
      ggtitle(rename_based_on_codebook(var,var_codebook,
                                       "var_code","var_short_text")) +
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            legend.position = "none")
  }
# test
plot_var_dist_reduced(colnames(responses)[13])
table(responses$`5a_showcase_videoconf`)

# small multiples for instrument preferences
plot_list <- lapply(colnames(responses)[which(colnames(responses) == "1_official_statement") : ncol(responses)],
                    plot_var_dist_reduced)

cowplot::plot_grid(plotlist = plot_list,
                   nrow = round(sqrt(length(plot_list))) + 1,
                   ncol = round(sqrt(length(plot_list))))
ggsave("Viz_outputs/instrument_prefs_small_multiples.png", width = 18, height = 8)

# preference clustering ----


