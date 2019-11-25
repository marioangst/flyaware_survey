
# setup - packages needed for functions

library(plotly)
library(ggplot2)
library(GGally)
library(grid)
library(gridExtra)

# this script implements a number of functions used in the analysis. their scope is limited to this repository.

rename_based_on_codebook <- Vectorize(function(input,codebook,rawvar,codevar){
  #make sure there is only one coded entry in rawvar for input
  z <- codebook[[as.character(rawvar)]] %in% as.character(input)
  numberofentries <- sum(z, na.rm=TRUE)
  if (numberofentries > 1){
    replacement <- paste("Warning: More than one entry for","",as.character(gsub(input,pattern = ",",replacement = "")),"","in codebook")
  }
  if (numberofentries == 0){
    replacement <- paste("No entry for", as.character(input), "in codebook")
  }
  if (numberofentries == 1){
    replacement <- as.character(codebook[[as.character(codevar)]][codebook[[as.character(rawvar)]] %in% as.character(input)])
  }
  return(replacement)
},vectorize.args = c("input"))

# the relatively complicated reformating of the variable name is necessary 
# due to the number in the variable name

plot_var_dist <-
  function(var, fill_var = "position", percentage = FALSE, 
           y_limit = NA, plot_xlabs = TRUE,
<<<<<<< HEAD
           plot_legend = TRUE, graph_title = NULL){
    plot_data <- responses[!(is.na(responses[[var]])),]
    if(fill_var == "none"){
      p <- 
        ggplot(plot_data, aes_string(paste("`", as.character(var), "`", sep=""))) 
    }
    if(fill_var != "none"){
      p <- 
        ggplot(plot_data, aes_string(paste("`", as.character(var), "`", sep=""),
                                     fill = fill_var))
    }
=======
           plot_legend = TRUE, exclude_na = TRUE){
    if(exclude_na == TRUE){
      plot_data <- responses[!(is.na(responses[[var]])),]
    }
    if(exclude_na == FALSE){
      plot_data <- responses
    }
    p <- 
      ggplot(plot_data, aes_string(paste("`", as.character(var), "`", sep=""),
                                 fill = fill_var))
>>>>>>> cb4c9c9123aac6a5a0815f8620e84dafe86defff
    if(percentage == FALSE){
      p <- p + geom_bar() + ylim(0,y_limit)
    }
    if(percentage == TRUE){
      p <- p + geom_bar(position = "fill") + ylab("Percentage")
    }
    if(!is.null(graph_title)){
      p <- p +
        labs(title=graph_title)
    }
    if(is.null(graph_title)){
      p +
        labs(title=paste(rename_based_on_codebook(input = var,codebook = var_codebook,
                                                  rawvar = "var_code","var_short_text"),
                         "distribution of answers"), 
             subtitle=paste("Colored by",fill_var))
    }
    p <-  p +
<<<<<<< HEAD
=======
      labs(title=paste(rename_based_on_codebook(input = var,codebook = var_codebook,
                                                rawvar = "var_code","var_short_text"),
                       "distribution of answers"), 
           subtitle=paste("Colored by",fill_var)) + 
      scale_fill_brewer(palette = "Dark2", type = "div") +
      scale_x_discrete(drop = FALSE) +
>>>>>>> cb4c9c9123aac6a5a0815f8620e84dafe86defff
      ylab("Count") + xlab(" ") +
      theme(axis.text.x = element_text(angle=90, vjust=0.6, hjust = 1))
    if (plot_legend == FALSE){
      p <- p + theme(legend.position = "none")
    }
    if (plot_xlabs == FALSE){
      p <- p +
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank())
    }
    p
  }

# test
# plot_var_dist(colnames(responses)[8])
# plot_var_dist(colnames(responses)[15])
# plot_var_dist(colnames(responses)[8], plot_legend = FALSE, plot_xlabs = FALSE)

# for small multiples
plot_var_dist_reduced <-
  function(var, fill_var = "position", y_limit){
  function(var, fill_var = "position", y_limit, titlesize = 12){
    # remove NAs
    plot_data <- responses[!(is.na(responses[[var]])),]
    ggplot(plot_data, aes_string(paste("`", as.character(var), "`", sep=""))) +
      geom_bar(aes_string(fill = fill_var)) +
      ylim(NA,y_limit) +
      ggtitle(rename_based_on_codebook(var,var_codebook,
                                       "var_code","var_short_text")) +
      scale_fill_brewer(palette = "Dark2", type = "div") +
      scale_x_discrete(drop = FALSE) +
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            title = element_text(size = titlesize),
            legend.position = "none")
  }
# test
# plot_var_dist_reduced(colnames(responses)[13], y_limit = 150)
# plot_var_dist_reduced(colnames(responses)[15], y_limit = 150)
# table(responses$`5a_showcase_videoconf`)

# turn likert statements into factors ----
factor_likert_statements <- function(var){
  factor(var,
         levels = c("Strongly agree / Stimme voll und ganz zu" ,
                    "Agree / Stimme eher zu",
                    "Neither agree nor disagree / Weder Zustimmung noch Ablehnung",
                    "Disagree / Lehne eher ab",
                    "Strongly disagree / Lehne voll und ganz ab"),
         labels = c("Strongly agree" ,
                    "Agree",
                    "Neither agree nor disagree",
                    "Disagree",
                    "Strongly disagree"),
         ordered = TRUE)
}