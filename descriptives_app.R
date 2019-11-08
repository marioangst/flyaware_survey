library(shiny)
library(plotly)
library(ggplot2)
library(reshape2)

source("read_in_clean_data.R", encoding = "UTF-8")

# ---- plotting functions

# plotting functions

plot_var_dist <-
  function(var, fill_var = "position"){
    ggplot(responses, aes_string(paste("`", as.character(var), "`", sep=""))) +
      geom_bar(aes_string(fill = fill_var)) + 
      labs(title=paste(rename_based_on_codebook(input = var,codebook = var_codebook,
                                                rawvar = "var_code","var_short_text"),
          " distribution of answers"), 
           subtitle=paste("Colored by ",fill_var)) + 
      theme(axis.text.x = element_text(angle=90, vjust=0.6, hjust = 1))
  }

# utility funcion ---------

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

# Define UI -----------

choices_list <- as.list(colnames(responses))
names(choices_list) <- rename_based_on_codebook(input = colnames(responses),codebook = var_codebook,
                                                rawvar = "var_code","var_short_text")

ui <- fluidPage(
  
  # Application title
  titlePanel("Survey response distributions for Eawag flyaware survey"),
  
  # 
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "var_choice",choices = choices_list,
                  label = "Variable",selected = "position")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotlyOutput("distr_viz")
    )
  )
)

# Define server -------------
server <- function(input, output) {
  
  output$distr_viz <- renderPlotly({
    ggplotly(plot_var_dist(as.character(input$var_choice)), height = 800)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)