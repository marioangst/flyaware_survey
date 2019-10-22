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
      labs(title=paste(var," distribution"), 
           subtitle=paste("Colored by ",fill_var)) + 
      theme(axis.text.x = element_text(angle=90, vjust=0.6, hjust = 1))
  }

# Define UI
ui <- fluidPage(
  
  # Application title
  titlePanel("Survey response distributions for Eawag flyaware survey"),
  
  # 
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "var_choice",choices = colnames(responses),
                  label = "Variable",selected = "position")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotlyOutput("distr_viz")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distr_viz <- renderPlotly({
    ggplotly(plot_var_dist(as.character(input$var_choice)), height = 800)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)