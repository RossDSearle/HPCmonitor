#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

library(ssh)
library(stringr)



server <- 'sea084@pearcey-i1.hpc.csiro.au'

# session <- ssh_connect(server, passwd='Gobs4066')
# resp <- ssh_exec_wait(session, command = "squeue -u sea084")
# ssh_disconnect(session)

session <- ssh_connect(server, passwd='Gobs4066')
#resp <- ssh_exec_internal(session, command = "squeue -u sea084")

cmd <- '/apps/R/3.6.1/bin/Rscript /datasets/work/af-digiscapesm/work/Ross/SLGA/Shiny/HPC/test1.R'
resp <- ssh_exec_internal(session, command =cmd)
ssh_disconnect(session)


rb <- readBin(resp$stdout, what='character')

#ot <- str_split(rb, pattern = '\n')

ot <- str_replace_all(rb, pattern = '\n', '<br>')



# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Old Faithful Geyser Data"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
     selectInput('usr', label = 'user', choices = c('sea084', 'mal000')),
     textInput('pwd', label = 'pwd'),
     actionButton('ServerLogin',  label = 'Login')
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      # textOutput("foo"),
      # tags$style(type="text/css", "#foo {white-space: pre-wrap;}"),
      htmlOutput('bob')
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$bob <- renderUI({
   HTML(ot)
  })
  
  # output$foo <- renderText({
  #   rb
  # })
}

# Run the application
shinyApp(ui = ui, server = server)
