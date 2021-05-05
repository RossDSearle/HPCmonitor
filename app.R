library(shiny)
library(ssh)
library(stringr)
library(rhandsontable)

source(paste0(getwd(), '/appConfig.R'))


session <- ssh_connect(host, passwd='Gobs4066')
cmd <- paste0('/apps/R/3.6.1/bin/Rscript /datasets/work/af-digiscapesm/work/Ross/SLGA/Shiny/HPC/taskController.R Get_Job_Log')
print(cmd)
resp <- ssh_exec_internal(session, command=cmd)
ssh_disconnect(session)
rb <- readBin(resp$stdout, what='character')
jobsdf <- read.table(text=rb, header=F, skip=1)



ui <- fluidPage(
  

  titlePanel("Pearcey Remote Monitoring"),
  
  sidebarLayout(
    sidebarPanel(
     selectInput('usr', label = 'user', choices = c('sea084', 'mal000')),
     textInput('pwd', label = 'pwd'),
     actionButton('ServerLogin',  label = 'Login'),
     selectInput('jobList', label = 'Jobs', choices = paste0(rev(jobsdf$V2), '_', rev(jobsdf$V3))),
     selectInput('task', label = 'Task', choices = tasks),
     actionButton('doQuery',  label = 'Query'),
     
    ),
    
    mainPanel(
      # textOutput("foo"),
      # tags$style(type="text/css", "#foo {white-space: pre-wrap;}"),
      #htmlOutput('MainResponse')
      rHandsontableOutput('mainDT')
    )
  )
)


server <- function(input, output) {
  
  RV <- reactiveValues()
  RV$currentResponse = NULL
  
  output$mainDT <- DT::renderDataTable({
    
    req(RV$currentResponse)
    print(head(RV$currentResponse))
    DT::datatable(RV$currentResponse)
  })
  
  output$mainDT <- renderRHandsontable({
    req(RV$currentResponse)
      rhandsontable(RV$currentResponse)
  })
  
  output$MainResponse <- renderUI({
    req(RV$currentResponse)
   HTML(RV$currentResponse)
  })
  
  observeEvent(input$ServerLogin, {
    print('Login click')
  })
  
  observeEvent(input$doQuery, {
    session <- ssh_connect(host, passwd='Gobs4066')
    #resp <- ssh_exec_internal(session, command = "squeue -u sea084")
    
    t <- str_replace_all(input$task, " ", "_")
    cmd <- paste0('/apps/R/3.6.1/bin/Rscript /datasets/work/af-digiscapesm/work/Ross/SLGA/Shiny/HPC/taskController.R ', t)
    print(cmd)
    resp <- ssh_exec_internal(session, command=cmd)
    ssh_disconnect(session)
    
    rb <- readBin(resp$stdout, what='character')
    odf <- read.table(text=rb, header=T, skip=0)
    
    
   # ot <- str_replace_all(rb, pattern = '\n', '<br>')
    #print(head(odf))
    RV$currentResponse <- odf
    
    
  })
}

# Run the application
shinyApp(ui = ui, server = server)
