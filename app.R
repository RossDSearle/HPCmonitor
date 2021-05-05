library(shiny)
library(ssh)
library(stringr)
library(rhandsontable)

source(paste0(getwd(), '/appConfig.R'))


session <- ssh_connect(host, passwd='Gobs4066')
cmd <- paste0('/apps/R/3.6.1/bin/Rscript /datasets/work/af-digiscapesm/work/Ross/SLGA/Shiny/HPC/taskController.R Show_Job_Log')
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
     selectInput('task', label = 'Task', choices = tasks, selected='Show Jobs Info'),
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
    
    if(t=='Show_Job_Log'){
      
      odfx <- read.table(text=rb, header=F, skip=1)
      odf2 <- data.frame(JobID=odfx$V2, jobName=odfx$V3, startTime = paste0(odfx$V4, ' ',odfx$V5, ' ',odfx$V6, ' ',odfx$V7, ' ',odfx$V8), startIter=odfx$V9, endIter=odfx$V10)
      RV$currentResponse <- odf2
    }else if(t=='Show_Jobs_Info'){
      RV$currentResponse <- read.table(text=rb, header=T, skip=0)
    }else if(t=='Show_Queue'){
      odf <- read.table(text=gsub("\\[1\\] 0", "", rb), header=F, skip=1)
      odf2 <- data.frame(JobID=odf$V1, jobName=odf$V3, ident = paste0(odf$V4), ST=odf$V5, TIME=odf$V6, NODES=odf$V7, NODELIST=odf$V8)
      RV$currentResponse <- odf2
      
    }

  })
}

# Run the application
shinyApp(ui = ui, server = server)
