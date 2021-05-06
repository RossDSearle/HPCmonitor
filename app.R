library(shiny)
library(ssh)
library(stringr)
library(rhandsontable)
library(shinybusy)
library(glouton)  ## cookie handling
source(paste0(getwd(), '/appConfig.R'))


# session <- ssh_connect(host, passwd='Gobs4066')
# cmd <- paste0('/apps/R/3.6.1/bin/Rscript /datasets/work/af-digiscapesm/work/Ross/SLGA/Shiny/HPC/taskController.R Show_Job_Log')
# print(cmd)
# resp <- ssh_exec_internal(session, command=cmd)
# ssh_disconnect(session)
# rb <- readBin(resp$stdout, what='character')
# jobsdf <- read.table(text=rb, header=F, skip=1)



ui <- fluidPage(
  

  add_busy_spinner(spin = "flower", margins = c(0, 0), position='full-page', color = 'red',height = "80px", width = "80px"),
  use_glouton(),
  
  titlePanel("Pearcey Remote Monitoring"),
  
  sidebarLayout(
    sidebarPanel(
     selectInput('usr', label = 'user', choices = appUsers),
     #passwordInput('pwd', label = 'pwd'),
     textInput('pwd', label = 'pwd'),
     actionButton('SaveLogin',  label = 'Save login Info'),
     #selectInput('jobList', label = 'Jobs', choices = paste0(rev(jobsdf$V2), '_', rev(jobsdf$V3))),
     HTML('<br><br><br>'),
     selectInput('task', label = 'Task', choices = tasks, selected='Show Jobs Info'),

     
    ),
    
    mainPanel(
      rHandsontableOutput('mainDT')
    )
  )
)


server <- function(input, output, session) {
  
  session$allowReconnect(TRUE)

  
  RV <- reactiveValues()
  RV$currentResponse = NULL
  RV$isStarting = NULL
  
  output$mainDT <- renderRHandsontable({
    req(RV$currentResponse)
      rhandsontable(RV$currentResponse)
  })
  
  
  observeEvent(input$SaveLogin, {
    glouton::add_cookie(name="ShinyHPCMonitor", value=paste0(input$usr, 'XXXX',input$pwd))
  })
  
  
  
  observe({

   ck <- glouton::fetch_cookie(name="ShinyHPCMonitor", session = session)
   cks <- paste(ck, collapse="")

   if(!is.null(cks) & length(cks)>0){
     pwd <-str_split(cks, 'XXXX')
     updateTextInput(session = session, inputId = 'pwd', value = pwd[[1]][2])
     RV$isStarting="a"
   }
  })
 
  
  
  observe({

    
    print(RV$isStarting)
    print(input$usr)
    print(input$pwd)
    
    theTask <- input$task
    
    isolate({ 
      
      if(is.null(input$usr) | is.null(input$pwd)){
        return()
      }
      if(input$usr=='' | input$pwd==''){
        return()
      }
      
      u <- input$usr
      p <- input$pwd
      h <- paste0(u,'@', host)
      
    session <- ssh_connect(host=h, passwd=p)

    t <- str_replace_all(theTask, " ", "_")
    cmd <- paste0('/apps/R/3.6.1/bin/Rscript /datasets/work/af-digiscapesm/work/Ross/SLGA/Shiny/HPC/taskController.R ', t)
    resp <- ssh_exec_internal(session, command=cmd)
    ssh_disconnect(session)

    rb <- readBin(resp$stdout, what='character')

    if(t=='Show_Job_Log'){

      odfx <- read.table(text=rb, header=F, skip=1)
      odf2 <- data.frame(JobID=odfx$V2, jobName=odfx$V3, startTime = paste0(odfx$V4, ' ',odfx$V5, ' ',odfx$V6, ' ',odfx$V7, ' ',odfx$V8), startIter=odfx$V9, endIter=odfx$V10)
      RV$currentResponse <- odf2
    }else if(t=='Show_Jobs_Info'){
      RV$currentResponse <- read.table(text=rb, header=T, skip=0)
    }else if( t== 'Show_Jobs_Info_-_Verbose'){
      odf <-  read.table(text=rb, header=F, skip=1)
      odf2 <- data.frame(JobID= odf$V2, jobName=odf$V3, startTime=paste0(odf$V4, ' ',odf$V5, ' ',odf$V6, ' ',odf$V7, ' ',odf$V8), startIt=odf$V9, endIt=odf$V10, PENDING=odf$V11, COMPLETED=odf$V12, FAILED=odf$V13, RUNNING=odf$V14, CANCELLED=odf$V15, TIMEOUT=odf$V16, OUT_OF_MEMORY=odf$V17)
      RV$currentResponse <- odf2
    } else if(t=='Show_Queue'){
      odf <- read.table(text=gsub("\\[1\\] 0", "", rb), header=F, skip=1)
      odf2 <- data.frame(JobID=odf$V1, jobName=odf$V3, ident = paste0(odf$V4), ST=odf$V5, TIME=odf$V6, NODES=odf$V7, NODELIST=odf$V8)
      RV$currentResponse <- odf2
    }else if(t=='Show_Number_CPUs_In_Use'){
      odf<- read.table(text=rb, header=F, skip=0)
      odf2 <- data.frame(CPUs_In_Use=odf$V2)
      RV$currentResponse <- odf2
    }else if(t=='Show_All_Users'){
      odf<- read.table(text=rb, header=T, skip=0)
      RV$currentResponse <- odf
    }
    else if(t=='HPC_Load'){
      odf<- read.table(text=rb, header=F, skip=0)
      odf2 <- data.frame(HPC_Load=odf$V2)
      RV$currentResponse <- odf2
    }

    })
    
  })
}

# Run the application
shinyApp(ui = ui, server = server,
         
         onStart = function() {
           cat("Doing application setup\n")
           
           onStop(function() {
             cat("Doing application cleanup\n")

           })
         })
