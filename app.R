library(shiny)
library(shinyMobile)
library(shinyjs)
library(ssh)
library(stringr)
library(rhandsontable)
library(shinybusy)
library(glouton)  ## cookie handling
library(shinycssloaders)

source(paste0(getwd(), '/appConfig.R'))
source(paste0(getwd(), '/DontSync_other.R'))


logfilename='x'

defWidth = '380px'

shiny::shinyApp(
  ui = f7Page(
    title = "Pearcey Remote Monitoring",
    init = f7Init(skin = "auto", theme = "light", filled = T, color = 'lightblue'),
    tags$head(tags$link( rel="icon", type="image/png", href="cpu.png", sizes="32x32" ),
              tags$link( rel="apple-touch-icon", href="apple-touch-icon.png" )
    ),
    
    useShinyjs(),
    use_glouton(),

    
    #add_busy_bar(color = "#FF0000", centered = FALSE, height = "18px"),
    #add_busy_spinner(spin = "fading-circle", margins = c(0, 0), position='full-page', height = "80px", width = "80px"),
    #add_busy_spinner(spin = "flower", margins = c(0, 0), position='full-page', color = 'red',height = "80px", width = "80px"),
    
    preloader = F,
    
    f7TabLayout(
      panels = tagList(
        
        
        
        f7Panel(title = "Login", side = "left", theme = "dark", effect = "cover",
                
                f7Picker(
                  inputId = 'usr',
                  label = "user", 
                  choices = appUsers,
                ),
                f7Password(inputId = 'pwd', label = "pwd", value = '' ),
                #f7Text(inputId = 'pwd', label = "pwd", value = '' ),
                
                f7Button(inputId = 'SaveLogin', label = "Save login Info", src = NULL, color = 'green', fill = TRUE, outline = F, shadow = T, rounded = T, size = 'small')
                
                #actionButton('SaveLogin',  label = 'Save login Info')
                
                
        )
        #f7Panel(title = "Right Panel", side = "right", theme = "dark", "Blabla", effect = "cover")
      ),
      
      ##################################  NAVIGATION BAR   ##################################      
      navbar = f7Navbar(
        # title = shiny::tags$div(style="background-image: url('Logos/HdrBkGrdImage.PNG');", tags$img(src = "Logos/csiro.png", width = "40px", height = "40px"), "Boowora Agricultutral Research Station "),
        title = tags$div( tags$div(style="vertical-align:middle!important; text-align:left!important; display:inline-block;", "Pearcey Remote Monitoring"), HTML('&nbsp&nbsp&nbsp'), tags$div(style="float: right;", tags$img(src = "Logos/csiro.png", width = "40px", height = "40px", align='right'))),
        hairline = T,
        shadow = T,
        left_panel = T,
        right_panel = F
      ),
      
      
      ##################################  UI - Monitoring  ##################################         
      
      f7Tabs(
        animated = T,
        #swipeable = TRUE,
        f7Tab(
          tabName = "Monitor",
          icon = f7Icon("list_number_rtl"),
          active = TRUE,
          f7Float( f7Shadow(
            intensity = 10,
            hover = TRUE,
            tags$div( style=paste0("width: ", defWidth),
                      
                      f7Card(
                        title = NULL,
                        footer = NULL,
                        
                        verbatimTextOutput("pollText"),

                        f7Picker(
                          inputId = 'task',
                          label = 'Tasks', 
                          choices = tasks,
                        ),HTML('<BR>'),
                        f7Button(inputId = 'Update', label = "Update", src = NULL, color = 'green', fill = TRUE, outline = F, shadow = T, rounded = T, size = 'small'),
                      )
            )
          ), side = "left" ),
          
          f7Float(  f7Shadow(
            intensity = 100,
            hover = TRUE,
            tags$div( style=paste0("min-width: ", defWidth),
                      f7Card(
                        #rHandsontableOutput('mainDT')
                        shinycssloaders::withSpinner( tableOutput('mainDT') )
                        
                      )
                      )
            ), side = "left" ),
        ),
        
        ##################################  UI - Not Used yet   ##################################             
        
        f7Tab(
          tabName = "",
          icon = f7Icon("layers_fill", old = F),
          active = FALSE,
          f7Float(
            f7Shadow(
              intensity = 10,
              hover = TRUE,
              tags$div( style=paste0("width: ", defWidth),  
                        f7Card()
              )
            )
            , side = "left")
        )
        
      )
    )
  ),
  
  
  ##################################  SERVER  ##################################   
  server = function(input, output, session) {
    
    session$allowReconnect(TRUE)
    
    
    RV <- reactiveValues()
    RV$currentResponse = NULL
    RV$isStarting = NULL
    RV$isStarting="a"
    RV$NumCPUS=1
    
    
    observeEvent(input$uploadFilesBtn, {
      # When the button is clicked, wrap the code in a call to `withBusyIndicatorServer()`
      withBusyIndicatorServer("uploadFilesBtn", {
        Sys.sleep(1)
        if (input$select == "error") {
          stop("choose another option")
        }
      })
    })
    
    
    pollData <- reactivePoll(20000, session,

                             checkFunc = function() {
                                  paste0(Sys.time())
                             },
                             valueFunc = function() {
                               req(input$pwd)
                               u <- input$usr
                               p <- input$pwd
                               h <- paste0(u,'@', host)
                               sshsession <- ssh_connect(host=h, passwd=p)
                               t <- 'Show_Number_CPUs_In_Use'
                               cmd <- paste0('/apps/R/3.6.1/bin/Rscript /datasets/work/af-digiscapesm/work/Ross/SLGA/Shiny/HPC/taskController.R ', t)
                               resp <- ssh_exec_internal(sshsession, command=cmd)
                               ssh_disconnect(sshsession)
                               
                               rb <- readBin(resp$stdout, what='character')
                               odf<- read.table(text=rb, header=F, skip=0)
                               odf2 <- data.frame(CPUs_In_Use=odf$V2)
                               paste0(odf$V2)
                               
                             }
    )
    
    output$pollText <- renderText({
      t <- pollData()
      print(t)
      paste0(t)
    })
    
    # output$mainDT <- renderRHandsontable({
    #   req(RV$currentResponse)
    #   rhandsontable(RV$currentResponse, readOnly = TRUE)
    # })
    
    output$mainDT <- renderTable({ 
      
      
      input$Update 
      
      
      # waiter_show( # show the waiter
      #   html = spin_fading_circles() # use a spinner
      # )
      
      isolate({ 
        
        theTask <- input$task
        
        if(is.null(input$usr) | is.null(p)){
          return()
        }
        if(input$usr=='' | input$pwd ==''){
          return()
        }
        
        u <- input$usr
        p <- input$pwd
        h <- paste0(u,'@', host)
        
        
        try({
          sshsession <- ssh_connect(host=h, passwd=p)
          
          t <- str_replace_all(theTask, " ", "_")
          cmd <- paste0('/apps/R/3.6.1/bin/Rscript /datasets/work/af-digiscapesm/work/Ross/SLGA/Shiny/HPC/taskController.R ', t)
          # print(cmd)
          resp <- ssh_exec_internal(sshsession, command=cmd)
          ssh_disconnect(sshsession)
          
          rb <- readBin(resp$stdout, what='character')
          #print(rb)
          
          if(t=='Show_Job_Log'){
            
            odfx <- read.table(text=rb, header=F, skip=1)
            odf2 <- data.frame(JobID=odfx$V2, jobName=odfx$V3, startTime = paste0(odfx$V4, ' ',odfx$V5, ' ',odfx$V6, ' ',odfx$V7, ' ',odfx$V8), startIter=odfx$V9, endIter=odfx$V10)
            RV$currentResponse <- odf2
          }else if(t=='Show_Jobs_Info'){
            df<-read.table(text=rb, header=T, skip=0)
            colnames(df) <- c('JobID', 'P', 'C', 'F', 'R', 'Ca', 'T', 'M')
            RV$currentResponse <- df
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

      
      RV$currentResponse}, striped = TRUE, bordered = TRUE, hover = TRUE, spacing = 'xs')  
    
    
    observeEvent(input$SaveLogin, {
      cstring <- paste0(input$usr, 'XXXX',input$pwd)
      glouton::add_cookie(name="ShinyHPCMonitor", value=paste0(cstring))
      print(paste0('Cookie Saved - ', cstring))
      
    })
    
    
    
    observe({
      print('Reading cookie')
      ck <- glouton::fetch_cookie(name="ShinyHPCMonitor", session = session)
    #  print(ck)
      cks <- paste(ck, collapse="")
      print(paste0('Cookie is - ',cks))
      if(!is.null(cks) & length(cks)>0){
        pwd <-str_split(cks, 'XXXX')
      #  print(pwd)
        updateTextInput(session = session, inputId = 'pwd', value = pwd[[1]][2])
        #RV$isStarting="a"
      }
    })
    
  }
)









