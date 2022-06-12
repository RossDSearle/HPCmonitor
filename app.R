source('./appConfig.R')
########    USER INTERFACE  ########################################
####  .  ####

shiny::shinyApp(
  ui = f7Page(
    
    
    options = list(theme = "auto",
                   color = "lightblue",
                   pullToRefresh = F,
                   filled = T,
                   navbar = list(iosCenterTitle = F, hideOnPageScroll = F), 
                   toolbar = list(hideOnPageScroll = FALSE),
                   dark = T
    ),
    
    allowPWA = FALSE,  ## This turns off F7s default PWA generation and we use shiny.pwa as per below
    
    pwa("https://shiny.esoil.io/HPCmonitor/",  title = AppName, output = "www", icon='www/monitor.png', 
        offline_template = 'www/offline.html', offline_message='Sorry we are offline'),
    
    tags$head(tags$link( rel="icon", type="image/png", href="./monitorSmall.png", sizes="32x32" )),
    
    ########   Login Popup   ################     
    title = shortHostName,
    f7SingleLayout(
      navbar = f7Navbar(
       # title = paste0(shortHostName, ' HPC Monitoring'),
        hairline = FALSE,
        shadow = F,
        bigger = F,
        hideStatusbar=T,
        leftPanel = T,
        
        title = tags$div( tags$div(style="vertical-align:bottom; text-align:left!important; display:inline-block, height:120px; width:300px;"),
                          HTML('&nbsp&nbsp&nbsp'), tags$div(style="float: right; ", tags$img(src = "Logos/csiro3.png", width = "50px", height = "50px", align='right')),
                          HTML('&nbsp&nbsp&nbsp'),  tags$div(style="vertical-align: middle; font-size: 30px;  ",HTML(paste0('&nbsp&nbsp&nbsp', AppName)) )) 
        
      ),
      
      panels = tagList(
        f7Panel(title = "Settings", side = "left", theme = "light", effect = "cover",
                
                HTML('<BR><BR>
                     Click anywhere in the App to dismiss this panel.'),
                
                
                
                f7Stepper(
                  inputId='UIsettingsPollInterval',
                  label='CPU update interval (seconds)',
                  min = 20,
                  max = 60,
                  value = 20,
                  step = 5,
                  fill = T,
                  rounded = F,
                  raised = T,
                  size = 'large',
                  color = 'green',
                  wraps = FALSE,
                  autorepeat = TRUE,
                  manual = FALSE,
                  decimalPoint = 0,
                  buttonsEndInputMode = TRUE
                ),
                f7Text(inputId='UIsettingsDebugPath', label='Log Path', value = "", placeholder = NULL),
                HTML('<BR><BR>'),
                f7Button(
                  inputId = 'UIsettingsSave',
                  label = 'Close',
                  href = NULL,
                  color = 'blue',
                  fill = TRUE,
                  outline = FALSE,
                  shadow = FALSE,
                  rounded = T,
                  size = 'small'
                )
        )
      ),
      
      HTML('<BR>'),
      f7Button("UI_Login", paste0("Login to ", shortHostName), color = 'blue',),
      f7Sheet(
        id = "sheet1",
        label = "More",
        orientation = "bottom",
      
        
        f7Text(inputId='vvvvvv', label='User', value = "", placeholder = 'UserName')
        
        ),
      
      f7Popup(
        id = "popupLogin",
        title = paste0("Login to ", shortHostName),
        f7Text(inputId='usrVal', label='User', value = "", placeholder = 'UserName'),
        f7Password(inputId='pwdVal', label='User', value = "", placeholder = 'Password'),
        f7Button(inputId = "UI_popLoginBut", "Login", color = 'green'),
        htmlOutput(outputId = "LoginResult")
      ),
      
      uiOutput("ui"),
      
      
      #######   Cookie Management  ########### 
      
      tags$head(tags$script(src="js.cookie.js")),
      tags$head(tags$script(
        HTML('
        Shiny.addCustomMessageHandler ("readCookie",function (message) {
        var cookie = readCookie(message.name);
        Shiny.onInputChange("cookie", cookie);
      })

      function readCookie(name) {
        var nameEQ = name + "=";
        var ca = document.cookie.split(";");
        for(var i=0;i < ca.length;i++) {
                var c = ca[i];
                while (c.charAt(0)==" ") c = c.substring(1,c.length);
                if (c.indexOf(nameEQ) == 0) return      c.substring(nameEQ.length,c.length);
        }   
        return ""; }'))),
      
      tags$head(tags$script(
        HTML('
         Shiny.addCustomMessageHandler ("writeCookie",function (message) {
         const d = new Date();
         d.setTime(d.getTime() + (365*24*60*60*1000));
         let expires = "expires="+ d.toUTCString();
         document.cookie = "HPCAuth=" + message.name + ";" + expires + ";"           
      })
      ')))
    )
  ),
  
  
  server = function(input, output, session) {
    
    RV <- reactiveValues()
    RV$Init <- 1
    RV$currentCookie <- NULL
    RV$currentUsr <- NULL
    RV$currentPwd <- NULL
    RV$CPUUpdateInterval <- cpuUpdateInterval
    RV$debugPath <- NULL
    
    ############   SERVER CODE   ##########################################
    ##### . ####
    
    #################  Login To Server  ###################################
    
    observe({
      updateF7Stepper(inputId = 'UIsettingsPollInterval', value=RV$CPUUpdateInterval)
    })
    
    observe({
      updateF7Stepper(inputId = 'UIsettingsDebugPath', value='Ross')
    })
    
    
    observeEvent(input$UI_popLoginBut, {
      
      # warning(paste0('usr = ', input$pwdVal))
      # warning(paste0('pws = ', input$usrVal))
      # warning(paste0('host = ', hostName))
 
      resp <- authenticateServer(host=hostName, user=input$usrVal, passwd=input$pwdVal)
      
      if(resp){
        msg <- paste0(input$usrVal, 'HHHH', input$pwdVal)
        session$sendCustomMessage(type="writeCookie", message=list(name=msg))
        RV$currentUsr <- input$usrVal
        RV$currentPwd <- input$pwdVal
        
        #############################  Render the UI after Authentication     ########################          
        
        output$ui <- renderUI({

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
                      f7Button(inputId = 'Update', label = "Update",  color = 'green', fill = TRUE, outline = F, shadow = T, rounded = T, size = 'small'),
                    ),

                    tableOutput('mainDT')
          )
        })
        
        updateF7Popup(id = "popupLogin")
      }else{
        output$LoginResult <- renderText ({ paste0('<BR><H1 style="color: red;">Login to ', shortHostName, ' failed</H1>') })
      } 
    })
    
    ############################   Deal with cookie info  ##########################
    
    observeEvent(RV$Init,{
      session$sendCustomMessage(type="readCookie", message=list(name='HPCAuth'))
    })
    
    
    observeEvent(input$cookie,{
      RV$currentCookie <- input$cookie
    })
    
    observeEvent(input$UI_Login, {
      bits <- str_split(RV$currentCookie, 'HHHH')
      u <- bits[[1]][1]
      p <- bits[[1]][2]
      updateF7Text(inputId = 'usrVal', value = u)
      updateF7Text(inputId = 'pwdVal', value = p )
      
      #f7TogglePopup(id = "popupLogin")
      updateF7Popup(id = "popupLogin")
    })
    
    
    
    
    #########   Main Functionality ############
    
    output$mainDT <- renderTable({ 
      
      req(RV$currentUsr, RV$currentPwd)
     
      input$Update 
      
          isolate({ 
            
            progress <- shiny::Progress$new(style = 'notification')
            progress$set(message = "Retrieving Info", value = 50)
            # Close the progress when this reactive exits (even if there's an error)
            on.exit(progress$close())   
      
      theTask <- input$task
      
      u <- RV$currentUsr
      p <- RV$currentPwd
      h <- paste0(u,'@', hostName)
      
      try({
        
        sshsession <- ssh_connect(host=h, passwd=p)
        t <- str_replace_all(theTask, " ", "_")
        cmd <- paste0(Rpath, ' ',taskControllerPath, ' ', t, ' ', u)
        print(cmd)
        #cmd <- paste0('/apps/R/3.6.1/bin/Rscript /datasets/work/af-digiscapesm/work/Ross/SLGA/Shiny/HPC/taskController.R ', t, ' ', u)
        #cmd <- paste0('/apps/R/4.0.5/bin/Rscript /datasets/work/af-digiscapesm/work/Ross/SLGA/Shiny/HPC/taskController.R ', 'Show_Jobs_Info_-_Verbose', ' ', u)
        #cmd <- paste0('/apps/R/4.0.5/bin/Rscript /datasets/work/af-digiscapesm/work/Ross/SLGA/Shiny/HPC/taskController.R ', 'Show_Jobs_Info', ' ', u)
        #cmd <- paste0('/apps/R/4.0.5/bin/Rscript /datasets/work/af-digiscapesm/work/Ross/SLGA/Shiny/HPC/taskController.R Show_All_Users sea084')
        
        #cmd <- paste0('/apps/R/4.0.5/bin/Rscript /datasets/work/af-digiscapesm/work/Ross/SLGA/Shiny/HPC/taskController.R Show_Job_Log sea084')
        
        resp <- ssh_exec_internal(sshsession, command=cmd)
        ssh_disconnect(sshsession)
        
        rb <- readBin(resp$stdout, what='character')
        
       # cat(rb, file='/datasets/work/af-digiscapesm/work/Ross/rb.txt')
        
        if(t=='Show_Job_Log'){
          test
          
          odfx <- read.table(text=rb, header=F, skip=1, fill=T)
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
          odf <- read.table(text=gsub("\\[1\\] 0", "", rb), header=F, skip=1, fill=T)
          odf2 <- data.frame(JobID=odf$V1, jobName=odf$V3, ident = paste0(odf$V4), ST=odf$V5, TIME=odf$V6, NODES=odf$V7, NODELIST=odf$V8)
          RV$currentResponse <- odf2
        }else if(t=='Show_Number_CPUs_In_Use'){
          odf<- read.table(text=rb, header=F, skip=0)
          odf2 <- data.frame(CPUs_In_Use=odf$V2)
          RV$currentResponse <- odf2
        }else if(t=='Show_All_Users'){
          odf<- read.table(text=rb, header=T, skip=0, fill=T)
          RV$currentResponse <- odf
        }
        else if(t=='HPC_Load'){
          odf<- read.table(text=rb, header=F, skip=0, fill=T)
          odf2 <- data.frame(HPC_Load=odf$V2)
          RV$currentResponse <- odf2
        }
      })
      
       })
      
      
      RV$currentResponse}, striped = TRUE, bordered = TRUE, hover = TRUE, spacing = 'xs')  
    
########   CPU Count Polling  ##########
    
    pollData <- reactivePoll((cpuUpdateInterval * 1000), session,
                             
                             checkFunc = function() {
                               paste0(Sys.time())
                             },
                             valueFunc = function() {
                               
                               req(input$pwdVal, input$usrVal)
                               
                               if(input$pwdVal !='' | !is.null(input$pwdVal )){
                                 u <- input$usrVal
                                 p <- input$pwdVal
                                 h <- paste0(u,'@', hostName)
                                 
                                 
                                 sshsession <- ssh_connect(host=h, passwd=p)
                                 t <- 'Show_Number_CPUs_In_Use'
                                 cmd <- paste0(Rpath, ' ',taskControllerPath, ' ', t, ' ', u)
                                 
                                 resp <- ssh_exec_internal(sshsession, command=cmd)
                                 ssh_disconnect(sshsession)
                                 
                                 rb <- readBin(resp$stdout, what='character')
                                 odf<- read.table(text=rb, header=F, skip=0)
                                 odf2 <- data.frame(CPUs_In_Use=odf$V2)
                                 paste0(odf$V2)
                               }
                               
                             }
    )
    
    output$pollText <- renderText({
      t <- pollData()
      paste0(t)
    })
    
    
  }
)



##### . ####
##### . ####
########   Code Archive    #######


# tags$head(tags$script(
#   HTML('
#     Shiny.addCustomMessageHandler ("writeCookie",function (message) {
#     
#      //alert(message.name)
#      //var uid = document.getElementById("usr").value;
#      //var pwd = document.getElementById("pwd").value;
#      
#      //alert(pwd);
#     
#       //var ck = uid + "XXXX" + pwd;
#       //Cookies.set(\'HPCAuth\', message.name, { expires: 365 });
#       
#      // document.cookie = "HPCAuth=" + message.name +" { expires: 365 }";
#       
#        const d = new Date();
#        d.setTime(d.getTime() + (365*24*60*60*1000));
#       let expires = "expires="+ d.toUTCString();
#       document.cookie = "HPCAuth=" + message.name + ";" + expires + ";"           
#     })
#     ')
# ))
