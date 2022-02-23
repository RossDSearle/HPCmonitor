authenticateServer <- function(host, user, passwd){
  
  print(host)
  print(user)
  print(passwd)
  hostu <- paste0(user, '@', host)
  
  tryCatch(
    expr = {
      sshsession <- ssh_connect(host=hostu, passwd=passwd)
      cmd <- paste0('whoami')
      resp <- ssh_exec_internal(sshsession, command=cmd)
      ssh_disconnect(sshsession)
      rb <- readBin(resp$stdout, what='character')
      print(rb)
      
      if(str_remove(rb, '\n') == user){
        return(T)
      }else{
        return(F)
      }
    },
    error = function(e){
     # message('Caught an error!')
      # print(e)
      # message(e)
      return(F)
    },
    warning = function(w){
      #message('Caught an warning!')
      print(w)
      return(F)
    },
    finally = {
      #return(T)
    }
  )
}