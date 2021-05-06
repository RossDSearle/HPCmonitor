library(ssh)

host <-  'sea084@pearcey-i1.hpc.csiro.au'

session <- ssh_connect(host, passwd='Gobs4066')
cmd <- paste0('/apps/R/3.6.1/bin/Rscript /datasets/work/af-digiscapesm/work/Ross/SLGA/Shiny/HPC/taskController.R Show_Number_CPUs_In_Use')
print(cmd)
resp <- ssh_exec_internal(session, command=cmd)
ssh_disconnect(session)
rb <- readBin(resp$stdout, what='character')
odf <- read.table(text=rb, header=F, skip=0)

cat(rb, file='c:/temp/q.csv')

odf <- read.table(text=gsub("\\[1\\] 0", "", rb), header=F, skip=1)
odf2 <- data.frame(JobID=odf$V1, jobName=odf$V3, ident = paste0(odf$V4), ST=odf$V5, TIME=odf$V6, NODES=odf$V7, NODELIST=odf$V8)

odf2 <- data.frame(JobID= odf$V1, jobName=odf$V2, startTime=paste0(odf$V4, ' ',odf$V5, ' ',odf$V6, ' ',odf$V7, ' ',odf$V8, startIt=odf$V9), endIt=odf$V10, PENDING=odf$V11, COMPLETED=odf$V12, FAILED=odf$V13, RUNNING=odf$V14, CANCELLED=odf$V15, TIMEOUT=odf$V16, OUT_OF_MEMORY=odf$V17)


gsub("\\[1\\] 0", "", rb)
