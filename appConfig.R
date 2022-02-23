library(shiny)
library(shinyMobile)
library(shinyjs)
library(stringr)
library(ssh)
library(rhandsontable)
library(shinybusy)
library(shinycssloaders)
library(safer)
library(shinycssloaders)
library(shiny.pwa)

source('./appUtils.R')

AppName = 'HPCMonitR'
shortHostName = 'Petrichor'
hostName = paste0(shortHostName, '-i1.hpc.csiro.au')
defWidth = 300

Rpath <- '/apps/R/4.0.5/bin/Rscript' 
taskControllerPath <- '/datasets/work/af-digiscapesm/work/Ross/SLGA/Shiny/HPC/taskController.R'


tasks<-c('Show Jobs Info',
         #'Show Jobs Info - Verbose',
         'Show Queue',
         'Show Number CPUs In Use',
         'Show Job Log',
         'Show All Users',
         'HPC Load'
)

