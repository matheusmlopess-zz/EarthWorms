#CLEAN ENV
rm(list = ls())
#SUPRESS WARNINGS ...
showWarnings = FALSE
#....................##################################...................#
#....................######## BASIC FUNCTIONS #########...................#
#....................##################################...................#

# CHECK IF $x PACKAGE IS INSTALLED
  pkgTest <- function(pack_){
    if (!require(pack_,character.only = TRUE)){
      install.packages(pack_, dep = TRUE)
      if(!require(pack_, character.only = TRUE)) stop("Package not found")
    }
  }

  #MOVE FILE FUNCTION
   move_file <- function(from, to) {
   todir <- dirname(to)
   if (!isTRUE(file.info(todir)$isdir)) dir.create(todir, recursive=TRUE)
   file.rename(from = from,  to = to)
}

############################################################################ 
#.....................#################################....................#
############################################################################   
#
#                                PACKS & LIBs 
#  
   pkgTest("agricolae")
   pkgTest("svDialogs")
   pkgTest("plyr")
   library(agricolae)
   library(svDialogs)
   library(plyr)
   
   
   
  # devtools::install_github(c('jeroenooms/jsonlite', 'rstudio/shiny', 'ramnathv/htmlwidgets', 'timelyportfolio/listviewer'))
  # library(listviewer)
  # jsonedit( myList )
   
#
############################################################################ 
#                                         MAIN                             #
############################################################################

  #SYSTEM INFO VAR
  si             <- sessionInfo()
  userInfo       <- Sys.info()
  sistemInfo     <- lapply(si, function(x) if (is.list(x)) x[sort(names(x))] else sort(x))
 
      mainDir <- getwd()

	              pathToCSVfile <- paste(mainDir,"data.csv",sep="/")
	              data_CSV <- read.csv(pathToCSVfile,sep=";",na.string="" , header = T, dec=",", fileEncoding="ISO-8859-1")
	   
#### FunÇões
	              
mostraElementos <- function(list) {
for (item in 1:length(list)) {
  print("item -> "+ item  + list[[item]])
  }
}
