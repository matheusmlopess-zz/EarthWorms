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
   pkgTest("readxl")
   pkgTest("dplyr")
   pkgTest("sqldf")

   
   library(data.table)
   library(sqldf)
   library(dplyr)
   library(agricolae)
   library(svDialogs)
   library(plyr)
   library(readxl)
   

############################################################################ 
#                                         MAIN                             #
############################################################################

  #SYSTEM INFO VAR
  si             <- sessionInfo()
  userInfo       <- Sys.info()
  sistemInfo     <- lapply(si, function(x) if (is.list(x)) x[sort(names(x))] else sort(x))
 
      mainDir <- getwd()  # Project directory by default 
      subDir  <- "Henrique.xlsx" 
      pathToXLSxfile <- paste(mainDir,subDir,sep="/")
      
      if (file.exists(file.path(mainDir, subDir))){
         data_xlsx <- read_excel(pathToXLSxfile, na = "NA")
      } else {
         pathToXLSxfile <- file.choose()
         data_xlsx <- read_excel(pathToXLSxfile, na = "NA")
      }
      
      #CASO CSV FILE ALTERAR READ 
      #data_CSV  <- read.csv(pathToCSVfile,sep=";",na.string="" , header = T, dec=",", fileEncoding="ISO-8859-1")
	   
############################################################################ 
#                                 FUNÇOES                                  #
############################################################################
	              
mostraElementos <- function(list) {
for (item in 1:length(list)) {
  print( paste("[", item,"] [",list[[item]],"] ", sep=" "))
  }
}
      
agregaElementros <- function(list_toAggregate, DF) {
  
  DF <- setDT(list(list_toAggregate))
  selecaoDeColuna <- DF[ , .(info = list(V1)), by = V1]
 # mostraElementos(selecaoDeColuna$V1)
 # tamanhoListaSelecao <-length(selecaoDeColuna$info)

}

incrementa <- function(x)
{
  eval.parent(substitute(x <- x + 1))
}
