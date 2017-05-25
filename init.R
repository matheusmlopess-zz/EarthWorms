#CLEAN ENV
rm(list = ls())
#SUPRESS WARNINGS ...
showWarnings = FALSE
#..................................##################################..................................#
#..................................######## BASIC FUNCTIONS #########..................................#
#..................................##################################..................................#

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

#......................................................................................................#
#......................................................................................................#
#......................................................................................................#


#PACKS & LIBs 
#REQUEST PACK CHECK IF ITS OK IT LEAVES OTHERWISE IT INSTALL THE REQUESTED PACKAGE 
pkgTest("FactoMineR")
pkgTest("DiscriMiner")
pkgTest("MASS")
pkgTest("gWidgets")
pkgTest("gWidgetstcltk")
pkgTest("tcltk")
pkgTest("shiny")
pkgTest("shinyBS")
pkgTest("svDialogs")

#CORE LIBS FOR EarthWorm Project
library(FactoMineR)
library(DiscriMiner)
library(MASS)
library(gWidgets)
library(tcltk)
library(gWidgetstcltk)
library('stringr')
library(svDialogs)


#GUI API libs
library(shiny)
library(shinyBS)

########################################################################################
#                                         MAIN                                         #
########################################################################################

  #SYSTEM INFO VAR
  si             <- sessionInfo()
  userInfo       <- Sys.info()
  sistemInfo     <- lapply(si, function(x) if (is.list(x)) x[sort(names(x))] else sort(x))

  #SEE IF ITS A WINDOWS OR LINUX SYSTEM 
	if(grepl("Windows",userInfo[1])){

      mainDir <- paste("C:/Users",userInfo[6],"Desktop",sep="/")
	    subDir  <- "data"
	}else{
	  
	    dlgMessage(paste("EartWorms will be running in a Linux Distro AWESOME mate!\n", userInfo[1],sistemInfo$platform, sep ="  \n     "))$res
	    mainDir <- paste("~/home",userInfo[6],"Desktop",sep="/")
	    subDir  <- "data"
	  
	}
	   
	    # CHECK IF DATA FOLDER IS ALREADY SET
	    if (file.exists(file.path(mainDir, subDir))){
	      
	         #CHECK IF DATA FILE (CSV) IS IN THE FOLDER ALREADY
	          if(file.exists(file.path(finalDataPath))){
	              #CHECK IF DATA FOLDER & DATA FILE ARE SET TO GO....
	              #print("ALL SET: DATA FOLDER AND DATA FILE ON POSITION")
                dlgMessage("ALL SET \nData folder created in Desktop \nEarthWorms data file (CSV) selected!\n\n\n             INITIALIZING GUI        ")$res
	              return(0)
	            
	          }else{
	              #DO WHATERVER TO CREATE DATA FOLDER & SELECT DATA FILE
	              #print("ALMOST ALL SET: DATA FOLDER CREATED ALREAD... PLEASE SELECT DATA FILE")
	              dlgMessage("Data folder created in Desktop... \nNow, Please select the EarthWorms data file (CSV)!")$res
	              pathToCSVfile <- file.choose()
	              finalDataPath=paste(mainDir,subDir,"dataFile.csv",sep="/")
	              move_file(from = pathToCSVfile, to   = finalDataPath )
	              
                 #---------------------------------------------------------------------------------------------------
	               # RAPHA'S VARIABLE TO DATAS FILE CONTENT (send to workspace)
	               #---------------------------------------------------------------------------------------------------
      	         don <- read.csv(finalDataPath,sep=";",na.string="" , header = T, dec=",", fileEncoding="ISO-8859-1")
	         
	       }
	      
	  } else {
	      #DO WHATERVER TO CREATE DATA FOLDER & SELECT DATA FILE
	      print("Initializing platform for windows user ...")
	      dlgMessage("Initializing platform for windows user ... Creating Data folder and selecting Data Files (CSV)")$res
	      dir.create(file.path(mainDir, subDir), showWarnings = FALSE)
	      pathToCSVfile <- file.choose()
	      finalDataPath=paste(mainDir,subDir,"dataFile.csv",sep="/")
	      move_file(from = pathToCSVfile, to = finalDataPath )

	      #---------------------------------------------------------------------------------------------------
	      # RAPHA'S VARIABLE TO DATAS FILE CONTENT (send to workspace)
	      #---------------------------------------------------------------------------------------------------
	      don <- read.csv(finalDataPath,sep=";",na.string="" , header = T, dec=",", fileEncoding="ISO-8859-1")
	    
	  }
    
  
  # SETTING UP SHYNE APP GUI
    runApp("EarthWormsAPP/")

  
  
# pathToFolder <- file.choose()
# pathToFolderDir <- dirname(pathToFolder)
# setwd(pathToFolderDir)
# getwd()
# cat("a. Select Before Working Directory:") 
# 
# 
# don <- read.csv(pathToFolder,sep=";",na.string="" , header = T, dec=",", fileEncoding="ISO-8859-1")
# summary(don)


