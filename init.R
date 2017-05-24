# CHECK IF $x PACKAGE IS INSTALLED  
pkgTest <- function(pack_)
{
  if (!require(pack_,character.only = TRUE)){
    install.packages(pack_, dep = TRUE)
    if(!require(pack_, character.only = TRUE)) stop("Package not found")
  }
}

#FILE CHOOSE
fileChoose <- function(action="print", text = "Select a file...", type="open", ...) {
  gfile(text=text, type=type, ..., action = action, handler =  function(h,...) {  do.call(h$action, list(h$file)) })
}


#REQUEST PACK CHECK IF ITS OK IT LEAVES OTHERWISE IT INSTALL THE REQUESTED PACKAGE 
pkgTest("FactoMineR")
pkgTest("DiscriMiner")
pkgTest("MASS")
pkgTest("gWidgets")
pkgTest("gWidgetstcltk")
pkgTest("tcltk")
pkgTest("cairoDevice")

library(FactoMineR)
library(DiscriMiner)
library(MASS)
library(gWidgets)
library(tcltk)
library(gWidgetstcltk)
library('stringr')
library(cairoDevice)

#SYSTEM INFO VAR

  si <- sessionInfo()
  info <- lapply(si, function(x) if (is.list(x)) x[sort(names(x))] else sort(x))

# SETTING UP G.U.I
# When gWidgets is started, it tries to figure out what its default toolkit will be.

# GUI LAYOUT INITIALIZATION
  #SETING UP GUITOOLKIT
  require(gWidgets)
  options("guiToolkit"="tcltk")
  
  guiEarthWorm <- function() {
    
    ## set up
    availDists <- c(Normal = "rnorm", Exponential="rexp")
    testeDisponiveis <- c("Teste K", "epanechnikov", "rectangular","triangular")
    
    
    updatePlot <- function(h,...) {
      x <- do.call(availDists[svalue(distribution)],list(svalue(sampleSize)))
      plot(density(x, adjust = svalue(bandwidthAdjust), kernel = svalue(kernel)))
      rug(x)
    }
    
    ##The widgets
    win <- gwindow("EarthWorm")
    gp <- ggroup(horizontal=FALSE, cont=win)
    
    tmp <- gframe("Distribution", container=gp, expand=TRUE)
    distribution <- gradio(names(availDists), horizontal=FALSE,
                           cont=tmp,
                           handler=updatePlot)
    
    
    tmp <- gframe("Sample size", container=gp, expand=TRUE)
    sampleSize <- gradio(c(50,100,200, 300), cont=tmp,
                         handler =updatePlot)
    
    
    tmp <- gframe("Testes Disponiveis", container=gp, expand=TRUE)
    kernel <- gcombobox(testeDisponiveis, cont=tmp,
                        handler=updatePlot)
    
    tmp <- gframe("Bandwidth adjust", container=gp, expand=TRUE)
    bandwidthAdjust <- gslider(from=0,to=2,by=.01, value=1,
                               cont=tmp, expand=TRUE,
                               handler=updatePlot)
    
  }
  
  
  
  
# WINDOWS FRAME
# janelaFrame <- gwindow("Projeto EarthWorm by - git@github.com:matheusmlopess/EarthWorms.git")
# CREATE THE GROUP THAT GLUES GUI OBJECTS INTO THE WINDOWS FRAME CREATED 
# grp_nome <- ggroup(container = janelaFrame)
#THE BASIC GUI OBJ
# "
#  options("guiToolkit"="tcltk")
#  obj <- gbutton("Hello world", container = gwindow())
#  obj <- glabel("Hello world", container = gwindow())
#  obj <- gedit("Hello world", container = gwindow())
#  obj <- gtext("Hello world", container = gwindow())
#  obj <- gradio(c("hello","world"), container=gwindow())
#  obj <- gcombobox(c("hello","world"), container=gwindow())
#  obj <- gcombobox(c("hello","world"), editable=TRUE, container=gwindow())
#  obj <- gtable(c("hello","world"), container=gwindow())
#  obj <- gcheckboxgroup(c("hello","world"), container=gwindow())
#  obj <- gslider(from=0, to = 7734, by =100, value=0, container=gwindow())
#  obj <- gspinbutton(from=0, to = 7734, by =100, value=0, container=gwindow())
#  "
#SESSION_INFO
# obj <- gtext(
#     paste(
#     paste("Configurações Locais  :",info$locale,sep=" "),
#     paste("-------------------------------------------------------------------------------"),
#     paste("Plataforma usada :",info$platform,sep=" "),
#     paste("-------------------------------------------------------------------------------"),
#     paste("Sistema Operacional :",info$running,sep=" "),
#     paste("-------------------------------------------------------------------------------"),
#     c(info$basePkgs)
#   )
#   , container = grp_name)
#fileChoose(action="setwd", type="selectdir", text="Select a directory...")

confirmDialog <- function(message, handler=NULL) {
  
  janela1 <- gwindow("Confirmação")
  grupo1 <- ggroup(container = janela1)
  gimage("info", dirname="stock", size="dialog", container=grupo1)
  
  ## A group for the message and buttons
  inner.group <- ggroup(horizontal=FALSE, container = grupo1)
  glabel(message, container=inner.group, expand=TRUE)
  
  ## A group to organize the buttons
  button.group <- ggroup(container = inner.group)
  ## Push buttons to right
  addSpring(button.group)
  gbutton("Selecionar Pasta com os Dados", handler=handler, container=button.group)
  gbutton("Cancelar", handler = function(h,...) dispose(janela1),
          container=button.group)
  
  return()
}

# 
# 
# pathToFolder <- file.choose()
# pathToFolderDir <- dirname(pathToFolder)
# setwd(pathToFolderDir)
# getwd()
# cat("a. Select Before Working Directory:") 
# 
# 
# don <- read.csv(pathToFolder,sep=";",na.string="" , header = T, dec=",", fileEncoding="ISO-8859-1")
# summary(don)


