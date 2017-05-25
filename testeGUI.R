#CLEAN ENV
rm(list = ls())
#SUPRESS WARNINGS ...
showWarnings = FALSE

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


#MY LIBRARIES

#REQUEST PACK CHECK IF ITS OK IT LEAVES OTHERWISE IT INSTALL THE REQUESTED PACKAGE 
pkgTest("FactoMineR")
pkgTest("DiscriMiner")
pkgTest("MASS")
pkgTest("gWidgets")
pkgTest("gWidgetstcltk")
pkgTest("tcltk")
pkgTest("shiny")
pkgTest("shinyBS")

#CORE LIBS FOR EarthWorm Project
library(FactoMineR)
library(DiscriMiner)
library(MASS)
library(gWidgets)
library(tcltk)
library(gWidgetstcltk)
library('stringr')

#GUI API libs
library(shiny)
library(shinyBS)

Tamplate

name <- "myname"
ui = fluidPage(
  uiOutput("curName"),
  br(),
  actionButton("BUTnew", "Change"),
  bsModal("modalnew", "Change name", "BUTnew", size = "small",
          textOutput("textnew"),
          radioButtons("change_name", "", choices = list("Yes" = 1, "No" = 2, "I dont know" = 3),selected = 2),
          conditionalPanel(condition = "input.change_name == '1'",textInput("new_name", "Enter New Name:", ""))    
  )
)


server = function(input, output, session) {
  
  output$curName <- renderUI({textInput("my_name", "Current name: ", name)})
  
  observeEvent(input$BUTnew, {
    output$textnew <- renderText({paste0("Do you want to change the name?")})
  })
  
  observe({
    input$BUTnew
    if(input$change_name == '1'){
      if(input$new_name != ""){
        output$curName <- renderUI({textInput("my_name", "Current name: ", input$new_name)})
      }
      else{
        output$curName <- renderUI({textInput("my_name", "Current name: ", name)})
      }
    }
  })
}

runApp(list(ui = ui, server = server))
