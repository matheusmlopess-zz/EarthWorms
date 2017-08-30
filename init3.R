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

incrementa <- function(x){  eval.parent(substitute(x <- x + 1))   }


aplicaRestrict_1porcento <- function(  percent_Var,data_analysis1, vars_deletar, data_xlsx, my.MainLoop) {
  listaRestricoes<- list(1)
  data2 <- data_xlsx
  my.conter = 1
  count_iter = 0
  
  
  print("List of constraints below 1%")
  for (count_iter in 1:length(data_analysis1)) {
    if(data_analysis1[count_iter] < percent_Var){   
      listaRestricoes[[my.conter]] <- vars_deletar[count_iter]
      incrementa(my.conter)
      print(paste("Suprimir da coluna [ ",listaColunas[[my.MainLoop]] ," ] as linhas com [",vars_deletar[count_iter] ,"] > incidencia média: [ ", data_analysis1[count_iter]," ]", sep= " "))
    }
  }
  
  print(paste("Suprimindo restrição {1-Not consiteant data ( < than 1% incidence) } [ ",  listaColunas[[my.MainLoop]] ," ]", sep = " "))
  for(count_iter in 1:length(listaRestricoes)){
    #print( listaRestricoes[[count_iter]] )
    data2 <- data2[ data2$Clima != listaRestricoes[[count_iter]] , ]
  }
  
  assign("data_xlsx2", data2)
  
  print(         paste("Available data from main variable  [ ",listaColunas[[my.MainLoop]]," ]", sep = " "))
  auxVarMainVar  <- factor(data2[[my.MainLoop]])
  data_analysisA <- table(auxVarMainVar)/sum(table(auxVarMainVar))
  vars_deletar   <- names(data_analysisA)
  mostraElementos(vars_deletar)
  
}

loadVariable <- function(data_xlsx){
  
  #my.variableComparation <<- readline(prompt="Digite a variavel de comparação principal : ")
  #main.Loop.Variable   <<- readline(prompt="Digite o Variavel de compração caondição principal : ")
  #my.MainLoop <<- readline(prompt="Digite o segundo parâmetro/Condição para iterar: ")
  #my.InnerLoop1 <<- readline(prompt="Digite o parâmetro/Condição para iterar: ")
  #my.InnerLoop2 <<- readline(prompt="Digite o segundo parâmetro/Condição para iterar: ")
  
  main.Loop.Variable      <<- 10
  my.variableComparation  <<- 21
  my.MainLoop             <<- 9
  my.InnerLoop1           <<- 10
  my.InnerLoop2           <<- 11
  
  auxVarMainVar  <<- factor(data_xlsx[[my.MainLoop]])
  data_analysis1 <<- table(auxVarMainVar)/sum(table(auxVarMainVar))
  data_analysis2 <<- table(auxVarMainVar)
  vars_deletar   <<- names(data_analysis1)
  
 

  
}

