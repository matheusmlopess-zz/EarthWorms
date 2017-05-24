# CHECK IF $x PACKAGE IS INSTALLED  
# VERIFICA SE PACOTE ESTA INSTALDO
pkgTest <- function(pack_)
{
  if (!require(pack_,character.only = TRUE)){
    install.packages(pack_,dep=TRUE)
    if(!require(pack_,character.only = TRUE)) stop("Package not found")
  }
}

#REQUEST PACK CHECK IF ITS OK IT LEAVES OTHERWISE IT INSTALL THE REQUESTED PACKAGE 
pkgTest("FactoMineR")
pkgTest("DiscriMiner")
pkgTest("MASS")

library(FactoMineR)
library(DiscriMiner)
library(MASS)

cat("a. Select Before Working Directory:") 
pathToFolder <- file.choose()
pathToFolderDir <- dirname(pathToFolder)
setwd(pathToFolderDir)
getwd()

don <- read.csv(pathToFolder,sep=";",na.string="" , header = T, dec=",", fileEncoding="ISO-8859-1")
summary(don)
