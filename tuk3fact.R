if(!exists("foo", mode="function")) source("init2.R")

cat("\014")

listaColunas <- colnames(data_xlsx) 
mostraElementos(listaColunas)

#my.variableComparation <-readline(prompt="Digite a variavel de comparação principal : ")
#my.MainLoop   <-readline(prompt="Digite o caondição principal : ")
#my.InnerLoop1 <-readline(prompt="Digite o parâmetro/Condição para iterar: ")
#my.InnerLoop2 <-readline(prompt="Digite o segundo parâmetro/Condição para iterar: ")
main.Loop.Variable = 10

my.variableComparation = 21
my.MainLoop   = 9
my.InnerLoop1 = 10
my.InnerLoop2 = 11
my.conter = 1
count_iter = 0



auxVarMainVar <- factor(data_xlsx[[my.MainLoop]])
data_analysis1 <- table(auxVarMainVar)/sum(table(auxVarMainVar))
data_analysis2 <- table(auxVarMainVar)
vars_deletar <- names(data_analysis1)
vars_deletar1 <- vars_deletar

print(paste("Accessible values from main loop variable [ ",listaColunas[[my.MainLoop]]," ]", sep = " "))
mostraElementos(vars_deletar1)

listaRestricoes<- list(1)
data2 <- data_xlsx
print("List of constraints below 1")
for (count_iter in 1:length(data_analysis1)) {
  if(data_analysis1[count_iter] < 0.1){   
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
print(paste("Available data from main variable  [ ",listaColunas[[my.MainLoop]]," ]", sep = " "))
auxVarMainVar  <- factor(data2[[my.MainLoop]])
data_analysisA <- table(auxVarMainVar)/sum(table(auxVarMainVar))
vars_deletar   <- names(data_analysisA)
mostraElementos(vars_deletar)





selecionaInerloop_primeiroParametro <- data_xlsx[[listaColunas[[as.integer(my.InnerLoop1)]]]]
selecionaInerloop_segundoParametro <- data_xlsx[[listaColunas[[as.integer(my.InnerLoop2)]]]]

AgregaParametro1 <- setDT(list(selecionaInerloop_primeiroParametro))
AgregaParametro2 <- setDT(list(selecionaInerloop_segundoParametro))

selecaoDeColuna1 <- agregaElementros(selecionaInerloop_primeiroParametro,AgregaParametro1)
mostraElementos(selecaoDeColuna1[[1]])

selecaoDeColuna2 <- agregaElementros(selecionaInerloop_segundoParametro,AgregaParametro2)
mostraElementos(selecaoDeColuna2[[2]])

#DF <- setDT(list(selecionaInerloop_primeiroParametro))
#selecaoDeColuna <- DF[ , .(info = list(V1)), by = V1]
#mostraElementos(selecaoDeColuna$V1)
#tamanhoListaSelecao <-length(selecaoDeColuna$info)

############################################################################ 
#                                 INNER LOOP I                             #
############################################################################
for (count_size in 1:length(selecaoDeColuna1[[1]])) {
  #count_size =1
  
  dadoSelecionado <- subset(data_xlsx, 
                            eval(parse(text=listaColunas[[as.integer(my.InnerLoop1)]])) 
                            == 
                              toString(selecaoDeColuna1$V1[count_size]))
  
  print (paste(listaColunas[[as.integer(my.InnerLoop1)]],selecaoDeColuna1$V1[count_size], sep="  ->  "))
  
  m0 <- glm( (eval(parse(text=listaColunas[[as.integer(my.variableComparation)]]))+0.1) ~ 
               eval(parse(text=listaColunas[[as.integer(my.InnerLoop2)]])),
             family=Gamma(link='log'),
             data=dadoSelecionado)
  
  dfe <- df.residual(m0)
  mse <- deviance(m0)/dfe
  tuk <- HSD.test(y = dadoSelecionado[[my.variableComparation]],
                  trt = dadoSelecionado[[my.InnerLoop2]],
                  DFerror = dfe,
                  MSerror = mse)
  tuk$groups
  
}

############################################################################ 
#                                 INNER LOOP II                            #
############################################################################
for (count_size in 1:length(selecaoDeColuna2[[1]])) {
  #count_size =2
  count_size
  dadoSelecionado2 <- subset(data_xlsx, 
                             eval(parse(text=listaColunas[[as.integer(my.InnerLoop2)]])) 
                             == 
                               toString(selecaoDeColuna2$V1[count_size]))
  
  print (paste(listaColunas[[as.integer(my.InnerLoop2)]],selecaoDeColuna2$V1[count_size], sep="  ->  "))
  
  m0 <- glm( (eval(parse(text=listaColunas[[as.integer(my.variableComparation)]]))+0.1) ~ eval(parse(text=listaColunas[[as.integer(my.InnerLoop1)]])),
             family=Gamma(link='log'),
             data=dadoSelecionado)
  
  dfe <- df.residual(m0)
  mse <- deviance(m0)/dfe
  tuk <- HSD.test(y = dadoSelecionado2[[my.variableComparation]],
                  trt = dadoSelecionado2[[my.InnerLoop1]],
                  DFerror = dfe,
                  MSerror = mse)
  tuk$groups
  
}





dados2 <- dados1[!dados2$Bioma %in% c("Cerrado"), ]##quando precisa retirar dados

#Com a sele??o de clima, bioma e ecosistema, vamos fazer nosso tratamento de dados

#Primeiro, quero ver se, dentro de cada bioma, existe diferen?a de m?dias entre ecosistemas
d1 <- subset(dados2, Bioma=="Cerrado")
d2<- subset(dados2, Bioma=="Mata Atl?ntica")
#d3 <-  ... 

m0 <- glm((Dens+0.1) ~ Ecosistema,family=Gamma(link='log'), data=d1)
dfe <- df.residual(m0)
mse <- deviance(m0)/dfe
tuk <- HSD.test(y = d1$Dens,
                trt = d1$Ecosistema,
                DFerror = dfe,
                MSerror = mse)
tuk$groups

#Segundo e por fim, quero ver se, dentro de cada ecosistema, existe diferen?a entre biomas
d1 <- subset(dados2, Ecosistema=="Agricultura")
d2<- subset(dados2, Bioma=="Nativa")
#d3 <-  ... 

m0 <- glm((Dens+0.1) ~ Bioma,family=Gamma(link='log'), data=d1)
dfe <- df.residual(m0)
mse <- deviance(m0)/dfe
tuk <- HSD.test(y = Dens$Bioma,
                trt = Bioma,
                DFerror = dfe,
                MSerror = mse)
tuk$groups
