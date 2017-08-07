if(!exists("foo", mode="function")) source("init2.R")

cat("\014")

listaColunas <- colnames(data_xlsx) 
mostraElementos(listaColunas)

#my.variableComparation <-readline(prompt="Digite a variavel de comparação principal : ")
#my.MainLoop   <-readline(prompt="Digite o caondição principal : ")
#my.InnerLoop1 <-readline(prompt="Digite o parâmetro/Condição para iterar: ")
#my.InnerLoop2 <-readline(prompt="Digite o segundo parâmetro/Condição para iterar: ")

my.variableComparation = 21
my.MainLoop   = 9
my.InnerLoop1 = 10
my.InnerLoop2 = 11

auxVarMainVar <- factor(data_xlsx[[my.MainLoop]])
data_analysis1 <- table(auxVarMainVar)/sum(table(auxVarMainVar))
data_analysis2 <- table(auxVarMainVar)
vars_deletar <- names(data_analysis1)
#
print("Conição I - erro de 1%")
for (count_iter in 1:length(data_analysis1)) {
  #count_iter=5
  #print(vars_deletar[count_iter])
   if(data_analysis1[count_iter] < 0.1){   
     print(paste("Suprimir tabela de dados relacionados ao Clima [",vars_deletar[count_iter] ,"] com média [ ", data_analysis1[count_iter]," ]", sep= " "))
     data2 <- data_xlsx[ data_xlsx$Clima != data_analysis1[count_iter] , ] 
    }
    
}


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
