if(!exists("foo", mode="function")) source("init2.R")

cat("\014")

listaColunas <- colnames(data_xlsx) 
mostraElementos(listaColunas)

loadVariable(data_xlsx)
mostraElementos(vars_deletar)

aplicaRestrict_1porcento(0.1,data_analysis1, vars_deletar, data_xlsx, my.MainLoop )

print(paste("Accessible values from main loop variable [ ",listaColunas[[my.MainLoop]]," ]", sep = " "))

#Main loop goes here mfka

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
