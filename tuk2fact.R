if(!exists("foo", mode="function")) source("init.R")
# A ideia desse script ? criar uma fun??o que fa?a um test tukey para 
# 2 fatores, agrupando assim os tratamentos segundo esses dois fatores
# tais como Bioma e Ecosistema
# Uma fun??o complementar poder? tamb?m fazer isto separando por um 
# terceiro fator tal como Clima
# Como primeira etapa, tem que verificar que:
#  - o clima estudado contem pelo menos 1% do total dos dados E 
#  - um n?mero de linha >=5.
# table(dados$Clima)/sum(table(dados$Clima))
# table(dados$Clima)
cat("\014")
listaColunas <- colnames(data_CSV) 
mostraElementos(listaColunas)

dados1 <- data[!data$Clima %in% c("As"), ]

##fun??o que exlui climas que n?o se enquadram
#Dentro dos climas que se enquadram nessas condi??es, tem que verificar que
#cada conjunto Ecosistema-Bioma :
#  - represente pelo menos 1% dos dados NAQUELE CLIMA E 
#  - um n?mero de linha >=5

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
