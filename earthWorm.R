setwd("~/Desktop/gits/EarthWorms/")
source("init.R")
guiEarthWorm()

#SORT TABLE BY KEY=textura
#sort(don$textura)
# QUANTIFY BY TYPE OF KEY
#table(don$soloO)

# fazer uma m?dia de algumas variaveis num?ricas segundo a tipologia de textura
aggregate(don[24:40], don[41], FUN=sd, na.rm=TRUE)

#uma olhada para poder escolher melhor quais variaveis escolher, ja que s?o muitas e que n?o podemos fazer modelos com mais variaveis que individuos
PCA(don[,c(5:7,16,25:40)])

#fazer uma regress?o linear
m0 <- lm(Logbio~ umidade + pH + heal, data=don)
summary(m0)$adj.r.squared

#NAO CONSEGUI fazer uma automatiza??o de fazer uma tabela que d? o R quadrado dos modelos de regress?o linear para cada grupo da tipologia 
aggregate(summary(lm(don$Logbio~ don$umidade + don$pH + don$heal))$adj.r.squared ,by=don[41], FUN=lm(Logbio~ umidade + pH + heal), data=don,  na.rm=TRUE)

Areia <- subset(don, textura=="Areia")
Areiafranca <- subset(don, textura=="Areiafranca")
Argila <- subset(don, textura=="Argila")
Argiloarenosa <- subset(don, textura=="Argiloarenosa")
Argilossiltosa <- subset(don, textura=="Argilossiltosa")
Franca <- subset(don, textura=="Franca")
Francoargiloarenosa <- subset(don, textura=="Franco-argiloarenosa")
Francoargilossiltosa <- subset(don, textura=="Franco-argilossiltosa")
Francoarenosa <- subset(don, textura=="Francoarenosa")
Francoargilosa <- subset(don, textura=="Francoargilosa")
Francossiltosa <- subset(don, textura=="Francossiltosa")
Muitoargilosa <- subset(don, textura=="Muito argilosa")

#Areia <- subSet("textura","Areia")


#subSet{$1,$2}
#(
#  return var <-subset(don,$1=$2)
#)

Areia[,c(5:7,16,25:40)]
PCA(Areia[,c(5:7,16, 20, 24,25:40)])
cor(Areia[,c(5:7,16, 20, 24,25:40)], na.action=na.rm)

Areiafranca
Argila
Argiloarenosa
Argilossiltosa
Franca
Francoargiloarenosa
Francoargilossiltosa
Francoarenosa
Francoargilosa
Francossiltosa
Muitoargilosa

mAreia <- summary(lm(Logdens~ sup + k + ctc, data=Areia))$adj.r.squared
mAreiafranca <- summary(lm(Logbio~ umidade + pH + heal, data=Areiafranca))$adj.r.squared
mArgila <- summary(lm(Logbio~ umidade + pH + heal, data=Argila))$adj.r.squared
mArgiloarenosa <- summary(lm(Logbio~ umidade + pH + heal, data=Argiloarenosa))$adj.r.squared
mArgilossiltosa <- summary(lm(Logbio~ umidade + pH + heal, data=Argilossiltosa))$adj.r.squared
mFranca <- summary(lm(Logbio~ umidade + pH + heal, data=Franca))$adj.r.squared
mFrancoargiloarenosa <- summary(lm(Logbio~ umidade + pH + heal, data=Francoargiloarenosa))$adj.r.squared
mFrancoargilossiltosa <- summary(lm(Logbio~ umidade + pH + heal, data=Francoargilossiltosa))$adj.r.squared
mFrancoarenosa <- summary(lm(Logbio~ umidade + pH + heal, data=Francoarenosa))$adj.r.squared
mFrancoargilosa <- summary(lm(Logbio~ umidade + pH + heal, data=Francoargilosa))$adj.r.squared
mFrancossiltosa <- summary(lm(Logbio~ umidade + pH + heal, data=Francossiltosa))$adj.r.squared
mMuitoargilosa <- summary(lm(Logbio~ umidade + pH + heal, data=Muitoargilosa))$adj.r.squared

modelos <- list(mAreia,mAreiafranca,mArgila,mArgiloarenosa,mArgilossiltosa,mFranca,mFrancoargiloarenosa,mFrancoarenosa,mFrancoargilosa,mMuitoargilosa)
modelos

#---------------  TECNICA 2: USAR O PODER DISCRIMINANTE DA VARIAVEL DE INTERESSE PARA SEPARAR OS GRUPOS 
#Esta tecnica faz com que ao descer na arvore, a cada nivel, o Intervalo de Confianca da variavel de interesse vai melhorar


dpepoca <- discPower(don[,c(19, 23)],don[,c(2)])$F_statistic
dpestado <- discPower(don[,c(19, 23)],don[,c(4)])$F_statistic
dpclima <- discPower(don[,c(19, 23)],don[,c(8)])$F_statistic
dpbioma <- discPower(don[,c(19, 23)],don[,c(9)])$F_statistic
dpecosistema <- discPower(don[,c(19, 23)],don[,c(14)])$F_statistic
dpsolo <- discPower(don[,c(19, 23)],don[,c(15)])$F_statistic
dptextura <- discPower(don[,c(19, 23)],don[,c(41)])$F_statistic

dpepoca <- discPower(don[,c(19, 23)],don[,c(2)])$wilks_lambda[1]
dpestado <- discPower(don[,c(19, 23)],don[,c(4)])$wilks_lambda[1]
dpclima <- discPower(don[,c(19, 23)],don[,c(8)])$wilks_lambda[1]
dpbioma <- discPower(don[,c(19, 23)],don[,c(9)])$wilks_lambda[1]
dpecosistema <- discPower(don[,c(19, 23)],don[,c(14)])$wilks_lambda[1]
dpsolo <- discPower(don[,c(19, 23)],don[,c(15)])$wilks_lambda[1]
dptextura <- discPower(don[,c(19, 23)],don[,c(41)])$wilks_lambda[1]

plot(don$época, don$R3dens)#vemos que um valor baixo n?o ? capaz de separar grupos
plot(don$textura, don$R3dens)#vemos que um valor baixo n?o ? capaz de separar grupos
plot(don$clima, don$R3dens)
plot(don$bioma, don$R3dens)
plot(don$eco, don$R3dens)
plot(don$soloO, don$R3dens)
summary(don)

#ao considerar que a melhor tipologia ? o bioma (F value de 10,3)
aggregate(don[,c(19, 23)], don[9], FUN=mean, na.rm=TRUE )
aggregate(don[,c(19, 23)], don[9], FUN=sd, na.rm=TRUE )

#agora a ideia seria de ver dentro dos 5 grupos criados qual ? a outra tipologia que ? melhor separada pelas variaveis de interesse
#poderemos ver qual o se esta tipologia ? igual entre os 5 grupos agora criados.
#dentro deste grupo podemos ver se a razao cultivado/nativo ? guardada.
