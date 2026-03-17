#########################################
#                                       #
#  UNIVERSIDADE FEDERAL DE SANTA MARIA  #
#     Manejo Florestal - CFL 1056       #
#                                       #
#########################################
#                                       #
#  Prática 1 - Classificação de Sítos   #
#                                       #
#  Prof. Gabriel A. Orso                #
#                            18/03/2026 #
#########################################



# 1. Importando dados ----

dados <- read.csv2("Dados_Atividade.csv")

View(dados)

# As colunas dentro de 'dados' podem ser acessadas com o cifrão $

dados$idade
dados$hdom

# 2. Visualizando dados de sítio ----

plot(x = dados$idade, y = dados$hdom)

plot(hdom ~ idade, data = dados)


# 3. Ajustando o modelo de Schumacher ----
#   Hdom = B0 * B1^(1/t) 
# ln(Hdom) = B0 + B1 * (1/I)


# Vamos chamar as variáveis de "y" e "x"
y <- log(dados$hdom)
x <- 1/dados$idade

# Ajuste do modelo
mod.schu <- lm(formula = y ~ x)

# Algumas estatísticas de ajuste
summary(schu)

# Os coeficientes ajustados são obtidos com a função coef()

coef(mod.schu)

b0.schu <- coef(mod.schu)[1]
b1.schu <- coef(mod.schu)[2]


# 3.3. Gráfico das curvas de índice de sítio ----

x <- dados$idade

func.schu <- function(x) {(exp(b0.schu+b1.schu*(1/x)))}

plot(dados$idade,dados$hdom, main="Curvas de índice de Sítio Ajustadas",
     col="darkgrey",xlab="Idade (anos)",ylab="Hdom (m)",
     xlim=range(0,20),ylim=range(0,22),pch=20,cex=1.5)

curve(func.schu, from=min(x),to=max(x),col="red",lwd=2,add=T)

legend("bottomright",legend=c("Schumacher","Observados"),
     pch=c(NA,16),lty=c(1,NA),lwd=2,col=c("red","darkgrey"))



# 4. Construir curvas de índice de sítio com 5 classes ----
# pelo método da curva guia


iref <- 7
dados.iref <- subset(dados,dados$idade == 7)
max.iref <- max(dados.iref$hdom)
# max.iref <- 20  #
min.iref <- min(dados.iref$hdom)
# min.iref <- 7   #
amp <- max.iref-min.iref
nc <- 5
interv <- amp/nc
idades <- c(1:20)

# 4.1.  Formando os limites Superiores e Inferiores ----

# Sítio I (SI)
SI.LS <- max.iref
SI.LI <- SI.LS - interv
SI.CC <- mean(c(SI.LS,SI.LI))

# Sítio II (SII)
SII.LS <- SI.LI
SII.LI <- SII.LS-interv
SII.CC <- mean(c(SII.LS,SII.LI))

# Sítio III (SIII)
SIII.LS <- SII.LI
SIII.LI <- SIII.LS-interv
SIII.CC <- mean(c(SIII.LS,SIII.LI))

# Sítio IV (SIV)
SIV.LS <- SIII.LI
SIV.LI <- SIV.LS-interv
SIV.CC <- mean(c(SIV.LS,SIV.LI))

# Sítio V (SV)
SV.LS <- SIV.LI
SV.LI <- SV.LS-interv
SV.CC <- mean(c(SV.LS,SV.LI))

# Juntando todos os sítios
S <- c(SI.LS,SI.CC,SI.LI,SII.LS,SII.CC,SII.LI,SIII.LS,SIII.CC,SIII.LI,SIV.LS,SIV.CC,SIV.LI,SV.LS,SV.CC,SV.LI)
S



# 4.2. Estimativas  ----
# Função de Chapman com b0 isolado:

func.schu.iref <- function (S,idades,iref) {exp(log(S) + b1.schu*(1/idades - 1/iref))}



# 4.3. Tabela com os limites estimados ----

matriz.classes <- matrix(nrow=20,ncol=15)
rownames(matriz.classes) <- c("1","2","3","4","5","6","7=iref","8","9","10","11",
                              "12","13","14","15","16","17","18","19","20")
colnames(matriz.classes) <- c("I.LS","I.CC","I.LI","II.LS","II.CC","II.LI",
                              "III.LS","III.CC","III.LI","IV.LS","IV.CC","IV.LI",
                              "V.LS","V.CC","V.LI")

matriz.classes


matriz.classes[,1] <- func.schu.iref(SI.LS,idades,iref)
matriz.classes[,2] <- func.schu.iref(SI.CC,idades,iref)
matriz.classes[,3] <- func.schu.iref(SI.LI,idades,iref)
matriz.classes[,4] <- func.schu.iref(SII.LS,idades,iref)
matriz.classes[,5] <- func.schu.iref(SII.CC,idades,iref)
matriz.classes[,6] <- func.schu.iref(SII.LI,idades,iref)
matriz.classes[,7] <- func.schu.iref(SIII.LS,idades,iref)
matriz.classes[,8] <- func.schu.iref(SIII.CC,idades,iref)
matriz.classes[,9] <- func.schu.iref(SIII.LI,idades,iref)
matriz.classes[,10] <- func.schu.iref(SIV.LS,idades,iref)
matriz.classes[,11] <- func.schu.iref(SIV.CC,idades,iref)
matriz.classes[,12] <- func.schu.iref(SIV.LI,idades,iref)
matriz.classes[,13] <- func.schu.iref(SV.LS,idades,iref)
matriz.classes[,14] <- func.schu.iref(SV.CC,idades,iref)
matriz.classes[,15] <- func.schu.iref(SV.LI,idades,iref)


matriz.classes

S == matriz.classes[7,]


# 4.4 Curvas de sítio ----

func.schu.SI.LS <- function (x) {exp(log(SI.LS) + b1.schu*(1/x - 1/iref))}
func.schu.SII.LS <- function (x) {exp(log(SII.LS) + b1.schu*(1/x - 1/iref))}
func.schu.SIII.LS <- function (x) {exp(log(SIII.LS) + b1.schu*(1/x - 1/iref))}
func.schu.SIV.LS <- function (x) {exp(log(SIV.LS) + b1.schu*(1/x - 1/iref))}
func.schu.SV.LS <- function (x) {exp(log(SV.LS) + b1.schu*(1/x - 1/iref))}
func.schu.SV.LI <- function (x) {exp(log(SV.LI) + b1.schu*(1/x - 1/iref))}

func.schu.SI.CC <- function (x) {exp(log(SI.CC) + b1.schu*(1/x - 1/iref))}
func.schu.SII.CC <- function (x) {exp(log(SII.CC) + b1.schu*(1/x - 1/iref))}
func.schu.SIII.CC <- function (x) {exp(log(SIII.CC) + b1.schu*(1/x - 1/iref))}
func.schu.SIV.CC <- function (x) {exp(log(SIV.CC) + b1.schu*(1/x - 1/iref))}
func.schu.SV.CC <- function (x) {exp(log(SV.CC) + b1.schu*(1/x - 1/iref))}


plot(dados$idade,dados$hdom, main="Classes de Sítio",
     xlab="Idade (anos)",ylab="Hdom (m)",xlim=range(0,25),ylim=range(0,30),
     pch=20,col="darkgrey",cex=1.5)
curve(func.schu.SI.LS,from=min(0),to=max(20),col="black",lwd=1.5,add=T)
curve(func.schu.SII.LS,from=min(0),to=max(20),col="black",lwd=1,add=T)
curve(func.schu.SIII.LS,from=min(0),to=max(20),col="black",lwd=1,add=T)
curve(func.schu.SIV.LS,from=min(0),to=max(20),col="black",lwd=1,add=T)
curve(func.schu.SV.LS,from=min(0),to=max(20),col="black",lwd=1,add=T)
curve(func.schu.SV.LI,from=min(0),to=max(20),col="black",lwd=1,add=T)

curve(func.schu.SI.CC,from=min(0),to=max(20),col="red",lwd=1,lty=5,add=T)
curve(func.schu.SII.CC,from=min(0),to=max(20),col="blue",lwd=1,lty=5,add=T)
curve(func.schu.SIII.CC,from=min(0),to=max(20),col="green",lwd=1,lty=5,add=T)
curve(func.schu.SIV.CC,from=min(0),to=max(20),col="orange",lwd=1,lty=5,add=T)
curve(func.schu.SV.CC,from=min(0),to=max(20),col="purple",lwd=1,lty=5,add=T)
legend("bottomright",legend=c("Classe I","Classe II","Classe III","Classe IV",
       "Classe V"),lty=5,lwd=1,col=c("red","blue","green","orange","purple"))


# Utilizando o ggplot
library(ggplot2)
nx <- seq(1, 20, 0.01)

ggplot() +
  geom_point(aes(x=idade, y = hdom), data = dados,colour="black",size=2,alpha=0.4) +
  geom_ribbon(aes(x = nx,ymin = func.schu.SII.LS(nx), ymax=func.schu.SI.LS(nx)), fill = 'red', alpha = 0.2) +
  geom_ribbon(aes(x = nx,ymin = func.schu.SIII.LS(nx), ymax=func.schu.SII.LS(nx)), fill = 'blue', alpha = 0.2) +
  geom_ribbon(aes(x = nx,ymin = func.schu.SIV.LS(nx), ymax=func.schu.SIII.LS(nx)), fill = 'green', alpha = 0.2) +
  geom_ribbon(aes(x = nx,ymin = func.schu.SV.LS(nx), ymax=func.schu.SIV.LS(nx)), fill = 'orange', alpha = 0.2) +
  geom_ribbon(aes(x = nx,ymin = func.schu.SV.LI(nx), ymax=func.schu.SV.LS(nx)), fill = 'purple', alpha = 0.2) +
  
  stat_function(fun=func.schu.SI.CC,colour='red',lwd=1,alpha=0.5,lty=5) +
  stat_function(fun=func.schu.SII.CC,color='blue',lwd=1,alpha=0.5,lty=5) +
  stat_function(fun=func.schu.SIII.CC,color='green',lwd=1,alpha=0.5,lty=5) +
  stat_function(fun=func.schu.SIV.CC,color='orange',lwd=1,alpha=0.5,lty=5) +
  stat_function(fun=func.schu.SV.CC,color='purple',lwd=1,alpha=0.5,lty=5) +
  
  stat_function(fun=func.schu.SI.LS,color='black',lwd=0.7) +
  stat_function(fun=func.schu.SII.LS,color='black',lwd=0.7) +
  stat_function(fun=func.schu.SIII.LS,color='black',lwd=0.7) +
  stat_function(fun=func.schu.SIV.LS,color='black',lwd=0.7) +
  stat_function(fun=func.schu.SV.LS,color='black',lwd=0.7) +
  stat_function(fun=func.schu.SV.LI,color='black',lwd=0.7) +
  
  labs(x="Idade (anos)",y="Hdom (m)",title="Curvas de Sítio") +
  theme_bw()




############################################################################
############################################################################
###########                                                   ##############
###########                 MODELO NÃO LINEAR                 ##############
###########                                                   ##############
############################################################################



# 1. Importando dados ----

dados <- read.csv2("Dados_Atividade.csv")

View(dados)

# As colunas dentro de 'dados' podem ser acessadas com o cifrão $

dados$idade
dados$hdom

# 2. Visualizando dados de sítio ----

plot(x = dados$idade, y = dados$hdom)

plot(hdom ~ idade, data = dados)


# 3. Ajustando o modelo de Chapman-Richards ----

y <- dados$hdom
x <- dados$idade

mod.chap <- nls(formula = y ~ (b0*(1-(exp(-b1*x)))^b2),
                start = list(b0=20.33,b1=0.11733,b2=0.711317))

summary(mod.chap)

coef(mod.chap)

b0.chap <- coef(mod.chap)[1]
b1.chap <- coef(mod.chap)[2]
b2.chap <- coef(mod.chap)[3]


# 3.3. Gráfico das curvas de índice de sítio ----

x <- dados$idade

func.chap <- function (x) {b0.chap*(1-exp(-b1.chap*x))**b2.chap}

plot(dados$idade,dados$hdom, main="Curvas de índice de Sítio Ajustadas",
     col="darkgrey",xlab="Idade (anos)",ylab="Hdom (m)",
     xlim=range(0,20),ylim=range(0,22),pch=20,cex=1.5)

curve(func.chap,from=min(x),to=max(x),col="darkgreen",lwd=2,add=T)

legend("bottomright",legend=c("Chapman-Richards","Observados"),
       pch=c(NA,16),lty=c(1,NA),lwd=2,col=c("darkgreen", "darkgrey"))



# 4. Construir curvas de índice de sítio com 5 classes ----
# pelo método da curva guia


iref <- 7
dados.iref <- subset(dados,dados$idade == 7)
max.iref <- max(dados.iref$hdom)
max.iref <- 20  #
min.iref <- min(dados.iref$hdom)
min.iref <- 7   #
amp <- max.iref-min.iref
nc <- 5
interv <- amp/nc
idades <- c(1:20)

# 4.1.  Formando os limites Superiores e Inferiores ----

# Sítio I (SI)
SI.LS <- max.iref
SI.LI <- SI.LS - interv
SI.CC <- mean(c(SI.LS,SI.LI))

# Sítio II (SII)
SII.LS <- SI.LI
SII.LI <- SII.LS-interv
SII.CC <- mean(c(SII.LS,SII.LI))

# Sítio III (SIII)
SIII.LS <- SII.LI
SIII.LI <- SIII.LS-interv
SIII.CC <- mean(c(SIII.LS,SIII.LI))

# Sítio IV (SIV)
SIV.LS <- SIII.LI
SIV.LI <- SIV.LS-interv
SIV.CC <- mean(c(SIV.LS,SIV.LI))

# Sítio V (SV)
SV.LS <- SIV.LI
SV.LI <- SV.LS-interv
SV.CC <- mean(c(SV.LS,SV.LI))

# Juntando todos os sítios
S <- c(SI.LS,SI.CC,SI.LI,SII.LS,SII.CC,SII.LI,SIII.LS,SIII.CC,SIII.LI,SIV.LS,SIV.CC,SIV.LI,SV.LS,SV.CC,SV.LI)
S



# 4.2. Estimativas  ----
# Função de Chapman com b0 isolado:

func.chap.iref <- function (S,idades,iref) {(S*(1-exp(-b1.chap*idades))**b2.chap)/(1-exp(-b1.chap*iref))**b2.chap}



# 4.3. Tabela com os limites estimados ----

matriz.classes <- matrix(nrow=20,ncol=15)
rownames(matriz.classes) <- c("1","2","3","4","5","6","7=iref","8","9","10","11",
                              "12","13","14","15","16","17","18","19","20")
colnames(matriz.classes) <- c("I.LS","I.CC","I.LI","II.LS","II.CC","II.LI",
                              "III.LS","III.CC","III.LI","IV.LS","IV.CC","IV.LI",
                              "V.LS","V.CC","V.LI")

matriz.classes


matriz.classes[,1] <- func.chap.iref(SI.LS,idades,iref)
matriz.classes[,2] <- func.chap.iref(SI.CC,idades,iref)
matriz.classes[,3] <- func.chap.iref(SI.LI,idades,iref)
matriz.classes[,4] <- func.chap.iref(SII.LS,idades,iref)
matriz.classes[,5] <- func.chap.iref(SII.CC,idades,iref)
matriz.classes[,6] <- func.chap.iref(SII.LI,idades,iref)
matriz.classes[,7] <- func.chap.iref(SIII.LS,idades,iref)
matriz.classes[,8] <- func.chap.iref(SIII.CC,idades,iref)
matriz.classes[,9] <- func.chap.iref(SIII.LI,idades,iref)
matriz.classes[,10] <- func.chap.iref(SIV.LS,idades,iref)
matriz.classes[,11] <- func.chap.iref(SIV.CC,idades,iref)
matriz.classes[,12] <- func.chap.iref(SIV.LI,idades,iref)
matriz.classes[,13] <- func.chap.iref(SV.LS,idades,iref)
matriz.classes[,14] <- func.chap.iref(SV.CC,idades,iref)
matriz.classes[,15] <- func.chap.iref(SV.LI,idades,iref)


matriz.classes

S == matriz.classes[7,]


# 4.4 Curvas de sítio ----

func.chap.SI.LS <- function (x) {(SI.LS*(1-exp(-b1.chap*x))**b2.chap)/(1-exp(-b1.chap*iref))**b2.chap}
func.chap.SII.LS <- function (x) {(SII.LS*(1-exp(-b1.chap*x))**b2.chap)/(1-exp(-b1.chap*iref))**b2.chap}
func.chap.SIII.LS <- function (x) {(SIII.LS*(1-exp(-b1.chap*x))**b2.chap)/(1-exp(-b1.chap*iref))**b2.chap}
func.chap.SIV.LS <- function (x) {(SIV.LS*(1-exp(-b1.chap*x))**b2.chap)/(1-exp(-b1.chap*iref))**b2.chap}
func.chap.SV.LS <- function (x) {(SV.LS*(1-exp(-b1.chap*x))**b2.chap)/(1-exp(-b1.chap*iref))**b2.chap}
func.chap.SV.LI <- function (x) {(SV.LI*(1-exp(-b1.chap*x))**b2.chap)/(1-exp(-b1.chap*iref))**b2.chap}


func.chap.SI.CC <- function (x) {(SI.CC*(1-exp(-b1.chap*x))**b2.chap)/(1-exp(-b1.chap*iref))**b2.chap}
func.chap.SII.CC <- function (x) {(SII.CC*(1-exp(-b1.chap*x))**b2.chap)/(1-exp(-b1.chap*iref))**b2.chap}
func.chap.SIII.CC <- function (x) {(SIII.CC*(1-exp(-b1.chap*x))**b2.chap)/(1-exp(-b1.chap*iref))**b2.chap}
func.chap.SIV.CC <- function (x) {(SIV.CC*(1-exp(-b1.chap*x))**b2.chap)/(1-exp(-b1.chap*iref))**b2.chap}
func.chap.SV.CC <- function (x) {(SV.CC*(1-exp(-b1.chap*x))**b2.chap)/(1-exp(-b1.chap*iref))**b2.chap}


plot(dados$idade,dados$hdom, main="Classes de Sítio",
     xlab="Idade (anos)",ylab="Hdom (m)",xlim=range(0,25),ylim=range(0,30),
     pch=20,col="darkgrey",cex=1.5)
curve(func.chap.SI.LS,from=min(0),to=max(20),col="black",lwd=1.5,add=T)
curve(func.chap.SII.LS,from=min(0),to=max(20),col="black",lwd=1,add=T)
curve(func.chap.SIII.LS,from=min(0),to=max(20),col="black",lwd=1,add=T)
curve(func.chap.SIV.LS,from=min(0),to=max(20),col="black",lwd=1,add=T)
curve(func.chap.SV.LS,from=min(0),to=max(20),col="black",lwd=1,add=T)
curve(func.chap.SV.LI,from=min(0),to=max(20),col="black",lwd=1,add=T)

curve(func.chap.SI.CC,from=min(0),to=max(20),col="red",lwd=1,lty=5,add=T)
curve(func.chap.SII.CC,from=min(0),to=max(20),col="blue",lwd=1,lty=5,add=T)
curve(func.chap.SIII.CC,from=min(0),to=max(20),col="green",lwd=1,lty=5,add=T)
curve(func.chap.SIV.CC,from=min(0),to=max(20),col="orange",lwd=1,lty=5,add=T)
curve(func.chap.SV.CC,from=min(0),to=max(20),col="purple",lwd=1,lty=5,add=T)
legend("bottomright",legend=c("Classe I","Classe II","Classe III","Classe IV",
                              "Classe V"),lty=5,lwd=1,col=c("red","blue","green","orange","purple"))


# Utilizando o ggplot
library(ggplot2)
nx <- seq(1, 20, 0.01)

ggplot() +
  geom_point(aes(x=idade, y = hdom), data = dados,colour="black",size=2,alpha=0.4) +
  geom_ribbon(aes(x = nx,ymin = func.chap.SII.LS(nx), ymax=func.chap.SI.LS(nx)), fill = 'red', alpha = 0.2) +
  geom_ribbon(aes(x = nx,ymin = func.chap.SIII.LS(nx), ymax=func.chap.SII.LS(nx)), fill = 'blue', alpha = 0.2) +
  geom_ribbon(aes(x = nx,ymin = func.chap.SIV.LS(nx), ymax=func.chap.SIII.LS(nx)), fill = 'green', alpha = 0.2) +
  geom_ribbon(aes(x = nx,ymin = func.chap.SV.LS(nx), ymax=func.chap.SIV.LS(nx)), fill = 'orange', alpha = 0.2) +
  geom_ribbon(aes(x = nx,ymin = func.chap.SV.LI(nx), ymax=func.chap.SV.LS(nx)), fill = 'purple', alpha = 0.2) +
  
  stat_function(fun=func.chap.SI.CC,colour='red',lwd=1,alpha=0.5,lty=5) +
  stat_function(fun=func.chap.SII.CC,color='blue',lwd=1,alpha=0.5,lty=5) +
  stat_function(fun=func.chap.SIII.CC,color='green',lwd=1,alpha=0.5,lty=5) +
  stat_function(fun=func.chap.SIV.CC,color='orange',lwd=1,alpha=0.5,lty=5) +
  stat_function(fun=func.chap.SV.CC,color='purple',lwd=1,alpha=0.5,lty=5) +
  
  stat_function(fun=func.chap.SI.LS,color='black',lwd=0.7) +
  stat_function(fun=func.chap.SII.LS,color='black',lwd=0.7) +
  stat_function(fun=func.chap.SIII.LS,color='black',lwd=0.7) +
  stat_function(fun=func.chap.SIV.LS,color='black',lwd=0.7) +
  stat_function(fun=func.chap.SV.LS,color='black',lwd=0.7) +
  stat_function(fun=func.chap.SV.LI,color='black',lwd=0.7) +
  
  labs(x="Idade (anos)",y="Hdom (m)",title="Curvas de Sítio") +
  theme_bw()


