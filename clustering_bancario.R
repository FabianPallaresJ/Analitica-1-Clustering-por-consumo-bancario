library(readxl)
library(psych)
library("corrplot")
library(xlsx)
library(moments)


infobanca<-read_excel("infoclientebanca.xlsx")
View(infobanca)

attach(infobanca)
describe(infobanca[8:15])
describe(infobanca)

apply(is.na(infobanca), 2, sum) 



#kurtosis(infobanca)

par(mfrow=c(1,2))
hist(promedio_por_transaccion, main = "histograma de promedio")
hist(Numero_de_transacciones, main = "hist. # transacciones")
dev.off()

par(mfrow=c(1,2))
hist(porcentaje_nacional_total, main = "hist. %nacional")
hist(porcentaje_internacional_total, main = "hist. % internacional")

boxplot(promedio_por_transaccion, Numero_de_transacciones)

infobanca$por_visa <- porcentaje_visa_internacional + porcentaje_visa_nacional
infobanca$por_mc <- porcentaje_mastercard_nacional + porcentaje_mastercard_internacional
infobanca$por_otra <- porcentaje_otrafranquicia_internacional + Porcentaje_otrafranquicia_nacional
infobanca$por_finde <- porcVIERNES + porcSABADO + porcDOMINGO
infobanca$por_entres <- porcLUNES + porcMARTES + porcMIERCOLES + porcJUEVES
describe(infobanca)
quito <- c(-1,-2,-5,-6,-7,-8,-9,-10,-11,-12,-13,-19,-20,-21,-22,-23,-24,-25, -26)
infobanca <- infobanca[,quito]

describe(infobanca)
banca1 <- infobanca[,1:4]
banca2 <- infobanca[,7]
banca3 <- infobanca[,10]
banca4 <- as.data.frame(cbind(banca1, banca2, banca3))
banca5 <- apply(banca4,2,log1p)

bancafinal<-as.data.frame(cbind(banca5,porcentaje_manana, porcentaje_tarde, infobanca$por_visa, infobanca$por_mc, infobanca$por_finde, infobanca$por_entres))
colnames(bancafinal)[9:12]<-c("por_visa","por_mc","por_fds","por_sem")

describe(bancafinal)

banca <- as.data.frame(scale(bancafinal))
describe(banca)

banca[6] <- NULL
banca[3] <- NULL
banca[4] <- NULL
describe(banca)
boxplot(banca$porcentaje_nacional_total)
par(mfrow=c(2,2))

set.seed(5935)
#calculo la suma de cuadrados total
wss <- (nrow(banca)-1)*sum(apply(banca,2,var))
dev.off()
#calculo para cada solución de clustering 
for (i in 2:15) wss[i] <- sum(kmeans(banca,
                                     centers=i, nstart=10)$withinss)
plot(1:15, wss, type="b", xlab="Número de Clusters",
     ylab="Suma de cuadrados") 


library("cluster")
library("fpc")
# Evaluar usando el criterio ASW (average sillouethe width)
set.seed(2) #Para evitar aleatoriedad en los resultados


ratio <- sample(47871,4000)
a<-banca[ratio,]
  
dim(a)

clustering.asw <- kmeansruns(a,krange=2:15,criterion="asw",iter.max=100, runs= 100,critout=TRUE)
clustering.asw$bestk

gscar<-clusGap(a,FUN=kmeans,K.max=15,B=60)
gscar


bcluster<-kmeans(banca,centers=7,nstart=10,iter.max=20)
bcluster$size
bcluster$iter
bcluster$centers

library(ggplot2)

centros <- as.data.frame(bcluster$centers)
centros$grupos <- as.factor(rownames(centros))
centrosheat <- reshape2::melt(centros)
colnames(centrosheat) <- c("grupo", "variable", "centroide")
ggplot(centrosheat, aes(x = grupo, y = variable, fill = centroide, label = sprintf("%0.2f", round(centroide,digits = 2))))+geom_tile()+scale_fill_distiller(palette="RdBu")+geom_text()

clusplot(banca,bcluster$cluster, color=TRUE)


