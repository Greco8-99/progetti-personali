library(readxl)
calciatori<- read_excel("Statistiche_Calciatori.xls")
calciatori<-data.frame(calciatori[,2:18], row.names=calciatori$Giocatore)

calciatori
str(calciatori)
calciatori$Posizione=as.factor(calciatori$Posizione)
calciatori$Squadra=as.factor(calciatori$Squadra)
View(calciatori)
attach(calciatori)





#ANALISI ESPLORATIVA
summary(calciatori)
mySummary <- function(value) {
  qq <- quantile(value, na.rm = TRUE)
  mv <- c(mean(value, na.rm = TRUE), sd(value, na.rm = TRUE))
  di <- qq[4]-qq[2]
  tmp <- round(c(mv, qq[c(1, 5, 2:4)], di), 2)
  names(tmp) <- c("Media", "SD", "Min", "Max", "Q1",
                  "Mediana", "Q3", "DI")
  return(tmp)
}
apply(calciatori[,-c(1,2)],2,mySummary)


panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor,  ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}
pairs(calciatori[,-c(1,2)], pch=16, lower.panel=panel.cor, diag.panel=panel.hist, upper.panel=panel.smooth,
      cex.labels = 1.5, col.axis= "dark green", cex.axis=1, cex=0.7, col.lab="dark green", font.labels =2,
      cex.cor= 3)
cor(calciatori[-c(1,2)])


install.packages("descr")
library(descr)

summary(calciatori)

install.packages("fBasics")
library(fBasics)
basicStats(calciatori[,-c(1,2)]) #funzione pi� esaustiva della summary()
install.packages("labstatR")
install.packages("psych")
library(psych)
descrizione=describeBy(calciatori[,-c(1,2)], Posizione, na.rm=TRUE)
names(descrizione)
#Statistiche descrittive univariate con variabile di classificazione
cen=calciatori[44:113,]
barplot(descrizione$Att[-c(2,6,12),"mean"], col = rainbow(12),  main="ATTACCANTI", space = F, horiz = T)
legend("bottomright", legend = c("Valore_mln","Et�","Presenze","Titolare","Goal","Tiri",
                             "Tiri_Porta","Dribb_Riusciti","Assist","Pass_Chiave",
                             "Pall_Rubati","Tackle"),
       fill = palette(rainbow(12)), bg = "white", title = "VARIABILI", cex = 0.8)
barplot(descrizione$Cen[-c(2,6,12),"mean"], col = rainbow(12), main="CENTROCAMPISTI", space = F, horiz = T)
legend("bottomright", legend = c("Valore_mln","Et�","Presenze","Titolare","Goal","Tiri",
                             "Tiri_Porta","Dribb_Riusciti","Assist","Pass_Chiave",
                             "Pall_Rubati","Tackle"),
       fill = palette(rainbow(12)), bg = "white", title = "VARIABILI", cex = 0.8)
barplot(descrizione$Dif[-c(2,6,12),"mean"], col = rainbow(12), main="DIFENSORI", space = F, horiz = T)
legend("bottomright", legend = c("Valore_mln","Et�","Presenze","Titolare","Goal","Tiri",
             "Tiri_Porta","Dribb_Riusciti","Assist","Pass_Chiave",
             "Pall_Rubati","Tackle"),
  fill = palette(rainbow(12)), bg = "white", title = "VARIABILI", cex = 0.8)
table1<-table(descrizione$Att,descrizione$Cen,descrizione$Dif)
barplot(table1, beside=T)
descrizione$Att$mean
descrizione$Cen$mean
descrizione$Dif$mean

#ANALISI BIVARIATA - UGUAGLIANZA TRA MEDIE (CAMPIONI INDIPENDENTI)
calciatori

#confronto medie tra attaccanti e difensori
att=calciatori[1:43,]
dif=calciatori[114:176,]
attaccanti=att$Valore_mln #campione 1
difensori=dif$Valore_mln #campione 2
tabella=cbind(summary(attaccanti),summary(difensori))
colnames(tabella)=c("Attaccanti","Difensori")
tabella
par(mfrow=c(1,2))
hist(attaccanti, main="Valori di mercato attaccanti", xlab="Attaccanti")
hist(difensori, main="Valori di mercato difensori", xlab="Difensori")
par(mfrow=c(1,1))

media1=mean(attaccanti)
media2=mean(difensori)
media1
media2
d=media1-media2
d


boxplot(attaccanti,difensori, names=c("ATTACCANTI","DIFENSORI"), col=c("pink","yellow"))
outliers=boxplot(attaccanti,difensori, names=c("ATTACCANTI","DIFENSORI"), col=c("pink","yellow"))$out
outliers

shapiro.test(attaccanti)
shapiro.test(difensori)

#utilizziamo il test di Mann-Whitney
wilcox.test(attaccanti,difensori, correct=FALSE)
#accetto l'ipotesi nulla




#confronto medie tra calciatori di 2 squadre diverse
juv=calciatori[Squadra=="JUV",]
int=calciatori[Squadra=="INT",]
juve=juv$Variazione_val #campione 1
inter=int$Variazione_val #campione 2
tabella=cbind(summary(juve),summary(inter))
colnames(tabella)=c("Juve","Inter")
tabella
par(mfrow=c(1,2))
hist(juve, main="Valori di mercato Juventus", xlab="Juventus",
     col=c("gray8","gray30","gray47","gray66","gray80","white"))
hist(inter, main="Valori di mercato Inter", xlab="Inter",
     col=c("midnightblue","blue3","mediumblue","dodgerblue2","lightskyblue","lightsteelblue1","white"))
par(mfrow=c(1,1))

media1=mean(juve)
media2=mean(inter)
media1
media2
d=abs(media1-media2)
d

boxplot(juve,inter, names=c("JUVENTUS","INTER"), col=c("white","blue"))

shapiro.test(juve)
shapiro.test(inter)

var.test(juve,inter)

t.test(juve,inter, var.equal=T, althernative="less")




#ANLAISI COMPONENTI PRINCIPALI
calciatori.pca <- princomp(calciatori[,c(6:17)], cor=TRUE, scores=TRUE)
calciatori.pca
names(calciatori.pca)
str(calciatori.pca)
summary(calciatori.pca)
calciatori.pca$loadings
#Comp 1 � direttamente proporzionale a PTS, Presenze, Titolare, Minuti
#Comp 2 � direttamente proporzionale a Pass.Riusciti, Pall.Recuperati, Tackle e inversamente proporzionale a Tiri.Porta, Goal, Tiri
calciatori.pca$scores
plot(calciatori.pca) #mi produce un grafico delle varianze (ordinate) delle comp principali, ovvero gli autovalori della matrice di varianze e covarianze
screeplot(calciatori.pca, type=c("lines"), main = "Scree plot")
abline(h=1, lwd=3, col="red")


biplot(calciatori.pca, col=c("black","red"))
library(devtools) #installare devtools
install_github("vqv/ggbiplot")
library(ggbiplot)
ggbiplot(calciatori.pca)

# ggbipplot con nomi osservazioni
ggbiplot(calciatori.pca,
         labels=rownames(calciatori), labels.size = 4)

# biplot con individuazione gruppi
ggbiplot(calciatori.pca,
         labels=rownames(calciatori),
         groups=Posizione,
         ellipse=TRUE,
         labels.size = 3)



#ANALISI CLUSTER
calciatori.p=calciatori[,c(6:17)]
n<-nrow(calciatori.p)
p<-ncol(calciatori.p)

medie<-colMeans(calciatori.p)
scarto<-apply(calciatori.p, 2, sd)
CV <- medie/scarto
descrittive<-round(cbind(medie, scarto, CV),2)
descrittive

calc<-scale(calciatori[,c(6:17)]) #standardizzo le variabili
calc

calciatori.d<-dist(calc)
str(calciatori.d)
calciatori.d #matrice di dissomiglianza


agglo <- function(hc){
  data.frame(row.names=paste0("Cluster",seq_along(hc$height)),
             height=hc$height,
             components=ifelse(hc$merge<0,
                               hc$labels[abs(hc$merge)], paste0("Cluster",hc$merge)),
             stringsAsFactors=FALSE) }

par(mfrow=c(1,2))
par(mfrow=c(1,1))
### legame singolo
h1<-hclust(calciatori.d, method="single")
agglo(h1)
plot(h1,hang=-1,cex=.5)
rect.hclust(h1,4)

### legame completo
h2<-hclust(calciatori.d, method="complete")
agglo(h2)
plot(h2,hang=-1,cex=.5)
rect.hclust(h2,4)

### legame medio
h3<-hclust(calciatori.d, method="average")
agglo(h3)
plot(h3,hang=-1,cex=.5)
rect.hclust(h3,4)

### metodo del centroide
h4<-hclust(calciatori.d, method="centroid")
agglo(h4)
plot(h4,hang=-1,cex=.5)
rect.hclust(h4,4)

### metodo di Ward
h5<-hclust(calciatori.d, method="ward.D2")
agglo(h5)
plot(h5,hang=-1,cex=.5, xlab = "Metodo di Ward")
rect.hclust(h5,4)
par(mfrow=c(1,1))

single <- cutree(h1, 4)
complete <- cutree(h2, 4)
average<- cutree(h3, 4)
ward<- cutree(h5, 4)
centroide<-cutree(h4, 4)

t=table(ward)
table1<-cbind(single,complete, average,centroide,ward)
table1


### confronto tra metodi diversi con g=4 usando tabelle a doppia entrata
table(complete,ward)
table(single,ward)
table(centroide,ward)
table(average,ward)

h5cluster<- cutree(h5, k=4)
medie<-aggregate(calciatori.p, list(h5cluster), mean); medie
### altre descrittive
min<-aggregate(calciatori.p, list(h5cluster), min); min
max<-aggregate(calciatori.p, list(h5cluster), max); max
devstd<-aggregate(calciatori.p, list(h5cluster), sd); devstd

#### grafici interpretazione cluster
# organizzo i dati per usare la funzione ggplot
install.packages("ggplot2", dependencies=TRUE)
install.packages("reshape", dependencies=TRUE)
mydata<-calciatori[,c(6:17)]
col<-colnames(mydata)
mydataz<-data.frame(scale(mydata))
mydataz$clu<-h5cluster
library(reshape2)
dati <- melt(mydataz, measure.vars=col)
## carico il pacchetto grafico ggplot2
library(ggplot2)
ggplot(dati, aes(x = variable, y = value, color=variable)) +
  geom_boxplot() +
  facet_wrap(~ clu)


ggbiplot(calciatori.pca,
         labels=rownames(calciatori),
         groups=ward,
         labels.size = 4,
         ellipse=TRUE)

library(factoextra)
library(tidyverse)
PCA <- princomp(calciatori[,c(6:17)], cor = TRUE)
df <- cbind(calciatori.p, calciatori.pca$scores)
d <- dist(df[,13:14])
res <- hclust(d, method = "ward.D2")
res <- hcut(df[,13:14], k = 4, hc_func = "hclust", hc_method = "ward.D2")
res
plot(res,hang=-1,cex=.5, xlab = "Metodo di Ward")
rect.hclust(h5,4)
df$cluster_1 <- res$cluster
df[,-c(1:9)] %>%
  group_by(cluster_1) %>%
  summarise_all(mean)

fviz_cluster(res, pch=16)
medie<-aggregate(calciatori.p, list(h5cluster), mean); medie



#REGRESSIONE LINEARE MULTIPLA
library("PerformanceAnalytics")
chart.Correlation(calciatori[,-c(1,2)], histogram=T, pch=16, cex=3)

shapiro.test(log(Valore_mln))


modello1<-lm(log(Valore_mln)~.,data=calciatori[,-c(1:4)])
summary(modello1)
shapiro.test(modello1$residuals)
par(mfrow=c(1,2))
plot(modello1, pch=16)
par(mfrow=c(1,1))


#normalit� dei residui
#residui<-residuals(modello1) ##vettore dei residui
#shapiro<-shapiro.test(residui)
#shapiro
#qqnorm(scale(residui))
#abline(0,1)


#library(lmtest)
#omoschedasticit� dei residui
#modello<-formula(modello1)## memorizziamo la formula del modello in un oggettoper facilit� di manipolazione
#testbp<-bptest(modello,data=calciatori[,-c(1:4)])## test di Breusch-Pagan
#testbp

#linearit� dei residui
#plot(modello1,which=1,pch=16,sub='')

#correlazione seriale
#dw<-dwtest(modello,data=calciatori[,-c(1:4)]) ## test di Durbin-Watson
#dw

modello2=step(modello1)
summary(modello2)
shapiro.test(modello2$residuals)
par(mfrow=c(2,2))
plot(modello2, pch=16)
par(mfrow=c(1,1))

#omoschedasticit� dei residui
#modello<-formula(modello2)## memorizziamo la formula del modello in un oggettoper facilit� di manipolazione
#testbp<-bptest(modello,data=calciatori[,-c(1:4)])## test di Breusch-Pagan
#testbp
#scatter.smooth(modello2$fitted.values,abs(res))

#linearit� dei residui
#plot(modello2,which=1,pch=16,sub='')

#correlazione seriale
#dw<-dwtest(modello,data=calciatori[,-c(1:4)]) ## test di Durbin-Watson
#dw

#lev = hatvalues(modello2)
#p = length(modello2$coefficients)-1
#n = nrow(calciatori)
#plot(lev)
#abline(h=2*(p+1)/n, col="red")
#sum(lev > 2*(p+1)/n)
#mean(lev > 2*(p+1)/n)*100

#res = rstudent(modello2)
#plot(res)
#hist(res)
#plot(res)
#abline(h=c(2,-2),col="red")
#hist(res)
#abline(v=c(-2,+2),col="red")
#mean(res < -2 | res >+2)*100

#cookdist = cooks.distance(modello2)
#plot(cookdist)
#abline(h=4/n, col="red",lwd=2)
#mean(cookdist > 4/n)*100

library(car) #load the library
influencePlot(modello2, col="dark green")
summary(influencePlot(modello2))





data(SLID, package = "carData")

# creo la tabella tab1
tab1 <- table(SLID$language)

library(LabRS)
exptab(list(tab1), file = "tabelle.csv")
