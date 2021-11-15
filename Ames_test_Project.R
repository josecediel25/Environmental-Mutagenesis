####################################
#####Mutagenicidad UVR-A Y UVR-A en S. typhimurium TA102###
##Jose_Cediel
##  UVA
R_espont?neaA<-c(635,735,960)
RevertantesA_2500<-c(215,230,298)
RevertantesA_5000<-c(448,460,510)
RevertantesA_7500<-c(650,670,690)
RevertantesA_10000<-c(910,850,880)
RevertantesA_15000<-c(910,850,880)
RevertantesA_20000<-c(1320,1310,1315)
RevertantesA_30000<-c(1560,1542,1555)
RevertantesA_40000<-c(1760,1762,1759)


datos<-cbind(R_espont?neaA,RevertantesA_2500,RevertantesA_5000,RevertantesA_7500,RevertantesA_10000,RevertantesA_15000,RevertantesA_20000,RevertantesA_30000,RevertantesA_40000)
View(datos)
write.table(datos, "datos_UVRA.csv", sep=",")
PromA_0<-mean(R_espont?neaA)
PromA_espon<-round(PromA_0, digits = 2) 
no_irra<-0
PromA_1<-mean(round(RevertantesA_2500, digits = 2))
PromA_2500<-round(PromA_1, digits = 2) 

PromA_2<-mean(RevertantesA_5000)
PromA_5000<-round(PromA_2, digits = 2)
PromA_3<-mean(RevertantesA_7500)
PromA_7500<-round(PromA_3, digits = 2)
PromA_4<-mean(RevertantesA_10000)
PromA_10000<-round(PromA_4, digits = 2)
PromA_5<-mean(RevertantesA_20000)
PromA_20000<-round(PromA_5, digits=2)
PromA_6<-mean(RevertantesA_30000)
PromA_30000<-round(PromA_6, digits=2)
PromA_7<-mean(RevertantesA_40000)
PromA_40000<-round(PromA_7, digits=2)

IMA<-c(PromA_2500/PromA_espon,PromA_5000/PromA_espon,PromA_7500/PromA_espon,PromA_10000/PromA_espon, PromA_20000/PromA_espon,PromA_30000/PromA_espon,PromA_40000/PromA_espon )

datos_UVRA<-cbind(PromA_2500,PromA_5000,PromA_7500,PromA_10000,PromA_20000,PromA_30000,PromA_40000)
View(datos_UVRA)
IM_A<-round(IMA, digits = 2)
tabla_UVRA<-rbind(datos_UVRA, IM_A)
View(tabla_UVRA)
dimnames(tabla_UVRA)<-list(c("Revertantes","Indice de mutaci?n(IM)"), c("2500J/m^2", "5000J/m^2","7500J/m^2", "1000J/m^2", "2000J/m^2", "30000J/M^2", "40000J/m^2"))

setwd("D://3er_Semestre_UIS/Mutagenesis_ambienta_II/")
write.table(tabla_UVRA, "UVRA.csv", sep = ";")

##UVR-B
R_espont?neaB<-650
RevertantesB_10<-round(1095, digits = 0)

RevertantesB_20<-round(1400,digits = 0)
RevertantesB_30<-round(1660,digits = 0)
RevertantesB_40<-round(2000,digits = 0)
RevertantesB_80<-round(3728,digits = 0)
N_col_rever<-c(RevertantesB_10,RevertantesB_20,RevertantesB_30,RevertantesB_40,RevertantesB_80)
datos_UVRB<-cbind(RevertantesB_10,RevertantesB_20,RevertantesB_30,RevertantesB_40,RevertantesB_80)
View(datos_UVRB)
IMB<-c(RevertantesB_10/R_espont?neaB,RevertantesB_20/R_espont?neaB,RevertantesB_30/R_espont?neaB,RevertantesB_40/R_espont?neaB,RevertantesB_80/R_espont?neaB)
IM_B<-round(IMB, digits = 2)

IMB5_
IM_B
tabla_UVRB<-rbind(datos_UVRB,IM_B)
View(tabla_UVRB)
dimnames(tabla_UVRB)<-list(c("Revertantes","Indice de mutaci?n(IM)"), c("NO irradiado","10J/m^2", "20J/m^2","30J/m^2", "40J/m^2", "80J/m^2"))
write.table(tabla_UVRB, "UVRB.csv", sep = ";")

##################################
##UVRA
d?sis<-c(2500, 5000,7500, 10000, 20000, 30000, 40000)


Indi_muta<-IM_A
Indi_muta

Num_col_rever<-c(PromA_2500,PromA_5000,PromA_7500,PromA_10000,PromA_20000,PromA_30000,PromA_40000)
par(mfrow=c(1,2))
plot(d?sis, Num_col_rever,type = "o",ylab="N? colonias revertantes",xlab = "D?sis UVR-A (J/M^2)",ylim=c(0,2100),col=c("blue"), main = "REVERTANTES UVR-A")
plot(d?sis, Indi_muta, type = "o",ylab="?ndice de mutaci?n",xlab = "D?sis UVR-A (J/M^2)",ylim=c(0,2.5),col=c("red"), main = "REVERTANTES UVR-A")

##################################
##UVRB
dev.off()
d?sisB<-c(10, 20,30, 40, 80)
par(mfrow=c(1,2))
IN_muta<-IM_B
N_col_rever
plot(d?sisB, N_col_rever, type= "o",ylab="N? colonias revertantes",xlab = "D?sis UVR-B (J/M^2)",ylim=c(0,4000),col=c("blue"), main = "REVERTANTES UVR-B")
plot(d?sisB, IN_muta,type = "o",ylab="?ndice de mutaci?n",xlab = "D?sis UVR-B (J/M^2)",ylim=c(0,6),col=c("red"), main = "REVERTANTES UVR-B")

######Test estad?stico
D1<-c(215,230,298)
D2<-c(448,460,510)
D3<-c(650,670,690)
D4<-c(910,850,880)
D5<-c(910,850,880)
D6<-c(1320,1310,1315)
D7<-c(1560,1542,1555)
D8<-c(1760,1762,1759)

N_revertantes<-c(D1,D2,D3,D4,D5,D6,D7,D8)
D?sis<-rep(c("A","B","C","D","E", "F", "J", "K"),each=3)

tabla<-cbind(D?sis, N_revertantes)

View(tabla)
uvra<-as.data.frame(tabla)
View(uvra)
anova_uvra<-aov(N_revertantes~D?sis)
summary(anova_uvra)
plot(TukeyHSD(anova_uvra))
attach(as.data.frame(tabla))
shapiro.test(N_revertantes)
library(car)

####existen diferencias significativas entre el n?mero de revertantes segun las dosis##

### prueba test studient

####Existen diferencias en el indice de mutacion de UVRA Y UVRB?

IM_A5<-c(PromA_2500/PromA_espon,PromA_5000/PromA_espon,PromA_7500/PromA_espon,PromA_10000/PromA_espon, PromA_20000/PromA_espon)
IM_A5
IMAA<-round(IM_A5, digits = 2)
IMAA
IM_B
t.test(IMAA, IM_B)
####Hay diferencias significativas #####


