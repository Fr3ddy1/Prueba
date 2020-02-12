#GRAFICOS ESTUDIO TAREA VANESSA
library(readxl)
library(ggplot2)
library(WriteXLS)
library(tidyverse)

A <- read_excel("C:/Users/Ecuad/Downloads/Excel de practicas de expe.xlsx", 
                     range = "A1:DJ21")

str(A)

#GLOBAL
#EDAD
a1 <- as.data.frame(table(A$Edad))
names(a1) <- c("Edad","Frecuencia")

ggplot(data = a1,aes(x=Edad,y=Frecuencia))+geom_col()

#CARRERAS
a2 <- as.data.frame(table(A$Carrera))
names(a2) <- c("Carrera","Frecuencia")

ggplot(data = a2,aes(x=Carrera,y=Frecuencia))+geom_col()

#FUNCION
graf <- function(a,A){
  a2 <- as.data.frame(table(A[,a]))
  names(a2) <- c(names(A)[a],"Frecuencia")
  
  a3 <- ggplot(data = a2,aes(x=a2[,1],y=Frecuencia))+geom_col()+
    ggtitle (paste0("Frecuencia de variable ",names(a2)[1])) +
    labs(x = names(a2)[1],y = "Frecuencias") + 
    geom_text(aes(y = Frecuencia, label = Frecuencia), 
      position = position_dodge(width = 0.9), size=3, vjust=-1, hjust=0.5 ,col="black")
 
  ggsave(a3, filename = paste0(names(a2)[1],".png"), width = 20, height = 15, units = "cm", dpi = 150)
  
  
   return(a3)
}


graf(a=2,A)
graf(a=3,A)

#for (i in 31:114) {
#  graf(a=i,A)  
#}


#DISCRIMINACION POR CARRERA
le <- levels(as.factor(A$Carrera))

CS <- as.data.frame(A[which(A$Carrera==le[1]),])

medias_cs <- data.frame("Imagen"=names(A)[c(11,12,13,24,25,26,37,38,39,
                                         50,51,52,63,64,65,76,77,78,
                                         89,90,91,102,103,104)],"Media"=rep(0,24))

names(medias_cs)[2] <- paste0("Media_",le[1])

m <- c()

for (i in  c(11,12,13,24,25,26,37,38,39,
                   50,51,52,63,64,65,76,77,78,
                   89,90,91,102,103,104)) {
  
  m[i] <- mean(CS[,i])
  
  
}

medias_cs$Media <- m[which(!is.na(m))]


#FUNCION
med <- function(le,A){
  
  CS <- as.data.frame(A[which(A$Carrera==le),])
  
  medias_cs <- data.frame("Imagen"=names(A)[c(6,11,12,13,24,25,26,37,38,39,
                                              50,51,52,63,64,65,76,77,78,
                                              89,90,91,102,103,104)],"Media"=rep(0,25))
  
  names(medias_cs)[2] <- paste0("Media_",le)
  
  m <- c()
  
  for (i in  c(6,11,12,13,24,25,26,37,38,39,
               50,51,52,63,64,65,76,77,78,
               89,90,91,102,103,104)) {
    
    m[i] <- mean(CS[,i])
    
    
  }
  
  medias_cs[,2]<- m[which(!is.na(m))]
  return(medias_cs)
}

CS <- med(le = le[1],A)
DE <- med(le = le[2],A)
EC <- med(le = le[3],A)
ING <- med(le = le[4],A)
LE <- med(le = le[5],A)
PSI <- med(le = le[6],A)


write.csv(CS, "Medias_Vanessa_cs.csv")
write.csv(DE, "Medias_Vanessa_de.csv")
write.csv(EC, "Medias_Vanessa_ec.csv")
write.csv(ING, "Medias_Vanessa_ing.csv")
write.csv(LE, "Medias_Vanessa_le.csv")
write.csv(PSI, "Medias_Vanessa_psi.csv")

#CONTEO DE SI Y NO POR CARRERA PRIMERAS 4 PREGUNTAS

CS <- as.data.frame(A[which(A$Carrera==le[1]),])

medias_cs <- data.frame("x"=names(A)[c(7,8,9,10)],"SI"=rep(0,4),"NO"=rep(0,4))

names(medias_cs)[1] <- paste0("Conteo_",le[1])

m_si <- c()
m_no <- c()

for (i in  c(7,8,9,10)) {
  
  m_si[i] <- length(which(CS[,i]=="SI"))
  
  m_no[i] <- length(which(CS[,i]=="NO"))
  
}

medias_cs$SI <- m_si[which(!is.na(m_si))]

medias_cs$NO <- m_no[which(!is.na(m_no))]



#FUNCION
med1 <- function(le,A){
  
  CS <- as.data.frame(A[which(A$Carrera==le),])
  
  medias_cs <- data.frame("x"=names(A)[c(7,8,9,10)],"SI"=rep(0,4),"NO"=rep(0,4))
  
  names(medias_cs)[1] <- paste0("Conteo_",le)
  
  m_si <- c()
  m_no <- c()
  
  for (i in  c(7,8,9,10)) {
    
    m_si[i] <- length(which(CS[,i]=="SI"))
    
    m_no[i] <- length(which(CS[,i]=="NO"))
    
  }
  
  medias_cs$SI <- m_si[which(!is.na(m_si))]
  
  medias_cs$NO <- m_no[which(!is.na(m_no))]
  return(medias_cs)
}

med1(le[1],A)
med1(le[2],A)

write.csv(med1(le[1],A), "conteo_cs.csv")
write.csv(med1(le[2],A), "conteo_de.csv")
write.csv(med1(le[3],A), "conteo_ec.csv")
write.csv(med1(le[4],A), "conteo_ing.csv")
write.csv(med1(le[5],A), "conteo_le.csv")
write.csv(med1(le[6],A), "conteo_psi.csv")


#CONTEO GLOBAL PREGUNTAS 1-7

conteo <- data.frame("Conteo"=names(A)[c(14:19,27:32,40:45,53:58,
                                         66:71,79:84,92:97,105:110)],"SI"=rep(0,48),"NO"=rep(0,48))


m_si <- c()
m_no <- c()

for (i in  c(14:19,27:32,40:45,53:58,
             66:71,79:84,92:97,105:110)) {
  
  m_si[i] <- length(which(A[,i]=="SI"))
  
  m_no[i] <- length(which(A[,i]=="NO"))
  
}

conteo$SI <- m_si[which(!is.na(m_si))]

conteo$NO <- m_no[which(!is.na(m_no))]

conteo

write.csv(conteo, "conteo.csv")


#CONTEO REACCIONES
i1 <- table(c(as.character(A[,20]),as.character(A[,21]),as.character(A[,22]),as.character(A[,23])))

i2 <- table(c(as.character(A[,33]),as.character(A[,34]),as.character(A[,35]),as.character(A[,36])))

i3 <- table(c(as.character(A[,46]),as.character(A[,47]),as.character(A[,48]),as.character(A[,49])))

i4 <- table(c(as.character(A[,59]),as.character(A[,60]),as.character(A[,61]),as.character(A[,62])))

i5 <- table(c(as.character(A[,72]),as.character(A[,73]),as.character(A[,74]),as.character(A[,75])))

i6 <- table(c(as.character(A[,85]),as.character(A[,86]),as.character(A[,87]),as.character(A[,88])))

i7 <- table(c(as.character(A[,98]),as.character(A[,99]),as.character(A[,100]),as.character(A[,101])))

i8 <- table(c(as.character(A[,111]),as.character(A[,112]),as.character(A[,113]),as.character(A[,114])))

write.csv(i1, "img1.csv")
write.csv(i2, "img2.csv")
write.csv(i3, "img3.csv")
write.csv(i4, "img4.csv")
write.csv(i5, "img5.csv")
write.csv(i6, "img6.csv")
write.csv(i7, "img7.csv")
write.csv(i8, "img8.csv")
