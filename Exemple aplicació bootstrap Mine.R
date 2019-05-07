##Prova de la funció boostrap de la Dra Mine Çetinkaya-Rundel

load(url("http://assets.datacamp.com/course/dasi/inference.Rdata"))

#source("http://bit.ly/dasi_inference")
paul = factor(c(rep("yes", 8), rep("no", 0)), levels = c("yes", "no"))
#Ho: p=0.5, Ha: p>0.5
inference(paul, est = "proportion", type = "ht", method = "simulation", success="yes", null= 0.5, alternative="greater")
#p-value = 0.004

#Dos types -> ht (hypothesis test -> cal donar null value), ci (confidence interval)
#parameter we're interested in: "mean" (other options are "median", or "proportion"
#The alternative hypothesis can be "less", "greater", or "twosided".


##Aplicació de la funció a la base de dades de doctors

##Llegir bbdd
setwd("~/Anna/AQU/IL/6a Enquesta 2017/Doctorat") #posar adreça on tingueu csv -> get working director: getwd()

###Bbdd amb etiquets -> si us surten coses rares amb decimals: vigileu que per R el decimal és un punt
#Abans de llegir-ho amb R Studio vaig canviar configuració decimals

enq <- read.csv("prova2_doc.csv",sep =";", header=TRUE, fill =TRUE)
dim(enq)
str(enq)

###Depurar bbdd
missing_fixer <- function(na_value) {
  function(x) {
    x[x == na_value] <- NA
    x
  }
}
fix_missing_1 <- missing_fixer(-1)
fix_missing_2 <- missing_fixer(-2)
fix_missing_3 <- missing_fixer(-3)
enq[] <- lapply(enq, fix_missing_1)
enq[] <- lapply(enq, fix_missing_2)
enq[] <- lapply(enq, fix_missing_3)
str(enq)

#enq$lloctreb_c2 <- ifelse(enq$lloctreb_c ==1, 0,1)
enq$foraEsp_c <- ifelse(enq$lloctreb_c ==1, 0,1)
table(enq$lloctreb_c, enq$foraEsp_c)
enq$lloctreb_c = NULL


#Crear dictòmica funcions de doctor a partir funcions_c i eliminar funcions_c
enq$funcDr_c <- ifelse(enq$funcions_c ==1, 1, 0)
table(enq$funcions_c, enq$funcDr_c)
enq$funcions_c = NULL

#Faig petita la meva mostra, per poder veure com funciona boostrap
#1r selecciono 2017 i després em quedo amb el 25% de la seva gent -> a partir d'aquí simulo com funciona el bootstrap

enq17 <- subset(enq, enq$any_enquesta == "2017")
enq17$ocup <- ifelse(enq17$estatus_c==1, 1, 0)
table(enq17$estatus_c , enq17$ocup)

library(caTools)
set.seed(88)
split = sample.split(enq17$ocup, SplitRatio = 0.75)
split

petit <- subset(enq17, split == FALSE) 
table(petit$estatus_c , petit$ocup)
summary(petit$funcDr_c)

#Com seria la funció bootstrap (10.000 mostres), pels qui fan funcions de Dr? 

inference(petit$funcDr_c, type="ci", method="simulation", conflevel=0.95, est="mean", boot_method="se")
#Em surt mitjana 0.6224, i un interval bootrsap de ( 0.5705 , 0.6742 )
prop.table(table(enq17$funcDr_c)) # la real és de 59.47%, dins interval bootstrap

#Dins del subset "petits", hi ha diferència per gènere en la taxa d'ocupació?
inference(y = petit$funcDr_c, x = petit$sexe, est = "mean", type = "ht", null = 0, alternative = "twosided", method = "simulation")



