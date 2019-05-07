#Doctorat
##Llegir dades des de csv que he exportat prèviament amb SPSS (Fitxer sintaxiss)

#ULL: POSAR VOSTRE DIRECTORI TREBALL: getwd() -> i després enganxar-lo al setwd

setwd("~/Anna/AQU/IL/6a Enquesta 2017/Doctorat")

###Bbdd amb etiquets -> si us surten coses rares amb decimals: vigileu que per R el decimal és un punt
#Abans de llegir-ho amb R Studio vaig canviar configuració decimals

enq <- read.csv("prova2_doc.csv",sep =";", header=TRUE, fill =TRUE)
dim(enq)
str(enq)

#lloc de treball -> dictomitzar Europa i Resta món. Primer provo que està bé


#Fix missing
# Si més d'un tipus de valor de NA:
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


###convertir en factors les variables descriptives
conv_fact <- function(vble){
  vble <- as.factor(vble)
}
enq[ ,1:17] <- lapply(enq[ ,1:17], conv_fact)
str(enq)

names(enq[ ,25:30])
enq[ ,25:30] <- lapply(enq[ ,25:30], conv_fact)
names(enq[ ,51:53])
enq[,51:53]<- lapply(enq[ ,51:53], conv_fact)
enq[,55:59]<- lapply(enq[ ,55:59], conv_fact)
str(enq)




#Descriptiu
table(enq$any_enquesta)
round(prop.table(table(enq$codi_a, enq$tip_tesi),1)*100,2)


#correlacions competències: totes
library(dplyr)
compet <- select(enq, starts_with("n"))
names(compet)
#Treure vbles subset que no toquen
compet$nivest = NULL
compet$nmeto = NULL
#Una altra manera seria fer vector de no_vbles
#nonvars = c("year", "songtitle", "artistname", "songID", "artistID")
#SongsTrain = SongsTrain[ , !(names(SongsTrain) %in% nonvars)] 

#Gràfic correlacions entre competències
library(GGally)
ggcorr(compet)
ggcorr(compet,
       label=TRUE,
       label_alpha = TRUE)

pdf("FigCorrel.pdf") #Grava el gràfic en pdf

pairs(compet)

#Model per veure influència a treball en equip
#Forma de treball i tipus de tesi sembla que funcionin de manera indep pel que fa treb en equip
glm.fit <- lm(ntreb ~ for_treb, data=enq)
summary(glm.fit)

enq$tip_tesi <- relevel(enq$tip_tesi, "1") #poso com a categ de ref monografia
glm.fit <- lm(ntreb ~ for_treb + tip_tesi, data=enq)
summary(glm.fit)

#Capacitat de redacció versus tipus de tesu (0 col·lecció articles, 1 monografia)
#Forma de treball i tipus de tesis estan colinears pel que fa a la capacitat de redacció
glm.fit <- lm(nred ~ tip_tesi, data=enq)
summary(glm.fit)

glm.fit <- lm(nred ~ tip_tesi + for_treb, data=enq)
summary(glm.fit)

#Model adequació a partir d'on feina
enq$on_feina <- relevel(enq$on_feina, "3") #poso com a categ de ref treball a empresa
model1 <- glm(funcDr_c ~ on_feina, data=enq, family=binomial)
summary(model1) #AIC 4185.4
logLik(model1)
(oddsRatio_uni <- exp(model1$coefficients[2]))#prob fer fs Dr si estàs a la uni és 12 cops més gran que si no hi estàs 
(oddsRatio_CR <- exp(model1$coefficients[3])) #prob fer fs Dr si estàs a CR és 8 més gran que si..
#1-exp(model1$coefficients)

#Càlcul probabilits si treballo a uni
#De manera "cutre"
numerador <- exp(model1$coefficients[1]+ model1$coefficients[2]*1 + model1$coefficients[3]*0)
denominador <- (1 + exp(model1$coefficients[1]+ model1$coefficients[2]*1 + model1$coefficients[3]*0))
prob_adeq_si_uni <- numerador/denominador
prob_adeq_si_uni
#De manera "pofessional" -> multiplicant matriu combinacioens vbles per coeficients
vector_coef <- model1$coefficients
vector_coef
names(vector_coef)
n_vbles <- length(model1$coefficients)
n_vbles
matriu <- diag(n_vbles) #diag et crea una matriu identitat (identity matrix)
matriu[ ,1] <- 1 #necessito que beta 0 sempre sigui 1
matriu
logit <-  matriu %*% vector_coef #per multiplicar matrius necessitao %*%
probs <- round((exp(logit) /(1 + exp(logit))*100),2)
row.names(probs) <- names(vector_coef)
colnames(probs) <- "Probabilitat"
probs
 

#Intervals de confiança del model
confint.default(level=0.95,model1 )

#Per calcular logit, odds... 
#logit <- b0 +b1*x1 + b2*x2
#odds <- exp(logit)
#prob y =1 -> 1/(1 + e^(-Logit)) = 1/(1 + e^(1))



#Graficar regressió
regplot=function(x,y){
  fit=lm(y~x)
  plot(x,y)
  abline(fit,col="red")
}


#----------------PROVA COMPARACIÓ MITJANES VIA BOOSTRAP--------------
#Carregues funció Dra- Mine Çetensaya (de la Duke)
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

#intento calcular l'interval bootstrap dels qui fan funcions de doctor, del subconjunt 339
# Initialize the 'boot_means' object:
n <- 100
boot_means <- rep(NA, n)

# Insert your for loop:
for (i in 1:n) {
  samp <- sample(petit$funcDr_c, size= n, replace=TRUE)
  boot_means[i] <- mean(samp)}
hist(boot_means, breaks = 100)

#Això és un rotllet -> la funció de la Mine fa 10.000 bootrspaps amd dues funciosn (ci, i percentils), per numèriques i categòriques

load(url("http://assets.datacamp.com/course/dasi/inference.Rdata"))

#source("http://bit.ly/dasi_inference")
paul = factor(c(rep("yes", 8), rep("no", 0)), levels = c("yes", "no"))
#Ho: p=0.5, Ha: p>0.5
inference(paul, est = "proportion", type = "ht", method = "simulation", success="yes", null= 0.5, alternative="greater")
#p-value = 0.004


#Com seria la funció bootstrap (10.000 mostres), pels qui fan funcions de Dr? 
str(petit)

inference(petit$funcDr_c, type="ci", method="simulation", conflevel=0.95, est="mean", boot_method="se")
#Em surt mitjana 0.6224, i un interval bootrsap de ( 0.5705 , 0.6742 )
prop.table(table(enq17$funcDr_c)) # la real és de 59.47%, dins interval bootstrap

#Dins de petits, hi ha diferència per gènere en la taxa d'ocupació?ç
#Fa boostrap per grup homes, i bootrap pe grup dones
#Et dóna mitjana i sd de les dues, la diferència observada i el valor p-value de la comparació
#Dos types -> ht (hypothesis test -> cal donar null value), ci (confidence interval)
#parameter we're interested in: "mean" (other options are "median", or "proportion"
#The alternative hypothesis can be "less", "greater", or "twosided".
inference(y = petit$funcDr_c, x = petit$sexe, est = "mean", type = "ht", null = 0, alternative = "twosided", method = "simulation")

#enq_no_NA <-na.omit(enq)
#enq_no_NA <-enq[complete.cases(enq), ]
enq_no_NA <- enq[!is.na(enq), ]
summary(enq_no_NA)
dim(enq_no_NA)
#he buscat la 1a mitjana que he trobat, i una dicotòmica per controlar... no té sentit, però endavant
subset_ocupats <- subset(enq, enq$estatus_c == "1")
subset_ocupats <- enq[!is.na(subset_ocupats$impacte1), ]
summary(subset_ocupats)
inference(subset_ocupats$edat_c, subset_ocupats$sexe , type="ht",alternative="twosided", method="simulation", conflevel=0.95, est="mean", boot_method="se")


#---------------------------------
#Models predictius: TRAINING & TEST SET

enq$on_feina <- relevel(enq$on_feina, "3") #poso com a classe de referència treballar a l'empresa

# Install and load caTools package
install.packages("caTools")
library(caTools)

# Randomly split data
table(enq$any_enquesta, enq$funcDr_c) #any 2008 no tinc funcions2_c
any_enq <- unique(enq$any_enquesta)
any_enq

summary(enq$funcDr_c)
enqFunDr <- enq[!is.na(enq$funcDr_c), ] #subset només registres que no tenen NA a vble que vull predir
summary(enqFunDr$funcDr_c)
summary(enqFunDr$foraEsp_c)
length(enqFunDr$funcDr_c) #3967

#enqFunDr <- subset(enq, enq$any_enq!="2008")
table(enqFunDr$any_enquesta, enqFunDr$funcDr_c)
levels(enqFunDr$funcDr_c)

#Baseline model -> l'outcome més freqüent
prop.table(table(enqFunDr$funcDr_c)) # 62% Drs

#creo subset on no tingui NAs a funcion2_c
set.seed(88)
split = sample.split(enqFunDr$any_enquesta, SplitRatio = 0.75)
split

# Create training and testing sets
enqTrain = subset(enqFunDr, split == TRUE)
enqTest = subset(enqFunDr, split == FALSE)

#Model per predir si fan funcions de doctor d'acord amb on feina
#Les variables indepdnents no poden tenir NAs si faig traduccions -> les llargades de vectors no coincidiran
FsDrLog = glm(funcDr_c ~ on_feina, data=enqTrain, family=binomial)
summary(FsDrLog) #AIC 3199.7 (a més petit, millor)

#Model per predir si fan funcions de doctor d'acord amb on feina + si estan fora Esp
FsDrLog <- glm(funcDr_c ~ on_feina + foraEsp_c, data=enqTrain, family = binomial)
summary(FsDrLog) #AIC 3141.7

# Make predictions on training set. Ens dóna probabilits.
predictTrain = predict(FsDrLog, type="response")
#predictTrain = predict(FsDrLog, type="response")>0.94 

# Analyze predictions. Calcula probabilitats
summary(predictTrain)
tapply(predictTrain, enqTrain$funcDr_c, mean) #
length(predictTrain) #2974 #94% em prdiu No, 96% em prediu Sí
length(enqTrain$funcDr_c)  #3707
summary(enqTrain$funcDr_c)

#sensitivity = true positive rate (proporció funcions Dr agafat)
# specificity = true negative rate (prop que no fan funcs Dr agafat)

#Confusion matrix
table(enqTrain$funcDr_c, predictTrain > 0.5) #encerto 2863 casos, però tinc 112 falsos postius

#----------RECALCULAR EPRQUÈ FET AMB MODEL QUE PREDIU FUNCIONS UNI---------------

#La sensitivity em calcula: TP/(TP+FN). És més gran a més baix el thershold (0.5).
#L'especificity em calcula: TN/ (TN+FP). A més alt el thershold, sera més alta (i més baixa la sensitivity)
#sensitivity = probabilitat positivie given true
#specificity = probabilitat negative given true
t1 <- table(enqTrain$funcDr_c, predictTrain > 0.5)
sens05 <- 1436/(1436+412) #TP/(TP+FN) 
sens05 <- t1[2,2]/(t1[2,2]+t1[2,1])#TP/(TP+FN)
spec05 <- 112/(112+2863) # TN/(TN+FP) -> segona casella fila dividit per suma fila
spec05 <- t1[1,1]/(t1[1,1]+t1[1,2])# TN/(TN+FP) -> primera casella fila dividit per suma fila
sens05 #0.9623529
spec05 #0.03764706 -> no és un model bo per predir qui no fa de doctor

#accuracy
accuracy05 <- sum(diag(t1)/margin.table(t1)) #(TP + TN)/ total
accuracy05 #0.59

#si "t" molt alt -> menys prob predir y=1 (funcions Dr)
#si "t" molt baix -> poca probabiitat predir y=0 (no funcions Dr)

# Confusion matrix for threshold of 0.7
table(enqTrain$funcDr_c, predictTrain > 0.7)
table(enqTrain$funcDr_c, predictTrain > 0.2)

#Threshold 0.95
table(enqTrain$funcDr_c, predictTrain > 0.95)
sens95 <- 1666/(1666+22) #TP/(TP+FN)
spec95 <- 90/(90+22) # TN/(TN+FP)
sens95 #>99%
spec95 #80%
t1 <- table(enqTrain$funcDr_c, predictTrain > 0.95)
#accuracy
accuracy095 <- sum(diag(t1)/margin.table(t1)) #(TP + TN)/ total
accuracy095 #0.59

# Sensitivity and specificity
sens02 <- 2863/(112+2863) #TP/(TP+FN)
spec02 <- 112/(112+2863) # TN/(TN+FP)
sens02
spec02
t1 <- table(enqTrain$funcDr_c, predictTrain > 0.2)
#accuracy
accuracy02 <- sum(diag(t1)/margin.table(t1)) #(TP + TN)/ total
accuracy02 #0.59

# comparativa
sens <- c(sens02, sens05, sens95)
spec <- c(spec02, spec05, spec95)
accur <- c(accuracy02, accuracy05, accuracy095)
t_comp <- rbind(sens, spec, accur)
colnames(t_comp) <- c("thereshold 02", "thereshold 05", "thershold095")
(round(t_comp *100,2))

#La final seria
t1 <- table(enqTrain$funcDr_c, predictTrain > 0.95)
#accuracy
accuracy095 <- sum(diag(t1)/margin.table(t1)) #(TP + TN)/ total
accuracy095 #0.59

#Prediccions amb el threshold triat
predictTrain.probs = predict(FsDrLog, type="response")
predictTrain.pred <- ifelse(predictTrain.probs >0.95,"Dr","No Dr")
table(enqTrain$funcDr_c, predictTrain.pred)

#Out of sample prediction
predictTest.probs = predict(FsDrLog, type="response", newdata=enqTest)
predictTest.pred <- ifelse(predictTest.probs >0.95,"Dr","No Dr")
t1 <- table(enqTest$funcDr_c, predictTest.pred)
t1
sens_t1 <- t1[2,1]/(t1[2,1]+ t1[1,2]) #TP/(TP+FN)
spec_t1 <- t1[1,2]/ (t1[1,2]+t1[1,1])# TN/(TN+FP)
sens_t1 # 0.9465241
spec_t1 #0.8108108
accuracy <- sum(diag(t1)/margin.table(t1))
accuracy #0.4344758

#glm.probs=predict(glm.fit,newdata=Smarket[!train,],type="response")
#glm.pred=


#Install and load ROCR package -> Serveix per graficar i trhiar Threshold
install.packages("ROCR")
library(ROCR)


# Prediction function
ROCRpred = prediction(predictTrain, enqTrain$funcDr_c)

# Prediction function. T'ajuda a decidir Threshold sensitivity is in the y axis the false positive rate, or 1 minus the specificity,in hte x axis

# Performance function
ROCRperf = performance(ROCRpred, "tpr", "fpr") 

#Diu què vols que grafiquen i què vols a x i a y axis


# Plot ROC curve
plot(ROCRperf)

#The ROC curve always starts at the point (0, 0), sensitivity = 0. It does not cath Poor cases.
#But you will label all good care cases.meaning you have a false positive rate of 0
#The ROC curve always ends at the point (1,1): sensitivity=1
#you'll catch all of the poor care cases, but you'll label all good cases as poor (false positive rate of 1)
#In the middle, around (0.3, 0.8),you're correctly labeling about 80% of the poor care cases,
#with a 30% false positive rate

#Which threshold choose?
# If you're more concerned with having a high specificity rr low false positive rate, pick the threshold that maximizes the true positive rate while keeping the false positive rate really low.
#A threshold around (0.1, 0.5) on this ROC curve looks like a good choice in this case.
#On the other hand, if you're more concerned with having a high sensitivity or high true positive rate, ick a threshold that minimizes the false positive rate but has a very high true positive rate.
#A threshold around (0.3, 0.8) looks like a good choice in this case.


# Add colors
plot(ROCRperf, colorize=TRUE)

# Add threshold labels
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))


#Compute the test set predictions in R by running the command:
  
  predictTest = predict(FsDrLog, type="response", newdata=enqTest)

#You can compute the test set AUC by running the following two commands in R:
  
  ROCRpredTest = prediction(predictTest, enqTest$funcions2_c)

auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)

#What is the AUC of this model on the test set?
 auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)
auc #0.6785057 -The out of sample accuracy is of 68% (% del temps que predim correctament que fan fs de Dr -> 50% és pur guessing)


###------------------------- SINTAXI SATISFACCIÓ MEDICINA

library(dplyr)
fsat <- select(enq, starts_with("fsat"))
names(fsat)

#Gràfic correlacions entre satisfacció formació
#contingut cursos i relleva`ncia correlacionat
#Satisfacció amb recursos poc relacioant
#satisfacció general relacionat amb contingut.
#Direcció tesi només relacionat amb satsifacció tutoria
`
library(GGally)
ggcorr(fsat)
ggcorr(fsat,
       label=TRUE,
       label_alpha = TRUE)

summary(fsat)#tinc entre 180 i 24 NAs
dim(fsat) #4943    7

#depurat sense NAs
fsat2 <- fsat[complete.cases(fsat), ] #59 casos, 25 vbles
dim(fsat2) #4479    7

#completant NAs amb mice
library(mice)
set.seed(144) # És només pq a tothom li surti igual i no importa
fsat3 <- complete(mice(fsat)) #output mostra 5 iterations i ara tot compelt
summary(fsat3)
dim(fsat3) #4943    7

#Comparació bbdd amb NAs (fsat), bbd amb complete.Cases (fsat2), bbd amb mice (fsat3)
summary(fsat$fsat_gral)
summary(fsat2$fsat_gral)
summary(fsat3$fsat_gral)
#Queda una miiiica més ajusta dla fsat3 > fsat >2 en relació a mitjana fsat
summary(fsat$fsat_dirTesi)
summary(fsat2$fsat_dirTesi)
summary(fsat3$fsat_dirTesi)
#

#Veure com em predueu les altres variables de satisfacció la general. Totes significatives
#les deus que més la dels recursos (0.21), seguit del continugt (0.19), i direcció tesi (0.16)

lm.model <- lm(fsat_gral~.,data=fsat)
summary(lm.model)

#Si controlo per àmbit em continua donant igual
lm.model <- lm(fsat_gral~ codi_a + fsat_contingut + fsat_rellev + fsat_actform + fsat_tutoria + fsat_dirTesi +fsat_recursos,data=enq)
summary(lm.model)


#Vull veure si em prediu la intenció de repetir el doctorat
#Molt bonic: només em surt significatiu àmbit, direcció de tesi i recursos 
model1 <- glm(reptdoct ~ codi_a + fsat_contingut + fsat_rellev + fsat_actform + fsat_tutoria + fsat_dirTesi +fsat_recursos, data=enq, family=binomial)
summary(model1) #AIC 4185.4
logLik(model1)
(oddsRatio <- exp(model1$coefficients[ ]))#

#Hi afegeixo informació sobre qualitat ocupació per veure si m'afecte intenció repetir doctorat
#I infomció tipologia tesi
enq$on_feina <- relevel(enq$on_feina, "3") #poso com a categ de ref treball a empresa
model1 <- glm(reptdoct ~ codi_a + 
                fsat_contingut + fsat_rellev + fsat_actform + fsat_tutoria + fsat_dirTesi +fsat_recursos +
                funcDr_c +on_feina +
                for_treb + tes_emp_aux + part_ext + part_int + dr_internacional
              , data=enq, family=binomial)
summary(model1) #3022.1

#Càlcul probabilits repetir doctorat
#De manera "pofessional" -> multiplicant matriu combinacioens vbles per coeficients
vector_coef <- model1$coefficients
vector_coef
names(vector_coef)
n_vbles <- length(model1$coefficients)
n_vbles
matriu <- diag(n_vbles) #diag et crea una matriu identitat (identity matrix)
matriu[ ,1] <- 1 #necessito que beta 0 sempre sigui 1
matriu
logit <-  matriu %*% vector_coef #per multiplicar matrius necessitao %*%
probs <- round((exp(logit) /(1 + exp(logit))*100),2)
row.names(probs) <- names(vector_coef)
colnames(probs) <- "Probabilitat"
probs
sort(probs)

#Intervals de confiança del model
confint.default(level=0.95,model1 )

odds <- (oddsRatio <- exp(model1$coefficients[ ]))#
ranking_odds <- sort(odds); ranking_odds


#Càlcul probabilits si treballo a uni
#De manera "cutre"
numerador <- exp(model1$coefficients[1]+ model1$coefficients[2]*1 + model1$coefficients[3]*0)
denominador <- (1 + exp(model1$coefficients[1]+ model1$coefficients[2]*1 + model1$coefficients[3]*0))
prob_adeq_si_uni <- numerador/denominador
prob_adeq_si_uni
#De manera "pofessional" -> multiplicant matriu combinacioens vbles per coeficients
vector_coef <- model1$coefficients
vector_coef
names(vector_coef)
n_vbles <- length(model1$coefficients)
n_vbles
matriu <- diag(n_vbles) #diag et crea una matriu identitat (identity matrix)
matriu[ ,1] <- 1 #necessito que beta 0 sempre sigui 1
matriu
logit <-  matriu %*% vector_coef #per multiplicar matrius necessitao %*%
probs <- round((exp(logit) /(1 + exp(logit))*100),2)
row.names(probs) <- names(vector_coef)
colnames(probs) <- "Probabilitat"
probs


#Intervals de confiança del model
confint.default(level=0.95,model1 )

#Per calcular logit, odds... 
#logit <- b0 +b1*x1 + b2*x2
#odds <- exp(logit)
#prob y =1 -> 1/(1 + e^(-Logit)) = 1/(1 + e^(1))



#Surten coeficients negatius -> tenim intercolinearitat
library(corrplot)
par(mfrow=c(1,1))
med5 <- med4[ , -25]
M<-cor(med5)
corrplot(M, type="upper", method="pie")
#ordenar correlació d'acor amb coeficient correlació. "hclust" for hierarchical clustering
corrplot(M, type="lower", order="hclust", method="pie")


#Lasso regression -> per fer front a collinearitat
library(glmnet)
grid=10^seq(10, -2, length=100)
ridge.mod=glmnet(x, y, alpha = 0, lambda = grid)
y=med4$sat_global
x=model.matrix(sat_global~.-1,data=med4) #trec la variable resposta de la bbdd i la poso a vector y
fit.lasso=glmnet(x,y)
plot(fit.lasso,xvar="lambda",label=TRUE)

#gmlet té un funció de cross.validation 
cv.lasso=cv.glmnet(x,y)
plot(cv.lasso)
coef(cv.lasso)

#D'acord amb lasso 5 variables
#ï..sat_coneix
#sat_comunic
#sat_apren
#sat_problem
#sat_autonom
#sat_tics
#sat_innov
#sat_continu

#Model amb la selecció de variables del lasso: Multiple R-squared:  0.6862,	Adjusted R-squared:  0.6549 

lm.model <- lm(sat_global ~ ï..sat_coneix + sat_comunic + sat_apren +  sat_problem +  sat_autonom + sat_tics + sat_innov + sat_continu, data=med4)
par(mfrow=c(2,2))
plot(lm.model)
confint(lm.model, level = 0.95) #intervals de confiança del model
summary(lm.model) #R-squared 69.2%, Adjusted 65.2%

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#  (Intercept)    2.04088    0.57182   3.569 0.000698 ***
#  ï..sat_coneix  0.05223    0.07314   0.714 0.477848    
#  sat_comunic    0.13148    0.08248   1.594 0.116030    
#  sat_apren      0.09870    0.07969   1.238 0.220210    
#  sat_problem    0.16906    0.07690   2.198 0.031662 *  
#  sat_autonom    0.07456    0.06919   1.078 0.285331    
#  sat_tics       0.15387    0.05780   2.662 0.009879 ** 
#  sat_innov      0.05566    0.05538   1.005 0.318780    
#  sat_continu    0.03321    0.08020   0.414 0.680188  


