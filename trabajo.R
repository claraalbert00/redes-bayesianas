# Mònica Freixas, Cecilia Biosca, Clara Albert

# Carreguem llibreries
library(tidyverse)
library(bnlearn)
library(Rgraphviz)
library(gRain)
library(compareGroups)

# Llegim les dades

data=read.table("agaricus-lepiota.data", sep=",")
colnames(data) = c("class","cap-shape","cap-surface","cap-color","bruises","odor","gill-attachment","gill-spacing","gill-size","gill-color","stalk-shape","stalk-root","stalk-surface-above-ring","stalk-surface-below-ring","stalk-color-above-ring","stalk-color-below-ring","veil-type","veil-color","ring-number","ring-type","spore-print-color","population", "habitat")

View(data)
str(data)
dim(data)

#Fent el summary() veiem que no hi ha valors incorrectes
summary(data)
anyNA(data)

# Fem una comparació pels dos grups per treure els features que no ens aporten gaire informació o ens aporten massa
tabla<-compareGroups(class~.,data = data, include.miss = TRUE, method = 4)
(resultatstabla<-createTable(tabla,show.all = TRUE,show.n = TRUE))

# Agrupació cap-shape
levels(data$`cap-shape`) = c(levels(data$`cap-shape`),"a")
for (i in 1:nrow(data)){
  if (data$`cap-shape`[i] != 'x' & data$`cap-shape`[i]!= 'f') data$`cap-shape`[i] = 'a' 
}
data$`cap-shape`= droplevels(data$`cap-shape`)

#Eliminació cap-surface:
data=select(data, -"cap-surface")

#Agrupació cap-color:
levels(data$`cap-color`) = c(levels(data$`cap-color`),"a")
for (i in 1:nrow(data)){
  if (data$`cap-color`[i] != 'g' & data$`cap-color`[i]!= 'n' & data$`cap-color`[i]!= 'e') data$`cap-color`[i] = 'a' 
}
data$`cap-color`= droplevels(data$`cap-color`)

# Eliminació odor
data = select(data, -'odor')

# Eliminació gill-attachment
data = select(data, -'gill-attachment')

# Eliminació gill-color:
data = select(data, -'gill-color')

# Agrupació stalk-root
levels(data$`stalk-root`) = c(levels(data$`stalk-root`),"a","Desconegut")
for (i in 1:nrow(data)){
  if (data$`stalk-root`[i] != '?' & data$'stalk-root'[i]!= 'b') data$`stalk-root`[i] = 'a' 
  if (data$`stalk-root`[i]=='?') data$`stalk-root`[i]='Desconegut'
}
data$`stalk-root`= droplevels(data$`stalk-root`)

# Agrupació stalk-surface-above-ring
levels(data$`stalk-surface-above-ring`) = c(levels(data$`stalk-surface-above-ring`),"a")
for (i in 1:nrow(data)){
  if (data$`stalk-surface-above-ring`[i] != 'k' & data$`stalk-surface-above-ring`[i]!= 's') data$`stalk-surface-above-ring`[i] = 'a' 
}
data$`stalk-surface-above-ring`= droplevels(data$`stalk-surface-above-ring`)

# Eliminació stalk-surface-below-ring
data = select(data,-"stalk-surface-below-ring")

# Agrupació stalk-color-above-ring
levels(data$`stalk-color-above-ring`) = c(levels(data$`stalk-color-above-ring`),"a")
for (i in 1:nrow(data)){
  if (data$`stalk-color-above-ring`[i] != 'p' & data$`stalk-color-above-ring`[i]!= 'w') data$`stalk-color-above-ring`[i] = 'a' 
}
data$`stalk-color-above-ring`= droplevels(data$`stalk-color-above-ring`)

# Eliminació stalk-color-below-ring
data = select(data,-"stalk-color-below-ring")

# Eliminació veil-type
data = select(data, -'veil-type')

# Eliminació veil-color
data = select(data, -'veil-color')

# Eliminació ring-number
data = select(data, -'ring-number')

# Agrupació ring-type
levels(data$`ring-type`) = c(levels(data$`ring-type`),"a")
for (i in 1:nrow(data)){
  if (data$`ring-type`[i] != 'e' & data$`ring-type`[i]!= 'p') data$`ring-type`[i] = 'a' 
}
data$`ring-type`= droplevels(data$`ring-type`)

# Agrupació spore-print-color
levels(data$`spore-print-color`) = c(levels(data$`spore-print-color`),"d","l")
for (i in 1:nrow(data)){
  if (data$`spore-print-color`[i] == 'k' | data$`spore-print-color`[i] == 'n' | data$`spore-print-color`[i] == 'h') data$`spore-print-color`[i] = 'd' 
  else data$`spore-print-color`[i] = 'l'
}
data$`spore-print-color`= droplevels(data$`spore-print-color`)

# Agrupació population
levels(data$`population`) = c(levels(data$`population`),"a")
for (i in 1:nrow(data)){
  if (data$`population`[i] != 's' & data$`population`[i]!= 'v' & data$`population`[i] != 'y') data$`population`[i] = 'a' 
}
data$`population`= droplevels(data$`population`)

# Agrupació habitat
levels(data$`habitat`) = c(levels(data$`habitat`),"a")
for (i in 1:nrow(data)){
  if (data$`habitat`[i] != 'd' & data$`habitat`[i]!= 'g' & data$`habitat`[i] != 'p') data$`habitat`[i] = 'a' 
}
data$`habitat`= droplevels(data$habitat)

#Veiem les taules de freqüències de cada variable
apply(data,2,table)
dim(data)

#Primera idea per extreure la meitat dels casos
#index = 1:nrow(data)
#index = sample(index, length(index)*0.5)
#data = data[index,]
#table(data$class)


#Seleccionem els primers 4000 casos
data = data[1:4000,]
table(data$class)/nrow(data)

#Ens assegurem que totes les variables siguin tipus factor
str(data)


# No tenim cap variable contínua per tant no cal fer discretització. Totes són tipus factor.

# Funció per dividir la base de dades en "training" i "validate"
splitdf<- function(dataframe, seed=NULL){
  if(!is.null(seed)) set.seed(seed)
  index = 1:nrow(dataframe)
  trainingindex = sample(index, length(index)*0.8)
  trainingset = dataframe[trainingindex,]
  validateset = dataframe[-trainingindex,]
  list(trainingset=trainingset, validateset=validateset)
}

splits = splitdf(data, seed=666)
training = splits$trainingset
test = splits$validateset

str(training)
str(test)


#######################################################
# Naive Bayes
#######################################################

# Introduim la blacklist i la whitelist:
atributes=colnames(data[-c(1)])

bl_naive=data.frame(from=rep(atributes,each=13), to=rep(atributes,13)) # black list
wl_naive=data.frame(from=rep("class",13), to=atributes) # white list

xarxa_naive = hc(training, score="bic", whitelist = wl_naive, blacklist = bl_naive)
class(xarxa_naive)
graphviz.plot(xarxa_naive)

# Calculem el BIC score i el logaritme de la funció de versemblança
logLik(xarxa_naive,training)
BIC(xarxa_naive,training)

# Aprenem els paràmetres pel mètode MLE
xarxa.estimada_naive=bn.fit(xarxa_naive,training,method="mle"); xarxa.estimada_naive


#######################################################
# Augmented Bayes
#######################################################

# Introduim la blacklist i la whitelist:
bl_augmented=data.frame(from=atributes, to=rep("class",13)) # black list
wl_augmented=data.frame(from=rep("class",13), to=atributes) # white list

xarxa_augmented = hc(training, score="bic", whitelist = wl_augmented, blacklist = bl_augmented)

class(xarxa_augmented)
graphviz.plot(xarxa_augmented)

# Calculem el BIC score i el logaritme de la funció de versemblança
logLik(xarxa_augmented,training)
BIC(xarxa_augmented,training)

# Aprenem els paràmetres pel mètode MLE
xarxa.estimada_augmented=bn.fit(xarxa_augmented,training,method="mle")

#####################################################
# Validació dels models
#####################################################

#Separem el training en 10 parts

set.seed(123)
k=10
N=dim(training)[1]
Min_index=0
folds=list()
for(i in 1:k){
  if(i==k) Max_index=(floor(N/k))*i+(N%%k)
  else Max_index=(floor(N/k))*i
  
  folds[i]=list(training[(Min_index+1):Max_index,])
  Min_index=Max_index
}

#Validació del naive bayes

xarxa.grain_naive<-suppressWarnings(as.grain(xarxa.estimada_naive))


accuracy_naive=numeric(k)
recall_naive=numeric(k)
especificity_naive=numeric(k)
precision_naive=numeric(k)
F_naive=numeric(k)

for(i in 1:k){
  prediccio=NULL
  CL=NULL
  prueba=NULL
  distribucio=NULL
  #
  for (j in 1:nrow(folds[[i]])){
    if (is.numeric(predict(xarxa.grain_naive,response="class",folds[[i]][j,],predictors=atributes,type="dist")$pred[[1]][1,1])==FALSE) {
      prediccio[[j]]<-NA
      CL[[j]]<-0
      distribucio[[j]]<-c(rep(0,2))
    } else
    {
      prueba[[j]]<-predict(xarxa.grain_naive,response="class", folds[[i]][j,],predictors=atributes,type="dist")
      distribucio[[j]]<-prueba[[j]]$pred[[1]]
      prediccio[[j]]<-dimnames(distribucio[[j]])[[2]][which.max(distribucio[[j]])]
      CL[[j]]<-max(distribucio[[j]])
    }
  }
  confusion.matrix<- as.matrix(table(prediccio,folds[[i]]$class))
  accuracy_naive[i]<-sum(confusion.matrix[1,1]+confusion.matrix[2,2])/sum(confusion.matrix)
  recall_naive[i]<-sum(confusion.matrix[1,1])/sum(confusion.matrix[1,])
  especificity_naive[i]<-sum(confusion.matrix[2,2])/sum(confusion.matrix[2,])
  precision_naive[i]<-sum(confusion.matrix[1,1])/sum(confusion.matrix[,1])
  F_naive[i]<-2*((precision_naive[i]*recall_naive[i])/(precision_naive[i]+recall_naive[i]))
}


#Validació del augmented naive bayes

xarxa.grain_augmented<-suppressWarnings(as.grain(xarxa.estimada_augmented))

accuracy_augmented=numeric(k)
recall_augmented=numeric(k)
especificity_augmented=numeric(k)
precision_augmented=numeric(k)
F_augmented=numeric(k)

for(i in 1:k){
  prediccio=NULL
  CL=NULL
  prueba=NULL
  distribucio=NULL
  #
  for (j in 1:nrow(folds[[i]])){
    if (is.numeric(predict(xarxa.grain_augmented,response="class",folds[[i]][j,],predictors=atributes,type="dist")$pred[[1]][1,1])==FALSE) {
      prediccio[[j]]<-NA
      CL[[j]]<-0
      distribucio[[j]]<-c(rep(0,2))
    } else
    {
      prueba[[j]]<-predict(xarxa.grain_augmented,response="class", folds[[i]][j,],predictors=atributes,type="dist")
      distribucio[[j]]<-prueba[[j]]$pred[[1]]
      prediccio[[j]]<-dimnames(distribucio[[j]])[[2]][which.max(distribucio[[j]])]
      CL[[j]]<-max(distribucio[[j]])
    }
  }
  confusion.matrix<- as.matrix(table(prediccio,folds[[i]]$class))
  accuracy_augmented[i]<-sum(confusion.matrix[1,1]+confusion.matrix[2,2])/sum(confusion.matrix)
  recall_augmented[i]<-sum(confusion.matrix[1,1])/sum(confusion.matrix[1,])
  especificity_augmented[i]<-sum(confusion.matrix[2,2])/sum(confusion.matrix[2,])
  precision_augmented[i]<-sum(confusion.matrix[1,1])/sum(confusion.matrix[,1])
  F_augmented[i]<-2*((precision_augmented[i]*recall_augmented[i])/(precision_augmented[i]+recall_augmented[i]))
}

#Una vegada realitzada la validació del model, comparem els dos models


mean(accuracy_augmented); mean(accuracy_naive)
mean(recall_augmented); mean(recall_naive)
mean(especificity_augmented);mean(especificity_naive)
mean(precision_augmented);mean(precision_naive)
mean(F_augmented);mean(F_naive)


#Xarxa final amb totes les dades
xarxa.final=hc(data,score="bic",whitelist=wl_augmented,blacklist=bl_augmented)
graphviz.plot(xarxa.final)

xarxa.final.estimada=bn.fit(xarxa.final,data,method="mle")

xarxa.final.grain<-suppressWarnings(as.grain(xarxa.final.estimada))
xarxa.final.estimada

#Per donar valors concrets de les variables atributs, buscarem les característiques dels bolets a internet i farem la seva predicció

# Champiñó:
xarxa.evid=setEvidence(xarxa.final.grain, nodes=c("cap-color","bruises",
"gill-spacing","stalk-root","stalk-color-above-ring","ring-type","spore-print-color","population","habitat"),states=c("a","f","w","b","p","a","d","a","g"))
evid=xarxa.evid$evidence
querygrain(xarxa.evid, nodes=c("class"),type="marginal")

#Deixem les variables relacionades amb el color
xarxa.evid=setEvidence(xarxa.final.grain, nodes=c("cap-color","stalk-color-above-ring","spore-print-color"),states=c("a","p","d"))
evid=xarxa.evid$evidence
querygrain(xarxa.evid, nodes=c("class"),type="marginal")


# Chanterelle:
xarxa.evid=setEvidence(xarxa.final.grain, nodes=c("cap-shape","cap-color","bruises",
                                                  "gill-spacing","gill-size","stalk-root","stalk-surface-above-ring","stalk-color-above-ring","spore-print-color"
                                                  ,"habitat"),states=c("x","a","f","c","n","b","a","a","l","g"))
evid=xarxa.evid$evidence
querygrain(xarxa.evid, nodes=c("class"),type="marginal")


# Matamoscas:
xarxa.evid=setEvidence(xarxa.final.grain, nodes=c("cap-shape","cap-color",
                                                  "gill-spacing","gill-size","stalk-shape","ring-type","spore-print-color"
                                                  ),states=c("x","e","c","n","t","a","l"))
evid=xarxa.evid$evidence
querygrain(xarxa.evid, nodes=c("class"),type="marginal")

# Cortinarius orellanus:
xarxa.evid=setEvidence(xarxa.final.grain, nodes=c("cap-shape","cap-color","bruises",
                                                  "gill-spacing","gill-size","stalk-shape","stalk-root","stalk-surface-above-ring","spore-print-color"
                                                  ,"population"),states=c("x","n","f","c","n","e","b","k","l","y"))
evid=xarxa.evid$evidence
querygrain(xarxa.evid, nodes=c("class"),type="marginal")
