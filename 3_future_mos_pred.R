#model for mosquito and climate 
#projection for future mosquitoes(list=ls())
library(mgcv)
library(rJava)
library(xlsx)
library(R.matlab)
library(stringr)
data1<-read.xlsx("E:/?Ǹ/mos1x",1,encoding="UTF-8")
data2<-read.xlsx("E:/?Ǹ?moa2x",2,encoding="UTF-8")

###occurrence data of the two mosquito species and climate observations########
fml1<-mos~prmax+pran+tmean+tmin
fml2<-mos~prmax+pran+tmax+tmin

#fml<-mos~prmax+prmin+pran+tmax+tmin+tmean+gdd
#fml<-mos~prmax+pran+tmax+tmean

my.gam <- function(formula, data, family = binomial, k = 4) {
  response.name <- as.character(formula)[2]
  predictor.names <- labels(terms(formula))
  categorical <- sapply(data[,predictor.names], is.logical) |
    sapply(data[,predictor.names], is.factor)
  formula <- paste(response.name, "~",
                   paste(predictor.names[categorical], collapse = "+"),
                   paste("s(", predictor.names[!categorical], ", k=", k, ")", collapse = "+"))
  formula <- as.formula(formula)
  fit <- gam(formula, data, family = family)
  return(fit)
}

# fit a generalized additive model (GAM)
model1 <- my.gam(fml1, data1)
#summary(model1)
#par(mfrow=c(2,3))
#plot(model1,scheme=1,pages=1,residuals=TRUE)

model2 <- my.gam(fml2, data2)

#summary(model2)
#par(mfrow=c(2,3))
#plot(model2,scheme=1,pages=1,residuals=TRUE)

# δ��????Ԥ??
futuredata=readMat('E:/?Ǹ?????red.mat')

##
mdata=futuredata$asianjianmo.his
d=attributes(mdata)#???ݳ???
pred1=vector(length=0)
pred2=vector(length=0)
for (i in 1:d$dim[3]){
  new1=data.frame(prmax=mdata[1,,i],prmin=mdata[2,,i],pran=mdata[3,,i],tmax=mdata[4,,i],tmin=mdata[5,,i],tmean=mdata[6,,i],gdd=mdata[7,,i])
  p1=predict.gam(model1,new1, type = "response", progress = TRUE)
  p2=predict.gam(model2,new1, type = "response", progress = TRUE)
  pred1=cbind(pred1,p1)
  pred2=cbind(pred2,p2)
}
write.csv(pred1,file="E:/?Ǹ?????s.csv",row.names=FALSE)
write.csv(pred2,file="E:/?Ǹ???????/1v",row.names=FALSE)

##
mdata=futuredata$asianjianmo.15
d=attributes(mdata)#???ݳ???
pred1=vector(length=0)
pred2=vector(length=0)
for (i in 1:d$dim[3]){
  new1=data.frame(prmax=mdata[1,,i],prmin=mdata[2,,i],pran=mdata[3,,i],tmax=mdata[4,,i],tmin=mdata[5,,i],tmean=mdata[6,,i],gdd=mdata[7,,i])
  p1=predict.gam(model1,new1, type = "response", progress = TRUE)
  p2=predict.gam(model2,new1, type = "response", progress = TRUE)
  pred1=cbind(pred1,p1)
  pred2=cbind(pred2,p2)
}
write.csv(pred1,file="E:/?Ǹ???????/1910?asianmos1_15.csv",row.names=FALSE)
write.csv(pred2,file="E:/asianmos2_15.csv",row.names=FALSE)

##
mdata=futuredata$asianjianmo.20
d=attributes(mdata)#???ݳ???
pred1=vector(length=0)
pred2=vector(length=0)
for (i in 1:d$dim[3]){
  new1=data.frame(prmax=mdata[1,,i],prmin=mdata[2,,i],pran=mdata[3,,i],tmax=mdata[4,,i],tmin=mdata[5,,i],tmean=mdata[6,,i],gdd=mdata[7,,i])
  p1=predict.gam(model1,new1, type = "response", progress = TRUE)
  p2=predict.gam(model2,new1, type = "response", progress = TRUE)
  pred1=cbind(pred1,p1)
  pred2=cbind(pred2,p2)
}
write.csv(pred1,file="E:/asianmos1_20.csv",row.names=FALSE)
write.csv(pred2,file="E:/asianmos2_20.csv",row.names=FALSE)

##
mdata=futuredata$asianjianmo.25
d=attributes(mdata)#???ݳ???
pred1=vector(length=0)
pred2=vector(length=0)
for (i in 1:d$dim[3]){
  new1=data.frame(prmax=mdata[1,,i],prmin=mdata[2,,i],pran=mdata[3,,i],tmax=mdata[4,,i],tmin=mdata[5,,i],tmean=mdata[6,,i],gdd=mdata[7,,i])
  p1=predict.gam(model1,new1, type = "response", progress = TRUE)
  p2=predict.gam(model2,new1, type = "response", progress = TRUE)
  pred1=cbind(pred1,p1)
  pred2=cbind(pred2,p2)
}
write.csv(pred1,file="E:/asianmos1_25.csv",row.names=FALSE)
write.csv(pred2,file="E:/asianmos2_25.csv",row.names=FALSE)

##
mdata=futuredata$asianjianmo.30
d=attributes(mdata)#???ݳ???
pred1=vector(length=0)
pred2=vector(length=0)
for (i in 1:d$dim[3]){
  new1=data.frame(prmax=mdata[1,,i],prmin=mdata[2,,i],pran=mdata[3,,i],tmax=mdata[4,,i],tmin=mdata[5,,i],tmean=mdata[6,,i],gdd=mdata[7,,i])
  p1=predict.gam(model1,new1, type = "response", progress = TRUE)
  p2=predict.gam(model2,new1, type = "response", progress = TRUE)
  pred1=cbind(pred1,p1)
  pred2=cbind(pred2,p2)
}
write.csv(pred1,file="E:/asianmos1_30.csv",row.names=FALSE)
write.csv(pred2,file="E:/asianmos2_30.csv",row.names=FALSE)

##
mdata=futuredata$asianjianmo.35
d=attributes(mdata)#???ݳ???
pred1=vector(length=0)
pred2=vector(length=0)
for (i in 1:d$dim[3]){
  new1=data.frame(prmax=mdata[1,,i],prmin=mdata[2,,i],pran=mdata[3,,i],tmax=mdata[4,,i],tmin=mdata[5,,i],tmean=mdata[6,,i],gdd=mdata[7,,i])
  p1=predict.gam(model1,new1, type = "response", progress = TRUE)
  p2=predict.gam(model2,new1, type = "response", progress = TRUE)
  pred1=cbind(pred1,p1)
  pred2=cbind(pred2,p2)
}
write.csv(pred1,file="E:/asianmos1_35.csv",row.names=FALSE)
write.csv(pred2,file="E:/asianmos2_35.csv",row.names=FALSE)


##
mdata=futuredata$asianjianmo.40
d=attributes(mdata)#???ݳ???
pred1=vector(length=0)
pred2=vector(length=0)
for (i in 1:d$dim[3]){
  new1=data.frame(prmax=mdata[1,,i],prmin=mdata[2,,i],pran=mdata[3,,i],tmax=mdata[4,,i],tmin=mdata[5,,i],tmean=mdata[6,,i],gdd=mdata[7,,i])
  p1=predict.gam(model1,new1, type = "response", progress = TRUE)
  p2=predict.gam(model2,new1, type = "response", progress = TRUE)
  pred1=cbind(pred1,p1)
  pred2=cbind(pred2,p2)
}
write.csv(pred1,file="E:/asianmos1_40.csv",row.names=FALSE)
write.csv(pred2,file="E:/asianmos2_40.csv",row.names=FALSE)

##
mdata=futuredata$asianjianmo.45
d=attributes(mdata)#???ݳ???
pred1=vector(length=0)
pred2=vector(length=0)
for (i in 1:d$dim[3]){
  new1=data.frame(prmax=mdata[1,,i],prmin=mdata[2,,i],pran=mdata[3,,i],tmax=mdata[4,,i],tmin=mdata[5,,i],tmean=mdata[6,,i],gdd=mdata[7,,i])
  p1=predict.gam(model1,new1, type = "response", progress = TRUE)
  p2=predict.gam(model2,new1, type = "response", progress = TRUE)
  pred1=cbind(pred1,p1)
  pred2=cbind(pred2,p2)
}
write.csv(pred1,file="E:/asianmos1_45.csv",row.names=FALSE)
write.csv(pred2,file="E:/asianmos2_45.csv",row.names=FALSE)

##
mdata=futuredata$asianjianmo.50
d=attributes(mdata)#???ݳ???
pred1=vector(length=0)
pred2=vector(length=0)
for (i in 1:d$dim[3]){
  new1=data.frame(prmax=mdata[1,,i],prmin=mdata[2,,i],pran=mdata[3,,i],tmax=mdata[4,,i],tmin=mdata[5,,i],tmean=mdata[6,,i],gdd=mdata[7,,i])
  p1=predict.gam(model1,new1, type = "response", progress = TRUE)
  p2=predict.gam(model2,new1, type = "response", progress = TRUE)
  pred1=cbind(pred1,p1)
  pred2=cbind(pred2,p2)
}
write.csv(pred1,file="E:/asianmos1_50.csv",row.names=FALSE)
write.csv(pred2,file="E:/asianmos2_50.csv",row.names=FALSE)

##
mdata=futuredata$cityjianmo.his
d=attributes(mdata)#???ݳ???
pred1=vector(length=0)
pred2=vector(length=0)
for (i in 1:d$dim[3]){
  new1=data.frame(prmax=mdata[1,,i],prmin=mdata[2,,i],pran=mdata[3,,i],tmax=mdata[4,,i],tmin=mdata[5,,i],tmean=mdata[6,,i],gdd=mdata[7,,i])
  p1=predict.gam(model1,new1, type = "response", progress = TRUE)
  p2=predict.gam(model2,new1, type = "response", progress = TRUE)
  pred1=cbind(pred1,p1)
  pred2=cbind(pred2,p2)
}
write.csv(pred1,file="E:/citymos1_his.csv",row.names=FALSE)
write.csv(pred2,file="E:/citymos2_his.csv",row.names=FALSE)

##
mdata=futuredata$cityjianmo.15
d=attributes(mdata)#???ݳ???
pred1=vector(length=0)
pred2=vector(length=0)
for (i in 1:d$dim[3]){
  new1=data.frame(prmax=mdata[1,,i],prmin=mdata[2,,i],pran=mdata[3,,i],tmax=mdata[4,,i],tmin=mdata[5,,i],tmean=mdata[6,,i],gdd=mdata[7,,i])
  p1=predict.gam(model1,new1, type = "response", progress = TRUE)
  p2=predict.gam(model2,new1, type = "response", progress = TRUE)
  pred1=cbind(pred1,p1)
  pred2=cbind(pred2,p2)
}
write.csv(pred1,file="E:/citymos1_15.csv",row.names=FALSE)
write.csv(pred2,file="E:/citymos2_15.csv",row.names=FALSE)

##
mdata=futuredata$cityjianmo.20
d=attributes(mdata)#???ݳ???
pred1=vector(length=0)
pred2=vector(length=0)
for (i in 1:d$dim[3]){
  new1=data.frame(prmax=mdata[1,,i],prmin=mdata[2,,i],pran=mdata[3,,i],tmax=mdata[4,,i],tmin=mdata[5,,i],tmean=mdata[6,,i],gdd=mdata[7,,i])
  p1=predict.gam(model1,new1, type = "response", progress = TRUE)
  p2=predict.gam(model2,new1, type = "response", progress = TRUE)
  pred1=cbind(pred1,p1)
  pred2=cbind(pred2,p2)
}
write.csv(pred1,file="E:/citymos1_20.csv",row.names=FALSE)
write.csv(pred2,file="E:/citymos2_20.csv",row.names=FALSE)

##
mdata=futuredata$cityjianmo.25
d=attributes(mdata)#???ݳ???
pred1=vector(length=0)
pred2=vector(length=0)
for (i in 1:d$dim[3]){
  new1=data.frame(prmax=mdata[1,,i],prmin=mdata[2,,i],pran=mdata[3,,i],tmax=mdata[4,,i],tmin=mdata[5,,i],tmean=mdata[6,,i],gdd=mdata[7,,i])
  p1=predict.gam(model1,new1, type = "response", progress = TRUE)
  p2=predict.gam(model2,new1, type = "response", progress = TRUE)
  pred1=cbind(pred1,p1)
  pred2=cbind(pred2,p2)
}
write.csv(pred1,file="E:/citymos1_25.csv",row.names=FALSE)
write.csv(pred2,file="E:/citymos2_25.csv",row.names=FALSE)

##
mdata=futuredata$cityjianmo.30
d=attributes(mdata)#???ݳ???
pred1=vector(length=0)
pred2=vector(length=0)
for (i in 1:d$dim[3]){
  new1=data.frame(prmax=mdata[1,,i],prmin=mdata[2,,i],pran=mdata[3,,i],tmax=mdata[4,,i],tmin=mdata[5,,i],tmean=mdata[6,,i],gdd=mdata[7,,i])
  p1=predict.gam(model1,new1, type = "response", progress = TRUE)
  p2=predict.gam(model2,new1, type = "response", progress = TRUE)
  pred1=cbind(pred1,p1)
  pred2=cbind(pred2,p2)
}
write.csv(pred1,file="E:/citymos1_30.csv",row.names=FALSE)
write.csv(pred2,file="E:/citymos2_30.csv",row.names=FALSE)

##
mdata=futuredata$cityjianmo.35
d=attributes(mdata)#???ݳ???
pred1=vector(length=0)
pred2=vector(length=0)
for (i in 1:d$dim[3]){
  new1=data.frame(prmax=mdata[1,,i],prmin=mdata[2,,i],pran=mdata[3,,i],tmax=mdata[4,,i],tmin=mdata[5,,i],tmean=mdata[6,,i],gdd=mdata[7,,i])
  p1=predict.gam(model1,new1, type = "response", progress = TRUE)
  p2=predict.gam(model2,new1, type = "response", progress = TRUE)
  pred1=cbind(pred1,p1)
  pred2=cbind(pred2,p2)
}
write.csv(pred1,file="E:/citymos1_35.csv",row.names=FALSE)
write.csv(pred2,file="E:/citymos2_35.csv",row.names=FALSE)


##
mdata=futuredata$cityjianmo.40
d=attributes(mdata)#???ݳ???
pred1=vector(length=0)
pred2=vector(length=0)
for (i in 1:d$dim[3]){
  new1=data.frame(prmax=mdata[1,,i],prmin=mdata[2,,i],pran=mdata[3,,i],tmax=mdata[4,,i],tmin=mdata[5,,i],tmean=mdata[6,,i],gdd=mdata[7,,i])
  p1=predict.gam(model1,new1, type = "response", progress = TRUE)
  p2=predict.gam(model2,new1, type = "response", progress = TRUE)
  pred1=cbind(pred1,p1)
  pred2=cbind(pred2,p2)
}
write.csv(pred1,file="E:/citymos1_40.csv",row.names=FALSE)
write.csv(pred2,file="E:/citymos2_40.csv",row.names=FALSE)

##
mdata=futuredata$cityjianmo.45
d=attributes(mdata)#???ݳ???
pred1=vector(length=0)
pred2=vector(length=0)
for (i in 1:d$dim[3]){
  new1=data.frame(prmax=mdata[1,,i],prmin=mdata[2,,i],pran=mdata[3,,i],tmax=mdata[4,,i],tmin=mdata[5,,i],tmean=mdata[6,,i],gdd=mdata[7,,i])
  p1=predict.gam(model1,new1, type = "response", progress = TRUE)
  p2=predict.gam(model2,new1, type = "response", progress = TRUE)
  pred1=cbind(pred1,p1)
  pred2=cbind(pred2,p2)
}
write.csv(pred1,file="E:/citymos1_45.csv",row.names=FALSE)
write.csv(pred2,file="E:/citymos2_45.csv",row.names=FALSE)

##
mdata=futuredata$cityjianmo.50
d=attributes(mdata)#???ݳ???
pred1=vector(length=0)
pred2=vector(length=0)
for (i in 1:d$dim[3]){
  new1=data.frame(prmax=mdata[1,,i],prmin=mdata[2,,i],pran=mdata[3,,i],tmax=mdata[4,,i],tmin=mdata[5,,i],tmean=mdata[6,,i],gdd=mdata[7,,i])
  p1=predict.gam(model1,new1, type = "response", progress = TRUE)
  p2=predict.gam(model2,new1, type = "response", progress = TRUE)
  pred1=cbind(pred1,p1)
  pred2=cbind(pred2,p2)
}
write.csv(pred1,file="E:/citymos1_50.csv",row.names=FALSE)
write.csv(pred2,file="E: