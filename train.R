library(rgl)
library(xgboost)
library(data.table)
library(Matrix)
library(caret)
library(wordcloud2)
library(car)
library(plyr)
library(dplyr)
library(moments)
library(glmnet)
library(elasticnet)
library(knitr)
library(e1071)
library(ranger)
library(gbm)


poidf <- read.csv("./suzhou_number_poi_300.csv", stringsAsFactors = FALSE)
poicount <- read.csv("./suzhounewAPIPOIcount", stringsAsFactors = FALSE)
ctriptimeseries <- read.csv("./suzhou_real_customer_volume.csv", header=FALSE,stringsAsFactors = FALSE)
df <- read.csv("./suzhou_hotel_features_300.csv",stringsAsFactors=FALSE)


names(df) <- c("realsv","avgp","commentn","commerciala","creditcn","fs_breakfast","fs_familysuite",
      "fs_queenroom","fs_twinroom","fs_parkinglot","fs_swimpool","fs_pickup","fs_gym",
      "fs_airportshuttle","fs_grandopen","fs_pet","t_gardena","t_lakev","t_leisurev",
      "t_parentingt","t_romanticl","t_businesst","t_conferenceh","t_department","t_inn",
      "t_youthhostel","t_boutiqueh","t_designedh","t_hotsh","t_traditionalh",
      "t_deliciousf","t_plumf","id","lat","lng","name","openingt","roomn","picturen",
      "rstar","grade","distancers","distancenrs","distancedt","trip","scenery_5km","scenery_3km",                  
      "scenery_1km","subway","scenics","beautifuls","lane","environment","sea","nightl",
      "loc","buss","railways","tranquil","transport","view","General","Services","Rooma",
      "Activities","surroundf","timest","meishi","theater","recreation","lady","openinglong")
dfnames <- names(df)
df <- df[,setdiff(dfnames,c("subway","scenics","beautifuls","lane","environment","sea","nightl","loc","buss","railways","tranquil","transport","view"))]



df <- df[order(df$realsv,decreasing=TRUE),]
df$utilityor <- df$realsv / df$roomn
df$tradingor <- df$realsv * df$avgp / df$roomn
df$trading <- df$realsv * df$avgp
df <- df[order(df$utilityor,decreasing=TRUE),]
df <- df[df$roomn>5,]
df <- df[df$realsv>df$timest,]
df <- df[-c(1:2),]


df <- subset(df,select=-lng)
df <- subset(df,select=-lat)

df <- df[order(df[,"trading"],decreasing=TRUE),]
commercialareas <- unique(df[,"commerciala"])
names(commercialareas) <- 1:length(commercialareas)
df$commercialn <- 1
for(n in names(commercialareas)) set(df,which(df[,"commerciala"]==commercialareas[n]),"commercialn",as.integer(n))
df <- subset(df,select=-commerciala)


poidf$total <- rowSums(poidf[,2:ncol(poidf)],dims=1)
ppdf <- merge(df[,c("id","name","trading")], poidf,
               all.x = TRUE,
               by='id')
cn <- colnames(ctriptimeseries)
cn[1] <- "id"
colnames(ctriptimeseries) <- cn
ppdf <- merge(ppdf,ctriptimeseries[,c("id","V42","V43","V44","V45")],
              all.x = TRUE,
              by='id')
ppdf$typeratio <- (ppdf[,"V45"]+ppdf[,"V44"]+ppdf[,"V43"]+1)/(ppdf[,"V42"]+1)

ppdf <- ppdf[order(ppdf[,"trading"],decreasing=TRUE),]
poibackmean <- Matrix(0, 1, ncol(ppdf)-9)
colnames(poibackmean) <- colnames(ppdf[,4:(ncol(ppdf)-6)])
deleterow <- c()
for(rowi in c(1:nrow(ppdf)))
{
    if(sum((sum(ppdf[rowi,4:(ncol(ppdf)-6)])-ppdf[rowi,4:(ncol(ppdf)-6)]) < 1) > 0){
      print(rowi)
      deleterow <- c(deleterow,ppdf[rowi,"id"])
    }
}
for(did in deleterow){
  df <- df[-which(df$id == did),]
  ppdf <- ppdf[-which(ppdf$id == did),]
}


for(rowi in c(1:nrow(ppdf)))
{
  poibackmean <- poibackmean[1,] + ppdf[rowi,4:(ncol(ppdf)-6)]/(sum(ppdf[rowi,4:(ncol(ppdf)-6)])-ppdf[rowi,4:(ncol(ppdf)-6)])
}
poibackmean <- data.frame(as.matrix(poibackmean))
poibackmean <- poibackmean/nrow(ppdf)


N_mean <- Matrix(0, 1, ncol(ppdf)-9)
colnames(N_mean) <- colnames(ppdf[,4:(ncol(ppdf)-6)])
for(rowi in c(1:nrow(ppdf)))
{
  N_mean <- N_mean[1,] + ppdf[rowi,4:(ncol(ppdf)-6)]
}
N_mean <- data.frame(as.matrix(N_mean))
N_mean <- N_mean/nrow(ppdf)

topdf <- ppdf[which(ppdf[,"trading"]>700000),]
indexratio <- (nrow(topdf)-1:nrow(topdf)+1)/nrow(topdf)

tripdf <- ppdf[which(ppdf[1:nrow(topdf),"typeratio"] > 5),]
tripratio <- indexratio[which(ppdf[1:nrow(topdf),"typeratio"] > 5)]
tripdfmatrix <- as.matrix(tripdf[,4:(ncol(topdf)-6)])
tt <- matrix(rep(rowSums(tripdfmatrix),ncol(tripdfmatrix)),nrow(tripdfmatrix),ncol(tripdfmatrix))
ttrr <- matrix(rep(tripratio,ncol(tripdfmatrix)),nrow(tripdfmatrix),ncol(tripdfmatrix))
ttfreq <- colSums(ttrr*tripdfmatrix/(tt-tripdfmatrix))/sum(tripratio)
trip_k <- t(data.frame(ttfreq))/poibackmean
trip_k[which(is.na(trip_k))] <- 1
trip_k <- log(trip_k)
trip_k[1,which(trip_k < -max(trip_k))] <- -max(trip_k)

businessdf <- ppdf[which(ppdf[1:nrow(topdf),"typeratio"] < 2),]
businessratio <- indexratio[which(ppdf[1:nrow(topdf),"typeratio"] < 2)]
businessdfmatrix <- as.matrix(businessdf[,4:(ncol(topdf)-6)])
bb <- matrix(rep(rowSums(businessdfmatrix),ncol(businessdfmatrix)),nrow(businessdfmatrix),ncol(businessdfmatrix))
bbrr <- matrix(rep(businessratio,ncol(businessdfmatrix)),nrow(businessdfmatrix),ncol(businessdfmatrix))
bbfreq <- colSums(bbrr*businessdfmatrix/(bb-businessdfmatrix))/sum(businessratio)
business_k <- t(data.frame(bbfreq))/poibackmean
business_k[which(is.na(business_k))] <- 1
business_k <- log(business_k)
business_k[1,which(business_k < -max(business_k))] <- -max(business_k)


ppdf$tripMQJ <- 0
ppdf$businessMQJ <- 0
for(rowi in 1:nrow(ppdf)) ppdf[rowi,"tripMQJ"] <- sum((ppdf[rowi,4:(ncol(ppdf)-8)] - N_mean)*trip_k)
for(rowi in 1:nrow(ppdf)) ppdf[rowi,"businessMQJ"] <- sum((ppdf[rowi,4:(ncol(ppdf)-8)] - N_mean)*business_k)


df <- merge(df, ppdf[,c("id","total","tripMQJ","businessMQJ")],
            all.x = TRUE,
            by='id')


df[which(is.na(df[,"tripMQJ"])),"tripMQJ"] <- 0
df[which(is.na(df[,"businessMQJ"])),"businessMQJ"] <- 0
df[which(is.na(df[,"total"])),"total"] <- 0

olddf <- df[which(df[,"openingt"]<"2015-01" | df[,"openingt"]=="unknown"),]
olddf <- olddf[order(olddf[,"trading"],decreasing=TRUE),]
oldY <- olddf$trading
olddf$trading <- olddf$trading/10000
olddf$label <- 0
olddf[which(olddf$trading>1000),"label"]  <- 3
olddf[which(olddf$trading<1000 & olddf$trading>100),"label"]  <- 2
olddf[which(olddf$trading<100 & olddf$trading>10),"label"] <- 1
olddf[which(olddf$trading<10),"label"] <- 0
oldLabel <- olddf$label
olddf <- subset(olddf,select=-label)
olddf <- subset(olddf,select=-timest)
oldname <- olddf$name
olddf <- subset(olddf,select=-name)
oldid <- olddf$id
olddf <- subset(olddf,select=-id)
olddf <- subset(olddf,select=-openingt)
olddf <- subset(olddf,select=-commentn)
olddf <- subset(olddf,select=-realsv)
olddf <- subset(olddf,select=-utilityor)
olddf <- subset(olddf,select=-tradingor)
olddf <- subset(olddf,select=-trading)


Xtrain <- data.frame(olddf)
Ytrain <- data.frame(oldY)
Ltrain <- data.frame(oldLabel)

xgboost_cv <- function(X_train,y,cv=10,seed=123,nrd=5)
{
  set.seed(seed)
  cat("Preparing Data\n")
  randomcv <- floor(runif(nrow(X_train),1,(cv+1)))
  for(i in 1:cv)
  {
    param <- list(objective = "reg:linear",
              nthread = 4,colsample_bytree = 0.6,
              max_depth=15,
              eta = 0.2,
              gamma = 1.0)
    dmodel <- xgb.DMatrix(as.matrix(subset(X_train, randomcv != i)),label=as.matrix(subset(y, randomcv != i)))
    dvalid <- xgb.DMatrix(as.matrix(subset(X_train, randomcv == i)),label=as.matrix(subset(y, randomcv == i)))
    m1 <- xgb.train(data = dmodel, param, nrounds = nrd, early.stop.round=2, maximize=FALSE, watchlist = list(eval = dvalid,train = dmodel))
    imp <- xgb.importance(model = m1, feature_names = colnames(X_train))
    imp <- data.frame(imp)
    imp$Count <- 1
    ypredict <- predict(m1, as.matrix(subset(X_train, randomcv == i)))
    if(i == 1)
    {
      output <- cbind(subset(y, randomcv == i),ypredict)
      imptc <- data.frame(Feature=names(X_train),Gain=rep(0,ncol(X_train)),Cover=rep(0,ncol(X_train)),Frequence=rep(0,ncol(X_train)),Count=rep(0,ncol(X_train)))
      for(ft in imp$Feature)
        imptc[which(imptc$Feature == ft),2:5] <- imptc[which(imptc$Feature == ft),2:5] + imp[which(imp$Feature==ft),2:5]
    }
    if(i > 1)
    {
      output <- rbind(output,cbind(subset(y, randomcv == i),ypredict))
      for(ft in imp$Feature)
        imptc[which(imptc$Feature == ft),2:5] <- imptc[which(imptc$Feature == ft),2:5] + imp[which(imp$Feature==ft),2:5]
    }
    gc()
  }
  for(rowi in 1:nrow(imptc)) if(imptc[rowi,5]>0) imptc[rowi,2:4] <- imptc[rowi,2:4]/imptc[rowi,5]
  return(list(output, imptc))
}



ob1000 <- rep(0,nrow(Xtrain))
imptc1000 <- data.frame(Feature=names(Xtrain),Gain=rep(0,ncol(Xtrain)),Cover=rep(0,ncol(Xtrain)),Frequence=rep(0,ncol(Xtrain)),Count=rep(0,ncol(Xtrain)))
count <- 0


for(i in 1:10){
  print(i)
  count <- count + 1
  sss <- floor(runif(1,min=1,max=1000))
  result <- xgboost_cv(Xtrain,Ytrain,seed=sss,nrd=40)
  ob <- result[[1]][order(result[[1]][,1],decreasing=TRUE),]
  if(count == 1){
    ob10 <- ob[,2]
    imptc <- result[[2]]
  }else{
    ob10 <- ob10 + ob[,2]
    for(ft in imptc$Feature) imptc[which(imptc$Feature == ft),2:4] <- imptc[which(imptc$Feature == ft),2:4] + result[[2]][which(result[[2]]$Feature==ft),2:4]
  }
  if(count == 10){
    ob10 <- ob10/10
    ob1000 <- ob1000 + ob10
    imptc[,2:4] <- imptc[,2:4]/10
    for(ft in imptc1000$Feature) imptc1000[which(imptc1000$Feature == ft),2:4] <- imptc1000[which(imptc1000$Feature == ft),2:4] + imptc[which(imptc$Feature == ft),2:4]
    count <- 0
  }
}





if(FALSE){
imptc1000[,2:4] <- imptc1000[,2:4]/10
ob1000 <- ob1000/10
}
ob1000 <- data.frame(ob1000)
ob1000 <- ob1000/10000
oldY <- oldY/10000
ob1000xgb <- ob1000

rightratio <- rep(0,4)
rightratio[1] <- sum(ob1000[which(oldY>1000),1] > 1000)/sum(oldY>1000)
rightratio[2] <- sum(ob1000[which(oldY<1000 & oldY>100),1]<1000 & ob1000[which(oldY<1000 & oldY>100),1]>100)/sum(oldY<1000 & oldY>100)
rightratio[3] <- sum(ob1000[which(oldY<100 & oldY>10),1]<100 & ob1000[which(oldY<100 & oldY>10),1]>10)/sum(oldY<100 & oldY>10)
rightratio[4] <- sum(ob1000[which(oldY<10),1]<10 )/sum(oldY<10)
rightratio_xgb <- rightratio




####################################GBM
olddf <- df[which(df[,"openingt"]<"2015-01" | df[,"openingt"]=="unknown"),]
olddf <- olddf[order(olddf[,"trading"],decreasing=TRUE),]
olddf$trading <- olddf$trading/10000
oldY <- olddf$trading
olddf <- subset(olddf,select=-timest)
oldname <- olddf$name
olddf <- subset(olddf,select=-name)
oldid <- olddf$id
olddf <- subset(olddf,select=-id)
olddf <- subset(olddf,select=-openingt)
olddf <- subset(olddf,select=-commentn)
olddf <- subset(olddf,select=-realsv)
olddf <- subset(olddf,select=-utilityor)
olddf <- subset(olddf,select=-tradingor)

Xtrain <- data.frame(olddf)
Ytrain <- data.frame(oldY)

gbm_cv <- function(X_train,y,cv=10,seed=123)
{
  set.seed(seed)
  cat("Preparing Data\n")
  randomcv <- floor(runif(nrow(X_train),1,(cv+1)))
  for(i in 1:cv)
  {#multinomial (classification when there are more than 2 classes)

    model_gbm <- gbm(trading~.,data=subset(X_train, randomcv != i),distribution="gaussian",n.trees=1000,
        shrinkage=0.05,interaction.depth=3,bag.fraction = 0.7,
        train.fraction = 0.8,n.minobsinnode = 10,cv.folds = 8,
        keep.data=TRUE,verbose=TRUE,n.cores=1)

    ypredict <- predict(model_gbm,newdata=subset(subset(X_train,select=-trading), randomcv == i))
    if(i == 1)
    {
      output <- cbind(subset(y, randomcv == i),ypredict)
    }
    if(i > 1)
    {
      output <- rbind(output,cbind(subset(y, randomcv == i),ypredict))
    }
    gc()
  }
  return(output)
}

ob1000 <- rep(0,nrow(olddf))
count <- 0

for(i in 1:10){
  print(i)
  count <- count + 1
  sss <- floor(runif(1,min=1,max=1000))
  result <- gbm_cv(olddf,oldY,seed=sss)
  ob <- result[order(result[,1],decreasing=TRUE),]
  if(count == 1){
    ob10 <- ob[,2]
  }else{
    ob10 <- ob10 + ob[,2]
  }
  if(count == 10){
    ob10 <- ob10/10
    ob1000 <- ob1000 + ob10
    count <- 0
  }
}


if(FALSE){
imptc1000[,2:4] <- imptc1000[,2:4]/10
ob1000 <- ob1000/10
}
ob1000 <- data.frame(ob1000)
ob1000gbm <- ob1000

rightratio <- rep(0,4)
rightratio[1] <- sum(ob1000[which(oldY>1000),1] > 1000)/sum(oldY>1000)
rightratio[2] <- sum(ob1000[which(oldY<1000 & oldY>100),1]<1000 & ob1000[which(oldY<1000 & oldY>100),1]>100)/sum(oldY<1000 & oldY>100)
rightratio[3] <- sum(ob1000[which(oldY<100 & oldY>10),1]<100 & ob1000[which(oldY<100 & oldY>10),1]>10)/sum(oldY<100 & oldY>10)
rightratio[4] <- sum(ob1000[which(oldY<10),1]<10 )/sum(oldY<10)
rightratio_gbm <- rightratio







##################################random forest
olddf <- df[which(df[,"openingt"]<"2015-01" | df[,"openingt"]=="unknown"),]
olddf <- olddf[order(olddf[,"trading"],decreasing=TRUE),]
olddf$trading <- olddf$trading/10000
oldY <- olddf$trading
olddf <- subset(olddf,select=-timest)
oldname <- olddf$name
olddf <- subset(olddf,select=-name)
oldid <- olddf$id
olddf <- subset(olddf,select=-id)
olddf <- subset(olddf,select=-openingt)
olddf <- subset(olddf,select=-commentn)
olddf <- subset(olddf,select=-realsv)
olddf <- subset(olddf,select=-utilityor)
olddf <- subset(olddf,select=-tradingor)
#olddf <- subset(olddf,select=-trading)


Xtrain <- data.frame(olddf)
Ytrain <- data.frame(oldY)

ranger_cv <- function(X_train,y,cv=10,seed=123)
{
  set.seed(seed)
  cat("Preparing Data\n")
  randomcv <- floor(runif(nrow(X_train),1,(cv+1)))
  for(i in 1:cv)
  {
    model_rg <- ranger(trading~., data=subset(X_train, randomcv != i), write.forest = TRUE)
    cat("predict\n")
    ypredict <- predict(model_rg,dat=subset(subset(X_train,select=-trading), randomcv == i))
    if(i == 1)
    {
      output <- cbind(subset(y, randomcv == i),ypredict$predictions)
    }
    if(i > 1)
    {
      output <- rbind(output,cbind(subset(y, randomcv == i),ypredict$predictions))
    }
    gc()
  }
  return(output)
}


ob1000 <- rep(0,nrow(olddf))
count <- 0

for(i in 1:10){
  print(i)
  count <- count + 1
  sss <- floor(runif(1,min=1,max=1000))
  result <- ranger_cv(olddf,oldY,seed=sss)
  ob <- result[order(result[,1],decreasing=TRUE),]
  if(count == 1){
    ob10 <- ob[,2]
  }else{
    ob10 <- ob10 + ob[,2]
  }
  if(count == 10){
    ob10 <- ob10/10
    ob1000 <- ob1000 + ob10
    count <- 0
  }
}


if(FALSE){
imptc1000[,2:4] <- imptc1000[,2:4]/10
ob1000 <- ob1000/10
}
ob1000 <- data.frame(ob1000)
ob1000ranger <- ob1000

rightratio <- rep(0,4)
rightratio[1] <- sum(ob1000[which(oldY>1000),1] > 1000)/sum(oldY>1000)
rightratio[2] <- sum(ob1000[which(oldY<1000 & oldY>100),1]<1000 & ob1000[which(oldY<1000 & oldY>100),1]>100)/sum(oldY<1000 & oldY>100)
rightratio[3] <- sum(ob1000[which(oldY<100 & oldY>10),1]<100 & ob1000[which(oldY<100 & oldY>10),1]>10)/sum(oldY<100 & oldY>10)
rightratio[4] <- sum(ob1000[which(oldY<10),1]<10 )/sum(oldY<10)
rightratio_ranger <- rightratio




##############################################lasso
olddf <- df[which(df[,"openingt"]<"2015-01" | df[,"openingt"]=="unknown"),]
olddf <- olddf[order(olddf[,"trading"],decreasing=TRUE),]
olddf$trading <- log(olddf$trading+1)
oldY <- olddf$trading
olddf <- subset(olddf,select=-timest)
oldname <- olddf$name
olddf <- subset(olddf,select=-name)
oldid <- olddf$id
olddf <- subset(olddf,select=-id)
olddf <- subset(olddf,select=-openingt)
olddf <- subset(olddf,select=-commentn)
olddf <- subset(olddf,select=-realsv)
olddf <- subset(olddf,select=-utilityor)
olddf <- subset(olddf,select=-tradingor)
olddf <- subset(olddf,select=-trading)

numericfeature <- c("avgp","roomn","picturen","distancers","distancenrs","distancedt","scenery_5km",
                    "scenery_3km","scenery_1km","General","Services","Rooma","Activities","surroundf",
                    "meishi","theater","recreation","lady","openinglong","total","tripMQJ","businessMQJ")

skewedfeatures <- sapply(numericfeature,function(x){skewness(olddf[,x],na.rm=TRUE)})
skewedfeatures <- skewedfeatures[skewedfeatures>0.75]
for(x in names(skewedfeatures))
  olddf[,x] <- log(olddf[,x]+1)



Xtrain <- data.frame(olddf)
Ytrain <- data.frame(oldY)

CARET.TRAIN.CTRL <- trainControl(method="repeatedcv",
                                 number=5,
                                 repeats=5,
                                 verboseIter=FALSE)

lasso_cv <- function(X_train,y,cv=10,seed=123)
{
  set.seed(seed)
  cat("Preparing Data\n")
  randomcv <- floor(runif(nrow(X_train),1,(cv+1)))
  for(i in 1:cv)
  {
    model_lasso <- train(x=subset(X_train, randomcv != i),y=subset(oldY, randomcv != i),
                      method="glmnet",metric="RMSE",maximize=FALSE,
                      trControl=CARET.TRAIN.CTRL,tuneGrid=expand.grid(alpha=1,
                      lambda=c(seq(1,0.1,-0.1),seq(0.09,0,-0.001))))

    ypredict <- predict(model_lasso,newdata=subset(X_train, randomcv == i))
    if(i == 1)
    {
      output <- cbind(subset(y, randomcv == i),ypredict)
    }
    if(i > 1)
    {
      output <- rbind(output,cbind(subset(y, randomcv == i),ypredict))
    }
    gc()
  }
  return(output)
}

ob1000 <- rep(0,nrow(olddf))
count <- 0

for(i in 1:10){
  print(i)
  count <- count + 1
  sss <- floor(runif(1,min=1,max=1000))
  result <- lasso_cv(olddf,oldY,seed=sss)
  ob <- result[order(result[,1],decreasing=TRUE),]
  if(count == 1){
    ob10 <- ob[,2]
  }else{
    ob10 <- ob10 + ob[,2]
  }
  if(count == 1){
    ob10 <- ob10/10
    ob1000 <- ob1000 + ob10
    count <- 0
  }
}


if(FALSE){
imptc1000[,2:4] <- imptc1000[,2:4]/10
ob1000 <- ob1000/10
}
ob1000 <- data.frame(ob1000)
ob1000 <- (exp(ob1000)-1)/10000
ob1000lasso <- ob1000
oldY <- (exp(oldY)-1)/10000


rightratio <- rep(0,4)
rightratio[1] <- sum(ob1000[which(oldY>1000),1] > 1000)/sum(oldY>1000)
rightratio[2] <- sum(ob1000[which(oldY<1000 & oldY>100),1]<1000 & ob1000[which(oldY<1000 & oldY>100),1]>100)/sum(oldY<1000 & oldY>100)
rightratio[3] <- sum(ob1000[which(oldY<100 & oldY>10),1]<100 & ob1000[which(oldY<100 & oldY>10),1]>10)/sum(oldY<100 & oldY>10)
rightratio[4] <- sum(ob1000[which(oldY<10),1]<10 )/sum(oldY<10)
rightratio_lasso <- rightratio






######################################plot
rtlasso <- cbind(c(1:4),data.frame(rightratio_lasso),rep("lasso",4))
rtxgb <- cbind(c(1:4),data.frame(rightratio_xgb),rep("xgb",4))
rtgbm <- cbind(c(1:4),data.frame(rightratio_gbm),rep("gbm",4))
rtrg <- cbind(c(1:4),data.frame(rightratio_ranger),rep("ranger",4))


colnames(rtlasso) <- c("lev","precise","grp")
colnames(rtxgb) <- c("lev","precise","grp")
colnames(rtgbm) <- c("lev","precise","grp")
colnames(rtrg) <- c("lev","precise","grp")


rt <- data.frame(as.numeric(rtlasso[,2]),as.numeric(rtxgb[,2]),as.numeric(rtgbm[,2]),as.numeric(rtrg[,2]))
colnames(rt) <- c("Lasso","XGB","GBM","RF")
rownames(rt) <- c(">1000","100< <=1000","10< <=100","<=10")
pdf(file="./modelevaluation.pdf", width=8, height=7)
par(mai = c(1.02,1,0.82,0.05))
barplot(t(rt), beside=TRUE,cex.names = 1.5,cex.axis=2, ylim=c(0,1), col=c("darkblue","red","orange","brown"))
title(ylab="Recall",xlab="Trading Volume(*10000/year)",cex.lab=2)
legend("topleft", colnames(rt), cex=1.4,ncol=2, fill=c("darkblue","red","orange","brown"))
dev.off()



fscore_cp <- function(y_pred, y_true){
  P3 <- sum(y_pred[which(y_true == 3)] == 3)/sum(y_pred == 3)
  R3 <- sum(y_pred[which(y_true == 3)] == 3)/sum(y_true == 3)

  P2 <- sum(y_pred[which(y_true == 2)] == 2)/sum(y_pred == 2)
  R2 <- sum(y_pred[which(y_true == 2)] == 2)/sum(y_true == 2)

  P1 <- sum(y_pred[which(y_true == 1)] == 1)/sum(y_pred == 1)
  R1 <- sum(y_pred[which(y_true == 1)] == 1)/sum(y_true == 1)

  P0 <- sum(y_pred[which(y_true == 0)] == 0)/sum(y_pred == 0)
  R0 <- sum(y_pred[which(y_true == 0)] == 0)/sum(y_true == 0)

  AP <- (P3+P2+P1+P0)/4
  AR <- (R3+R2+R1+R0)/4
  FS <- 2*AP*AR/(AP+AR)
  return(FS)
}

fscore_eval <- function(y_pred, dtrain) {
  y_true <- getinfo(dtrain, "label")
  return(list(metric="fscore", value=fscore_cp(y_pred,y_true)))
}


recall_cp <- function(y_pred, y_true) {
  R3 <- sum(y_pred[which(y_true == 3)] == 3)/sum(y_true == 3)
  R2 <- sum(y_pred[which(y_true == 2)] == 2)/sum(y_true == 2)
  R1 <- sum(y_pred[which(y_true == 1)] == 1)/sum(y_true == 1)
  R0 <- sum(y_pred[which(y_true == 0)] == 0)/sum(y_true == 0)
  if(is.na(R3))
    AR <- (R2+R1+R0)/3
  else AR <- (R3+R2+R1+R0)/4
  return(AR)
}

recall_eval <- function(y_pred, dtrain) {
  y_true <- getinfo(dtrain, "label")
  return(list(metric="recall", value=precision_cp(y_pred,y_true)))
}

precision_cp <- function(y_pred, y_true) {
  P3 <- sum(y_pred[which(y_true == 3)] == 3)/sum(y_pred == 3)
  P2 <- sum(y_pred[which(y_true == 2)] == 2)/sum(y_pred == 2)
  P1 <- sum(y_pred[which(y_true == 1)] == 1)/sum(y_pred == 1)
  P0 <- sum(y_pred[which(y_true == 0)] == 0)/sum(y_pred == 0)
  if(is.na(P3))
    AP <- (P2+P1+P0)/3
  else AP <- (P3+P2+P1+P0)/4
  return(AP)
}

precision_eval <- function(y_pred, dtrain) {
  y_true <- getinfo(dtrain, "label")
  return(list(metric="recall", value=precision_cp(y_pred,y_true)))
}

avgfscore <- function(ob1000){
  labelarr <- rep(0,length(oldY))
  labelarr[which(ob1000[,1]>1000)] <- 3
  labelarr[which(ob1000[,1]<1000 & ob1000[,1]>100)] <- 2
  labelarr[which(ob1000[,1]<100  & ob1000[,1]>10) ] <- 1
  labelarr[which(ob1000[,1]<10)] <- 0

  fscore_cp(labelarr,oldLabel)
}

avgprecision <- function(ob1000){
  labelarr <- rep(0,length(oldY))
  labelarr[which(ob1000[,1]>1000)] <- 3
  labelarr[which(ob1000[,1]<1000 & ob1000[,1]>100)] <- 2
  labelarr[which(ob1000[,1]<100  & ob1000[,1]>10) ] <- 1
  labelarr[which(ob1000[,1]<10)] <- 0

  precision_cp(labelarr,oldLabel)
}


avgrecall <- function(ob1000){
  labelarr <- rep(0,length(oldY))
  labelarr[which(ob1000[,1]>1000)] <- 3
  labelarr[which(ob1000[,1]<1000 & ob1000[,1]>100)] <- 2
  labelarr[which(ob1000[,1]<100  & ob1000[,1]>10) ] <- 1
  labelarr[which(ob1000[,1]<10)] <- 0

  recall_cp(labelarr,oldLabel)
}


avgfscore_arr <- rep(0, 4)
avgfscore_arr[1] <- avgfscore(ob1000xgb)
avgfscore_arr[2] <- avgfscore(ob1000gbm)
avgfscore_arr[3] <- avgfscore(ob1000lasso)
avgfscore_arr[4] <- avgfscore(ob1000ranger)


avgprecision_arr <- rep(0, 4)
avgprecision_arr[1] <- avgprecision(ob1000xgb)
avgprecision_arr[2] <- avgprecision(ob1000gbm)
avgprecision_arr[3] <- avgprecision(ob1000lasso)
avgprecision_arr[4] <- avgprecision(ob1000ranger)


avgrecall_arr <- rep(0, 4)
avgrecall_arr[1] <- avgrecall(ob1000xgb)
avgrecall_arr[2] <- avgrecall(ob1000gbm)
avgrecall_arr[3] <- avgrecall(ob1000lasso)
avgrecall_arr[4] <- avgrecall(ob1000ranger)


library(RColorBrewer)
cols<-brewer.pal(n=6,name="Set1")


metrict <- data.frame(as.numeric(avgfscore_arr),as.numeric(avgprecision_arr),as.numeric(avgrecall_arr))
colnames(metrict) <- c("F1-score","Precision","Recall")
rownames(metrict) <- c("XGB","GBM","Lasso","RF")
pdf(file="./metricevaluation.pdf", width=8, height=7)
par(mai = c(1.02,1,0.82,0.05))
barplot(t(metrict-0.5), yaxt="n", beside=TRUE,cex.names = 1.5,cex.axis=2, ylim=c(0,0.35), col=c("lightblue","lightgreen","grey"))
axis(2,at=seq(0, 0.35, 0.05),labels=seq(0.5,0.85,0.05),cex.axis=2)
title(ylab="Value",xlab="different approaches",cex.lab=2)
legend("topleft", colnames(metrict), cex=1.4,ncol=3, fill=c("lightblue","lightgreen","grey"))
dev.off()
