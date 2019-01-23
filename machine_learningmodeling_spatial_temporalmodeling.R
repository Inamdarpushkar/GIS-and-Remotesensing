#install packages
install.packages("caret")
library(raster)
library(caret)
library(leaflet)
library(mapview)
library(sf)

#Loading the dataset
sentinel<-stack("M:\\One Concern\\Geostat2018\\practice\\data\\sentinel2017.grd")
training<-read_sf("M:\\One Concern\\Geostat2018\\practice\\data\\trainingSites.shp")

#Extract raster information

viewRGB(sentinel,r=3,g=2,b=1,map.types="Esri.WorldImagery")+mapview(training)
extr <- extract (sentinel,training,df=TRUE)
extr<-merge(extr,training,by.x="ID",by.y="id")

#r split
set.seed(100)
#stratified sampling based on the class
trainids<-createDataPartition(extr$Class,list = FALSE,p=0.3)
trainDat <- extr[trainids,]
testDat <- extr[-trainids,]
boxplot(trainDat$yellowness~trainDat$Class,las=2)
boxplot(trainDat$B08~trainDat$Class,las=2)


#featurePlot
featurePlot(x=trainDat[,c("B03","B04","B08","yellowness")],
            y=factor(trainDat$Class),
            plot = "pairs",
            auto.key=list(columns=4))

#
predictors <-c("B02","B03","B04","B08","B05","B06","B07","B8A","NDVI","yellowness")
response<-"Class"

#Randon Forest
set.seed(100)
model<-train(trainDat[,predictors],trainDat[,response],method = "rf",
             trControl = trainControl(method = "cv"),importance=TRUE)

print(model)
plot(varImp(model))
prediction<-predict(sentinel,model)
spplot(prediction,col.regions=c("brown","darkgreen","black","yellow","green","white","red","blue"))

## Model validation

pred_valid<-predict(model,testDat)
table(testDat$Class,pred_valid)






