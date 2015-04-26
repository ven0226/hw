rm(list=ls(all=T))
library(tiff)
library(raster)
library(e1071)
library(caret)
library(tree)
library(nnet)
setwd("/Volumes/MySpace/Courses/Sem2/CSC791/HW/3/hw3-gt-data")
set.seed(2)
files <- list.files(pattern="ilk-3b-1024.tif")
du <- stack(files)
gmm.df <- as.data.frame(du) 
names(gmm.df)[1]  <- "r"
names(gmm.df)[2]  <- "g"
names(gmm.df)[3]  <- "b"

output <- list.files(pattern="seg_sup.tif")
seg.du <- stack(output) 

seg.fac <- as.factor(seg.df[,1])

training_old <- read.csv("ilk-tr-xy.txt",header=FALSE)
trcoords <- training_old[,-c(1,4)]
names(trcoords)[1]  <- "X"
names(trcoords)[2]  <- "Y"
r  <- NULL
g <- NULL
b <- NULL
w <- 1024
h <- 1024
xCoords <- NULL
yCoords <- NULL
level <- 3
label <- NULL
for (i in 1:nrow(trcoords)){
#for (i in 1:1){
  y <- trcoords$X[i] -1
  x <- trcoords$Y[i] -1
  l <- training_old[,4][i]
  for(j in 1:level){
    for(k in 1:level){
      if(((x - 1 + j) >= 1) && ((y - 1 + k) >= 1)&& ((x - 1 + j) < w) && ((y - 1 + k) < h)){
        xC <- x - 1 + j
        yC <- y - 1 + k
        label <- c(label,l)
        xCoords <- c(xCoords,xC)
        yCoords <- c(yCoords,yC)
        xy = ((xC-1) * 1024) + yC
        r <- c(r,gmm.df[,1][xy])
        g <- c(g,gmm.df[,2][xy])
        b <- c(b,gmm.df[,3][xy])
      }
    }
  }
}
training  <- cbind(X=xCoords,Y=yCoords,r,g,b,label)
#training  <- cbind(training,rgb)
training <- as.data.frame(training)

test_old <- read.csv("ilk-te-xy.txt",header=FALSE)
test <- test_old[,-c(1,4)]
r  <- NULL
g <- NULL
b <- NULL
for (i in 1:nrow(test)){
  y <- test[,1][i]
  x <- test[,2][i]
  xy = ((x - 1)*1024) +  y
  r <- c(r,gmm.df[,1][xy])
  g <- c(g,gmm.df[,2][xy])
  b <- c(b,gmm.df[,3][xy])
}
rgb  <- cbind(r,g,b)
test  <- cbind(test,rgb,class=test_old[,4])
names(test)[1]  <- "X"
names(test)[2]  <- "Y"
train  <- training
train <- training[,-c(1,2)]

##############
##NaiveBayes##
##############
train$label  <- as.factor(train$label)
naiveModel <- naiveBayes(label ~ ., data = train)
test_set <- test[,-c(1,2)]
test_set$class  <- as.factor(test_set$class)
table(predict(naiveModel, test_set[,-c(4)]), test_set$class)

naivePredict <- predict(naiveModel, gmm.df,type="class")
gclusters <- raster(du)
gclusters <- setValues(gclusters, naivePredict) 
plot(gclusters,main="Naive Bayes")

#write.csv(train,"train.csv")
#write.csv(test_set,"test.csv")
#fulPredict <- predict(m, gmm.df)

########
##tree##
########

train$label  <- as.factor(train$label)
treeModel  <- tree(label ~ .,data=train)
#plot(treeModel)
#text(treeModel,pretty=0)
test_set <- test[,-c(1,2)]
test_set$class  <- as.factor(test_set$class)
table(predict(treeModel, test_set[,-c(4)],type="class"), test_set$class)

treePredict <- predict(treeModel, gmm.df,type="class")
gclusters <- raster(du)
gclusters <- setValues(gclusters, treePredict) 
plot(gclusters,main="Decision Tree")

#######
##SVM##
#######

train$label  <- as.factor(train$label)
model <- svm(label ~ ., data = train)
#summary(model)
table(predict(model, test[,3:5]), test[,6])
svmPredict <- predict(model, gmm.df,type="class")
gclusters <- raster(du)
gclusters <- setValues(gclusters, svmPredict) 
plot(gclusters,main="SVM")

#####################
###Neural networks###
#####################

train$label  <- as.factor(train$label)
modelNeural <- multinom(label~., data=train, maxit=500, trace=T)
table(predict(modelNeural, type="class", newdata=test[,3:5]),test[,6])
neuralPredict <- predict(modelNeural, gmm.df,type="class")
gclusters <- raster(du)
gclusters <- setValues(gclusters, neuralPredict) 
plot(gclusters,main="Neural Network")
