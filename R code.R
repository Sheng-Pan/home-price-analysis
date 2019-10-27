setwd('C:/Users/Pan/Desktop/新建文件夹/case.study.homeforsale')
library(ggplot2)
library(tabplot)
library(caret)
##读取数据
pricedata <- read.csv("HomesForSale.csv", header = TRUE)
summary(pricedata)
##各州房价分布图
ggplot(pricedata,aes(x=factor(State),y=Price))+
  geom_boxplot(outlier.colour="red", outlier.shape=8, outlier.size=4)
##房价与卫生间数量直方图
p = ggplot(data = pricedata, mapping = aes(x = Baths))
p + geom_histogram(bins = 10)
p = ggplot(data = pricedata, mapping = aes(x = Price))
p + geom_histogram(bins = 10)
##检验房价均值相同
t.test(pricedata[,2][pricedata[,1]=='NJ'], pricedata[,2][pricedata[,1]=='NY'], paired = TRUE)
##检验卫生间大于3间概率相同
bathnj = pricedata[,5][pricedata[,1]=='NJ'];
bathny = pricedata[,5][pricedata[,1]=='NY'];
t.test(bathnj>=3, bathny>=3, paired = TRUE)
tableplot(pricedata)
##单看CA的房子，卫生间预测房子的大小,划分检验集
dataCA <- pricedata[pricedata[,1]=='CA',]
set.seed(1234)
trainIndex <- createDataPartition(dataCA$Price, p = .7,
                                  list = FALSE,
                                  times = 1)

train0 <- dataCA[ trainIndex,]
test0  <- dataCA[-trainIndex,]
##设置cv
fitControl <- trainControl(method = "cv")
##卫生间数量线性模型预测
linemodel <- train(Price~Baths, train0, method = "lm", trControl = fitControl)
pred <- predict(linemodel, test0)
RMSE(pred, test0$Price)
plot(test0$Baths, test0$Price, main="Price and Baths(CA)", 
     xlab="Baths ", ylab="Price", pch=19)
abline(linemodel$finalModel$coefficients, col="red")
##全因素线性模型预测
set.seed(1234)
allmodel <- train(Price~., train0, method = "lm", trControl = fitControl)
pred <- predict(allmodel, test0)
RMSE(pred, test0$Price)
#建立模型
lm1 = lm(Price ~., data = pricedata)
summary(lm1)
par(mfrow = c(2, 2))  # 画2*2的图
plot(lm1, which = c(1:4)) 
##寻找最佳聚类数
k.plot <- function(data, nc, seed=1234){
  #假设分为一组时的总的离差平方和              
  k <- (nrow(data)-1)*sum(apply(data,2,var)) 
  for (i in 2:nc){
    set.seed(seed) 
    k[i] <- kmeans(data, centers=i, iter.max = 100)$tot.withinss
  }
  plot(1:nc, k, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares",col = '#9933FF',
       lwd = 2, main = 'Choose best Clusters')
}
pricedata<-pricedata[-which(pricedata[,2]==5900),]#去掉离群点
standrad <- data.frame(scale(pricedata[,c('Price','Size')]))#标准化数据
myplot <-k.plot(standrad, nc = 15)
##将样本聚为5类
set.seed(3456)
clust <- kmeans(x = standrad, centers = 5, iter.max = 100)
table(pricedata$State,clust$cluster)
## 按聚类结果，比较房子的面积、价格,以及每单位面积价格
y <- data.frame(pricedata$Price,pricedata$Size,pricedata$Price/pricedata$Size)
colnames(y) <- c('Price','Size','Price per Size')
aggregate(y, list(clust$cluster), mean)
# 绘制面积价格价的散点图，并按聚类进行划分
p <- ggplot(data = y[,1:2], mapping = aes(x = Size,y = Price, color = factor(clust$cluster)))
p <- p + geom_point(pch = 20, size = 3)
p + scale_colour_manual(values = c("red","blue", "green", "orange",'yellow'))
