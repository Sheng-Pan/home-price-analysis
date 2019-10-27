setwd('C:/Users/Pan/Desktop/�½��ļ���/case.study.homeforsale')
library(ggplot2)
library(tabplot)
library(caret)
##��ȡ����
pricedata <- read.csv("HomesForSale.csv", header = TRUE)
summary(pricedata)
##���ݷ��۷ֲ�ͼ
ggplot(pricedata,aes(x=factor(State),y=Price))+
  geom_boxplot(outlier.colour="red", outlier.shape=8, outlier.size=4)
##����������������ֱ��ͼ
p = ggplot(data = pricedata, mapping = aes(x = Baths))
p + geom_histogram(bins = 10)
p = ggplot(data = pricedata, mapping = aes(x = Price))
p + geom_histogram(bins = 10)
##���鷿�۾�ֵ��ͬ
t.test(pricedata[,2][pricedata[,1]=='NJ'], pricedata[,2][pricedata[,1]=='NY'], paired = TRUE)
##�������������3�������ͬ
bathnj = pricedata[,5][pricedata[,1]=='NJ'];
bathny = pricedata[,5][pricedata[,1]=='NY'];
t.test(bathnj>=3, bathny>=3, paired = TRUE)
tableplot(pricedata)
##����CA�ķ��ӣ�������Ԥ�ⷿ�ӵĴ�С,���ּ��鼯
dataCA <- pricedata[pricedata[,1]=='CA',]
set.seed(1234)
trainIndex <- createDataPartition(dataCA$Price, p = .7,
                                  list = FALSE,
                                  times = 1)

train0 <- dataCA[ trainIndex,]
test0  <- dataCA[-trainIndex,]
##����cv
fitControl <- trainControl(method = "cv")
##��������������ģ��Ԥ��
linemodel <- train(Price~Baths, train0, method = "lm", trControl = fitControl)
pred <- predict(linemodel, test0)
RMSE(pred, test0$Price)
plot(test0$Baths, test0$Price, main="Price and Baths(CA)", 
     xlab="Baths ", ylab="Price", pch=19)
abline(linemodel$finalModel$coefficients, col="red")
##ȫ��������ģ��Ԥ��
set.seed(1234)
allmodel <- train(Price~., train0, method = "lm", trControl = fitControl)
pred <- predict(allmodel, test0)
RMSE(pred, test0$Price)
#����ģ��
lm1 = lm(Price ~., data = pricedata)
summary(lm1)
par(mfrow = c(2, 2))  # ��2*2��ͼ
plot(lm1, which = c(1:4)) 
##Ѱ����Ѿ�����
k.plot <- function(data, nc, seed=1234){
  #�����Ϊһ��ʱ���ܵ����ƽ����              
  k <- (nrow(data)-1)*sum(apply(data,2,var)) 
  for (i in 2:nc){
    set.seed(seed) 
    k[i] <- kmeans(data, centers=i, iter.max = 100)$tot.withinss
  }
  plot(1:nc, k, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares",col = '#9933FF',
       lwd = 2, main = 'Choose best Clusters')
}
pricedata<-pricedata[-which(pricedata[,2]==5900),]#ȥ����Ⱥ��
standrad <- data.frame(scale(pricedata[,c('Price','Size')]))#��׼������
myplot <-k.plot(standrad, nc = 15)
##��������Ϊ5��
set.seed(3456)
clust <- kmeans(x = standrad, centers = 5, iter.max = 100)
table(pricedata$State,clust$cluster)
## �����������ȽϷ��ӵ�������۸�,�Լ�ÿ��λ����۸�
y <- data.frame(pricedata$Price,pricedata$Size,pricedata$Price/pricedata$Size)
colnames(y) <- c('Price','Size','Price per Size')
aggregate(y, list(clust$cluster), mean)
# ��������۸�۵�ɢ��ͼ������������л���
p <- ggplot(data = y[,1:2], mapping = aes(x = Size,y = Price, color = factor(clust$cluster)))
p <- p + geom_point(pch = 20, size = 3)
p + scale_colour_manual(values = c("red","blue", "green", "orange",'yellow'))