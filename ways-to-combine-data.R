data=read.csv('fruits.csv')
sum(is.na(data))
data$store1[is.na(data$store)]=mean(data$store1,na.rm = FALSE)

data=AirPassengers
plot(decompose(data))
plot(data)
plot.ts(data)
abline(lm(data ~ time(data)),col='red')

a=log(data)
b=diff(a)
b
boxplot(data ~ cycle(data))

data=read.csv('fruits.csv')
# pie(data$fruits,)

cat=table(data$store1,data$store2)
cat
cat=table(cut(table$store1,breaks=seq(0,1,by=0.2)),cut(table$store2,breaks=seq(0,1,by=0.2)))
t=table(data$fruits,data$store1,data$store2)

r = prop.table(t,margin=1)

m=chisq.test(as.matrix(t))


# k means:
X=data[1:2][2:]
wcss<- numeric()
for (i in 1:10){
  model=kmeans(X,centers = i,nstart = 10)
  wcss[i]=model$tot.withinss
}
plot((1:10),wcss)
model=kmeans(X,centers=2,nstart = 2)
clusplot(X,model$clusters)

data<- read.csv('Mall_Customers.csv')
x <- data.frame(AnnualIncome=data$Annual.Income..k..,SpendingScore=data$Spending.Score..1.100.)

d=dist(x,method='euclidean')
p=hclust(d,method='ward.D')
plot(p)


pca=PCA(data[,sapply(data,is.numeric)],graph=FALSE,ncp=2)
pca$eig
pca$var$coord
plot(pca, axes = c(1, 2), choix = c("ind","var","varcor"),
     ellipse = NULL, xlim = NULL, ylim = NULL, habillage="none", 
     col.hab = NULL, col.ind="black", col.ind.sup="blue", 
     col.quali="magenta", col.quanti.sup="blue", col.var="black",
     label = c("all","none","ind","ind.sup","quali","var","quanti.sup"),
     invisible = c("none","ind","ind.sup","quali","var","quanti.sup"), 
     lim.cos2.var = 0., title = NULL, palette=NULL,
     autoLab = c("auto","yes","no"), new.plot = FALSE, select = NULL, 
     unselect = 0.7, shadowtext = FALSE, legend = list(bty = "y", x = "topleft"),
     graph.type = c("ggplot","classic"), ggoptions = NULL)

data<- read.csv('Mall_Customers.csv')
X=data[1]
Y=data[3]
a=sample.split( Y, SplitRatio = 2/3, group = NULL )
train=data[a,2:3]
test=data[!a,2:3]
reg=lm(CustomerID ~  age, data=train)
