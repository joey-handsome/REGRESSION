data=read.csv("108_student.csv",header=T)
# View(data)
sum(is.na(data))
summary(data)
data$學校代碼=factor(data$學校代碼)
#第一大題
data1=subset(data,data$日間.進修別=="D 日"&data$體系別=="1 一般")
nrow(data1)
# View(data1)
name=names(data1)
names(data1)=paste0("x",c(1:length(data1)))
name=cbind(name,names(data1))
data1_1=subset(data1,data1$x4=="M 碩士"|data1$x4=="B 學士")

#(1)
a=aggregate(x5~x2+x4,data1_1,sum)
k1=a[a$x4=="M 碩士",c(1,3)]
k2=a[a$x4=="B 學士",c(1,3)]
newdata=merge(k1,k2,by="x2")
colnames(newdata)=c("學校","碩士人數","學士人數")
plot(newdata$碩士人數,newdata$學士人數,main="各學校碩士與學士散佈圖",xlab="碩士",ylab="學士")
cor(newdata$碩士人數,newdata$學士人數)
cor.test(newdata$碩士人數,newdata$學士人數)

cor(newdata$碩士人數,newdata$學士人數,method="spearman")
cor.test(newdata$碩士人數,newdata$學士人數,method="spearman")
#(2)
school=NULL
for(i in 1:nrow(data1)){
  if(substr(data1$x2[i],1,2)=="國立"|substr(data1$x2[i],3,4)=="市立"){
   school[i]="公立"
   }else{school[i]="私立"}
}
data1_2=cbind(data1,school)
data1_2
name

library(psych)
boy=aggregate(x6~school,data1_2,sum);boy#男生計
girl=aggregate(x7~school,data1_2,sum);girl#女生計
phi=merge(boy,girl,by="school");colnames(phi)=c("學校屬性","男生","女生");phi
phi(phi[,2:3])#不同學校屬性(公立或私立)與性別間呈現正低度線性相關。
chisq.test(phi[,2:3])
#(3)
data1_3=subset(data1,data1$x4=="M 碩士"|data1$x4=="B 學士"|data1$x4=="D 博士")
par(mar=c(1,1,1,1))
school_name=c("國立臺灣大學","國立政治大學","國立清華大學","國立交通大學","輔仁大學","銘傳大學","中國文化大學")
plt=function(school_name){
  data1_3_1=data1_3[data1_3$x2==school_name,c("x2","x4","x5")]
  data1_3_1$"百分比"=data1_3_1$x5/sum(data1_3_1$x5)
  pie(data1_3_1$x5, labels = paste(data1_3_1$x4,round(data1_3_1$百分比,3)*100,"%"),main=paste0(school_name,"-學生人數的結構"))
  print(paste0(school_name,"博士佔",round(data1_3_1$"百分比"[1],3)*100,"%，","碩士佔",round(data1_3_1$"百分比"[2],3)*100,"%，","學士佔",round(data1_3_1$"百分比"[3],3)*100,"%"))
}
plt(school_name[1])
plt(school_name[2])
plt(school_name[3])
plt(school_name[4])
plt(school_name[5])
plt(school_name[6])
plt(school_name[7])
dev.off()


#第二題
data2=read.csv("108_teacher.csv",header=T)
sum(is.na(data2))
summary(data2)
data2=subset(data2,data2$日間.進修別=="日"&data2$體系別=="1 一般")
data2$教師總計=apply(data2[,4:37],1, sum)
name2=cbind(names(data2),paste0("x",c(1:length(data2))))
names(data2)=paste0("x",c(1:length(data2)))
View(data2)


#(1)
a=aggregate(x5~x2,data1,sum)
a
b=merge(a,data2[,c(2,40)],by="x2");colnames(b)=c("學校","學生總計","教師總計")
b
school_2=NULL
for(i in 1:nrow(b)){
  if(substr(b$學校[i],1,2)=="國立"|substr(b$學校[i],3,4)=="市立"){
    school_2[i]="公立"
  }else{school_2[i]="私立"}
}
b=cbind(b,school_2)
b$"師生比"=b$學生總計/b$教師總計
plot(b[b$school_2=="公立",]$學生總計,b[b$school_2=="公立",]$教師總計,col="blue",xlab="學生總計",ylab="教師總計",main="各校教師和學生規模散佈圖")
points(b[b$school_2=="私立",]$學生總計,b[b$school_2=="私立",]$教師總計,col="red")
legend("topleft",legend=c("私立", "公立"),
       col=c("red", "blue"),pch=c(1,1))
rule=matrix(0,nrow(b),1)
for(i in 1:length(rule)){
  d=subset(data1,data1$x2==b$學校[i]&data1$x4=="M 碩士")
  if(nrow(d)==0){rule[i]="no"}else if(nrow(d)>0){rule[i]="yes"}
  if(sum(rule=="yes")==nrow(b)){print("所篩選的學校皆有碩士班")}
}
plot(b$師生比,ylab="生師比",main="教育部生師比規定")
abline(h=32,col="red")
#(2)
model=lm(b$學生總計~b$教師總計)
summary(model)

#(3)
a1=b[b$school_2=="公立",];a1
a2=b[b$school_2=="私立",];a2

model1=lm(a1$學生總計~a1$教師總計)
summary(model1)

model2=lm(a2$學生總計~a2$教師總計)
summary(model2)

#(5)
#資料讀取
staff=read.table("108_staff.csv",header = T,sep=",")
name=names(staff)
names(staff)=paste0("x",c(1:length(staff)))
name_staff=cbind(name,names(staff))


library(readxl)
land=read_excel("108_land.xls",skip=8)
View(land)
data_land=land[,c(1,2,4)];names(data_land)=c("x1","x2","x4")
View(data_land)
for(i in 1:nrow(data_land)){
  if(is.na(data_land[i,1])==T){data_land[i,1]=data_land[i-1,1]}
}
library=read.table("108_library2.csv",header = T,sep=",")
name=names(library)
names(library)=paste0("x",c(1:length(library)))
name_library=cbind(name,names(library))

staff1=subset(staff,staff$x38=="1一般")
staff_sum=aggregate(x3+x4~x2,staff1,sum);colnames(staff_sum)=c("x2","職員人數總計")

land_sum=aggregate(x4~x1,data_land,sum);colnames(land_sum)=c("x2","校地面積總計")
data_5=merge(staff_sum,land_sum,by="x2")
View(land_sum)
library_sum=aggregate(x4+x5+x6+x7+x8+x9+x10+x11+x12+x13~x2,library,sum);colnames(library_sum)=c("x2","中外文圖書總計")
data_6=merge(data_5,library_sum,by="x2")
colnames(data_6)[1]="學校"
newdata=merge(b,data_6,by="學校")
dataset=newdata[,c("學校","學生總計","教師總計","職員人數總計","校地面積總計","中外文圖書總計")]
colnames(dataset)=c("y","x1","x2","x3","x4","x5")
model5=lm(y~.,data=dataset)
summary(model5)
View(dataset)


