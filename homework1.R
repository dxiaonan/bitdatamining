library(car)
library(lattice)
setwd("F:/zeo_dxn/personal space/datamining/data")
#==读取数据==
hdata = read.table('horse-colic.data', 
                       col.names=c('surgery','age','hospital number','rectal temperture','pulse',
                                   'respiratory rate','temperature of extremities','peripheral pulse',
                                   'mucous membranes','capillary refill time','pain','peristalsis',
                                   'abdominal distension','nasogastric tube','nasogastric reflux',
                                   'nasogastric reflux PH','rectal examination','abdomen','packed cell volume',
                                   'total protein','abdominocentesis appearance','abdomcentesis total protein',
                                   'outcome','surgical lesion','type of lesion 25','type of lesion 26',
                                   'type of lesion 27','cp_data'),
                       na.string=c('?'))
#==数据Tag 整理==
hdata$surgery<-factor(hdata$surgery,labels=c('yes','no'))
hdata$age<-factor(hdata$age,labels=c('adult','young'))
hdata$hospital.number<-as.character(hdata$hospital.number)
hdata$temperature.of.extremities<-factor(hdata$temperature.of.extremities,labels=c('normal','warm','cool','cold'))
hdata$peripheral.pulse<-factor(hdata$peripheral.pulse,labels = c('normal','increase','reduced','absent'))
hdata$mucous.membranes<-factor(hdata$mucous.membranes,labels =c('normal pink','bright pink','pale pink','pale cyanotic','bright red/injected','dark cyanotic'))
hdata$capillary.refill.time<-factor(hdata$capillary.refill.time,labels = c("<3 seconds",">3 seconds","?"))
hdata$pain<-factor(hdata$pain,labels = c('alert, no pain','depressed','intermittent mild pain','intermittent severe pain','continuous severe pain'))
hdata$peristalsis<-factor(hdata$peristalsis,labels = c("hypermotile","normal"	,"hypomotile","absent"))
hdata$abdominal.distension<-factor(hdata$abdominal.distension,labels =c("none",	"slight",	"oderate","severe"))
hdata$nasogastric.tube<-factor(hdata$nasogastric.tube,labels = c("none", "slight", "significant")) 
hdata$nasogastric.reflux<-factor(hdata$nasogastric.reflux,labels = c("none",">1 liter","<1 liter"))
hdata$rectal.examination<-factor(hdata$rectal.examination,labels = c('normal','increase','reduced','absent'))
hdata$abdomen<-factor(hdata$abdomen,labels = c('nomal','other','firm feces in the large intestine','distended small intestine','distended large intestine'))
hdata$abdominocentesis.appearance<-factor(hdata$abdominocentesis.appearance,labels = c('clear','cloudy','serosanguinous'))
hdata$outcome<-factor(hdata$outcome,labels = c('lived','died','was euthanized'))
hdata$surgical.lesion<-factor(hdata$surgical.lesion,labels = c('yes','no'))
hdata$cp_data<-factor(hdata$cp_data,labels = c('yes','no'))

#==数据摘要==

summary(hdata)

#==数据可视化==
#1
#绘制直方图
hist(hdata$pulse, xlab='', main='Histogram of pulse value')
rug(hdata$pulse)
#绘制QQ图
qqPlot(hdata$pulse, xlab='', main='Norm QQ Plot of pulse')
#绘制箱形图
boxplot(hdata$pulse, main='Box Plot of pulse')
rug(hdata$pulse,side=2)
abline(h=mean(hdata$pulse,na.rm=T),lty=2)
#2
hist(hdata$respiratory.rate, xlab='', main='Histogram of respiratory.rate value')
rug(hdata$respiratory.rate)
qqPlot(hdata$respiratory.rate, xlab='', main='Norm QQ Plot of respiratory.rate')
boxplot(hdata$respiratory.rate, main='Box Plot of respiratory.rate')
rug(hdata$respiratory.rate,side=2)
abline(h=mean(hdata$respiratory.rate,na.rm=T),lty=2)
#3
hist(hdata$rectal.temperture, xlab='', main='Histogram of rectal.temperture value')
rug(hdata$rectal.temperture)
qqPlot(hdata$rectal.temperture, xlab='', main='Norm QQ Plot of rectal.temperture')
boxplot(hdata$rectal.temperture, main='Box Plot of rectal.temperture')
rug(hdata$rectal.temperture,side=2)
abline(h=mean(hdata$rectal.temperture,na.rm=T),lty=2)
#4
hist(hdata$nasogastric.reflux.PH, xlab='', main='Histogram of reflux.PH')
rug(hdata$nasogastric.reflux.PH)
qqPlot(hdata$nasogastric.reflux.PH, xlab='', main='Norm QQ Plot of reflux.PH')
boxplot(hdata$nasogastric.reflux.PH, main='Box Plot of reflux.PH')
rug(hdata$nasogastric.reflux.PH,side=2)
abline(h=mean(hdata$nasogastric.reflux.PH,na.rm=T),lty=2)
#5
hist(hdata$total.protein, xlab='', main='Histogram of total.protein')
rug(hdata$total.protein)
qqPlot(hdata$total.protein, xlab='', main='Norm QQ Plot of total.protein')
boxplot(hdata$total.protein, main='Box Plot of total.protein')
rug(hdata$total.protein,side=2)
abline(h=mean(hdata$total.protein,na.rm=T),lty=2)
#6
hist(hdata$packed.cell.volume, xlab='', main='Histogram of packed.cell.volume')
rug(hdata$packed.cell.volume)
qqPlot(hdata$packed.cell.volume, xlab='', main='Norm QQ Plot of packed.cell.volume')
boxplot(hdata$packed.cell.volume, main='Box Plot of packed.cell.volume')
rug(hdata$packed.cell.volume,side=2)
abline(h=mean(hdata$packed.cell.volume,na.rm=T),lty=2)
#7
hist(hdata$abdomcentesis.total.protein, xlab='', main='Histogram of abdomcentesis.total.protein')
rug(hdata$abdomcentesis.total.protein)
qqPlot(hdata$abdomcentesis.total.protein, xlab='', main='Norm QQ Plot of abdomcentesis.total.protein')
boxplot(hdata$total.protein, main='Box Plot of abdomcentesis.total.protein')
rug(hdata$abdomcentesis.total.protein,side=2)
abline(h=mean(hdata$abdomcentesis.total.protein,na.rm=T),lty=2)


#==缺失数据处理==
#剔除缺失数据
omitdata = na.omit(hdata)
write.table(omitdata,'Omited.txt',col.names = F,row.names = F, quote = F)

#使用高频数据替换
library("DMwR")
library(grid)
replace = hdata[-manyNAs(hdata),]
preprocess2 = centralImputation(replace )
write.table(preprocess2,'CentralImputation.txt',col.names = F,row.names = F, quote = F)

#通过变量相关性填补缺失值
symnum(cor(hdata$pulse,hdata$respiratory.rate,use='complete.obs'))
lm(formula=respiratory.rate~pulse, data=hdata)
preprocess3 = hdata[-manyNAs(hdata),]
fillrespiratory.rate <- function(pulse){
  if(is.na(pulse))    return(NA)
  else return (10.2001 + 0.2867 * pulse)
}
preprocess3[is.na(preprocess3$respiratory.rate),'respiratory.rate'] <- sapply(preprocess3[is.na(preprocess3$respiratory.rate),'respiratory.rate'],fillrespiratory.rate)
write.table(preprocess3,'linearDefault.txt',
            col.names = F,row.names = F, quote = F)

#通过案例的相关性填补缺失值

preprocess4 <-knnImputation(hdata,k=10)
write.table(preprocess4,'knnImputation.txt',
            col.names = F,row.names = F, quote = F)
library("VIM")
matrixplot(hdata)
matrixplot(omitdata)
matrixplot(preprocess2)
matrixplot(preprocess3)
