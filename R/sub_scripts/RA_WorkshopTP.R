######################################################################
# PV(23.05.18) By  Paris Vasilakopoulos (JRC-EU, Ispra, Italy)
# Resilience Assessment
######################################################################
# rm(list = ls())
# #prepare data
# setwd("../TGAMs_Covariance matrix_biomass")
# load("funs.RData")
# 
# Data <- read.csv("Data.csv", header = TRUE)
str(Data)
Data$year<-as.numeric(Data$Year)
Data$pc1sys<-Data$PC1
Data$pc1str<-Data$chl_win

#Create lagged pc1stressor vectors
Data$pc1str1<-NA
Data$pc1str2<-NA
Data$pc1str1[c(2:NROW(Data))]<-Data$pc1str[c(1:(NROW(Data)-1))]
Data$pc1str2[c(3:NROW(Data))]<-Data$pc1str[c(1:(NROW(Data)-2))]

#get rid of years 1994-1995
Data<-Data[5:NROW(Data),]

Data<-Data[,c("year","pc1sys","pc1str2")]
rownames(Data) <- 1:nrow(Data)

########################################################################
#If x- and y-axis on different scales, now is a good time to standardise it/them
colnames(Data)[3] <- "pc1str"
#z-standardisation carried out here
S1<-scale(Data$pc1str) #save for back-calculation
#S2<-scale(Data$pc1sys) #save for back-calculation
Data$pc1str<-as.numeric(scale(Data$pc1str))
#Data$pc1sys<-as.numeric(scale(Data$pc1sys))

#use TGAM with no lag
years.char <- paste((Data$year), sep = "")

TGAM<-threshold.gam(formula(pc1sys~s(pc1str,k=3,by=I(1*(year<=r)))+s(pc1str,k=3,by=I(1*(year>r)))),a=0.1,b=0.9,data=Data,threshold.name='year',nthd=100)
summary(TGAM$res)

#identify the threshold year
plot(TGAM$rv,TGAM$gcvv,type='l',xlab='Threshold variable',ylab='GCV')
abline(v=TGAM$mr)
TGAM$mr#threshold year = 2007
#TGAM$mr=2007.1

#quick plot of the TGAM fitted vs observed values
Dataup<-Data[1:which(Data$year == round(TGAM$mr)),] #upper branch years
Datalow<-Data[which(Data$year == round(TGAM$mr)+1):NROW(Data),] #lower branch years
m<-gam(pc1sys~s(pc1str, k=3), data=Dataup)
n<-gam(pc1sys~s(pc1str, k=3), data=Datalow)
newdata1<-data.frame(pc1str=runif(1000,min(Dataup$pc1str), max(Dataup$pc1str)))
newdata2<-data.frame(pc1str=runif(1000,min(Datalow$pc1str), max(Datalow$pc1str)))
y<-predict.gam(m,newdata1)
z<-predict.gam(n,newdata2)

plot(y~newdata1$pc1str,ylim=c(min(Data$pc1sys)-sd(Data$pc1sys)/3,max(Data$pc1sys)+sd(Data$pc1sys)/3),xlim=c(min(Data$pc1str)-sd(Data$pc1str)/3,max(Data$pc1str)+sd(Data$pc1str)/3),type="n")
lines(newdata1$pc1str[order(newdata1$pc1str)], y[order(newdata1$pc1str)], xlim=range(newdata1$pc1str), ylim=range(y))
par(new=TRUE)
plot(z~newdata2$pc1str,ylim=c(min(Data$pc1sys)-sd(Data$pc1sys)/3,max(Data$pc1sys)+sd(Data$pc1sys)/3),xlim=c(min(Data$pc1str)-sd(Data$pc1str)/3,max(Data$pc1str)+sd(Data$pc1str)/3),type="n")
lines(newdata2$pc1str[order(newdata2$pc1str)], z[order(newdata2$pc1str)], xlim=range(newdata2$pc1str), ylim=range(z))
with(Data, text(Data$pc1sys~Data$pc1str, labels = years.char))

#Calculate vertical component of resilience
#here I'm simply using the residuals for this, but one could calculate actual distance
#of point from line (but little effect on the shape of the stability landscape)
Data$vc<-abs(TGAM$res$residuals)

#Identify the position of tipping points
#First tipping point(F1):
Data<-rbind(Data, c("F1",NA,NA,0)) #F1 obviously has vc=0 
Data[,2:ncol(Data)]<-sapply(Data[,2:ncol(Data)],as.numeric)

#calculate x-value (pc1str) of F1
ff1<-which.max(rowSums(Data[1:which(Data$year == round(TGAM$mr)),3:4])+0.1) #may add -1 here to force tipping year's resilience not to be calculated based on upper branch
ff1<-names(ff1)
Data[nrow(Data),3]<--abs(Data[ff1,3])-abs(Data[ff1,ncol(Data)])
#this x-value for F1 ensures that all Res estimates of upper branch will be non-negative. 
#0.05 is arbitrary to give some extra space - can be omitted

#calculate y-value (pc1sys) of F1
o<-gam(pc1sys~s(pc1str, k=3), data=rbind(Dataup,Data[NROW(Data),1:3]))
Data[nrow(Data),2]<-predict.gam(o, newdata=data.frame(pc1str=Data[nrow(Data),3]))

#Second tipping point (F2):
Data<-rbind(Data, c("F2",NA,NA,0))
Data[,2:ncol(Data)]<-sapply(Data[,2:ncol(Data)],as.numeric)
#Data$vc<--Data$vc

#calculate x-value (pc1str) of F2
ff2<-which.max(rowSums(Data[which(Data$year == round(TGAM$mr)+1):NROW(Data),3:4]))
ff2<-names(ff2)
Data[nrow(Data),3]<-Data[ff2,3]+abs(Data[ff2,ncol(Data)]+0.1)
#this x-value for F2 ensures that all Res estimates of lower branch will be non-negative.
#0.05 is arbitrary to give some extra space

#calculate y-value (pc1sys) of F2
p<-gam(pc1sys~s(pc1str, k=3), data=rbind(Datalow,Data[NROW(Data),1:3]))
Data[nrow(Data),2]<-predict.gam(p, newdata=data.frame(pc1str=Data[nrow(Data),3]))

#calculate the horizontal component of resilience
Data$hc<-NA
Data$hc[1:which(Data$year == round(TGAM$mr))]<-abs(Data$pc1str[nrow(Data)-1]-Data$pc1str[1:which(Data$year == round(TGAM$mr))])

Data$hc[which(Data$year == round(TGAM$mr)+1):(nrow(Data)-2)]<-abs(Data$pc1str[which(Data$year == round(TGAM$mr)+1):(nrow(Data)-2)]-Data$pc1str[nrow(Data)])
Data$hc[(nrow(Data)-1):nrow(Data)]<-0

#calculate resilience
Data$res<-Data$hc-Data$vc
Data$res[13]=0 #manually set because it is unclear if part of first or second state

#calculate relative resilience
Data$relres<-Data$res/max(Data$res)

#prepare line extensions to tipping points
newdata3<-data.frame(pc1str=runif(100,Data$pc1str[NROW(Data)-1],min(Dataup$pc1str)))
newdata4<-data.frame(pc1str=runif(100,max(Datalow$pc1str),Data$pc1str[NROW(Data)]))
x<-predict.gam(o,newdata3)
w<-predict.gam(p,newdata4)

#adding two theoretical points at the beginning of each branch to stabilize the shape of the stability landscape:
# newdata<-rbind(Data, c("T1",predict.gam(m, newdata=data.frame(pc1str=min(Dataup$pc1str))),
#                        min(Dataup$pc1str),0,NA,NA,NA))
# newdata<-rbind(newdata, c("T2",predict.gam(n, newdata=data.frame(pc1str=max(Datalow$pc1str))),
#                           max(Datalow$pc1str),0,NA,NA,NA))
# Data<-newdata
# Data[,2:ncol(Data)]<-sapply(Data[,2:ncol(Data)],as.numeric)
# Data$hc[NROW(Data)-1]<-
#   abs(Data$pc1str[NROW(Data)-3]-Data$pc1str[NROW(Data)-1])
# Data$hc[NROW(Data)]<-
#   abs(Data$pc1str[NROW(Data)]-Data$pc1str[NROW(Data)-2])
# 

#unscale stressors
Data$pc1str= Data$pc1str * attr(S1, 'scaled:scale') + attr(S1, 'scaled:center')
newdata1$pc1str= newdata1$pc1str * attr(S1, 'scaled:scale') + attr(S1, 'scaled:center')
newdata2$pc1str= newdata2$pc1str * attr(S1, 'scaled:scale') + attr(S1, 'scaled:center')
newdata3$pc1str= newdata3$pc1str * attr(S1, 'scaled:scale') + attr(S1, 'scaled:center')
newdata4$pc1str= newdata4$pc1str * attr(S1, 'scaled:scale') + attr(S1, 'scaled:center')

# #unscale system
# Data$pc1sys= Data$pc1sys * attr(S2, 'scaled:scale') + attr(S2, 'scaled:center')
# newdata1$pc1sys= newdata1$pc1sys * attr(S2, 'scaled:scale') + attr(S2, 'scaled:center')
# newdata2$pc1sys= newdata2$pc1sys * attr(S2, 'scaled:scale') + attr(S2, 'scaled:center')
# newdata3$pc1sys= newdata3$pc1sys * attr(S2, 'scaled:scale') + attr(S2, 'scaled:center')
# newdata4$pc1sys= newdata4$pc1sys * attr(S2, 'scaled:scale') + attr(S2, 'scaled:center')

#interpolate to build stability landscape
library(akima)

xx<-interp(Data$pc1str,Data$pc1sys,-Data$relres, xo=seq(min(Data$pc1str), max(Data$pc1str),length=500),yo=seq(min(Data$pc1sys), max(Data$pc1sys),length=500))

#add 1994-95
newdata<-rbind(c("1995",2.862,
                       0.785,0,NA,NA,NA),Data)
newdata<-rbind(c("1994",3.166,
                       0.732,0,NA,NA,NA),newdata)
Data<-newdata
Data$pc1sys<-as.numeric(Data$pc1sys)
Data$pc1str<-as.numeric(Data$pc1str)
Data$res<-as.numeric(Data$res)
Data$relres<-as.numeric(Data$relres)

#Folded stability landscape
years.char <- paste((Data$year), sep = "")

#So, here's the figure (first version):
#tiff(file = "GSA6plot.tiff",width = 3500,height = 2400, res=300, compression="lzw")
filled.contour(xx, col=terrain.colors(21), ylim=c(min(Data$pc1sys)-0.2,max(Data$pc1sys)+0.2),xlim=c(min(Data$pc1str)-0.1,max(Data$pc1str)+0.1),
               plot.axes = {points(Data$pc1sys[1:(NROW(Data)-2)]~Data$pc1str[1:(NROW(Data)-2)], type="n");
                 points(Data$pc1sys[(NROW(Data)-1):(NROW(Data))]~Data$pc1str[(NROW(Data)-1):(NROW(Data))],pch=16);
               #  points(Data$pc1sys[(NROW(Data)-1):NROW(Data)]~Data$pc1str[(NROW(Data)-1):NROW(Data)], type="n");
                 axis(1); axis(2); lines(newdata2$pc1str[order(newdata2$pc1str)], z[order(newdata2$pc1str)],lwd="2");
lines(newdata1$pc1str[order(newdata1$pc1str)], y[order(newdata1$pc1str)],lwd="2");
lines(newdata3$pc1str[order(newdata3$pc1str)], x[order(newdata3$pc1str)],lwd="2",lty=3);
lines(newdata4$pc1str[order(newdata4$pc1str)], w[order(newdata4$pc1str)],lwd="2",lty=3);
  #  with(Data, text(Data$pc1sys~Data$pc1str, labels = years.char)) 
with(Data, text(Data$pc1sys[1:(NROW(Data)-2)]~Data$pc1str[1:(NROW(Data)-2)], labels = years.char[1:(NROW(Data)-2)],cex=0.8)) 
with(Data, text(Data$pc1sys[(NROW(Data)-1)]~Data$pc1str[(NROW(Data)-1)], labels = years.char[(NROW(Data)-1)],pos=2,cex=1.1))
with(Data, text(Data$pc1sys[(NROW(Data))]~Data$pc1str[(NROW(Data))], labels = years.char[(NROW(Data))],pos=4,cex=1.1))
},
key.axes= axis(4,at=seq(-1,0,by=0.2),labels=paste(seq(1,0,by=-0.2))),
plot.title=title(xlab="Stressors (chla_win; 2-year lag)",ylab="Northern Spain community (PC1sys)"))
#dev.off()
#Dashed line between tipping points should be drawn in PowerPoint (for now)

