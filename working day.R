# install.packages("ggplot2")
# install.packages("gdata")
setwd("~/Documents/code/R/coexistence of unemployment and overwork")

library(foreign)
library(ggplot2)
require(gdata)

peasant = data.frame(y=rep(0,times=500+1000+100), hour=rep(0, times=500+1000+100))
peasant$y <- c(runif(500, 1000, 4000), runif(1000,4000,50000), runif(100, 5*10^4, 10^5))
peasant$hour <- c(runif(500, 160, 336), runif(1000,160,336), runif(100, 160, 336))


ggplot(peasant, aes(x=hour,y=y))+
  geom_point()+ylab("生活支出")+xlab("劳动上限")+
  theme(text=element_text(family="STKaiti",size=14))
peasant$minw <- peasant$y / peasant$hour
minminw <- min(peasant$minw)
maxminw <- max(peasant$minw)
supply = data.frame(wage=c(seq(from=minminw, to=maxminw/10, length.out=30),seq(from=maxminw/10, to=maxminw, length.out=70)))
supply$totalh <- rep(0, times = length(supply$wage))
supply$count <- rep(0, times = length(supply$wage))

for (i in 1:length(supply$wage)) {
  for (j in 1:length(peasant$minw)) {
    if(peasant$minw[j] < supply$wage[i]){
        supply$totalh[i] <- supply$totalh[i]+max(min(peasant$y[j]/supply$wage[i], peasant$hour[j]),8)
        supply$count[i] <- supply$count[i]+ 1
        }
        else{
          
        }
    }
}
supply$averageh <- supply$totalh/supply$count
supply <- supply[-c(1:5),]  
ggplot(supply, aes(y=wage,x=totalh))+
  geom_point()+ylab("小时工资率")+xlab("总劳动时间")+
  theme(text=element_text(family="STKaiti",size=14))

ggplot(supply, aes(y=wage,x=averageh))+
  geom_point()+ylab("小时工资率")+xlab("人均劳动时间")+
  theme(text=element_text(family="STKaiti",size=14))

ggplot(supply, aes(y=wage,x=count))+
  geom_point()+ylab("小时工资率")+xlab("产业后备军规模")+
  theme(text=element_text(family="STKaiti",size=14))


## ## 
peasant$minw <- peasant$y / peasant$hour
minminw <- c(min(peasant$minw[1:500]), min(peasant$minw[501:1500]), min(peasant$minw[1501:1600]))
maxminw <- c(max(peasant$minw[1:500]), max(peasant$minw[501:1500]), max(peasant$minw[1501:1600]))

supply0 <- data.frame(wage=c(seq(from=minminw[1], to=maxminw[1], length.out=50),seq(from=minminw[2], to=maxminw[2], length.out=50),
                             seq(from=minminw[3], to=maxminw[3],length.out = 50)),
                      factor = c(rep(1, times=50), rep(2, times=50),rep(3, times=50)))
supply0$totalh <- rep(0, times = length(supply0$wage))
supply0$count <- rep(0, times = length(supply0$wage))

for (i in 1:50) {
  for (j in 1:500) {
    if(peasant$minw[j] < supply0$wage[i]){
      supply0$totalh[i] <- supply0$totalh[i]+min(peasant$y[j]/supply0$wage[i], peasant$hour[j])
      supply0$count[i] <- supply0$count[i]+ 1
    }
    else{
      
    }
  }
}

for (i in 51:100) {
  for (j in 501:1500) {
    if(peasant$minw[j] < supply0$wage[i]){
      supply0$totalh[i] <- supply0$totalh[i]+min(peasant$y[j]/supply0$wage[i], peasant$hour[j])
      supply0$count[i] <- supply0$count[i]+ 1
    }
    else{
      
    }
  }
}

for (i in 101:150) {
  for (j in 1501:1600) {
    if(peasant$minw[j] < supply0$wage[i]){
      supply0$totalh[i] <- supply0$totalh[i]+min(peasant$y[j]/supply0$wage[i], peasant$hour[j])
      supply0$count[i] <- supply0$count[i]+ 1
    }
    else{
      
    }
  }
}


supply0$averageh <- supply0$totalh/supply0$count
ggplot(supply0, aes(y=wage,x=totalh,colour=as.factor(factor),shape=as.factor(factor)))+
  geom_point(size=3)+ylab("小时工资率")+xlab("总劳动时间")+
  theme(text=element_text(family="STKaiti",size=14), legend.position="bottom")+
  scale_shape_discrete(name="劳动力群体",
                       labels=c("底层劳动者", "中层劳动者", "高层劳动者"))+
  scale_colour_discrete(name="劳动力群体",
                       labels=c("底层劳动者", "中层劳动者", "高层劳动者"))


ggplot(supply0, aes(y=wage,x=averageh,colour=as.factor(factor),shape=as.factor(factor)))+
  geom_point(size=3)+ylab("小时工资率")+xlab("人均劳动时间")+
  theme(text=element_text(family="STKaiti",size=14), legend.position="bottom")+
  scale_shape_discrete(name="劳动力群体",
                       labels=c("底层劳动者", "中层劳动者", "高层劳动者"))+
  scale_colour_discrete(name="劳动力群体",
                        labels=c("底层劳动者", "中层劳动者", "高层劳动者"))

ggplot(supply0, aes(y=wage,x=averageh))+
  geom_point()+ylab("小时工资率")+xlab("人均劳动时间")+
  theme(text=element_text(family="STKaiti",size=14))

ggplot(supply, aes(y=wage,x=count))+
  geom_point()+ylab("小时工资率")+xlab("产业后备军规模")+
  theme(text=element_text(family="STKaiti",size=14))



## ## ##
supply1 <- data.frame(wage=c(seq(from=minminw[1], to=maxminw[2], length.out=50),seq(from=minminw[2], to=maxminw[3], length.out=50)),
                      factor = c(rep(1, times=50), rep(2, times=50)))
supply1$totalh <- rep(0, times = length(supply1$wage))
supply1$count <- rep(0, times = length(supply1$wage))

for (i in 1:50) {
  for (j in 1:1500) {
    if(peasant$minw[j] < supply1$wage[i]){
      supply1$totalh[i] <- supply1$totalh[i]+min(peasant$y[j]/supply1$wage[i], peasant$hour[j])
      supply1$count[i] <- supply1$count[i]+ 1
    }
    else{
      
    }
  }
}

for (i in 51:100) {
  for (j in 501:1600) {
    if(peasant$minw[j] < supply1$wage[i]){
      supply1$totalh[i] <- supply1$totalh[i]+min(peasant$y[j]/supply1$wage[i], peasant$hour[j])
      supply1$count[i] <- supply1$count[i]+ 1
    }
    else{
      
    }
  }
}



supply1$averageh <- supply1$totalh/supply1$count


ggplot(supply1, aes(y=wage,x=totalh,colour=as.factor(factor),shape=as.factor(factor)))+
  geom_point(size=3)+ylab("小时工资率")+xlab("总劳动时间")+
  theme(text=element_text(family="STKaiti",size=14), legend.position="bottom")+
  scale_shape_discrete(name="劳动力群体",labels=c("中低层劳动者", "中高层劳动者"))+
  scale_colour_discrete(name="劳动力群体",
                        labels=c("中低层劳动者", "中高层劳动者"))


ggplot(supply1, aes(y=wage,x=totalh,shape=factor(factor), colour=factor(factor)))+
  geom_point()+ylab("小时工资率")+xlab("总劳动时间")+
  theme(text=element_text(family="STKaiti",size=14))

ggplot(supply1, aes(y=wage,x=averageh,colour=as.factor(factor),shape=as.factor(factor)))+
  geom_point(size=3)+ylab("小时工资率")+xlab("人均劳动时间")+
  theme(text=element_text(family="STKaiti",size=14), legend.position="bottom")+
  scale_shape_discrete(name="劳动力群体",
                       labels=c("中低层劳动者", "中高劳动者"))+
  scale_colour_discrete(name="劳动力群体",
                        labels=c("中低层劳动者", "中高层劳动者"))


ggplot(supply1, aes(y=wage,x=averageh))+
  geom_point()+ylab("小时工资率")+xlab("人均劳动时间")+
  theme(text=element_text(family="STKaiti",size=14))

ggplot(supply, aes(y=wage,x=count))+
  geom_point()+ylab("小时工资率")+xlab("产业后备军规模")+
  theme(text=element_text(family="STKaiti",size=14))

