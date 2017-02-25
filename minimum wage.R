library(foreign)
library(ggplot2)

peasant = data.frame(y=rep(0,times=500+1000+100), hour=rep(0, times=500+1000+100))
peasant$y <- c(runif(500, 1000, 4000), runif(1000,4000,50000), runif(100, 5*10^4, 10^5))
peasant$hour <- c(runif(500, 160, 336), runif(1000,160,336), runif(100, 160, 336))

peasant$minw <- peasant$y / peasant$hour
minminw <- min(peasant$minw)
maxminw <- max(peasant$minw)
supply = data.frame(wage=c(seq(from=minminw, to=maxminw/10, length.out=30),seq(from=maxminw/10, to=maxminw, length.out=70)))
supply$totalh <- rep(0, times = length(supply$wage))
supply$count <- rep(0, times = length(supply$wage))

for (i in 1:length(supply$wage)) {
  for (j in 1:length(peasant$minw)) {
    if(peasant$minw[j] < supply$wage[i]){
      supply$totalh[i] <- supply$totalh[i]+
        (160+(peasant$hour[j]-160)*supply$wage[i]/peasant$minw[j])
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


