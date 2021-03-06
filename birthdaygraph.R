#load data directly from me
bdata <- read.csv("http://chmullig.com/wp-content/uploads/2012/06/births.csv")

#filter the junk that remains...
bdata<-bdata[(bdata$births > 1000),]

bdata$order <- rank(bdata$month + bdata$day/100)

#smooth
bdata$births[bdata$month==2 & bdata$day==29] <- bdata$births[bdata$month==2 & bdata$day==29]*4
birthloess = loess(births ~ order, span=1, iter=6, bdata)
bdata$predict <- predict(birthloess, bdata$order)
bdata$flag <- ifelse(bdata$births - bdata$predict > sd(bdata$births)*2.3, 3,
    ifelse(bdata$births - bdata$predict < sd(bdata$births)*-2.3, 1, 0))

#flag special days we might care about
bdata$flag[bdata$month==2 & bdata$day==14] <- 3
bdata$flag[bdata$month==10 & bdata$day==31] <- 1
bdata$flag[bdata$month==2 & bdata$day==29] <- 4

#plot
#png(file="birthdays.png", width=768, height=768)
#png(file="birthdays_hq.png", width=4267, height=4267, res=400)
#pdf(file="birthdays_hq.pdf", pointsize=10, height=10, width=10)

setEPS()
postscript("birthdaygraph.eps", width=6, height=6, pointsize=9)

plot(c(-366+bdata$order[bdata$month==12], bdata$order, 366+bdata$order[bdata$month==1]),
    c(bdata$births[bdata$month==12]/mean(bdata$births)*100, bdata$births/mean(bdata$births)*100, bdata$births[bdata$month==1]/mean(bdata$births)*100),
    xaxs="i", xaxt="n", xlab="Day", ylab="Births (% of average)", main="Births by Day of Year", type="l", lwd=0, cex=2)
    
axis(1, at=c(-30, by(bdata$order, list(bdata$month), min), 367),
    labels=c("Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan"),
    tck=1, lwd=0.5, lty=2, col="grey", cex.axis=0.99)
title(sub="Source: National Vital Statistics System natality data 1969-1988, as provided by Google BigQuery.  Graph by Chris Mulligan (chmullig.com)", cex.sub=.7, adj=0.5)

#shade months 0 and 13 
rect(-30, 0, 1, 200, col = "grey90", lwd=0)
rect(366, 0, 397, 200, col = "grey90", lwd=0)

#add the mean line
abline(a=100, b=0, col="grey20", lwd=0.5)


#label some outliers
with(bdata, text(order[flag==1], (births[flag==1]/mean(births))*100, paste(month[flag==1], "/", day[flag==1]), pos=1, cex=1))
with(bdata, text(order[flag==3], (births[flag==3]/mean(births))*100, paste(month[flag==3], "/", day[flag==3]), pos=3, cex=1))
with(bdata, text(order[flag==4], (births[flag==4]/mean(births))*100, paste(month[flag==4], " / ", day[flag==4], "*4", sep=""), pos=3, cex=1, offset=-1))

#write the actual line
lines(c(-366+bdata$order[bdata$month==12], bdata$order, 366+bdata$order[bdata$month==1]),
    c(bdata$births[bdata$month==12]/mean(bdata$births)*100, bdata$births/mean(bdata$births)*100, bdata$births[bdata$month==1]/mean(bdata$births)*100),
    xaxt="n", xlab="Day", ylab="Births", type="l", lwd=1.5)

dev.off()