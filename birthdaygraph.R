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
png(file="birthdays.png", width=768, height=768)
plot(births ~ order, data=bdata, xaxt="n", xlab="Day", ylab="Births", main="Births by Day of Year", type="l", lwd=0, cex=2)
axis(1, at=by(bdata$order, list(bdata$month), min),
    labels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
    tck=1, lwd=0.5, lty=2, col="grey", cex.axis=0.99)
title(sub="Source: National Vital Statistics System natality data, as provided by Google BigQuery.  Graph by Chris Mulligan (chmullig.com)", cex.sub=1, adj=0.75)

#add the mean line
abline(a=sum(bdata$births)/365.25, b=0, col="red", lwd=0.5)

#add the smoothed line
lines(bdata$order, bdata$predict, col="blue", lwd=0.5)

legend("topleft", c("Births", "Smoothed", "Mean"), col=c("black", "blue", "red"), lty=1, lwd=2, bty="n")

#label some outliers
with(bdata, text(order[flag==1], births[flag==1], paste(month[flag==1], "/", day[flag==1]), pos=1, cex=1))
with(bdata, text(order[flag==3], births[flag==3], paste(month[flag==3], "/", day[flag==3]), pos=3, cex=1))
with(bdata, text(order[flag==4], births[flag==4], paste(month[flag==4], " / ", day[flag==4], "*4", sep=""), pos=3, cex=1, offset=-1))

#write the actual line
lines(births ~ order, data=bdata, xaxt="n", xlab="Day", ylab="Births", type="l", lwd=1.5)

dev.off()