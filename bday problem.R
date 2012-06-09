#bdata <- read.csv("births.csv")
#load data directly from me
bdata <- read.csv("http://chmullig.com/wp-content/uploads/2012/06/births.csv")

bdata <- bdata[(bdata$births > 1000),]
bdata$dm <- bdata$month + bdata$day/100
bdata$rel <- bdata$births/sum(bdata$births)

m <- 30000
#m <- 1000
top <- 75
results_real <- rep(0,top)
results_synthetic <- rep(0,top)

for (i in 1:top) {
	matches <- 0
	for (j in 1:m) {
		trial <- sample(bdata$dm, i, replace=TRUE, prob=bdata$rel)
		if (length(unique(trial)) < i) {
			matches <- matches + 1
		}
	}
	results_real[i] <- matches/m
}
for (i in 1:top) {
	matches <- 0
	for (j in 1:m) {
		trial <- sample(1:365, i, replace=TRUE)
		if (length(unique(trial)) < i) {
			matches <- matches + 1
		}
	}
	results_synthetic[i] <- matches/m
}
png("bday_problem.png", width=768, height=768)
plot(results_synthetic, xlab=paste("Group Size. Trials per n:", m), ylab="Probability of Matching Birthdays", main="Birthday Problem with Real Data", col="red", type="l")
lines(results_real, col="black")
legend("bottomright", c("Synthetic", "Real"), col=c("red", "black"), lty=1, lwd=1)
title(sub="Source: National Vital Statistics System natality data 1969-1988, as provided by Google BigQuery.  Graph by Chris Mulligan (chmullig.com)", cex.sub=1, adj=0.75)
dev.off()

results_diff <- results_real - results_synthetic
diffmax <- max(abs(results_diff))

res <- data.frame(n=1:top, diff=results_diff, real=results_real, synthetic=results_synthetic)
res$smoothdiff <- predict(loess(diff ~ n, span=0.75, data=res), res$n)

png("bday_difference.png", width=768, height=768)
plot(results_diff, xlab=paste("Group Size. Trials per n:", m), ylab="P(Real) minus P(Syntehtic)", ylim=c(-diffmax, diffmax), main="Real minus Synthetic likelihood of matching birthdays")
abline(0,0, col="grey")
lines(res$n, res$smoothdiff)
title(sub="Source: National Vital Statistics System natality data 1969-1988, as provided by Google BigQuery.  Graph by Chris Mulligan (chmullig.com)", cex.sub=1, adj=0.75)
dev.off()

write.csv(res, file="bday_problem.csv")