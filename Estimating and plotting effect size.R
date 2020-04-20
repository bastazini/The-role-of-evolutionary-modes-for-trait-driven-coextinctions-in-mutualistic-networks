require(effsize)
teste1=read.table(file.choose(),h=T)

(cohen.d(teste1$robustness_distinc,teste1$robustness_randon))
cohen.d(teste1$robustness_trait,teste1$robustness_randon)


#graphs
x=c(1.2,1.8)
avg=c(-0.8940226, -0.9342016)
lower=c(-0.9205609,-0.9608505)
upper=c(-0.8674843,-0.9075528)
plot(1, type="n", xlab="", xaxt="n", ylab="Effect size (95%IC)", xlim=c(1, 2), ylim=c(-.97, 0))
points(x,avg, pch=16,cex=1.5)
arrows(x, lower, x, upper, length=0.05, angle=90, code=3,lwd=2.5)
abline(h=0,lty=2, lwd=2)
axis(side=1,at=x,label=c("Trait distinctess", "Trait size"),cex.lab=14, las=1)
par(mfrow=c(1,2))