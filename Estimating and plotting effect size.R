require(effsize)
##Loading data
random=read.table(file.choose(),h=T)
bm=read.table(file.choose(),h=T)
strong=read.table(file.choose(),h=T)
weak=read.table(file.choose(),h=T)

bm2=subset(bm$robustness_distinc, bm$power_H == 1);bm
rho_n=subset(bm$robustness_distinc, bm$power_H == 5);rho_n
cohen.d(rho_n,bm2)

#Graph comparing functiona extinctions vs random
teste=rbind(random,bm,strong, weak)

cohen.d(teste$robustness_trait,teste$robustness_randon)
cohen.d(teste$robustness_distinc,teste$robustness_randon)

dev.off()
par(mar=c(5,11,2,2))
x=c(0.8,1.8)
avg=c(-0.9223311,-0.8472918)
lower=c(-0.9356384,-0.8604990)
upper=c(-0.9090237,-0.83)

plot(1, type="n", ylab="", yaxt="n", xlab="Effect size (95%IC)", xlim=c(-.95, 0.05), ylim=c(0.6, 2), main="")
points(avg,x, pch=16,cex=1.5)
arrows(lower, x, upper,  x, length=0.05, angle=90, code=3,lwd=2.5)
abline(v=0,lty=2, lwd=3,col="red")
axis(side=2,at=x,label=c("Trait size","Functional distinctiveness"),cex.lab=14, las=2)




##Combining results
teste=rbind(random, bm,strong, weak)
bm2=subset(bm$robustness_distinc, bm$power_H == 1);bm
rho_n=subset(bm$robustness_distinc, bm$power_H == 5);rho_n
#1e-04,2,5
cohen.d(rho_n,bm2)


par(mar=c(5,11,2,2))
par(mfrow=c(3,1))
#graphs For the three scenarios
x=c(1,2,3)
avg=c(0.00687642,-0.09054176,-0.3089282)
lower=c(-0.04374000,-0.14118395,-0.3598455)
upper=c(0.05749284,-0.03989956,-0.2580109)


plot(1, type="n", ylab="", yaxt="n", xlab="", xlim=c(-.4, 0.1), ylim=c(0, 4), main="Random")
points(avg,x, pch=16,cex=1.5)
arrows(lower, x, upper,  x, length=0.05, angle=90, code=3,lwd=2.5)
abline(v=0,lty=2, lwd=3,col="red")
axis(side=2,at=x,label=c("Weak phylogenetic signal","Strong phylogenetic signal", "Strong phylognetic signal"),cex.lab=14, las=2)

##Trait size
x=c(1,2,3)
avg=c(0.01918798,-0.05514538,-0.1422591)
lower=c(-0.03142945,-0.105771267,-0.19293938)
upper=c( 0.06980541,-0.004,-0.09157888)
plot(1, type="n", ylab="", yaxt="n", xlab="", xlim=c(-.4, 0.1), ylim=c(0, 4),main="Trait size")
points(avg,x, pch=16,cex=1.5)
arrows(lower, x, upper,  x, length=0.05, angle=90, code=3,lwd=2.5)
abline(v=0,lty=2, lwd=3,col="red")
axis(side=2,at=x,label=c("Weak phylogenetic signal","Strong phylogenetic signal", "Strong phylognetic signal"),cex.lab=14, las=2)

##Functional distinctvness
x=c(.8,1.8)
avg=c()
lower=c()
upper=c()
plot(1, type="n", ylab="", yaxt="n", xlab="Effect size (95%IC)", xlim=c(-.4, 0.1), ylim=c(0, 4), main="Functional distinctiveness ")
points(avg,x, pch=16,cex=1.5)
arrows(lower, x, upper,  x, length=0.05, angle=90, code=3,lwd=2.5)
abline(v=0,lty=2, lwd=3,col="red")
axis(side=2,at=x,label=c("Trait","Strong phylogenetic signal", "Strong phylognetic signal"),cex.lab=14, las=2)

###



##Combining results
teste=rbind(random, bm,strong, weak)
bm2=subset(bm$robustness_distinc, bm$power_H == 1);bm
rho_n=subset(bm$robustness_distinc, bm$power_H == 5);rho_n
#1e-04,2,5
cohen.d(rho_n,bm2)


par(mar=c(5,11,2,2))
par(mfrow=c(3,1))
#graphs For the three scenarios
x=c(1,2,3)
avg=c(0.00687642,-0.09054176,-0.3089282)
lower=c(-0.04374000,-0.14118395,-0.3598455)
upper=c(0.05749284,-0.03989956,-0.2580109)


plot(1, type="n", ylab="", yaxt="n", xlab="", xlim=c(-.4, 0.1), ylim=c(0, 4), main="Random")
points(avg,x, pch=16,cex=1.5)
arrows(lower, x, upper,  x, length=0.05, angle=90, code=3,lwd=2.5)
abline(v=0,lty=2, lwd=3,col="red")
axis(side=2,at=x,label=c("Weak phylogenetic signal","Strong phylogenetic signal", "Strong phylognetic signal"),cex.lab=14, las=2)

##Trait size
x=c(1,2,3)
avg=c(0.01918798,-0.05514538,-0.1422591)
lower=c(-0.03142945,-0.105771267,-0.19293938)
upper=c( 0.06980541,-0.004,-0.09157888)
plot(1, type="n", ylab="", yaxt="n", xlab="", xlim=c(-.4, 0.1), ylim=c(0, 4),main="Trait size")
points(avg,x, pch=16,cex=1.5)
arrows(lower, x, upper,  x, length=0.05, angle=90, code=3,lwd=2.5)
abline(v=0,lty=2, lwd=3,col="red")
axis(side=2,at=x,label=c("Weak phylogenetic signal","Strong phylogenetic signal", "Strong phylognetic signal"),cex.lab=14, las=2)

##Functional distinctvness
x=c(1,2,3)
avg=c(0.04511751,-0.1093366,-0.3091695)
lower=c(-0.00550519,-0.15999068,-0.3600872)
upper=c(0.095740214,-0.05868254,-0.2582517)
plot(1, type="n", ylab="", yaxt="n", xlab="Effect size (95%IC)", xlim=c(-.4, 0.1), ylim=c(0, 4), main="Functional distinctiveness ")
points(avg,x, pch=16,cex=1.5)
arrows(lower, x, upper,  x, length=0.05, angle=90, code=3,lwd=2.5)
abline(v=0,lty=2, lwd=3,col="red")
axis(side=2,at=x,label=c("Weak phylogenetic signal","Strong phylogenetic signal", "Strong phylognetic signal"),cex.lab=14, las=2)

####
