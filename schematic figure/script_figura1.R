require(geiger)
require(plotrix)
require(picante)
require(bipartite)

#rm(list=ls())

power_H<-0.0001
power_L<-0.0001
n_spe_H<-10
n_spe_L<-10

	
geradados<-function(n_spe_H = 10, n_spe_L = 10, power_H, power_L)	{
		tree_H<-sim.bdtree(b=0.1,d=0,stop="taxa",n=n_spe_H, extinct=FALSE) 
		for (y in 1: length(tree_H$edge.length)) {
			if (tree_H$edge.length[y] == 0.00000000) 
				tree_H$edge.length[y] = 0.01
		}
		tree_L<-sim.bdtree(b=0.1,d=0,stop="taxa",n=n_spe_L, extinct=FALSE) 
		for (y in 1: length(tree_L$edge.length)) {
			if (tree_L$edge.length[y] == 0.00000000) 
				tree_L$edge.length[y] = 0.01
		}
		tree_H$tip.label=sprintf("H_%.3d",1:length(tree_H$tip.label))	
		tree_L$tip.label=sprintf("L_%.3d",1:length(tree_L$tip.label))	
		trait_H<-matrix(NA,n_spe_H,1)
		for(n in 1:1){
			trait_H[,n]<-rTraitCont(compute.brlen(tree_H,power= power_H),model="BM")
		}
		trait_H[,1]<-rescale(trait_H[,1],c(0,1))
		rownames(trait_H)<-tree_H$tip.label
		trait_L<-matrix(NA,n_spe_L,1)
		for(n in 1:1){
			trait_L[,n]<-rTraitCont(compute.brlen(tree_L,power=power_L),model="BM")
		}
		trait_L[,1]<-rescale(trait_L[,1],c(0,1))
		rownames(trait_L)<-tree_L$tip.label
		d_H<-matrix(runif(n_spe_H,max=0.25), n_spe_H,1)
		d_L<-matrix(runif(n_spe_L,max=0.25), n_spe_L,1)
		web<-matrix(NA,n_spe_L,n_spe_H)
		for(i in 1: n_spe_L){
			for(j in 1: n_spe_H){
				II<-abs(trait_H[j]-trait_L[i])
				III<-0.5*(d_H[j]+d_L[i])
				web[i,j]<-ifelse(II<III,1,0)
			}
		}
		colnames(web)<-tree_H$tip.label
		rownames(web)<-tree_L$tip.label
		z_H<-which(colSums(web)==0)
		if(length(z_H)>0){
			for(i in 1:length(z_H)){
				web[sample(1:n_spe_L,1),z_H[i]]<-1
			}
		}
		z_L<-which(rowSums(web)==0)
		if(length(z_L)>0){
			for(i in 1:length(z_L)){
				web[z_L[i],sample(1:n_spe_H,1)]<-1
			}
		}
	RES<-list(tree_H=tree_H, tree_L=tree_L,trait_H = trait_H, trait_L= trait_L, web= web)
return(RES)
}

DADOS<-geradados(10,10,power_H=0.001,power_L=0.001)

par(mar=c(0.4,0.4,0.4,0.4))
layout(matrix(c(0,0,1,0,0,2,3,4,5),3,3,byrow=T), widths=c(0.4,0.4,1),heights=c(0.4,0.4,1))
plot(DADOS$tree_H,show.tip.label=F, direction="downwards")
plot(DADOS$trait_H[,1],rep(1,length(DADOS$trait_H[,1])),xlab = "", ylab = "", xaxt = "n", yaxt = "n", type = "n", ylim = c(0.9,1.1), xlim = c(1, length(DADOS$trait_H[,1])),bty="n")
points(1:length(DADOS$trait_H[,1]),rep(1,length(DADOS$trait_H[,1])),cex=DADOS$trait_H[,1]+1,pch=19)
plot(DADOS$tree_L,show.tip.label=F)
plot(DADOS$trait_L[,1],rep(1,length(DADOS$trait_L[,1])),xlab = "", ylab = "", xaxt = "n", yaxt = "n", type = "n", xlim = c(0.9,1.1), ylim = c(1, length(DADOS$trait_L[,1])),bty="n")
points(rep(1,length(DADOS$trait_L[,1])),1:length(DADOS$trait_L[,1]),cex=DADOS$trait_L[,1]+1,pch=19)
color2D.matplot(DADOS$web,ylab="",xlab="", yrev=F,cs1=c(1,0),cs2=c(1,0),cs3=c(1,0),border="white",xaxt = "n", yaxt = "n",axes=F)

DADOS<-geradados(10,10,power_H=0.001,power_L=1)

par(mar=c(0.4,0.4,0.4,0.4))
layout(matrix(c(0,0,1,0,0,2,3,4,5),3,3,byrow=T), widths=c(0.4,0.4,1),heights=c(0.4,0.4,1))
plot(DADOS$tree_H,show.tip.label=F, direction="downwards")
plot(DADOS$trait_H[,1],rep(1,length(DADOS$trait_H[,1])),xlab = "", ylab = "", xaxt = "n", yaxt = "n", type = "n", ylim = c(0.9,1.1), xlim = c(1, length(DADOS$trait_H[,1])),bty="n")
points(1:length(DADOS$trait_H[,1]),rep(1,length(DADOS$trait_H[,1])),cex=DADOS$trait_H[,1]+1,pch=19)
plot(DADOS$tree_L,show.tip.label=F)
plot(DADOS$trait_L[,1],rep(1,length(DADOS$trait_L[,1])),xlab = "", ylab = "", xaxt = "n", yaxt = "n", type = "n", xlim = c(0.9,1.1), ylim = c(1, length(DADOS$trait_L[,1])),bty="n")
points(rep(1,length(DADOS$trait_L[,1])),1:length(DADOS$trait_L[,1]),cex=DADOS$trait_L[,1]+1,pch=19)
color2D.matplot(DADOS$web,ylab="",xlab="", yrev=F,cs1=c(1,0),cs2=c(1,0),cs3=c(1,0),border="white",xaxt = "n", yaxt = "n",axes=F)

DADOS<-geradados(10,10,power_H=0.001,power_L=5)

par(mar=c(0.4,0.4,0.4,0.4))
layout(matrix(c(0,0,1,0,0,2,3,4,5),3,3,byrow=T), widths=c(0.4,0.4,1),heights=c(0.4,0.4,1))
plot(DADOS$tree_H,show.tip.label=F, direction="downwards")
plot(DADOS$trait_H[,1],rep(1,length(DADOS$trait_H[,1])),xlab = "", ylab = "", xaxt = "n", yaxt = "n", type = "n", ylim = c(0.9,1.1), xlim = c(1, length(DADOS$trait_H[,1])),bty="n")
points(1:length(DADOS$trait_H[,1]),rep(1,length(DADOS$trait_H[,1])),cex=DADOS$trait_H[,1]+1,pch=19)
plot(DADOS$tree_L,show.tip.label=F)
plot(DADOS$trait_L[,1],rep(1,length(DADOS$trait_L[,1])),xlab = "", ylab = "", xaxt = "n", yaxt = "n", type = "n", xlim = c(0.9,1.1), ylim = c(1, length(DADOS$trait_L[,1])),bty="n")
points(rep(1,length(DADOS$trait_L[,1])),1:length(DADOS$trait_L[,1]),cex=DADOS$trait_L[,1]+1,pch=19)
color2D.matplot(DADOS$web,ylab="",xlab="", yrev=F,cs1=c(1,0),cs2=c(1,0),cs3=c(1,0),border="white",xaxt = "n", yaxt = "n",axes=F)


DADOS<-geradados(10,10,power_H=5,power_L=0.001)

par(mar=c(0.4,0.4,0.4,0.4))
layout(matrix(c(0,0,1,0,0,2,3,4,5),3,3,byrow=T), widths=c(0.4,0.4,1),heights=c(0.4,0.4,1))
plot(DADOS$tree_H,show.tip.label=F, direction="downwards")
plot(DADOS$trait_H[,1],rep(1,length(DADOS$trait_H[,1])),xlab = "", ylab = "", xaxt = "n", yaxt = "n", type = "n", ylim = c(0.9,1.1), xlim = c(1, length(DADOS$trait_H[,1])),bty="n")
points(1:length(DADOS$trait_H[,1]),rep(1,length(DADOS$trait_H[,1])),cex=DADOS$trait_H[,1]+1,pch=19)
plot(DADOS$tree_L,show.tip.label=F)
plot(DADOS$trait_L[,1],rep(1,length(DADOS$trait_L[,1])),xlab = "", ylab = "", xaxt = "n", yaxt = "n", type = "n", xlim = c(0.9,1.1), ylim = c(1, length(DADOS$trait_L[,1])),bty="n")
points(rep(1,length(DADOS$trait_L[,1])),1:length(DADOS$trait_L[,1]),cex=DADOS$trait_L[,1]+1,pch=19)
color2D.matplot(DADOS$web,ylab="",xlab="", yrev=F,cs1=c(1,0),cs2=c(1,0),cs3=c(1,0),border="white",xaxt = "n", yaxt = "n",axes=F)


DADOS<-geradados(10,10,power_H=5,power_L=1)

par(mar=c(0.4,0.4,0.4,0.4))
layout(matrix(c(0,0,1,0,0,2,3,4,5),3,3,byrow=T), widths=c(0.4,0.4,1),heights=c(0.4,0.4,1))
plot(DADOS$tree_H,show.tip.label=F, direction="downwards")
plot(DADOS$trait_H[,1],rep(1,length(DADOS$trait_H[,1])),xlab = "", ylab = "", xaxt = "n", yaxt = "n", type = "n", ylim = c(0.9,1.1), xlim = c(1, length(DADOS$trait_H[,1])),bty="n")
points(1:length(DADOS$trait_H[,1]),rep(1,length(DADOS$trait_H[,1])),cex=DADOS$trait_H[,1]+1,pch=19)
plot(DADOS$tree_L,show.tip.label=F)
plot(DADOS$trait_L[,1],rep(1,length(DADOS$trait_L[,1])),xlab = "", ylab = "", xaxt = "n", yaxt = "n", type = "n", xlim = c(0.9,1.1), ylim = c(1, length(DADOS$trait_L[,1])),bty="n")
points(rep(1,length(DADOS$trait_L[,1])),1:length(DADOS$trait_L[,1]),cex=DADOS$trait_L[,1]+1,pch=19)
color2D.matplot(DADOS$web,ylab="",xlab="", yrev=F,cs1=c(1,0),cs2=c(1,0),cs3=c(1,0),border="white",xaxt = "n", yaxt = "n",axes=F)


DADOS<-geradados(10,10,power_H=5,power_L=5)

par(mar=c(0.4,0.4,0.4,0.4))
layout(matrix(c(0,0,1,0,0,2,3,4,5),3,3,byrow=T), widths=c(0.4,0.4,1),heights=c(0.4,0.4,1))
plot(DADOS$tree_H,show.tip.label=F, direction="downwards")
plot(DADOS$trait_H[,1],rep(1,length(DADOS$trait_H[,1])),xlab = "", ylab = "", xaxt = "n", yaxt = "n", type = "n", ylim = c(0.9,1.1), xlim = c(1, length(DADOS$trait_H[,1])),bty="n")
points(1:length(DADOS$trait_H[,1]),rep(1,length(DADOS$trait_H[,1])),cex=DADOS$trait_H[,1]+1,pch=19)
plot(DADOS$tree_L,show.tip.label=F)
plot(DADOS$trait_L[,1],rep(1,length(DADOS$trait_L[,1])),xlab = "", ylab = "", xaxt = "n", yaxt = "n", type = "n", xlim = c(0.9,1.1), ylim = c(1, length(DADOS$trait_L[,1])),bty="n")
points(rep(1,length(DADOS$trait_L[,1])),1:length(DADOS$trait_L[,1]),cex=DADOS$trait_L[,1]+1,pch=19)
color2D.matplot(DADOS$web,ylab="",xlab="", yrev=F,cs1=c(1,0),cs2=c(1,0),cs3=c(1,0),border="white",xaxt = "n", yaxt = "n",axes=F)


