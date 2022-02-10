source("script_figura1.r")


M<-read.table("matriz.txt")
M<-as.matrix(M)

# pdf("testefig.pdf", paper="a4")
pdf("testefig.pdf", height = 12, width = 8)

par(mar=c(0,0,0,0),oma=c(2,2,8,1))
layout(M, widths=c(0.5,0.2,1,0.2,0.5,0.2,1),heights=c(0.5,
													  0.4,
													  1,
													  0.15,
													  0.5,
													  0.4,
													  1,
													  0.15,
													  0.5,
													  0.4,
													  1))

DADOS<-geradados(10,10,power_H=0.001,power_L=0.001)
dis_trait_H <- vegdist(DADOS$trait_H[,1], method = "euclidean")
clu_H <- hclust(dis_trait_H, method = "ward.D")
distinc <- evol.distinct(as.phylo(clu_H), type = "fair.proportion")
distinc.order <- distinc[order(distinc[, 2], decreasing = TRUE),]
extinct.col <- match(as.character(distinc.order[, 1]), colnames(DADOS$web))
trait.order <-DADOS$trait_H[order(DADOS$trait_H[, 1, drop = FALSE], decreasing = TRUE), , drop = FALSE]
trait.col <- match(row.names(trait.order), colnames(DADOS$web))


plot(DADOS$tree_H,show.tip.label=F, direction="downwards")
plot(rep(DADOS$trait_H[,1], 3),rep(1:3,each = length(DADOS$trait_H[,1])),xlab = "", ylab = "", xaxt = "n", yaxt = "n", type = "n", ylim = c(0.9,3.2), xlim = c(1, length(DADOS$trait_H[,1])),bty="n")
points(1:length(DADOS$trait_H[,1]),rep(3,length(DADOS$trait_H[,1])),cex=(DADOS$trait_H[,1]+1)*1.4,pch=19)
text(1:length(DADOS$trait_H[,1]),rep(2,length(DADOS$trait_H[,1])), 
	 order(trait.col))
text(1:length(DADOS$trait_H[,1]),rep(1.5,length(DADOS$trait_H[,1])), 
	 order(extinct.col))
plot(DADOS$tree_L,show.tip.label=F)
plot(DADOS$trait_L[,1],rep(1,length(DADOS$trait_L[,1])),xlab = "", ylab = "", xaxt = "n", yaxt = "n", type = "n", xlim = c(0.9,1.1), ylim = c(1, length(DADOS$trait_L[,1])),bty="n")
points(rep(1,length(DADOS$trait_L[,1])),1:length(DADOS$trait_L[,1]),cex=(DADOS$trait_L[,1]+1)*1.4,pch=19)
color2D.matplot(DADOS$web,ylab="",xlab="", yrev=F,cs1=c(1,0),cs2=c(1,0),cs3=c(1,0),border="white",xaxt = "n", yaxt = "n",axes=F)

DADOS<-geradados(10,10,power_H=0.001,power_L=1)
dis_trait_H <- vegdist(DADOS$trait_H[,1], method = "euclidean")
clu_H <- hclust(dis_trait_H, method = "ward.D")
distinc <- evol.distinct(as.phylo(clu_H), type = "fair.proportion")
distinc.order <- distinc[order(distinc[, 2], decreasing = TRUE),]
extinct.col <- match(as.character(distinc.order[, 1]), colnames(DADOS$web))
trait.order <-DADOS$trait_H[order(DADOS$trait_H[, 1, drop = FALSE], decreasing = TRUE), , drop = FALSE]
trait.col <- match(row.names(trait.order), colnames(DADOS$web))


plot(DADOS$tree_H,show.tip.label=F, direction="downwards")
plot(rep(DADOS$trait_H[,1], 3),rep(1:3,each = length(DADOS$trait_H[,1])),xlab = "", ylab = "", xaxt = "n", yaxt = "n", type = "n", ylim = c(0.9,3.2), xlim = c(1, length(DADOS$trait_H[,1])),bty="n")
points(1:length(DADOS$trait_H[,1]),rep(3,length(DADOS$trait_H[,1])),cex=(DADOS$trait_H[,1]+1)*1.4,pch=19)
text(1:length(DADOS$trait_H[,1]),rep(2,length(DADOS$trait_H[,1])), 
	 order(trait.col))
text(1:length(DADOS$trait_H[,1]),rep(1.5,length(DADOS$trait_H[,1])), 
	 order(extinct.col))
plot(DADOS$tree_L,show.tip.label=F)
plot(DADOS$trait_L[,1],rep(1,length(DADOS$trait_L[,1])),xlab = "", ylab = "", xaxt = "n", yaxt = "n", type = "n", xlim = c(0.9,1.1), ylim = c(1, length(DADOS$trait_L[,1])),bty="n")
points(rep(1,length(DADOS$trait_L[,1])),1:length(DADOS$trait_L[,1]),cex=(DADOS$trait_L[,1]+1)*1.4,pch=19)
color2D.matplot(DADOS$web,ylab="",xlab="", yrev=F,cs1=c(1,0),cs2=c(1,0),cs3=c(1,0),border="white",xaxt = "n", yaxt = "n",axes=F)

DADOS<-geradados(10,10,power_H=0.001,power_L=5)
dis_trait_H <- vegdist(DADOS$trait_H[,1], method = "euclidean")
clu_H <- hclust(dis_trait_H, method = "ward.D")
distinc <- evol.distinct(as.phylo(clu_H), type = "fair.proportion")
distinc.order <- distinc[order(distinc[, 2], decreasing = TRUE),]
extinct.col <- match(as.character(distinc.order[, 1]), colnames(DADOS$web))
trait.order <-DADOS$trait_H[order(DADOS$trait_H[, 1, drop = FALSE], decreasing = TRUE), , drop = FALSE]
trait.col <- match(row.names(trait.order), colnames(DADOS$web))


plot(DADOS$tree_H,show.tip.label=F, direction="downwards")
plot(rep(DADOS$trait_H[,1], 3),rep(1:3,each = length(DADOS$trait_H[,1])),xlab = "", ylab = "", xaxt = "n", yaxt = "n", type = "n", ylim = c(0.9,3.2), xlim = c(1, length(DADOS$trait_H[,1])),bty="n")
points(1:length(DADOS$trait_H[,1]),rep(3,length(DADOS$trait_H[,1])),cex=(DADOS$trait_H[,1]+1)*1.4,pch=19)
text(1:length(DADOS$trait_H[,1]),rep(2,length(DADOS$trait_H[,1])), 
	 order(trait.col))
text(1:length(DADOS$trait_H[,1]),rep(1.5,length(DADOS$trait_H[,1])), 
	 order(extinct.col))
plot(DADOS$tree_L,show.tip.label=F)
plot(DADOS$trait_L[,1],rep(1,length(DADOS$trait_L[,1])),xlab = "", ylab = "", xaxt = "n", yaxt = "n", type = "n", xlim = c(0.9,1.1), ylim = c(1, length(DADOS$trait_L[,1])),bty="n")
points(rep(1,length(DADOS$trait_L[,1])),1:length(DADOS$trait_L[,1]),cex=(DADOS$trait_L[,1]+1)*1.4,pch=19)
color2D.matplot(DADOS$web,ylab="",xlab="", yrev=F,cs1=c(1,0),cs2=c(1,0),cs3=c(1,0),border="white",xaxt = "n", yaxt = "n",axes=F)


DADOS<-geradados(10,10,power_H=5,power_L=0.001)
dis_trait_H <- vegdist(DADOS$trait_H[,1], method = "euclidean")
clu_H <- hclust(dis_trait_H, method = "ward.D")
distinc <- evol.distinct(as.phylo(clu_H), type = "fair.proportion")
distinc.order <- distinc[order(distinc[, 2], decreasing = TRUE),]
extinct.col <- match(as.character(distinc.order[, 1]), colnames(DADOS$web))
trait.order <-DADOS$trait_H[order(DADOS$trait_H[, 1, drop = FALSE], decreasing = TRUE), , drop = FALSE]
trait.col <- match(row.names(trait.order), colnames(DADOS$web))


plot(DADOS$tree_H,show.tip.label=F, direction="downwards")
plot(rep(DADOS$trait_H[,1], 3),rep(1:3,each = length(DADOS$trait_H[,1])),xlab = "", ylab = "", xaxt = "n", yaxt = "n", type = "n", ylim = c(0.9,3.2), xlim = c(1, length(DADOS$trait_H[,1])),bty="n")
points(1:length(DADOS$trait_H[,1]),rep(3,length(DADOS$trait_H[,1])),cex=(DADOS$trait_H[,1]+1)*1.4,pch=19)
text(1:length(DADOS$trait_H[,1]),rep(2,length(DADOS$trait_H[,1])), 
	 order(trait.col))
text(1:length(DADOS$trait_H[,1]),rep(1.5,length(DADOS$trait_H[,1])), 
	 order(extinct.col))
plot(DADOS$tree_L,show.tip.label=F)
plot(DADOS$trait_L[,1],rep(1,length(DADOS$trait_L[,1])),xlab = "", ylab = "", xaxt = "n", yaxt = "n", type = "n", xlim = c(0.9,1.1), ylim = c(1, length(DADOS$trait_L[,1])),bty="n")
points(rep(1,length(DADOS$trait_L[,1])),1:length(DADOS$trait_L[,1]),cex=(DADOS$trait_L[,1]+1)*1.4,pch=19)
color2D.matplot(DADOS$web,ylab="",xlab="", yrev=F,cs1=c(1,0),cs2=c(1,0),cs3=c(1,0),border="white",xaxt = "n", yaxt = "n",axes=F)


DADOS<-geradados(10,10,power_H=5,power_L=1)
dis_trait_H <- vegdist(DADOS$trait_H[,1], method = "euclidean")
clu_H <- hclust(dis_trait_H, method = "ward.D")
distinc <- evol.distinct(as.phylo(clu_H), type = "fair.proportion")
distinc.order <- distinc[order(distinc[, 2], decreasing = TRUE),]
extinct.col <- match(as.character(distinc.order[, 1]), colnames(DADOS$web))
trait.order <-DADOS$trait_H[order(DADOS$trait_H[, 1, drop = FALSE], decreasing = TRUE), , drop = FALSE]
trait.col <- match(row.names(trait.order), colnames(DADOS$web))


plot(DADOS$tree_H,show.tip.label=F, direction="downwards")
plot(rep(DADOS$trait_H[,1], 3),rep(1:3,each = length(DADOS$trait_H[,1])),xlab = "", ylab = "", xaxt = "n", yaxt = "n", type = "n", ylim = c(0.9,3.2), xlim = c(1, length(DADOS$trait_H[,1])),bty="n")
points(1:length(DADOS$trait_H[,1]),rep(3,length(DADOS$trait_H[,1])),cex=(DADOS$trait_H[,1]+1)*1.4,pch=19)
text(1:length(DADOS$trait_H[,1]),rep(2,length(DADOS$trait_H[,1])), 
	 order(trait.col))
text(1:length(DADOS$trait_H[,1]),rep(1.5,length(DADOS$trait_H[,1])), 
	 order(extinct.col))
plot(DADOS$tree_L,show.tip.label=F)
plot(DADOS$trait_L[,1],rep(1,length(DADOS$trait_L[,1])),xlab = "", ylab = "", xaxt = "n", yaxt = "n", type = "n", xlim = c(0.9,1.1), ylim = c(1, length(DADOS$trait_L[,1])),bty="n")
points(rep(1,length(DADOS$trait_L[,1])),1:length(DADOS$trait_L[,1]),cex=(DADOS$trait_L[,1]+1)*1.4,pch=19)
color2D.matplot(DADOS$web,ylab="",xlab="", yrev=F,cs1=c(1,0),cs2=c(1,0),cs3=c(1,0),border="white",xaxt = "n", yaxt = "n",axes=F)


DADOS<-geradados(10,10,power_H=5,power_L=5)
dis_trait_H <- vegdist(DADOS$trait_H[,1], method = "euclidean")
clu_H <- hclust(dis_trait_H, method = "ward.D")
distinc <- evol.distinct(as.phylo(clu_H), type = "fair.proportion")
distinc.order <- distinc[order(distinc[, 2], decreasing = TRUE),]
extinct.col <- match(as.character(distinc.order[, 1]), colnames(DADOS$web))
trait.order <-DADOS$trait_H[order(DADOS$trait_H[, 1, drop = FALSE], decreasing = TRUE), , drop = FALSE]
trait.col <- match(row.names(trait.order), colnames(DADOS$web))


plot(DADOS$tree_H,show.tip.label=F, direction="downwards")
plot(rep(DADOS$trait_H[,1], 3),rep(1:3,each = length(DADOS$trait_H[,1])),xlab = "", ylab = "", xaxt = "n", yaxt = "n", type = "n", ylim = c(0.9,3.2), xlim = c(1, length(DADOS$trait_H[,1])),bty="n")
points(1:length(DADOS$trait_H[,1]),rep(3,length(DADOS$trait_H[,1])),cex=(DADOS$trait_H[,1]+1)*1.4,pch=19)
text(1:length(DADOS$trait_H[,1]),rep(2,length(DADOS$trait_H[,1])), 
	 order(trait.col))
text(1:length(DADOS$trait_H[,1]),rep(1.5,length(DADOS$trait_H[,1])), 
	 order(extinct.col))
plot(DADOS$tree_L,show.tip.label=F)
plot(DADOS$trait_L[,1],rep(1,length(DADOS$trait_L[,1])),xlab = "", ylab = "", xaxt = "n", yaxt = "n", type = "n", xlim = c(0.9,1.1), ylim = c(1, length(DADOS$trait_L[,1])),bty="n")
points(rep(1,length(DADOS$trait_L[,1])),1:length(DADOS$trait_L[,1]),cex=(DADOS$trait_L[,1]+1)*1.4,pch=19)
color2D.matplot(DADOS$web,ylab="",xlab="", yrev=F,cs1=c(1,0),cs2=c(1,0),cs3=c(1,0),border="white",xaxt = "n", yaxt = "n",axes=F)

mtext("Higher trophic level",outer = T,  font = 2, line = 5.2)
mtext("Lower trophic level", side = 2, outer=T, font = 2, line = 0.2)

mtext("Late diversification, weak", side=3,outer=T, line=1.6, adj = 0.26, cex = 0.9)
mtext(expression("phylogenetic signal ("~rho==1e-04~")"), side=3, outer=T, line=0, adj = 0.26, cex = 0.9)
mtext("Early diversification, strong", side=3,outer=T, line=1.6, adj = 1, cex = 0.9)
mtext(expression("phylogenetic signal ("~rho==5~")"), side=3,outer=T, line=0, adj = 1, cex = 0.9)

mtext("Extintion order", side=3,outer=T,line=-8, font = 2, adj = 0, cex = 0.9)
mtext("Trait size", side=3,outer=T,line=-10, adj = 0, cex = 0.8)
mtext("Trait distinctiveness", side=3,outer=T,line=-11.2, adj = 0, cex = 0.8)

mtext(expression(rho==1e-04), side = 3,outer=T, line=-28, adj = 0, cex = 0.8)
mtext(expression(rho==1e-04), side = 3,outer=T, line=-28, adj = 0.58, cex = 0.8)
mtext(expression(rho==1~"(Brownian motion)"), side=3,outer=T,line=-55, adj = 0, cex = 0.8)
mtext(expression(rho==1~"(Brownian motion)"), side=3,outer=T,line=-55, adj = 0.65, cex = 0.8)
mtext(expression(rho==5), side=3,outer=T,line=-82, adj = 0, cex = 0.8)
mtext(expression(rho==5), side=3,outer=T,line=-82, adj = 0.54, cex = 0.8)


dev.off()

# # 
# DADOS<-geradados(10, 10, power_H=1,power_L=0.001)
# dis_trait_H <- vegdist(DADOS$trait_H[,1], method = "euclidean")
# clu_H <- hclust(dis_trait_H, method = "ward.D")
# distinc <- evol.distinct(as.phylo(clu_H), type = "fair.proportion")
# distinc.order <- distinc[order(distinc[, 2], decreasing = TRUE),]
# extinct.col <- match(as.character(distinc.order[, 1]), colnames(DADOS$web))
# trait.order <-DADOS$trait_H[order(DADOS$trait_H[, 1, drop = FALSE], decreasing = TRUE), , drop = FALSE]
# trait.col <- match(row.names(trait.order), colnames(DADOS$web))
# 
# M <- matrix(1:2)
# layout(M, heights=c(0.5,0.4))
# 
# plot(DADOS$tree_H,show.tip.label=F, direction="downwards")
# plot(rep(DADOS$trait_H[,1], 3),rep(1:3,each = length(DADOS$trait_H[,1])),xlab = "", ylab = "", xaxt = "n", yaxt = "n", type = "n", ylim = c(0.9,3.1), xlim = c(1, length(DADOS$trait_H[,1])),bty="n")
# points(1:length(DADOS$trait_H[,1]),rep(3,length(DADOS$trait_H[,1])),cex=DADOS$trait_H[,1]+1,pch=19)
# text(1:length(DADOS$trait_H[,1]),rep(2,length(DADOS$trait_H[,1])), 
# 	 order(trait.col))
# text(1:length(DADOS$trait_H[,1]),rep(1.5,length(DADOS$trait_H[,1])), 
# 	 order(extinct.col))
# 
