library(RColorBrewer)
Water <- read.table("index.txt",header = T,sep = "\t",row.names = 1)
Water$Group <- factor(Water$Group,levels = c("0 days","3 days","6 days","9 days","12 days","15 days","18 days","21 days","24 days"))

fit <- aov(Chao1~Group,data = Water)
Tukey <- TukeyHSD(fit)
write.csv(Tukey$Group,file = "Tukey.test.chao1.csv")

library(multcomp)
tiff(filename = "chao1.tif",width = 5400,height = 5400,res = 700,type = "cairo",compression = "lzw")
par(mar=c(4,6,8,2))  
tuk<-glht(fit,linfct=mcp(Group="Tukey"))
p1 <- plot(cld(tuk,level=0.05),col=brewer.pal(9,"Set1"),xlab = "",ylab = "",xaxt = "n",cex = 4)
par(new = T)
par(xpd=TRUE)
p1
mtext("Chao1 index",line = 3,side = 2,font = 2,cex = 2)
text(x = p1,y=550,label = levels(Water$Group),font = 2,srt = 45,adj = c(1,1),cex = 1.2)
dev.off()

fit <- aov(Shannon~Group,data = Water)
Tukey <- TukeyHSD(fit)
write.csv(Tukey$Group,file = "Tukey.test.shannon.csv")

tiff(filename = "shannon.tif",width = 5400,height = 5400,res = 700,type = "cairo",compression = "lzw")
par(mar=c(4,6,8,2))  
tuk<-glht(fit,linfct=mcp(Group="Tukey"))
p1 <- plot(cld(tuk,level=0.05),col="lightgrey",xlab = "",ylab = "",xaxt = "n",cex = 4)
par(new = T)
par(xpd=TRUE)
p1
mtext("Shannon index",line = 3,side = 2,font = 2,cex = 2)
text(x = p1,y = 5.2,label = levels(Water$Group),font = 2)
dev.off()
