library(vegan)
library(ape)
library(ggplot2)
library(grid)
library(RColorBrewer)

data <- read.csv("family.txt", head=TRUE,sep="\t",row.names = 1)
groups <- read.table("group.txt",sep = "\t",header = F,colClasses = c("character"))
groups <- as.list(groups)
data <- t(data)
data[is.na(data)] <- 0
data <- vegdist(data)

length=length(unique(as.character(groups$V1)))
times1=length%/%8
res1=length%%8
times2=length%/%5
res2=length%%5
col1=rep(1:8,times1)
col=c(col1,1:res1)
pich1=rep(c(21:25),times2)
pich=c(21:24)

cbbPalette <- brewer.pal(9,"Set1")
pcoa<- pcoa(data, correction = "none", rn = NULL)
PC1 = pcoa$vectors[,1]
PC2 = pcoa$vectors[,2]
plotdata <- data.frame(rownames(pcoa$vectors),PC1,PC2,groups$V2,groups$V3)
colnames(plotdata) <-c("sample","PC1","PC2","Time","Treatment")
pc1 <-floor(pcoa$values$Relative_eig[1]*100)
pc2 <-floor(pcoa$values$Relative_eig[2]*100)
plotdata$Time <- factor(plotdata$Time,levels = c("0 days","3 days","6 days","9 days","12 days","15 days","18 days","21 days","24 days"))
plotdata$Treatment <- factor(plotdata$Treatment,levels = c("Rescue","Diarrhea","After recovery","Release"))

p2<-ggplot(plotdata, aes(PC1, PC2)) +
geom_point(aes(shape=Treatment,fill=Time),size=10)+
scale_shape_manual(values=pich)+
scale_fill_manual(values=cbbPalette)+
labs(title="PCoA - The composition of gut microbiome") + 
  xlab(paste("PC1 ( ",pc1,"%"," )",sep="")) + 
  ylab(paste("PC2 ( ",pc2,"%"," )",sep=""))+
theme(text=element_text(size=30))+
geom_vline(aes(xintercept = 0),linetype="dotted")+
geom_hline(aes(yintercept = 0),linetype="dotted")+
theme(panel.background = element_rect(fill='white', colour='black'),
      panel.grid=element_blank(), 
      axis.title = element_text(color='black',size=34),
      axis.ticks.length = unit(0.4,"lines"), axis.ticks = element_line(color='black'),
      axis.line = element_line(colour = "black"), 
      axis.title.x=element_text(colour='black', size=34),
      axis.title.y=element_text(colour='black', size=34),
      axis.text=element_text(colour='black',size=28),
      legend.title=element_blank(),
      legend.text=element_text(size=28),
      legend.key=element_blank(),legend.position = "right",
      legend.background = element_rect(colour = "black"),
      legend.key.height=unit(1.2,"cm"))+
theme(plot.title = element_text(size=34,colour = "black",hjust = 0.5,face = "bold"))

cairo_pdf("PCoA12.pdf",height=12,width=15)
p2
tiff(filename="PCoA12.tif",res=600,height=5400,width=7200,type="windows",compression = "lzw")
p2
dev.off()

pich1 <- c(21,21,22,22,22,23,23,23,24)
p5<-ggplot(plotdata, aes(PC1, PC2)) +
  geom_point(aes(shape=Time,fill=Time),size=10)+
  scale_shape_manual(values=pich1)+
  scale_fill_manual(values=cbbPalette) +
  labs(title="PCoA - The composition of gut microbiome") + 
  xlab(paste("PC1 ( ",pc1,"%"," )",sep="")) + 
  ylab(paste("PC2 ( ",pc2,"%"," )",sep=""))+
  theme(text=element_text(size=30))+
  geom_vline(aes(xintercept = 0),linetype="dotted")+
  geom_hline(aes(yintercept = 0),linetype="dotted")+
  theme(panel.background = element_rect(fill='white', colour='black'),
        panel.grid=element_blank(), 
        axis.title = element_text(color='black',size=34),
        axis.ticks.length = unit(0.4,"lines"), axis.ticks = element_line(color='black'),
        axis.line = element_line(colour = "black"), 
        axis.title.x=element_text(colour='black', size=34),
        axis.title.y=element_text(colour='black', size=34),
        axis.text=element_text(colour='black',size=30),
        legend.title=element_blank(),
        legend.text=element_text(size=28),
        legend.key=element_blank(),legend.position = "right",
        legend.background = element_rect(colour = "black"),
        legend.key.height=unit(1.2,"cm"))+
  theme(plot.title = element_text(size=34,colour = "black",hjust = 0.5,face = "bold"))
cairo_pdf("PCoA12-2.pdf",height=12,width=15)
p5
tiff(filename="PCoA12-2.tif",res=600,height=5400,width=7200,type="windows",compression = "lzw")
p5
dev.off()