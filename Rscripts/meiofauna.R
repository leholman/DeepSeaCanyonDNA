library(ggplot2)
library(vegan)


#Minimum adequate models - all sediment layer depths but no organic matter - significant relationship between diversity scores and trawling impact
meiofauna_diversity<-read.table("meiofauna_diversity.txt", header=T)
meiofauna_diversity
attach(meiofauna_diversity)
names(meiofauna_diversity)
hist(log(Shannon))

logshannon<-log(Shannon)

Shannon_model<-lm(Shannon~Human_Impact*Layer)
Shannon_model
summary(Shannon_model)
Shannon_model2<-update(Shannon_model,~.-Human_Impact:Layer)
summary(Shannon_model2)
anova(Shannon_model,Shannon_model2)
Shannon_model3<-update(Shannon_model2,~.-Layer)
summary(Shannon_model3)
anova(Shannon_model2,Shannon_model3)
Shannon_model4<-update(Shannon_model2,~.-Human_Impact)
summary(Shannon_model4)
anova(Shannon_model2,Shannon_model4)
anova(Shannon_model,Shannon_model3)

boxplot(Shannon~Human_Impact, col=c("cornsilk3","darkolivegreen3"), xlab="Trawling Impact", ylab="Shannon Diversity Index Score")
boxplot(Shannon~Layer)

boxplot(Shannon~stat)

#data that excludes sediment layers 10-15cm due to lack of organic matter data
shannon_OMdiversity<-read.table("meiofauna_diversity_OM.txt", header=T)
shannon_OMdiversity
attach(shannon_OMdiversity)
names(shannon_OMdiversity)


#Minimum adequate model - significant relationship between lipid concentration and meiofauna diversity
ShannonOM_model1<-lm(Shannon~Human_Impact*Layer*PRT*CHO*LIP)
summary(ShannonOM_model1)
ShannonOM_model2<-update(ShannonOM_model1,.~.-Human_Impact:Layer:PRT:CHO:LIP)
summary(ShannonOM_model2)
anova(ShannonOM_model1,ShannonOM_model2)
ShannonOM_model3<-update(ShannonOM_model2,.~.-Layer:PRT:CHO:LIP)
summary(ShannonOM_model3)
anova(ShannonOM_model2,ShannonOM_model3)
ShannonOM_model4<-update(ShannonOM_model3,.~.-Human_Impact:PRT:CHO:LIP)
summary(ShannonOM_model4)
anova(ShannonOM_model3,ShannonOM_model4)
ShannonOM_model5<-update(ShannonOM_model4,.~.-Human_Impact:Layer:CHO:LIP)
summary(ShannonOM_model5)
anova(ShannonOM_model4,ShannonOM_model5)
ShannonOM_model6<-update(ShannonOM_model5,.~.-Human_Impact:Layer:PRT:LIP)
summary(ShannonOM_model6)
anova(ShannonOM_model5,ShannonOM_model6)
ShannonOM_model7<-update(ShannonOM_model6,.~.-Human_Impact:Layer:PRT:CHO)
summary(ShannonOM_model7)
anova(ShannonOM_model6,ShannonOM_model7)
ShannonOM_model8<-update(ShannonOM_model7,.~.-PRT:CHO:LIP)
summary(ShannonOM_model8)
anova(ShannonOM_model7,ShannonOM_model8)
ShannonOM_model9<-update(ShannonOM_model8,.~.-Layer:CHO:LIP)
summary(ShannonOM_model9)
anova(ShannonOM_model8,ShannonOM_model9)
ShannonOM_model10<-update(ShannonOM_model9,.~.-Human_Impact:CHO:LIP)
summary(ShannonOM_model10)
anova(ShannonOM_model9,ShannonOM_model10)
ShannonOM_model11<-update(ShannonOM_model10,.~.-Layer:PRT:LIP)
summary(ShannonOM_model11)
anova(ShannonOM_model10,ShannonOM_model11)
ShannonOM_model12<-update(ShannonOM_model11,.~.-Human_Impact:PRT:LIP)
summary(ShannonOM_model12)
anova(ShannonOM_model11,ShannonOM_model12)
ShannonOM_model13<-update(ShannonOM_model12,.~.-Human_Impact:Layer:LIP)
summary(ShannonOM_model13)
anova(ShannonOM_model12,ShannonOM_model13)
ShannonOM_model14<-update(ShannonOM_model13,.~.-Layer:PRT:CHO)
summary(ShannonOM_model14)
anova(ShannonOM_model13,ShannonOM_model14)
ShannonOM_model15<-update(ShannonOM_model14,.~.-Human_Impact:PRT:CHO)
summary(ShannonOM_model15)
anova(ShannonOM_model14,ShannonOM_model15)
ShannonOM_model16<-update(ShannonOM_model15,.~.-Human_Impact:Layer:CHO)
summary(ShannonOM_model16)
anova(ShannonOM_model15,ShannonOM_model16)
ShannonOM_model17<-update(ShannonOM_model16,.~.-Human_Impact:Layer:PRT)
summary(ShannonOM_model17)
anova(ShannonOM_model16,ShannonOM_model17)
ShannonOM_model18<-update(ShannonOM_model17,.~.-CHO:LIP)
summary(ShannonOM_model18)
anova(ShannonOM_model17,ShannonOM_model18)
ShannonOM_model19<-update(ShannonOM_model18,.~.-LIP:PRT)
summary(ShannonOM_model19)
anova(ShannonOM_model18,ShannonOM_model19)
ShannonOM_model20<-update(ShannonOM_model19,.~.-Layer:LIP)
summary(ShannonOM_model20)
anova(ShannonOM_model19,ShannonOM_model20)
ShannonOM_model21<-update(ShannonOM_model20,.~.-Human_Impact:LIP)
summary(ShannonOM_model21)
anova(ShannonOM_model20,ShannonOM_model21)
ShannonOM_model22<-update(ShannonOM_model21,.~.-PRT:CHO)
summary(ShannonOM_model22)
anova(ShannonOM_model21,ShannonOM_model22)
ShannonOM_model23<-update(ShannonOM_model22,.~.-Layer:CHO)
summary(ShannonOM_model23)
anova(ShannonOM_model22,ShannonOM_model23)
ShannonOM_model24<-update(ShannonOM_model23,.~.-Human_Impact:CHO)
summary(ShannonOM_model24)
anova(ShannonOM_model23,ShannonOM_model24)
ShannonOM_model25<-update(ShannonOM_model24,.~.-Layer:PRT)
summary(ShannonOM_model25)
anova(ShannonOM_model24,ShannonOM_model25)
ShannonOM_model26<-update(ShannonOM_model25,.~.-Human_Impact:PRT)
summary(ShannonOM_model26)
anova(ShannonOM_model25,ShannonOM_model26)
ShannonOM_model27<-update(ShannonOM_model26,.~.-Human_Impact:Layer)
summary(ShannonOM_model27)
anova(ShannonOM_model26,ShannonOM_model27)
ShannonOM_model28<-update(ShannonOM_model27,.~.-PRT)
summary(ShannonOM_model28)
anova(ShannonOM_model27,ShannonOM_model28)
ShannonOM_model29<-update(ShannonOM_model28,.~.-Layer)
summary(ShannonOM_model29)
anova(ShannonOM_model28,ShannonOM_model29)
ShannonOM_model30<-update(ShannonOM_model29,.~.-CHO)
summary(ShannonOM_model30)
anova(ShannonOM_model29,ShannonOM_model30)
ShannonOM_model31<-update(ShannonOM_model30,.~.-Human_Impact)
summary(ShannonOM_model31)
anova(ShannonOM_model30,ShannonOM_model31)
anova(ShannonOM_model1,ShannonOM_model31)

plot(Shannon~LIP)
text(Shannon~LIP, labels=shannon_OMdiversity$sample_site)


LIP_plot_data<-read.table("meiofauna_diversity_OM_no10_15.txt", header=TRUE)
attach(LIP_plot_data)
names(LIP_plot_data)

plot(Shannon~LIP, ylim=c(0,1.05),xlim=c(0.2,0.7),col=LIP_plot_data$Colour, xlab="Lipid Concentration (mg C/g sed)", ylab="Shannon Diversity Index Score")
abline(ShannonOM_model31, lty=2)
legend(0.21,1, legend=c("Untrawled", "Trawled"), col=c("darkolivegreen3","cornsilk3"), pch=16, cex=1)
arrows(LIP, Shannon-Lower, LIP,Shannon+Upper, length=0.05, angle=90, code=3)
points(Shannon~LIP, ylim=c(0,1.05),col=LIP_plot_data$Colour, pch=16, cex=1.5)



#nMDS plots
MDS_plot<-read.table("meiofaunaMDS.txt", header=T)
OTU_MDS<-MDS_plot[4:8]
OTU_MDS.mds <- metaMDS(comm = OTU_MDS, k=3,distance = "bray", trace = FALSE, autotransform = FALSE)
MDS_xy <- data.frame(OTU_MDS.mds$points)
MDS_xy$Condition <- MDS_plot$Human_Impact
MDS_xy$Layer <- MDS_plot$Layer
MDS_xy$Impact <- MDS_plot$Human_Impact
MDS_xy$Color <- MDS_plot$Color
plot(OTU_MDS.mds$points) 
plot(OTU_MDS.mds$points, col=c("cornsilk3","darkolivegreen3")[MDS_plot$Human_Impact],pch=c(16,17)[as.integer(MDS_plot$Human_Impact)],main="",cex=2.5,xlab="",ylab="")
ordispider(OTU_MDS.mds,MDS_plot$factor,
           lty=1,
           lwd=2,
           col=c("cornsilk3","darkolivegreen3","cornsilk3","darkolivegreen3","cornsilk3","darkolivegreen3","cornsilk3","darkolivegreen3","cornsilk3","darkolivegreen3"))
ordihull(OTU_MDS.mds,MDS_plot$factor,
         draw = "polygon",
         lty=0,
         col=c("cornsilk3","darkolivegreen3","cornsilk3","darkolivegreen3","cornsilk3","darkolivegreen3","cornsilk3","darkolivegreen3","cornsilk3","darkolivegreen3"))
text(OTU_MDS.mds, labels = MDS_plot$Layer, adj = c(0.5,0.5))

OTU_MDS.mds$stress

data.scores <- as.data.frame(scores(OTU_MDS.mds))  #Using the scores function from vegan to extract the site scores and convert to a data.frame
data.scores$site <- rownames(data.scores)  # create a column of site names, from the rownames of data.scores
data.scores$grp <- grp  #  add the grp variable created earlier
head(data.scores)  #look at the data


#Meiofauna abundance 
meio_abund<-read.table("meiofauna_abundance.txt", header=T)
attach(meio_abund)
names(meio_abund)


abund_stats<-lm(log(total_abund)~sample_site)
summary(abund_stats)
anova(abund_stats)
TukeyHSD(aov(log(total_abund)~sample_site))

hist(log(total_abund))

#permanovas
meio_perm<-read.table("meiofaunaMDS_adonis.txt", header=T)
attach(meio_perm)
names(meio_perm)

meio_perm_env<-read.table("meiofaunaMDS_adonis_env.txt", header=T)
attach(meio_perm_env)
names(meio_perm_env)

adonis<-adonis2(meio_perm ~ Human_Impact+Layer, data = meio_perm_env)
adonis


dis <- vegdist(meio_perm)
depth <- factor(c(rep(1,3), rep(2,3),rep(3,3),rep(4,3),rep(5,3),rep(6,3),rep(7,3),rep(8,3),rep(9,3),rep(10,3)), labels = c("I0-1","I1-3","I3-5","I5-10","I10-15","0-1","1-3","3-5","5-10","10-15"))
impact<- factor(c(rep(1,15), rep(2,15)), labels = c("Impacted","Pristine"))

meiodepthmod<-betadisper(dis,depth)
meioimpmod<-betadisper(dis,impact)
anova(meiodepthmod)
anova(meioimpmod)
plot(meioimpmod)
plot(meiodepthmod)

#BAR GRAPH
meiotax<-read.table("meiofaunastacked.txt",header=TRUE)
meiotax
names(meiotax)
attach(meiotax)

meiocbPalette <- c("royalblue1", "#FFCC66", "violetred2", "springgreen4", "#996633", "#99FFFF","#00CC99", "#CCFF33", "#CCCCCC")

Phylum <- factor(meiotax$phylum, levels = c("Amphipoda", "Gastroricha","Copepoda", "Polychaete", "Nematoda"))
Depth <- factor(meiotax$Site, levels = c("trawled_0_1", 
                                         "untrawled_0_1", 
                                         "trawled_1_3", 
                                         "untrawled_1_3", 
                                         "trawled_3_5", 
                                         "untrawled_3_5", 
                                         "trawled_5_10",
                                         "untrawled_5_10",
                                         "trawled_10_15",
                                         "untrawled_10_15"))

ggplot(meiotax, aes(fill=Phylum, y=Count, x=Depth)) + 
  geom_bar(stat="identity") + theme_classic()+
  scale_x_discrete(labels=c("Trawled \n 0 - 1", 
                            "Untrawled \n 0 - 1", 
                            "Trawled \n 1 - 3", 
                            "Untrawled \n 1 - 3", 
                            "Trawled \n 3 - 5", 
                            "Untrawled \n 3 - 5", 
                            "Trawled \n 5 - 10", 
                            "Untrawled \n 5 - 10",
                            "Trawled \n 10 - 15", 
                            "Untrawled \n 10 - 15"))+
  labs( x = "Sediment Layer (cm)", y = "Total Abundance")+
  scale_fill_manual(values=meiocbPalette)

