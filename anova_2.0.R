setwd("C:/Users/Maria/Documents/Courses/ENVS4420")

library("readxl")
library("agricolae")
library("tidyverse")
library("AICcmodavg")
library("multcompView")
library("plotrix")
library("wesanderson") #colour palettes

mass.ph.data <- read_excel("mass_ph_data.xlsx", sheet="Total")

#anovas by block---------------------->
block1 <- subset(mass.ph.data, block==1, select=c(WR,MC,AR,mass,pH))
block2 <- subset(mass.ph.data, block==2, select=c(WR,MC,AR,mass,pH))
block3 <- subset(mass.ph.data, block==3, select=c(WR,MC,AR,mass,pH))

#interactions - mass
interaction.b1 <- aov(mass~as.factor(WR)+as.factor(AR), data=block1)
summary(interaction.b1)
interaction.b2 <- aov(mass~as.factor(WR)+as.factor(MC), data=block2)
summary(interaction.b2)
interaction.b3 <- aov(mass~as.factor(MC)+as.factor(AR), data=block3)
summary(interaction.b3)

#interactions - pH
anova.b1pH <- aov(pH ~ as.factor(WR)+as.factor(AR),data = block1)
summary(anova.b1pH)
anova.b2pH <- aov(pH ~ as.factor(WR)+as.factor(MC),data = block2)
summary(anova.b2pH)
anova.b3pH <- aov(pH ~ as.factor(MC)+as.factor(AR),data = block3)
summary(anova.b3pH)

#tukey by block
tukey.b1.mass <- TukeyHSD(interaction.b1)
tukey.b2.mass <- TukeyHSD(interaction.b2)
tukey.b3.mass <- TukeyHSD(interaction.b3)
tukey.b1.pH <- TukeyHSD(anova.b1pH)
tukey.b2.pH <- TukeyHSD(anova.b2pH)
tukey.b3.pH <- TukeyHSD(anova.b3pH, na.rm=T)


#mass ----------------------------->
#b1 tukey w/ letters
tukeyb1.mass.letters <- multcompLetters4(interaction.b1,tukey.b1.mass)
print(tukeyb1.mass.letters)
#b2 tukey w/ letters
tukeyb2.mass.letters <- multcompLetters4(interaction.b2,tukey.b2.mass)
print(tukeyb2.mass.letters)
#b3 tukey w/ letters
tukeyb3.mass.letters <- multcompLetters4(interaction.b3,tukey.b3.mass)
print(tukeyb3.mass.letters)
#pH ----------------------------->
#b1 tukey w/ letters
tukeyb1.pH.letters <- multcompLetters4(anova.b1pH,tukey.b1.pH)
print(tukeyb1.pH.letters)
#b2 tukey w/ letters
tukeyb2.pH.letters <- multcompLetters4(anova.b2pH,tukey.b2.pH)
print(tukeyb2.pH.letters)
#b3 tukey w/ letters
tukeyb3.pH.letters <- multcompLetters4(anova.b3pH,tukey.b3.pH)
print(tukeyb3.pH.letters)

#plots--------------------------->
#tables for data + tukey letters-------->
serror <- function(x) sd(x)/sqrt(length(x))
#b1mass
b1_summary <- group_by(mass.ph.data,WR,AR) %>% summarise(mean=mean(mass), sd=sd(mass), se=serror(mass)) %>% arrange(desc(mean))
tukeyb1.mass.letters <- as.data.frame.list(tukeyb1.mass.letters$`as.factor(WR):as.factor(AR)`)
b1_summary$Tukey <- tukeyb1.mass.letters$Letters
#b2mass
b2_summary <- group_by(mass.ph.data,WR,MC) %>% summarise(mean=mean(mass), sd=sd(mass), se=serror(mass)) %>% arrange(desc(mean))
tukeyb2.mass.letters <- as.data.frame.list(tukeyb2.mass.letters$`as.factor(WR):as.factor(MC)`)
b2_summary$Tukey <- tukeyb2.mass.letters$Letters
#b3mass
b3_summary <- group_by(mass.ph.data,MC,AR) %>% summarise(mean=mean(mass), sd=sd(mass),se=serror(mass)) %>% arrange(desc(mean))
tukeyb3.mass.letters <- as.data.frame.list(tukeyb3.mass.letters$`as.factor(MC):as.factor(AR)`)
b3_summary$Tukey <- tukeyb3.mass.letters$Letters

#b1pH
b1_summarypH <- group_by(mass.ph.data,WR,AR) %>% summarise(mean=mean(pH), sd=sd(pH),se=serror(pH)) %>% arrange(desc(mean))
tukeyb1.pH.letters <- as.data.frame.list(tukeyb1.pH.letters$`as.factor(WR):as.factor(AR)`)
b1_summarypH$Tukey <- tukeyb1.pH.letters$Letters
#b2pH
b2_summarypH <- group_by(mass.ph.data,WR,MC) %>% summarise(mean=mean(pH), sd=sd(pH),se=serror(pH)) %>% arrange(desc(mean))
tukeyb2.pH.letters <- as.data.frame.list(tukeyb2.pH.letters$`as.factor(WR):as.factor(MC)`)
b2_summarypH$Tukey <- tukeyb2.pH.letters$Letters
#b3pH
b3_summarypH <- group_by(mass.ph.data,MC,AR) %>% summarise(mean=mean(pH), sd=sd(pH),se=serror(pH)) %>% arrange(desc(mean))
tukeyb3.pH.letters <- as.data.frame.list(tukeyb3.pH.letters$`as.factor(MC):as.factor(AR)`)
b3_summarypH$Tukey <- tukeyb3.pH.letters$Letters

#graphs including tukey letters ---->

#b1mass
b1_summary$WR <- factor(b1_summary$WR, level = c("1:4","1:1","4:1"))
ggplot(b1_summary, aes(x=factor(WR), y=mean, fill=as.factor(AR))) + geom_bar(stat = "identity", position = "dodge", alpha = 0.9) + 
  geom_errorbar(aes(ymin = mean-se, ymax=mean+se), position = position_dodge(0.9), width = 0.25, show.legend = F)+ labs(x = "dry mass peat : green waste", y = "Final Mass of Mixture (g)", fill = "Aeration Rate (L/min)") + 
  scale_y_continuous(limits = c(0,310), expand = c(0, 0), breaks=seq(0,310,20)) + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ theme(legend.position = "bottom", legend.box = "horizontal",text=element_text(size=15)) + 
  geom_text(aes(label = ), position = position_dodge(0.9), size = 7, vjust = -2.0, hjust = 0.5, color = "gray25")  + 
  scale_fill_manual(values=wes_palette(n=3, name="GrandBudapest1")) + guides(color=guide_legend("Aeration Rate (L/min)", fill = FALSE))
ggsave("b1barplot2.png", width=20,height=15,dpi=200, units = "cm", device="png")
#b2mass
b2_summary$WR <- factor(b2_summary$WR, level = c("1:4","1:1","4:1"))
ggplot(b2_summary, aes(x=factor(WR), y=mean, fill=as.factor(MC))) + geom_bar(stat = "identity", position = "dodge", alpha = 0.9) + 
  geom_errorbar(aes(ymin = mean - se, ymax=mean+se), position = position_dodge(0.9), width = 0.25, show.legend = F)+ labs(x = "dry mass peat : green waste", y = "Final Mass of Mixture (g)", fill = "Moisture Content (%)") + 
  scale_y_continuous(limits = c(0,310), expand = c(0, 0),breaks=seq(0,310,20)) + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ theme(legend.position = "bottom", legend.box = "horizontal",text=element_text(size=15)) + 
  geom_text(aes(label = Tukey), position = position_dodge(0.9), size = 7, vjust = -2.0, hjust = 0.5, color = "gray25")  + 
  scale_fill_manual(values=wes_palette(n=3, name="GrandBudapest2")) + guides(color=guide_legend("Mositure Content (%)", fill = FALSE))
ggsave("b2barplot2.png", width=20,height=15,dpi=200, units = "cm", device="png")
#b3mass
b3_summary$MC <- factor(b3_summary$MC, level = c("75","80","85"))
ggplot(b3_summary, aes(x=factor(MC), y=mean, fill=as.factor(AR))) + geom_bar(stat = "identity", position = "dodge", alpha = 0.9) + 
  geom_errorbar(aes(ymin = mean - se, ymax=mean+se), position = position_dodge(0.9), width = 0.25, show.legend = F)+ labs(x = "Mositure Content (%)", y = "Final Mass of Mixture (g)", fill = "Aeration Rate (L/min)") + 
  scale_y_continuous(limits = c(0,310), expand = c(0, 0),breaks=seq(0,310,20)) + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ theme(legend.position = "bottom", legend.box = "horizontal",text=element_text(size=15)) + 
  geom_text(aes(label = Tukey), position = position_dodge(0.9), size = 7, vjust = -2.0, hjust = 0.5, color = "gray25")  + 
  scale_fill_manual(values=wes_palette(n=3, name="Darjeeling2")) +  guides(color=guide_legend("Aeration Rate (L/min)", fill = FALSE))
ggsave("b3barplot2.png", width=20,height=15,dpi=200, units = "cm", device="png")

#b1pH
b1_summarypH$WR <- factor(b1_summarypH$WR, level = c("1:4","1:1","4:1"))
ggplot(b1_summarypH, aes(x=factor(WR), y=mean, fill=as.factor(AR))) + geom_bar(stat = "identity", position = "dodge", alpha = 0.9) + 
  geom_errorbar(aes(ymin = mean - se, ymax=mean+se), position = position_dodge(0.9), width = 0.25, show.legend = F)+ labs(x = "dry mass peat : green waste", y = "pH", fill = "Aeration Rate (L/min)") + 
  scale_y_continuous(limits = c(0,10), expand = c(0, 0),breaks=seq(0,10,1)) + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ theme(legend.position = "bottom", legend.box = "horizontal",text=element_text(size=15)) + 
  geom_text(aes(label = Tukey), position = position_dodge(0.9), size = 7, vjust = -2.0, hjust = 0.5, color = "gray25")  + 
  scale_fill_manual(values=wes_palette(n=3, name="GrandBudapest1")) + guides(color=guide_legend("Aeration Rate (L/min)", fill = FALSE))
ggsave("b1pHbarplot2.png", width=20,height=15,dpi=200, units = "cm", device="png")
#b2pH
b2_summarypH$WR <- factor(b2_summarypH$WR, level = c("1:4","1:1","4:1"))
ggplot(b2_summarypH, aes(x=factor(WR), y=mean, fill=as.factor(MC))) + geom_bar(stat = "identity", position = "dodge", alpha = 0.9) + 
  geom_errorbar(aes(ymin = mean - se, ymax=mean+se), position = position_dodge(0.9), width = 0.25, show.legend = F)+ labs(x = "dry mass peat : green waste", y = "pH", fill = "Moisture Content (%)") + 
  scale_y_continuous(limits = c(0,10), expand = c(0, 0), breaks=seq(0,10,1)) + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ theme(legend.position = "bottom", legend.box = "horizontal",text=element_text(size=15)) + 
  geom_text(aes(label = Tukey), position = position_dodge(0.9), size = 7, vjust = -2.0, hjust = 0.5, color = "gray25")  + 
  scale_fill_manual(values=wes_palette(n=3, name="GrandBudapest2")) + guides(color=guide_legend("Mositure Content (%)", fill = FALSE))
ggsave("b2pHbarplot2.png", width=20,height=15,dpi=200, units = "cm", device="png")
#b3pH
b3_summarypH$MC <- factor(b3_summarypH$MC, level = c("75","80","85"))
ggplot(b3_summarypH, aes(x=factor(MC), y=mean, fill=as.factor(AR))) + geom_bar(stat = "identity", position = "dodge", alpha = 0.9) + 
  geom_errorbar(aes(ymin = mean - se, ymax=mean+se), position = position_dodge(0.9), width = 0.25, show.legend = F)+ labs(x = "Moisture Content (%)", y = "pH", fill ="Aeration Rate (L/min") + 
  scale_y_continuous(limits = c(0,10), expand = c(0, 0), breaks=seq(0,10,1)) + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ theme(legend.position = "bottom", legend.box = "horizontal", text=element_text(size=15)) + 
  geom_text(aes(label = Tukey), position = position_dodge(0.9), size = 7, vjust = -2.0, hjust = 0.5, color = "gray25")  + 
  scale_fill_manual(values=wes_palette(n=3, name="Darjeeling2")) + guides(color=guide_legend("Aeration Rate (L/min)", fill = FALSE))
ggsave("b3pHbarplot2.png", width=20,height=15,dpi=200, units = "cm", device="png")
