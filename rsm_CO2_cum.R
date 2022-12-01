# Response surface model for ENVS*4420
# "Optimizing Composting of Vertical Farm Waste"
# Mariaelisa Polsinelli
# University of Guelph
# December 2021
# See https://cran.r-project.org/web/packages/rsm/vignettes/rsm.pdf
# and http://www2.uaem.mx/r-mirror/web/packages/rsm/vignettes/rsm-plots.pdf

install.packages("rsm")
library("rsm")

setwd("C:/Users/Maria/Documents/Courses/ENVS4420/Data_analysis/CO2_cum")
data.block1 <- read.csv("co2_cum_g_block1.csv")
data.block2 <- read.csv("co2_cum_g_block1.csv")
data.block3 <- read.csv("co2_cum_g_block1.csv")


organizeBlock <- function(x,xcol){
#oragnizeBlock(dataset,treatment col)
  #modify .csv to have columns for each treatment variable: 
  #mass ratio, moisture content, aeration rate
  
  MR <- c(0.2, 0.5, 0.8) #%s based on ratios 1:4, 1:1, 4:1
  MC <- c(75,80,85)
  AR <- c(0.25, 0.5, 0.75)
  
  trmtA <- grep("A", xcol)
  for (i in trmtA){
    x$mass_ratio[i] <- MR[2]
    x$moisture_content[i] <- MC[1]
    x$aeration_rate[i] <- AR[1]
  }
  trmtB <- grep("B", xcol)
  for (i in trmtB){
    x$mass_ratio[i] <- MR[2]
    x$moisture_content[i] <- MC[3]
    x$aeration_rate[i] <- AR[1]
  }
  trmtC <- grep("C", xcol)
  for (i in trmtC){
    x$mass_ratio[i] <- MR[2]
    x$moisture_content[i] <- MC[1]
    x$aeration_rate[i] <- AR[3]
  }
  trmtD <- grep("D", xcol)
  for (i in trmtD){
    x$mass_ratio[i] <- MR[2]
    x$moisture_content[i] <- MC[3]
    x$aeration_rate[i] <- AR[3]
  }
  trmtE <- grep("E", xcol)
  for (i in trmtE){
    x$mass_ratio[i] <- MR[2]
    x$moisture_content[i] <- MC[2]
    x$aeration_rate[i] <- AR[2]
  }
  return(x)
}
#edit MR, MC, and AR to be specific to blocks treatments 
#before each organizeBlock function call
data.block1 <- organizeBlock(data.block1,data.block1$trmt)
data.block2 <- organizeBlock(data.block2,data.block2$trmt)
data.block3 <- organizeBlock(data.block2,data.block3$trmt)

#code data for rsm
data_set <- coded.data(data.block1, x1 ~ (mass_ratio - 0.5)/0.3, 
                       x2 ~ (moisture_content - 80)/5, 
                       x3 ~ (aeration_rate - 0.5)/0.25)

#combine blocks, djoin will automatically code them
rsm_data_set <- djoin(data_set, data.block2, data.block3)

#rsm
data_rsm <- rsm(cum_co2_g ~ Block + SO(x1,x2,x3), data=rsm_data_set)

#save summary output to a .txt file
sink("rsm_summary.txt")
print(summary(data_rsm))
sink()

#plot spacing (3 plots in 1 row)
par(mfrow = c(1,3))

#Save plots in six separate PNG files
png.hook = list(
  pre.plot = function(lab)
    png(file = paste(lab[3],lab[4],".png", sep = "")),
  post.plot = function(lab)
    dev.off())

#create contour plots
contour_plots <- contour(data_rsm, ~x1+x2+x3, image=T, at=xs(data_rsm), hook = png.hook)
dev.off()

#create perspective plots
persp_plots <- persp(data_rsm, ~x1+x2+x3, col = "heat", contours = "colors",
                     at=xs(data_rsm), hook = png.hook)



