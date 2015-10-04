#PROGRAM: DOSE RESPONSE EC50 and SE
#  Calculates the EC50 and SE for dose response
#  quality control experiments, reads in 'FemFract.txt' file

rm(list = ls())  # clear workspace
#THEA CHANGE this to your correct folder for this QC
setwd("")
getwd()  # check wd has been changed, make sure file is here

#PACKAGES
if(!require("drc")) {
  install.packages("drc")
  require("drc")
}  # if drc is not installed, install it then load it

#----------------------------------------------------------------

#INPUT
FemFract<-read.table("FemFract.txt",header=T, fill=TRUE)  # read in the data
head(FemFract)  # check the data top 6 rows
str(FemFract)  # check the type of data, are the variables the right class e.g. numeric?
#----------------------------------------------------------------
#ANALYSIS
FitDRC <- drm(FemRatio ~ Dose_ug, data = FemFract, fct = LL.3())
LD50 <- ED(FitDRC,c(50),interval="delta")

#----------------------------------------------------------------
#OUTPUT
png('tcld50.png')

plot(FitDRC,broken=TRUE, pch = 19, axes = FALSE, ylab = "Mortality (%)", xlab = "Dose (ug / mg)", yaxt="n")
axis(side = 1, at = c(0, 0.1, 1, 10, 100))
axis(side = 2, at = c(0, 5, 10, 15, 20, 25), labels = c(0, 20, 40, 60, 80, 100))

dev.off()






