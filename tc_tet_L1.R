#BIOASSAY L1
rm(list = ls()) #clear worksapce

setwd("")  #set wd
getwd() #check wd has been changed, make sure file is here
library("epitools")

#INPUT
data <- read.csv("tc_tet_L1.csv")  # read csv file  = "construct.csv")
tet <- subset(data,data$treatment == "T")
nt <- subset(data,data$treatment == "NT")

#enter data column by column, didnt check it was ok to pool!
tab1=matrix(c(sum(tet[,"t40"]),sum(nt[,"t40"]),50,50),nc=2)
chisq.test(tab1, correct=TRUE)
chisq.test(tab1, correct=TRUE,simulate.p.value = TRUE,B=10000)

##############
library(binom)
con.table <- tab1
tet <- binom.confint(x=con.table[1,1], n=con.table[1,2],
                     conf.level=0.95, methods="exact")
non.tet <- binom.confint(x=con.table[2,1], n=con.table[2,2],
                         conf.level=0.95, methods="exact")

barplot(height=c(tet[1,4], non.tet$mean),
        names.arg=c("Tet", "Non-Tet"), ylim=c(0,1),
        col="white", ylab="Survival (L1 to adult)", main="", xlim=c(0,4))
arrows(x0=6/8 , y0=tet$lower,
       x1=6/8 , y1=tet$upper, code=3, angle=90)
arrows(x0=15/8 , y0=non.tet$lower,
       x1=15/8 , y1=non.tet$upper, code=3, angle=90)



