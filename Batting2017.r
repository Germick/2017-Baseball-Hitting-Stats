## Alex Germick - 10-23-2107
## Who made contact in 2017?
## Can Aaron Judge repeat his season?

rm(list=ls(all=TRUE))
getwd()
setwd("C:/Users/AG/Downloads")

###Read 2017 Batting Statistics
batting <- read.csv("Bat2017.csv")

###Filter Batters with 500 at bats and check for NA
batting500AB <- batting[batting$PA>500,]
sum(is.na(batting500AB))


###Highest Batting Average
batting500AB[which.max(batting500AB$BA),c(2,4,19)]

# It's Jose!
# Name  Tm    BA
# 37 JoseÂ Altuve\\altuvjo01 HOU 0.346

### Most Plate Apps
batting500AB[which.max(batting500AB$PA),c(2,4,7)]
# Name  Tm  PA
# 136 CharlieÂ Blackmon*\\blackch02 COL 725

### Most Homeruns 
batting500AB[which.max(batting500AB$HR),]

### Most Sacrifices
batting500AB[which.max(batting500AB$SF),]


### Construct SORate, BBRate, and In- Play Rate
batting500AB$SORate <- batting500AB$SO/batting500AB$PA
batting500AB[is.nan(batting500AB[,31])] <- 0

batting500AB$BBRate <- batting500AB$BB/batting500AB$PA
batting500AB[is.nan(batting500AB[,32])] <- 0

batting500AB$IPRate <- 1-batting500AB$SORate-batting500AB$BBRate
batting500AB$SBRate <- (batting500AB$SB/(batting500AB$SB+batting500AB$CS))

batting500AB$HRRateAB <- batting500AB$HR/batting500AB$AB
batting500AB[is.nan(batting500AB[,35])] <- 0
batting500AB$HRRatePA <- batting500AB$HR/batting500AB$PA
batting500AB[is.nan(batting500AB[,36])] <- 0
sum(is.na(batting500AB))

batting500AB <- na.omit(batting500AB)
sum(is.na(batting500AB))


### Worst In Play Rate
batting500AB[which.min(batting500AB$IPRate),c(2,4,33)]
# Name  Tm    IPRate
# 500 JoeyÂ Gallo*\\gallojo01 TEX 0.4906015


### Best In Play Rate
batting500AB[which.max(batting500AB$IPRate),c(2,4,33)]
# Yuli, what a pickup
# Name  Tm    IPRate
# 616 YuliÂ Gurriel\\gourryu01 HOU 0.8510638

### Worst Strikeout Rate
batting500AB[which.max(batting500AB$SORate),c(2,4,31)]

## Best BB Rate
batting500AB[which.max(batting500AB$BBRate),c(2,4,32)]

## Homeruns vs. SO Rate
plot(batting500AB$HR,batting500AB$SORate)

## Average vs. In-Play Rate
plot(batting500AB$BA,batting500AB$IPRate)
my_model <- lm(IPRate~BA,data=batting500AB)
abline(my_model,col=("red"))


my_model_resid <- resid(my_model)
plot(density(my_model_resid))
which.min(my_model_resid)
batting500AB[68,c(2,4,33)]


# Name  Tm    IPRate
# 756 AaronÂ Judge\\judgeaa01 NYY 0.5058997

### Largest negative residual is Aaron Judge suggesting his batting average was abnormally high
### against his contact rate. Would have to investigate hard contact rate to know more about his BA.


#Stolen Base Rate vs. Stolen bases
plot(batting500AB$SBRate,batting500AB$SB)



