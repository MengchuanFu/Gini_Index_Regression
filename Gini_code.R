### Homework 3: SDGB 7840

### Mengchuan Fu (Mike)
### Session: 3:40PM
### A14008047
require(usdm)

# read data and data preprocessing
gini <- read.csv("gini.csv", header=TRUE, stringsAsFactors=FALSE) 
gini[gini[]==".."]<- NA
gini <- gini[complete.cases(gini),]
indicators <-read.csv("indicators.csv",header=TRUE,sep=",",stringsAsFactors = FALSE)

new_gini <- merge(gini,indicators,all.x=T)
new_gini[new_gini[]==".."]<- NA

for (i in c(3:8)){
  new_gini[,i]<-as.numeric(new_gini[,i])
}

new_gini <- new_gini[complete.cases(new_gini),]
colnames(new_gini) <- c("Country","Gini","Wage","Tax","Trade","Net_income","Labor_force","Education")

# scatterplot matrix
pairs(new_gini[,c(-1)], main="Pairwise Scatterplots of Variables", las=TRUE, col="#0080ff70", pch=19)

# summary statistics
summary(new_gini[,-1])
round(apply(new_gini[,-1], 2, sd), digits=3)

par(mfrow=c(2,4))
hist(new_gini$Gini, col="darkolivegreen4", las=TRUE, xlab="Gini", 
     main="Histogram of Gini", cex.axis=1.5, cex.lab=1.5, cex.main=1.8)
hist(new_gini$Wage, col="darkolivegreen4", las=TRUE, xlab="Wage",
     main="Histogram of Wage", cex.axis=1.5, cex.lab=1.5, cex.main=1.8)
hist(new_gini$Tax, col="darkolivegreen4", las=TRUE, xlab="Tax ($)", 
     main="Histogram of Tax", cex.axis=1.5, cex.lab=1.5, cex.main=1.8)
hist(new_gini$Trade, col="darkolivegreen4", las=TRUE, xlab="Trade", 
     main="Histogram of Trade", cex.axis=1.5, cex.lab=1.5, cex.main=1.8)
hist(new_gini$Net_income, col="darkolivegreen4", las=TRUE, xlab="Net_income", 
     main="Histogram of Net_income", cex.axis=1.5, cex.lab=1.5, cex.main=1.8)
hist(new_gini$Labor_force, col="darkolivegreen4", las=TRUE, xlab="Labor_force", 
     main="Histogram of Labor_force", cex.axis=1.5, cex.lab=1.5, cex.main=1.8)
hist(new_gini$Education, col="darkolivegreen4", las=TRUE, xlab="Education", 
     main="Histogram of Education", cex.axis=1.5, cex.lab=1.5, cex.main=1.8)

# some correlations
corr <- round(cor(new_gini[,c(2:8)]), digits=3)

cor_labor <- round(cor(new_gini[,2],log(new_gini[,7])),digits=3)
new_gini <- new_gini[,-7]
pairs(new_gini[,c(-1)], main="Pairwise Scatterplots of Variables", las=TRUE, col="#0080ff70", pch=19)

# fit model
lm.gini <- lm(Gini~Wage+Tax+Trade+Net_income+Education,data=new_gini)
summary(lm.gini)
vif(new_gini[,-1])

# logx and logy models of unsignificant variables
trans_cor<-list()
trans_cor[[1]] <- summary(lm(Gini~log(Tax),data=new_gini))
trans_cor[[2]] <- summary(lm(Gini~log(Trade),data=new_gini)) 
trans_cor[[3]] <- summary(lm(Gini~log(Education),data=new_gini))
trans_cor[[4]] <- summary(lm(log(Gini)~Tax,data=new_gini))
trans_cor[[5]] <- summary(lm(log(Gini)~Trade,data=new_gini))
trans_cor[[6]] <- summary(lm(log(Gini)~Education,data=new_gini))
trans_cor

# scatterplot matrix new
gini_2 <- new_gini 
gini_2 <- gini_2[,-4]
log_Trade <- log(new_gini[,5])
log_Education <- log(new_gini[,7])
gini_2[,4] <- log_Trade
gini_2[,6] <- log_Education
colnames(gini_2) <- c("Country","Gini","Wage","log_Trade","Net_income","log_Education")
pairs(gini_2[,c(-1)], main="Pairwise Scatterplots of Variables", las=TRUE, col="#0080ff70", pch=19)

# fit model
lm.gini_2 <- lm(Gini~Wage+log_Trade+Net_income+log_Education,data=gini_2)
summary(lm.gini_2)
vif(gini_2[,-1])


# get rid of education
lm.gini_3 <- lm(Gini~Wage+log_Trade+Net_income,data=gini_2)
summary(lm.gini_3)

# compute VIF
gini_3 <- gini_2[,-6]
pairs(gini_3[,c(-1)], main="Pairwise Scatterplots of Variables", las=TRUE, col="#0080ff70", pch=19)
  
vif(gini_3[,-1])

# partial f-test
anova(lm.gini_3,lm.gini_2)

# histograms
par(mfrow=c(1,4))
hist(gini_3$Gini, col="darkolivegreen4", las=TRUE, xlab="Gini", 
     main="Histogram of Gini", cex.axis=1.5, cex.lab=1.5, cex.main=1.8)
hist(gini_3$Wage, col="darkolivegreen4", las=TRUE, xlab="Wage",
     main="Histogram of Wage", cex.axis=1.5, cex.lab=1.5, cex.main=1.8)
hist(gini_3$log_Trade, col="darkolivegreen4", las=TRUE, xlab="log_Trade", 
     main="Histogram of log_Trade", cex.axis=1.5, cex.lab=1.5, cex.main=1.8)
hist(gini_3$Net_income, col="darkolivegreen4", las=TRUE, xlab="Net_income", 
     main="Histogram of Net_income", cex.axis=1.5, cex.lab=1.5, cex.main=1.8)

# summary statistics
summary(gini_3[,c("Gini", "Wage","log_Trade","Net_income")])
round(apply(gini_3[,c("Gini", "Wage","log_Trade","Net_income")], 2, sd), digits=3)



# residuals vs. fitted values
plot(lm.gini_3$fitted.values, lm.gini_3$residuals, las=TRUE, pch=19, 
     main=expression("Residuals vs. Fitted Values"), xlab="fitted values", ylab="residuals",
     cex.axis=1.8, cex.lab=1.8, cex.main=1.8,ylim=c(-22,22))
abline(h=c(0, 3*summary(lm.gini_3)$sigma, -3*summary(lm.gini_3)$sigma), lty=2, 
       col=c("gray50", "darkolivegreen4", "darkolivegreen4"), lwd=2)
# residuals vs. Wage
plot(gini_3$Wage, lm.gini_3$residuals, las=TRUE, pch=19, 
     main=expression("Residuals vs. Wage"), xlab=expression(paste("Wage")), 
     ylab="residuals",  cex.axis=1.8, cex.lab=1.8, cex.main=1.8)
abline(h=0, lty=2, col="gray50", lwd=2)
  # residuals vs. log_Trade
plot(gini_3$log_Trade, lm.gini_3$residuals, las=TRUE, pch=19, 
     main=expression("Residuals vs. log_Trade"), xlab=expression(paste("log_Trade")), 
     ylab="residuals",  cex.axis=1.8, cex.lab=1.8, cex.main=1.8)
abline(h=0, lty=2, col="gray50", lwd=2)
# residuals vs. Net_income
plot(gini_3$Net_income, lm.gini_3$residuals, las=TRUE, pch=19, 
     main=expression("Residuals vs. Net_income"), xlab=expression(paste("Net_income")), 
     ylab="residuals",  cex.axis=1.8, cex.lab=1.8, cex.main=1.8)
abline(h=0, lty=2, col="gray50", lwd=2)

qqnorm(lm.gini_3$residuals, pch=19, col="darkolivegreen4", las=TRUE, main="Normal Q-Q Plot")
qqline(lm.gini_3$residuals, lwd=2)



# residuals vs. fited value
plot(gini_3$Wage, lm.gini_3$residuals, las=TRUE, pch=19,
     main=expression(paste("Residuals vs. ","Fited value")), xlab=expression(AB[1450]), ylab="residuals",
      cex.axis=1.8, cex.lab=1.8, cex.main=1.8)
abline(h=0, lty=2, col="gray50", lwd=2)
dev.off()
