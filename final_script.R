library(ggplot2)
library(pheatmap)
library(gridExtra)
library(naniar)
library(factoextra)
library(leaps)
library(pROC)

setwd("~/Desktop/UniPd/1 - Secondo Semestre/Statistical Learning/Part II/Progetto")
setwd("/Users/Fra/Desktop/STATISTICAL LEARNING/MOD. B/PROGETTO")
setwd("/home/shift97/Documents/studio/Padova/Unipd/Didattica/I Anno, II Semestre/Statistical Learning/Mod. B/2019-2020/Project/tennis_tournaments/")
source('functions.R')

filepath = "final_tennis_stp_swapped.csv"
data = read.csv(filepath, header = T)

## MISSING VALUES
# we observe that ST variables contain many NAs and also
# many TPW are NA (Wimbledon tournaments miss those data)
gg_miss_var(data)

dim(na.omit(data)) # too few instances

# STP variables are not necessarily not NA by construction;
# we summarized the information in the ST variables, so we can remove them
data = subset(data, select = -c(X, ST1.1, ST2.1, ST3.1, ST4.1, ST5.1, 
                                     ST1.2, ST2.2, ST3.2, ST4.2, ST5.2))

dim(na.omit(data)) # now there are more
# we now remove any row which has NAs on any attribute (this excludes Wimbledon)
data = na.omit(data)

# finally, we remove SSP variables which carry the same information as FSP
data = subset(data, select = -c(SSP.1, SSP.2))

# we create a row identifier for each match
data[, 1] = paste(data$Player1, data$Player2, sep='/')
data = subset(data, select = -c(Player2))
colnames(data)[1] = 'Match'

# Handling categorical variable:
data$GND <- as.factor(data$GND)
data$CRT <- as.factor(data$CRT)
attach(data)
is.ordered(GND)
is.ordered(CRT)

palette = RColorBrewer::brewer.pal(name='Dark2', 8)


### EDA
### BOXPLOTS
#Boxplots of player 1
par(mfrow=c(2,6))
par(pin=c(1.2,1.2))
boxplot(FSP.1, main='% First Serve', col=palette[1])
boxplot(FSW.1, main='First Serve Won', col='burlywood3')
boxplot(SSW.1, main='Second Serve Won', col='darkviolet')
boxplot(ACE.1, main='Ace', col=palette[2])
boxplot(DBF.1, main='Double Faults', col=palette[3])
boxplot(WNR.1, main='Winners', col=palette[4])
boxplot(UFE.1, main='Unforced Errors', col=palette[5])
boxplot(BPC.1, main='Break Points Created', col=palette[6])
boxplot(BPW.1, main='Break Points Won', col=palette[7])
boxplot(NPA.1, main='Net Points Attempted', col=palette[8])
boxplot(NPW.1, main='Net Points Won', col='coral1')
boxplot(TPW.1, main='Total Points Won',  col='antiquewhite')

### HISTOGRAMS
# First Player only
par(mfrow=c(2,6))
par(pin=c(1.2,1.2))
hist(FSP.1, main='% First Serve', col=palette[1], probability = T)
lines(density(FSP.1))
hist(FSW.1, main='First Serve Won', col='burlywood3', probability = T)
lines(density(FSW.1))
hist(SSW.1, main='Second Serve Won', col='darkviolet', probability = T)
lines(density(SSW.1))
hist(ACE.1, main='Ace', col=palette[2], probability = T)
lines(density(ACE.1))
hist(DBF.1, main='Double Faults', col=palette[3], probability = T)
lines(density(DBF.1))
hist(WNR.1, main='Winners', col=palette[4], probability = T)
lines(density(WNR.1))
hist(UFE.1, main='Unforced Errors', col=palette[5], probability = T)
lines(density(UFE.1))
hist(BPC.1, main='Break Points Created', col=palette[6], probability = T)
lines(density(BPC.1))
hist(BPW.1, main='Break Points Won', col=palette[7], probability = T)
lines(density(BPW.1))
hist(NPA.1, main='Net Points Attempted', col=palette[8], probability = T)
lines(density(NPA.1))
hist(NPW.1, main='Net Points Won', col='coral1', probability = T)
lines(density(NPW.1))
hist(TPW.1, main='Total Points Won',  col='antiquewhite', probability = T)
lines(density(TPW.1))

### FSP + FSW
# stato grafico non valido risolto con: dev.off()
plt1 = double_plot(FSP.1, FSP.2, "First Served Percentage", c(0, 100))
plt2 = double_plot(FSW.1, FSW.2, "First Served Won", c(0, 100))
quadruple_plot(plt1, plt2)

# check for normality
par(mfrow=c(1,2))
qqnorm(FSP.1, main='FSP.1')
qqline(FSP.1)
qqnorm(FSP.2, main='FSP.2')
qqline(FSP.2)

ggplot(data, aes(y = FSP.1, fill = as.factor(STP), x = STP)) +
  theme_bw() +
  geom_boxplot() +
  scale_fill_brewer(palette="Dark2", name= 'STP') +
  ggtitle('Boxplot - FSP.1 vs STP')

ggplot(data, aes(y = FSP.2, fill = as.factor(STP), x = STP)) +
  theme_bw() +
  geom_boxplot() +
  scale_fill_brewer(palette="Dark2", name= 'STP') +
  ggtitle('Boxplot - FSP.2 vs STP')

### Aces Won (Not in Presentation)
double_plot(ACE.1, ACE.2, "Aces Won")

### Doubles faults committed 
double_plot(DBF.1, DBF.2, "Doubles faults committed")

par(mfrow=c(1,2))
qqnorm(data$DBF.1, main='DBF.1')
qqline(data$DBF.1)
qqnorm(data$DBF.2, main='DBF.2')
qqline(data$DBF.2)
par(mfrow=c(1,1))


par(mfrow=c(2,2))
#hist(data[data$STP == 1,]$DBF.1) # we only have two instances
hist(data[data$STP == 2,]$DBF.1, main = '2 Sets Played', xlab = 'DBF.1', col = palette[1])
hist(data[data$STP == 3,]$DBF.1, main = '3 Sets Played', xlab = 'DBF.1', col = palette[2])
hist(data[data$STP == 4,]$DBF.1, main = '4 Sets Played', xlab = 'DBF.1', col = palette[3])
hist(data[data$STP == 5,]$DBF.1, main = '5 Sets Played', xlab = 'DBF.1', col = palette[4])

### Winners earned (Not in Presentation)
double_plot(WNR.1, WNR.2, "Winners earned")

### Unforced errors committed (Not in Presentation)
double_plot(UFE.1, UFE.2, "Unforced errors committed")

### Break Points
plt1 = double_plot(BPC.1, BPC.2, "Break Points Created", c(0, 30))
plt2 = double_plot(BPW.1, BPW.2, "Break Points Won", c(0, 30))
quadruple_plot(plt1, plt2)

par(mfrow=c(1,2))
qqnorm(data$BPC.1, main='BPC.1')
qqline(data$BPC.1)
qqnorm(data$BPW.1, main='BPW.1')
qqline(data$BPW.1)
par(mfrow=c(1,1))
# notice lower-left region with many points

### Net Points (Not in Presentation)
plt1 = double_plot(NPA.1, NPA.2, "Net Points attempted", c(0, 100))
plt2 = double_plot(NPW.1, NPW.2, "Net Points won", c(0, 80))
quadruple_plot(plt1, plt2)

### Total points won
double_plot(TPW.1, TPW.2, "Total points won") #bimodal?

### EXPLORING BIMODALITY
### GENDER
pl_gnd_1 = ggplot(data, aes(TPW.1)) +
  geom_bar(aes(fill=ifelse(GND == 0, palette[1], palette[2]))) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x = 'Player 1') +
  scale_fill_identity("Gender", labels = c('F', 'M'), guide='legend')
pl_gnd_2 = ggplot(data, aes(TPW.2)) +
  geom_bar(aes(fill=ifelse(GND == 0, palette[1], palette[2]))) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x = 'Player 2') +
  scale_fill_identity("Gender", labels = c('F', 'M'), guide='legend')
pl_cond_gnd = grid.arrange(grobs = list(pl_gnd_1, pl_gnd_2), n_row = 2, top="TPW vs Gender")

### STP
pl_stp_1 = ggplot(data, aes(TPW.1)) +
  geom_bar(aes(fill=ifelse(STP > 3, palette[4], palette[3]))) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x = 'Player 1') +
  scale_fill_identity("Sets Played", labels = c('3 or less', 'more than 3'), guide='legend')
pl_stp_2 = ggplot(data, aes(TPW.2)) +
  geom_bar(aes(fill=ifelse(STP > 3, palette[4], palette[3]))) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x = 'Player 2') +
  scale_fill_identity("Sets Played", labels = c('3 or less', 'more than 3'), guide='legend')
pl_cond_stp = grid.arrange(grobs = list(pl_stp_1, pl_stp_2), n_row = 2, top="TPW vs STP")

## CONDITIONED SCATTERPLOTS (Not in Presentation)
predictors1 = list(NPA.1, UFE.1)
predictors2 = list(NPA.2, UFE.2)
hues = list(Round)
main_titles = c("Net Points Attempted", "Unforced Errors")
legend_titles = c("Round")

for (i in 1:length(predictors1)) {
  for (j in 1:length(hues)) {
    cond_scatter(predictors1[[i]], predictors2[[i]], TPW.1, TPW.2, hues[[j]],
                 titles = c('Player 1', 'Player 2', main_titles[i]), 
                 legend_tit = legend_titles[j],
                 x_lab = main_titles[i], y_lab = 'Total Points Won')
  }
}
# we see that the tournament Round does not influence neither NPA nor UFE


### PCA
data_pca = subset(data, select = -c(Match, Result, FNL.1, FNL.2))
pca1 = prcomp(data_pca, scale=T)
summary(pca1)

loadings = as.data.frame(pca1$rotation)
loadings_c1 = loadings$PC1
loadings_c2 = loadings$PC2
loadings_c3 = loadings$PC3
loadings_c4 = loadings$PC4
loadings_c5 = loadings$PC5

### Visualize loadings
par(mfrow=c(1,2))
pl_load_pc1 = barplot(loadings_c1, names.arg  = colnames(data_pca),
                      cex.names = 0.7, las = 2, main="1st principal component loadings", col=2)
pl_load_pc2 = barplot(loadings_c2, names.arg  = colnames(data_pca),
                      cex.names = 0.7, las = 2, main="2nd principal component loadings", col=3)

# Fractions of total variance explained by the different principal components
pca1$sdev
pca1$sdev / sum(pca1$sdev)


# (Not in Presentation)
par(mfrow=c(1,1))
pl_load_pc3 = barplot(loadings_c3, names.arg  = colnames(data_pca),
                      cex.names = 0.7, las = 2, main="3rd principal component loadings")
pl_load_pc4 = barplot(loadings_c4, names.arg  = colnames(data_pca),
                      cex.names = 0.7, las = 2, main="4th principal component loadings")
pl_load_pc5 = barplot(loadings_c5, names.arg  = colnames(data_pca),
                      cex.names = 0.7, las = 2, main="5th principal component loadings")


### BIPLOTS
### PC1 vs PC2
plot_c1 = abs(loadings_c1) >= 0.25
plot_c2 = abs(loadings_c2) >= 0.25 # 0.17 to include UFE
variables2plot = colnames(data_pca)[plot_c1 | plot_c2]

fviz_pca_biplot(pca1,
                label='var',
                geom.ind = 'point',
                select.var=list(name=variables2plot),
                habillage = data$STP,
                legend.title = "No. Sets Played",
                title = "Biplot - PC1 vs PC2")

fviz_pca_biplot(pca1,
                label='var',
                geom.ind = 'point',
                select.var=list(name=variables2plot),
                habillage = data$Result,
                legend.title = "Winner",
                title = "Biplot - PC1 vs PC2")










######### Simple Linear Regression

## PAIRS PLOTS w/ each player's variables
not_points_1 = c(2, 6, 10, 12, 13, 15, 32)
pairs(data[c(17, not_points_1)], 
      diag.panel=panel.hist, upper.panel=panel.cor, lower.panel=panel.smooth)

# Correlation with STP is obvious.

#### UFE.1 Unforced Errors committed by player 1 (Numeric) 
reg.out <- lm(TPW.1 ~ UFE.1)
summary(reg.out)
plot(UFE.1, TPW.1, pch=20, main = "Simple Linear Regression")
abline(reg.out, col = 3)
# plot(reg.out)


#### BPC.1 Break Points Created by player 1 (Numeric) 
reg.out <- lm(TPW.1 ~ BPC.1)
summary(reg.out)
plot(BPC.1, TPW.1, pch=20, main = "Simple Linear Regression")
abline(reg.out, col = 3)


#### NPA.1 Net Points Attempted by player 1 (Numeric) 
reg.out <- lm(TPW.1 ~ NPA.1)
summary(reg.out)
plot(NPA.1, TPW.1, pch=20, main = "Simple Linear Regression")
abline(reg.out, col = 3)







####### Multiple linear regression on variables 1
reg.out.red <- lm(TPW.1~FSP.1+DBF.1+UFE.1+BPC.1+NPA.1) 
summary(reg.out.red)

## HEATMAPS
palette = colorRampPalette(c("brown3", "white", "deepskyblue4"))
predictive_cols = c(6, 10, 12, 13, 15)

cor_mat_player = cor(data[predictive_cols])
heat1 = pheatmap(cor_mat_player, cluster_rows = F, 
                 cluster_cols = F, display_numbers = T,
                 color = palette(100))

reg.out.red <- update(reg.out.red, ~.-UFE.1)
summary(reg.out.red)
# The R2 decrease a lot.

# Restart with the initial model: 
reg.out.red <- lm(TPW.1~FSP.1+DBF.1+UFE.1+BPC.1+NPA.1)

reg.out.red <- update(reg.out.red, ~.-DBF.1)
summary(reg.out.red)

reg.out.red <- update(reg.out.red, ~.-FSP.1)
summary(reg.out.red)


## Diagnostic of the best model:
par(mfrow=c(1,1))
plot(reg.out.red)

# Removing Outliers:
res <- residuals(reg.out.red)
boxplot(res)
res_b <- boxplot(res)
ls_res_b <- as.numeric(rownames(data.frame(res_b$out)))
# View(data[ls_res_b,])
length(ls_res_b)
data_wo = data[-ls_res_b,]
attach(data_wo)
reg.out.red.wo <- lm(TPW.1~UFE.1+BPC.1+NPA.1) 
summary(reg.out.red.wo)
# (Not in Presentation)
plot(reg.out.red.wo)


# Removing HL points:
attach(data)
X = data
X$X = 1
X = X[, c("X","UFE.1", "BPC.1", "NPA.1")]
X = as.matrix(X)
H = X %*% solve(t(X) %*% X) %*% t(X)
dim(H)
length(diag(H))
which.max(diag(H))
# istanza 531 del dataset originale, istanza alla riga 274 del dataset X.

# Let's compute the threshold for defining high leverage points:
t <- (2*dim(X)[2])/dim(X)[1]
# threshold computed is 0.018: 2*p / n
hl_points <- which(diag(H)>t)
length(hl_points)
# We have 35 hl points: let's try to remove them
X_whl = X[-hl_points,]
X_whl = data.frame(X_whl)
X_whl['TPW.1'] = data[-hl_points,]$TPW.1
attach(X_whl)

mod.out.whl <- lm(TPW.1~UFE.1+BPC.1+NPA.1) 
summary(mod.out.whl)







####### Multiple linear regression full
attach(data)
reg.out <- lm(TPW.1~Round+FSP.1+DBF.1+UFE.1+BPC.1+NPA.1+FSP.2+FSW.2+SSW.2+
                ACE.2+WNR.2+BPC.2+BPW.2+NPA.2+NPW.2+TPW.2+STP+GND+CRT) 
summary(reg.out)

####### Best Subset Selection
dim(data)
reg.full <- regsubsets(TPW.1~Round+FSP.1+DBF.1+UFE.1+BPC.1+NPA.1+FSP.2+FSW.2+SSW.2+
                            ACE.2+WNR.2+BPC.2+BPW.2+NPA.2+NPW.2+TPW.2+STP+GND+CRT, 
                       nvmax = 19, method="exhaustive", data=data)
summary(reg.full)
reg.full.summary <- summary(reg.full)
reg.full.summary$rss
reg.full.summary$bic
# I want now to analyse graphically the models obtained:
par(mfrow=c(1,2))
#par(mar=c(5.1, 4.1, 4.1, 2.1))
# R2
plot(reg.full.summary$rsq, xlab="Number of Variables", ylab="R²", type="l")
which.max(reg.full.summary$rsq)
points(19,reg.full.summary$adjr2[19], col="red",cex=2,pch=20)
plot(reg.full, scale="r2")
# Here, we have plotted the RSS against the number of predictors considered in that model.
#  So, we would expect a decreasing function as it is.

# Adjusted R2
plot(reg.full.summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")
# Here, we have plotted the Adjusted R^2 against the number of predictors present in each model.
which.max(reg.full.summary$adjr2)
points(14,reg.full.summary$adjr2[14], col="red",cex=2,pch=20)
# Here, we have marked the point in which we have the highest Adjusted R^2. From the analysis
#  of this Adjusted R^2 we should choose the model with 14 covariates.
plot(reg.full, scale="adjr2")

# check assumptions of the model
par(mfrow=c(1,1))
plot(lm(TPW.1~FSP.1+UFE.1+BPC.1+NPA.1+FSP.2+FSW.2+SSW.2+
       ACE.2+BPW.2+NPA.2+NPW.2+TPW.2+STP+GND))


# Mallow's Cp
plot(reg.full.summary$cp,xlab="Number of Variables",ylab="Cp",type='l')
# Here, we have plotted the value of Cp for each best model
which.min(reg.full.summary$cp)
points(11,reg.full.summary$cp[11],col="red",cex=2,pch=20)
# and marked the lowest point, that corresponds to the model with 6 regressors.
plot(reg.full, scale="Cp")

# check assumptions of the model
par(mfrow=c(1,1))
plot(lm(TPW.1~BPC.1+FSP.2+FSW.2+SSW.2+
          ACE.2+BPW.2+NPA.2+NPW.2+TPW.2+STP+GND))

# BIC
plot(reg.full.summary$bic,xlab="Number of Variables",ylab="BIC",type='l')
which.min(reg.full.summary$bic)
points(8,reg.full.summary$bic[8],col="red",cex=2,pch=20)
# The BIC tends to select less complex model, thanks to its nature. Indeed, in this case the
#  BIC suggests that the best odel is the one with 8 predictors.
plot(reg.full, scale="bic")

# check assumptions of the model
par(mfrow=c(1,1))
plot(lm(TPW.1~BPC.1+ACE.2+BPW.2+NPA.1+NPW.2+TPW.2+STP+GND))




# compare linear regression models w/ lm using PCs (Not in Presentation)
### PCA with all predictive variables removed
predictive_cols = c(1, 3:5, 7:9, 11, 14, 16, 17, 22, 24)
data_nopred = data[-predictive_cols]

pca_nopred = prcomp(data_nopred, scale=T, retx = T)
summary(pca_nopred)

### PCA w/out predictive variables in linear regression 
reduced_data = as.data.frame(pca_nopred$x)[c('PC1', 'PC2', 'PC3')]
red_data_rows = rownames(reduced_data)
reduced_data['TPW.1'] = data[red_data_rows, 'TPW.1']
reduced_data['TPW.2'] = data[red_data_rows, 'TPW.2']

pca_lm_p1 = lm(data = reduced_data, TPW.1 ~ PC1 + PC2 + PC3)
summary(pca_lm_p1)

# we notice that the fourth principal component is not 
# significant when used in mult. regression









################LOGISTIC REGRESSION
n <-  dim(data)[1]

#Roc Curve function
roc_curve <- function(Result, predictions){
  roc.out <- roc(Result, predictions, levels=c(0,1))
  plot(roc.out)
  plot(roc.out, legacy.axes=TRUE)
  plot(roc.out, legacy.axes=TRUE, xlab="False positive rate", ylab="True positive rate")
  auc(roc.out) #with this I compute the area under the curve
  plot(roc.out,  print.auc=TRUE, legacy.axes=TRUE, xlab="False positive rate", ylab="True positive rate") #in this way I added also the information about the area under the curve  
}

# Precision and Recall:
prec_rec_acc <- function(Result, predictions){
  conf.matrix <- table(predictions, Result)
  print(conf.matrix)
  cat('Accuracy ', ((conf.matrix[2,2] + conf.matrix[1,1]) / (conf.matrix[2,2] + conf.matrix[1,1] + conf.matrix[2,1] + conf.matrix[1,2])), '\n')
  cat('Precision ', conf.matrix[2,2] / (conf.matrix[2,1] + conf.matrix[2,2]), '\n')
  cat('Recall ', conf.matrix[2,2]/(conf.matrix[1,2] + conf.matrix[2,2]) )
}


#BREAK POINTS CREATED (Not in Presentation)
plot(BPC.1+rnorm(n),BPC.2+rnorm(n), pch=20, col=c(Result+2,Result+3), cex=0.6, lwd=0.8,
     xlab="BPC.1", ylab="BPC.2", main='BPC.1 vs BPC.2')
legend(19,5, legend=c("Match Won by Player 2","Match Won by Player 1"), pch=20, col=2:3, cex=0.6)
#Break points created are clearly related to the winning of the match by a player.
#From tennis pov, indeed, break points are points that assign a game to a player, if won.
#So, the more a player creates break points, the more is also the probability to win them, the more
#is the probability to win the match.


log.reg.BPC <- glm(Result~BPC.1+BPC.2, family='binomial')
summary(log.reg.BPC)
#We can see that I obtain a model where both the covariates are significant. 

#Let's plot the separation line given by the log.reg. Here I set p=0.5
x <- seq(-10,200)
y <- -((coefficients(log.reg.BPC)[2])*x+coefficients(log.reg.BPC)[1])/(coefficients(log.reg.BPC)[3])
#y
lines(x,y, lwd=2, lty=3)

pred.BPC <- predict(log.reg.BPC, type='response')
pred.BPC <- round(pred.BPC, 0)


##create a vector of predictions (for the ROC curve)
pred.BPC.v <- rep(0,dim(data)[1])
pred.BPC.v[pred.BPC==1] <- 1
#pred.BPC.v 

#Let's see the confusion matrix of this model
conf.matrix <- table(pred.BPC, data$Result)
conf.matrix 

#Error rate
(conf.matrix[1,2]+conf.matrix[2,1])/dim(data)[1]
#Accuracy
(conf.matrix[1,1]+conf.matrix[2,2])/dim(data)[1]
#Giving to the model information just on the BPC, the model is able to predict well 82% of the cases.
#We can say that BPC is very significant in determining the winner of the match.

#Roc Curve
roc_curve(data$Result,pred.BPC.v)

# Precision, Recall
prec_rec_acc(data$Result,pred.BPC)



#BREAK POINTS WON
plot(BPW.1+rnorm(n),BPW.2+rnorm(n), pch=20, col=c(Result+2,Result+3), cex=0.6, lwd=0.8,
     xlab="BPW.1", ylab="BPW.2", main="BPW.1 vs BPW.2")
legend(8,0, legend=c("Match Won by Player 2","Match Won by Player 1"), pch=20, col=2:3, cex=0.6)
#Same discussion made on break points created..


log.reg.BPW <- glm(Result~BPW.1+BPW.2, family='binomial')
summary(log.reg.BPW)
#We can see that I obtain a model where both the covariates are significant. 

#Let's plot the separation line given by the log.reg. Here I set p=0.5
x <- seq(-10,200)
y <-  -((coefficients(log.reg.BPW)[2])*x+coefficients(log.reg.BPW)[1])/(coefficients(log.reg.BPW)[3])
#y
lines(x,y, lwd=2, lty=3)

pred.BPW <- predict(log.reg.BPW, type='response')
pred.BPW <- round(pred.BPW, 0)
#pred.BPW
#create a vector of predictions
pred.BPW.v <- rep(0,dim(data)[1])
pred.BPW.v[pred.BPW==1] <- 1
#pred.BPW.v 

#Let's see the confusion matrix of this model
conf.matrix <- table(pred.BPW, data$Result)
conf.matrix 

#Error rate
(conf.matrix[1,2]+conf.matrix[2,1])/dim(data)[1]
#Accuracy
(conf.matrix[1,1]+conf.matrix[2,2])/dim(data)[1]
#Giving to the model information just on the BPC, the model is able to predict well 94% of the cases.
#We can say that BPC is very significant in determining the winner of the match.

#Roc Curve
roc_curve(data$Result, pred.BPW.v)

# Precision, Recall
prec_rec_acc(data$Result, pred.BPW)


#TOTAL POINTS WON
plot(TPW.1+rnorm(n), TPW.2+rnorm(n), pch=20, col=c(Result+2,Result+3), cex=0.6, lwd=0.8,
     xlab="TPW.1", ylab="TPW.2", main="TPW.1 vs TPW.2")
legend(170,50, legend=c("Match won by Player 2","Match won by Player 1"), pch=20, col=2:3, cex=0.6)
#Same discussion made on break points created..


log.reg.TPW <- glm(Result~TPW.1+TPW.2, family='binomial')
summary(log.reg.TPW)
#We can see that I obtain a model where both the covariates are significant. 

#Let's plot the separation line given by the log.reg. Here I set p=0.5
x <- seq(-10,300)
y <-  -((coefficients(log.reg.TPW)[2])*x+coefficients(log.reg.TPW)[1])/(coefficients(log.reg.TPW)[3])
#y
lines(x,y, lwd=2, lty=3)

pred.TPW <- predict(log.reg.TPW, type='response')
pred.TPW <- round(pred.TPW, 0)
#create a vector of predictions
pred.TPW.v <- rep(0,dim(data)[1])
pred.TPW.v[pred.TPW==1] <- 1

#Let's see the confusion matrix of this model
conf.matrix <- table(pred.TPW, data$Result)
conf.matrix 

#Error rate
(conf.matrix[1,2]+conf.matrix[2,1])/dim(data)[1]
#Accuracy
(conf.matrix[1,1]+conf.matrix[2,2])/dim(data)[1]
#Giving to the model information just on the BPC, the model is able to predict well 94% of the cases.
#We can say that BPC is very significant in determining the winner of the match.

#Roc Curve
roc_curve(data$Result,pred.TPW.v)

# Precision, Recall
prec_rec_acc(data$Result, pred.TPW)


#UNFORCED ERRORS 
plot(UFE.1+rnorm(n),UFE.2+rnorm(n), pch=20, col=c(Result+2,Result+3), cex=0.6, lwd=0.8,
     xlab="UFE.1", ylab="UFE.2", main="UFE.1 vs UFE.2")
legend(65,24, legend=c("Match Won by Player 2","Match Won by Player 1"), pch=20, col=2:3, cex=0.6)
#As we expected, the number of unforced error is quite significant in determining the winner of the match.
#Indeed unforced errors behave in a similar way to winner points, but obviously in the other way around:
#indeed we can notice that, if with winner points we had that the more Player1 had a higher score
#in winner points, the more he won, (red points (player1 win) were above the green points) here we have the countrary:
#the more for example Player1 makes unforced error, the more Player2 wins the match. Indeed, the colours 
#here are reversed.

log.reg.UFE <- glm(Result~UFE.1+UFE.2, family='binomial')
summary(log.reg.UFE)
#We can see that I obtain a model where both the covariates are significant. 

#Let's plot the separation line given by the log.reg. Here I set p=0.5
x <- seq(0,200)
y <-  -((coefficients(log.reg.UFE)[2])*x+coefficients(log.reg.UFE)[1])/(coefficients(log.reg.UFE)[3])
#y
lines(x,y, lwd=2, lty=3)

pred.UFE <- predict(log.reg.UFE, type='response')
pred.UFE <- round(pred.UFE, 0)
#pred.UFE
#create a vector of predictions
pred.UFE.v <- rep(0,dim(data)[1])
pred.UFE.v[pred.UFE==1] <- 1
#pred.UFE.v 

#Let's see the confusion matrix of this model
conf.matrix <- table(pred.UFE, data$Result)
conf.matrix 

#Error rate
(conf.matrix[1,2]+conf.matrix[2,1])/dim(data)[1]
#Accuracy
(conf.matrix[1,1]+conf.matrix[2,2])/dim(data)[1]
#Giving to the model information just on the UFE, the model is able to predict well 70% of the cases.
#We can say that UFE is quite significant in determining the winner of the match.
#We obtain, as expected, a similar result to WNR.

#Roc Curve
roc_curve(data$Result,pred.UFE.v)

# Precision, Recall
prec_rec_acc(data$Result, pred.UFE)



#DOUBLE FAULTS COMMITTED
plot(DBF.1+rnorm(n),DBF.2+rnorm(n), pch=20, col=c(Result+2,Result+3), cex=0.6, lwd=0.8,
     xlab="DBF.1", ylab="DBF.2", main='DBF.1 vs DBF.2')
legend(12,17, legend=c("Match Won by Player 2","Match Won by Player 1"), pch=20, col=2:3, cex=0.6)
#Double faults committed is not clearly related to the winning of the match by a certain player.
#We can indeed say that the number of double faults is something that is quite equally distributed
#among the players. So we can't have a clear separation of winner and loser players.


log.reg.DBF <- glm(Result~DBF.1+DBF.2, family='binomial')
summary(log.reg.DBF)
#We can see that I obtain a model where both the covariates are significant. 

#Let's plot the separation line given by the log.reg. Here I set p=0.5
x <- seq(-10,200)
y <- -((coefficients(log.reg.DBF)[2])*x+coefficients(log.reg.DBF)[1])/(coefficients(log.reg.DBF)[3])
#y
lines(x,y, lwd=2, lty=3)

pred.DBF <- predict(log.reg.DBF, type='response')
pred.DBF <- round(pred.DBF, 0)
#pred.DBF
#create a vector of predictions
pred.DBF.v <- rep(0,dim(data)[1])
pred.DBF.v[pred.DBF==1] <- 1
#pred.DBF.v 

#Let's see the confusion matrix of this model
conf.matrix <- table(pred.DBF, data$Result)
conf.matrix 

#Error rate
(conf.matrix[1,2]+conf.matrix[2,1])/dim(data)[1]
#Accuracy
(conf.matrix[1,1]+conf.matrix[2,2])/dim(data)[1]
#Giving to the model information just on the DBF, the model is able to predict well 56% of the cases.
#Indeed I can see also from the plot that DBF is not a strong information to predict the winner of the match.


#Roc Curve
roc_curve(data$Result,pred.DBF.v)

# Precision, Recall
prec_rec_acc(data$Result, pred.DBF)



#FIRST SERVE PERCENTAGE
plot(FSP.1+rnorm(n),FSP.2+rnorm(n), pch=20, col=c(Result+2,Result+3), cex=0.6, lwd=0.8,
     xlab="FSP.1", ylab="FSP.2", main='FSP.1 vs FSP.2')
legend(70,45, legend=c("Match Won by Player 2","Match Won by Player 1"), pch=20, col=2:3, cex=0.6)
#Double faults committed is not clearly related to the winning of the match by a certain player.
#We can indeed say that the number of double faults is something that is quite equally distributed
#among the players. So we can't have a clear separation of winner and loser players.

log.reg.FSP <- glm(Result~FSP.1+FSP.2, family='binomial')
summary(log.reg.FSP)
#We can see that I obtain a model where both the covariates are significant. 

#Let's plot the separation line given by the log.reg. Here I set p=0.5
x <- seq(-10,200)
y <- -((coefficients(log.reg.FSP)[2])*x+coefficients(log.reg.FSP)[1])/(coefficients(log.reg.FSP)[3])
#y
lines(x,y, lwd=2, lty=3)

pred.FSP <- predict(log.reg.FSP, type='response')
pred.FSP <- round(pred.FSP, 0)
#pred.FSP
#create a vector of predictions
pred.FSP.v <- rep(0,dim(data)[1])
pred.FSP.v[pred.DBF==1] <- 1
pred.FSP.v 

#Let's see the confusion matrix of this model
conf.matrix <- table(pred.FSP, data$Result)
conf.matrix 

#Error rate
(conf.matrix[1,2]+conf.matrix[2,1])/dim(data)[1]
#Accuracy
(conf.matrix[1,1]+conf.matrix[2,2])/dim(data)[1]
#Giving to the model information just on the DBF, the model is able to predict well 56% of the cases.
#Indeed I can see also from the plot that DBF is not a strong information to predict the winner of the match.

#Roc Curve
roc_curve(data$Result, pred.FSP.v)

# Precision, Recall
prec_rec_acc(data$Result, pred.FSP)








#Logistic Regression on the full dataset
################
######FULL MODEL
################

#Let's try to predict the Result of the match using all the variables
full.model <- glm(Result~., data = data, family = binomial) 
#If I try to use all the variables the algorithm doesn't converge


#Remove FNL.1,FNL.2,Match
data <- subset(data, select=-c(FNL.1,FNL.2,Match))
mod.noFNL <- glm(Result~., data = data, family = binomial) 
summary(mod.noFNL)

#Let's analyze this model 
predictions <- predict(mod.noFNL, type='response')
predictions <- round(predictions, 0)
predictions
#create a vector of predictions
predictions.v <- rep(0,436)
predictions.v[predictions==1] <- 1
predictions.v 

#Let's see the confusion matrix of this model
conf.matrix <- table(predictions, Result)
conf.matrix  
#As I can see the model is able to predict quite perfectly the result of the match.

#Error rate
(conf.matrix[1,2]+conf.matrix[2,1])/436
#Accuracy
(conf.matrix[1,1]+conf.matrix[2,2])/436

#Roc Curve
roc_curve(data$Result,predictions.v)




####### Stepwise Backward Variable Selection
# The saturated model is still contained in reg.out:
summary(mod.noFNL)

step.mod <- step(mod.noFNL, steps=28, trace=0, direction="backward")
# We are choosing the backward stepwise selection, so we will start from the full model.
#  The argument steps defines after how many backward steps you have to stop, the maximum
#  number of steps is of course p(total number of covariates)
#  The argumnet trace defines the type of output: with trace=0 we have the classical one
summary(step.mod)
# This summary refers to the more detailed algo.

# With trace=1 the output is a far more detailed output:
step.mod <- step(mod.noFNL, steps=28,  trace=1, direction="backward")
# Let's analyse the first scheme:
#  we have starting AIC = 1802.1. The scheme represents the value of RSS and AIC for the 
#  neighbors of the full model, that are all the models with p-1 predictors. We choose the
#  best one among these and we proceed. The best one, that is the one with lowest AIC is the 
#  one without WNR.2. And we proceed like that for ten steps. We consider just the 
#  neighbors of the model chosen in the previous step.

# LOOCV on final model
l <- c()

for (i in 1:dim(data)[1]){
  test <- data[i,]
  prediction = predict(step.mod, test[, -Result], type="response")
  l <- c(l,prediction)
}
predictions = round(l, 0)
accuracy <- sum(predictions==data$Result)/dim(data)[1]
print(accuracy)

# ROC curve
roc_curve(Result, predictions)

# Precision, Recall
prec_rec_acc(Result, predictions)









## KNN
library(class)
set.seed(127)
attach(data)
data = subset(data, select = -c(Match, FNL.1, FNL.2))
attach(data)
########################################
#Leave-one-out cross validation
########################################
alt <- seq(1, 150, by=3)
acc <- list()
for (k in alt) {
  l <- c()
  for (i in 1:dim(data)[1]){
    train <- data[-i,]
    test <- data[i,]
    prediction <- knn(train[, -Result], test[, -Result], train$Result, k=k)
    l <- c(l,prediction)
  }
  predictions <- l - 1
  accuracy <- sum(predictions==data$Result)/dim(data)[1]
  acc <- c(acc, accuracy)
}

# Quali sono i valori di k che producono risultati migliori?
m <- matrix(acc, length(alt), 1)
rownames(m) <- alt
m

max(unlist(m))
sum(m == max(unlist(m)))
# Due massimi:
rownames(m)[m == max(unlist(m))]

acc_df <- data.frame(unlist(m), row.names = alt)
colnames(acc_df) <- 'acc'
attach(acc_df)

# Using the best k, we store the predictions:
k_best <- min(as.integer(rownames(m)[m == max(unlist(m))]))
l <- c()
for (i in 1:dim(data)[1]){
  train <- data[-i,]
  test <- data[i,]
  prediction <- knn(train[, -Result], test[, -Result], 
                    train$Result, k=k_best)
  l <- c(l,prediction)
}
predictions <- l - 1

#confusion matrix
conf.matrix <- table(predictions, data$Result)
conf.matrix

#roc_curve
roc_curve(data$Result, predictions)

# Precision, Recall
prec_rec_acc(Result, predictions)




### kNN on Normalized and Standardized data
no_factor_data = data[-c(1, 2, 27, 28, 29)]
col_means = colMeans(no_factor_data)
col_mins = apply(no_factor_data, 2, min)
col_ranges = apply(no_factor_data, 2, range)
ranges = col_ranges[2,] - col_ranges[1,]
col_std = apply(no_factor_data, 2, sd)

cent_data = no_factor_data
for (i in rownames(cent_data)) {
  cent_data[i, ] = cent_data[i, ] - col_means
}

norm_data = no_factor_data
for (i in rownames(norm_data)) {
  norm_data[i, ] = (norm_data[i, ] - col_mins) / ranges
}

std_data = cent_data
for (i in rownames(cent_data)) {
  std_data[i, ] = cent_data[i, ] / col_std
}  

cent_data$Result = data$Result
norm_data$Result = data$Result
std_data$Result = data$Result

LOOCV_knn = function(input_data) {
  attach(input_data)
  alt <- seq(1, 150, by=3)
  acc <- list()
  for (k in alt) {
    l <- c()
    for (i in 1:dim(input_data)[1]){
      train <- input_data[-i, ]
      train_res = train$Result
      train = subset(train, select = -c(Result))
      test <- input_data[i,]
      test = subset(test, select = -c(Result))
        
      prediction <- knn(train, 
                        test, 
                        train_res, k=k)
      l <- c(l, prediction)
    }
    predictions <- l - 1
    accuracy <- sum(predictions==input_data$Result) / dim(input_data)[1]
    acc <- c(acc, accuracy)
  }
  detach(input_data)
  return(acc)
}

loocv_res = LOOCV_knn(cent_data) # centered data
loocv_res = LOOCV_knn(norm_data) # normalised data
loocv_res = LOOCV_knn(std_data) # standardized data

acc = loocv_res

# Quali sono i valori di k che producono risultati migliori?
m <- matrix(acc, length(alt), 1)
rownames(m) <- alt
m

max(unlist(m))
sum(m == max(unlist(m)))
rownames(m)[m == max(unlist(m))]

acc_df <- data.frame(unlist(m), row.names = alt)
colnames(acc_df) <- 'acc'
attach(acc_df)
detach(acc_df)

# Using the best k, we store the predictions:
k_best <- min(as.integer(rownames(m)[m == max(unlist(m))]))
l <- c()
LOOCV_knn_kbest = function(input_data, k_best) {
  attach(input_data)
  
  for (i in 1:dim(input_data)[1]){
    train <- input_data[-i, ]
    train_res = train$Result
    train = subset(train, select = -c(Result))
    test <- input_data[i,]
    test = subset(test, select = -c(Result))
    
    prediction <- knn(train, test, 
                      train_res, k=k_best)
    l <- c(l, prediction)
  }
  
  detach(input_data)
  return(l)
}

l = LOOCV_knn_kbest(cent_data, k_best)
l = LOOCV_knn_kbest(norm_data, k_best)
l = LOOCV_knn_kbest(std_data, k_best)

predictions <- l - 1

#confusion matrix
conf.matrix <- table(predictions, norm_data$Result)
conf.matrix

# Precision, Recall
prec_rec_acc(norm_data$Result, predictions)









######## 5 Tennis intepretation

### Playing style: influence of gender on ACEM
### PC1 vs PC3 
plot_c3 = abs(loadings_c3) >= 0.15
variables2plot = colnames(data_pca)[plot_c1 | plot_c3]
variables2plot = c('ACE.1', 'ACE.2', 'GND', 'STP')

fviz_pca_biplot(pca1, 
                axes = c(1,3),
                label='var', 
                geom.ind = 'point',
                habillage = data$GND,
                select.var=list(name=variables2plot),
                col.ind = 'grey',
                repel = T,
                legend.title = "Gender",
                title = "Biplot - PC1 vs PC3") + 
  scale_shape_manual(values=c(0, 1))

## Conditioning on gender
### BOXPLOT of normalized ACEs conditioned on gender
ggplot(data, aes(y = ACEM/STP, fill = as.factor(GND), x = GND)) +
  theme_bw() +
  geom_boxplot() +
  scale_fill_brewer(palette="Dark2", name= 'GND', labels = c('F', 'M')) +
  ggtitle('Boxplot - ACE/STP vs GND')

# Test to compare # aces
data['ACEM'] = ACE.1 + ACE.2
attach(data)

var.test(ACEM/STP~GND)
#Eteroschedasticity
t.test(ACEM/STP~GND, equal.var=FALSE, alternative='l')



# Test to compare TPW means between male and females matches. (Not in Presentation)
# We assume that TPW mean is normally distributed thanks to the CLT.
data['TPM'] = TPW.1 + TPW.2
attach(data)
norm_male_pts = data[GND==1,]$TPM/data[GND==1,]$STP
norm_female_pts = data[GND==0,]$TPM/data[GND==0,]$STP

var.test(norm_male_pts, norm_female_pts)
# we reject H0: heteroschedasticity
t.test(norm_male_pts, norm_female_pts, equal.var=F)



### BOXPLOT of normalized FSP conditioned on gender
data['FSPM'] = (FSP.1 + FSP.2)/2
attach(data)
ggplot(data, aes(y = FSPM/STP, fill = as.factor(GND), x = GND)) +
  theme_bw() +
  geom_boxplot() +
  scale_fill_brewer(palette="Dark2", name= 'GND', labels = c('F', 'M')) +
  ggtitle('Boxplot - FSP/STP vs GND') +
  ylim(min(FSPM/STP), max(FSPM/STP))

var.test(FSPM/STP~GND)
#Eteroschedasticity
t.test(FSPM/STP~GND, equal.var=FALSE, alternative='g')


### BOXPLOT of normalized FSW conditioned on gender
data['FSWM'] = (FSW.1 + FSW.2)
data['SSWM'] = (SSW.1 + SSW.2)
attach(data)
ggplot(data, aes(y = FSWM/STP, fill = as.factor(GND), x = GND)) +
  theme_bw() +
  geom_boxplot() +
  scale_fill_brewer(palette="Dark2", name= 'GND', labels = c('F', 'M')) +
  ggtitle('Boxplot - FSW/STP vs GND') +
  ylim(min(SSWM/STP), max(FSWM/STP))

var.test(FSWM/STP~GND)
#Homoschedasticity
t.test(FSWM/STP~GND, equal.var=TRUE, alternative='l')

### BOXPLOT of normalized SSW conditioned on gender
ggplot(data, aes(y = SSWM/STP, fill = as.factor(GND), x = GND)) +
  theme_bw() +
  geom_boxplot() +
  scale_fill_brewer(palette="Dark2", name= 'GND', labels = c('F', 'M')) +
  ggtitle('Boxplot - SSW/STP vs GND') +
  ylim(min(SSWM/STP), max(FSWM/STP))

var.test(SSWM/STP~GND)
#Homoschedasticity
t.test(SSWM/STP~GND, equal.var=TRUE, alternative='l')



### BOXPLOT of normalized NPA conditioned on gender
data['NPAM'] = (NPA.1 + NPA.2)
attach(data)
ggplot(data, aes(y = NPAM/STP, fill = as.factor(GND), x = GND)) +
  theme_bw() +
  geom_boxplot() +
  scale_fill_brewer(palette="Dark2", name= 'GND', labels = c('F', 'M')) +
  ggtitle('Boxplot - NPA/STP vs GND')  +
  ylim(min(NPAM/STP), max(NPAM/STP))

#NPAM/STP~GND
var.test(NPAM/STP~GND)
#Homoschedasticity
t.test(NPAM/STP~GND, equal.var=TRUE, alternative='l')


  
### BOXPLOT of normalized NPW conditioned on gender
data['NPWM'] = (NPW.1 + NPW.2)
attach(data)
ggplot(data, aes(y = NPWM/STP, fill = as.factor(GND), x = GND)) +
  theme_bw() +
  geom_boxplot() +
  scale_fill_brewer(palette="Dark2", name= 'GND', labels = c('F', 'M')) +
  ggtitle('Boxplot - NPW/STP vs GND')  +
  ylim(min(NPAM/STP), max(NPAM/STP))

var.test(NPWM/STP~GND)
#Homoschedasticity
t.test(NPWM/STP~GND, equal.var=TRUE, alternative='l')




### BOXPLOT of normalized BPC conditioned on gender
data['BPCM'] = (BPC.1 + BPC.2)
attach(data)
ggplot(data, aes(y = BPCM/STP, fill = as.factor(GND), x = GND)) +
  theme_bw() +
  geom_boxplot() +
  scale_fill_brewer(palette="Dark2", name= 'GND', labels = c('F', 'M')) +
  ggtitle('Boxplot - BPC/STP vs GND')  +
  ylim(min(BPCM/STP), max(BPCM/STP))

var.test(BPCM/STP~GND)
#Eteroschedasticity
t.test(BPCM/STP~GND, equal.var=FALSE, alternative='g')




### BOXPLOT of normalized BPW conditioned on gender
data['BPWM'] = (BPW.1 + BPW.2)
attach(data)
ggplot(data, aes(y = BPWM/STP, fill = as.factor(GND), x = GND)) +
  theme_bw() +
  geom_boxplot() +
  scale_fill_brewer(palette="Dark2", name= 'GND', labels = c('F', 'M')) +
  ggtitle('Boxplot - BPW/STP vs GND')  +
  ylim(min(BPWM/STP), max(BPWM/STP))

var.test(BPWM/STP~GND)
#Eteroschedasticity
t.test(BPWM/STP~GND, equal.var=FALSE, alternative='g')



### BOXPLOTS on Rounds influence
data_wim = read.csv(filepath, header = T)
attach(data_wim)
data_wim = data_wim[STP!=0,]
data_wim = subset(data_wim, select = -c(X, ST1.1, ST2.1, ST3.1, ST4.1, ST5.1, 
                                ST1.2, ST2.2, ST3.2, ST4.2, ST5.2))
data_wim = subset(data_wim, select = -c(SSP.1, SSP.2))
data_wim[, 1] = paste(data_wim$Player1, data_wim$Player2, sep='/')
data_wim = subset(data_wim, select = -c(Player2))
colnames(data_wim)[1] = 'Match'

attach(data_wim)

count_notna = function(data, column) {
  print(sum(!is.na(data[data$CRT == 0, ][, column])))
  print(sum(!is.na(data[data$CRT == 1, ][, column])))
  print(sum(!is.na(data[data$CRT == 2, ][, column])))
}

data_wim['ACEM'] = ACE.1 + ACE.2
attach(data_wim)
count_notna(data_wim, "ACEM")
ggplot(data_wim, aes(y = ACEM/STP, fill = as.factor(Round), x = Round)) +
  theme_bw() +
  geom_boxplot() +
  scale_fill_brewer(palette="Dark2", name= 'Round', labels = c(1:7)) +
  ggtitle('Boxplot - ACE/STP vs Round')

ggplot(data_wim, aes(y = ACEM/STP, fill = as.factor(CRT), x = CRT)) +
  theme_bw() +
  geom_boxplot() +
  scale_fill_brewer(palette="Dark2", name= 'Court', labels = c('Hard', 'Grass', 'Clay')) +
  ggtitle('Boxplot - ACE/STP vs Court')

# Anova? Checking assumptions
#Test on the mean --> Round
#ACEM/STP
attach(data_wim)
data_wim.ACEM <- data_wim[complete.cases(data_wim$ACEM),]
data_wim.ACEM <- data_wim.ACEM[complete.cases(data_wim.ACEM$Round),]
data_wim.ACEM$Round <- as.factor(Round)
attach(data_wim.ACEM)
#Normality test
for (i in seq(1:7)){
  selected_round = data_wim.ACEM[Round==i,]
  cat('Round ',i)
  print(shapiro.test(selected_round$ACEM/selected_round$STP))
}
#Variance test
bartlett.test(ACEM/STP~Round)


#Test on the mean --> Court
#ACEM/STP
attach(data_wim)
data_wim.ACEM <- data_wim[complete.cases(data_wim$ACEM),]
data_wim.ACEM <- data_wim.ACEM[complete.cases(data_wim.ACEM$CRT),]
attach(data_wim.ACEM)
data_wim.ACEM$CRT <- as.factor(data_wim.ACEM$CRT)
attach(data_wim.ACEM)
#Normality test
for (i in c(0,1,2)){
  selected_crt = data_wim.ACEM[data_wim.ACEM$CRT==i,]
  cat('Court ',i)
  print(shapiro.test(selected_crt$ACEM/selected_crt$STP))
}
#Variance test
attach(data_wim.ACEM)
bartlett.test(ACEM/STP~data_wim.ACEM$CRT)
 
# Not in Presentation
data_wim['DBFM'] = (DBF.1 + DBF.2)
attach(data_wim)
count_notna(data_wim, "DBFM")
ggplot(data_wim, aes(y = DBFM/STP, fill = as.factor(Round), x = Round)) +
  theme_bw() +
  geom_boxplot() +
  scale_fill_brewer(palette="Dark2", name= 'Round', labels = c(1:7)) +
  ggtitle('Boxplot - DBF/STP vs Round')

ggplot(data_wim, aes(y = DBFM, fill = as.factor(CRT), x = CRT)) +
  theme_bw() +
  geom_boxplot() +
  scale_fill_brewer(palette="Dark2", name= 'Court', labels = c('Hard', 'Grass', 'Clay')) +
  ggtitle('Boxplot - DBF vs Court')


data_wim['NPAM'] = NPA.1 + NPA.2
attach(data_wim)
count_notna(data_wim, "NPAM")

ggplot(data_wim, aes(y = NPAM/STP, fill = as.factor(Round), x = Round)) +
  theme_bw() +
  geom_boxplot() +
  scale_fill_brewer(palette="Dark2", name= 'Round', labels = c(1:7)) +
  ggtitle('Boxplot - NPA/STP vs Round')

ggplot(data_wim, aes(y = NPAM/STP, fill = as.factor(CRT), x = CRT)) +
  theme_bw() +
  geom_boxplot() +
  scale_fill_brewer(palette="Dark2", name= 'Court', labels = c('Hard', 'Grass', 'Clay')) +
  ggtitle('Boxplot - NPA/STP vs Court')

data_wim['UFEM'] = (UFE.1 + UFE.2)
attach(data_wim)
count_notna(data_wim, "UFEM")
ggplot(data_wim, aes(y = UFEM/STP, fill = as.factor(Round), x = Round)) +
  theme_bw() +
  geom_boxplot() +
  scale_fill_brewer(palette="Dark2", name= 'Round', labels = c(1:7)) +
  ggtitle('Boxplot - UFE/STP vs Round')

ggplot(data_wim, aes(y = UFEM/STP, fill = as.factor(CRT), x = CRT)) +
  theme_bw() +
  geom_boxplot() +
  scale_fill_brewer(palette="Dark2", name= 'Court', labels = c('Hard', 'Grass', 'Clay')) +
  ggtitle('Boxplot - UFE/STP vs Court')

# Not in Presentation
data_wim['WNRM'] = (WNR.1 + WNR.2)
attach(data_wim)
count_notna(data_wim, "WNRM")

ggplot(data_wim, aes(y = WNRM/STP, fill = as.factor(Round), x = Round)) +
  theme_bw() +
  geom_boxplot() +
  scale_fill_brewer(palette="Dark2", name= 'Round', labels = c(1:7)) +
  ggtitle('Boxplot - WNR/STP vs Round')

ggplot(data_wim, aes(y = WNRM/STP, fill = as.factor(CRT), x = CRT)) +
  theme_bw() +
  geom_boxplot() +
  scale_fill_brewer(palette="Dark2", name= 'Court', labels = c('Hard', 'Grass', 'Clay')) +
  ggtitle('Boxplot - WNR/STP vs Court')
  

data_wim['SSWM'] = (SSW.1 + SSW.2)
attach(data_wim)
count_notna(data_wim, "SSWM")

ggplot(data_wim, aes(y = SSWM/STP, fill = as.factor(Round), x = Round)) +
  theme_bw() +
  geom_boxplot() +
  scale_fill_brewer(palette="Dark2", name= 'Round', labels = c(1:7)) +
  ggtitle('Boxplot - SSW/STP vs Round')

ggplot(data_wim, aes(y = SSWM/STP, fill = as.factor(CRT), x = CRT)) +
  theme_bw() +
  geom_boxplot() +
  scale_fill_brewer(palette="Dark2", name= 'Court', labels = c('Hard', 'Grass', 'Clay')) +
  ggtitle('Boxplot - SSW/STP vs Court')





