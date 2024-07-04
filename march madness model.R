
#conservative alpha level for sample size: 0.01


#######################################################################
########## Loading in the library #####################################
########################################################################
library(tidyverse)
library(bookdown)
library(ggplot2)
library(car)
library(corrplot)
library(Hmisc)
library(readxl)
library(caret)
library(glmnet)
library(nortest)
library(MASS)
library(forcats)
library(gmodels)
library(vcdExtra)
library(mgcv)
library(DescTools)
library(ROCit)
library(randomForest)
library(xgboost)
library(smbinning)
library(Ckmeans.1d.dp)
library(e1071)
library(klaR)
library(earth)
#######################################################################
########## Reading in the dataset #####################################
########################################################################

mm_data <- read_excel("march_madness.xlsx", sheet = 2)

#only want 2008-2023
mm_data <- mm_data %>%
  dplyr::filter(!(Year %in% 2000:2007))


########################################################################
########## Paired t-tests for Pre/Post tourney stats #####################################
########################################################################

#FGPct - no diff
shapiro.test(with(mm_data, mm_data$Diff_FGPct - mm_data$Diff_FGPct_PRE))
wilcox.test(mm_data$Diff_FGPct, mm_data$Diff_FGPct_PRE, paired = TRUE, alternative = 'two.sided')

#3ptPct - no diff
shapiro.test(with(mm_data, mm_data$Diff_3ptPct - mm_data$Diff_3ptPct_PRE))
wilcox.test(mm_data$Diff_3ptPct, mm_data$Diff_3ptPct_PRE, paired = TRUE, alternative = 'two.sided')

#FTPct - sig diff b/w the two
shapiro.test(with(mm_data, mm_data$Diff_FTPct - mm_data$Diff_FTPct_PRE))
wilcox.test(mm_data$Diff_FTPct, mm_data$Diff_FTPct_PRE, paired = TRUE, alternative = 'two.sided')

#TRB - sig diff b/w the two
shapiro.test(with(mm_data, mm_data$Diff_TRB - mm_data$Diff_TRB_PRE))
wilcox.test(mm_data$Diff_TRB, mm_data$Diff_TRB_PRE, paired = TRUE, alternative = 'two.sided')

#AST - sig diff b/w the two
shapiro.test(with(mm_data, mm_data$Diff_AST - mm_data$Diff_AST_PRE))
wilcox.test(mm_data$Diff_AST, mm_data$Diff_AST_PRE, paired = TRUE, alternative = 'two.sided')

#STL - sig diff b/w the two
shapiro.test(with(mm_data, mm_data$Diff_STL - mm_data$Diff_STL_PRE))
wilcox.test(mm_data$Diff_STL, mm_data$Diff_STL_PRE, paired = TRUE, alternative = 'two.sided')

#BLK- sig diff b/w the two
shapiro.test(with(mm_data, mm_data$Diff_BLK - mm_data$Diff_BLK_PRE))
wilcox.test(mm_data$Diff_BLK, mm_data$Diff_BLK_PRE, paired = TRUE, alternative = 'two.sided')

#TOV - sig diff b/w the two
shapiro.test(with(mm_data, mm_data$Diff_TOV - mm_data$Diff_TO_PRE))
wilcox.test(mm_data$Diff_TOV, mm_data$Diff_TO_PRE, paired = TRUE, alternative = 'two.sided')

#PF - sig diff b/w the two
shapiro.test(with(mm_data, mm_data$Diff_PF - mm_data$Diff_PF_PRE))
wilcox.test(mm_data$Diff_PF, mm_data$Diff_PF_PRE, paired = TRUE, alternative = 'two.sided')

#PPG - sig diff b/w the two
shapiro.test(with(mm_data, mm_data$Diff_PPG - mm_data$Diff_PPG_PRE))
wilcox.test(mm_data$Diff_PPG, mm_data$Diff_PPG_PRE, paired = TRUE, alternative = 'two.sided')

#OppFGPct - sig diff b/w the two
shapiro.test(with(mm_data, mm_data$Diff_OppFGPct - mm_data$Diff_OppFGPct_PRE))
wilcox.test(mm_data$Diff_OppFGPct, mm_data$Diff_OppFGPct_PRE, paired = TRUE, alternative = 'two.sided')

#OppPPG - sig diff b/w the two
shapiro.test(with(mm_data, mm_data$Diff_OppPPG - mm_data$Diff_OppPPG_PRE))
wilcox.test(mm_data$Diff_OppPPG, mm_data$Diff_OppPPG_PRE, paired = TRUE, alternative = 'two.sided')

#WLPct - sig diff b/w the two
shapiro.test(with(mm_data, mm_data$Diff_WLPct - mm_data$Diff_WLPct_PRE))
wilcox.test(mm_data$Diff_WLPct, mm_data$Diff_WLPct_PRE, paired = TRUE, alternative = 'two.sided')

#Pace - no diff
shapiro.test(with(mm_data, mm_data$Diff_Pace - mm_data$Diff_Pace_PRE))
wilcox.test(mm_data$Diff_Pace, mm_data$Diff_Pace_PRE, paired = TRUE, alternative = 'two.sided')

#eFGPct - no diff
shapiro.test(with(mm_data, mm_data$Diff_eFGPct - mm_data$Diff_eFGPct_PRE))
wilcox.test(mm_data$Diff_eFGPct, mm_data$Diff_eFGPct_PRE, paired = TRUE, alternative = 'two.sided')


#####################################################################
############## Removing post-MM columns + train / test split
#####################################################################


######## convert MM round to factor
#mm_data$Round <- as.factor(mm_data$Round) - not using Round as predictor
#mm_data$Winner_A <- as.factor(mm_data$Winner_A) - can't factor for smbinning

mm_data$Year <- as.factor(mm_data$Year)
mm_data$TeamA <- as.factor(mm_data$TeamA)
mm_data$TeamB <- as.factor(mm_data$TeamB)
mm_data$A_Seed <- as.factor(mm_data$A_Seed)
mm_data$B_Seed <- as.factor(mm_data$B_Seed)

#creating weights for earlier rounds in the want to get these right
mm_data <- mm_data %>%
  mutate(wts = case_when(
    Round == 'Round64' ~ 320,
    Round == 'Round32' ~ 160,
    Round == 'Sweet16' ~ 80,
    Round == 'Elite8' ~ 40,
    Round == 'Final4' ~ 20,
    Round == 'Championship' ~ 10,
    TRUE ~ 0  # Default condition, if none of the above conditions are met
  ))

train <- mm_data[mm_data$Year != 2023,] #2000 - 2022 tournaments
valid <- mm_data[mm_data$Year == 2023,] #2023 tournament

train <- as.data.frame(train)
valid <- as.data.frame(valid) #this helps SMBinning work

conditional2023 <- read_excel("march_madness.xlsx", sheet = 5)

#####################################################################
############## EDA
#####################################################################

str(train)
summary(train)
sum(is.na(train))

####### Plot visuals of data
ggplot(data = train, aes(x=Winner_A)) +
  geom_histogram(stat = 'count') +
  theme_classic()  +
  labs(x = "Higher Seed Wins", y = "Count", title = "Count of Games Won by Higher Seed")

ggplot(data = train, aes(x=fct_infreq(Round))) +
  geom_bar() +
  theme_classic() +
  labs(x = "March Madness Round", y = "Count", title = "Count of Games Played by Round")

allTeams <- c(unlist(mm_data$TeamA),unlist(mm_data$TeamB))

barplot(sort(table(allTeams), decreasing = TRUE)[1:10],
        main = 'Top 10 Teams to Play in March Madness (2008-2023)',
        xlab = 'Teams',
        ylab = 'Total Games Played')

######## histograms of all variables 

var.names <- colnames(train)[c(11:44)]
for (i in var.names) {
  hist(train[[i]], xlab = i)
}


########## correlation plot
res <- cor(train[c(11:44)])

corrplot::corrplot(res,type = "upper", order = "hclust", 
                   tl.col = "black", tl.srt = 65, tl.cex = 0.7,
#                   title = "Correlation Matrix on NFL Running Back Data (2012-2022)", 
                   mar=c(0,0,1,0))


########## see all correlations better

# ++++++++++++++++++++++++++++
# flattenCorrMatrix - flattens out correlations to 2x2
# ++++++++++++++++++++++++++++
# cormat : matrix of the correlation coefficients
flattenCorrMatrix <- function(cormat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut]
  )
}

prev.corrs <- train[c(11:44)]
res2<-rcorr(as.matrix(prev.corrs))
previous.correlations <- flattenCorrMatrix(res2$r)

View(dplyr::arrange(previous.correlations, desc(cor)))

#####################################################################
############## explore relationships?
#####################################################################

#Binary predictors: Top12_Week6APPoll - not using anymore
#Continuous predictors: everything else

################ Binary predictors

#Top12_Week6APPoll - good representation amongst all values
table(train$Winner_A, train$Top12_Week6APPoll)

ggplot(data = train) +
  geom_bar(mapping = aes(x = Winner_A, fill = Top12_Week6APPoll))

CrossTable(train$Winner_A, train$Top12_Week6APPoll, expected = TRUE) #assumptions passed - expected values greater than 5
chisq.test(table(train$Winner_A, train$Top12_Week6APPoll)) #significant, < 2.67e-05... so the distribution of one variable changes across the distribution of another variable
#mantel haenszel test
CMHtest(table(train$Winner_A, train$Top12_Week6APPoll))$table[1,] #linear association, < 2.05e-05
#interpretation
OddsRatio(table(train$Winner_A, train$Top12_Week6APPoll)) #1.637964 for train 2000-2022

################ Continuous predictors

for (i in var.names) {
  xvar <- (train[[i]]) #this is needed to get the gam to run
  fit.gam <- mgcv::gam(Winner_A~s(xvar), data = train, family = binomial(link = "logit"), method = "REML")
  logit.model <- glm(Winner_A ~ train[[i]], data = train, family = binomial(link = "logit"))
  pval <- anova(logit.model, fit.gam, test = 'Chisq')$`Pr(>Chi)`[2]
  
  if (pval < 0.01) {
    print(paste(i, "**___ASSUMPTION NOT MET___**", round(pval,5), summary(fit.gam)$edf))
  } else {
    print(paste(i, "Assumption Met", round(pval,5), summary(fit.gam)$edf))
  }
  
  plot(fit.gam, xlab = i)
}



######################################################################
#SMBinning - all continuous variables and factor variables
########################################################################

#####################################
############### Creating bins for continuous/interval variables
######################################

numeric <- colnames(train[,sapply(train,is.numeric)])
numeric <- numeric[5:38] #don't need A_Score, B_Score, or MOV (response vars) or wts

allBins <- c()
#generates possible bins for numeric vars
for (i in numeric){
  allBins[[i]] = smbinning(df = train, y = "Winner_A", x = i) 
}

#plots of strong variables
smbinning.plot(allBins$Diff_TotalRPI,option="dist",sub="Diff_TotalRPI")
smbinning.plot(allBins$Diff_RPI_SOS,option="dist",sub="Diff_RPI_SOS")
smbinning.plot(allBins$Diff_PythagoreanExp,option="dist",sub="Diff_Pythagorean_Exp")
smbinning.plot(allBins$Diff_AvgMOV_PRE,option="dist",sub="Diff_AvgMOV_PRE")

smbinning.sumiv.plot(smbinning.sumiv(df = train[c(9:44)], y = "Winner_A"))


#test if IV > 0.01. 0.1 is standard for credit modeling, but want to increase predictability, and then generates binned column
for (j in numeric) {
  if (is.na(allBins[[j]]["iv"])) {
    next
  } else if (allBins[[j]]["iv"] >= 0.01) {
    train <- smbinning.gen(df = train, ivout = allBins[[j]], chrname = paste(j, "_bin", sep = ""))
  }
}

for (j in numeric) {
  if (is.na(allBins[[j]]["iv"])) {
    next
  } else if (allBins[[j]]["iv"] >= 0.01) {
    valid <- smbinning.gen(df = valid, ivout = allBins[[j]], chrname = paste(j, "_bin", sep = ""))
  }
}

#bins for conditionals 2023
#Diff_STL:  <=-0.2 , >-0.2
#Diff_TO: <=-2.9, >-2.9
#Diff_AdjEM: <=-2.6411, <=10.1276, <=15.1675, >15.1675

conditional2023 <- conditional2023 %>%
  mutate(Diff_STL_PRE_bin = case_when(
    Diff_STL_PRE <= -0.2 ~ "01 <= -0.2",
    Diff_STL_PRE > -0.2 ~ "02 > -0.2",
    TRUE ~ "0"  # Default condition, if none of the above conditions are met
  ),
    Diff_TO_PRE_bin = case_when(
      Diff_TO_PRE <= -2.9 ~ "01 <= -2.9",
      Diff_TO_PRE > -2.9 ~ "02 > -2.9",
      TRUE ~ "0"  # Default condition, if none of the above conditions are met
    ),
    Diff_AdjEM_PRE_bin = case_when(
      Diff_AdjEM_PRE <= -2.6411 ~ "01 <= -2.6411",
      Diff_AdjEM_PRE <= 10.1276 ~ "02 <= 10.1276",
      Diff_AdjEM_PRE <=15.1675 ~ "03 <= 15.1675",
      Diff_AdjEM_PRE > 15.1675 ~ "04 > 15.1675", 
      TRUE ~ "0"  # Default condition, if none of the above conditions are met
    ))

conditional2023$Diff_STL_PRE_bin <- as.factor(conditional2023$Diff_STL_PRE_bin)
conditional2023$Diff_TO_PRE_bin <- as.factor(conditional2023$Diff_TO_PRE_bin)
conditional2023$Diff_AdjEM_PRE_bin <- as.factor(conditional2023$Diff_AdjEM_PRE_bin)

#####################################
############### Creating bins for factor variables
######################################

#NOT USING ROUND AS PREDICTOR, SO DON'T RUN

factors <- colnames(train[,sapply(train,is.factor)])
factors <- factors[6] #only want Round (don't want binned vars)


allBins.factor <- c()

for (i in factors){
  allBins.factor[[i]] = smbinning.factor(df = train, y = "Winner_A", x = i) 
}


#test if IV > 0.05, and then generates binned column
for (j in factors) {
  if (allBins.factor[[j]]["iv"] >= 0.01) {
    train <- smbinning.factor.gen(df = train, ivout = allBins.factor[[j]], chrname = paste(j, "_bin", sep = ""))
  }
}

for (j in factors) {
  if (allBins.factor[[j]]["iv"] >= 0.01) {
    valid <- smbinning.factor.gen(df = valid, ivout = allBins.factor[[j]], chrname = paste(j, "_bin", sep = ""))
  }
}




############################################################################
######### Testing if RPI should be added to the model or not #####################
##################################################################################

#likelihood ratio test with and without RPI variables variable

train$Winner_A <- as.factor(train$Winner_A)

modwithRPI <- glm(Winner_A ~ .
                  , data = train[c(9, 46:70)], family = binomial(link = "logit"))


modwithoutRPI <- glm(Winner_A ~ .
                  , data = train[c(9, 46:61, 64:70)], family = binomial(link = "logit"))

lmtest::lrtest(modwithRPI, modwithoutRPI) #without both variables
#fail to reject null hypothesis, so the model without RPI is better. these 2 don't sig predict outcome

AIC(modwithRPI) #929.9752
AIC(modwithoutRPI) #924.6336

BIC(modwithRPI) #1149.904
BIC(modwithoutRPI) #1136.521

#dropping the RPI variables and Round
train <- train %>%
  dplyr::select(-Diff_TotalRPI_PRE_bin, -Diff_RPI_SOS_PRE_bin, -Round)

valid <- valid %>%
  dplyr::select(-Diff_TotalRPI_PRE_bin, -Diff_RPI_SOS_PRE_bin, -Round)
#####################################################################################











#################################################################################
########## Not using weights #######################################################
###################################################################################



#####################################################################
############## logistic regression model (without RPI)
#####################################################################


##################
#### re run train code above that defines bins 
#################


full.model <- glm(Winner_A ~ Diff_FGPct_PRE_bin + Diff_AST_PRE_bin + Diff_STL_PRE_bin + 
                    Diff_BLK_PRE_bin + Diff_TO_PRE_bin + Diff_PF_PRE_bin + Diff_PPG_PRE_bin + 
                    Diff_OppPPG_PRE_bin + Diff_UnadjOffEff_PRE_bin + Diff_UnadjDefEff_PRE_bin + 
                    Diff_WLPct_PRE_bin + Diff_OReb_PRE_bin + Diff_AvgMOV_PRE_bin + 
                    Diff_ExtraScoringChancesPG_PRE_bin + Diff_PythagoreanExp_PRE_bin + 
                    Diff_Luck_PRE_bin + Diff_AdjO_PRE_bin + Diff_AdjD_PRE_bin + 
                    Diff_AdjEM_PRE_bin + Diff_BARTHAG_PRE_bin + Diff_WAB_PRE_bin + 
                    Diff_OppTO_PRE_bin + Diff_OppTO_TeamTO_Ratio_PRE_bin
                  , data = train, family = binomial(link = "logit"))

empty.model <- glm(Winner_A ~ 1, data = train, family = binomial(link = "logit"))

#use 0.01to adjust for sample size
step.model <- step(full.model, scope = list(lower = empty.model, upper = full.model), 
                   direction = "backward", 
                   k = qchisq(0.01, 1, lower.tail = FALSE))

new.model <- glm(Winner_A ~ Diff_STL_PRE_bin + Diff_TO_PRE_bin + Diff_AdjEM_PRE_bin
                 , data = train, family = binomial(link = "logit"))

#Checking multicollinearity and using Likelihood Ratio Test (LRT)
car::vif(new.model) #all < 5
car::Anova(new.model, test = 'LR', type = 'III') #all sig



p_hat <- predict(new.model, type = "response")

p1 <- p_hat[train$Winner_A == 1]
p0 <- p_hat[train$Winner_A == 0]

coef_discrim <- mean(p1) - mean(p0)

ggplot(train, aes(p_hat, fill = factor(Winner_A))) + 
  geom_density(alpha = 0.7) + 
  scale_fill_grey() + 
  labs(x = "Predicted Probability", fill = "Outcome", title = paste("Coefficient of Discrimination = ", round(coef_discrim,3), sep = ""))




logit_meas <- measureit(p_hat, train$Winner_A, measure = c("ACC", "SENS", "SPEC"))

print(logit_meas)


logit_roc <- rocit(p_hat, train$Winner_A)
plot(logit_roc) #chance line = randomly flipped a coin to assign 0's and 1's

plot(logit_roc)$optimal  #cutoff = optimal cutoff that maximizes Youden index 
summary(logit_roc)


###############################################
####### making predictions on validation set 
###############################################

p_hat <- predict(new.model, newdata = valid, type = "response")
logit_roc <- rocit(p_hat, valid$Winner_A)
plot(logit_roc) #chance line = randomly flipped a coin to assign 0's and 1's

plot(logit_roc)$optimal  #cutoff = optimal cutoff that maximizes Youden index 
summary(logit_roc)

LogReg <- ifelse(p_hat>0.7199926,1,0)


conditional2023$espnLogReg <- predict(new.model, newdata = conditional2023, type = 'response')
conditional2023$winnerLogReg <- ifelse(conditional2023$espnLogReg > 0.7199926,1, 0)

View(conditional2023[c(1:5, 41:42)])

################################################
############## Random Forest (Without RPI) ####   
#################################################

train <- mm_data[mm_data$Year != 2023,] #2000 - 2022 tournaments
valid <- mm_data[mm_data$Year == 2023,] #2023 tournament

train <- as.data.frame(train)
valid <- as.data.frame(valid) #resets train set & removes bins from above

train$Winner_A <- as.factor(train$Winner_A)
valid$Winner_A <- as.factor(valid$Winner_A)

train <- train %>%
  dplyr::select(-c(Year, TeamA, TeamB, A_Seed, B_Seed, A_Score, 
                   B_Score, MOV, wts, Diff_RPI_SOS_PRE, Diff_TotalRPI_PRE,Round))

valid <- valid %>%
  dplyr::select(-c(Year, TeamA, TeamB, A_Seed, B_Seed, A_Score, 
                   B_Score, MOV, wts, Diff_RPI_SOS_PRE, Diff_TotalRPI_PRE,Round))



set.seed(12345) #can set ntree to 500 or 1000
rf.mm <- randomForest(Winner_A ~ ., data = train, ntree = 500, 
                      importance = TRUE, method = 'class')

plot(rf.mm, main = "Number of Trees Compared to MSE")

#tuning model

set.seed(12345)
tuneRF(x = train[,-1], y = train[,1], #column 1 has response variable
       plot = TRUE, ntreeTry = 500, stepFactor = 0.5, method = 'class')

#above output said optimal mtry is 5.
set.seed(12345)
rf.mm <- randomForest(Winner_A ~., 
                      data = train, ntree = 500, mtry = 5, importance = TRUE,
                      method = 'class')

################################################# variable importance
varImpPlot(rf.mm,
           sort = TRUE,
           n.var = 25,
           main = "Look for Variables Below Random Variable")


importance(rf.mm, type = 1)


####### evaluating model on training set

p_hat <- predict(rf.mm, type = "prob")
logit_roc <- rocit(p_hat[,2],train$Winner_A)
plot(logit_roc) #chance line = randomly flipped a coin to assign 0's and 1's

plot(logit_roc)$optimal  #cutoff = optimal cutoff that maximizes Youden index 
summary(logit_roc)


###############################################
####### making predictions on validation set 
###############################################

p_hat <- predict(rf.mm, newdata = valid, type = "prob")
logit_roc <- rocit(p_hat[,2], valid$Winner_A)
plot(logit_roc) #chance line = randomly flipped a coin to assign 0's and 1's

plot(logit_roc)$optimal  #cutoff = optimal cutoff that maximizes Youden index 
summary(logit_roc)

RandFor <- ifelse(p_hat[,2]>0.722  ,1,0)


conditional2023 <- read_excel("march_madness.xlsx", sheet = 5)
conditional2023$espn <- predict(rf.mm, newdata = conditional2023, type = 'prob')
conditional2023$winner <- ifelse(conditional2023$espn[,2] > 0.722,1, 0)

View(conditional2023[c(1:5, 38:39)])





################################################
############## XGBoost (without RPI) ##### 
#################################################

############################################ creating the model
train_x <- model.matrix(Winner_A ~ ., data = train)[, -1]
train_y <- as.numeric(train$Winner_A)-1
#weights <- train$wts

valid_x <- model.matrix(Winner_A ~., data = valid)[,-1]
valid_y <- as.numeric(valid$Winner_A)-1


######### CV to minimize number of trees --> 4 or 5
set.seed(12345)
xgbcv.mm <- xgb.cv(data = train_x, label = train_y, 
                   nrounds = 50, nfold = 10) #, subsample = 0.8, eta = 0.4, max_depth = 2, colsample_bytree = 0.9)



############# grid search to optimize parameters
tune_grid <- expand.grid(
  nrounds = 3, #c(4:6),
  eta = c(0.05, 0.1, 0.25,0.4, 0.45),
  max_depth = c(2:5),
  gamma = c(0),
  colsample_bytree = c(0.9, 1),
  min_child_weight = 1,
  subsample = c(0.8, 0.9, 1)
)

set.seed(12345) #
xgb.ins.caret <- caret::train(x = train_x, y = train_y,
                              method = "xgbTree",
                              tuneGrid = tune_grid,
                              trControl = trainControl(method = 'cv', # Using 10-fold cross-validation
                                                       number = 10),
                              objective = "binary:logistic",
                              verbosity =0) #gets rid of warning messages

plot(xgb.ins.caret)

xgb.ins.caret$bestTune


set.seed(12345)
xgb.mm <- xgboost(data = train_x, label = train_y, 
                  nrounds = 3, objective = "binary:logistic",
                  max_depth = 2, eta = 0.4, subsample = 0.8)


#################################### AUROC comparison

p_hat <- predict(xgb.mm, newdata = train_x, type = "prob")
logit_roc <- rocit(p_hat, train$Winner_A)
plot(logit_roc) #chance line = randomly flipped a coin to assign 0's and 1's

plot(logit_roc)$optimal  #cutoff = optimal cutoff that maximizes Youden index 
summary(logit_roc)


################################### variable importance

#using optimal parameters from previous tuning
xgb.importance(feature_names = colnames(train_x), model = xgb.mm)

#will automatically cluster variables statistically based on similar gain
xgb.ggplot.importance(xgb.importance(feature_names = colnames(train_x), model = xgb.mm))



###############################################
####### making predictions on validation set 
###############################################

p_hat <- predict(xgb.mm, newdata = valid_x, type = "prob")
logit_roc <- rocit(p_hat, valid$Winner_A)
plot(logit_roc) #chance line = randomly flipped a coin to assign 0's and 1's

plot(logit_roc)$optimal  #cutoff = optimal cutoff that maximizes Youden index 
summary(logit_roc)

XGB <- ifelse(p_hat>0.6957421 ,1,0)


conditional2023 <- read_excel("march_madness.xlsx", sheet = 5)
conditional2023_x <- model.matrix(~.-Year-TeamA-TeamB-A_Seed-B_Seed -espnLogReg - winnerLogReg, data = conditional2023)[,c(-1)]
conditional2023$espnXGB <- predict(xgb.mm, newdata = conditional2023_x, type = 'prob')
conditional2023$winnerXGB <- ifelse(conditional2023$espnXGB > 0.6957421,1, 0)

View(conditional2023[c(1:5, 37:39)])







#############################################################
############## SVM (without RPI) ##### 
##########################################################


#Building the svm model
set.seed(12345)
svm.mm <- svm(train_x, train_y, kernel = "radial", probability = TRUE)

#calibrate probabilities with Platt Scaling
platt_model <- glm(as.factor(train_y) ~ as.vector(attr(predict(svm.mm, train_x, probability = TRUE), "decision.values")), family = binomial)

# Make predictions with calibrated probabilities
calibrated_probs <- predict(platt_model, type = "response", newdata = as.vector(attr(predict(svm_model, iris, probability = TRUE), "decision.values")))


p_hat <- predict(svm.mm, newdata = train_x)
logit_roc <- rocit(p_hat, train$Winner_A)
plot(logit_roc) #chance line = randomly flipped a coin to assign 0's and 1's

plot(logit_roc)$optimal  #cutoff = optimal cutoff that maximizes Youden index 
summary(logit_roc)

###############################################
####### making predictions on validation set 
###############################################

p_hat <- predict(svm.mm, newdata = valid_x, probability = TRUE)
logit_roc <- rocit(p_hat, valid$Winner_A)
plot(logit_roc) #chance line = randomly flipped a coin to assign 0's and 1's

plot(logit_roc)$optimal  #cutoff = optimal cutoff that maximizes Youden index 
summary(logit_roc)

SupportVectorM <- ifelse(p_hat>0.9704848 ,1,0)

conditional2023 <- read_excel("march_madness.xlsx", sheet = 5)
conditional2023_x <- model.matrix(~.-Year-TeamA-TeamB-A_Seed-B_Seed, data = conditional2023)[,-1]
conditional2023$espn <- predict(svm.mm, newdata = conditional2023_x, probability = TRUE)
conditional2023$winner <- ifelse(conditional2023$espn > 0.9704848,1, 0)

View(conditional2023[c(1:5, 38:39)])


conditional2023 %>%
  group_by(TeamA) %>%
  summarise(total = sum(winner))

################################################
############## Naive Bayes (without RPI) #####
#################################################



#Tuning the best model (w Categorical Target Variable)

tune_grid <- expand.grid(
  usekernel = c(TRUE, FALSE),
  fL = c(0, 0.5, 1),
  adjust = c(0.1, 0.5, 1)
)

set.seed(12345)
nb.mm.caret <- caret::train(Winner_A ~ ., data = train,
                            method = "nb", 
                            tuneGrid = tune_grid,
                            trControl = trainControl(method = 'cv', number = 10))

nb.mm.caret$bestTune

set.seed(12345)
nb.mm <- naiveBayes(Winner_A ~., data = train, laplace = 0, usekernel = TRUE, fL = 0, adjust = 0.1)




p_hat <- predict(nb.mm, newdata = train, type = "raw")
logit_roc <- rocit(p_hat[,2], train$Winner_A)
plot(logit_roc) #chance line = randomly flipped a coin to assign 0's and 1's

plot(logit_roc)$optimal  #cutoff = optimal cutoff that maximizes Youden index 
summary(logit_roc)

###############################################
####### making predictions on validation set 
###############################################

p_hat <- predict(nb.mm, newdata = valid, type = 'raw')
logit_roc <- rocit(p_hat[,2], valid$Winner_A)
plot(logit_roc) #chance line = randomly flipped a coin to assign 0's and 1's

plot(logit_roc)$optimal  #cutoff = optimal cutoff that maximizes Youden index 
summary(logit_roc)

NBayes <- ifelse(p_hat[,2]>0.2291801   ,1,0)










################################################
############## MARS (without RPI) #####
#################################################


mars2 <- earth(Winner_A ~ ., data = train, glm = list(family = binomial(link= "logit")))
summary(mars2)

evimp(mars2)



p_hat <- predict(mars2, newdata = train, type = "response")
logit_roc <- rocit(p_hat[,1], train$Winner_A)
plot(logit_roc) #chance line = randomly flipped a coin to assign 0's and 1's

plot(logit_roc)$optimal  #cutoff = optimal cutoff that maximizes Youden index 
summary(logit_roc)

###############################################
####### making predictions on validation set 
###############################################

p_hat <- predict(mars2, newdata = valid, type = 'response')
logit_roc <- rocit(p_hat[,1], valid$Winner_A)
plot(logit_roc) #chance line = randomly flipped a coin to assign 0's and 1's

plot(logit_roc)$optimal  #cutoff = optimal cutoff that maximizes Youden index 
summary(logit_roc)

planetMARS <- ifelse(p_hat[,1]>0.7355581 ,1,0)



conditional2023 <- read_excel("march_madness.xlsx", sheet = 5)
conditional2023$espnMARS <- predict(mars2, newdata = conditional2023, type = 'response')
conditional2023$winnerMARS <- ifelse(conditional2023$espnMARS[,1] > 0.7355581,1, 0)

View(conditional2023[c(1:5, 38:39)])








#################################################################################
########## Using weights #######################################################
###################################################################################


################################################
############## Random Forest  ##### 
#################################################


train <- mm_data[mm_data$Year != 2023,] #2000 - 2022 tournaments
valid <- mm_data[mm_data$Year == 2023,] #2023 tournament

train <- as.data.frame(train)
valid <- as.data.frame(valid) #resets train set & removes bins from above

train$Winner_A <- as.factor(train$Winner_A)
valid$Winner_A <- as.factor(valid$Winner_A)

train <- train %>%
  dplyr::select(-c(Year, TeamA, TeamB, A_Seed, B_Seed, A_Score, 
                   B_Score, MOV, Diff_RPI_SOS_PRE, Diff_TotalRPI_PRE,Round))

valid <- valid %>%
  dplyr::select(-c(Year, TeamA, TeamB, A_Seed, B_Seed, A_Score, 
                   B_Score, MOV, Diff_RPI_SOS_PRE, Diff_TotalRPI_PRE,Round))

weights = train$wts




set.seed(12345) #can set ntree to 500 or 1000
rf.mm <- randomForest(Winner_A ~ ., data = train, ntree = 500, 
                      importance = TRUE, method = 'class', weight = weights)

plot(rf.mm, main = "Number of Trees Compared to MSE")

#tuning model

set.seed(12345)
tuneRF(x = train[,-1], y = train[,1], #column 1 has response variable
       plot = TRUE, ntreeTry = 500, stepFactor = 0.5, method = 'class', weight = weights)

#above output said optimal mtry is 2.
set.seed(12345)
rf.mm <- randomForest(Winner_A ~., 
                      data = train, ntree = 500, mtry = 2, importance = TRUE,
                      method = 'class', weight = weights)

################################################# variable importance
varImpPlot(rf.mm,
           sort = TRUE,
           n.var = 27,
           main = "Look for Variables Below Random Variable")


importance(rf.mm, type = 1)


####### evaluating model on training set

p_hat <- predict(rf.mm, type = "prob")
logit_roc <- rocit(p_hat[,2],train$Winner_A)
plot(logit_roc) #chance line = randomly flipped a coin to assign 0's and 1's

plot(logit_roc)$optimal  #cutoff = optimal cutoff that maximizes Youden index 
summary(logit_roc)


###############################################
####### making predictions on validation set 
###############################################

p_hat <- predict(rf.mm, newdata = valid, type = "prob")
logit_roc <- rocit(p_hat[,2], valid$Winner_A)
plot(logit_roc) #chance line = randomly flipped a coin to assign 0's and 1's

plot(logit_roc)$optimal  #cutoff = optimal cutoff that maximizes Youden index 
summary(logit_roc)

RF_weighted <- ifelse(p_hat[,2]>0.810 ,1,0)


################################################
############## SVM ##### 
#################################################


############################################ creating the model
train_x <- model.matrix(Winner_A ~ ., data = train)[, -1]
train_y <- as.numeric(train$Winner_A)-1
#weights <- train$wts

valid_x <- model.matrix(Winner_A ~., data = valid)[,-1]
valid_y <- as.numeric(valid$Winner_A)-1


#Building the svm model
set.seed(12345)
svm.mm <- svm(train_x, train_y, kernel = "radial", weight = weights)


p_hat <- predict(svm.mm, newdata = train_x)
logit_roc <- rocit(p_hat, train$Winner_A)
plot(logit_roc) #chance line = randomly flipped a coin to assign 0's and 1's

plot(logit_roc)$optimal  #cutoff = optimal cutoff that maximizes Youden index 
summary(logit_roc)

###############################################
####### making predictions on validation set 
###############################################

p_hat <- predict(svm.mm, newdata = valid_x)
logit_roc <- rocit(p_hat, valid$Winner_A)
plot(logit_roc) #chance line = randomly flipped a coin to assign 0's and 1's

plot(logit_roc)$optimal  #cutoff = optimal cutoff that maximizes Youden index 
summary(logit_roc)

SVM_weighted <- ifelse(p_hat>0.9158410  ,1,0)



###################################################################
###### XGBoost
####################################################################


######### CV to minimize number of trees --> 4 or 5
set.seed(12345)
xgbcv.mm <- xgb.cv(data = train_x, label = train_y, 
                   nrounds = 50, nfold = 10, weight = weights) #, subsample = 0.8, eta = 0.4, max_depth = 2, colsample_bytree = 0.9)



############# grid search to optimize parameters
tune_grid <- expand.grid(
  nrounds = 3, #c(4:6),
  eta = c(0.05, 0.1, 0.25,0.4, 0.45),
  max_depth = c(2:5),
  gamma = c(0),
  colsample_bytree = c(0.9, 1),
  min_child_weight = 1,
  subsample = c(0.8, 0.9, 1)
)

set.seed(12345) #
xgb.ins.caret <- caret::train(x = train_x, y = train_y,
                              method = "xgbTree",
                              tuneGrid = tune_grid,
                              trControl = trainControl(method = 'cv', # Using 10-fold cross-validation
                                                       number = 10),
                              objective = "binary:logistic",
                              weights = weights,
                              verbosity =0) #gets rid of warning messages

xgb.ins.caret$bestTune


set.seed(12345)
xgb.mm <- xgboost(data = train_x, label = train_y, 
                  nrounds = 3, objective = "binary:logistic",
                  max_depth = 2, eta = 0.45, subsample = 1, weight = weights)


#################################### AUROC comparison

p_hat <- predict(xgb.mm, newdata = train_x, type = "prob")
logit_roc <- rocit(p_hat, train$Winner_A)
plot(logit_roc) #chance line = randomly flipped a coin to assign 0's and 1's

plot(logit_roc)$optimal  #cutoff = optimal cutoff that maximizes Youden index 
summary(logit_roc)



###############################################
####### making predictions on validation set 
###############################################

p_hat <- predict(xgb.mm, newdata = valid_x, type = "prob")
logit_roc <- rocit(p_hat, valid$Winner_A)
plot(logit_roc) #chance line = randomly flipped a coin to assign 0's and 1's

plot(logit_roc)$optimal  #cutoff = optimal cutoff that maximizes Youden index 
summary(logit_roc)

XGB_weighted <- ifelse(p_hat>0.7139316 ,1,0)


conditional2023 <- read_excel("march_madness.xlsx", sheet = 5)
conditional2023_x <- model.matrix(~.-Year-TeamA-TeamB-A_Seed-B_Seed, data = conditional2023)[,-1]
conditional2023$espn <- predict(xgb.mm, newdata = conditional2023_x)
conditional2023$winner <- ifelse(conditional2023$espn > 0.7139316,1, 0)

View(conditional2023[c(1:5, 38:39)])



##############################################################################
###### Ensemble #
############################################################################

conditional2023$Ensemble <- ifelse(conditional2023$winnerLogReg + conditional2023$winnerXGB + conditional2023$winnerMARS >=2,1,0)
View(conditional2023[c(1:5, 44)])

# ensemble?
ensemble <- ifelse(LogReg + XGB + planetMARS >= 2, 1,0)
ensemble

#confusion matrix
conf_matrix <- table(valid$Winner_A, ensemble)

# Calculate true positive rate (Sensitivity)
TPR <- conf_matrix[2, 2] / sum(conf_matrix[2, ])

# Calculate false positive rate
FPR <- conf_matrix[1, 2] / sum(conf_matrix[1, ])

# Alternatively, calculate AUC manually
AUC <- (1 + TPR - FPR) / 2

AUC

#LogReg 
#RandFor
#XGB
#SupportVectorM
#NBayes
#planetMARS
#RF_weighted
#SVM_weighted
#XGB_weighted



####################################################################################
######### TEST 2023
#####################################################################################
#Train AUC (2008-2022):

#Logistic  --        AUC = 0.7435
#Random Forest  --   AUC = 0.6827
#XGBoost  --         AUC = 0.7489
#SVM  --             AUC = 0.8964
#Naive Bayes  --     AUC = 0.7038
#MARS --             AUC = 0.7713
#RF (weighted) --    AUC = 0.6847
#SVM (weighted)      AUC = 0.9098
#XGB (weighted)      AUC = 0.7483
#Ensemble            AUC = ****

#Valid AUC (2023):

#Logistic  --        AUC = 0.5628, ESPN Score = 430
#Random Forest  --   AUC = 0.6196, ESPN Score = 310
#XGBoost  --         AUC = 0.6220, ESPN Score = 710
#SVM  --             AUC = 0.5993, ESPN Score = --
#Naive Bayes  --     AUC = 0.5161, ESPN Score = --
#MARS --             AUC = 0.6268, ESPN Score = 700
#RF (weighted) --    AUC = 0.5580, ESPN Score = --
#SVM (weighted)      AUC = 0.6100, ESPN Score = --
#XGB (weighted)      AUC = 0.6262, ESPN Score = --
#Ensemble            AUC = 0.6370, ESPN Score = 430

#Den: AST, STL, PF, WLPct, SOS, ORtg



##################################################################################
#### Re-run on combined train/test
################################################################################

mm_data <- read_excel("march_madness.xlsx", sheet = 2)

#only want 2008-2023
train <- mm_data %>%
  dplyr::filter(!(Year %in% 2000:2007))

test <- read_excel("march_madness.xlsx", sheet = 7)

train$Year <- as.factor(train$Year)
train$TeamA <- as.factor(train$TeamA)
train$TeamB <- as.factor(train$TeamB)
train$A_Seed <- as.factor(train$A_Seed)
train$B_Seed <- as.factor(train$B_Seed)
train$Winner_A <- as.factor(train$Winner_A)

train <- train %>%
  dplyr::select(-c(Year, TeamA, TeamB, A_Seed, B_Seed, A_Score, 
                   B_Score, MOV, Round, Diff_TotalRPI_PRE, Diff_RPI_SOS_PRE))


################################################
############## XGBoost (without RPI) ##### 
#################################################

############################################ creating the model
train_x <- model.matrix(Winner_A ~ ., data = train)[, -1]
train_y <- as.numeric(train$Winner_A)-1
#weights <- train$wts


######### CV to minimize number of trees --> 4 or 5
set.seed(12345)
xgbcv.mm <- xgb.cv(data = train_x, label = train_y, 
                   nrounds = 50, nfold = 10) #, subsample = 0.8, eta = 0.4, max_depth = 2, colsample_bytree = 0.9)



############# grid search to optimize parameters
tune_grid <- expand.grid(
  nrounds = 5, #c(4:6),
  eta = c(0.05, 0.1, 0.25,0.4, 0.45),
  max_depth = c(2:5),
  gamma = c(0),
  colsample_bytree = c(0.9, 1),
  min_child_weight = 1,
  subsample = c(0.8, 0.9, 1)
)

set.seed(12345) #
xgb.ins.caret <- caret::train(x = train_x, y = train_y,
                              method = "xgbTree",
                              tuneGrid = tune_grid,
                              trControl = trainControl(method = 'cv', # Using 10-fold cross-validation
                                                       number = 10),
                              objective = "binary:logistic",
                              verbosity =0) #gets rid of warning messages

plot(xgb.ins.caret)

xgb.ins.caret$bestTune


set.seed(12345)
xgb.mm <- xgboost(data = train_x, label = train_y, 
                  nrounds = 5, objective = "binary:logistic",
                  max_depth = 2, eta = 0.4, subsample = 1, colsample_bytree = 0.9)


#################################### AUROC comparison

p_hat <- predict(xgb.mm, newdata = train_x, type = "prob")
logit_roc <- rocit(p_hat, train$Winner_A)
plot(logit_roc) #chance line = randomly flipped a coin to assign 0's and 1's

plot(logit_roc)$optimal  #cutoff = optimal cutoff that maximizes Youden index 
summary(logit_roc)


################################### variable importance

#using optimal parameters from previous tuning
xgb.importance(feature_names = colnames(train_x), model = xgb.mm)

#will automatically cluster variables statistically based on similar gain
xgb.ggplot.importance(xgb.importance(feature_names = colnames(train_x), model = xgb.mm))



###############################################
####### making predictions on validation set 
###############################################

p_hat <- predict(xgb.mm, newdata = valid_x, type = "prob")
logit_roc <- rocit(p_hat, valid$Winner_A)
plot(logit_roc) #chance line = randomly flipped a coin to assign 0's and 1's

plot(logit_roc)$optimal  #cutoff = optimal cutoff that maximizes Youden index 
summary(logit_roc)

XGB <- ifelse(p_hat>0.6957421 ,1,0)


test_x <- model.matrix(~.-Year-TeamA-TeamB-A_Seed-B_Seed, data = test)[,-1]
test$espnXGB <- predict(xgb.mm, newdata = test_x, type = 'prob')
test$winnerXGB <- ifelse(test$espnXGB > 0.7065721,1, 0)

View(test[c(1:5, 37:39)])





################################################
############## MARS (without RPI) #####
#################################################


mars2 <- earth(Winner_A ~ ., data = train, glm = list(family = binomial(link= "logit")))
summary(mars2)

evimp(mars2)



p_hat <- predict(mars2, newdata = train, type = "response")
logit_roc <- rocit(p_hat[,1], train$Winner_A)
plot(logit_roc) #chance line = randomly flipped a coin to assign 0's and 1's

plot(logit_roc)$optimal  #cutoff = optimal cutoff that maximizes Youden index 
summary(logit_roc)

###############################################
####### making predictions on validation set 
###############################################

p_hat <- predict(mars2, newdata = valid, type = 'response')
logit_roc <- rocit(p_hat[,1], valid$Winner_A)
plot(logit_roc) #chance line = randomly flipped a coin to assign 0's and 1's

plot(logit_roc)$optimal  #cutoff = optimal cutoff that maximizes Youden index 
summary(logit_roc)

planetMARS <- ifelse(p_hat[,1]>0.7355581 ,1,0)



test$espnMARS <- predict(mars2, newdata = test, type = 'response')
test$winnerMARS <- ifelse(test$espnMARS[,1] > 0.6996445,1, 0)

View(test[c(1:5, 38:41)])

