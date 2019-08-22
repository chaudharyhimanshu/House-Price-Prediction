library(ggplot2)
#sale_price <- train$SalePrice
#train <- train[,!(names(train) %in% 'SalePrice')]
#complete_data <- rbind(train,test)
attach(train)
features <- sapply(train,class)
categorical_features <- c()
numerical_features <- c()
for(i in 1:length(names(complete_data))) {
  if(features[i] == 'numeric')
    numerical_features <- c(numerical_features,attributes(features)[[1]][i])
  else 
    categorical_features <- c(categorical_features,attributes(features)[[1]][i])
}

colSums(is.na(train))
colSums(is.na(test))
c(nrow(train), nrow(test))
#Normalising SalePrice
train$sp <- (train$SalePrice - mean(SalePrice))/ sd(sale_price)

# Alley
train[is.na(train$Alley),]$Alley <- 'No Alley'
table(train$Alley)
t.test(train[which(train$Alley == 'Grvl'),]$sp - train[which(train$Alley == 'Pave'),]$sp,
       var.equal = False)
g <- ggplot(data = train, aes(x = Alley, y = SalePrice, col = Alley)) + geom_boxplot()
fit <- glm(SalePrice ~ Alley, data = train)
ggplot(data = train, aes(x = Alley, y = SalePrice)) + geom_violin() + geom_smooth()

#Fence
table(train$Fence)
train[is.na(train$Fence),]$Fence <- 'No Fence'
ggplot(data = train, aes(x = Fence, y = SalePrice)) + geom_violin()
ggplot(data = train, aes(x = Fence, y = SalePrice)) + geom_boxplot()
fit2 <- lm(SalePrice ~ Fence, data = train)
summary(fit2)$coef
fence2 <- relevel(as.factor(train$Fence), 'No Fence')
fit3 <- lm(SalePrice ~ fence2, data = train)
round(summary(fit3)$coef,5)
fence3 <- relevel(as.factor(train$Fence), 'GdWo')
round(summary(lm(SalePrice ~ fence3, data = train))$coef, 5)


fit4 <- update(fit,SalePrice ~  Alley + Fence)
summary(fit4)$coef
plot(fit4)


#PoolQC
train[is.na(train$PoolQC),]$PoolQC <- 'No Pool'
table(train$PoolQC)
fit5 <- update(fit,SalePrice ~ Alley + Fence + PoolQC)
plot(fit5)
anova(fit, fit4, fit5)
ggplot(data = train, aes(x = PoolQC, y = SalePrice)) + geom_point()


#LotArea
t.test(train$LotArea)
ggplot(train, aes(x = LotArea, y = SalePrice, col = Neighborhood)) + geom_point() + geom_smooth(method = 'lm', se = FALSE) + facet_wrap( ~ PoolQC)
c(range(train$LotArea), range(test$LotArea))
fit1 <- lm(SalePrice ~ LotArea, data = train)
train <- train[which(train$LotArea < 60000),]


#LotFrontage
hist(train$LotFrontage)
table(train$LotFrontage)
plot(train$LotFrontage, train$SalePrice)
plot(train$LotArea, train$LotFrontage)
abline(lm(SalePrice ~ LotFrontage, data = train))
t.test(train$LotFrontage, conf.level = 0.99)
fit2 <- lm(SalePrice ~ LotArea + LotFrontage, data = train)
anova(fit1, fit2)
range(isna_lotfrontage)
plot(isna_lotfrontage, train[is.na(train$LotFrontage),]$SalePrice)
t.test(complete_data$LotFrontage, conf.level = 0.99)
range(complete_data[is.na(complete_data$LotFrontage),]$LotArea)
complete_data <- rbind(train[,1:ncol(train)-1], test)
#x <- sample(c(68,69,70), 483, replace = TRUE)
#complete_data[is.na(complete_data$LotFrontage),]$LotFrontage <- x
ggplot(data = complete_data, aes(x = LotFrontage, y = LotArea,  alpha = 0.3)) +
  geom_point() + facet_wrap(~Neighborhood) #+ geom_vline(xintercept = mean(LotFrontage))
table(complete_data[is.na(complete_data$LotFrontage),]$Neighborhood)
#filling lotfrontage by 99% confint on the basis of lotarea and neighborhood
lot_na <- names(table(complete_data[is.na(complete_data$LotFrontage),]$Neighborhood))
for (i in lot_na) {
  x <- as.integer(t.test(complete_data[which(complete_data$Neighborhood == i),]$LotFrontage, conf.level = 0.99)$conf.int[1])
  y <- as.integer(t.test(complete_data[which(complete_data$Neighborhood == i),]$LotFrontage, conf.level = 0.99)$conf.int[2])
  len <- length(complete_data[which(complete_data$Neighborhood
                             == i),][ is.na(complete_data[which(complete_data$Neighborhood
                                                                == i),]$LotFrontage),]$LotFrontage)
  complete_data[which(complete_data$Neighborhood
                      == i),][ is.na(complete_data[which(complete_data$Neighborhood
                                                                 == i),]$LotFrontage),]$LotFrontage = 
    sample(c(x:y), len, replace = TRUE)
}
plot(complete_data$LotFrontage, complete_data$LotArea)



#Electrical
sum(is.na(train$Electrical)) # only 1 value in train
ggplot(data = train, aes(x = Electrical , y = SalePrice)) + geom_violin()
ggplot(data = train, aes(x = Electrical , y = SalePrice)) + geom_boxplot()

cbind(table(train$Electrical), table(test$Electrical))
t.test(train[which(train$Electrical == 'FuseA'),]$SalePrice, conf.level = 0.99)
t.test(train[which(train$Electrical == 'SBrkr'),]$SalePrice, conf.level = 0.99)
train[is.na(train$Electrical),]$SalePrice
table(complete_data[which(complete_data$Neighborhood == 'Timber'),]$Electrical)# every home in Timber have SBrkr wiring
train[is.na(train$Electrical),]$Electrical = 'SBrkr'



#PoolQC
sum(is.na(complete_data$PoolQC))
#assigning NO pool to NA entries
complete_data[is.na(complete_data$PoolQC),]$PoolQC = 'No Pool'
train[is.na(train$PoolQC),]$PoolQC = 'No Pool'
test[is.na(test$PoolQC),]$PoolQC = 'No Pool'
ggplot(data = train, aes(x = PoolQC , y = SalePrice)) + geom_point()
ggplot(data = train, aes(x = PoolQC , y = SalePrice)) + geom_boxplot()
rbind(table(train$PoolQC), table(test$PoolQC))
#this variable is having too much values of No Pool.
#There are 2 house having ex pool, but one have decent price
#while other have very high price
#Fa and Gd both have only 2 entries and both category have almost same
#price house. 
#While in test set there are almost all Gd entries, so 2 Gd entries
#in train will no tell any thing about the house
#DROP THIS COLUMN




#MiscFeatures
train[is.na(train$MiscFeature),]$MiscFeature = 'None'
test[is.na(test$MiscFeature),]$MiscFeature = 'None'
complete_data[is.na(complete_data$MiscFeature),]$MiscFeature = 'None'
rbind(table(train$MiscFeature), table(test$MiscFeature))
ggplot(data = train, aes(x = MiscFeature, y = SalePrice)) + geom_point()



#FireplaceQu
train[is.na(train$FireplaceQu),]$FireplaceQu = 'None'
test[is.na(test$FireplaceQu),]$FireplaceQu = 'None'
complete_data[is.na(complete_data$FireplaceQu),]$FireplaceQu = 'None'
rbind(table(train$FireplaceQu), table(test$FireplaceQu))
ggplot(data = train, aes(x = FireplaceQu, y = SalePrice, alpha = 0.3)) +
  geom_point()
ggplot(data = train, aes(x = FireplaceQu, y = SalePrice, alpha = 0.3)) +
  geom_violin()
ggplot(data = train, aes(x = FireplaceQu, y = SalePrice, alpha = 0.3)) +
  geom_boxplot()

median(train[which(train$FireplaceQu == 'Gd'),]$SalePrice)
median(train[which(train$FireplaceQu != 'None'),]$SalePrice)
median(train[which(train$FireplaceQu == 'None'),]$SalePrice)
mean(train[which(train$FireplaceQu == 'None'),]$SalePrice)
mean(train[which(train$FireplaceQu != 'None'),]$SalePrice)
t.test(train[which(train$FireplaceQu == 'Ex'),]$SalePrice)
t.test(train[which(train$FireplaceQu == 'TA'),]$SalePrice)
t.test(train[which(train$FireplaceQu == 'Gd'),]$SalePrice, 
       train[which(train$FireplaceQu == 'TA'),]$SalePrice, paired = FALSE)
t.test(train[which(train$FireplaceQu == 'None'),]$SalePrice)
t.test(train[which(train$FireplaceQu != 'None'),]$SalePrice)

#Utilities
table(train$Utilities)
table(test$Utilities)
c(sum(is.na(train$Utilities)), sum(is.na(test$Utilities)))
#Not at all usefull, as all house have AllPub


#MasVnrType and MasVnrArea
rbind(table(train$MasVnrType), table(test$MasVnrType))
#train[which(is.na(train$MasVnrType)),]$MasVnrArea = 0
#test[which(is.na(test$MasVnrType)),]$MasVnrArea = 0
#train[which(is.na(train$MasVnrType)),]$MasVnrType = 'None'
#test[which(is.na(test$MasVnrType)),]$MasVnrType = 'None'

