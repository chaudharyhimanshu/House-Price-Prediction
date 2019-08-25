library(ggplot2)
#sale_price <- train$SalePrice
#train <- train[,!(names(train) %in% 'SalePrice')]
complete_data <- rbind(train[,1:ncol(train)-1], test)
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

#Categorical to numeric
cat_num <- function(x, name) {
  index = 1
  for(i in name) {
    x[which(x == i)] = as.integer(index)
    index = index + 1
  }
  return(as.numeric(x))
}

colSums(is.na(train))
colSums(is.na(test))
c(nrow(train), nrow(test))
#Normalising SalePrice
train$sp <- (train$SalePrice - mean(SalePrice))/ sd(sale_price)

# Alley
train[is.na(train$Alley),]$Alley <- 'No Alley'
test[is.na(test$Alley),]$Alley <- 'No Alley'
complete_data[is.na(complete_data$Alley),]$Alley <- 'No Alley'
table(train$Alley)
t.test(train[which(train$Alley == 'Grvl'),]$sp - train[which(train$Alley == 'Pave'),]$sp,
       var.equal = False)
g <- ggplot(data = train, aes(x = Alley, y = SalePrice, col = Alley)) + geom_boxplot()
fit <- glm(SalePrice ~ Alley, data = train)
ggplot(data = train, aes(x = Alley, y = SalePrice)) + geom_violin() + geom_smooth()
train <- train[,!(names(train) %in% 'Alley')]
test <- test[,!(names(test) %in% 'Alley')]
complete_data <- complete_data[,!(names(complete_data) %in% 'Alley')]

#Fence
rbind(table(train$Fence), table(test$Fence))
train[is.na(train$Fence),]$Fence <- 'No Fence'
test[is.na(test$Fence),]$Fence <- 'No Fence'
complete_data[is.na(complete_data$Fence),]$Fence <- 'No Fence'
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

cor(train$Fence, train$SalePrice)

#PoolQC
train[is.na(train$PoolQC),]$PoolQC <- 'No Pool'
test[is.na(test$PoolQC),]$PoolQC <- 'No Pool'
complete_data[is.na(complete_data$PoolQC),]$PoolQC <- 'No Pool'
table(train$PoolQC)
fit5 <- update(fit,SalePrice ~ Alley + Fence + PoolQC)
plot(fit5)
anova(fit, fit4, fit5)
ggplot(data = train, aes(x = PoolQC, y = SalePrice)) + geom_point()


#LotArea
t.test(train$LotArea)
ggplot(train, aes(x = LotArea, y = SalePrice, col = Neighborhood)) + geom_point() + geom_smooth(method = 'lm', se = FALSE) + facet_wrap( ~ PoolQC)
ggplot(train, aes(x = LotArea, y = SalePrice)) + geom_point() + geom_smooth(method = 'lm')
c(range(train$LotArea), range(test$LotArea))
fit1 <- lm(SalePrice ~ LotArea, data = train)
train <- train[which(train$LotArea < 60000),]
cor(train$LotArea, train$SalePrice) # very low correlation


#GrLiveArea
ggplot(train, aes(x = GrLivArea, y = SalePrice)) + 
  geom_point() + geom_smooth()
ggplot(complete_data, aes(x = GrLivArea)) + 
  geom_histogram(aes(y = ..density..)) +
  geom_density() 
cor(train$GrLivArea, train$SalePrice)
train <- train[which(train$GrLivArea < 4500),]



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

#MoSold and YrSold and YearBuilt
ggplot(train, aes(x = MoSold, y = SalePrice)) + geom_point() + geom_smooth() + facet_wrap(~YrSold)
cor(train$MoSold, train$SalePrice)
ggplot(train, aes(x = YrSold, y = SalePrice)) + geom_point() + geom_smooth() + facet_wrap(~MoSold)
ggplot(train, aes(x = YearBuilt, y = SalePrice)) + geom_point() + geom_smooth() + facet_wrap(~YrSold)


ggplot(train, aes(x = YearBuilt, y = SalePrice)) + geom_point() + geom_smooth()
train[which(train$SalePrice > 400000 & train$YearBuilt < 1900),]



#PoolQC
sum(is.na(complete_data$PoolQC))
#assigning NO pool to NA entries
complete_data[is.na(complete_data$PoolQC),]$PoolQC = 'No Pool'
train[is.na(train$PoolQC),]$PoolQC = 'No Pool'
test[is.na(test$PoolQC),]$PoolQC = 'No Pool'
ggplot(data = train, aes(x = PoolQC , y = SalePrice)) + geom_point()
ggplot(data = train, aes(x = PoolQC , y = SalePrice)) + geom_boxplot()
rbind(table(train$PoolQC), table(test$PoolQC))
train <- train[,(!(names(train) %in% 'PoolQC'))]
test <- test[,(!(names(test) %in% 'PoolQC'))]
complete_data <- complete_data[,(!(names(complete_data) %in% 'PoolQC'))]
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


#MiscVal
ggplot(train, aes(x = MiscVal)) + geom_density(fill = 'lightblue')
ggplot(train[which(train$MiscVal > 0),], aes(x = MiscVal, y = SalePrice)) + geom_point() + geom_smooth()
train[which(train$MiscVal > 2500),]



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
train <- train[,(!(names(train) %in% 'Utilities'))]
test <- test[,(!(names(test) %in% 'Utilities'))]
complete_data <- complete_data[,(!(names(complete_data) %in% 'Utilities'))]
#Not at all usefull, as all house have AllPub


#OverallCond and OverallQual
#no na in both column
table(train$OverallCond)
table(train$OverallQual)
cor(train$OverallCond, train$OverallQual)
ggplot(train, aes(x = OverallQual, y = SalePrice)) +
  geom_point() + geom_smooth()
ggplot(train, aes(x = OverallCond, y = SalePrice)) +
  geom_point() + geom_smooth()
cor(train$OverallQual,train$SalePrice)
cor(train$OverallCond,train$SalePrice)


#MasVnrType and MasVnrArea
rbind(table(train$MasVnrType), table(test$MasVnrType))
#train[which(is.na(train$MasVnrType)),]$MasVnrArea = 0
#test[which(is.na(test$MasVnrType)),]$MasVnrArea = 0
#train[which(is.na(train$MasVnrType)),]$MasVnrType = 'None'
#test[which(is.na(test$MasVnrType)),]$MasVnrType = 'None'
ggplot(data = train, aes(x = MasVnrType, y = SalePrice, alpha = 0.2)) + geom_point()
ggplot(data = train, aes(x = MasVnrType, y = SalePrice, alpha = 0.2)) + geom_boxplot()
ggplot(data = train, aes(x = MasVnrType, y = SalePrice, alpha = 0.2)) + geom_violin()
ggplot(data = train, aes(x = MasVnrArea, y = SalePrice, alpha = 0.2)) + geom_point() + geom_smooth()
nrow(train[which(train$MasVnrArea == 0),])
nrow(test[which(test$MasVnrArea == 0),])
ggplot(data = complete_data, aes(x = MasVnrArea))  + geom_density()


#MSZoning and Neighborhood
rbind(table(train$MSZoning), table(test$MSZoning))
sum(is.na(test$MSZoning))
ggplot(train, aes(x = MSZoning, y = SalePrice)) + geom_boxplot()
test[is.na(test$MSZoning),]$Neighborhood
ggplot(train, aes(x = MSZoning,y = SalePrice)) + geom_boxplot() +
  facet_wrap( ~ Neighborhood)
table(complete_data[which(complete_data$Neighborhood == 'IDOTRR'),]$MSZoning)
table(complete_data[which(complete_data$Neighborhood == 'Mitchel'),]$MSZoning)
#Similar to Neighborhood
#Neighborhood have no missing value

y = cat_num(x = train$Neighborhood)
cor(as.numeric(y), train$SalePrice)


#GarageCars, GarageArea
rbind(table(train$GarageCars), table(test$GarageCars))
ggplot(train, aes(x = GarageArea, y = SalePrice)) + geom_point()
cor(train$GarageArea, train$SalePrice)
cor(train$GarageCars, train$SalePrice)
sum(is.na(test$GarageArea))
sum(is.na(test$GarageCars))
test[is.na(test$GarageCars),]$GarageArea #test[is.na(test$GarageArea),]$GarageCars both are same
test[is.na(test$GarageCars),]$GarageType
ggplot(complete_data, aes(x = GarageCars, y = GarageArea)) + geom_point()
t.test(complete_data[which(complete_data$GarageCars == 3),]$GarageArea, conf.level = .99)
t.test(complete_data[which(complete_data$GarageCars == 2),]$GarageArea, conf.level = 0.99)
cor(train$GarageArea, train$GrLivArea)
fit <- lm(GarageCars ~ GrLivArea, data = train)
predict(fit, newdata = data.frame(GrLivArea = c(test[is.na(test$GarageCars),]$GrLivArea)))
test[is.na(test$GarageCars),]$GarageCars = 2
test[is.na(test$GarageArea),]$GarageArea = 519
median(complete_data[which(complete_data$GarageCars == 2),]$GarageArea)
#drop garage area


#GarageQual and GarageCond
table(complete_data$GarageCond)
table(complete_data$GarageQual)
test[is.na(test$GarageCond),]
train[is.na(train$GarageCond),]$GarageCond = 'None'
train[is.na(train$GarageQual),]$GarageQual = 'None'
train[is.na(train$GarageFinish),]$GarageFinish = 'None'
name = c("None", "Po",   "Fa",   "TA",   "Gd",   "Ex" )
y = cat_num(train$GarageCond, name)
cor(y, train$SalePrice)
x = cat_num(train$GarageQual, name)
cor(y, train$SalePrice)
plot(y, train$SalePrice)
name_finsih = c('Fin','RFn', 'Unf', 'None')
z = cat_num(train$GarageFinish, name_finsih)
cor(z,train$SalePrice)
table(train$GarageFinish)
ggplot(train, aes(x = GarageFinish, y = SalePrice)) + geom_point()
#Drop both column. High leverage, all cor is because of TA
#0.95 cor in Qual and Cond
#consider GaraegeFinish

#GarageType and GarageYrBlt
table(complete_data$GarageType)
ggplot(data = train, aes(x = GarageYrBlt, y = SalePrice)) + geom_line()+ geom_smooth()
#yr <60 constt price, yr> 60 there is increase in price
#main prob is to handle the the data with no garage as they will affect the trend if we
#add 0 in GarageYrBlt column
#make only 3 var,i.e away, home, none



#Bsmt
#BsmtCond and BsmtQual
train[is.na(train$BsmtQual),]$BsmtQual = 'None'
table(train$BsmtQual)
y <- cat_num(train$BsmtQual, name)
cor(y, train$SalePrice)
ggplot(train, aes(x = y, y = SalePrice)) + geom_point() +
  geom_smooth()
test[is.na(test$BsmtQual),]$BsmtQual = 'None'
table(test$BsmtQual)

train[is.na(train$BsmtCond),]$BsmtCond = "None"
table(train$BsmtCond)
y <- cat_num(train$BsmtCond, name)
cor(y, train$SalePrice)



