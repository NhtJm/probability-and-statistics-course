# import library
# List of packages to install
packages <- c(
  "dplyr",
  "ggplot2",
  "ggfortify",
  "ggcorrplot",
  "broom",
  "rio",
  "anytime",
  "knitr",
  "naniar",
  "lubridate",
  "car",
  "stats",
  "VIM",
  "magrittr",
  "ggpubr",
  "corrplot",
  "hexbin",
  "MASS",
  "zoo"
)

# Install packages that are not yet installed
new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
if (length(new_packages) > 0) {
  install.packages(new_packages)
}
# import library
pacman::p_load(
  dplyr,
  ggplot2,
  ggfortify,
  ggcorrplot,
  broom,
  rio,
  anytime,
  knitr,
  naniar,
  lubridate,
  car,
  stats,
  VIM,
  magrittr,
  ggpubr,
  corrplot,
  hexbin,
  MASS,
  zoo
)

################
#useful function
hbplot <- function(df, col, na.rm = T) { 
  par(mfrow = c(1,2))
  
  hist(df[[col]],
       main = paste("Histogram of", col),
       xlab = col)
  
  boxplot(df[[col]],
          main = paste("Boxplot of", col),
          horizontal = T,
          xlab = col,
          ylab = "",
          las = 1)}
################
#load dataset
all_gpus <- read.csv("All_GPUs.csv")
all_gpus$Release_Year <- year(dmy(all_gpus$Release_Date))

data = all_gpus %>%
  select("Manufacturer","Release_Year","Boost_Clock","Core_Speed","Max_Power","Memory","Memory_Bandwidth","Memory_Bus","Memory_Speed","Release_Price","Shader","TMUs")
summary(data)
#conversion
data <- data %>%
  mutate(
    Max_Power = as.numeric(gsub("[^0-9.]","", Max_Power)),
    Boost_Clock = as.numeric(gsub("[^0-9.]", "", Boost_Clock)),
    Core_Speed = as.numeric(gsub("[^0-9.]","", Core_Speed)),
    Memory = as.numeric(gsub("[^0-9.]","", Memory)),
    Memory_Bandwidth = as.numeric(gsub("[^0-9.]","", Memory_Bandwidth)),
    Memory_Bus = as.numeric(gsub("[^0-9.]","", Memory_Bus)),
    Memory_Speed = as.numeric(gsub("[^0-9.]", "", Memory_Speed)),
    Release_Price = as.numeric(gsub("[^0-9.]", "", Release_Price)),
    Shader = as.numeric(gsub("[^0-9.]", "", Shader)), 
    TMUs = as.numeric(gsub("[^0-9.]", "", TMUs)),
  )
head(data,n = 5)
#check NA in selected data
missing_counts <- colSums(is.na(data)) 
missing_counts
#applying k-nearest neighbor to predict missing values
df <- kNN(data, k = sqrt(nrow(data)), imp_var = FALSE)
summary(df)
missing_counts <- colSums(is.na(df)) 
missing_counts
#export for later use
export(df,"gpu_clean.csv")
###############
#descriptive statistics
df <- read.csv("gpu_clean.csv")

my_table=(table(df$Manufacturer))
cat("Number of Manufacturers :",nrow(my_table))
show(my_table)
my_table=(table(df$Release_Year))
cat("Total years :",nrow(my_table))
show(my_table)

hbplot(df, "Boost_Clock")
hbplot(df, "Core_Speed")
hbplot(df, "Max_Power")
hbplot(df, "Memory")
hbplot(df, "Memory_Bandwidth")
hbplot(df, "Memory_Bus")
hbplot(df, "Memory_Speed")
hbplot(df, "Release_Price")
hbplot(df, "Shader")
hbplot(df, "TMUs")

df['Max_Power'] <- log(df['Max_Power'])
df['Boost_Clock'] <- log(df['Boost_Clock'])
df['Core_Speed'] <- log(df['Core_Speed'])
df['Memory'] <- log(df['Memory'])
df['Memory_Bandwidth'] <- log(df['Memory_Bandwidth'])
df['Memory_Bus'] <- log(df['Memory_Bus'])
df['Memory_Speed'] <- log(df['Memory_Speed'])
df['Release_Price'] <- log(df['Release_Price'])
df['Shader'] <- log(df['Shader'])
df['TMUs'] <- log(df['TMUs'])


hbplot(df, "Boost_Clock")
hbplot(df, "Core_Speed")
hbplot(df, "Max_Power")
hbplot(df, "Memory")
hbplot(df, "Memory_Bandwidth")
hbplot(df, "Memory_Bus")
hbplot(df, "Memory_Speed")
hbplot(df, "Release_Price")
hbplot(df, "Shader")
hbplot(df, "TMUs")


cor_matrix = cor(df[, 3:ncol(df)])
corrplot(cor_matrix,
         method = "pie",
         addCoef.col = "red",
         tl.cex = 0.8,           
         addCoefasPercent = TRUE, 
         number.cex = 0.8,      
         mar = c(0, 0, 0, 0),    
         cl.pos = "r",            
         cl.ratio = 0.2,          
         cl.offset = 1.3,)    
cor_matrix
##########################
#inferential statistics
df <- read.csv("gpu_clean.csv")

df['Max_Power'] <- log(df['Max_Power'])
df['Boost_Clock'] <- log(df['Boost_Clock'])
df['Core_Speed'] <- log(df['Core_Speed'])
df['Memory'] <- log(df['Memory'])
df['Memory_Bandwidth'] <- log(df['Memory_Bandwidth'])
df['Memory_Bus'] <- log(df['Memory_Bus'])
df['Memory_Speed'] <- log(df['Memory_Speed'])
df['Release_Price'] <- log(df['Release_Price'])
df['Shader'] <- log(df['Shader'])
df['TMUs'] <- log(df['TMUs'])

df <- df[df$Release_Price < 8,]
df <- df[df$Release_Price > 3.5,]

#predict: 
set.seed(50)
# use 80% of dataset as training and 20% as testing
sample <- sample(c(T, F), nrow(df), replace=T, prob=c(0.8,0.2))

#anova
aovRelease_Price <- select(df, Manufacturer, Release_Price)

aov_one <- aov(Release_Price~Manufacturer, data=aovRelease_Price)
aov_one
summary(aov_one)

ggqqplot(aovRelease_Price$Release_Price)

aovRelease_Price$Manufacturer <- as.factor(aovRelease_Price$Manufacturer)
leveneTest(Release_Price ~ Manufacturer, data = aovRelease_Price, center=mean)

oneway.test(Release_Price ~ Manufacturer, data = aovRelease_Price)

#####
#MLR
data = df %>%
  select("Boost_Clock","Core_Speed","Max_Power","Memory","Memory_Bandwidth","Memory_Bus","Memory_Speed","Release_Price","Shader","TMUs")
train <- data[sample, ]
test <- data[!sample, ]

mdl_price_vs_all = lm(Release_Price~., data=train)
summary(mdl_price_vs_all)

par(mfrow=c(1, 2))
ggplot(mdl_price_vs_all, aes(x = resid(mdl_price_vs_all), y=..density..)) + theme_classic() +  geom_density() +
  geom_histogram(bins=100, fill = 'steelblue', color = 'black') +
  labs(title = 'Histogram of Residuals', x = 'Residuals', y =
         'Frequency') +
  theme(legend.position='bottom', plot.title = element_text(hjust = 0.5))

plot(mdl_price_vs_all, which = 3)
plot(mdl_price_vs_all, which = 2)
coef(mdl_price_vs_all)

comtab.lr <- test['Release_Price']
comtab.lr['predicted_RP'] <- as.data.frame(predict(mdl_price_vs_all, newdata = test))
# Plotting
# The majority of points lie near the line, so its ok. 
ggplot(comtab.lr, aes(x = Release_Price, y = predicted_RP)) +
  geom_point(shape=1, color="blue") +
  geom_abline(mapping=aes(intercept= 0, slope = 1), color="darkblue") +
  labs(x = "log(Release_Price)", y = "Predicted")

pred <- data.frame(predict(mdl_price_vs_all, newdata = test))
compare <- cbind(test$Release_Price,pred)
colnames(compare) <- c("test_set","prediction")
head(compare,10)

SSE <- sum((test$Release_Price - pred)^2)
SST <- sum((test$Release_Price - mean(test$Release_Price))^2)
cat("The accuracy of the model on test set: " , round((1 - SSE / SST )* 100 ,2) , "%" )


#comtab.lr <- test['Release_Price']
#comtab.lr['predicted_RP'] <- as.data.frame(predict(mdl_price_vs_all, newdata = test))

#ggplot(comtab.lr, aes(x = exp(Release_Price), y = exp(predicted_RP))) +
#geom_point(shape=1, color="blue") +
 # geom_abline(mapping=aes(intercept= 0, slope = 1), color="darkblue") +
  #labs(x = "Release_Price", y = "Predicted")
############################
#SVM
library(e1071)
svm_model = svm(Release_Price~. , data=train)
summary(svm_model)
svm_prediction_linear <- predict(svm_model)
pred_svm <- data.frame(predict(svm_model, newdata = test))
compare_2 <- cbind(test$Release_Price,pred_svm)
colnames(compare_2) <- c("test_set","prediction")
head(compare_2,10)
# Calculate R-squared manually for SVM model
predictions_svm <- predict(svm_model, newdata = test)
SSE_svm <- sum((predictions_svm - test$Release_Price)^2)
SST_svm <- sum((test$Release_Price - mean(test$Release_Price))^2)
r2_score <- 1 - SSE_svm / SST_svm
print(paste("The accuracy of the model on test set: ",r2_score))
comtab.svm <- test['Release_Price']
comtab.svm['predicted_RP'] <- as.data.frame(predict(svm_model, newdata = test))
# Plotting
residuals <- comtab.svm$Release_Price - comtab.svm$predicted_RP
qqnorm(residuals)
qqline(residuals)
# The majority of points lie near the line, so its ok. 
ggplot(comtab.svm, aes(x = Release_Price, y = predicted_RP)) +
  geom_point(shape=1, color="blue") +
  geom_abline(mapping=aes(intercept= 0, slope = 1), color="darkblue") +
  labs(x = "log(Release_Price)", y = "Predicted_RP")

#######################
library("randomForest")
model.rfr <- randomForest(formula = Release_Price ~ ., data = train, ntree = 500)
print(model.rfr)


comtab.rfr <- test['Release_Price']
comtab.rfr['RP_predicted'] <- as.data.frame(predict(model.rfr, newdata = test), row.names = NULL)
# Evaluate model performance
SSE <- sum((comtab.rfr$Release_Price - comtab.rfr$RP_predicted)^2)
SST <- sum((comtab.rfr$Release_Price - mean(comtab.rfr$Release_Price))^2)
cat("The accuracy of the model on test set: " , round((1 - SSE / SST )* 100 ,2) , "%" )


residuals2 <- comtab.rfr$Release_Price - comtab.rfr$RP_predicted
qqnorm(residuals2)
qqline(residuals2)

# Plot the predicted - actual
ggplot(comtab.rfr, aes(x = Release_Price, y = RP_predicted)) +
  geom_point(shape = 1, color = "blue") +
  geom_abline(mapping = aes(intercept = 0, slope = 1), color = "darkblue") +
  labs(x = "log(Release_Price)", y = "RP Predicted")
###############################################




model1 <- mdl_price_vs_all #multi-linear 
model2 <- svm_model #svm
model3 <- model.rfr #random forest
predictions_m1 <- predict(model1, test)
predictions_m2 <- predict(model2, test)
predictions_m3 <- predict(model3, test)
# Calculate RMSE (Root Mean Squared Error)
rmse_m1 <- sqrt(mean((test$Release_Price - predictions_m1)^2))
rmse_m2 <- sqrt(mean((test$Release_Price - predictions_m2)^2))
rmse_m3 <- sqrt(mean((test$Release_Price - predictions_m3)^2))
# Calculate MAE (Mean Absolute Error)
mae_m1 <- mean(abs(test$Release_Price - predictions_m1))
mae_m2 <- mean(abs(test$Release_Price - predictions_m2))
mae_m3 <- mean(abs(test$Release_Price - predictions_m3))

paste("RMSE for model1: ", rmse_m1)
paste("MAE for model1: ", mae_m1)

paste("RMSE for model2: ", rmse_m2)
paste("MAE for model2: ", mae_m2)

paste("RMSE for model3: ", rmse_m3)
paste("MAE for model3: ", mae_m3)



#cc03 - gr9


#library("RFpredInterval")
#out2 <- rfpi(formula = Release_Price ~ ., traindata = train,
 #            testdata = test, split_rule = "ls",pi_method = c("lm", "quant"),
  #           rf_package = "ranger", params_ranger = list(num.trees = 50))
## get the PI with "quant" method for the testdata
#cbind(exp(out2$quant_interval$lower), exp(out2$quant_interval$upper))

#out2 <- rfpi(formula = Release_Price ~ ., traindata = train,
 #            testdata = test, split_rule = "ls", pi_method = c("lm", "quant", "spi"),
  #           rf_package = "ranger", params_ranger = list(num.trees = 50))
#print(out2)

#cbind(exp(out2$quant_interval$lower), exp(out2$quant_interval$upper))







































