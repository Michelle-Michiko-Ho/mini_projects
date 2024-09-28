##########################################
###Instructor Name: Alfonso Berumen
###Student    Name: Ming-chun Michelle Ho
###Course     Name: Machine Learning in R
###Quarter    Term: Winter 2020
###Content        : Final Project
##########################################

library(DataExplorer)
library(nycflights13)
library(tidyverse)
library(tidymodels)
library(skimr)
#install.packages("skimr")
#install.packages("installr")
#library(installr)
#install.pandoc()
library(tm)
library(wordcloud)
library(RColorBrewer)
library(SnowballC)
library(sqldf)
library(scatterplot3d)
library(corrplot)
library(raster)
library(graphics)
require(stats)
library(lattice)
library(plotly)
library(dplyr)
library(ggplot2)
library(hrbrthemes)
library(gridExtra)
library(cowplot)
library(cluster)
library(factoextra)
library(GGally)
library(randomForest)
library('boot')


data_all <-read.csv("ST_all_data.csv")
data_hist <- subset(data_all, type=="HIST")

#EDA: complete stats.(packages)
DataExplorer::create_report(data_hist)
skimr::skim(data_hist)
dplyr::glimpse(data_hist)
typeof(data_hist$prime_r)   #<dbl> represents double!

plot_qq(data_hist[,4:19])
plot_qq(data_hist[,c(14:19, 30:32)])
plot_qq(data_hist[,21:29])

#EDA: individual stats.
str(data_hist)
summary(data_hist)
head(data_hist, 10)   #default n=6
tail(data_hist)
mean(data_hist$infla_r_cpi, na.rm=TRUE)
quantitle(data_hist$infla_r_cpi, probs=seq(0,1,0.25), na.rm=TRUE)
quantitle(data_hist$infla_r_cpi, probs=seq(0,1,0.125), na.rm=TRUE)
fivenum(data_hist$infla_r_cpi, na.rm=TRUE)

#Correlation Analysis
cor(data_hist[,c(4:19, 21:32)]) 

m <- cor(data_hist[,c(4:19)])  #stage1 evaluatation to drop variables
m2 <- cor(data_hist[,c(4,21:32)]) #stage2 evaluation to drop variables

corrplot(m, method ="number")
corrplot(m2, method ="number")

##independent variables to drop due to the weak relationships 
#w/ inflation rate!

#gdpg_r dig_r
#treasr_y_5yr treasr_y_10yr	
#type
#exchg_r_eu	exchg_r_apac exchg_r_jp

##After dropping variables
m3 <- cor(data_hist[,c(4,6,8:10,13:19, 21:22, 24:25,27:28,30:32)]) 
corrplot(m3, method ="number")
x<-cor(data_hist[,c(4,6,8:10,13:19, 21:22, 24:25,27:28,30:32)],data_hist[4])
format(round(x,2), nsmall=2)

##Trend Graph

df <- data_hist %>%
  select(date_comp1,GDP_nominal, Inflation) %>%
  gather(key = "Indicator", value = "value", -date_comp1)
#head(df, 3)

tail(df %>% gather ("GDP_nominal","Inflation",-date_comp1))
#typeof(data_hist)


ggplot(df, aes(x = date_comp1, y = value))+
  geom_line(aes(color = Indicator), size = 1) +
  scale_color_manual(values = c("dodgerblue","gold" )) +
  ggtitle("UC CPI Inflation Rate vs US Nominal GPD Rate\n(2000 to 2017)")+
  theme(plot.title=element_text(hjust=0.5, face="bold"))+
  theme_minimal()

df1 <- data_hist %>%
  select(date_comp1,Inflation_real_eu, Inflation) %>%
  gather(key = "Indicator", value = "value", -date_comp1)

G1<-ggplot(df1, aes(x = date_comp1, y = value))+
  geom_line(aes(color = Indicator), size = 1) +
  scale_color_manual(values = c("dodgerblue","gold" )) +
  ggtitle("Inflation Rate Comparison\n US vs EU\n(2000 to 2017)")+
  theme(plot.title=element_text(hjust=0.5, face="bold"))+
  theme_minimal()

df2 <- data_hist %>%
  select(date_comp1,Inflation_real_apac, Inflation) %>%
  gather(key = "Indicator", value = "value", -date_comp1)

G2<-ggplot(df2, aes(x = date_comp1, y = value))+
  geom_line(aes(color = Indicator), size = 1) +
  scale_color_manual(values = c("dodgerblue","green")) +
  ggtitle("Inflation Rate Comparison\n US vs APAC\n(2000 to 2017)")+
  theme(plot.title=element_text(hjust=0.5, face="bold"))+
  theme_minimal()

df3 <- data_hist %>%
  select(date_comp1,Inflation_real_jp,Inflation) %>%
  gather(key = "Indicator", value = "value", -date_comp1)

G3<-ggplot(df3, aes(x = date_comp1, y = value))+
  geom_line(aes(color = Indicator), size = 1) +
  scale_color_manual(values = c("dodgerblue","magenta2" )) +
  ggtitle("Inflation Rate Comparison\n US vs Japan\n(2000 to 2017)")+
  theme(plot.title=element_text(hjust=0.5, face="bold"))+
  theme_minimal()

df4 <- data_hist %>%
  select(date_comp1,Inflation_real_uk, Inflation) %>%
  gather(key = "Indicator", value = "value", -date_comp1)

G4<-ggplot(df4, aes(x = date_comp1, y = value))+
  geom_line(aes(color = Indicator), size = 1) +
  scale_color_manual(values = c("dodgerblue","red" )) +
  ggtitle("Inflation Rate Comparison\n US vs UK\n(2000 to 2017)")+
  theme(plot.title=element_text(hjust=0.5, face="bold"))+
  theme_minimal()

grid.arrange(G1,G2,G3,G4,nrow=2)

par(mfrow = c(3,2))
boxplot(data_hist$Inflation,
        main = "Inflation Rate (2000 - 2017)",
        xlab = "Range",
        ylab = "Inflation Rate",
        col = "yellow",
        border = "brown",
        horizontal = TRUE,
        notch = TRUE,
	  ylab =expression(bold("Inflation Rate"))	
)
boxplot(data_hist$GDP_nominal,
        main = "Nominal GDP Rate (2000 - 2017)",
        xlab = "Range",
	  col = "dodgerblue",
        border = "brown",
        horizontal = TRUE,
        notch = TRUE,
	  ylab =expression(bold("Nominal GDP Rate"))	
)
boxplot(data_hist$DispIncome_nominal,
        main = "Nominal Disposable Income Rate (2000 - 2017)",
        xlab = "Range",
        col = "lightblue",
        border = "brown",
        horizontal = TRUE,
        notch = TRUE,
	  ylab =expression(bold("Nominal Disposable Income Rate"))	
)

boxplot(data_hist$Unemployment,
        main = "Unemployment Rate (2000 - 2017)",
        xlab = "Range",
        col = "pink",
        border = "brown",
        horizontal = TRUE,
        notch = TRUE,
	  ylab =expression(bold("Unemployment Rate"))	
)
boxplot(data_hist$Treasury3Mon,
        main = "3-Month Treasury Rate (2000 - 2017)",
        xlab = "Range",
        col = "purple",
        border = "brown",
        horizontal = TRUE,
        notch = TRUE,
	  ylab =expression(bold("3-Month Treasury Rate"))	
)

boxplot(data_hist$BBBCorpYd,
        main = "BBB Corporate Yield Rate (2000 - 2017)",
        xlab = "Range",
        col = "cyan",
        border = "brown",
        horizontal = TRUE,
        notch = TRUE,
	  ylab =expression(bold("BBB Corporate Yield Rate"))	
)

par(mfrow = c(3,2))
boxplot(data_hist$Inflation,
        main = "US Inflation Rate (2000 - 2017)",
        xlab = "Range",
        ylab = "Inflation Rate",
        col = "yellow",
        border = "brown",
        horizontal = TRUE,
        notch = TRUE,
	  ylab =expression(bold("Inflation Rate"))	
)
boxplot(data_hist$Inflation_real_eu,
        main = "EU Inflation Rate (2000 - 2017)",
        xlab = "Range",
        ylab = "Inflation Rate",
        col = "gray",
        border = "brown",
        horizontal = TRUE,
        notch = TRUE,
	  ylab =expression(bold("Inflation Rate"))	
)
boxplot(data_hist$Inflation_real_apac,
        main = "APAC Inflation Rate (2000 - 2017)",
        xlab = "Range",
        ylab = "Inflation Rate",
        col = "gold",
        border = "brown",
        horizontal = TRUE,
        notch = TRUE,
	  ylab =expression(bold("Inflation Rate"))	
)
boxplot(data_hist$Inflation_real_jp,
        main = "JP Inflation Rate (2000 - 2017)",
        xlab = "Range",
        ylab = "Inflation Rate",
        col = "lightpink",
        border = "brown",
        horizontal = TRUE,
        notch = TRUE,
	  ylab =expression(bold("Inflation Rate"))	
)

boxplot(data_hist$Inflation_real_uk,
        main = "UK Inflation Rate (2000 - 2017)",
        xlab = "Range",
        ylab = "Inflation Rate",
        col = "lightgreen",
        border = "brown",
        horizontal = TRUE,
        notch = TRUE,
	  ylab =expression(bold("Inflation Rate"))	
)

#####################################
###### Modeling Method and Technique
#####################################

#1. Linear Regression

m1<-lm(Inflation~GDP_real+GDP_nominal+DispIncome_real+DispIncome_nominal+Unemployment+
	Treasury3Mon+Treasury5Yr+Treasury10Yr+BBBCorpYd+Mortgage+Prime+DowJonesIndx+
	HousePriceIndx+ComlRealEstIndx+VIX+GDP_real_eu+Inflation_real_eu+Exchange_eu+
	GDP_real_apac+Inflation_real_apac+Exchange_apac+GDP_real_jp+Inflation_real_jp+
	Exchange_jp+GDP_real_uk+Inflation_real_uk+Exchange_uk, data=data_hist)
summary(m1)

#Drop Real GDP/DIG/10YrTreasuryYield vs replace 10Yr w/ 5Yr (similar result)

m2<-lm(Inflation~GDP_nominal+DispIncome_nominal+Unemployment+
	Treasury3Mon+Treasury5Yr+BBBCorpYd+Mortgage+Prime+DowJonesIndx+
	HousePriceIndx+ComlRealEstIndx+VIX+GDP_real_eu+Inflation_real_eu+Exchange_eu+
	GDP_real_apac+Inflation_real_apac+Exchange_apac+GDP_real_jp+Inflation_real_jp+
	Exchange_jp+GDP_real_uk+Inflation_real_uk+Exchange_uk, data=data_hist)

summary(m2)

#Drop 5YrTreasury (AdjR increased/Up)

m2<-lm(Inflation~GDP_nominal+DispIncome_nominal+Unemployment+
	Treasury3Mon+Treasury5Yr+BBBCorpYd+Mortgage+Prime+DowJonesIndx+
	HousePriceIndx+ComlRealEstIndx+VIX+GDP_real_eu+Inflation_real_eu+Exchange_eu+
	GDP_real_apac+Inflation_real_apac+Exchange_apac+GDP_real_jp+Inflation_real_jp+
	Exchange_jp+GDP_real_uk+Inflation_real_uk+Exchange_uk, data=data_hist)

m3<-lm(Inflation~GDP_nominal+DispIncome_nominal+Unemployment+
	Treasury3Mon+BBBCorpYd+Mortgage+Prime+DowJonesIndx+
	HousePriceIndx+ComlRealEstIndx+VIX+GDP_real_eu+Inflation_real_eu+Exchange_eu+
	GDP_real_apac+Inflation_real_apac+Exchange_apac+GDP_real_jp+Inflation_real_jp+
	Exchange_jp+GDP_real_uk+Inflation_real_uk+Exchange_uk, data=data_hist)

summary(m3)

#Drop Exchange Rate (1:UK, AdjR Up; 2:JP, Up; 3:APAC, Up; 4:EU, Down, then Keep)

m4<-lm(Inflation~GDP_nominal+DispIncome_nominal+Unemployment+
	Treasury3Mon+BBBCorpYd+Mortgage+Prime+DowJonesIndx+
	HousePriceIndx+ComlRealEstIndx+VIX+GDP_real_eu+Inflation_real_eu+
	Exchange_eu+GDP_real_apac+Inflation_real_apac+GDP_real_jp+Inflation_real_jp+
	GDP_real_uk+Inflation_real_uk, data=data_hist)

summary(m4)

#Drop Infla Rate (1:UK, AdjR Down; 2:JP, Down; 3:APAC, Up; 4:EU, Down. If Down,Keep)

m5<-lm(Inflation~GDP_nominal+DispIncome_nominal+Unemployment+
	Treasury3Mon+BBBCorpYd+Mortgage+Prime+DowJonesIndx+
	HousePriceIndx+ComlRealEstIndx+VIX+GDP_real_eu+Inflation_real_eu+
	Exchange_eu+GDP_real_apac+GDP_real_jp+Inflation_real_jp+
	GDP_real_uk+Inflation_real_uk, data=data_hist)

summary(m5)

#Drop GDP Rate (1:UK, AdjR Down; 2:JP, Up; 3:APAC, Up; 4:EU, Down. If Down,Keep)
#Evalute the number of stars as well to determine Keep or Drop
#EU GDP though Down, not much difference in AdjR, 1 star dropped though!

m6<-lm(Inflation~GDP_nominal+DispIncome_nominal+Unemployment+
	Treasury3Mon+BBBCorpYd+Mortgage+Prime+DowJonesIndx+
	HousePriceIndx+ComlRealEstIndx+VIX+GDP_real_eu+Inflation_real_eu+
	Exchange_eu+Inflation_real_jp+
	GDP_real_uk+Inflation_real_uk, data=data_hist)

summary(m6)

#Drop Indices (1:VIX, AdjR Up; 2:DJI, Up; 3:Com'lReal Estate, Down(yet star Up by 3),
#thus drop; 4:HousePriceIndex, Down (both). If Down,Keep)
#Evalute the number of stars as well to determine Keep or Drop
#EU GDP though Down, not much difference in AdjR, 1 star dropped though!

m7<-lm(Inflation~GDP_nominal+DispIncome_nominal+Unemployment+
	Treasury3Mon+BBBCorpYd+Mortgage+Prime+
	HousePriceIndx+GDP_real_eu+Inflation_real_eu+
	Exchange_eu+Inflation_real_jp+
	GDP_real_uk+Inflation_real_uk, data=data_hist)
summary(m7)

#Drop International (1:UK Infla, AdjR Down (a bit while stars remain);
#2:UK GDP, Down (a bit, stars drop 1); 3:Just US indicator (N=8, AdjR=0.24, 3 stars)
#US Economy is connected with these regions sig.
#thus drop; 4:HousePriceIndex, Down (both). If Down,Keep)
#Evalute the number of stars as well to determine Keep or Drop
#EU GDP though Down, not much difference in AdjR, 1 star dropped though!

m8<-lm(Inflation~GDP_nominal+DispIncome_nominal+Unemployment+Treasury3Mon+
		Mortgage+Prime+HousePriceIndx+GDP_real_eu+Inflation_real_eu+Exchange_eu+
		Inflation_real_jp, data=data_hist)
summary(m8)

m8_cor <- cor(data_hist[,c(4,6,8:10, 14:15, 17, 21:23, 28)]) 
corrplot(m8_cor, method ="number")

#PCA
apply(data_hist[,c(4:19,21:32)], 2, mean)
apply(data_hist[,c(4:19,21:32)],2,var)
pr.out=prcomp(data_hist[,c(4:19,21:32)], scale=TRUE)
names(pr.out)
pr.out$center
#biplot(pr.out, scale=0)
pr.var=pr.out$sdev^2
pve=pr.var/sum(pr.var)
plot(pve, xlab="Principal Component", ylab="Proportion of Variance Explained", ylim=c(0,1),type='b')
plot(cumsum(pve), xlab="Principal Component", ylab="Cumulative Proportion of Variance Explained", ylim=c(0,1),type='b')
#fviz_eig(pr.out)

fviz_pca_var(pr.out,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
             )

#RandomForest
data_rf <- read.csv("ST_all_data_rf.csv")

RNGkind(sample.kind = "Rounding")
set.seed(10000)

sample = sample.int(n = nrow(data_rf), size = floor(.75*nrow(data_rf)),
 replace = F)
train = data_rf[sample, ] #just the samples
test  = data_rf[-sample, ] #everything but the samples

# Verify train and test sets size
nrow(train) + nrow(test) == nrow(data_rf)


glm_hist = glm(Inflation~GDP_nominal+DispIncome_nominal+Unemployment+Treasury3Mon+
		Mortgage+Prime+HousePriceIndx+GDP_real_eu+Inflation_real_eu+Exchange_eu+
		Inflation_real_jp, data=data_rf)
k_fold_cv_error = cv.glm(data_rf , glm_hist, K=5)
k_fold_cv_error$delta

glm_cv_rmse = sqrt(k_fold_cv_error$delta)[1]
glm_cv_rmse 

#RandomForest classifier

names(train)

RNGkind(sample.kind = "Rounding")
set.seed(10000)

train_y = train[,1]    # Data frame
train_x = train[,-1]  # Numeric vector

head(train_y)
head(train_x)

rf = randomForest(x=train_x, y=train_y , ntree=500, importance=TRUE)

rf$importance

oob_prediction = predict(rf) 

train_mse = mean(as.numeric((oob_prediction - train_y)^2))
oob_rmse = sqrt(train_mse)
oob_rmse
#[1]1.623187

test_y = test[,1]
test_x = test[,-1]

y_pred = predict(rf, test_x)
test_mse = mean(((y_pred - test_y)^2))
test_rmse = sqrt(test_mse)
test_rmse
#[1] 0.9858469


#Word Cloud

data_word <-readLines("ML_FinalProject.txt")
data_word<-toupper(data_word)

# gsub(<<chr to replace>>, <<chr to replace with>>, <<text vector>>)
data_word<-gsub("!", "", hmtxt)
data_word<-gsub("-", " ", hmtxt)
data_word<-gsub("'", " ", hmtxt)
data_word<-gsub("\\.", "", hmtxt)	## Note, we'll see an error here if we use "."
data_word<-gsub("?", "", hmtxt)		## \\ is the escape character in R
data_word<-gsub("\\)", "", hmtxt)
data_word<-gsub("\\(", "", hmtxt)

newhmcorpus<-Corpus(VectorSource(data_word))
inspect(newhmcorpus)
newhmcorpus<- tm_map(newhmcorpus, removeNumbers)
newhmcorpus<- tm_map(newhmcorpus, removeWords, c("a","it"))
newhmcorpus<- tm_map(newhmcorpus, removeWords, stopwords("english"))
newhmcorpus<- tm_map(newhmcorpus, removePunctuation)
newhmcorpus<- tm_map(newhmcorpus, stripWhitespace)

## Matrix of words, how many words are in each line
termdocmat <- TermDocumentMatrix(newhmcorpus, control=list(tolower = FALSE))
wordmat <- as.matrix(termdocmat)

## Tally up the lines
wordmat_tally <- sort(rowSums(wordmat),decreasing=TRUE)
worddat <- data.frame(word = names(wordmat_tally),freq=wordmat_tally)
head(worddat)

RNGkind(sample.kind = "Rounding")
set.seed(1114)		
wordcloud(words = worddat$word, freq = worddat$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors = c("dodgerblue","red","green","gold","cyan","purple","yellow","navyblue","pink",
		"skyblue","orange","darkblue","magenta4"))


###################################################################################################


