########################################
###Class: Introduction to Data Science
###Student Name : Ming-chun Michelle Ho
###DueDate: December 15, 2019
###Purpose: Final Project (Alternative)
########################################

#Q1. Access the Data Set

#The first step is to access the data set and load it into the R environment. Follow
#these steps in order to complete this step:
#• Download the housing.csv data set found in the homeworks folder in
#the class GitHub repository. The data set has 20,640 rows and 10 variables.
#• Read the data set into R using a data frame named housing.
#• Perform head() and tail() functions on the data frame to get a feel
#for the actual data values.
#• Perform a summary() function on the data frame to get a sense for the
#data classes, range of values for numeric variables, and levels for any factor
#variable.

#A1.

data <- read.csv("housing.csv")

dim(data)
	#[1] 20640    10

head(data)
	#  longitude latitude housing_median_age total_rooms total_bedrooms population
	#1   -122.23    37.88                 41         880            129        322
	#2   -122.22    37.86                 21        7099           1106       2401
	#3   -122.24    37.85                 52        1467            190        496
	#4   -122.25    37.85                 52        1274            235        558
	#5   -122.25    37.85                 52        1627            280        565
	#6   -122.25    37.85                 52         919            213        413
	#  households median_income median_house_value ocean_proximity
	#1        126        8.3252             452600        NEAR BAY
	#2       1138        8.3014             358500        NEAR BAY
	#3        177        7.2574             352100        NEAR BAY
	#4        219        5.6431             341300        NEAR BAY
	#5        259        3.8462             342200        NEAR BAY
	#6        193        4.0368             269700        NEAR BAY

tail(data)
	#	      longitude latitude housing_median_age total_rooms total_bedrooms
	#20635   -121.56    39.27                 28        2332            395
	#20636   -121.09    39.48                 25        1665            374
	#20637   -121.21    39.49                 18         697            150
	#20638   -121.22    39.43                 17        2254            485
	#20639   -121.32    39.43                 18        1860            409
	#20640   -121.24    39.37                 16        2785            616
	#      population households median_income median_house_value ocean_proximity
	#20635       1041        344        3.7125             116800          INLAND
	#20636        845        330        1.5603              78100          INLAND
	#20637        356        114        2.5568              77100          INLAND
	#20638       1007        433        1.7000              92300          INLAND
	#20639        741        349        1.8672              84700          INLAND
	#20640       1387        530        2.3886              89400          INLAND


summary(data)
	#> summary(data)
	#   longitude         latitude     housing_median_age  total_rooms   
	# Min.   :-124.3   Min.   :32.54   Min.   : 1.00      Min.   :    2  
	# 1st Qu.:-121.8   1st Qu.:33.93   1st Qu.:18.00      1st Qu.: 1448  
	# Median :-118.5   Median :34.26   Median :29.00      Median : 2127  
	# Mean   :-119.6   Mean   :35.63   Mean   :28.64      Mean   : 2636  
	# 3rd Qu.:-118.0   3rd Qu.:37.71   3rd Qu.:37.00      3rd Qu.: 3148  
	# Max.   :-114.3   Max.   :41.95   Max.   :52.00      Max.   :39320  
                                                                    
	# total_bedrooms     population      households     median_income    
	# Min.   :   1.0   Min.   :    3   Min.   :   1.0   Min.   : 0.4999  
	# 1st Qu.: 296.0   1st Qu.:  787   1st Qu.: 280.0   1st Qu.: 2.5634  
	# Median : 435.0   Median : 1166   Median : 409.0   Median : 3.5348  
	# Mean   : 537.9   Mean   : 1425   Mean   : 499.5   Mean   : 3.8707  
	# 3rd Qu.: 647.0   3rd Qu.: 1725   3rd Qu.: 605.0   3rd Qu.: 4.7432  
	# Max.   :6445.0   Max.   :35682   Max.   :6082.0   Max.   :15.0001  
	# NA's   :207                                                        
	# median_house_value   ocean_proximity
	# Min.   : 14999     <1H OCEAN :9136  
	# 1st Qu.:119600     INLAND    :6551  
	# Median :179700     ISLAND    :   5  
	# Mean   :206856     NEAR BAY  :2290  
	# 3rd Qu.:264725     NEAR OCEAN:2658  
	# Max.   :500001                      

 
#2. Data Visualization
#Now, let’s do some exploratory data analysis (EDA) using some visualizations.
#Follow these steps:
#• Create histograms for each numeric variable (e.g. not
#ocean_proximity).
#• Examine the plots and provide a commentary on what the visualizations
#reveal. 

	library(ggplot2)
	library(GGally)
	library(corrplot)
	options(scipen=999)

	ggscatmat(data,columns =c(1:9))
	
	require(gridExtra)
	hstgrm1 <-ggplot(data, aes(x=longitude)) + 
		 geom_histogram(binwidth =0.8, fill = "dodgerblue", col = "gold",
       	 alpha = 0.9)+
		 xlab("Longitude")+
		 ylab("Count")+
		 ggtitle("Longitude") +
		 theme(plot.title = element_text(size = 15, hjust=0.5),
			axis.title.x=element_text(hjust=0.5, face="bold"),
			axis.title.y=element_text(hjust=0.5, face="bold"),
			axis.text.y=element_text(face="bold"),
			axis.text.x=element_text(face="bold"),
	)

	hstgrm2<- ggplot(data, aes(x=latitude)) + 
		 geom_histogram(binwidth =0.8, fill = "navyblue", col = "gold",
       	 alpha = 0.9)+
		 xlab("Latitude")+
		 ylab("Count")+
		 ggtitle("Latitude") +
		 theme(plot.title = element_text(size = 15, hjust=0.5),
			axis.title.x=element_text(hjust=0.5, face="bold"),
			axis.title.y=element_text(hjust=0.5, face="bold"),
			axis.text.y=element_text(face="bold"),
			axis.text.x=element_text(face="bold"),
	)
	hstgrm2

	hstgrm3<- ggplot(data, aes(x=housing_median_age)) + 
		 geom_histogram(binwidth =0.8, fill = "darkgreen", col = "cyan",
       	 alpha = 0.9)+
		 xlab("Housing Median Age")+
		 ylab("Count")+
		 ggtitle("Housing Median Age") +
		 theme(plot.title = element_text(size = 15, hjust=0.5),
			axis.title.x=element_text(hjust=0.5, face="bold"),
			axis.title.y=element_text(hjust=0.5, face="bold"),
			axis.text.y=element_text(face="bold"),
			axis.text.x=element_text(face="bold"),
	)
	hstgrm3
	hstgrm4 <-ggplot(data, aes(x=total_rooms)) + 
		 geom_histogram(binwidth =1.2, fill = "forestgreen", col = "limegreen",
       	 alpha = 0.9)+
		 xlab("Total Rooms")+
		 ylab("Count")+
		 ggtitle("Total Rooms") +
		 theme(plot.title = element_text(size = 15, hjust=0.5),
			axis.title.x=element_text(hjust=0.5, face="bold"),
			axis.title.y=element_text(hjust=0.5, face="bold"),
			axis.text.y=element_text(face="bold"),
			axis.text.x=element_text(face="bold"),
	)
	
	grid.arrange(hstgrm1, hstgrm2, hstgrm3, hstgrm4, ncol=2, nrow=2)

	hstgrm5 <-ggplot(data, aes(x=total_bedrooms)) + 
		 geom_histogram(binwidth =1, fill = "cyan", col = "orange",
       	 alpha = 0.9)+
		 xlab("Total Bedrooms")+
		 ylab("Count")+
		 ggtitle("Total Bedrooms") +
		 theme(plot.title = element_text(size = 15, hjust=0.5),
			axis.title.x=element_text(hjust=0.5, face="bold"),
			axis.title.y=element_text(hjust=0.5, face="bold"),
			axis.text.y=element_text(face="bold"),
			axis.text.x=element_text(face="bold"),
	)

	hstgrm6<- ggplot(data, aes(x=population)) + 
		 geom_histogram(binwidth =0.8, fill = "green", col = "cyan",
       	 alpha = 0.9)+
		 xlab("Population")+
		 ylab("Count")+
		 ggtitle("Population") +
		 theme(plot.title = element_text(size = 15, hjust=0.5),
			axis.title.x=element_text(hjust=0.5, face="bold"),
			axis.title.y=element_text(hjust=0.5, face="bold"),
			axis.text.y=element_text(face="bold"),
			axis.text.x=element_text(face="bold"),
	)
	hstgrm6

	hstgrm7<- ggplot(data, aes(x=households)) + 
		 geom_histogram(binwidth =0.8, fill = "magenta", col = "magenta4",
       	 alpha = 0.9)+
		 xlab("Households")+
		 ylab("Count")+
		 ggtitle("Households") +
		 theme(plot.title = element_text(size = 15, hjust=0.5),
			axis.title.x=element_text(hjust=0.5, face="bold"),
			axis.title.y=element_text(hjust=0.5, face="bold"),
			axis.text.y=element_text(face="bold"),
			axis.text.x=element_text(face="bold"),
	)
	hstgrm7
	hstgrm8 <-ggplot(data, aes(x=median_income)) + 
		 geom_histogram(binwidth =1.2, fill = "slateblue4", col = "yellow",
       	 alpha = 0.9)+
		 xlab("Median Income")+
		 ylab("Count")+
		 ggtitle("Median Income") +
		 theme(plot.title = element_text(size = 15, hjust=0.5),
			axis.title.x=element_text(hjust=0.5, face="bold"),
			axis.title.y=element_text(hjust=0.5, face="bold"),
			axis.text.y=element_text(face="bold"),
			axis.text.x=element_text(face="bold"),
	)
	
	grid.arrange(hstgrm5, hstgrm6, hstgrm7, hstgrm8, ncol=2, nrow=2)

	hstgrm9 <-ggplot(data, aes(x=median_house_value)) + 
		 geom_histogram(binwidth =1.2, fill = "mediumorchid", col = "navy",
       	 alpha = 0.9)+
		 xlab("Median House Value")+
		 ylab("Count")+
		 ggtitle("Median House Value") +
		 theme(plot.title = element_text(size = 15, hjust=0.5),
			axis.title.x=element_text(hjust=0.5, face="bold"),
			axis.title.y=element_text(hjust=0.5, face="bold"),
			axis.text.y=element_text(face="bold"),
			axis.text.x=element_text(face="bold"),
	)
	hstgrm9	

	#Comments on each Histogram:
	#Overall, it's clear that the distributions are not normal/bell-shaped due to focuses on certain ranges (more counts) and/or extreme cases (outliers in relation to the majority), pulling the distributions to the right-tailed, in some.
	#Longitude: it appears there are certain Longitudes have more observations than others. (not normally distributed)
	#Latitude: Similarly, certain Latitudes have more observations than others.(not normally distributed)
	#Housing Median Age: it's interesting to see the extreme occurred in age 52 with more than 1200. Otherwise, it would have been like a well-balanced/bell-shaped distribution.
	#Total Rooms: it depicts Total Rooms with 10,000 and above can be the threshold causing the right-tailed distribution. By either evenly subseting the data or divide the data into two may be helpful to learn more characterics in this area.  
	#Total Bedrooms: In the same token as Total Rooms, it shows Total Bedrooms with 2,000 and above can be the cutoff to evaluate separately as to have a better understanding for the majority while looking at the extreme closely and strategically as well.  
	#Population: it shows the similar pattern as Total Bedrooms in that 10,000 can be the point to separate the data into two further evaluations as to take care of the data behaviours for the majority and the minority in details.
	#Households: Likewise - 2,000 can be the point to break the dataset into further investigation/reading in terms of the majority vs the extreme.
	#Median Income: Not normal distributed, tailed to the right due to few extreme cases where income is greated than 13 (the unit may be 10,000)
	#Median House Value: it appears to be quite a disproportion in quantitives for values under versus greater than or equal to $500,000.
	#                    It would be interesting to look into the break down by other variables to have a deeper understanding.
	

#3. Data Transformation
#The next step is to transform data as necessary. Follow these steps in order to
#complete this step:
#3.1 We see from the summary() results above that there are many NA values
#in the total_bedrooms variable (the only variable with missing values).
#This needs to be addressed by filling in missing values using imputation. You
#can use the “median” for missing total_bedrooms values. The median
#is used instead of mean because it is less influenced by extreme outliers.
#This may not be the best method, as these missing values could represent
#actual buildings (e.g. a warehouse) with no bedrooms, but imputation often
#makes the best of a bad situation.
#A3.1
	library(e1071)
	data_missing <- data
	data_repaired <- impute(data_missing[,1:9], what = 'median')
	data_repaired <- as.data.frame(data_repaired)
	data_repaired$ocean_proximity <- data$ocean_proximity
	summary(data_repaired)

#• Split the ocean_proximity variable into a number of binary categorical
#variables. Many machine learning algorithms in R can handle categoricals in
#a single column as a factor class, but we will cater to the lowest common
#denominator and do the splitting. Once you’re done splitting, you can
#remove the ocean_proximity variable.

	data_repaired$IHOCEAN <-rep(0, 20640)
	data_repaired$IHOCEAN[data_repaired$ocean_proximity  == "<1H OCEAN"] <-1

	data_repaired$INLAND <-rep(0, 20640)
	data_repaired$INLAND[data_repaired$ocean_proximity  == "INLAND"] <-1

	data_repaired$ISLAND <-rep(0, 20640)
	data_repaired$ISLAND[data_repaired$ocean_proximity  == "ISLAND"] <-1

	data_repaired$"NEARBAY" <-rep(0, 20640)
	data_repaired$NEARBAY[data_repaired$ocean_proximity  == "NEAR BAY"] <-1

	data_repaired$NEAROCEAN <-rep(0, 20640)
	data_repaired$NEAROCEAN[data_repaired$ocean_proximity  == "NEAR OCEAN"] <-1


	colnames(data_repaired) <- c("longitude", "latitude", "housing_median_age",
		 "total_rooms", "total_bedrooms", "population", "households",
		 "median_income", "median_house_value", "ocean_proximity", "<1H OCEAN",
		 "INLAND", "ISLAND","NEAR BAY", "NEAR OCEAN")		
	
	summary(data_repaired)

	data_repaired <- data_repaired[,c(1:9, 11:15)] #drop "ocean_proximity"
	

#• Use the total_bedrooms and total_rooms variables (and
#households) to create new mean_number_bedrooms and
#mean_number_rooms variables as these are likely more accurate
#depictions of the houses in a given group. You can then remove the
#total_bedrooms and total_rooms variables.

	data_repaired$mean_number_bedrooms <- data_repaired$total_bedrooms/data_repaired$households
	data_repaired$mean_number_rooms <- data_repaired$total_rooms/data_repaired$households
	
	data_repaired <- data_repaired[,c(13,10:11,14,12,1:3,6:8, 15:16,9)]
	summary(data_repaired)

#• Perform feature scaling. Scale each numerical variable except for
#median_house_value (as this is what we will work to predict). The
#predictor values are scaled so that coefficients in some machine learning
#algorithms are given equal weight.

	data_repaired_scale <-scale(data_repaired[, 1:13])
	summary(data_repaired_scale)
	
#• The result of your data transformation processes, you should have a new
#data frame named cleaned_housing with the following variables:
#"NEAR BAY" "<1H OCEAN" "INLAND"
#"NEAR OCEAN" "ISLAND" "longitude"
#"latitude" "housing_median_age" "population"
#"households" "median_income" "mean_bedrooms"
#"mean_rooms" "median_house_value"

	cleaned_housing <- as.data.frame(data_repaired_scale)
	cleaned_housing$median_house_value <- data_repaired$median_house_value
	class(cleaned_housing)
	names(cleaned_housing)

		#> class(cleaned_housing)
		#[1] "data.frame"

		#> names(cleaned_housing)
		# [1] "NEAR BAY"             "<1H OCEAN"            "INLAND"              
		# [4] "NEAR OCEAN"           "ISLAND"               "longitude"           
		# [7] "latitude"             "housing_median_age"   "population"          
		#[10] "households"           "median_income"        "mean_number_bedrooms"
		#[13] "mean_number_rooms"    "median_house_value"  


#4. Create Training and Test Sets
#Now we can prepare for machine learning by creating the training and test sets
#using a random sample index.
#• Create a random sample index for the cleaned_housing data frame.
#• Create a training set named train consisting of 80% of the rows of the
#housing data frame.
#• Create a test set named test consisting of 20% of the rows of the
#housing data frame. 

	n <- nrow(cleaned_housing)
	train <- round(n*0.8)
	test <- round(n*0.2)
	
	set.seed(1000)
	tindex <- sample(n, train)
	
	trainCleanedHousing <- cleaned_housing[tindex,]
	testCleanedHousing <- cleaned_housing[-tindex,]

#5.
	library(randomForest)			
	train_x <- trainCleanedHousing[,-14]
	train_y <- trainCleanedHousing[,14]
	set.seed(1000)
	rf <- randomForest(x=train_x, y=train_y, ntree=500, importance = TRUE)
	names(rf)


		# [1] "call"            "type"            "predicted"       "mse"            
		# [5] "rsq"             "oob.times"       "importance"      "importanceSD"   
		# [9] "localImportance" "proximity"       "ntree"           "mtry"           
		#[13] "forest"          "coefs"           "y"               "test"           
		#[17] "inbag"          

	#Note: for %IncMSE, there are two different results
	#Result1:

	rf$importance
	     		#	                  %IncMSE IncNodePurity
			
			NEAR BAY              483368367  1300908957175
			<1H OCEAN            1667095058  4257252331470
			INLAND               4057989556 30118870396711
			NEAR OCEAN            532751248  2384348782424
			ISLAND                  1194784    69588079693
			longitude            6733178882 25381955966940
			latitude             5439349913 21954830824517
			housing_median_age   1102138870  9992099309857
			population           1040940421  7417965585606
			households           1085384601  7714249627064
			median_income        8481192672 75421889862121
			mean_number_bedrooms  413645606  7425703360280
			mean_number_rooms    1716772405 19532661881893

	#Result2:

	importance(rf)
			
			#                       %IncMSE  IncNodePurity
			NEAR BAY             13.131065  1300908957175
			<1H OCEAN            19.625219  4257252331470
			INLAND               46.167287 30118870396711
			NEAR OCEAN           17.395337  2384348782424
			ISLAND                5.501444    69588079693
			longitude            68.019878 25381955966940
			latitude             62.102792 21954830824517
			housing_median_age   87.007970  9992099309857
			population           41.289458  7417965585606
			households           40.767119  7714249627064
			median_income        98.562676 75421889862121
			mean_number_bedrooms 48.001313  7425703360280
			mean_number_rooms    50.084646 19532661881893
	 rf 

			#Call:
			# randomForest(x = train_x, y = train_y, ntree = 500, importance = TRUE) 
			#               Type of random forest: regression
			#                     Number of trees: 500
			#No. of variables tried at each split: 4

			#          Mean of squared residuals: 2391489448
			#                    % Var explained: 82

	#Due to two types of %IncMSE, there are 2 types of rankings as a result
      #Based on rf$importance

	varimp <- data.frame(rf$importance)

  	vi1 <- ggplot(varimp, aes(x=reorder(rownames(varimp),IncNodePurity), y=IncNodePurity)) +
		       geom_bar(stat="identity", fill="dodgerblue", colour="cyan") +
			 coord_flip() + theme_bw(base_size = 8) +
			 labs(title="Prediction using RandomForest with 500 trees",
			 subtitle="Variable importance (IncNodePurity)", x="Variable", y="Variable importance (IncNodePurity)")+
			 theme(plot.title = element_text(size = 15, hjust=0.5),
			plot.subtitle = element_text(size = 15, hjust=0.5),
			axis.title.x=element_text(hjust=0.5, face="bold"),
			axis.title.y=element_text(hjust=0.5, face="bold"),
			axis.text.y=element_text(face="bold"),
			axis.text.x=element_text(face="bold"))
  
	vi2 <- ggplot(varimp, aes(x=reorder(rownames(varimp),X.IncMSE), y=X.IncMSE)) +
			 geom_bar(stat="identity", fill="gold", colour="cyan") +
			 coord_flip() + theme_bw(base_size = 8) +
			 labs(title="Prediction using RandomForest with 500 trees",
			 subtitle="Variable importance (%IncMSE)", x="Variable", y="Variable importance (%IncMSE)")+
			 theme(plot.title = element_text(size = 15, hjust=0.5),
			plot.subtitle = element_text(size = 15, hjust=0.5),
			axis.title.x=element_text(hjust=0.5, face="bold"),
			axis.title.y=element_text(hjust=0.5, face="bold"),
			axis.text.y=element_text(face="bold"),
			axis.text.x=element_text(face="bold"))


	grid.arrange(vi1, vi2, ncol=2)

	#Based on importance(rf)
		
	varImpPlot(rf)
		
#6.

		# Not specifying a data source forces OOB predictions
		oob_prediction = predict(rf)
		
		# Now compute the training set RMSE
		train_mse = mean(as.numeric((oob_prediction - train_y)^2))
		oob_rmse = sqrt(train_mse)
		oob_rmse

			#[1] 48902.86

		test_x <- testCleanedHousing[,-14]
		test_y <- testCleanedHousing[,14]

		y_pred = predict(rf , test_x)

		# Now compute the test set RSME
		test_mse = mean(((y_pred - test_y)^2))
		test_rmse = sqrt(test_mse)
		test_rmse
			
			#[1] 49060.41

			#In comparison b/t the training and the test results, they are
			#somewhat close to each other: 48902.86 vs 49060.41.
			#The difference is 157.55.
			#For housing price at the median range, this is considered no difference, meaning a good prediction and neither overfit or under.
		
		t.test(oob_prediction, y_pred)
		        
			#Welch Two Sample t-test

			#data:  oob_prediction and y_pred
			#t = -0.81654, df = 6347.1, p-value = 0.4142
			#alternative hypothesis: true difference in means is not equal to 0
			#95 percent confidence interval:
			# -4745.397  1954.641
			#sample estimates:
			#mean of x mean of y 
			# 206899.4  208294.8 

####################################
########         End       #########
####################################	
