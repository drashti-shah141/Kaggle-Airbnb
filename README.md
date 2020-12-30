# Airbnb Rental Prices Predictive Modeling

Predictive NYC Rental Prices of AirBnb

Link- https://www.kaggle.com/c/predictlala2020 

# Summary
“Which variables can predict AirBnb’s listing’s price?” was the question that started my journey in the Kaggle competition.  We had to create the best predictor model with 90 variables and 39,527 observations. The score was evaluated based on Root Mean Squared Error. The lower the RMSE, the better the model.

At first, we submitted the sample code file resulting in the first mandatory submission with an RMSE of 104.760. My initial approach was unorganized and unplanned. I was just trying to get a model work based on the numeric variables. Soon, I realized that I need a planned approach and a clear commented R code file. My exploratory data analysis began by looking at the response variable-Price. I realized that this variable had a few outliers with values of zero. So, I removed these erroneous values from Price. 

Next, I removed all variables that were irrelevant such as those containing repetitive information (maximum-minimum nights, minimum maximum nights, neighborhood, neighborhood group, etc) or those containing information that could be captured in one variable (Market, Street neighborhood cleansed, etc captured in neighborhood group cleansed ). A few variables were removed as they had the same value across all observations (Country code, State, country, has availability, etc). Then with further exploration, I realized that there were multiple types of NA’s in various variables such as “N/A”, NA, or a missing value. I replaced those different types with a single NA string. 

Before I went towards making models, I knew I had to do some feature engineering and make appropriate changes to existing variables. I tackled this data cleaning process by individual variable wise. Undoubtedly, this section contained the most amount of missteps but also produced some of my strongest variables. There were outliers in one variable in my current variables set- Maximum Nights so I replaced all values higher than 1200 with a median of all values less than 1200. In my unorganized approach in the beginning I had explored the missing values or NA’s in all numeric variables so I imputed all those missing values with the median as imputing with mice package didn’t make a difference. Further, I looked at the host response time string variable and imputed the missing values with “a few days or more”. I knew that Property type was an important variable but it had too many so after exploring all the levels, I combined levels into approximately 10 levels. Now it was time to look at zipcodes, this variable had some bad formats so I changed those values and a value of “1m” which was repeated a few times in zipcodes was replaced with the most common zip code value in the dataset-”11221”.

My subsequent batch of features involved specific property features with a heavy focus on amenities and host verifications. First, I created 2 columns counting the total amenities offered by a listing and total host verifications offered by a listing. The process of dividing the amenities was very challenging. I kept thinking about how I could do it efficiently with only a few lines of code. So, I considered a few popular amenities by creating a logical variable for each of them individually. This will have a value of 0 if the amenity is not offered by the listing and 1 if the amenity is offered. This resulted in 13 additional variables. I should have rather created Boolean variables for all amenities. I started thinking from the host’s perspective and by putting myself in their shoes, I thought of a few additional features such as counting the words in the string variables such as Description, Space, Summary, Name, etc. This resulted in transforming the string variables into 11 numeric variables. 9 made it in my models. As my last foray into understanding the host’s perspective, I counted the uppercase letters in Name and created a new variable called Name Upper. I progressed to transform the bed_type column into a Boolean variable which distinguished whether a property had a real_bed or not. My initial unorganized approach had shown that first_review, last-review, and host_since were not important predictors. I made the mistake of ignoring them completely in my planned approach realizing very late that maybe the length of an owner being a host might have had some relevance on the price. 

After cleaning all variables, I isolated all numeric variables and observed correlations to check if this would further give me any additional variables that I can remove. This helped me to eliminate a few extra variables. All the cleaning that I implemented for my analysis dataset was also implemented for the scoring dataset. Next, I created two clean files so I don’t have to run the whole previous code every time I want to run models. For further feature selections, I performed a lasso regression analysis for almost all the features that I had in the clean datasets. I used the yielded features with coefficients higher than zero as my base to create a feature set.

Based on these variables, I used the caret package to further divide the analysis data into train and test datasets with a 75:25 split with groups of 100. I realized later that I may have benefitted from a better grouping such as 5% or 10% of the observations. I started with random forest models. I used the ranger package as I knew it performs better computationally. And it did, my best model was an untuned ranger random forest model with a public RMSE of 58.94. Although tuned random forests took a significant amount of time to run, they did not yield the kind of results I expected with worse RMSE’s. I jumped the gun by using directly the cross-validated gradient boosting model and my computer crashed several times even after long hours of processing. I aimed a finding a better model. And I realized that I haven’t even looked at Xgboost as we only briefly went over it in the class.

As Professor Lala had said earlier, time turned out to be the most precious resource in this competition. I was almost in my last two days before final submission to completely understand, research, and run XGBoost models. I started using stack overflow to figure out what would I need to do with my factor variables. I ran a model and got a public RMSE of 59.54. I did not get a chance to take a stab at tuned Xgboost models as I ran out of attempts. I didn’t even realize that my initial model submissions were all predicting prices in scoring dataset based on only train data and not overall data. So, I rectified my mistake in running these last two models.  

Takeaways- In the initial exploration, my unplanned approach wasted a lot of time and I ended up being overwhelmed with the huge amount of data rather than focusing on following a neat approach. I ended up getting a lot of errors due to that approach. But slowly, after refamiliarizing myself with class code notes and online resources, I was introduced to a vast amount of new knowledge and confidence. Due to time constraints and other course assignments, I couldn’t give this assignment as much time as I had hoped despite starting early, but the attempts made me well prepared for any future predictive modeling. While I made many mistakes from ignoring a few variables, being unorganized initially to not utilizing time wisely, this assignment made me more proficient in R and gave me a sense of hope that despite coming from a non-coding background, if I work hard enough, I can get better at it.
