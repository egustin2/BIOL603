midterm<- read.csv("Midterm_data.csv")


# Setup

packages <- c("MASS", "ggplot2", "plyr", "readxl", "dplyr", "ResourceSelection", "performance") 

# Ask R if they are already installed
new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]

# If they aren't installed already, tell R that you want them
if(length(new.packages)) install.packages(new.packages)

# This command will load the packages that are already installed, then download and install
# the new ones and load those too.
lapply(packages, library, character.only = TRUE)

#QAQC

str(midterm)
#examine the initial structure of the data

summary(midterm)
#examine the summary of the data


head(
  midterm %>%
    select(
      where(
        ~sum(!is.na(.x)) > 0
      )
    )
) 
#removing all rows that are only NA

midterm <- midterm %>%
  select(
    where(
      ~sum(!is.na(.x)) > 0
    )
  )
#applying that to the data frame


#plot(midterm[c(4:15, 20:25)])
plot(midterm[c(5, 7:9, 11, 12:13, 20:22, 24:25)])
#plotting to see which variables could coordinate with one another


#Do adult ticks prefer different environments? If so, do they prefer wetlands or forest environments?

chisq.test(x = midterm$nlcdClass, y = midterm$adultCount) #running chi squared tests on the data. p-value = 0.045, significant!
#We reject the null hypothesis of independence of observations based on this p-value.

pattern1 <- grepl(pattern = "dy", x = midterm$nlcdClass, ignore.case = TRUE)
pattern2 <- grepl(pattern = "st", x = midterm$nlcdClass, ignore.case = TRUE)
midterm <- midterm %>%
  mutate(
    New_nlcdClass = if_else(
      pattern1 == TRUE, "Woody Wetlands", 
      if_else(
        pattern2 == TRUE, "Decidiuous Forest", NA
      )
    )
  )
#Changing everything to make sure that there are only two titles since some are spelled wrong or not capitalized.

unique(midterm$New_nlcdClass) #Making sure the above line of code worked and there's now only two variables in the new column.

ggplot(data = midterm, aes(x = plotID, y = adultCount, fill = New_nlcdClass)) +
  geom_boxplot() +
  scale_fill_viridis_d(option = "mako", alpha = 0.7) +
  theme_bw() #building visualization of the variables in the dataset

#I'm thinking I need to add in larva and nymph counts and I could do that through a mixed effects model because adultCount only goes up to 1. The above visualization doesn't look great with only adult count. If you look at larva and nymph counts they begin to look better.

ggplot(data = midterm, aes(x = plotID, y = larvaCount, fill = New_nlcdClass)) +
  geom_boxplot() +
  scale_fill_viridis_d(option = "mako", alpha = 0.7) +
  theme_bw()

ggplot(data = midterm, aes(x = plotID, y = nymphCount, fill = New_nlcdClass)) +
  geom_boxplot() +
  scale_fill_viridis_d(option = "mako", alpha = 0.7) +
  theme_bw()

#I'm happy with my visualizations and will move on and begin to look at potential models to begin to build. I'm mainly going to use adult count because it is significant but I also really want to include larva and nymph counts because they are far more interesting to look at and model build around. 

#Regression Assumptions:
#1. Linearity of Data: The relationship between the predictor (x) and the outcome (y) is assumed to be linear. (Assumption met)
#2. Normality of Residuals: The residual errors are assumed to be normally distributed. (Unknown right now but we will build a model and look at the plots to see if this one has been met)
#3. Homogeneity of Residual Variance: The residuals are assumed to have constant variance (homoscedasticity) (We will look at residuals and decide this soon)
#4. Independence of residuals error terms (we will know soon) (I'll talk more about the residuals after I build each model)

model1<- lm(adultCount~ New_nlcdClass + larvaCount + nymphCount, data = midterm) #building the multivariate model

plot(model1) #plotting residuals

#Residuals vs. fitted: The diagnostic plot shows that the residuals are somewhat randomly distributed around 0. They could definitely could be more random. There are some points in the upper corner that should be addressed but overall not as bad as I was expecting. 
#Q-Q residuals: This looks horrible. They theoretically follow the line but also the line is flat and doesn't really show a trend. The line then also jumps up to the same corner and continues. I will need to build a different model after looking at this plot. 
#Scale-Location: These points should randomly be scattered around the graph but they aren't. They show a slight upwards trend and the same corner has a ton of points up there. 
#Residuals vs. Leverage: The points kind of all fall inside the Cook's distance area but also it is king of hard to tell with 161. The whole model should be completely rebuilt. These plots indicate that this model may not be the best for this data. 

check_collinearity(model1)
#checking for multicollinearity

#The model actually has low correlation which is kind of interesting. This doesn't mean it is a good model but it may indicate that the three variables I provided have little to do with each other. I'd find that kind of confusing considering I think the more age class of ticks there are there would potentially be around the same number of other age classes of ticks. 

#Rebuilding the model
model2<- lm(adultCount ~ New_nlcdClass + larvaCount + nymphCount + totalSampledArea + samplingMethod + decimalLatitude + elevation + decimalLongitude + samplingProtocolVersion, data = midterm)
#I'm adding a ton more variables in this time to see what something like that will look like in terms of residual plots and multicollinearity.

plot(model2) #Plotting residuals again

#Residuals vs. Fitted: The diagnostic plot should show all residuals randomly distributed around 0. There's definitely a pretty clear trend influencing the points. It is better but still not good and further work will need to be done on them. 
#Q-Q Residuals: the points mostly follow the line and actually look better than the previous plot but still do not look good. There's a pretty big jump between the plots that follow the line and the points up at the top corner. Although, I will say that there are less points. 
#Scale-Location: These ones look a lot worse I think. For some reason now they are shaped like a checkmark. I do not think this is an improvement. The points should be randomly scattered around the plot and they show a clear pattern. 
#Residuals vs. Leverage: The points are all within the Cook's distance area, however, there is a clear pattern and based on the other plots, more modeling will need to be done to work with the question I am trying to answer. 

check_collinearity(model2)
#checking for multicollinearity again

#I'm actually really surprised that there is still low correlation with all of these variables in the model.

#Building the full model
model3<- lm(adultCount ~ New_nlcdClass + larvaCount*nymphCount + totalSampledArea*nymphCount + samplingMethod*nymphCount + decimalLatitude*nymphCount + elevation*nymphCount + decimalLongitude*nymphCount + samplingProtocolVersion*nymphCount + larvaCount*totalSampledArea + larvaCount*samplingMethod + larvaCount*decimalLatitude + larvaCount*elevation + larvaCount*decimalLongitude + larvaCount*samplingProtocolVersion + totalSampledArea*samplingMethod + totalSampledArea*decimalLatitude + totalSampledArea*elevation + totalSampledArea*decimalLongitude + totalSampledArea*samplingProtocolVersion + samplingMethod*decimalLatitude + samplingMethod*elevation + samplingMethod*decimalLongitude + samplingMethod*samplingProtocolVersion + decimalLatitude*elevation + decimalLatitude*decimalLongitude + decimalLatitude*samplingProtocolVersion + elevation*decimalLongitude + elevation*samplingProtocolVersion + decimalLongitude*samplingProtocolVersion, data = midterm)

summary(model3) #Examining the full model 
#Residuals: the median should be very close to zero. In this case it is.
#Intercept: -2.541e+03 in this case is not significant because you cannot have negative tick values. 
#Intercept standard error: This is a really large error indicating the model is probably a poor choice for the dataset. 
#T-value: a negative t-value indicates that there is a negative correlation between predictor and response. 
#The p-value in this case is not significant at 0.7642.
#Residual Standard Error: If this were zero it would be a perfect fit (overfitted) for the data, unfortunately it is 0.2781 which shows it is not. Theoretically, it should be far closer to zero considering it is a full model and should be overfitted. 
#Multiple R^2 and adjusted R^2: This is a measure of how much of the variance in the response is explained by the predictor. This value is 0.258 indicating it accounts for about 25.8% of the variance which is honestly really bad. 
#F-statistic: the F-statistic adjusts for the number of predictors to maintain a constant error rate. The reported f-statistic in this case is 0.8375 which is very bad. This is not a good model for the data. 

#This model is not good and needs to be refined.

#I'm removing sampling protocol version because I don't think I should have added it in the first place and I feel like it is slightly redundant with some of the other very similar variables that I tested. 

#Building the first reduced model:
rmodel1<- lm(adultCount ~ New_nlcdClass + larvaCount*nymphCount + totalSampledArea*nymphCount + samplingMethod*nymphCount + decimalLatitude*nymphCount + elevation*nymphCount + decimalLongitude*nymphCount + larvaCount*totalSampledArea + larvaCount*samplingMethod + larvaCount*decimalLatitude + larvaCount*elevation + larvaCount*decimalLongitude + totalSampledArea*samplingMethod + totalSampledArea*decimalLatitude + totalSampledArea*elevation + totalSampledArea*decimalLongitude + samplingMethod*decimalLatitude + samplingMethod*elevation + samplingMethod*decimalLongitude + decimalLatitude*elevation + decimalLatitude*decimalLongitude + elevation*decimalLongitude, data = midterm)

plot(rmodel1)
#Residuals vs. Fitted: The diagnostic plot shows that residuals are still being influenced by one variable because they are not being randomly distributed around 0. This model needs to be further refined. 
#Q-Q Residuals: This plot actually looks a little better because the lines are somewhat trending upwards and are following the line. There are still some points that cannot be accounted for within this model. 
#Scale-Location: These points should be randomly scattered around the graph. Unfortunately I cannot get that big check mark to leave. This graph tells me that this model is still not a good fit for the data. 
#Residuals vs. Leverage: Many points no longer are accounted for in this model. This plot actually got a lot worse than the previous models. Further work needs to be done to make this a good model for the data.

anova(model3, rmodel1) #Seeing how different the two models are from one another. 
#This model is slightly different from the previous full model that I ran. The p-value is 0.3215. 

check_collinearity(rmodel1) #seeing what has high correlation so I know what I need to take out

#Building yet another model: Removing the highest values
rmodel2<- lm(adultCount ~ New_nlcdClass + larvaCount*nymphCount + samplingMethod*nymphCount + larvaCount*samplingMethod + decimalLatitude*decimalLongitude, data = midterm)

check_collinearity(rmodel2) #Using this to double check correlation as I take out each variable. The above is now that everything is below high correlation.

AIC(rmodel1)
AIC(rmodel2)
#Comparing AIC values. rmodel2 has a lower AIC value and no high correlation variables!

plot(rmodel2)
#Residuals vs. Fitted: Values still have a skew but all are around zero and are "randomly" distributed around zero. 
#Q-Q Residuals: The points mostly follow the line. They unfortunately still jump up to the top corner but I think this data set is messy and I might not get better results. 
#Scale-Location: The checkmark is still there but I would argue that it is smaller. If this were a good model these would be scattered randomly around the graph better. 
#Residuals vs. Leverage: All points once again fall within the limits of the Cook's distance lines! That's much better than before. 

summary(rmodel2) #Examining the full model 
#Residuals: the median should be very close to zero. In this case it is.
#Intercept: 4.415e+02 in this case is significant because you could potentially have that many ticks per space.
#Intercept standard error: This is a fairly large error indicating the model could probably be further refined. This being said it is much better than the previous value.  
#T-value: a positive t-value indicates that there is a positive correlation between predictor and response. 
#The p-value in this case is not significant at 0.605. This being said it is much better than the previous p-value in the last model I tested. 
#Residual Standard Error: This should be a little closer to zero. It is now 0.2668 indicating that the model could probably still be adjusted but is still better than the last time I tested it. 
#Multiple R^2 and adjusted R^2: This is a measure of how much of the variance in the response is explained by the predictor. This value is 0.097 indicating it accounts for about 9.7% of the variance which is confusing to me considering it went down. This is not the perfect model but the rest of the info tells me it is at least a better model than before. 
#F-statistic: the F-statistic adjusts for the number of predictors to maintain a constant error rate. The reported f-statistic in this case is 1.515.
#p-value: The p-value is 0.1401 which is not significant at the 0.05 level set for most ecological data projects. This being said, it is much closer to that value the the previous model.

#I would say that this model is a lot better than the previous iterations of the models I made. I think there may be more to do on it but I think in general it is a lot better. 

#Final Model
rmodel2<- lm(adultCount ~ New_nlcdClass + larvaCount*nymphCount + samplingMethod*nymphCount + larvaCount*samplingMethod + decimalLatitude*decimalLongitude, data = midterm)


#Potential graphs. None of them are good but I should have picked different variables from the beginning. 
ggplot(data = midterm, aes(x=adultCount, y= New_nlcdClass)) +geom_point(color = "blue") +geom_smooth(method = 'lm')

ggplot(midterm, aes(x = factor(New_nlcdClass), y = adultCount)) + geom_boxplot()

ggplot(midterm, aes(x = New_nlcdClass, y = adultCount)) +
  geom_point(alpha = 0.3) + 
  geom_smooth(method = "lm", method.args = list(family = "binomial"), se = TRUE) +
  labs(y = "Number of Adult Ticks", x = "Biome")



