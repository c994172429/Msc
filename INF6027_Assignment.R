#load the libray to use
library(tidyverse)

#import dataset in RStudio
music_test <- read.table("song_pop.csv", header = TRUE)
music_test_2 <- read.table("acoustic_features.csv", header = TRUE)
#merge the data together
new_music_data <- music_test_2 %>% left_join(music_test, by="song_id")
#check if there are any null value
any(is.na(new_music_data))
#select the popular music from the dataset
pop_music <- filter(new_music_data, new_music_data$is_pop == 'True')

#summary and graph danceability and valence to check which value is 
#better(mean, median, frequency)
summary(pop_music$danceability)
summary(pop_music$valence)

hist(pop_music$danceability)
hist(pop_music$valence)

#add a new variable for save average for danceability and valence
avg_dance_year <- pop_music %>%
  group_by(year) %>%
  summarise(avg_dance = mean(danceability, na.rm = TRUE))

avg_valence_year <- pop_music %>%
  group_by(year) %>%
  summarise(avg_valence = mean(valence, na.rm = TRUE))

#graph the trend for danceability and valence
ggplot(avg_dance_year, aes(x = year, y = avg_dance)) + geom_line()
ggplot(avg_valence_year, aes(x = year, y = avg_valence)) + geom_line()
#use cor.test to calculate the correlation coefficient
cor.test(avg_dance_year$year, avg_dance_year$avg_dance)
cor.test(avg_valence_year$year, avg_valence_year$avg_valence)

#predict trend of danceability
nrow(avg_dance_year)
#splited data
dance_train <- avg_dance_year[1:50,]
dance_test <- avg_dance_year[51:55,]
#fit the linear regression model to the data
mod_dance <- lm(
  formula = avg_dance~year,
  data=dance_train
)
#show the equation
coef(mod_dance)
#save the result
coefs_dance <- coef(mod_dance)
#draw a line to show how well the model fits data
ggplot(
  data=dance_train,
  aes(x=year, y=avg_dance)
) +
  geom_point() +
  geom_abline(mapping=aes(
    slope=coefs_dance["year"],
    intercept=coefs_dance["(Intercept)"]
  ), color='red')
#predict the danceability trend with the new model
predict(
  mod_dance,
  newdata = dance_test,
  interval = 'confidence'
)
#adds predicted and residuals to test data
dance_test$predicted <- predict(mod_dance, newdata=dance_test)
dance_test$residuals <- dance_test$predicted - dance_test$avg_dance
#show the difference between predicted data and real data
ggplot(
  data=dance_test,
  aes(x=year,y=avg_dance)
) +
  geom_point(size=3) + # make the actual values show up more clearly
  geom_point(size=2, aes(y=predicted), shape=1) + # show the predicted values
  geom_segment(aes(xend=year, yend=predicted), alpha=0.9, color='red') +
  geom_abline(mapping=aes(
    slope=coefs_dance["year"],
    intercept=coefs_dance["(Intercept)"]
  ), color='gray'
  )
#calculated the sum of square errors
sum(dance_test$residuals**2)

#predict the trend of valence
nrow(avg_valence_year)
#splited data
valence_train <- avg_valence_year[1:50,]
valence_test <- avg_valence_year[51:55,]
#fit the linear regression model to the data
mod_valence <- lm(
  formula = avg_valence~year,
  data=valence_train
)
#show the equation
coef(mod_valence)
#save the result
coefs_valence <- coef(mod_valence)
#draw a line to show how well the model fits data
ggplot(
  data=valence_train,
  aes(x=year, y=avg_valence)
) +
  geom_point() +
  geom_abline(mapping=aes(
    slope=coefs_valence["year"],
    intercept=coefs_valence["(Intercept)"]
  ), color='red')
#predict the danceability trend with the new model
predict(
  mod_valence,
  newdata = valence_test,
  interval = 'confidence'
)
#adds predicted and residuals to test data
valence_test$predicted <- predict(mod_valence, newdata=valence_test)
valence_test$residuals <- valence_test$predicted - valence_test$avg_valence
#show the difference between predicted data and real data
ggplot(
  data=valence_test,
  aes(x=year,y=avg_valence)
) +
  geom_point(size=3) + # make the actual values show up more clearly
  geom_point(size=2, aes(y=predicted), shape=1) + # show the predicted values
  geom_segment(aes(xend=year, yend=predicted), alpha=0.9, color='red') +
  geom_abline(mapping=aes(
    slope=coefs_valence["year"],
    intercept=coefs_valence["(Intercept)"]
  ), color='gray'
  )
#calculated the sum of square errors
sum(valence_test$residuals**2)

