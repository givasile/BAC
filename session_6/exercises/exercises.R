# Load libraries
library(dplyr)
library(readr)
library(MASS)
library(rstanarm)
library(brms)

# 1️⃣ --- Bike Sharing Dataset ---
bike_raw <- readr::read_csv("session_6/exercises/bike_sharing_dataset/day.csv")

# Double-check type
print(class(bike_raw))

bike_clean <- bike_raw %>%
  dplyr::mutate(
    season = factor(season),
    yr = factor(yr),
    mnth = factor(mnth),
    weekday = factor(weekday),
    workingday = factor(workingday),
    holiday = factor(holiday)
  ) %>%
  dplyr::select(cnt, temp, atemp, hum, windspeed, season, yr, mnth, weekday, workingday, holiday) %>%
  dplyr::mutate(
    temp = scale(temp),
    atemp = scale(atemp),
    hum = scale(hum),
    windspeed = scale(windspeed)
  )

# Final storage
Y_bike <- bike_clean$cnt
X_bike <- dplyr::select(bike_clean, -cnt)

# 2️⃣ --- Boston Housing Dataset ---
data(Boston)

boston_clean <- Boston %>%
  dplyr::select(medv, rm, lstat, age) %>%
  dplyr::mutate(
    rm = scale(rm)[,1],
    lstat = scale(lstat)[,1],
    age = scale(age)[,1]
  )

Y_boston <- boston_clean$medv
X_boston <- dplyr::select(boston_clean, -medv)


# Bike Sharing: simple linear model
bike_lm <- stan_glm(
  cnt ~ temp + atemp + hum + windspeed + season + yr + mnth + weekday + workingday + holiday,
  data = bike_clean,
  family = gaussian(),
  chains = 2, iter = 1000
)
print(bike_lm)

# Boston: simple linear model
boston_lm <- stan_glm(
  medv ~ rm + lstat + age,
  data = boston_clean,
  family = gaussian(),
  chains = 2, iter = 1000
)
print(boston_lm)
