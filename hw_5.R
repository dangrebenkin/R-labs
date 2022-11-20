library(foreign)
dataset <- read.table('ceb.dat')
dataset$children <- round(dataset$mean * dataset$n)
head(dataset)

# Задание 1. Проанализировать влияние уровня образования на фертильность женщин Фиджи.
#            Проверить согласие построенной модели и данных.

education_and_fertility <-
  glm(children ~ educ,
      data = dataset,
      family = "poisson")

summary(education_and_fertility)

pchisq(education_and_fertility$deviance,
       df = 66,
       lower.tail = FALSE)

# Задание 2. Проанализировать влияние времени в браке на фертильность женщин Фиджи.
#            Проверить согласие построенной модели и данных.

duration_and_fertility <-
  glm(children ~ dur,
      data = dataset,
      family = "poisson")

summary(duration_and_fertility)

pchisq(duration_and_fertility$deviance,
       df = 64,
       lower.tail = FALSE)

# Задание 3. Проанализировать влияние уровня образования, времени в браке
#            и места жительства на фертильность женщин Фиджи.
#            Проверить согласие построенной модели и данных.
#            Построить прогнозы.

dataset

educ_dur_res_and_fertility <-
  glm(
    children ~ educ + dur + res,
    data = dataset,
    family = "poisson",
    offset = log(dataset$n)
  )

summary(educ_dur_res_and_fertility)

pchisq(educ_dur_res_and_fertility$deviance,
       df = 59,
       lower.tail = FALSE)
dataset

test_data <-
  data.frame(
    "dur" = "25-29",
    "educ" = "sec+",
    "res" = "urban",
    "n" = 62
  )
predict(educ_dur_res_and_fertility, test_data, type = "response")

tail(dataset)
