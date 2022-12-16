# # Третье задание
#
# Обозначим через $S(t)$ - функцию выживания, а через $S*(t)$ - оценку Каплана-Мейера.
#
# 1. Используя дельта-метод, вывести формулу Гринвуда.
# Вывод не обязан быть строгим: в частности при выводе можно предпологать,
# что $t_j$ - фиксированные (не случайные моменты времени).

# 2,3 Построить вальдовские доверительные интервалы для $S*(t)$ по формуле Гринвуда.
# Недостаток интервалов из пункта 2 в том, что они могут выходить за пределы значений функции
# $S(t)$ (отрезок [0; 1]). Найдите log-log доверительный интервал для $S*(t)$, ещё раз применив
# дельта метод к $log(-log( S*(t) ))$. Может ли доверительный интервал для $S(t)$, построенный
# по этой статистике, выходить за пределы отрезка $[0; 1]$?

dataset <- read.csv("dataset-96848.csv")
names(dataset)[names(dataset) == "delta"] <- "died"
dataset <- dataset [,-1]
dataset <- dataset[order(dataset$time),]

custom_survival_method <- function(df, alpha) {
  CIs_df <- data.frame(matrix(ncol = 9, nrow = 0))
  colnames(CIs_df) <- c(
    "t_j",
    "d_j",
    "n_j",
    "s_j",
    "S_t",
    "low_CI_greenwood",
    "up_CI_greenwood",
    "low_CI_loglog",
    "up_CI_loglog"
  )
  all_timepoints <- unique(df$time)
  S_t <- 1
  for (t_j in all_timepoints) {
    d_j <- nrow(df[df["time"] == t_j & df["died"] == 1,])
    if (d_j > 0) {
      n_j <- nrow(df)
      s_j <- 1 - d_j / n_j
      S_t <- S_t * s_j
      
      summ <- 0
      if (nrow(CIs_df) > 0) {
        for (i in 1:nrow(CIs_df)) {
          calculated_d_j <- CIs_df[i, "d_j"]
          calculated_n_j <- CIs_df[i, "n_j"]
          summ <-
            summ + (calculated_d_j / (calculated_n_j * (
              calculated_n_j - calculated_d_j
            )))
        }
      }
      
      summ <- summ + (d_j / (n_j * (n_j - d_j)))
      DS <- summ * S_t * S_t
      
      # про интервалы: https://myweb.uiowa.edu/pbreheny/7210/f15/notes/9-10.pdf
      
      # greenwood
      low_CI_greenwood <- S_t - sqrt(DS) * qnorm(1 - alpha / 2)
      up_CI_greenwood <- S_t + sqrt(DS) * qnorm(1 - alpha / 2)
      
      # log-log
      low_CI_loglog <-
        S_t ^ (exp(-1 * qnorm(1 - alpha / 2) * sqrt(summ) / log(S_t)))
      up_CI_loglog <-
        S_t ^ (exp(qnorm(1 - alpha / 2) * sqrt(summ) / log(S_t)))
      
      CIs_df[nrow(CIs_df) + 1,] = c(
        t_j,
        d_j,
        n_j,
        s_j,
        S_t,
        low_CI_greenwood,
        up_CI_greenwood,
        low_CI_loglog,
        up_CI_loglog
      )
      df <- df[df["time"] != t_j, ]
    } else {
      df <- df[!(df["time"] == t_j & df["died"] == 0), ]
    }
  }
  return(CIs_df)
}

result <- custom_survival_method(dataset, 0.05)
result

#   4. Сравните полученный интервал с выводом $survfit(..., conf.type = "log-log")$.
# Для демонстрации решения можно использовать любые данные, к которым применим
# анализ выживаемости.

library(survival)
fit1 = survfit(Surv(time, died) ~ 1, data = dataset, conf.type = "plain")
summary(fit1)

fit2 = survfit(Surv(time, died) ~ 1, data = dataset, conf.type = "log-log")
summary(fit2)
