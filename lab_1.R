# ------------------------------------------------------------------------------
# ЗАДАНИЕ 1
# ------------------------------------------------------------------------------

# Данные/выборка: высота и вес (https://www.kaggle.com/datasets/mustafaali96/weight-height)

database <-
  read.csv(file = "/mnt/LinuxFiles/R_homework/weight-height.csv")
X <- database$Height
Y <- database$Weight

# 1. Построить корреляционное поле между показателями X и Y.

plot(X, Y, xlab = 'Высота', ylab = 'Вес')

# 2. С помощью коэффициента парной корреляции оценить тесноту линейной связи между показателями X и Y
# (не использовать встроенную функцию нахождения коэффициента корреляции, необходимо написать свою).

coefficient_correlation_calculation <- function(x, y)
{
  difference_x <- x - mean(x)
  difference_y <- y - mean(y)
  differences_product_sum <- sum(difference_x * difference_y)
  sqrt_sums <- sqrt(sum(difference_x ^ 2) * sum(difference_y ^ 2))
  coefficient <- differences_product_sum / sqrt_sums
  return(coefficient)
}

coefficient_correlation <-
  coefficient_correlation_calculation(X, Y) # 0.9247563, высокая степень связи между X и Y

# 3. Методом наименьших квадратов найти точечные оценки параметров линейного уравнения регрессии Y на X
# и проверить их значимость с помощью критерия Стьюдента (не использовать встроенные функции нахождения
# МНК-оценок и проверку значимости, необходимо написать свои функции; критерий Стьюдента должен выдавать p-value).

b1 <-
  (mean(X * Y) - (mean(X) * mean(Y))) / (mean(X ^ 2) - (mean(X)) ^ 2) # 7.717288
b0 <- mean(Y) - b1 * mean(X) # -350.7372

theoretical_Y_values_calc <- function(x_i)
{
  y_theoretical <- b0 + (b1 * x_i)
  return(y_theoretical)
}

theoretical_Y_values <- sapply(X, theoretical_Y_values_calc) # значения Y, предсказанные с помощью уравнения
rest_var_value <-
  (sum((Y - theoretical_Y_values) ^ 2) )/ (length(X) - 2) # несмещенная оценка остаточной дисперсии

p_value_calc_Student <- function(x, y)
{
  s_b1 <-
    sqrt(rest_var_value / (length(x) * ((mean( x ^ 2 )) - (mean(x)) ^ 2)))
  t <- b1 / s_b1
  p_value <- (2 * (1 - pt(t, df = length(x) - 2)))
  return(p_value)
}

p_value_calc_Student(X, Y) # 0

# 4. Выписать выборочное уравнение линейной регрессии Y на Х. Дать содержательную интерпретацию коэффициента регрессии.

### b0+(b1*x_i)=-350.7372+7.717288*x_i=y_i
### 1. при увеличении высоты на 1 ед., значение веса возрастает в среднем на b1=7.7173
### 2. y_i - значение веса - не может быть отрицательным, но т.к. b0<0, то значение высоты
### должно в среднем быть больше чем 45.44825.
min_height_value <- 350.7372 / 7.717288 # 45.44825

# 5. Найти интервальные оценки для параметров линейного уравнения регрессии с заданным уровнем доверия.

standard_b0_error_value <-
  sqrt((rest_var_value * sum(X ^ 2)) / ((length(X) * sum(X ^ 2)) - (sum(X) ^ 2)))
standard_b1_error_value <-
  sqrt(rest_var_value / sum((X - mean(X)) ^ 2))
### Если доверительная вероятность равна 0.95, число измерений 10000, то коэффициент Стьюдента равен 1.9602
t_coefficient <- 1.9602
lower_border_beta0 <-
  b0 - (t_coefficient * standard_b0_error_value) # -354.8761
upper_border_beta0 <-
  b0 + (t_coefficient * standard_b0_error_value) # -346.5983
lower_border_beta1 <-
  b1 - (t_coefficient * standard_b1_error_value) # 7.655028
upper_border_beta1 <-
  b1 + (t_coefficient * standard_b1_error_value) # 7.779547

# 6. Вычислить коэффициент детерминации(не использовать встроенную функцию, необходимо написать свою).

determinantion_coefficient <-
  coefficient_correlation ^ 2 # 0.8551742

# 7. Дать содержательную интерпретацию вычисленного коэффициента детерминации.

### В 85.5% случаев изменение показателя высоты приводят к изменению показателя роста, т.е. точность
### подбора параметров уравнения высокая.

# 8. При помощи критерия Фишера проверить адекватность построенного уравнения регрессии (не использовать
# встроенную функцию, необходимо написать свою; критерий Фишера должен выдавать p-value)).

p_value_calc_Fisher <- function(x)
{
  f_coef <-
    (determinantion_coefficient * (length(x) - 2)) / (1 - determinantion_coefficient)
  t <- sqrt(f_coef)
  p_value <- (2 * (1 - pt(t, df = length(x) - 2)))
  return(p_value)
}

p_value_calc_Fisher(X)

# 9. Дать точечный прогноз среднего значения переменной Y при заданном значении регрессора (значение выбрать самостоятельно).

x_i <- 1000
y_i <- theoretical_Y_values_calc(x_i) # 7366.55

# 10. Воспользоваться встроенными в функциями R для проверки проделанных расчетов.

model <- lm(data = database, Y ~ X)
summary(model) # совпало
confint(model, level = 0.90) # совпало
cor(X, Y) # совпало

# ------------------------------------------------------------------------------
# ЗАДАНИЕ 2
# ------------------------------------------------------------------------------

library("readxl")
new_database <-
  read_excel("/mnt/LinuxFiles/R_homework/Данные_Лабораторная_1.xlsx")
head(new_database)

# 3. Примените критерий Манна-Уитни для выявления различий на каком-нибудь понравившимся датасете.
# (Например, проверьте гипотезу о том, есть ли различия между мужчинами и женщинами по Росту,
#   (Размеру ноги) по собранным нами данным (файл с данными прикрепляю)).

men <- new_database[new_database$Пол == '1',]
women <- new_database[new_database$Пол == '0',]
X_1 <- men$Размер
Y_1 <- women$Размер
wilcox.test(X_1, Y_1, exact = FALSE) # 0.0001747 < 0.05, различия есть

# 4*. Для критерия Манна–Уитни самостоятельно попробуйте реализовать нахождение оценки Ходжеса-Лемана
# и доверительного интервала (Bauer) для параметра сдвига.
# Сравните свой результат с выводом wilcox.test().

median(X_1 - Y_1) # 5
wilcox.test(X_1, Y_1, exact = FALSE, conf.int = TRUE)

# 5. Примените критерий Вилкоксона для выявления различий на каком-нибудь понравившимся датасете.
# (Например, проверьте гипотезу о том, есть ли различия между пульсом ДО и ПОСЛЕ физических упражнений
#   по собранным нами данным (файл с данными прикрепляю)).

pulse_before <- new_database$ПульсДО
pulse_after <- new_database$ПульсПОСЛЕ
wilcox.test(pulse_before,
            pulse_after,
            paired = TRUE,
            exact = FALSE) # 2.762e-05 < 0.05, распределение не
### симметрично

