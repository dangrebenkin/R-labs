# Данные/выборка: высота и вес (https://www.kaggle.com/datasets/mustafaali96/weight-height)

database <-
  read.csv(file = "weight-height.csv")
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
p_value_calc <- function(x, y) 
{
  e <- (y - b1 * x)
  s_b1 <-
    sqrt((sum(e ^ 2) / (length(x) - 2)) / (length(x) * ((mean(x ^ 2)) - (mean(x)) ^ 2)))
  t_b1 <- b1 / s_b1
  p_value <- (2 * (1 - pt(t_b1, df = length(x) - 2)))
  return(p_value)
}
p_value_calc(X, Y) # 0

# 4. Выписать выборочное уравнение линейной регрессии Y на Х. Дать содержательную интерпретацию коэффициента регрессии.

### b0+(b1*x_i)=-350.7372+7.717288*x_i=y_i
### 1. при увеличении высоты на 1 ед., значение веса возрастает в среднем на b1=7.7173
### 2. y_i - значение веса - не может быть отрицательным, но т.к. b0<0, то значение высоты
### должно в среднем быть больше чем 45.44825.
min_height_value <- 350.7372 / 7.717288 # 45.44825

# 5. Найти интервальные оценки для параметров линейного уравнения регрессии с заданным уровнем доверия.

theoretical_Y_values_calc <- function(x_i)
{
  y_theoretical <- b0 + (b1 * x_i)
  return(y_theoretical)
}
theoretical_Y_values <- sapply(X, theoretical_Y_values_calc)
rest_var_value <-
  1 / (length(X)) * sum((Y - theoretical_Y_values) ^ 2) # несмещенная оценка остаточной дисперсии
standard_b0_error_value <-
  sqrt((rest_var_value * sum(X ^ 2)) / ((length(X) * sum(X ^ 2)) - (sum(X) ^ 2)))
standard_b1_error_value <-
  sqrt(rest_var_value / sum((X - mean(X)) ^ 2))
### Если доверительная вероятность равна 0.95, число измерений 10000, то коэффициент Стьюдента равен 1.9602
t_coefficient <- 1.9602
lower_border_beta0 <-
  b0 - (t_coefficient * standard_b0_error_value) # -354.8757
upper_border_beta0 <-
  b0 + (t_coefficient * standard_b0_error_value) # -346.5987
lower_border_beta1 <-
  b1 - (t_coefficient * standard_b1_error_value) # 7.655035
upper_border_beta1 <-
  b1 + (t_coefficient * standard_b1_error_value) # 7.779541

# 6. Вычислить коэффициент детерминации(не использовать встроенную функцию, необходимо написать свою).

determinantion_coefficient <- coefficient_correlation ^ 2 # 0.8551742

# 7. Дать содержательную интерпретацию вычисленного коэффициента детерминации.

### В 85.5% случаев изменение показателя высоты приводят к изменению показателя роста, т.е. точность
### подбора параметров уравнения высокая.

# 9. Дать точечный прогноз среднего значения переменной Y при заданном значении регрессора (значение выбрать самостоятельно).

x_i <- 1000
y_i <- theoretical_Y_values_calc(x_i) # 7366.55
