# 1. Реализовать алгоритм Fisher's scoring algorithm для нахождения оценок параметров в логистической регрессии.

fisher_scoring_algorithm <- function(X, y) {
  p_i_calc <- function(v)
    return(exp(v) / (1 + exp(v)))
  
  beta_old <- matrix(rep(0, ncol(X)))
  beta_new <- matrix(rep(1, ncol(X)))
  check_change <- TRUE
  
  while (check_change == TRUE) {
    mu <- X %*% beta_old
    p_i <- p_i_calc(mu)
    s_beta <- t(X) %*% (y - p_i)
    f_beta <-  t(X) %*% diag(as.vector(p_i * (1 - p_i))) %*% X
    beta_new <- beta_old + solve(f_beta) %*% s_beta
    check_change <- all(abs(beta_new - beta_old) > 0.0001)
    beta_old <- beta_new
  }
  covariance_matrix <- solve(f_beta)
  standard_values <-
    sqrt(covariance_matrix[row(covariance_matrix) == col(covariance_matrix)])
  return(list(beta_new, standard_values))
}

# 2, 3 Найти стандартные ошибки ("Std. Error") для этих оценок, используя их асимптотическую нормальность;
# сравнить результаты вашего алгоритма и glm на любом понравившемся наборе данных.

dataset <- mtcars
head(dataset)

features <- as.matrix(cbind(1, dataset[c(2, 5)]))
response <- as.matrix(dataset$vs)

colnames(features) <- NULL
colnames(response) <- NULL

fisher_alg_coef <- fisher_scoring_algorithm(features, response)

fisher_alg_coef[1] # коэффициенты алгоритма
fisher_alg_coef[2] # стандартные ошибки

columns <- colnames(dataset[c(2, 5)])
columns_to_calc <- paste(columns, collapse = "+")
model <- glm(data = dataset,
             as.formula(paste("vs", "~", columns_to_calc, sep = "")), family = binomial)
summary(model) # совпадает
