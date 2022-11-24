# 1,2 Реализовать алгоритм Fisher's scoring algorithm для нахождения оценок параметров в логистической регрессии; 
# Найти стандартные ошибки ("Std. Error") для этих оценок, используя их асимптотическую нормальность 

fisher_scoring_algorithm <- function(X, y) {
  
  beta_old <- matrix(rep(0, ncol(X))) # starting values
  beta_new <- matrix(rep(1, ncol(X))) # estimated values
  check_change <- TRUE
  
  while (check_change == TRUE) {
    mu <- X %*% beta_old # linear predictor
    p_i <- exp(mu) / (1 + exp(mu)) # logistic function
    s_beta <- t(X) %*% (y - p_i) # score function
    f_beta <-  t(X) %*% diag(as.vector(p_i * (1 - p_i))) %*% X # fisher matrix
    beta_new <- beta_old + solve(f_beta) %*% s_beta 
    check_change <- all(abs(beta_new - beta_old) > 0.0001) # check difference
    beta_old <- beta_new
  }
  covariance_matrix <- solve(f_beta)
  standard_errors <-
    sqrt(covariance_matrix[row(covariance_matrix) == col(covariance_matrix)]) # Std. Error
  return(list(beta_new, standard_errors))
}

# 3 Cравнить результаты вашего алгоритма и glm на любом понравившемся наборе данных.

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
