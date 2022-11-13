# Задание 1. Нарисовать график odds в зависимости от p.

p <- seq(0, 1, 0.001)
odds <- p / (1 - p)
plot(p,
     odds,
     xlab = 'p',
     ylab = 'odds',
     type = "l")

# Задание 2. Нарисовать графит logit(p).

logit <- log(odds)
plot(p,
     logit,
     xlab = 'p',
     ylab = 'logit(p)',
     type = "l")

# Задание 3. Исследовать данные mtcars. Составить какую-нибудь
# логистическую регрессию и проанализировать ее.

dataset <- mtcars
# help(mtcars)
columns <- colnames(dataset)[-8]

akaike_criteria <- function(columns_to_calc)
{
  model <-
    glm(data = dataset,
        as.formula(paste("vs", "~", columns_to_calc, sep = "")), family = binomial)
  val <- AIC(model)
  return(val)
}

forward <- function (columns) {
  current_best_aic <- 100
  best_combination <- ""
  
  while (length(columns) > 0) {
    values <- list()
    for (i in columns) {
      if (best_combination != "") {
        i <- paste(i, best_combination, sep = "+")
      }
      aic_value <- akaike_criteria(i)
      values <- c(unlist(values), aic_value)
    }
    best_value <- min(unlist(values))
    best_value_index <- match(best_value, values)
    if (best_value < current_best_aic) {
      if (best_combination != "") {
        best_combination <-
          paste(columns[best_value_index], best_combination, sep = "+")
      } else {
        best_combination <- columns[best_value_index]
      }
      current_best_aic <- best_value
      columns <- columns[-best_value_index]
      
    } else {
      break
    }
  }
  return (best_combination)
}

good_factors <- forward(columns)
pseudo_best_model <-
  glm(data = dataset, as.formula(paste("vs", "~", good_factors, sep = "")), family = binomial)
summary(pseudo_best_model) # AIC маленький, а p-value почти 1

columns <- colnames(dataset)[c(2, 5)]
columns_string <- paste(columns, collapse = "+")
best_model <-
  glm(data = dataset, as.formula(paste(
    "vs", "~", paste(columns_string, sep = "+"), sep = ""
  )), family = binomial)
summary(best_model)

# Задание 4. Вычислить TP, FP, FN, TN.

TP <- 0
TN <- 0
FP <- 0
FN <- 0

dataset <- mtcars
columns <- colnames(dataset)[c(2, 5)]
columns_string <- paste(columns, collapse = "+")
formula_for_model <-
  as.formula(paste('vs', "~", columns_string, sep = ""))

validation <- function(train_dataset, test_dataset) {
  model <-
    glm(formula_for_model, data = train_dataset, family = binomial)
  prediction <-
    predict(model, newdata = test_dataset[-3], type = "response")
  if (prediction >= 0.5) {
    if (test_dataset$vs == 1) {
      TP <<- TP + 1
    } else {
      FP <<- FP + 1
    }
  } else{
    if (test_dataset$vs == 0) {
      TN <<- TN + 1
    } else {
      FN <<- FN + 1
    }
  }
}

subdataset <- dataset[c(2, 5, 8)]
lines <- seq(1, 32, 1)
for (line_number in lines) {
  train <- subdataset[-line_number, ]
  test <- subdataset[line_number, ]
  validation(train, test)
}

# Задание 5. Вычислить Sensitivity (чувствительность) и Specificity (специфичность)

Sensitivity <- TP / (TP + FN) #0.78
Specificity <- TN / (TN + FP) #0.88
