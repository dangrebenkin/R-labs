dataset <- swiss
columns <- colnames(dataset)[-1]

r_squared_ajucted <- function(columns_to_calc)
{
  model <-
    lm(data = dataset,
       as.formula(paste("Fertility", "~", columns_to_calc, sep = "")))
  val <- summary(model)$adj.r.squared
  return(val)
}

# forward

forward <- function (columns) {
  current_best_r_adj <- 0
  best_combination <- ""
  
  while (length(columns) > 0) {
    values <- list()
    for (i in columns) {
      if (best_combination != "") {
        i <- paste(i, best_combination, sep = "+")
      }
      r_squared_ajucted_value <- r_squared_ajucted(i)
      values <- c(unlist(values), r_squared_ajucted_value)
    }
    best_value <- max(unlist(values))
    best_value_index <- match(best_value, values)
    if (best_value > current_best_r_adj) {
      if (best_combination != "") {
        best_combination <-
          paste(columns[best_value_index], best_combination, sep = "+")
      } else {
        best_combination <- columns[best_value_index]
      }
      current_best_r_adj <- best_value
      columns <- columns[-best_value_index]
      
    } else {
      break
    }
  }
  return (best_combination)
}

forward(columns)

# back

columns <- colnames(dataset)[-1]

back <- function (columns) {
  best_combination <- columns
  current_best_r_adj <-
    r_squared_ajucted(paste(best_combination, collapse = "+"))
  values <- list()
  while (length(columns) > 0) {
    for (i in columns) {
      i_index <- match(i, best_combination)
      combo <- best_combination[-i_index]
      if (length(combo) > 1) {
        combo <- paste(combo, collapse = "+")
      }
      r_squared_ajucted_value <- r_squared_ajucted(combo)
      values <- c(unlist(values), r_squared_ajucted_value)
    }
    best_value <- max(unlist(values))
    best_value_index <- match(best_value, values)
    if (best_value > current_best_r_adj) {
      current_best_r_adj <- best_value
      columns <- columns[-best_value_index]
      best_combination <- best_combination[-best_value_index]
    } else {
      break
    }
  }
  return (paste(best_combination, collapse = "+"))
}

back(columns)
