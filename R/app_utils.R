#
# This script contains all the auxiliary functions necessary to run the shiny 
# app adequately

#' Empty tables generator
#' @description Generate empty the template data.frames 
#' @param type Type of data to fill the table with.
#'
#' @return A data.table filled with the expected type. 

empty_dt <- function(type){
  
  if (type == "numeric") {
    mat <- matrix(0, nrow = 8, ncol = 12) %>% 
      data.table::data.table()
    
  } else if (type == "character_std") {
    mock_concs <- c(0, 1, 10, 20, 50, 60, 100, 200)
    std_sample <- paste("std", mock_concs, sep = "_")
    mock_data <- c(std_sample, rep("sample", 96-8))
    
    mat <- matrix(mock_data, nrow = 8, ncol = 12) %>% 
      data.table::data.table()
  } else if (type == "character_blank") {
    mock_data <- c(rep("std", 8),
                   rep("blank", 8), 
                   rep("sample", 96-16))
    
    mat <- matrix(mock_data, nrow = 8, ncol = 12) %>% 
      data.table::data.table()
    
  }  else {
    mat <- matrix("", nrow = 8, ncol = 12) %>% 
      data.table::data.table()
    
  }
  return(mat)
  
}

#' Tables aggregator
#' 
#' @description Pivots and merges the data.tables 
#' @param table_lists a list of data.tables. 
#' @param env_vars The list of variables.
#'
#' @return A merged long data.table

agg_tables <- function(table_lists, env_vars) {
  
  # Load vars
  layer_name <- env_vars$plate_names
  
  # Generate collection list
  out_long <- vector("list", length = length(layer_name))
  names(out_long) <- layer_name
  
  # Iterate over tables
  n <- 1
  
  for (i in table_lists) {
    
    i[, row := LETTERS[1:8]]
    z <- data.table::melt.data.table(i, id.vars = "row", variable.name = "col")
    z[, col := gsub(pattern = "V", replacement = "", x = col) %>%
        as.numeric()]
    z[, well := paste0(row, col)]
    data.table::setorder(z, row, col, well)
    data.table::setnames(z, "value", layer_name[n])
    out_long[[n]] <- z
    n <- n + 1
  }
  
  merged_table <- Reduce(function (...) { merge(..., all = TRUE) },   # Full join
                         out_long)
  
  return(merged_table)
}

#' Normal linear regression
#' 
#' @description Runs a linear regression from the data
#' @param d data set
#' @return Linear model

fitlm <- function(d){
  std_model <- lm(signal ~ conc, data = d)
  
  return(std_model)
}

#' Interpolate linear regression
#' @description Interpolates the x values from the y values using the linear model
#' @param y_values The y values to interpolate
#' @param model The model to use for interpolation
#' @return The x values

interpolate_lm_x <- function(y_values, model) {
  model_coef <- coef(model)
  par_a <- model_coef[1] 
  par_b <- model_coef[2] 
  
  x_values <- (y_values - par_a)/par_b
  return(x_values)
}

#' 4PL-regression
#' @description Runs a 4PL regression from the data
#' @param d data set
#' @return 4PL model

fit4pl <- function(d) {
  
  model <- drm(signal~conc, data = d, fct = LL.4())
  return(model)
}

#' Interpolate 4PL
#' @description Interpolates the x values from the y values using the 4PL model
#' @param y_values The y values to interpolate
#' @param model The model to use for interpolation
#' @return The x values

interpolate_4pl_x <- function(y_values, model) {
  
  # Load model parameters
  model_coef <- coef(model)
  par_b <- model_coef[1] 
  par_c <- model_coef[2]
  par_d <- model_coef[3] 
  par_e <- model_coef[4] 
  
  # Reverse y=c + (d-c)/(1+exp(b(log(x)-log(e)))
  x_values <- (log(par_d) - log(y_values))/par_b + log(par_e) 
  return(exp(x_values))
}


#' R^2 calculation for DRC
#' @description Calculates the R^2 value for a DRC
#' @param y The y values
#' @param y_hat The predicted y values
#' @return The R^2 value

custom_R2 <- function(y, y_hat) {
  SS_res <- sum((y - y_hat) ^ 2)
  SS_tot <- sum((y - mean(y)) ^ 2)
  R2 <- 1 - SS_res / SS_tot
  return(R2)
}

#' LM to latex
#' @description Generates a latex formula from a linear model.
#' @param model The model to use for interpolation
#' @param R2 The R^2 value
#' @return The latex formula

lm_to_latex <- function(model, R2) {
  # Extract the coefficients and names of the regressors
  coefs <- coef(model)
  names <- names(coefs)
  
  # Initialize the formula string with the intercept
  eqn <- paste0(round(coefs[1], digits = 2))
  
  # Loop over the remaining coefficients and add them to the formula string
  for (i in 2:length(coefs)) {
    coef_str <- round(coefs[i], digits = 2)
    if (coef_str > 0) {
      eqn <- paste0(eqn, " + ", coef_str, names[i])
    } else {
      eqn <- paste0(eqn, " - ", abs(coef_str), names[i])
    }
  }
  
  # Return the final formula string
  formula_str <-c(paste0("$$R^2 = ", R2,"$$"),
                  paste0("$$ y = ", eqn, "$$"))
  
  return(formula_str)
}

#' 4-parameters logistic to latex 
#' @description Generates a latex formula from a 4-parameters logistic model.
#' @param model The model to use for interpolation
#' @param R2 The R^2 value
#' @return The latex formula

LL4_to_latex <- function(model, R2) {
  model_coef <- coefficients(model) %>% round(., digits = 2)
  par_b <- abs(model_coef[1]) 
  par_c <- abs(model_coef[2])
  par_d <- abs(model_coef[3])
  par_e <- abs(model_coef[4]) 
  
  eqn <- paste0("{", par_c, "} + \\frac{", par_d," - ",par_c, "}{1 + e^{", 
                par_b, " *(log(x) - log(", par_e, "))}} ")
  formula_str <- c(paste0("$$R^2 = ", R2, "$$"),
                   paste0("$$ y = ", eqn, "$$")
  )
  return(formula_str)
}

#'  Extrapolate the data in the standard curve
#' @description Generates a latex formula from a 4-parameters logistic model.
#' @param long_data The long data set
#' @param reg_type The regression type
#' @return The latex formula

extrapolate_data <- function(long_data, reg_type = "Linear Regression") {
  
  # Load variables
  z <- data.table::copy(long_data)
  
  # Correct Abs
  z[, corr_wl := wavelenght1 - wavelenght2]
  z[, signal := corr_wl - mean(corr_wl[blank_sample == "blank"])]
  
  # Regression model with standard curve
  std_data <- z[standard != "sample"]
  std_data[, c("type","conc") := tstrsplit(standard, "_")]
  std_data[, conc := as.numeric(conc)]
  std_data[, `log(conc)` := log(conc)]
  
  if (reg_type == "Linear Regression") {
    model <- fitlm(std_data)
    z[, interpolated_conc := interpolate_lm_x(y_values = signal, model)]
    std_data[, fitted_y := predict(model, newdata = .SD)]
    R2 <- custom_R2(y = std_data$signal, std_data$fitted_y)
    latex <- lm_to_latex(model, round(R2, 4))
    
  } else if (reg_type == "4-Parameters Logistic") {
    model <- fit4pl(std_data)
    z[, interpolated_conc := exp(interpolate_4pl_x(y_values = signal, model))]
    std_data[, fitted_y := predict(model, newdata = .SD)]
    R2 <- custom_R2(y = std_data$signal, std_data$fitted_y)
    latex <- LL4_to_latex(model, round(R2, 4))
    
  } else {
    stop()
  }
  
  # Get real conc
  z[, real_conc := interpolated_conc * dilution]
  
  # Plot data
  ggp <- ggplot(std_data, aes(x = conc)) +
    geom_line(aes(y = fitted_y), col = "black") +
    geom_point(aes(y = signal), col = "red") +
    labs(y = "Signal", x = "Concentration") +
    theme_minimal(base_size = 20)
  
  out <- list(data = z, model = model, plot = ggp, latex = latex)
  
  
  return(out)
  
}

# Different useful variables
env_vars <- list(plate_names = c("wavelenght1", "wavelenght2", "dilution", "standard", 
                                 "blank_sample","celltype", "treatments"),
                 plate_types = c("wavelenght1" = "numeric", 
                                 "wavelenght2" = "numeric",
                                 "dilution" = "numeric", 
                                 "standard" = "character_std", 
                                 "blank_sample" = "character_blank",
                                 "celltype" = "character", 
                                 "treatments" = "character"),
                 text = list(introduction = "
              <br>The purpose of this tool is tostreamline the process of reshaping 
              the output data generated by the ClarioStar. By automating this 
              task, users can save time and reduce the risk of errors.<br>
              
              <br>To use the tool, start by inputting the main absorbance values in 
              the template wavelength1, the secondary absorbance in wavelength2, 
              and the dilution factor. Then, navigate to the 'standard' tab and 
              select which wells contain the standard curve. It is important to 
              use the notation 'std_000' when inputting this information, where 
              000 represents the concentration of the standard at that well.<br>
              
              <br>Next, move to the 'blank_sample' tab and indicate which wells 
              contain the blanks. Finally, use the 'celltype' and 'treatments' 
              tabs to input additional information about the different features of each well.<br>
              
              <br>Once all of the necessary information has been inputted, click on 
              the 'process data' button. The output will be displayed as a table 
              below, showing the reshaped data with the interpolated values. 
              This allows for easy analysis and interpretation of the results.<br>

                          ")
                 
)

