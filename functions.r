# Programmer: Jeff Brennan
# Last updated: 5/31/2019

# TODO: make functions agnostic
# Libraries ----

# Graphics
library(ggplot2)
library(ggpmisc)  # plot annotation
library(ggpubr)  # plot shortcuts / QoL

# Data analysis
library(gvlma)  # 
library(ARTool)  # ANOVA nonparametrics

# Data cleaning
library(dplyr)
library(miscTools)
library(reshape)

# Formatting output
library(knitr)
library(kableExtra)


# Plotting ----

Plot_Relabeller = function(var, df_type="none") { 
  # Requires 2 df premade - formats title labels for plots with statistics and p values
  # var_units: 3 cols (var name, new var name, units)
  # var_df: 3 cols (var name, r2 / x2, p value)

  # Format var for plot (r shorthand -> readable text) + units
  var_out = var_units[var_units$name == var, 2]
  unit = var_units[var_units$var == var, 3]    
  
  # Correlation test
  if (df_type == "num") {
    p_val = var_df[var_df$var == var, 3]
    r2 = var_df[var_df$var == var, 2]
    new_label = paste0(var_out, ' ', unit, '\n', 'R', '\u00b2', ' = ', r2, ' | p = ', p_val)

  # Chi test
  } else if (df_name == "cat") {
    x2 = var_df[var_df$var == var, 2]
    p_val = var_df[var_df$var == var, 3]
    new_label = paste0(var_out, ' ', unit, '\n', 'X', '\u00b2', ' = ', x2, ' | p = ', p_val)
    
  # No test
  } else if (df_name == "none") {
    new_label = paste0(var_out, ' ', unit)
    
  }
  return(new_label)
}

Plot_Predictor = function(predictor, outcome, predictor_time, TBI_NA) {

  # Plots scatter plot of significant linear regressoion predictors
  lr_formula = as.formula(paste(outcome, "~", predictor))
  LR_FIT = lm(lr_formula, data=df)
  r2 = summary(LR_FIT)$r.squared
  r2_text = paste0('r2: ', round(r2, 4))
  
  ggplot(data = df) +
    aes_string(x = predictor, y = outcome) +
    geom_point() +
    geom_smooth(method='lm', formula = y~x) +
    labs(title = r2_text)
  
  # Assess the impact of transformations on the linear relationship
  transformations = Model_Diagnostics(predictor, outcome, predictor_time, TBI_NA)
  return (transformations)
}

Model_Diagnostics = function(predictor, outcome, time, df) {
  log_out_c = 0
  log_pred_c = 0
  
  # Adds c (1) to log model when parameter contains 0 to handle error
  if (min(df[, outcome]) == 0)
    log_out_c = 1
  
  if (min(df[, predictor]) == 0)
    log_pred_c = 1
  
  log_out = log(df[, outcome] + log_out_c)
  log_pred = log(df[, predictor] + log_pred_c)
  sqrt_out = sqrt(df[, outcome])
  sqrt_pred = sqrt(df[, predictor])
  
  lr_formula = as.formula(paste(outcome, "~", predictor))
  log_formula = as.formula(paste(log_out, "~", log_pred))
  sqrt_formula = as.formula(paste(sqrt_out, "~", sqrt_pred))
  
  lr_fit = lm(lr_formula, data=df)
  log_fit =  lm(log_formula, data=df)
  sqrt_fit = lm(sqrt_formula, data=df)
  
  # fix coefficient names
  names(lr_fit$coefficients)[2] = predictor
  names(log_fit$coefficients)[2] = predictor
  names(sqrt_fit$coefficients)[2] = predictor
  
  # Plot diagnostics
  par(mfrow=c(2,2))
  plot(lr_fit, main='Untransformed')
  dev.off()

  par(mfrow=c(2,2))
  plot(log_fit, main='Log')
  dev.off()
  
  par(mfrow=c(2,2))
  plot(sqrt_fit, main='Sqrt')
  dev.off()
  
  # Returns models 
  output = list('Untransformed' = lr_fit, 'Log' = log_fit, 'Sqrt' = sqrt_fit)
  return(output)
  
}

Handle_Outliers = function(var) {
  # Creates new vars with dropped outliers
  cat('\n', var)
  marker_noout = paste0(var, "_noout")
  
  # New column based on old values, outliers will be assigned NA
  df[, var_noout] <<- df[, var]
  
  # Lower limit: can increase 1.5 multiplier to make more conservative
  marker_low = quantile(df[, var], 0.25, na.rm = T) -
    1.5 * IQR(df[, var], na.rm = T)
  
  # Upper limit: can increase 1.5 multiplier to make more conservative
  marker_up = quantile(df[, var], 0.75, na.rm = T) +
    1.5 * IQR(df[, var], na.rm = T)
  
  # Defines normal observation as between lower and upper limits
  marker_noout = df[df[, var] > marker_low &
                           df[, var] < marker_up, ]
  
  # Shrinks data frame to var ID and level of selected marker
  marker_noout = marker_noout[, c("ID", var)]
  
  #  ID differences between full list and list without outliers
  marker_out = setdiff(subset(df[, "ID"], !is.na(df[, var])),
                       marker_noout[, "ID"])
  
  if (length(marker_out) > 0)
    df[df[, "ID"] %in% marker_out, ][, var_noout] <<- NA
  
  # Compare five num summaries of original and new values
  print(fivenum(df[, var], na.rm=T))
  print(fivenum(df[, var_noout], na.rm=T))
  
  # Display number of outliers dropped
  cat(paste0("Outliers dropped: ", length(marker_out), "\n"))
}


LR_Test = function(var, num_var, df){
  # Pulls r2 and pvalue from simple LR between two numeric vars

  lr_formula = as.formula(paste(var, ' ~ ', num_var))
  lr_fit = lm(lr_formula, data = df)
  
  r2 = summary(lr_fit)$r.squared
  p_val = summary(lr_fit)$coefficients[2, 4]
  
  lr_df[lr_df$var == var, 2] <<- round(r2, 3)
  
  if (p_val < 0.001) {
    lr_df[lr_df$var == var, 3] <<- '< 0.001'
  } else {
    lr_df[lr_df$var == var, 3] <<- round(p_val, 3)
  }
}

Cat_Test = function(var, cat_var){
  
  cat_formula = as.formula(paste(var, ' ~ ', cat_var))
  cat_result = kruskal.test(cat_formula, data = subset(df, !is.na(cat)))
  
  x2 = cat_result$statistic
  p_val = cat_result$p.value
  
  cat_df[cat_df$var == var, 2] <<- round(x2, 3)
  
  if (p_val < 0.001) {
    cat_df[cat_df$var == var, 3] <<- '< 0.001'
  } else {
    cat_df[cat_df$var == var, 3] <<- round(p_val, 3)
  }
}


Shapiro_Get = function(df, condition, var){
  # Returns shapiro test for a number of comparisons

  for (i in seq(1:length(condition))){
    var_sub = df[, var][df[, condition] == i]
    var_sub = var_sub[!is.na(var_sub)]
    sub_len = length(var_sub)
    
    if(sub_len >= 3){
      
      var_shapiro = shapiro.test(var_sub)
      var_p = round(var_shapiro$p.value, 4)
      
      sub_len = sprintf('%02d', sub_len)
      
      if (var_p < 0.001)
        var_p = '< 0.001'
      
      cat(paste(i, '|| n =', sub_len, '||p:', var_p), '\n')
      
    } else {
      cat(paste(i, '|| n =', sub_len, '||', 'Insufficient observations\n'))
    }
  }
}


Lassofier = function(out, pred_time, pred_range, pred_df, out_df) {
  # Performs LASSO analysis on a given set of outcomes and predictors
  df = out_df[!is.na(out_df[out]) ,]
  pred = as.matrix(df[pred_range])
  
  CV = cv.glmnet(pred, df[[out]])
  lambda = CV$lambda.min
  
  plot(CV)
  title(out, line = 3)
  dev.off()
  
  coef = CV$glmnet.fit$beta[, CV$glmnet.fit$lambda == lambda]
  if (sum(coef) == 0) {
    
    cat('\nAll coefficients are 0\n')
    cat('======================================\n')
    
  }else{
    
    coef = append(coef, out, after=0)
    pred_coef_24hr <<- insertRow(pred_coef_24hr, counter_24hr, coef)
    counter_24hr <<- counter_24hr + 1

    results = data.frame(value=coef)
    print(results)
    
    cat('======================================\n')
  }
}

# Performs linear regression on each significant predictor identified in LASSO
Regressifier = function(coef, i, outcome) {
  
  # Data maniplulation
  row_select = coef[i, 2:13]
  selected_x = names(row_select[, row_select != 0, drop = FALSE])
  
  print(outcome)
  
  df_out = df[!is.na(df[outcome]) ,]
  
  # Converting predictors into format to pass into lr model
  predictors = paste(selected_x, collapse=" + ")
  lr_formula = as.formula(paste(outcome, "~", predictors))
  lr_fit = lm(lr_formula, data=df_out)
  
  # Extracting predictors with significant p-values
  p_value = summary(lr_fit)$coefficients[, 4]
  predict = tail(p_value, (length(p_value) - 1))
  sig_predict = names(predict[predict < 0.05])
  
  print(kable(xtable(lr_fit)))
  
  output = list('lr_fit' = lr_fit, 'df_out' = df_out, 'sig_pred' = sig_predict)
  return(output)
}


# Formatting ----

Format_PVal = function(val) {
  if (val < 0.001) {
    result = '< 0.001'
  } else {
    result <<- round(val, 3)
  }
}


# Table manipulation ----
# VLOOKUP column replacement - looks up original var  and replaces it with updated var
target_df$original_var = lookup_df$new_var[match(target_df$original_var, lookup_df$original_var)]
