
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

# Creates new var without outlier
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

# Pulls r2, r, and pvalue from simple LR between two numeric vars
# If data is not normal, gets spearman instead
Num_Test = function(var, num_var, df){
  
    row_pointer = which(num_var_df[, 'var'] == var)
    shapiro_p = shapiro.test(df[, var])[['p.value']]
    
    if (shapiro_p < 0.05) {
      r_val = cor(df[, var], df[, 'num_var'], method = 'spearman')  
      r2_val = NA
      p_fmt = NA
      
    } else if (shapiro_p >= 0.05) { 
      num_var_formula = as.formula(paste(var, "~ num_var"))
      num_var_fit = lm(num_var_formula, data = df)
      
      r_val = cor(df[, var], df[,'num_var'], method = 'pearson')
      r2_val = summary(num_var_fit)$r.squared
      p_val = summary(num_var_fit)$coefficients[2, 4]
      
      bonf_val = 0.05 / 5
      p_fmt = Format_PVal(p_val, 3, bonf_val)
      
    }
    
    num_var_df[row_pointer, 3] <<- round(r2_val, 3)
    num_var_df[row_pointer, 4] <<- p_fmt
    num_var_df[row_pointer, 5] <<- round(r_val, 3)
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

Diagnostic_Plots = function(outcome, predictor, source_df, transformation, trans_label) {
    
    trans_out = Transformation_Labeller(transformation)

    # Overall diagnostics
    png(paste0('viz/model', trans_out, '_diagnostics.png'), 
        width=1200, height=1200, res=200)
    par(mfrow = c(2,2), mar=c(2,2,2,2))
    plot(model_lm) 
    dev.off()

    # make residuals
    model_resid = model_lm$residuals
    MSE_resid = anova(model_lm)[2,3]
    model_semistud = model_resid / sqrt(MSE_resid)
    
    # residual qqplot
    png(paste0('viz/residual_qq', trans_out, '.png'))
    plot(model_lm, which=(c(2, 1)))
    dev.off()

    # semistud qqplot
    source_df$model_semistud = model_semistud
    ggplot(source_df, aes(sample=model_semistud)) + 
        geom_qq() +
        geom_qq_line() +
        labs(title = 'Outcome & Predictor Semistudentized Residuals QQ Plot') +
        theme_minimal()
    ggsave('viz/semistud_resid_qqplot', trans_out, '.png')
    
    # semistud & fitted values
    ggplot(source_df, aes(x=pred, y=model_semistud)) + 
        geom_point() + 
        labs(title = 'Semistudentized Residuals by Predictor',
             y= 'Semistudentized residuals', x='Fitted Predictor') +
        theme_minimal()
    ggsave('viz/semistud_resid_fitted', trans_out, '.png')
    
    png('viz/model_boxcox.png')
    plot(boxcox(model_lm))
    dev.off()
}

# Var selection ----

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


# Regression ----

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

# Summarizes multiple regression models given model fit
Regression_Summarizer = function(fit) {
  
  # Get p values of all predictors: -1 excludes intercept
  multi_p_values = summary(fit)[['coefficients']][, 4][-1]
  
  # Get coefs of all predictors: -1 excludes intercept
  multi_coefs = summary(fit)[['coefficients']][, 1][-1]
  
  # Only predictors with p < 0.05
  sig_predict = names(multi_p_values[multi_p_values < 0.05])
  
  output = list('sig_preds' = sig_predict, 'preds' = names(multi_p_values), 
                'p_vals' = multi_p_values, 'coefs' = multi_coefs)
  return(output)
  
}

# Basic exploratory plots for continuous variables
Var_Exploration = function(outcome, predictor, source_df, transformation, trans_label){
    for(var in c(outcome, predictor)) {
    
    # Boxplot
    ggplot(source_df, aes_string(y=var)) + 
        geom_boxplot() + 
        labs(title = paste0(var, ' Boxplot')) +
        theme_minimal() + 
        theme(axis.title = element_blank(),
              axis.text.x = element_blank())
    
    ggsave(paste0('viz/', var, '_boxplot.png'))
    
    # Histogram
    ggplot(source_df, aes_string(x=var)) + 
        geom_histogram() +
        labs(title = paste0(var, ' Histogram')) +
        theme_minimal()
    
    ggsave(paste0('viz/', var, '_histogram.png'))
           
    # QQ plot
    ggplot(source_df, aes_string(sample=var)) + 
        geom_qq() +
        geom_qq_line() +
        labs(title = paste0(var, ' QQ Plot')) +
        theme_minimal()
    ggsave(paste0('viz/', var, '_qqplot.png'))
    }
    
    # Explore Y ~ X
    ggplot(source_df, aes_string(x=predictor, y=outcome)) + 
        geom_point() + 
        labs(title = paste0(outcome, ' by ', predictor)) +
        theme_minimal()
    ggsave(paste0('viz/', outcome, '_', predictor, '_scatter.png'))

}

# Converts boxcox transformation to label for filenames
Transformation_Labeller = function(transformation) { 

    trans_labels = c('-3', '-2', '-1', '-0.5', '0', '0.5', '1', '2', '3')
    names(trans_labels) = c('Inverse_Cube', 'Inverse_Square', 'Inverse', 'Inverse_Sqrt',
                            'Log', 'Sqrt', 'Identity', 'Square', 'Cube')

    return(names(trans_labels[trans_labels == transformation]))
}


# Runs diagnostics on simple linear model and returns summary of assumptions
Diagnostics = function(outcome, predictor, source_df, transformation, trans_label){
    
    # add source_df to global environment for boxcox
    source_df <<- source_df
    
    lr_summary = as.data.frame(matrix(ncol=2, nrow=7))
    colnames(lr_summary) = c('Diagnostic', 'Value')
    lr_summary[, 'Diagnostic'] = c('Model Formula', 'Normality', 'Homoscedasticity','Skewness',
                                'Kurtosis', 'Link Function', 'Conv. Transformation')
    
    
    if (transformation == 'identity'){
        model_formula <<- as.formula(paste0(outcome, ' ~ ', predictor))
    } else if (transformation == '0') {
      
      # TODO: handle 0 vals
      log_outcome = log(source_df[, outcome])
      model_formula <<- as.formula(paste0(log_outcome, ' ~ ', predictor))

    } else {
        model_formula <<- as.formula(paste0(outcome, '^', transformation, ' ~ ', predictor))
    }
    
    # fit model and get residuals
    model = lm(model_formula, data=source_df)
    fit_resid = model[['residuals']]
    
    #overall assessment
    gen_assessment = gvlma(model_formula, data=source_df)

    # gets model terms as character for printing in diagnostic summary
    formula_char = as.character(model_formula)
    model_out = paste0(formula_char[2], formula_char[1], formula_char[3])
    
    # shapiro & breusch-pagan tests for key model assumptions
    normality = shapiro.test(fit_resid)[['p.value']][[1]]
    homosced = bptest(model_formula, data=source_df)[['p.value']][[1]]

    # pulled from gvlma summary
    skew = gen_assessment[['GlobalTest']][['DirectionalStat1']][['pvalue']]
    kurt = gen_assessment[['GlobalTest']][['DirectionalStat2']][['pvalue']]
    link = gen_assessment[['GlobalTest']][['DirectionalStat3']][['pvalue']]

    # gets most convenient transformation for the model
    lambda_index = which.max(boxcox(model)[['objective']])
    conv_trans = boxcox(model)[['lambda']][lambda_index]

    lr_summary[, 'Value'] = c(model_out, normality, homosced,
                              skew, kurt, link, conv_trans)
    
    return(list(model, lr_summary))
}


# Pointer function that directs exploratory plots, diagnostics, and transformations
Regression_Analyzer = function(outcome, predictor, source_df, transformation='identity') { 

    trans_label = Transformation_Labeller(transformation)

    Var_Exploration(outcome, predictor, source_df, trans_label)
    lr_results = Diagnostics(outcome, predictor, source_df, transformation, trans_label)
    lr_summary = lr_results[[2]]
    
    # wipe function specific objects from environment (needed for boxcox)
    rm(source_df, envir = .GlobalEnv)
    rm(model_formula, envir = .GlobalEnv)
    
    return(lr_summary)
}

# Given a multiple linear regression model, creates log, log-link, and square root transformed models
# Requires: Regression_Summarizer
Multi_Transform = function(lr_model) { 
  
  # Read in model
  lr_fit = lr_model[['fit']]
  
  lr_formula = lr_model[['lr_formula']]
  df = lr_model[['df']]
  
  lr_fit[['call']][['formula']] = lr_formula  # handles parsing error
  
  predictors = lr_model[['preds']]

  lr_fit[['call']][['data']] = df
  
  # Sqrt model
  df[, 'sqrt_outcome'] = sqrt(df[, outcome])
  
  sqrt_formula = as.formula(paste0('sqrt_outcome ~ ', paste(predictors, collapse =' + ')))
  sqrt_fit = lm(sqrt_formula, data = df)
  sqrt_summary = Regression_Summarizer(sqrt_fit)
  
  sqrt_results = list('fit' = sqrt_fit, 'df' = df, 'type' = 'Sqrt',
                      'sig_preds' = sqrt_summary[['sig_preds']],
                      'preds' = sqrt_summary[['preds']], 
                      'p_vals' = sqrt_summary[['p_vals']],
                      'coefs' = sqrt_summary[['coefs']])
  
  
  # Log prep: imputed df for when values need to be altered
  imp_df <<- df[colnames(df) %in% c(predictors, outcome)]

  # Only outcomes and categorical predictors have values of 0 
  if(min(imp_df[, outcome], na.rm = T) == 0)
    imp_df[, outcome] <<- (imp_df[, outcome] + 1)

  # Log model
  imp_df[, 'log_outcome'] = log(imp_df[, outcome])
  log_formula = as.formula(paste0('log_outcome ~ ', paste(predictors, collapse =' + ')))
  log_fit = lm(log_formula, data = imp_df)
  log_summary = Regression_Summarizer(log_fit)
  
  log_results = list('fit' = log_fit, 'df' = imp_df, 'type' = 'Log',
                          'sig_preds' = log_summary[['sig_preds']],
                          'preds' = log_summary[['preds']], 
                          'p_vals' = log_summary[['p_vals']],
                          'coefs' = log_summary[['coefs']])
  
  
  # Log link model
  log_link_fit = glm(lr_formula, family=gaussian(link="log"), data=imp_df)
  log_link_summary = Regression_Summarizer(log_link_fit)
  
  log_link_results = list('fit' = log_link_fit, 'df' = imp_df, 'type' = 'Log-link',
                'sig_preds' = log_link_summary[['sig_preds']],
                'preds' = log_link_summary[['preds']], 
                'p_vals' = log_link_summary[['p_vals']],
                'coefs' = log_link_summary[['coefs']])
  
  
  output = list(log_results, log_link_results, sqrt_results)
  return(output)
}

# Summarizes regression results in matrix
# R: All outcomes | C: All predictors
# Accounts for interaction terms, which are summarized in a single column
Multi_Summary = function(output, outcome) {

  predictors = output[['preds']]
  coefs = output[['coefs']]
  p_vals = output[['p_vals']]
  
  fit = output[['fit']]
  transformation = output[['type']]

  # Splitting of interaction from predictors
  interactions = predictors[grepl(':', predictors)]
    
  # Model diagnostics
  png(filename=paste0('viz/summary/', outcome, '_', transformation,
                      '_multi_diagnostic.png'))
  par(mfrow=c(2,2))
  plot(fit)
  dev.off()
  
  for (predictor in predictors) {
    
    # Interaction Analysis
    if (predictor %in% interactions) {
      inter_results = list()
      for (interaction in interactions) { 
        inter_p = round(as.numeric(p_vals[interaction]), 3)
        result = paste0(interaction, ' p: ', inter_p)
        inter_results = append(inter_results, result)                            
      }
      output = paste0(unlist(inter_results), collapse= ' | ')  
    
    # Predictor analysis analysis
    } else { 
      result_coef = round(as.numeric(coefs[predictor]), 3)
      result_p = round(as.numeric(p_vals[predictor]), 3)
      
      if (time == '24hr' & result_p < bonf_24hr)
        result_p = paste0(as.character(result_p), '***')
      
      if (time == '6mo' & result_p < bonf_6mo)
        result_p = paste0(as.character(result_p), '***')

      output = paste0('coef: ', result_coef, ' | p: ', result_p)
      }
    # df pointing
      
    sum_row = which(df[, 'Outcome'] == outcome &
                    df[, 'Transformation'] == transformation)

    if (predictor %in% interactions) { 
      sum_col = which(colnames(df) == 'Interaction')
    } else { 
      sum_col = which(colnames(df) == predictor)
      }
    
    df[sum_row, sum_col] <<- output
      
  }
}

# Given a dataframe with rows of potential models based on lasso selection, calculate adjustment for
# multiple comparisons (Bonferonni)
Bonf_Calc = function(df) {
  bonf_counter = 0
  
  for (i in 1:nrow(df)) {
    var_check = max(df[i, 2:10])
    if (var_check != 0)
      bonf_counter = bonf_counter + 1
  }
  
  cat('There are', bonf_counter, 'outcomes with at least one selected predictor')
  bonf_value = 0.05 / bonf_counter
  return(bonf_value)
}


# Formatting ----

# Takes var, looks up in helper df, returns relabeled var
# Also supports labels with r2 and p vals from stat summary dfs for plotting
Relabeller = function(var, df_name="none") { 
  var_out = var_helper[var_helper$var == var, 2]
  unit = var_helper[var_helper$var == var, 4]    
  
  if (df_name == "linear_regression") {
    p_val = Age_df[Age_df$var == var, 3]
    r2 = Age_df[Age_df$var == var, 2]
    new_label = paste0(var_out, " ", unit, "\n", "R", "\u00b2", " = ", r2, " | p = ", p_val)
    
  } else if (df_name == "none") {
    new_label = paste0(var_out, " ", unit)
    
  }
  return(new_label)
}


Format_PVal = function(val) {
  if (val < 0.001) {
    result = '< 0.001'
  } else {
    result = round(val, 3)
  }
  return(result)
}

# Retrieves p value from multivariate model
Model_P = function(model) {
  f = summary(model)$fstatistic
  p = pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) = NULL

  return(p)
}

# Converts factor to numeric 
To_Numeric = function(x) {
  return(as.numeric(levels(x))[x])
}

# Categorizes a series of variables based on specified outcome
# match_list: list of strings that will partially match with category
# label: the assigned category label
# df: Dataframe with the format Var | Label
# Example: given outcomes a, b, c are tests for mood
## match_list = c('a', 'b', 'c') | label = 'Mood'
Var_Labeller = function(match_list, label) {
  matching_out = lapply(match_list, function(x) which(df$Var %like% x))
  out_measures[unlist(matching_out), 'Label'] <<- label
}

# Returns logistic regression stats using roc library
# cm: confusion matrix made using caret library
# actual: actual values; fitted: fitted values
Get_Fit_Stats = function(cm, actual, fitted, df){

  auc = roc(df[, actual], df[, fitted])[['auc']]
  
  plot_stats = list(cm[['byClass']][c(1:4)])
  stats_out = round(unlist(c(auc, plot_stats)), 4)

  names(stats_out)[c(1, 4:5)] = c('AUC', 'PPV', 'NPV')
  
  return(stats_out)
}