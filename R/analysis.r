library(ggplot2)
library(ggpmisc)   # plot annotation
library(ggpubr)    # plot shortcuts / QoL
library (ggsignif) # significance annotation

# Data analysis
library(gvlma)     # 
library(ARTool)    # ANOVA nonparametrics
library(lmtest)    # breusch-pagan
library(EnvStats)  # boxcox 
library(Smisc)     # test for linear independence

# Data cleaning
library(dplyr)
library(miscTools)
library(reshape)

# Formatting output
library(knitr)
library(kableExtra)

# Data import ----
data("mtcars", "UKDriverDeaths", "UKgas", "iris")

# Helper functions 
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

Transformation_Labeller = function(transformation) { 

    trans_labels = c('-3', '-2', '-1', '-0.5', '0', '0.5', '1', '2', '3')
    names(trans_labels) = c('Inverse_Cube', 'Inverse_Square', 'Inverse', 'Inverse_Sqrt',
                            'Log', 'Sqrt', 'Identity', 'Square', 'Cube')

    return(names(trans_labels[trans_labels == transformation]))
}

Diagnostics = function(outcome, predictor, source_df, transformation, trans_label){
    
    # add source_df to global environment for boxcox
    source_df <<- source_df
    
    lr_summary = as.data.frame(matrix(ncol=2, nrow=7))
    colnames(lr_summary) = c('Diagnostic', 'Value')
    lr_summary[, 'Diagnostic'] = c('Model Formula', 'Normality', 'Homoscedasticity','Skewness',
                                'Kurtosis', 'Link Function', 'Conv. Transformation')
    
    
    if (transformation == 'identity'){
        model_formula <<- as.formula(paste0(outcome, ' ~ ', predictor))
    } else{
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

Regression_Analyzer = function(outcome, predictor, source_df, transformation) { 

    trans_label = Transformation_Labeller(transformation)

    Var_Exploration(outcome, predictor, source_df, trans_label)
    lr_results = Diagnostics(outcome, predictor, source_df, transformation, trans_label)
    lr_summary = lr_results[[2]]
    
    # wipe function specific objects from environment (needed for boxcox)
    rm(source_df, envir = .GlobalEnv)
    rm(model_formula, envir = .GlobalEnv)
    
    return(lr_summary)
}

# Convert promise data type to data.frame by displaying data
head(mtcars)
head(UKDriverDeaths)
head(iris)
head(UKgas)
head(USArrests)

# Regression ----


lr_summary = Regression_Analyzer('Outcome', 'Predictor', df, 'Identity')
trans_summary = Regression_Analyzer('Outcome', 'Predictor', df, lr_summary[7, 'Value'])


# **Summary tables ----



# Logistic ----

# **Diagnostics ----

# **Summary graphics ----

# **AUC-ROC ----

# **Summary tables ----






# Multivariate ----

# **Diagnostics ----

# **Remediation ----


# splines - when nonlinearity is not met
dummy_df = as.data.frame(matrix(nrow = 100, ncol = 5))
colnames(dummy_df) = c('y', 'Var2', 'Var3', 'Var4', 'Var5')

# Create dummy values for Var2 based on actual max and min of var in real dataset
dummy_df$Var2 = seq(min(subset(df[, 'y'],
                                       !is.na(df[, 'y']), na.rm=T)),
                            max(subset(df[, 'y'],
                                       !is.na(df[, 'y']), na.rm=T)),
                            len=100)

# Set confounders as constants based on their median/mode
dummy_df$Var3 = 5  # median/mode
dummy_df$Var4 = 25  # median
dummy_df$Var5 = 1  # median/mode

# fit initial model and rcs model
model = glm(y ~ Var1 + Var2 + Var3 + Var4, data=df)
rcs_model = ols(y ~ rcs(Var1) + Var2 + Var3 + Var4, data=df)

# Predict values for dummy df
dummy_df$Y_pred = predict(model, dummy_df)
dummy_df$Y_Var2_RCS = predict(rcs_model, dummy_df)

# Likelihood ratio test
anova(model, rcs_model, test='LRT')

# Plot to compare difference in predictions
ggplot(data=df, aes(x=Var2, y=y)) +
  geom_point(data=df, aes(x=Var2, y=y)) +
  geom_line(data=dummy_df, aes(x=Var2, y=Y_Var2_RCS, linetype="dotted"), size=0.75) +
  geom_line(data=dummy_df, aes(x=Var2, y=Y_pred, linetype="solid"), size=1) +
  labs(title = 'Y ~ Var2 RCS & LR Predicted Values',
       subtitle = paste0('RCS: Fitted values using Var2 RCS\n',
                         'Prediction: Model prediction with all parameters except Var2 held constant')) +
  scale_linetype_manual("", values=c("dotted", "solid"),
                        labels = c("RCS", "Prediction")) +
  theme_bw()


# **Summary graphics ----

# **Summary tables ----





# Association ----