
# Libraries ----

# Graphics
library(ggplot2)
library(ggpmisc)  # plot annotation
library(ggpubr)  # plot shortcuts / QoL
library (ggsignif)  # significance annotation

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


# Data import ----
data("mtcars", "UKDriverDeaths", "UKgas", "iris")

# Convert promise data type to data.frame by displaying data
head(mtcars)
head(UKDriverDeaths)
head(iris)
head(UKgas)
head(USArrests)


test = data.frame(deaths=as.matrix(UKgas), date=time(UKgas))

# Saving plots ----

# Base R
png(paste0('file', var1, 'name.png'))
# par(mfrow = c(2,2))  # useful for diagnostic plots
plot(graphic)  # use plot(model) to output diagnostics  
dev.off()   # complete write and close

# Boxplots ----

# +Standard ----
# B+W
ggplot(data = mtcars) + 
  aes(x = as.factor(cyl), y = mpg) +
  geom_boxplot()

# Color
# Blue
ggplot(data = mtcars) + 
  aes(x = as.factor(cyl), y = mpg) +
  geom_boxplot(fill = 'skyblue')

ggplot(data = mtcars) + 
  aes(x = as.factor(cyl), y = mpg) +
  geom_boxplot(fill = 'slategray2')

# +Jittered ----
ggplot(data = mtcars) + 
  aes(x = as.factor(cyl), y = mpg) +
  geom_boxplot(outlier.shape = NA, fill = 'skyblue') +
  geom_point(position = position_jitter(width = 0.05), size = 1.5)

# Jittered points w/ transparency
ggplot(data = mtcars) + 
  aes(x = as.factor(cyl), y = mpg) +
  geom_boxplot(outlier.shape = 'star', fill = 'skyblue') +
  geom_point(position = position_jitter(width = 0.05), size = 1.5, alpha = 0.3)

# +Horizontal ----
# Horizontal plots can be used to compare levels vertically between factors
# Especially useful with faceted plots (can see where box falls on line between groups)
# Standard
ggplot(data = mtcars) + 
  aes(x = as.factor(cyl), y = mpg) +
  geom_boxplot() + 
  coord_flip()

# Horizontal w/ one way facet
# Black and white for printability
ggplot(data = mtcars) +
  aes(x=as.factor(gear), y = mpg, fill=as.factor(gear)) +
  geom_boxplot() + 
  facet_wrap(~ cyl, ncol=1) + 
  coord_flip() +
  guides(fill=guide_legend(reverse=TRUE)) +
  scale_fill_grey(start = 0.9, end = 0.4) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major.x = element_line(color='gray70'),
        panel.grid.major.y = element_blank(),
        panel.background = element_rect(color='gray50', fill=NA, size=1),
        strip.background=element_rect(fill='white'),
        strip.text.x = element_text(size=12, hjust=0))


# Horizontal w/ two way facet
ggplot(data = mtcars) +
  aes(x=as.factor(cyl), y = mpg, fill=as.factor(cyl)) +
  geom_boxplot() + 
  facet_grid(rows=vars(gear), cols=vars(carb), scales="free_x", switch = 'y') +
  coord_flip() +
  guides(fill=guide_legend(reverse=TRUE)) +
  scale_x_discrete(position='left') +
  scale_fill_grey(start = 0.9, end = 0.4) +
  theme(axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position = 'none',
        panel.background = element_rect(color='gray50', fill=NA, size=1),
        strip.text.x = element_text(size=10))

# +Significance----

# ++Stars ----
ggboxplot(mtcars, x = "cyl", y = "mpg") + 
  stat_compare_means(method='anova', label.y = 40) + 
  stat_compare_means(label = "p.signif", method = "t.test",
                     ref.group = "4", label.y=35)

# ++Stars & lines ----
# Create pairwise comparisons for significance lines
box_comparisons = list( c("4", "6"), c("6", "8"), c("4", "8"))

ggboxplot(mtcars, x = "cyl", y = "mpg") +
  stat_compare_means(comparisons = box_comparisons, label.y = c(37, 28, 40), label = "p.signif")

# Stars + lines using ggplot w/ ggsignif package
# Allows for more background customization control - lines don't look as good
ggplot(data = mtcars) +
  aes(x = as.factor(cyl), y = mpg) + 
  geom_boxplot() +
  geom_signif(comparisons = box_comparisons, map_signif_level=TRUE)

# With reference line
ggplot(data = mtcars) +
  aes(x = as.factor(cyl), y = mpg) + 
  geom_hline(aes(yintercept=22), linetype = 'dashed') +
  geom_boxplot() +
  geom_signif(comparisons = box_comparisons, map_signif_level=TRUE)

# Histograms ----

# Standard
ggplot(data = mtcars) +
  aes(x = mpg) + 
  geom_histogram(bins = 10)

# Color
ggplot(data = mtcars) +
  aes(x = mpg) + 
  geom_histogram(bins = 10, fill = 'dodgerblue')

# Minimal
ggplot(data = mtcars) +
  aes(x = mpg) +
  labs(title = 'SHORT SUMMARY OF VIZ', subtitle = 'DESCRIPTION OF DATA ANALYZED') +
  geom_histogram(bins = 10) + 
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) +
  theme(panel.background = element_rect(linetype = 1, color='black', fill=NA, size=1.5))

# Scatter plots ----

# Standard
ggplot(data = mtcars) +
  aes(x = mpg, y = hp) + 
  geom_point()

# With correlation line
ggplot(data = mtcars) +
  aes(x = mpg, y = hp) +
  geom_point() +
  geom_smooth(method = 'lm')

# White background
ggplot(data = mtcars) +
  aes(x = mpg, y = hp) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(title = 'SHORT SUMMARY OF VIZ', subtitle = 'DESCRIPTION OF DATA ANALYZED') +
  theme(axis.title.y=element_blank(),
      panel.grid.major.y = element_blank(),
      panel.background = element_rect(color='gray50', fill=NA, size=1),
      strip.background=element_rect(fill='white'),
      strip.text.x = element_text(size=10, hjust=0))


# +Facet ----
# Use the plot relabeller function to add p values and r2 values to the title
ggplot(data = mtcars) +
  aes(x = mpg, y = hp) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(title = 'SHORT SUMMARY OF VIZ', subtitle = 'DESCRIPTION OF DATA ANALYZED') +
  facet_wrap(~ cyl, scales = 'free') +
  theme(axis.title.y=element_blank(),
        panel.grid.major.y = element_blank(),
        panel.background = element_rect(color='gray50', fill=NA, size=1),
        strip.background=element_rect(fill='white'),
        strip.text.x = element_text(size=10, hjust=0.5))


# Heat maps ----
# Standard
ggplot(data = mtcars) + 
  aes(y=gear, x=cyl) +
  geom_tile(aes(fill=mpg)) 


# B&W
ggplot(data = mtcars) + 
  aes(y=gear, x=cyl) +
  geom_tile(aes(fill=mpg)) + 
  scale_fill_gradient(low='grey', high='black') +
  theme(panel.grid.major=element_blank(),
        panel.background = element_rect(color='gray50', fill=NA, size=1))
        

# Time series ----
# X must be time
# TODO: convert time series data to dataframe so this works
# ggplot(data = df,
#        aes_string(x = 'Time', y = TBI_var, group = 'PatientNo')) +
#   geom_line(color='grey50') +
#   stat_summary(aes(group = 1), geom = 'point',
#                fun.y = mean, color = 'red', size = 2.5) +
#   scale_x_discrete(expand=c(0.05,0)) +
#   theme_minimal()

# Interaction (ANOVA) ----
ggplot(data = mtcars) +
  aes(x=cyl, y=mpg, group=gear, color=gear) +
  labs(title = "INSERT TITLE", subtitle = "INSERT SUBTITLE") +
  stat_summary(fun.y="mean", geom="line") +
  stat_summary(fun.y="mean", geom="point") +
  guides(color=guide_legend(reverse=TRUE)) +
  scale_x_discrete(expand=c(0.1,0.1)) 

# B&W for journals
ggplot(data = mtcars) +
  aes(x=cyl, y=mpg, group=gear, shape=gear) +
  labs(title = "INSERT TITLE", subtitle = "INSERT SUBTITLE") +
  stat_summary(fun.y="mean", geom="line") +
  stat_summary(fun.y="mean", geom="point") +
  guides(color=guide_legend(reverse=TRUE)) +
  scale_x_discrete(expand=c(0.1,0.1)) 



# Logistic regression ----

# Log fit w/statistics % CI

# SE_low and SE_high are manually created in fitted df using model
# log_label uses conf matrix package output 
ggplot(data=fitted_df, aes(x=pred_val, y=fitted_out)) + 
  geom_point(mapping = aes(y=gs_out)) +
  geom_line(size=1.5, alpha=0.8, color='black') + 
  geom_line(mapping = aes(y = rf_pred), color='#00971b', size=1.5, alpha=0.8) + 
  labs(y='outcome',
       title=paste0('Logistic Fit for ', label,
                    ' | n=', nrow(source_df))) +
  geom_ribbon(mapping=aes(ymin=SE_low, ymax=SE_high), alpha=0.2) +
  annotate("text", label=log_label, hjust=0,
           x=0.85*(max(source_df[, 'pred_val'])),
           y = 0.25) + 
  theme_pubr()

# AUC/ROC
ggplot(data=fitted_df, aes(d=outcome, m=pred_val)) +
  geom_roc() +
  labs(y='Sensitivity', x='1 - Specificity') +
  theme_pubr()


# Nonlinear fitting ----

# dummy_df = created dataframe with single sequenced predictor and remaining preds fixed
# plots fitted value from spline model to original model

# lr: solid line
# spline: dotted line

# see analysis.r for dummy df creation

ggplot(data=df, aes(x=pred_var, y=outcome)) +
  geom_point(data=df, aes(x=pred_var, y=outcome)) +
  geom_line(data=dummy_df, aes(x=pred_var, y=outcome_rcs, linetype="dotted"), size=0.75) +
  geom_line(data=dummy_df, aes(x=pred_var, y=outcome_lr, linetype="solid"), size=1) +
  labs(title = 'outcome ~ pred_var RCS & LR Predicted Values',
       caption = paste0('RCS: Model prediction with RCS applied to pred_var\n',
                        'LM: Linear model prediction')) +
  scale_linetype_manual("", values=c("dotted", "solid"),
                        labels = c("RCS", "LM")) +
  theme_pubr() +
  theme(legend.position = 'right', plot.caption=element_text(hjust=0))



# Tables (rmd) ----

# Q summary - easy grouping of multiple vars
# Outputs numeric summaries for full dataframes
library('qwraps2')
```{r, results='asis'}
df = loaded_df
summary <-
  df %>%
    dplyr::select(.data$Var1, .data$Var2) %>%
    qsummary(.,
          numeric_summaries = list("Mean (SD)" = "~ qwraps2::mean_sd(%s)"),
          n_perc_args = list(digits = 1, show_symbol = TRUE, na_rm = TRUE))

by_group_var = summary_table(dplyr::group_by(df, .data$GroupVar), summary)
out = print(by_group_var,
            rtitle = "INSERT TITLE",
            cnames = c("CUSTOMIZE", "COL", "NAMES"))
```

# Kable 
# Standard 
knitr::kable(df)

# Collapse rows automatically, remove extra line separators
k_table = knitr::kable(df, booktabs = T)
collapse_rows(k_table)

# Auto config
# gray!30 - a light gray for differentiation of rows 
# linesep = "" - makes all rows equal height (kable makes every 5th row taller by default)
  knitr::kable(summary_df, booktabs = T, linesep = "") %>%
    kable_styling(latex_options = 'striped', stripe_color = 'gray!30')


# Collapsed rows
# Row_spec - manually change row color etc.. 
  # 2:3, 6:7 makes two lines gray every other two lines (used when df is grouped)
# scale_down - fill page
# Latex_hline - remove horizontal divider
kable(df, 'latex', booktabs = T, linesep = "", row.names = F) %>%
  kable_styling(latex_options = "scale_down") %>%
  row_spec(c(2:3, 6:7), extra_latex_after = "\\rowcolor{gray!30}") %>%
  collapse_rows(1, latex_hline = 'none')


# Conditional coloring
# Requires rows to not be collapsed

# Highlight and bold significant p values
kable(df, booktabs = T, row.names = F, linesep = "") %>%
    kable_styling(latex_options = "scale_down") %>%
      row_spec(which(df$P_Value < 0.05), bold = T, color = "red")


# Long table 
kable(df, longtable = T, booktabs = T, row.names = F, linesep = "") %>%
    kable_styling(latex_options = c("repeat_header"))