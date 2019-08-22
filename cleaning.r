
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


# Reshaping ----

# Reshapes df by column name (where columns indicate time points)
# sep: use to identify separator between var name and time point
melt_df = reshape(df, direction = "long",
# REORDERING ----

# Reorder factor by length (number of obs.)
# Reverse order (high -> low)
df$Var1 = reorder(df$Var1, df$Var1, function(x){ -length(x)})
# REORDERING ----

# Reorder factor by length (number of obs.)
# Reverse order (high -> low)
df$Var1 = reorder(df$Var1, df$Var1, function(x){ -length(x)})

# Default (low -> high)
df$Var1 = reorder(df$Var1, df$Var1, FUN = length)

# Manual reorder
# length of levels must match original, can swap order etc. to fit viz needs
df$Var1 = factor(df$Var1, levels= c('a', 'b'))


# Types ----

# Convert factor to numeric
# Optimized: https://stackoverflow.com/questions/3418128/how-to-convert-a-factor-to-integer-numeric-without-loss-of-information
df$factor_var = as.numeric(levels(df$factor_var))[df$factor_var ]


# Default (low -> high)
df$Var1 = reorder(df$Var1, df$Var1, FUN = length)

# Manual reorder
# length of levels must match original, can swap order etc. to fit viz needs
df$Var1 = factor(df$Var1, levels= c('a', 'b'))


# Types ----

# Convert factor to numeric
# Optimized: https://stackoverflow.com/questions/3418128/how-to-convert-a-factor-to-integer-numeric-without-loss-of-information
df$factor_var = as.numeric(levels(df$factor_var))[df$factor_var ]

                    idvar = c("ID"),
                    timevar = "Time",
                    sep = "_",
                    varying = 2:ncol(df))


# DF MANIPULATION ----

# *matching ----

# VLOOKUP column replacement - looks up original var and replaces it with updated var
target_df$original_var = lookup_df$new_var[match(target_df$original_var, lookup_df$original_var)]

# Match by row -> get index
row_index = which(df[, 'Col'] == 'Condition')

# Match by row based on list of potential matches
row_index = sapply(match_list, function(x) which(df$var %like% x)))

# Match by col -> get index
col_index = which(colnames(df) == 'Condition')

# * changing vars----
# conditionally change var1 in df based on value of var2
df$var1[df$var2 == 'Condition'] = 'Value'

# without atomic vectors
df[which(df$var2 =='Condition'), 'var1'] = "Value"

# conditionally change var1 in df based on matches in var2 to a list
df[unlist(lapply(match_list, function(x) which(df$var2 %like% x))), 'Var1'] = 'Value'

# Sort dataframe 
# Alphabetically
df = df[order(df$var), ]

# Filter by contents of another list
new_df = old_list[old_list %in% colnames(old_df)]


# REORDERING ----

# Reorder factor by length (number of obs.)
# Reverse order (high -> low)
df$Var1 = reorder(df$Var1, df$Var1, function(x){ -length(x)})

# Default (low -> high)
df$Var1 = reorder(df$Var1, df$Var1, FUN = length)

# Manual reorder
# length of levels must match original, can swap order etc. to fit viz needs
df$Var1 = factor(df$Var1, levels= c('a', 'b'))


# Types ----

# Convert factor to numeric
# Optimized: https://stackoverflow.com/questions/3418128/how-to-convert-a-factor-to-integer-numeric-without-loss-of-information
df$factor_var = as.numeric(levels(df$factor_var))[df$factor_var ]
