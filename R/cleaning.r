
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
time_reshaped_df = reshape(df, direction = "long",
                    idvar = c("ID"),
                    timevar = "Time",
                    sep = "_",
                    varying = 2:ncol(df))


# Melt by repeated values
melted_df = melt(df, id=c('Var1', 'Var2'),
               measure.vars = colnames(adult_rand_df[c('PCL_Gold_Standard', 'PCL_GOSE')]))
# Data manipulation ----
# VLOOKUP column replacement - looks up original var and replaces it with updated var
target_df$original_var = lookup_df$new_var[match(target_df$original_var, lookup_df$original_var)]


# Reorder factor by length (number of obs.)
# Reverse order (high -> low)
df$Var1 = reorder(df$Var1, df$Var1, function(x){ -length(x)})

# Default (low -> high)
df$Var1 = reorder(df$Var1, df$Var1, FUN = length)


# Manual reorder
# length of levels must match original, can swap order etc. to fit viz needs
df$Var1 = factor(df$Var1, levels= c('a', 'b'))


# Sort dataframe 
# Alphabetically
df = df[order(df$var), ]

# Filter by contents of another list
new_df = old_list[old_list %in% colnames(old_df)]


# Types ----

# Convert factor of numbers to numeric
# Optimized: https://stackoverflow.com/questions/3418128/how-to-convert-a-factor-to-integer-numeric-without-loss-of-information
df$factor_var = as.numeric(levels(df$factor_var))[df$factor_var ]


# Convert categorical factor to numeric (for use in models etc)
# num_list = manually designated numbers to be assigned to levels in factor_var
df$num_var = c(num_list)[as.numeric(df$factor_var)]
