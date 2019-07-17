# Programmer: Jeff Brennan
# Last updated: 5/28/2019


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


# Colors ----

# +Favorites ----

# Text codes 
light_colors = c('skyblue', 'slateblue', 'slategray2', 'lightgreen')
bold_colors = c('dodgerblue', 'darkorchid', 'firebrick1', 'limegreen')


# Latex codes 

# gray stripes in table: black!10
# usage: kable_styling(latex_options = 'striped', stripe_color = 'black!10')


# +Gradients ----


# Shapes ----
solid_geom = c(15, 17, 18)     # square, tirangle, diamond
clear_geom = c(0, 2, 5)        # square, triangle, diamond
semi_solid_geom = c(7, 9, 12)  # square, triangle, square


# Symbols ----

symbols = c('\u00b2')  # squared



