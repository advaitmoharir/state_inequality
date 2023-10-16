# ----------------------------------------------
# Title: 00_master.R
# Purpose: Master file to install packages and
#          run all codes
# Author: Advait Moharir, Rajendran Narayanan
# Status: In Progress
# ---------------------------------------------

# Load required packages and state root directory

library(pacman)
p_load(tidyverse, dplyr, ggplot2, corrplot, 
       ggfortify, dendextend,clustree, cowplot,
       fixest, purrr, mice, ggpubr, factoextra)  # installs and loads reqd. packages.

# Set location

here::i_am("state_inequality.Rproj")

# Add files to run here


