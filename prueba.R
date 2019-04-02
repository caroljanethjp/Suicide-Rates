# Load tidyverse library
library(tidyverse)

# Load native iris dataset
df <- iris

# Check names of columns to find out which column you want to subset on
names(df)

# Since we want to "group by" the species type we can either group all other variables we want in a list, or just exclude the species column if we want the remaining variables
new_df <- df %>% nest(-Species)

# Check that the new dataframe is what we expected
new_df
