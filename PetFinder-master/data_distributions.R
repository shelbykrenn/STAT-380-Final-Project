


# Packges used ------------------------------------------------------------
library(readr)
library(tidyverse)

# Read/Process Data -------------------------------------------------------
train <- read_csv("Data/train.csv")

# read labels/lookup tables
breed_labels <- read.csv("Data/breed_labels.csv")
color_labels <- read.csv("Data/color_labels.csv")
state_labels <- read.csv('Data/state_labels.csv')

plot_data <- train %>%
  left_join(color_labels, by = c("Color1" = "ColorID")) %>% 
  select(everything(), ColorName1 = ColorName) %>% 
  left_join(color_labels, by = c("Color2" = "ColorID")) %>% 
  select(everything(), ColorName2 = ColorName) %>%
  left_join(color_labels, by = c("Color3" = "ColorID")) %>% 
  select(everything(), ColorName3 = ColorName) %>% 
  left_join(breed_labels, by = c("Breed1" = "Breed"))

# Find types of all training data columns
types <- sapply(train, typeof)

# Remove non-numeric columns
numeric_data <- train[,(types == 'integer') | (types ==  'double')]
numeric_names <- names(types[(types ==  'integer') | (types == 'double')])

# Create plots ------------------------------------------------------------

# For each column in the numeric data, create a plot, or grob, output is
# a list of grobs
histo_list <- map(numeric_names, ~{

  ggplot(numeric_data, aes_string(x=.x)) + 
    geom_histogram(aes(y=..density..), colour="black", fill="white") +
    geom_density(alpha=.2, fill="#FF6666") +
    ggtitle(paste('Histogram for:', .x))
  
})

# Arrange the grobs
ml <- marrangeGrob(grobs = histo_list, nrow=2, ncol=2)

# Save arranged grobs to pdf -----------------------------------------------
ggsave("pet_finder_hists.pdf", ml)






