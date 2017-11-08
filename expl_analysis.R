#require libraries
require(feather)
require(tidyverse)
require(plotly)

df <- read_feather("C:/Users/Radim/Documents/ph_ads_payment_indicator.feather")

#
str(df)
summary(df)

#how many ads were promoted
dim(df)[1]-sum(df$was_promoted=='f')

#price distribution, xlab capped at 60k
price_plot <- ggplot(df) + geom_density(aes(x = round(df$price,3))) + scale_x_continuous(limits = c(0, 60000)) 

#peaks correspond to 30K, 35K, 40K..
ggplotly(price_plot)




