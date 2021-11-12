rm(list = ls())
library(tidyverse)
# note: the following code requires the zoo package
# use install.packages('zoo') if you don't have it
source("./baranyi_functions.r")

# the data have already been cut out from
# the excel spreadsheet, open this .csv to see how it should look
OD_path = "25oct16_OD.csv"

# read in the tecan data
OD = read.table(OD_path, sep = ",", as.is = TRUE, row.names = 1)
# transpose it so wells are columns, then convert to dataframe
OD = t(OD)
OD = data.frame(OD)
names(OD)[1:3] = c("cycle","seconds","temp")
# convert to long format by "gathering"
OD = OD %>%
  gather(well, OD, -cycle, -seconds, -temp) %>%
  mutate(hour = seconds / 60 / 60)
# plot with ggplot
OD %>%
  ggplot(aes(x = hour, y = OD, group = well))+
  geom_line()

# group data by well, do per-well fits, and save the summarized
# data as "growth_rate"
#
# there will be warnings, because we're trying to fit even to
# the control wells. 
n_tries = 5 # how many times to try fitting the curve, if it fails
growth_rate = OD %>%
  group_by(well) %>%
  summarize(model_fit = fit_baranyi(hour, log(OD), n_tries),
            fit_variable = c("growth_rate", "lag", "ymax", "y0")) %>%
  ungroup() %>%
  pivot_wider(names_from = fit_variable, values_from = model_fit)

#take a look
head(growth_rate)

# note that the ymax and y0 are in log(OD), important for using 
# baranyi function

# plot just the growth rates 
growth_rate %>%
  ggplot(aes(x = well, y = growth_rate))+
  geom_point()+
  labs(x = "well", y = "growth rate (/hr)")


# lets add the predicted data to the dataframe
# OD_pred is the predicted line based on the curve fit
all_data = left_join(OD, growth_rate) %>%
  group_by(well) %>%
  mutate(OD_pred = baranyi(hour, growth_rate[1], lag[1],ymax[1], y0[1])) %>%
  ungroup

# here I'm arbitrarily choosing 10 wells to plot,
# you'll probably want a different analysis

all_data %>%
  filter(well %in% sample(unique(well), 10)) %>%
  ggplot(aes(x = hour, y = OD_pred, group = well))+
  geom_line(color = "blue")+
  geom_line(aes(y = OD))+
  facet_wrap(~well)

#or the whole plate
all_data %>%
  mutate(well = factor(well, levels = str_sort(unique(well), numeric = TRUE))) %>%
  ggplot(aes(x = hour, y = OD_pred, group = well))+
  geom_line(color = "blue")+
  geom_line(aes(y = OD))+
  facet_wrap(~well, nrow = 8)+
  theme_bw(5)


# all the data is in all_data, or the growth rates are in growth_rate