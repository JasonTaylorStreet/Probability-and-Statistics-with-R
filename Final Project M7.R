# Overall Goal
#   Your goal is to examine 6 variables, including potential subgroups based on
#   location or date, and 4 relationships between 2 variables (visually and
#   through an appropriate hypothesis test). In preparing the data, you can
#   create subsets of the data set to make it easier to work with based on
#   what you need. Read all the instructions below before making decisions on
#   how to slice down the data set. Take note that some of the variables are
#   measuring a similar thing, so be sure to choose unique variables to have the
#   most accurate results.

################################################################################
# Task 1
# Load Realtor data for state
library(package="tidyverse")
full.state<- read.csv(file = "C:/Users/jasst/OneDrive/Desktop/William & Mary/Prob and Stats with R/data/RDC_Inventory_Core_Metrics_State_History.csv",
                      stringsAsFactors = TRUE)
# select variables of interest
state.cleaned<- full.state %>%
  select(state_id, month_date_yyyymm,  median_listing_price,
         active_listing_count, pending_listing_count, price_reduced_count, median_days_on_market,
         median_square_feet)
# Maintain continuous 48 states plus District of Columbia.
# Remove Alaska, Marshall Islands, and Hawaii.
continuous.state<- subset(state.cleaned, !(state_id %in% c("ak", "mh", "hi")))
#   Code any missing values appropriately and rename variables (1-6) where necessary
#   with explanation in comments of what the new named variable represents.
continuous.state<-continuous.state%>%
  rename(state = state_id)%>%
  rename(date = month_date_yyyymm)
# I removed the Marshall Island as that had only 1 observation. I removed Alaska
# and Hawaii as the locations/access may have impact on trends over time that is
# not consistent with the continuous states plus District of Colombia. After
# verifying that there was no data missing for remaining observations, I
# maintained the names of the variables as these are succinctly descriptive. I
# shorted the state and date for ease.

################################################################################
# Task 2
# Variable 1 - Median Listing Price
variable.1<- continuous.state
variable.1%>%
  summarise(mean = mean(median_listing_price),
            sd = sd(median_listing_price),
            median = median(median_listing_price),
            IQR = IQR(median_listing_price),
            mode = names(x=sort(table(median_listing_price),
                              decreasing = TRUE))[1],
            index.qual.var = qualvar::B(x=table(continuous.state$median_listing_price)))
# Based on the central tendencies, there may potentially be a right skew, and
# there is some spread/variation .

# Variable 2 - Active Listing Count
variable.2<- continuous.state
variable.2%>%
  summarise(mean = mean(active_listing_count),
            sd = sd(active_listing_count),
            median = median(active_listing_count),
            IQR = IQR(active_listing_count),
            mode = names(x=sort(table(active_listing_count),
                                decreasing = TRUE))[1],
            index.qual.var = qualvar::B(x=table(continuous.state$active_listing_count)))
# There might be a few high values/outliers shifting the mean and median higher.
# The standard deviation, IQR, and B indicate much variability/spread, while the
# small mode indicates considerable more frequent lower values. This is most
# likely strong right skewed

# Variable 3 - Pending Listing Count
variable.3<- continuous.state
variable.3%>%
  summarise(mean = mean(pending_listing_count),
            sd = sd(pending_listing_count),
            median = median(pending_listing_count),
            IQR = IQR(pending_listing_count),
            mode = names(x=sort(table(pending_listing_count),
                                decreasing = TRUE))[1],
            index.qual.var = qualvar::B(x=table(continuous.state$pending_listing_count)))
# There is similar trend in the data as with variable 2 where a smaller number
# of high values is skewing and there is more frequent lower values. Therefore
# likely strong right skewed

# Variable 4 - Price Reduction Count
variable.4<- continuous.state
variable.4%>%
  summarise(mean = mean(price_reduced_count),
            sd = sd(price_reduced_count),
            median = median(price_reduced_count),
            IQR = IQR(price_reduced_count),
            mode = names(x=sort(table(price_reduced_count),
                                decreasing = TRUE))[1],
            index.qual.var = qualvar::B(x=table(continuous.state$price_reduced_count)))
# While the value of B is less than variables 2 and 3, it is still moderate for
# spread, and following the same thought process as 2 and 3, a few high values
# is most likely pulling the mean and median and causing a right skew.

# Variable 5 - Median Days on the Market
variable.5<- continuous.state
variable.5%>%
  summarise(mean = mean(median_days_on_market),
            sd = sd(median_days_on_market),
            median = median(median_days_on_market),
            IQR = IQR(median_days_on_market),
            mode = names(x=sort(table(median_days_on_market),
                                decreasing = TRUE))[1],
            index.qual.var = qualvar::B(x=table(continuous.state$median_days_on_market)))
# The central tendencies appear similar, the standard deviation and IQR
# are large, however the B is small (compared to the other variables),
# indicating less spread. This may be normal distributed.

# Variable 6 - Median Square Feet
variable.6<- continuous.state
variable.6%>%
  summarise(mean = mean(median_square_feet),
            sd = sd(median_square_feet),
            median = median(median_square_feet),
            IQR = IQR(median_square_feet),
            mode = names(x=sort(table(median_square_feet),
                                decreasing = TRUE))[1],
            index.qual.var = qualvar::B(x=table(continuous.state$median_square_feet)))
# compared to the previous variables, this variable appears more narrow. while
# mean and median are similar, mode indicates more frequent lower. however based
# on the lower spread/variance the data is probably normal distributed.

# Variable 7 - Date
variable.7<-table(continuous.state$date)
print(variable.7)
prop.table(variable.7)
# The data is equal for each month of 49 entries which is indicative that the
# 48 states plus District of Columbia are accounted for.


################################################################################
# Task 3
# Grouping 1 - group by year for list price
group.state.year<-continuous.state%>%
  group_by(year.first.four = substr(date,1,4))
group.state.year%>%
  summarise(mean = mean(median_listing_price),
            sd = sd(median_listing_price),
            median = median(median_listing_price),
            IQR = IQR(median_listing_price),
            mode = names(x=sort(table(median_listing_price),
                                decreasing = TRUE))[1])
# Each year, the central tendencies indicate a rise in values, however the
# standard deviation and IQR are also increasing

# Grouping 2 - group by the month regardless of the year for list price
group.state.month<-continuous.state%>%
  group_by(month.last.two = substr(date,5,6))
group.state.month%>%
  summarise(mean = mean(median_listing_price),
            sd = sd(median_listing_price),
            median = median(median_listing_price),
            IQR = IQR(median_listing_price),
            mode = names(x=sort(table(median_listing_price),
                                decreasing = TRUE))[1])
# The time of year shows a slight rise in house price towards the spring and
# early summer. Additionally, the standard deviation and IQR are showing more
# spread.

# Grouping 3 - group by days on the market
group.state.market.days<-continuous.state%>%
  mutate(segment.days=cut(x=median_days_on_market,
                          breaks=c(0, 24, 49, 74, 99, 124, 149, 500),
                           labels=c("<25", "25-49", "50-74", "75-99", "100-124",
                                    "125-149", "150+")))%>%
  group_by(segment.days)
group.state.market.days%>%
  summarise(mean = mean(median_listing_price),
            sd = sd(median_listing_price),
            median = median(median_listing_price),
            IQR = IQR(median_listing_price),
            mode = names(x=sort(table(median_listing_price),
                                decreasing = TRUE))[1])
# Due to the continuous nature of this variable, I grouped the days in segments
# of 25 up to 150 where the frequency of repeated days on market were much less.
# There is much variation more variation in houses that sell faster as compared
# to those on the market for longer.

################################################################################
# Task 4
# Graph 1 - Median Price by Year
library(package="ggplot2")
group.state.year%>%
  ggplot(aes(x=year.first.four, y=median_listing_price, fill=year.first.four))+
  geom_boxplot()+
  theme_minimal()+
  scale_color_brewer(palette="Set2", guide =FALSE)+
  labs(x="Year", y="Median Listing Price", fill="Year",
       title="Median listing price across the continuous 48 states and DC by year")+
  scale_y_continuous(labels=scales::dollar_format(prefix="$"))
# There appears to be a general rise in median pricing and spread over the years
# from approximately $250,000 to $375,000. There also appears that as the years
# progress, it becomes less normally distributed with a leaning to right skewed
# to much more right skewed and leptokurtic

# Graph 2 - Median Price by Month
group.state.month%>%
  ggplot(aes(x=month.last.two, y=median_listing_price, fill= month.last.two))+
  geom_boxplot()+
  theme_minimal()+
  scale_color_brewer(palette="Set2", guide =FALSE)+
  labs(x="Year", y="Median Listing Price", fill="Month of the Year",
       title="Median listing price across the continuous 48 states and DC for a given month",
       subtitle = "Between 2016 and 2022")+
  scale_y_continuous(labels=scales::dollar_format(prefix="$"))
# This shows a slight increase $275,000 to $300,000 in the listing price and
# spread towards leptokurtic around the months the May and June as compared to
# January and December. Additionally, this shows that regardless of the time of
# year, there are many outliers lending to the the right skew for each month.

# Graph 3 - Median Square Feet by Year
group.state.year%>%
  ggplot(aes(x=year.first.four, y=median_square_feet, fill=year.first.four))+
  geom_boxplot()+
  theme_minimal()+
  scale_color_brewer(palette="Set2", guide =FALSE)+
  labs(x="Year", y="Median Square Feet", fill="Year",
       title="Median square feet across the continuous 48 states and DC by year")
# Each year, the median of house size does shift and for each year, it appears
# to be normal distributed leaning towards right skew. While there are outliers
# high and low, there appears some changes over the years on the size of the
# houses being sold and with 2022 leaning towards leptokurtic.

# Graph 4 - Median price vs Days on Market
group.state.market.days%>%
  ggplot(aes(x=segment.days, y=median_listing_price, fill=segment.days))+
  geom_boxplot()+
  theme_minimal()+
  scale_color_brewer(palette="Set2", guide =FALSE)+
  labs(x="Days on Market", y="Median Listing Price", fill="Days on Market",
       title="Median listing price across the continuous 48 states and DC by days on market")+
  scale_y_continuous(labels=scales::dollar_format(prefix="$"))
# There appears to be a negative relationship in the median price based on how
# long it is on the market from just under $600,000 for <25 days to
# approximately $250,000 for 75 days or more. The spread become more narrow as
# well. the 25-49 appears to be the most spread along with a right skew and
# leptokurtic for days 25-124.

################################################################################
# Task 5
#   I would predict that there is a negative relationship for price based on the
#   number of days the listing is on the market as well as for the number of
#   houses that are actively being sold. Conversely, I predict a positive
#   relationship for price as the years progress and for the size of a house.


################################################################################
# Task 6
#   Step 1:
#   Hypothesis 1 - days on the market impacts price
#     H0: The slope of the line in regards to days on the market equals zero
#     HA: The slope of the line in regards to days on the market does not equal
#         zero
#   Hypothesis 2 - square feet of a house impacts price
#     H0: The slope of the line in regards to square feet of the house equals
#         zero
#     HA: The slope of the line in regards to square feet of the house does not
#         equal zero
#   Hypothesis 3 - as months/years progress it impacts price
#     H0: The slope of the line in regards to time equals zero
#     HA: The slope of the line in regards to time does not equal zero
#   Hypothesis 4 - the number of active houses impacts price
#     H0: The slope of the line in regards to the number of house available
#         equals zero
#     HA: The slope of the line in regards to the number of house available does
#         not equal zero
#   Steps 2 & 3:
multiple.price<-lm(median_listing_price ~ median_days_on_market +
                         median_square_feet + date + active_listing_count,
                       data=continuous.state)
summary(multiple.price)
#   Steps 4 & 5:
#     Goodness of Fit: with residueal standard error of $106,200, R-squared of
#       0.2288, and adjusted R-squared of 0.228, indicates a weak fit with
#       significant amount of unexplained variance.
#     F-Statistic has a p-value of <.05 indicating that the overall model is
#       statistically significant therefore:
#     Hypothesis 1: The output shows a p-value on the median_days_on_market row
#       of <.05 from a t-value. This was significant and means our predictor
#       variable does influence the price variable and that we can reject the
#       null hypothesis and show support for our alternative hypothesis.
#     Hypothesis 2: The output shows a p-value on the median_square_feet row of
#       <.05 from a t-value. This was significant and means our predictor
#       variable does influence the price variable and that we can reject the
#       null hypothesis and show support for our alternative hypothesis.
#     Hypothesis 3: The output shows a p-value on the date row of 0.00759
#       (p<.05) from a t-value. This was significant and means our predictor
#       variable does influence the price variable and that we can reject the
#       null hypothesis and show support for our alternative hypothesis.
#     Hypothesis 4: The output shows a p-value on the active_listing_count row
#       of <.05 from a t-value. This was significant and means our predictor
#       variable does influence the price variable and that we can reject the
#       null hypothesis and show support for our alternative hypothesis.
#     In regards to predictions from task 5, the results agree with the
#       exception of the number of available houses appears to be a positive
#       correlation.

################################################################################
# Task 7
#   The strongest predictors of price were the days on market and as time
#     progresses forward.
#   In regards to Realty, there are quite a few things that impact the results:
#     Confounding variables/bias such as schools, crimes, politics, etc...
#     Additional outside variable(s) may impact both predictor and explanatory
#     There may be different effects depending on what/how they are combined and
#       therefore leading to more complex relationships
#     Location (amount of rural vs urban)
#     Natural Events (forest fires, devastating storms/floods, etc...)
#     Economic factors along with market speculation and investor behavior
