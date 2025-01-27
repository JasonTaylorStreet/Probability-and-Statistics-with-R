#Task 1 Boxplot and Violin
# 1A: Load "chickwts" dataset
library(package="tidyverse")
library(package="ggplot2")
data("chickwts")
# 1B. Create boxplot using ggplot
chickwts%>%
  ggplot(aes(x=weight, y=feed))+
  geom_boxplot(aes(fill=feed))+
  theme_minimal()+
  scale_fill_brewer(palette="Set2", guide=FALSE)
# 1C. Turn into a violin plot
chickwts%>%
  ggplot(aes(x=weight, y=feed))+
  geom_violin(aes(fill=feed))+
  theme_minimal()+
  scale_fill_brewer(palette="Set2", guide=FALSE)
# 1D. Description: Outliers appear to exist for the sunflower category.
#     Whiskers extend no further from box than 1.5 time the IQR.
#     Outliers are more than 1.5 times the IQR past the box and as is
#     illustrated on the boxplot, from task 1B,there exists data points
#     past the whiskers.


#Task 2 Grouped and Stacked Bar Graph
# 2A. Bring in BarChart.csv, ensure both State/Team are factors
state.team<-read.csv(file= "D:/Documents/Previous Classes/BUAD512A_class_files/Data/BarChart.csv",
                   stringsAsFactors = TRUE)
# 2B. Make a total sales variable using the dataset that groups by state and
#     team and summarizes based on the sum of ComputersSold.
total_sales<-state.team%>%
  group_by(State, Team)%>%
  summarise(ComputersSold=sum(ComputersSold))
# 2C. Use ggplot create Grouped Bar for total sales
ggplot(total_sales, aes(factor(State), ComputersSold, fill=factor(Team)))+
  geom_bar(stat="identity", position="dodge")+
  ggtitle("Total Sales by Team and State")+
  theme_minimal()+
  scale_fill_brewer(palette="Set2")
# 2D. Description: Team A for each state sold more, with the Virginia Team A
#                  selling the most.
# 2E. Turn into Stacked Bar
#     Description: Virginia had the most total sales
ggplot(total_sales, aes(factor(State), ComputersSold, fill=factor(Team)))+
  geom_bar(stat="identity")+
  theme_minimal()+
  scale_fill_brewer(palette="Set2")

#Task 3 Density Plot
# 3A. Bring in fredgraph.csv
yield.inflate<-read.csv(file= "D:/Documents/Previous Classes/BUAD512A_class_files/Data/fredgraph.csv")
# 3B. Use ggplot for two different density plots
# Plot 1 (T10YIE_CHG)
# Description: With a density of just over 1.00, the most probable rate
#              will be approximately 0.25.
#              The majority of rates will occur between -1 and 1.
yield.inflate%>%
  ggplot(aes(x=T10YIE_CHG))+
  geom_density(color="orange", lwd=1, fill="orange", alpha=.6)+
  theme_minimal()
# Plot 2 (DGS10_CHG)
# Description: The highest probability density is just under 0.6 and
#              there is a higher probability for a rate of more than -1 or 1.
yield.inflate%>%
  ggplot(aes(x=DGS10_CHG))+
  geom_density(color="darkgreen", lwd=1, fill="darkgreen", alpha=.4)+
  theme_minimal()
# 3C. Use ggplot scatterplot both continuous variables in the dataset.
# Description: There is a positive trend between the 2 continuous variables.
yield.inflate%>%
  ggplot(aes(x=T10YIE_CHG, y=DGS10_CHG))+
  geom_point()+
  stat_smooth(method="lm")
