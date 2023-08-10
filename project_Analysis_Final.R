# Questions to be answered

# 1. Variations in Sale Amounts across Towns
# 2. What is common to clusters by property type and residential type?
# 3. Is there a correlation between the assessed value of properties and their geographical location? Can we identify areas with higher or lower property values?
# 4. Are there any relationships between the assessed value or sale amount and specific property features like the size of the property or the number of bedrooms/bathrooms?
# 5. What is the trend between assessed value and sale value over the year? 
# 6. What is the easiest selling residential types
# 7. Which town has the quickest sales and which property type?
# 8. Change in trends across residential types over the years in regards to sales and their profitability 
# 9. Correlation of prices of units to their location and properties before and after the Recession


# Description of dataset:
#realestate_Analysis_raw: the raw data of the real estate data
#realestate_Analysis: cleaned data of the real estate data
#realestate_Analysis_glm: after applied linear regression model to cleaned data
#realestate_Analysis_agg_complete: aggregate by property type and residential type
#realestate_Analysis_agg: after its aggregated ungroup to select significant variables
#realestate_Analysis_cluster: Conduct hierarchical cluster analysis
#realestate_Analysis_cluster_ward.D2: applied word.D2 for clustering
#realestate_Analysis_3clusts: define cluster size of 3
#realestate_Analysis_agg_bind: binding the aggregated data
#partition_Dataset: use partition method for seperate training and testing sample 
#partition_Dataset_train: training dataset
#partition_Dataset_test: testing dataset
#reg_realestate_Analysis_2: applied linear regression model of lm(Assessed.Value ~ Residential.Type)
#reg_realestate_Analysis <- applied linear regression model of lm(Assessed.Value ~ Town)
#realestate_Analysis_compact:compute sale_time, the time it took to sell the property
# xts_obj: convert data frame to xts object
#xts_obj_weekly: Compute weekly Open-High-Low-Close data (or OHLC) for sale_time
#xts_obj_monthly: Compute monthly Open-High-Low-Close data (or OHLC) for sale_time
#xts_obj_quarterly: Compute quarterly Open-High-Low-Close data (or OHLC) for sale_time
#xts_obj_yearly: Compute yearly Open-High-Low-Close data (or OHLC) for sale_time
#realestate_Analysis_compact_2: aggregate the data and use time series for the analysis
#xts_obj_2: convert data frame to xts object
#xts_obj_2_weekly: Compute weekly Open-High-Low-Close data (or OHLC) for mean sales ratio values
#xts_obj_2_monthly: Compute monthly Open-High-Low-Close data (or OHLC) for mean sales ratio values
#xts_obj_2_quarterly: Compute quarterly Open-High-Low-Close data (or OHLC) for sale_time
#xts_obj_2_yearly: Compute yearly Open-High-Low-Close data (or OHLC) for mean sales ratio values
#reaq2: prices of units to their location and properties before the Recession
#model2: applied glm model to find corrleation prices of units to their location and properties before the Recession
#reaq1: prices of units to their location and properties after the Recession
#model1: applied glm model to find corrleation prices of units to their location and properties after the Recession

# The Office of Policy and Management maintains a listing of all real estate sales with a sales price of $2,000 or greater that occur between October 1 and September 30 of each year. For each sale record, the file includes: town, property address, date of sale, property type (residential, apartment, commercial, industrial or vacant land), sales price, and property assessment.

# Note: Here, we load in required libraries. Other libraries may also be loaded as we go on to highlight what libraries was required for some specific sections
library(ggplot2) # for visualization
library(lubridate) # for working with dates
library(magrittr) # for piping of commands
library(dplyr) # for data manipulation
library(ggthemes) # for theme_solarized
library(dendextend) # for visualization
library(factoextra) # for visualization
library(gridExtra) # for visualization
library(caret) # for regression and classification training
library(broom) # for transform to tidy dataframe 
library(viridis) # for color maps
library(xts) # for time series
library(quantmod) # for modeling 


# Read in data 
realestate_Analysis_raw <- read.csv('Documents/Real_Estate_Sales_2001-2020_GL.csv')


# We want to familiarize with the data in terms of:
# checking for missing (NA) values
# checking for white spaces
# checking for column names
# checking the structure of the dataset

# Explore the raw data
sum(is.na(realestate_Analysis_raw$Date.Recorded)) # check NA values
sum(grepl("^\\s|\\s$", realestate_Analysis_raw$Date.Recorded)) # check white spaces
realestate_Analysis_raw$Date.Recorded
realestate_Analysis_raw[1:10,]
colnames(realestate_Analysis_raw)
colnames(realestate_Analysis_raw)[2]
str(realestate_Analysis_raw)
unique(realestate_Analysis_raw$Date.Recorded)
head(realestate_Analysis_raw)
sum(realestate_Analysis_raw$Property.Type == "") # check sum of counts of missing values
sum(realestate_Analysis_raw$Residential.Type == "" & realestate_Analysis_raw$Property.Type != "") # check for two columns both having missing values
realestate_Analysis_raw%>%
  filter(Property.Type == "") # extract missing values


# Data cleaning pipeline

# Clean the data (remove "***Unknown***" values in "Town" column, coerce "Date.Recorded" to Date type, remove rows where "Property.Type" & "Residential.Type" are "", extract year out of Date.Recorded, append January 1st to List.Year to answer question 8)
realestate_Analysis <- realestate_Analysis_raw %>%
  filter(Town != "***Unknown***", Sale.Amount != 0, Property.Type != "", Residential.Type != "") %>%
  mutate(Date.Recorded = as.Date(Date.Recorded, format = "%m/%d/%Y"),
         Year.Recorded = as.integer(format(Date.Recorded, "%Y")), 
         Assumed_List.Date = as.Date(paste(List.Year, "01-01", sep = "-")), 
         Town = as.factor(Town))


# Note: While creating the pipeline, we discussed that a filter of NAs did not reveal missing values as missing values could be any or all of "-", NA, etc. However, we found out that there were "***Unknown***" values and filtered them out.

# Note: The date was in text/character format. This would not be useful for data transformation involving time/date.


# Explore the clean data
sum(is.na(realestate_Analysis))
nrow(realestate_Analysis)
nrow(na.omit(realestate_Analysis))
realestate_Analysis$Date.Recorded
realestate_Analysis[1:10, ]
colnames(realestate_Analysis)
str(realestate_Analysis)
class(realestate_Analysis$Date.Recorded)
unique(realestate_Analysis$Town)
head(realestate_Analysis)

# Use the data to answer questions

# 1. What is the correlation of prices of units to their location and access? 

# Note: We attempted to use the cor() function but this did not work because we could not meaningfully coerce location to numeric values.

# Note: When we attempted to visualize the data as-is, the plot was muddled up because of its size. So we had to introduce a filter of sale amounts less than $400,000

# We will use linear regression for this to identify significant variables in the dataset.

realestate_Analysis_glm <- glm(formula = Sale.Amount ~ Town + Residential.Type + Property.Type,  family = "gaussian", data = realestate_Analysis)
summary(realestate_Analysis_glm)


# Note: Clustering appeared to be a viable option here since we were looking for similarities between property type and residential type by clusters.

# 2. What is common to clusters by property type and residential type?

# We aggregate by property type and residential type

realestate_Analysis_agg_complete <- realestate_Analysis %>%
  group_by(Property.Type, Residential.Type) %>%
  summarise(mean_Price = mean(Sale.Amount), median_Price = median(Sale.Amount), min_Price = min(Sale.Amount), max_Price = max(Sale.Amount))

realestate_Analysis_agg <- realestate_Analysis %>%
  group_by(Property.Type, Residential.Type) %>%
  summarise(mean_Price = mean(Sale.Amount), median_Price = median(Sale.Amount), min_Price = min(Sale.Amount), max_Price = max(Sale.Amount)) %>%
  ungroup() %>%
  select(-c(Property.Type, Residential.Type, median_Price, min_Price, max_Price))

# Conduct hierarchical cluster analysis
realestate_Analysis_cluster <- dist(x = realestate_Analysis_agg, method = 'euclidean') 
realestate_Analysis_cluster_ward.D2 <- hclust(d = realestate_Analysis_cluster, method='ward.D2')
plot(realestate_Analysis_cluster_ward.D2)

# Note: We computed Cophenetic correlation coefficient to check the fit of the hierachical clustering and it appeared to be a good fit at 0.7239674.

cor(cophenetic(realestate_Analysis_cluster_ward.D2), realestate_Analysis_cluster)#0.7239674


# Note: The plots below were used to visualize the clusters for better analysis and on observationn, we arrived at a cluster of size 3.

plot(color_branches(as.dendrogram(realestate_Analysis_cluster_ward.D2), k=3))

plot(realestate_Analysis_cluster_ward.D2)
rect.hclust(tree=realestate_Analysis_cluster_ward.D2, k = 3, border='tomato')

plot(color_branches(as.dendrogram(realestate_Analysis_cluster_ward.D2), k = 3, groupLabels = F))

realestate_Analysis_3clusts = cutree(tree = realestate_Analysis_cluster_ward.D2, k=3)
table(realestate_Analysis_3clusts)

grid.arrange(fviz_dend(x = realestate_Analysis_cluster_ward.D2, k=2),
             fviz_dend(x = realestate_Analysis_cluster_ward.D2, k=3),
             fviz_dend(x = realestate_Analysis_cluster_ward.D2, k=4))

realestate_Analysis_agg_bind <- cbind(realestate_Analysis_agg_complete, realestate_Analysis_3clusts) %>%
  arrange(realestate_Analysis_3clusts)

colnames(realestate_Analysis_agg_bind)[7] <- "Cluster Name"
realestate_Analysis_agg_bind


# 3. Is there a correlation between the assessed value of properties and their geographical location? Can we identify areas with higher or lower property values?

# We will use linear regression for the correlation part. We will fit a linear regression model

# Set seed for reproducibility
set.seed(1089)

# Partition to get 80% into training dataset
partition_Dataset <- createDataPartition(realestate_Analysis$Assessed.Value, groups = 10, p = 0.8, list = FALSE, times = 1)

partition_Dataset_train <- realestate_Analysis[partition_Dataset, ]
partition_Dataset_test <- realestate_Analysis[-partition_Dataset, ]

# Check the length of total, training and testing datasets
nrow(partition_Dataset_train) #487037
nrow(partition_Dataset_test) #121754
nrow(realestate_Analysis) #608791

# Fit a linear regression model
reg_realestate_Analysis <- lm(Assessed.Value ~ Town, data = partition_Dataset_train)
glance(reg_realestate_Analysis) # the r squared is very low
summary(reg_realestate_Analysis)
head(realestate_Analysis)

p_values <- summary(reg_realestate_Analysis)$coefficients[,4]

# Get the names of the variables with p-values less than 0.05
significant_vars <- names(p_values[p_values < 0.05])

# Print the list of significant variables
print(significant_vars)

# Note: Because of the huge size of the dataset, sale amount has been expressed in millions and filtered to exclude values lower than $32 million

realestate_Analysis %>%
  group_by(Town) %>%
  summarise(minimum_Amount = min(Sale.Amount)/1000000, maximum_Amount = max(Sale.Amount)/1000000) %>%
  ungroup() %>%
  filter(maximum_Amount >= 32) %>%
  arrange(desc(maximum_Amount)) %>%
  ggplot(aes(x = reorder(Town, -maximum_Amount), maximum_Amount, fill = Town)) +
  geom_col() +
  scale_fill_manual(values = c("darkorange1", "orange3", "rosybrown3", "black", "purple", "seagreen4", "indianred4", "dodgerblue2")) +
  scale_y_continuous (limits = c(0, 350), breaks = seq(0, 350, by = 50)) +
  theme_wsj() +
  theme(plot.title = element_text(size=17), legend.key.size = unit(0.3, 'cm'), legend.title = element_text(size=15), legend.title.align = 0.1, legend.position = "none") +
  theme(axis.text.x = element_text(angle = 15, hjust = 1)) +
  labs(title = "Distribution of Top Towns Relative to Maximum Sale Amount", x = "Town", y = "Maximum Sale Amount (in millions)")


# 4. Are there any relationships between the assessed value or sale amount and specific property features like the size of the property or the number of bedrooms/bathrooms?

# We will also use linear regression for this question.

reg_realestate_Analysis_2 <- lm(Assessed.Value ~ Residential.Type, data = partition_Dataset_train)

# Check the model-level values
glance(reg_realestate_Analysis_2) # the r squared is pretty much 0
summary(reg_realestate_Analysis_2)


p_values_2 <- summary(reg_realestate_Analysis_2)$coefficients[,4]

# Get the names of the variables with p-values less than 0.05
significant_vars_2 <- names(p_values_2[p_values_2 < 0.05])

# Print the list of significant variables
print(significant_vars_2)


# Note: The closest variable to property size is Residential Type
# Note: To reduce the impact of outliers, median is used as center of tendency

# library(RColorBrewer)
realestate_Analysis %>%
  group_by(Residential.Type) %>%
  summarise(median_Assessed_value = median(Assessed.Value)/1000) %>%
  ungroup() %>%
  ggplot(aes(x = reorder(Residential.Type, -median_Assessed_value), median_Assessed_value, fill = factor(Residential.Type))) +
  geom_col() +
  scale_fill_manual(values = c("coral3", "cornflowerblue", "aquamarine4", "burlywood4", "darkgoldenrod2")) +
  theme_solarized_2() +
  theme(plot.title = element_text(size=17), legend.key.size = unit(0.3, 'cm'), legend.title = element_text(size=15), legend.title.align = 0.1, legend.position = "none") +
  labs(title = "Relationship between Median of Assessed Value and Residential Type", x = "Residential Type", y = "Median Assessed Value (in thousands)", fill = "Residential Type")


# 5. What is the trend between assessed value and sale value over the years? What makes the sales ratio higher than 1, say when and where?

# Note: To reduce the impact of outliers, median is used as center of tendency.
# Note: Mean value didn't work well here because of the impact of variation on the value so median was used as it is immune to extreme values relatively speaking
realestate_Analysis%>%
  group_by(Year.Recorded, Residential.Type) %>%
  summarise(median_Sales_Ratio = median(Sales.Ratio)) %>%
  ungroup() %>%
  arrange((desc(median_Sales_Ratio))) %>%
  ggplot(aes(x = Year.Recorded, y = median_Sales_Ratio)) +
  scale_x_continuous(breaks = unique(realestate_Analysis$Year.Recorded)) +
  scale_y_continuous (limits = c(0, 1.60), breaks = seq(0, 1.60, by = 0.2)) +
  geom_line(aes(linetype = as.factor(Residential.Type), color = as.factor(Residential.Type)), linewidth = 1.0) +
  scale_color_manual(values = c("cyan3", "green3", "rosybrown", "red", "purple")) +
  theme_solarized_2() +
  theme(axis.text.x = element_text(angle = 15, hjust = 1)) +
  theme(plot.title = element_text(size=17), legend.key.size = unit(1.7, 'cm'), legend.title = element_text(size=15), legend.title.align = 0.1, legend.position = "bottom") +
  labs(title = "Yearly Trend of Median Sales Ratio by Residential Type", x = "Year of Sale", y = "Median Sales Ratio", color = "Residential Type", linetype = "Residential Type")


# 6. Which residential type is the easiest one to sell?

# Note: The List.Year column does not state the date it was Listed while Date.Recorded which is the day sale was made is a proper date. Consequently, this ease of sale is being inferred from the count of units sold

realestate_Analysis%>%
  group_by(Residential.Type) %>%
  summarise(total = n()) %>%
  ungroup() %>%
  mutate(proportion_Sold = round(total/sum(total), 3)) %>%
  arrange(desc(proportion_Sold)) %>%
  ggplot(aes(x = Residential.Type, y = proportion_Sold, fill = as.factor(Residential.Type))) +
  geom_col() +
  scale_fill_manual(values = c("coral3", "cornflowerblue", "aquamarine4", "burlywood3", "black")) +
  scale_y_continuous(limits = c(0, 0.75), breaks = seq(0, 0.75, by = 0.1)) +
  theme_solarized_2() +
  theme(plot.title = element_text(size=17), legend.key.size = unit(0.2, 'cm'), legend.title = element_text(size=15), legend.title.align = 0.1, legend.position = "none") +
  labs(title = "Proportion of Houses Sold per Residential Type", x = "Residential Type", y = "Proportion Sold", fill = "Residential Type")


# 7. Assuming Listing was made in the first month of the year, which town has the quickest sales and which property type?

# We will aggregate the data and use time series for the analysis

# We compute sale_time, the time it took to sell the property
realestate_Analysis_compact <- realestate_Analysis%>%
  mutate(sale_time = as.numeric(difftime(Date.Recorded, Assumed_List.Date, units = "days"))/(365.25/12)) %>%
  filter(sale_time >= 0) %>%
  arrange(sale_time) %>%
  select(Date.Recorded, Year.Recorded, Sale.Amount, sale_time, Town, Assumed_List.Date)

head(realestate_Analysis)
head(realestate_Analysis_compact)

# Convert data frame to xts object
xts_obj <- xts(realestate_Analysis_compact[,4], order.by=as.Date(realestate_Analysis_compact$sale_time))

# Compute weekly Open-High-Low-Close data (or OHLC) for sale_time
xts_obj_weekly <- to.period(xts_obj, period = "weeks", OHLC = TRUE, name = "Weekly_Sale_Time")

# Compute monthly Open-High-Low-Close data (or OHLC) for sale_time
xts_obj_monthly <- to.period(xts_obj, period = "months", OHLC = TRUE, name = "Monthly_Sale_Time")

# Compute quarterly Open-High-Low-Close data (or OHLC) for sale_time
xts_obj_quarterly <- to.period(xts_obj, period = "quarters", OHLC = TRUE, name = "Quarterly_Sale_Time")

# Compute yearly Open-High-Low-Close data (or OHLC) for sale_time
xts_obj_yearly <- to.period(xts_obj, period = "years", OHLC = TRUE, name = "Yearly_Sale_Time")

# 8. Change in trends across top 5 towns over the years in regards to sales and their profitability

# We will also aggregate the data and use time series for the analysis
realestate_Analysis_compact_2 <- realestate_Analysis%>%
  group_by(Date.Recorded, Town) %>%
  summarise(mean_Sales_Ratio = mean(Sales.Ratio)) %>%
  ungroup() %>%
  arrange((desc(mean_Sales_Ratio)))

#convert data frame to xts object
xts_obj_2 <- xts(realestate_Analysis_compact_2[,3], order.by=as.Date(realestate_Analysis_compact_2$Date.Recorded))

length((xts_obj_2))

# Compute weekly Open-High-Low-Close data (or OHLC) for mean sales ratio values
xts_obj_2_weekly <- to.period(xts_obj_2, period = "weeks", OHLC = TRUE, name  = "Weekly_Sales_Ratio")

# Compute monthly Open-High-Low-Close data (or OHLC) for mean sales ratio values
xts_obj_2_monthly <- to.period(xts_obj_2, period = "months", OHLC = TRUE, name  = "Monthly_Sales_Ratio")

# Compute quarterly Open-High-Low-Close data (or OHLC) for sale_time
xts_obj_2_quarterly <- to.period(xts_obj_2, period = "quarters", OHLC = TRUE, name = "Quarterly_Sales_Ratio")

# Compute yearly Open-High-Low-Close data (or OHLC) for mean sales ratio values
xts_obj_2_yearly <- to.period(xts_obj_2, period = "years", OHLC = TRUE, name  = "Yearly_Sales_Ratio")


#9. Correlation of prices of units to their location and properties before and after the Recession
#before 2011
reaq2<- realestate_Analysis %>%
  filter(List.Year < 2011) %>%
  select(Serial.Number, List.Year, Town, Assessed.Value, Sale.Amount, Property.Type, Residential.Type)

model2 = glm(Sale.Amount~Town+Residential.Type+Property.Type,data=reaq2,family='gaussian')
summary(model2)

p_values2 <- summary(model2)$coefficients[,4]

# Get the names of the variables with p-values less than 0.05
significant_vars2 <- names(p_values2[p_values2 < 0.05])

# Print the list of significant variables
print(significant_vars2)

#post 2011
reaq1<- realestate_Analysis %>%
  filter(List.Year > 2011) %>%
  select(Serial.Number, List.Year, Town, Assessed.Value, Sale.Amount, Property.Type, Residential.Type)

model1 = glm(Sale.Amount~Town+Residential.Type+Property.Type,data=reaq1,family='gaussian')
summary(model1)

p_values1 <- summary(model1)$coefficients[,4]

# Get the names of the variables with p-values less than 0.05
significant_vars1 <- names(p_values1[p_values1 < 0.05])

# Print the list of significant variables
print(significant_vars1)