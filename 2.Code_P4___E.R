#Required Packages
library(readxl)
library(ggplot2)
library(timeSeries)
library(fPortfolio)

#Data
Walmart <- read_excel("~/Proyecto_4_Portafolio_Eficiente_2___English/4.Walmart_Data_P4___E.xlsx")
Walmart$Date <- as.Date(Walmart$Date) #Transforming the "Date" column of the "Walmart" data frame to data type "Date" 

Costco <- read_excel("~/Proyecto_4_Portafolio_Eficiente_2___English/5.Costco_Data_P4___E.xlsx")
Costco$Date <- as.Date(Costco$Date) #Transforming the "Date" column of the "Costco" data frame to data type "Date"

US10Y<- read_excel("~/Proyecto_4_Portafolio_Eficiente_2___English/6.US10Y_Data_P4___E.xlsx")
US10Y$Date <- as.Date(US10Y$Date) #Transforming the "Date" column of the "US10Y" data frame to data type "Date" 





#1 WALMART AND COSTCO DAILY PRICE CHART

#Walmart Daily Price Chart
ggplot(Walmart, aes(x = Date, y = Adjusted_Closing_Price_WMT_USD)) + geom_bar(stat = "identity", fill = "blue") + labs(x = "Date", y = "Price (USD)", title = "Walmart Daily Price Chart (From 01/01/2019 to 12/31/2023)")

#Costco Daily Price Chart
ggplot(Costco, aes(x = Date, y = Adjusted_Closing_Price_COST_USD)) + geom_bar(stat = "identity", fill = "red") + labs(x = "Date", y = "Price (USD)", title = "Costco Daily Price Chart (From 01/01/2019 to 12/31/2023)")






#2 CALCULATION AND CHART OF DAILY LOGARITHMIC RETURNS FOR WALMART AND COSTCO

#Columns of the "Walmart_Logarithmic_Returns" Data Frame
Date = Walmart$Date
Logaritmic_Returns_WMT_Percentege = c(NA, diff(log(Walmart$Adjusted_Closing_Price_WMT_USD)) * 100)

##Columns of the "Costco_Logarithmic_Returns" Data Frame
Date = Costco$Date
Logaritmic_Returns_COST_Percentege = c(NA, diff(log(Costco$Adjusted_Closing_Price_COST_USD)) * 100)

#"Walmart_Logarithmic_Returns" Data Frame
Logaritmic_Returns_Walmart <- na.omit(data.frame(Date, Logaritmic_Returns_WMT_Percentege))

#"Costco_Logarithmic_Returns" Data Frame
Logaritmic_Returns_Costco <- na.omit(data.frame(Date, Logaritmic_Returns_COST_Percentege))

#"Walmart_Logarithmic_Returns" Chart
ggplot(Logaritmic_Returns_Walmart, aes(x = Date, y = Logaritmic_Returns_WMT_Percentege)) + geom_bar(stat = "identity", fill = "blue") + labs(x = "Date", y = "Logaritmic Returns (%)", title = "Walmart Logarithmic Returns Chart (From 01/01/2019 to 12/31/2023)")

#"Costco_Logarithmic_Returns" Chart
ggplot(Logaritmic_Returns_Costco, aes(x = Date, y = Logaritmic_Returns_COST_Percentege)) + geom_bar(stat = "identity", fill = "red") + labs(x = "Date", y = "Logaritmic Returns (%)", title = "Costco Logarithmic Returns Chart (From 01/01/2019 to 12/31/2023)")





#3 CALCULATION AND CHART OF THE EFFICIENT FRONTIER FOR 16 PORTFOLIOS

#Walmart's Average Logarithmic Returns
μ1 <- mean(Logaritmic_Returns_Walmart$Logaritmic_Returns_WMT_Percentege)

#Costco's Average Logarithmic Returns
μ2 <- mean(Logaritmic_Returns_Costco$Logaritmic_Returns_COST_Percentege)

#Walmart's Standard Deviation Logarithmic Returns
σ1 <- sd(Logaritmic_Returns_Walmart$Logaritmic_Returns_WMT_Percentege)

#Costco's Standard Deviation Logarithmic Returns
σ2 <- sd(Logaritmic_Returns_Costco$Logaritmic_Returns_COST_Percentege)

#Walmart's and Costco's Correlation Logarithmic Returns
ρ <- cor(Logaritmic_Returns_Walmart$Logaritmic_Returns_WMT_Percentege, Logaritmic_Returns_Costco$Logaritmic_Returns_COST_Percentege)

#Columns of the "Portfolios" Data Frame
Num_Portfolio = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)
W_WMT = c(2.0, 1.8, 1.6, 1.4, 1.2, 1.0, 0.8, 0.6, 0.4, 0.2, 0.0, -0.2, -0.4, -0.6, -0.8, -1.0) #Walmart_Weights
W_COST = c(-1.0, -0.8, -0.6, -0.4, -0.2, 0.0, 0.2, 0.4, 0.6, 0.8, 1.0, 1.2, 1.4, 1.6, 1.8, 2.0) #Costco_Weights
μp  =  ((μ1*W_WMT) + (μ2*W_COST))
σp = sqrt(((W_WMT^2)*(σ1^2))+((W_COST^2)*(σ2^2))+(2*ρ*W_WMT*W_COST*σ1*σ2))

#"Portfolios" Data Frame
Portfolios <- data.frame(Num_Portfolio,W_WMT,W_COST,μp,σp)

#Efficient Frontier Chart
ggplot(Portfolios, aes(x = σp, y = μp)) + geom_point() + labs(x = "Risk-σp (%)", y = "Return-μp (%)", title = "Efficient Frontier Chart For 16 Portfolios")





#4 CALCULATION AND CHART OF THE MINIMUM VARIANCE PORTFOLIO (MVP)

#"Returns" Data Frame
Returns <- na.omit(data.frame(Date,Logaritmic_Returns_WMT_Percentege,Logaritmic_Returns_COST_Percentege))
Returns_TS <- as.timeSeries(Returns)#Returns in time series
                                           
#Risk Free Rate
Risk_Free_Rate_Percentaje<- mean(US10Y$Adjusted_Closing_Price_US10Y_USD)

#MVP
MVP <- minvariancePortfolio(Returns_TS, `setRiskFreeRate<-`(portfolioSpec(),2.272104))

#MVP Weights
W_MVP <- round(getWeights(MVP),1)
print(W_MVP)
#The minimum variance portfolio (MVP) coincides with portfolio number 8 of the 16 portfolios in the data frame "Portfolios"

#MVP Chart
Eficient_Frontier <- portfolioFrontier(Returns_TS)
plot(Eficient_Frontier, c(1))
plot(Eficient_Frontier, c(2)) #MVP