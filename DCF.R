# DISCOUNTED CASH FLOW MODEL



# access Quandl's API 
Quandl.api_key('Sp7tA8SGPyjyqrzkssRT')

# get SHARADAR information from Quandl
data_amd <- Quandl.datatable('SHARADAR/SF1', dimension="ART", ticker='AMD', paginate = T)
# convert Quandl information to dataframe
data_amd <- data.frame(data_amd)
# add quarters to the tabke
data_amd$quarter <- quarters(data_amd$reportperiod)



# create subsets

# recent 2018 subset
recent_subset <- subset(data_amd, data_amd$reportperiod == "2018-06-30")
recent_subset
# 2017 subset
subset_2017 <- subset(data_amd, data_amd$reportperiod == "2017-07-01")
# 2016 subset
subset_2016 <- subset(data_amd, data_amd$reportperiod == "2016-06-25")
# 2015 subset
subset_2015 <- subset(data_amd, data_amd$reportperiod == "2015-06-27")
# 2014 subset
subset_2014 <- subset(data_amd, data_amd$reportperiod == "2014-06-28")
# 2013 subset
subset_2013 <- subset(data_amd, data_amd$reportperiod == "2013-06-29")
# 2012 subset
subset_2012 <- subset(data_amd, data_amd$reportperiod == "2012-06-30")
# 2011 subset
subset_2011 <- subset(data_amd, data_amd$reportperiod == "2011-07-02")
# 2010 subset
subset_2010 <- subset(data_amd, data_amd$reportperiod == "2010-06-26")
# 2009 subset
subset_2009 <- subset(data_amd, data_amd$reportperiod == "2009-06-27")
# 2008 subset
subset_2008 <- subset(data_amd, data_amd$reportperiod == "2008-06-28")




# get ebit data
ebit_data <- data_amd$ebit
data_ebit_period <- data_amd[,c("reportperiod","ebit", "quarter")]
data_ebit_period
recent_ebit <- recent_subset$ebit
recent_ebit
# taxes
taxes_data <- data_amd$taxexp
recent_taxes_data <- recent_subset$taxexp
#capex
capex_data <- data_amd$capex
recent_capex_data <- recent_subset$capex
# depreciation and amort
depr_amor <- data_amd$depamor
recent_depr_amor <- recent_subset$depamor
# working capital
working_capital <- data_amd$workingcapital
recent_working_capital <- recent_subset$workingcapital
working_capital_2017 <- subset_2017$workingcapital
# cash
cash_data <- data_amd$cashneq
recent_cash_data <- recent_subset$cashneq
cash_2017 <- subset_2017$cashneq








# % change
perc_change_funct <- function(new, old){
  increase <- new - old
  perc_change <- increase / old * 100
}



# ebit % change
ebit_1y_change <- perc_change_funct(recent_subset$ebit, subset_2017$ebit)
ebit_1y_change
ebit_5y_change <- perc_change_funct(recent_subset$ebit, subset_2013$ebit)
ebit_5y_change
# taxes % change
taxes_1y_change <- perc_change_funct(recent_subset$taxexp, subset_2017$taxexp)
taxes_1y_change
taxes_5y_change <- perc_change_funct(recent_subset$taxexp, subset_2013$taxexp)
taxes_5y_change
# CAPEX % change
capex_1y_change <- perc_change_funct(recent_subset$capex, subset_2017$capex)
capex_1y_change
capex_5y_change <- perc_change_funct(recent_subset$capex, subset_2013$capex)
capex_5y_change
# Depreciation & Amortization % change
depr_amor_1y_change <- perc_change_funct(recent_subset$depamor, subset_2017$depamor)
depr_amor_1y_change
depr_amor_5y_change <- perc_change_funct(recent_subset$depamor, subset_2013$depamor)
depr_amor_5y_change
# Working capital % change
working_capital_1y_change <- perc_change_funct(recent_subset$workingcapital, subset_2017$workingcapital)
working_capital_1y_change
working_capital_5y_change <- perc_change_funct(recent_subset$workingcapital, subset_2013$workingcapital)
working_capital_5y_change
# Cash % change
cash_1y_change <- perc_change_funct(recent_subset$cashneq, subset_2017$cashneq)
cash_1y_change
cash_5y_change <- perc_change_funct(recent_subset$cashneq, subset_2013$cashneq)
cash_5y_change






# Percentage sales method 





# create two vectors: one for historical (hist) revenues
# and one for projected (proj) revenues
hist <- c(recent_subset$revenue, subset_2017$revenue, subset_2016$revenue, subset_2015$revenue, subset_2014$revenue, subset_2013$revenue, subset_2012$revenue, subset_2011$revenue, subset_2010$revenue, subset_2009$revenue, rep(0,5))






#forcast should be build up to unlevered free cash flow (FCFF)
# **********
# calculate changes in non-cash working capital 
ncwc_recent <- recent_working_capital
ncwc_old <- working_capital_2017
# find change/increase in working capital
wc_1y_change <- ncwc_recent - ncwc_old
# change formula on ncwc from present date amd ncwc from two years ago






# Net Income function
net_income_func <- function(revenue, COGS, op_exp, interest_exp, taxes){
  gross_profit <- revenue - COGS
  ebit <- gross_profit - op_exp
  pre_tax_income <-ebit - interest_exp
  net_income <- pre_tax_income - taxes
}
#net_income_func(recent_subset$revenue, recent_subset$c


# present Net Income
present_NI <- recent_subset$netinc






# fcfe function
fcfe_func <- function(net_income, depn_amor, capex, inc_wc){
  fcfe <- net_income + depn_amor - capex - inc_wc
  print(fcfe)
}
present_fcfe <- fcfe_func(present_NI, recent_depr_amor, recent_capex_data, ncwc_1y_change)




# PV of FCFE 
k_e <- 0.15
cf <- rep(100, 5)
cf <- data.frame(cf)

cf$period <- seq(1, 5, 1)

cf$pv_factor <- 1 / (1 + k_e)^cf$period

cf$pv <- cf$cf * cf$pv_factor

cf

pv_fcfe <- sum(cf$pv)
pv_fcfe
  
  
  





# terminal value function; PGR = perpetuity growth rate
terminal_value_func <- function(FCFE_end_of_forecast, cost_of_equity, PGR = .03){
  terminal_value <- (FCFE_end_of_forecast*(1 + PGR))/(cost_of_equity - PGR)
}


# PV of Terminal Value
pv_of_tv <- function(term_value_yr5, k_e){
  pv_tv <- term_value_yr5 / (1 + k_e)^5
  print(pv_tv)
}

tv_yr5 <- 858.33
k_e <- 0.15

pv_of_terminal <- pv_of_tv(tv_yr5, k_e)

  







# Combine PV of FCFE abd PV of Terminal Value
equity_value <- pv_fcfe + pv_of_terminal
equity_value
# convert to a per share number
# assume x million shares outstanding
shout <- 969
equity_per_share <- equity_value / shout
equity_per_share








#calculate FCFF; ebit = ebit, t = taxes, dep_and_amort = depreciation and amortization, change_NCWC = increases in noncash working capital
fcff_funct <- function(ebitda, t, capex, dep_and_amort, increase_ncwc){
  fcff <- (ebitda - t - capex + dep_and_amort - increase_ncwc)
}

#fcff_funct(recent_ebitda, recent_taxes_data, recent_capex_data, recent_depr_amor, change_ncwc)

recent_fcff <- fcff_funct(recent_ebitda, recent_taxes_data, recent_capex_data, recent_depr_amor, ncwc_1y_change)
recent_fcff


