library(tidyverse)
library(reshape2)

tax.data <- read_csv("./Data/TaxRevenue.csv") # from https://www.pewtrusts.org/en/research-and-analysis/data-visualizations/2014/fiscal-50#ind0 , 4th quarter 2017 4 quarter rolling average
partisan.data <- read_csv("./Data/partisanComposition.csv") #from http://www.ncsl.org/Portals/1/Documents/Elections/Legis_Control_2017_March_1_9%20am.pdf
pension.data <- read_csv("./Data/PPD_PlanLevel.csv") # from http://publicplansdata.org/public-plans-database/the-2018-data-update/
gdp.data <- read_csv("./Data/percapgdp.csv") # from https://apps.bea.gov/itable/iTable.cfm?ReqID=70&step=1#reqid=70&step=1&isuri=1 
emp.data <- read_csv("./Data/emp.data.csv") # from http://www.governing.com/gov-data/public-workforce-salaries/states-most-government-workers-public-employees-by-job-type.html


tax.data <- tax.data %>% 
  select(-c(X52, X53))
tax.data <- melt(tax.data) %>% 
  filter(QtrRolAvg == "2017Q4") %>% #4QtrRolAvg dollars in thousands
  rename(BudgRev = value, GovtName = variable)

pension.data <- pension.data %>% 
  filter(fy == 2017) %>% 
  inner_join(partisan.data, by = "GovtName") %>% #removes local govts from data set
  left_join(tax.data, by = "GovtName") %>% 
  left_join(gdp.data, by = "GovtName") %>% 
  left_join(emp.data, by = "GovtName") %>% 
  select(fy, PlanFullName, GovtName, PerCapGDP, ActFundedRatio_GASB, UAAL_GASB, BudgRev, LegControl, GovParty, TotMembership, state_emp_per_tenk_pop) %>% 
  group_by(GovtName) %>% 
  summarise(mean(ActFundedRatio_GASB)) %>% 
  ungroup()

attach(pension.data)

pension.model <- lm(ActFundedRatio_GASB ~ LegControl*GovParty+state_emp_per_tenk_pop+BudgRev*PerCapGDP)
summary(pension.model)
model1 <- step(pension.model)
summary(model1)



pension.data$PlanFullName

