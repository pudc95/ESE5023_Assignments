#1.1
setwd("D:/ESE5023")
library(tidyr)
library(dplyr)
library(ggplot2)
library(gapminder)
Sig_Eqs0 <- read.csv(file = "signif.csv", header = T)
Sig_Eqs <- as_tibble(Sig_Eqs0)
Sig_Eqs
#1.2
DEATHS_Sum <- Sig_Eqs %>%
  group_by(COUNTRY) %>%
  summarize(DEATHS_sum = sum(DEATHS, na.rm = TRUE)) %>%
  arrange(desc(DEATHS_sum))
DEATHS_Sum
#1.3
Sig_Eqs %>%
  select(YEAR, EQ_PRIMARY) %>%
  filter(EQ_PRIMARY > 6) %>%
  group_by(YEAR) %>%
  summarize(Num = length(EQ_PRIMARY)) %>%
  ggplot(aes(x=YEAR, y=Num)) + 
  geom_line()
#1.4
CountEq_LargestEq <- function(Country){
  COUNTRY_EQ <- Sig_Eqs %>%
    select(YEAR, MONTH, DAY, COUNTRY, EQ_PRIMARY) %>%
    filter(EQ_PRIMARY > 0 &  COUNTRY == Country) %>%
    group_by(COUNTRY) 
  Count <-COUNTRY_EQ %>%
    select(EQ_PRIMARY) %>%
    summarize(Num = length(EQ_PRIMARY))
  #print(Count)
  Max_Day <- COUNTRY_EQ[which(COUNTRY_EQ$EQ_PRIMARY == max(COUNTRY_EQ$EQ_PRIMARY)),]
  #print(Max_Day)
  return(list(Count,Max_Day))
}
CountEq_LargestEq("GREECE")
Country <- Sig_Eqs %>% 
  pull(COUNTRY)
Country<- Country[!duplicated(Country)]
Country

CountEq <- c()
for (i in 1:length(Country)) {
  CountEq <- c(CountEq,CountEq_LargestEq(Country[i]))
}
CountEq


