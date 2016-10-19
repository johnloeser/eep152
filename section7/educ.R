library(ggplot2)
library(dplyr)
library(magrittr)
library(tidyr)
library(zoo)

setwd("/home/jal/Dropbox/Apps/Github/eep152/section7/")

# # DOWNLOAD WDI FROM WORLD BANK
download.file("http://databank.worldbank.org/data/download/WDI_csv.zip", "WDI_csv.zip")
unzip("WDI_csv.zip", exdir = "WDI_csv/")
file.remove("WDI_csv.zip")

# # DOWNLOAD BARRO LEE
download.file("http://www.barrolee.com/data/BL_v2.1/BL2013_MF2599_v2.1.csv", "bl.csv")

bl <- read.csv("bl.csv")

bl <- bl %>%
  select(year, yr_sch, WBcode) %>%
  rename(Country.Code = WBcode)

data <- read.csv("WDI_csv/WDI_Data.csv")

countries <- unique(data$Country.Name)[-(1:47)]

data <- data %>% filter(Country.Name %in% countries)

# unique(data$Indicator.Name)[grepl("gdp", unique(data$Indicator.Name), ignore.case = T)]
# unique(data$Indicator.Name)[grepl("education", unique(data$Indicator.Name), ignore.case = T)]
# unique(data$Indicator.Name)[grepl("enrolment", unique(data$Indicator.Name), ignore.case = T)]

data <- data %>%
  filter(Indicator.Name %in% c("GDP per capita (constant 2010 US$)"))

data <- data %>% gather(year, val, -(1:4))

data <- data %>% mutate(year = substr(year, 2, 5) %>% as.integer)

data <- data %>% select(-Country.Name, -Indicator.Name)

data <- data %>% spread(Indicator.Code, val)

data <- data %>% rename(gdppc = NY.GDP.PCAP.KD)

data <- data %>%
  group_by(Country.Code) %>%
  mutate(gdppc = order_by(year, na.locf(gdppc, na.rm = F))) %>%
  ungroup

data <- data %>% filter(year %in% c(1980, 2010))

data <- data %>% mutate(lgdppc = log(gdppc))

data <- data %>% left_join(bl)

data <- data %>%
  group_by(Country.Code) %>%
  mutate(dyr_sch = yr_sch - lag(yr_sch, order_by = year),
         dlgdppc = lgdppc - lag(lgdppc, order_by = year)) %>%
  ungroup

data <- data %>%
  group_by(Country.Code) %>%
  mutate(allvar = any(!is.na(dyr_sch)) & any(!is.na(dlgdppc))) %>%
  ungroup %>%
  filter(allvar) %>%
  select(-allvar)

g1 <- ggplot(data, aes(x = yr_sch, y = lgdppc, col = factor(year))) +
  geom_text(aes(label = Country.Code)) +
  xlab("years of schooling") + ylab("log per capita gdp") +
  guides(col = guide_legend(title = "year"))

reg1 <- lm(lgdppc ~ yr_sch, data = data %>% filter(year == 1980))
reg2 <- lm(lgdppc ~ yr_sch, data = data %>% filter(year == 2010))

summary(reg1)
summary(reg2)

g2 <- ggplot(data, aes(x = yr_sch, y = lgdppc, col = factor(year))) +
  geom_path(aes(group = Country.Code), col = "black") +
  geom_point() +
  xlab("years of schooling") + ylab("log per capita gdp") +
  guides(col = guide_legend(title = "year"))

g3 <- ggplot(data, aes(x = dyr_sch, y = dlgdppc)) +
  geom_text(aes(label = Country.Code)) +
  xlab("change in years of schooling") + ylab("change in log per capita gdp")

reg3 <- lm(dlgdppc ~ dyr_sch, data = data)
reg4 <- lm(lgdppc ~ yr_sch + factor(year) + factor(Country.Code), data = data)

summary(reg3)
summary(reg4)

# file.remove("bl.csv")
# unlink("WDI_csv", recursive = T)