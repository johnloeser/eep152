library(ggplot2)
library(dplyr)
library(magrittr)
library(tidyr)
library(zoo)
library(foreign)

setwd("/home/jal/Dropbox/Apps/Github/eep152/section10/")

# # PART 1 : WDI DATA

# # # DOWNLOAD WDI FROM WORLD BANK
download.file("http://databank.worldbank.org/data/download/WDI_csv.zip", "WDI_csv.zip")
unzip("WDI_csv.zip", exdir = "WDI_csv/")
file.remove("WDI_csv.zip")

# # # DOWNLOAD BARRO LEE
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
  filter(Indicator.Name %in% c("GDP per capita (constant 2010 US$)",
                               "Population ages 0-14 (% of total)",
                               "Population ages 15-64 (% of total)",
                               "Contraceptive prevalence, any methods (% of women ages 15-49)",
                               "Contraceptive prevalence, modern methods (% of women ages 15-49)"))

data <- data %>% gather(year, val, -(1:4))

data <- data %>% mutate(year = substr(year, 2, 5) %>% as.integer)

data <- data %>% select(-Country.Name, -Indicator.Name)

data <- data %>% spread(Indicator.Code, val)

data <- data %>% rename(gdppc = NY.GDP.PCAP.KD,
                        pop0014 = SP.POP.0014.TO.ZS,
                        pop1564 = SP.POP.1564.TO.ZS,
                        contracepany = SP.DYN.CONU.ZS,
                        contracepmod = SP.DYN.CONM.ZS)

data <- data %>%
  group_by(Country.Code) %>%
  mutate(gdppc = order_by(year, na.locf(gdppc, na.rm = F)),
         pop0014 = order_by(year, na.locf(.01 * pop0014, na.rm = F)),
         pop1564 = order_by(year, na.locf(.01 * pop1564, na.rm = F)),
         contracepany = order_by(year, na.locf(.01 * contracepany, na.rm = F)),
         contracepmod = order_by(year, na.locf(.01 * contracepmod, na.rm = F))) %>%
  ungroup

data <- data %>% filter(year %in% c(1980, 2010))

data <- data %>% mutate(lgdppc = log(gdppc))

data <- data %>% left_join(bl)

data <- data %>% mutate(childdeprat = pop0014 / pop1564)

data <- data %>%
  group_by(Country.Code) %>%
  mutate(dyr_sch = yr_sch - lag(yr_sch, order_by = year),
         dlgdppc = lgdppc - lag(lgdppc, order_by = year),
         dcontracepany = contracepany - lag(contracepany, order_by = year),
         dchilddeprat = childdeprat - lag(childdeprat, order_by = year)) %>%
  ungroup

data <- data %>%
  group_by(Country.Code) %>%
  mutate(allvar = any(!is.na(dyr_sch)) & any(!is.na(dlgdppc)) &
           any(!is.na(dcontracepany)) & any(!is.na(dchilddeprat))) %>%
  ungroup %>%
  filter(allvar) %>%
  select(-allvar)

g1 <- ggplot(data, aes(x = yr_sch, y = childdeprat, col = factor(year))) +
  geom_text(aes(label = Country.Code)) +
  xlab("years of schooling") + ylab("children per adult") +
  guides(col = guide_legend(title = "year"))

reg1 <- lm(childdeprat ~ yr_sch, data = data %>% filter(year == 1980))
reg2 <- lm(childdeprat ~ yr_sch, data = data %>% filter(year == 2010))

summary(reg1)
summary(reg2)

g2 <- ggplot(data, aes(x = yr_sch, y = childdeprat, col = factor(year))) +
  geom_path(aes(group = Country.Code), col = "black") +
  geom_point() +
  xlab("years of schooling") + ylab("children per adult") +
  guides(col = guide_legend(title = "year"))

g3 <- ggplot(data, aes(x = dyr_sch, y = dchilddeprat)) +
  geom_text(aes(label = Country.Code)) +
  xlab("change in years of schooling") + ylab("change in children per adult")

reg3 <- lm(dchilddeprat ~ dyr_sch, data = data)
reg4 <- lm(dchilddeprat ~ dyr_sch + dlgdppc, data = data)
reg5 <- lm(dchilddeprat ~ dyr_sch + dlgdppc + dcontracepany, data = data)
# reg6 <- lm(yr_sch ~ childdeprat + factor(year) + factor(Country.Code), data = data)

summary(reg3)
summary(reg4)
summary(reg5)

file.remove("bl.csv")
unlink("WDI_csv", recursive = T)

# # PART 2 : RWANDA HOUSEHOLD SURVEY DATA

# NOTE : YOU'D NEED TO DOWNLOAD THESE FILES FOR THIS TO WORK
eicvh <- read.dta("/home/jal/Dropbox/research/2016/rwaconssurvey/data/eicv4/cs_s0_s5_household.dta")
eicvi <- read.dta("/home/jal/Dropbox/research/2016/rwaconssurvey/data/eicv4/cs_s1_s2_s3_s4_s6a_s6e_s6f_person.dta")

eicvh <- eicvh %>%
  select(hhid, province, district, ur2012, s0qb) %>%
  rename(urbrur = ur2012) %>%
  mutate(s0qb = as.character(s0qb)) %>%
  mutate(ubudehe = substr(s0qb, nchar(s0qb), nchar(s0qb))) %>%
  select(-s0qb) %>%
  filter(ubudehe != "t") %>%
  mutate(ubudehe = as.integer(ubudehe))

eicvi <- eicvi %>%
  select(hhid, s1q1, s1q3y, s4aq1, s4aq2) %>%
  mutate(schooling = c("Primary 6,7,8" = 6, "Post primary 2" = 8, "Primary 4" = 4, "Primary 5" = 5,
                       "Secondary 6" = 12, "Post primary 1" = 7, "Post primary 3" = 9,
                       "Post primary 4" = 10, "Secondary 4" = 10, "Secondary 3" = 9, "Primary 1" = 1,
                       "University 2" = 14, "Primary 3" = 3, "Primary 2" = 2, "Post primary 6,7,8" = 12,
                       "University 1" = 13, "Secondary 5" = 11, "Secondary 2" = 8, "Pre-primary" = 0,
                       "Univeristy 4" = 16, "Secondary 1" = 7, "University 3" = 15,
                       "Not complete P1" = 0, "University 5" = 16, "University 7" = 16,
                       "Post primary 5" = 11, "University 6" = 16, "Missing" = NA)[s4aq2]) %>%
  mutate(schooling = ifelse(s4aq1 == "No", 0, schooling)) %>%
  select(-s4aq1, -s4aq2) %>%
  rename(sex = s1q1, age = s1q3y)

g4 <- ggplot(data = eicvi %>% group_by(age, sex) %>% summarise(schooling = mean(schooling, na.rm = T)),
       aes(x = age, y = schooling, col = sex, group = sex)) + geom_line() +
  xlim(0, 60) +
  geom_abline(slope = 1, intercept = -5, col = "red", linetype = 2)

eicvh2 <- eicvi %>%
  filter(age >= 18) %>%
  group_by(hhid) %>%
  summarise(maxed = max(schooling, na.rm = T),
            maxmaleed = max(subset(schooling, sex == "Male"), na.rm = T),
            maxfemaleed = max(subset(schooling, sex == "Female"), na.rm = T)) %>%
  ungroup %>%
  mutate(maxed = ifelse(maxed < 0, NA, maxed),
         maxmaleed = ifelse(maxmaleed < 0, NA, maxmaleed),
         maxfemaleed = ifelse(maxfemaleed < 0, NA, maxfemaleed))
eicvh3 <- eicvi %>%
  group_by(hhid) %>%
  summarise(childrenperadult = sum(age <= 15) / sum(age <= 65)) %>%
  ungroup
eicvh4 <- eicvi %>%
  filter(age <= 15 & age >= 5) %>%
  left_join(eicvi %>% group_by(age) %>% summarise(avgsch = mean(schooling, na.rm = T))) %>%
  group_by(hhid) %>%
  summarise(releduc = mean(schooling - avgsch, na.rm = T)) %>%
  ungroup

eicvh <- eicvh %>%
  left_join(eicvh2) %>%
  left_join(eicvh3) %>%
  left_join(eicvh4)

g5 <- ggplot(data = eicvh %>% group_by(ubudehe) %>%
               summarise(releduc = mean(releduc, na.rm = T)),
             aes(x = ubudehe, y = releduc)) + geom_point()

lm(releduc ~ factor(ubudehe), data = eicvh) %>% summary
lm(releduc ~ maxed + factor(ubudehe), data = eicvh) %>% summary
lm(releduc ~ maxmaleed + maxfemaleed + factor(ubudehe), data = eicvh) %>% summary
lm(releduc ~ maxmaleed + maxfemaleed + childrenperadult + factor(ubudehe) + factor(district), data = eicvh) %>% summary
