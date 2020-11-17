# ________________________________________________________
# Title:    Thesis
# Author:   Giulio Ciacchini
# Version:  14 March 2019
# ________________________________________________________

library(readxl)
library(tidyverse)
library(lubridate)
if (!require('mFilter')) install.packages('mFilter'); library('mFilter')
library(lfe)
library(plm)
install.packages("stargazer"); library(stargazer)
library(Hmisc) # to make time lags and correlation matrix
#library(lmtest) # to compute granger test

rm(list = ls())
options(scipen = 4) # number of significant digits to show, in this case 4

View(read_xlsx("C:/Users/giuli/Docs/Useful Docs/Thesis/data.xlsx"))

# prepare dataframe
df <-read_xlsx("C:/Users/giuli/Docs/Useful Docs/Thesis/data.xlsx") %>%
  setNames(c("country", "code", "year", "gdp", "pbl", "prv")) %>%
  select(., c("country", "code", "year", "gdp", "pbl", "prv")) %>%
  mutate(., country = as.factor(country)) %>%
  mutate(., year = as.Date(as.character(year), format = "%Y")) %>%
  na.omit(.) %>%
  group_by(country) %>%
  mutate(., gdp_log = log(gdp)) %>%
  mutate(., gdp_dif = c(NA, diff(gdp_log))) %>%
  mutate(., gdp_hp = hpfilter(gdp_log, 100)$cycle) %>% #cyclical components, deviation from the trend
  mutate(., pbl_lag = Lag(c(pbl)),shift=1) %>%
  mutate(., pbl_log = log(pbl)) %>%
  mutate(., pbl_sq = (pbl^2)) %>%
  mutate(., pbl_dif = c(NA, diff(pbl_log))) %>%
  #mutate(., pbl_dif_sq = (pbl_dif^2)) %>%
  mutate(., pbl_dif_lag = Lag(c(pbl_dif)),shift=1) %>%
  mutate(., prv_lag = Lag(c(prv)),shift=1) %>%
  mutate(., prv_log = log(prv)) %>%
  mutate(., prv_sq = (prv^2)) %>%
  mutate(., prv_dif = c(NA, diff(prv_log))) %>%
  #mutate(., prv_dif_sq = (prv_dif^2)) %>%
  mutate(., prv_dif_lag = Lag(c(prv_dif)),shift=1) %>%
  #mutate(., gdp_dif3 = lead(gdp_dif,3)) %>%
  #mutate(., gdp_f3 = lead(gdp_f,3)) %>%
  mutate(., pbl_hp = hpfilter(pbl, 100)$cycle) %>%
  #mutate(., pbl_hp_sq = (pbl_hp^2)) %>%
  mutate(., pbl_hp_lag = Lag(c(pbl_hp)),shift=1) %>%
  mutate(., prv_hp = hpfilter(prv, 100)$cycle) %>%
  #mutate(., prv_hp_sq = (prv_hp^2)) %>%
  mutate(., prv_hp_lag = Lag(c(prv_hp)),shift=1) %>%
  #mutate(.,prv_pbl = prv_hp*pbl_hp) %>%
  mutate(., pbl_high = if_else(pbl > 62.8, 1, 0)) %>%
  mutate(., prv_high = if_else(prv > 122.8, 1, 0)) %>%
  ungroup() %>%
  na.omit(.)
View(df)

head(df, 10)
dim(df)
glimpse(df)
describe(df)
summary(df)

install.packages("skimr")
library(skimr)
skim(df)

install.packages("devtools")
library(devtools)

devtools::install_github("ropensci/visdat")
library(visdat)

#The Vis_dat() function of the visdat package is a great way to visualize the data type and missing data within a data frame.
vis_miss(df)
vis_dat(df)

pdata <- pdata.frame(df, index=c("country","year"), drop.index=TRUE, row.names=TRUE)
View(pdata)

gdp_dif3 <-lag(pdata$gdp_dif, k=,3)
gdp_hp3 <-lead(pdata$gdp_hp, k=,3)
df <-cbind(df,gdp_dif3,gdp_hp3)

#gdp_lag
gdp_lag3 <-lag(pdata$gdp_dif, k=,3)
df <-cbind(df,gdp_lag3)

#gdp_lead
gdp_dif1 <-lead(pdata$gdp_dif, k=,1)
gdp_dif2 <-lead(pdata$gdp_dif, k=,2)
gdp_dif4 <-lead(pdata$gdp_dif, k=,4)
gdp_dif5 <-lead(pdata$gdp_dif, k=,5)
gdp_dif6 <-lead(pdata$gdp_dif, k=,6)
df <-cbind(df,gdp_dif1,gdp_dif2,gdp_dif4,gdp_dif5,gdp_dif6)

#pbl_lag
pbl_lag2 <-lead(pdata$pbl_dif, k=,-2)
prv_lag2 <-lag(pdata$prv_dif, k=,2)
pbl_hp_lag2 <-lag(pdata$pbl_hp, k=,2)
prv_hp_lag2 <-lag(pdata$prv_hp, k=,2)
df <-cbind(df,pbl_lag2,prv_lag2,pbl_hp_lag2,prv_hp_lag2)

pbl_lag3 <-lead(pdata$pbl_dif, k=,-3)
prv_lag3 <-lag(pdata$prv_dif, k=,3)
pbl_lag4 <-lead(pdata$pbl_dif, k=,-4)
prv_lag4 <-lag(pdata$prv_dif, k=,4)
pbl_lag5 <-lead(pdata$pbl_dif, k=,-5)
prv_lag5 <-lag(pdata$prv_dif, k=,5)
df <-cbind(df,pbl_lag3,prv_lag3,pbl_lag4,prv_lag4,pbl_lag5,prv_lag5)

#pbl_lead
pbl_lead3 <-lead(pdata$pbl, k=,3)
df <-cbind(df,pbl_lead3)

rownames(df) <- c()  # to delete rownames
df <-na.omit(df)
View(df)

#Low Pudebt countries
df_low_pbl <-subset(df, code %in% c("3","4","5","7","9","11"))

#High Pudebt countries
df_high_pbl <-subset(df, code %in% c("1","2","6","8","10"))

# only low-debt dataset
df_low_pbl_dummy <- df %>%
  filter(., pbl_high == 0)

# only high-debt dataset
df_high_pbl_dummy <- df %>%
  filter(., pbl_high == 1)

#-----------------------------------------------------------------------------
# Baseline panel data models (instead of HP(100) I use growth leading 3 years)
#_____________________________________________________________________________

# The coeff of x1 indicates how much Y changes overtime, on average per country, 
# when X increases by one unit. 
ols <-lm(data = df,
         formula = gdp_dif3 ~ pbl +prv)

summary(ols)

res_dif0 <- felm(data = df,
                 formula = gdp_dif3 ~ pbl - 1| country)

#ggplot(df, aes(x=pbl, y=gdp_dif3, shape=country, color=country)) +
#  geom_point()

ggplot(df, aes(pbl,gdp_dif3)) +
  geom_point() +
  xlab("Public debt-to-GDP t") + ylab("GDP t+3") +
  geom_smooth(span = 0.8) +
  facet_wrap(~country)

ggplot(df, aes(pbl,gdp_dif3)) +
  geom_point() +
  xlab("Public debt-to-GDP t") + ylab("GDP t+3") +
  geom_smooth(span = 0.8)

ggplot(df, aes(prv,gdp_dif3)) +
  geom_point() +
  xlab("Private debt-to-GDP t") + ylab("GDP t+3") +
  geom_smooth(span = 0.8)

ggplot(df, aes(pbl_dif,gdp_dif3)) +
  geom_point() +
  xlab("Public debt growth t") + ylab("GDP t+3") +
  geom_smooth(span = 0.8)

ggplot(df, aes(prv_dif,gdp_dif3)) +
  geom_point() +
  xlab("Private debt growth t") + ylab("GDP t+3") +
  geom_smooth(span = 0.8)

res_dif1 <- felm(data = df,
                 formula = gdp_dif3 ~ prv - 1| country)

#ggplot(df, aes(x=prv, y=gdp_dif3, shape=country, color=country)) +
#  geom_point() +
#  geom_smooth(method = lm, se = FALSE)

res_dif2 <- felm(data = df,
                 formula = gdp_dif3 ~ pbl + prv - 1 | country)

res_dif2a <- felm(data = df,
                  formula = gdp_dif3 ~ pbl_lag + prv_lag - 1 | country)

cor(df$prv,df$prv_lag)
cor(df$pbl,df$pbl_lag)
cor(df$prv,df$pbl)
cor(df$gdp_dif,df$pbl)
cor(df$gdp_dif,df$prv)

#res_dif2a <- felm(data = df,
#                  formula = gdp_dif3 ~  pbl_lag + prv_lag - 1 | country)

#res_dif2a <- felm(data = df,
#                 formula = gdp_dif3 ~  pbl+prv+pbl_lag2 + prv_lag2 - 1 | country)

#res_dif2a <- felm(data = df,
#                  formula = gdp_dif3 ~  pbl_lag3 + prv_lag3 - 1 | country)


summary(res_dif0)
summary(res_dif1)
summary(res_dif2)
summary(res_dif2a)

# export table (log-difference results) - [code to copy-paste in latex]
stargazer(res_dif0,res_dif1,res_dif2,res_dif2a,
          title = "Variation Table 1 by Batini et al. (2018), Log-difference, gdp",
          align = TRUE)

# Time effect

res_time0 <- felm(data = df,
                  formula = gdp_dif3 ~ pbl - 1| year)

res_time1 <- felm(data = df,
                  formula = gdp_dif3 ~ prv - 1| year)

res_time2 <- felm(data = df,
                  formula = gdp_dif3 ~ pbl + prv - 1 | year)

res_time2a <- felm(data = df,
                   formula = gdp_dif3 ~ pbl_lag + prv_lag - 1 | year)

summary(res_time0)
summary(res_time1)
summary(res_time2)
summary(res_time2a)

res_dif2b <- felm(data = df,
                  formula = gdp_dif ~ pbl_lag + prv_lag - 1 | country)

res_dif2c <- felm(data = df,
                  formula = gdp_dif ~ pbl_lag2 + prv_lag2 - 1 | country)

res_dif2d <- felm(data = df,
                  formula = gdp_dif ~ pbl_lag3 + prv_lag3 - 1 | country)

res_dif2e <- felm(data = df,
                  formula = gdp_dif ~ pbl_lag4 + prv_lag4 - 1 | country)

res_dif2f <- felm(data = df,
                  formula = gdp_dif ~ pbl_lag5 + prv_lag5 - 1 | country)

summary(res_dif2b)
summary(res_dif2c)
summary(res_dif2d)
summary(res_dif2e)
summary(res_dif2f)

# export table (log-difference results) - [code to copy-paste in latex]
stargazer(res_dif2b,res_dif2c,res_dif2d,res_dif2e,res_dif2f,
          title = "Variation Table 1 by Batini et al. (2018), Log-difference, gdp",
          align = TRUE)

# Pbl over GDP

res_pbl1 <- felm(data = df,
                 formula = pbl ~ gdp_lag3  - 1| country)

res_pbl2 <- felm(data = df,
                 formula = prv ~ gdp_lag3  - 1| country)

res_pbl3 <- felm(data = df,
                 formula = pbl_lead3 ~ gdp_dif - 1| country)

summary(res_pbl1)
summary(res_pbl2)
summary(res_pbl3)

# GDP with different leads, from 1 to 6

res_dif_0 <- felm(data = df,
                  formula = gdp_dif ~ pbl + prv - 1 | country)

res_dif_1 <- felm(data = df,
                  formula = gdp_dif1 ~ pbl + prv - 1 | country)

res_dif_2 <- felm(data = df,
                  formula = gdp_dif2 ~ pbl + prv - 1 | country)

res_dif_3 <- felm(data = df,
                  formula = gdp_dif3 ~ pbl + prv - 1 | country)

res_dif_4 <- felm(data = df,
                  formula = gdp_dif4 ~ pbl + prv - 1 | country)

res_dif_5 <- felm(data = df,
                  formula = gdp_dif5 ~ pbl + prv - 1 | country)

res_dif_6 <- felm(data = df,
                  formula = gdp_dif6 ~ pbl + prv - 1 | country)

summary(res_dif_0)
summary(res_dif_1)
summary(res_dif_2)
summary(res_dif_4)
summary(res_dif_5)
summary(res_dif_6)

# export table (log-difference results) - [code to copy-paste in latex]
stargazer(res_dif_0,res_dif_1,res_dif_2,res_dif_3,res_dif_4,res_dif_5,res_dif_6,
          title = "Variation Table 1 by Batini et al. (2018), Log-difference, gdp",
          align = TRUE)


res_dif3 <- felm(data = df_low_pbl,
                 formula = gdp_dif3 ~ prv + pbl - 1 | country)

res_dif4 <- felm(data = df_high_pbl,
                 formula = gdp_dif3 ~ prv + pbl - 1 | country)

res_dif5 <- felm(data = df_low_pbl_dummy,
                 formula = gdp_dif3 ~ prv + pbl - 1 | country)

res_dif6 <- felm(data = df_high_pbl_dummy,
                 formula = gdp_dif3 ~ prv + pbl - 1 | country)

summary(res_dif3)
summary(res_dif4)
summary(res_dif5)
summary(res_dif6)

# export table (log-difference results) - [code to copy-paste in latex]
stargazer(res_dif3,res_dif4,res_dif5,res_dif6,
          title = "Variation Table 1 by Batini et al. (2018), Log-difference, gdp",
          align = TRUE)

res_dif7 <-felm(data= df,
                formula = gdp_dif3 ~ pbl + pbl_sq + prv + prv_sq -1 | country)

ggplot(df, aes(x=prv_sq, y=gdp_dif3, shape=country, color=country)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE)

res_dif8 <-felm(data= df,
                formula = gdp_dif3 ~ pbl + prv + prv*pbl -1 | country)

summary(res_dif7)
summary(res_dif8)

# export table (log-difference results) - [code to copy-paste in latex]
stargazer(res_dif7,res_dif8,
          title = "Variation Table 1 by Batini et al. (2018), Log-difference, gdp",
          align = TRUE)

PRV <- seq(from=0, to=500, by=1)

derPRV_square <- -0.00107 + 0.0000026 * PRV  # squared

plot(derPRV_square, type = "l", xlab = "Private debt-to-GDP", ylab = "Partial Derivative of Private debt-to-GDP")
lines(rep(0,length(derPRV_square)), col ="blue")

PRV <- seq(from=0, to=200, by=1)
PBL <- seq(from=0, to=200, by=1)

derPBL <- -0.00034 + 0.000005 * PRV   # interaction
derPRV <- -0.00063 + 0.000005 * PBL   # interaction

plot(derPBL, type = "l", xlab = "Private debt-to-GDP", ylab = "Partial Derivative of Public debt-to-GDP")
lines(rep(0,length(derPBL)), col ="blue")

plot(derPRV, type = "l", xlab = "Public debt-to-GDP", ylab = "Partial Derivative of Private debt-to-GDP")
lines(rep(0,length(derPRV)), col ="blue")


res_dif00 <- felm(data = df,
                  formula = gdp_dif ~ prv_lag + pbl_lag - 1 | country)

res_dif000 <-felm(data = df,
                  formula = pbl_lag ~ pbl_lag + prv_lag -1 | country)

res_dif0000 <-felm(data = df,
                   formula = prv_lag ~ prv_lag + pbl_lag -1 | country)

summary(res_dif00)
summary(res_dif000)
summary(res_dif0000)

# random <-plm(data = df,
#             formula = gdp_dif3 ~ prv + pbl  - 1 , index = "country","year", model = random)

# Hausmann Test: if the p value is significant use Fixed Effects
# phtest(res_dif2,random)

# export table (log-difference results) - [code to copy-paste in latex]
stargazer(res_dif1,res_dif2,res_dif3,res_dif4,res_dif5,res_dif6,
          title = "Variation Table 1 by Batini et al. (2018), Log-difference, gdp",
          align = TRUE)

# Using gdp_dif3 with prv_dif ad pbl_dif different results

res_diff0 <- felm(data = df,
                  formula = gdp_dif3 ~ pbl_dif - 1 | country)

res_diff1 <- felm(data = df,
                  formula = gdp_dif3 ~ prv_dif - 1 | country)

res_diff2 <- felm(data = df,
                  formula = gdp_dif3 ~ prv_dif + pbl_dif - 1 | country)

res_diff3 <- felm(data = df_low_pbl,
                  formula = gdp_dif3 ~ prv_dif + pbl_dif - 1 | country)

res_diff4 <- felm(data = df_high_pbl,
                  formula = gdp_dif3 ~ prv_dif + pbl_dif - 1 | country)

res_diff5 <- felm(data = df_low_pbl_dummy,
                  formula = gdp_dif3 ~ prv_dif + pbl_dif - 1 | country)

res_diff6 <- felm(data = df_high_pbl_dummy,
                  formula = gdp_dif3 ~ prv_dif + pbl_dif - 1 | country)

summary(res_diff0)
summary(res_diff1)
summary(res_diff2)
summary(res_diff3)
summary(res_diff4)
summary(res_diff5)
summary(res_diff6)

# Export table (log-difference results) - [code to copy-paste in latex]
stargazer(res_diff0,res_diff1,res_diff2,
          title = "Variation Table 1 by Batini et al. (2018), Log-difference gdp, prv, pbl",
          align = TRUE)

# Export table (log-difference results) - [code to copy-paste in latex]
stargazer(res_diff3,res_diff4,res_diff5,res_diff6,
          title = "Non-linearity check, Log-difference gdp, prv, pbl",
          align = TRUE)

res_diff7 <-felm(data= df,
                 formula = gdp_dif3 ~ prv_dif + prv_dif_sq + pbl_dif + pbl_dif_sq -1 | country)

res_diff8 <-felm(data= df,
                 formula = gdp_dif3 ~ prv_dif + pbl_dif + prv_dif*pbl_dif -1 | country)

res_diff9 <-felm(data= df,
                 formula = gdp_dif3 ~ prv_dif + prv_dif_sq + pbl_dif + pbl_dif_sq + prv_dif*pbl_dif -1 | country)

summary(res_diff7)
summary(res_diff8)
summary(res_diff9)

# Export table (square variables) - [code to copy-paste in latex]
stargazer(res_diff7,res_diff8,res_diff9,
          title = "Variation Table 1 by Batini et al. (2018), Log-difference gdp, prv, pbl",
          align = TRUE)

res_diff00 <- felm(data = df,
                   formula = gdp_dif ~ prv_lag3 + pbl_lag3 - 1 | country)

res_diff001 <- felm(data = df,
                    formula = gdp_dif ~ prv_dif_lag + pbl_dif_lag - 1 | country)

res_diff00a <- felm(data = df,
                    formula = pbl_dif ~ gdp_dif + pbl_dif_lag - 1 | country)

res_diff00b <- felm(data = df,
                    formula = prv_dif ~ prv_lag3 + pbl_lag3 - 1 | country)

res_diff00c <- felm(data = df,
                    formula = prv_dif ~ prv_dif_lag + pbl_dif_lag - 1 | country)

res_diff00d <- felm(data = df,
                    formula = pbl_dif ~ pbl_lag3 + prv_lag3 - 1 | country)

res_diff00e <- felm(data = df,
                    formula = pbl_dif ~ pbl_dif_lag + prv_dif_lag - 1 | country)

summary(res_diff00)
summary(res_diff001)
summary(res_diff00a)
summary(res_diff00b)
summary(res_diff00c)
summary(res_diff00d)
summary(res_diff00e)

grangertest(df$prv_dif ~ df$pbl_dif, order = 1, na.action = na.omit)

grangertest(df$pbl_dif ~ df$prv_dif, order = 1, na.action = na.omit)

#pgrangertest(prv ~ pbl, data = df, order  = 1L, index = NULL)

#-----------------------------------------------------
# Baseline panel data models with HP(100) gdp, prv, pbl
#______________________________________________________

res_hp0 <- felm(data = df,
                formula = gdp_hp3 ~ pbl_hp - 1 | country )

res_hp1 <- felm(data = df,
                formula = gdp_hp3 ~ prv_hp - 1 | country )

res_hp2 <- felm(data = df,
                formula = gdp_hp3 ~  pbl_hp + prv_hp - 1 | country)

res_hp2b <- felm(data = df,
                 formula = gdp_hp3 ~ pbl_hp_lag + prv_hp_lag - 1 | country)

cor(df$prv_hp,df$prv_hp_lag)
cor(df$pbl_hp,df$pbl_hp_lag)

#res_hp2b <- felm(data = df,
#                 formula = gdp_hp3 ~  pbl_hp_lag + prv_hp_lag - 1 | country)


summary(res_hp0)
summary(res_hp1)
summary(res_hp2)
summary(res_hp2b)

# Export table (HP(100) results) - [code to copy-paste in latex]
stargazer(res_hp0,res_hp1,res_hp2,res_hp2b,
          title = "Replication Table 1 by Batini et al. (2018), HP Filtered",
          align = TRUE)

# Export table (HP(100) results) - [code to copy-paste in latex]
stargazer(res_dif0,res_dif1,res_dif2,res_hp0,res_hp1,res_hp2,
          title = "Replication Table 1 by Batini et al. (2018), HP Filtered",
          align = TRUE)

res_hp3 <- felm(data = df_low_pbl,
                formula = gdp_hp3 ~ prv_hp + pbl_hp - 1 | country)

res_hp4 <- felm(data = df_high_pbl,
                formula = gdp_hp3 ~ prv_hp + pbl_hp - 1 | country)

res_hp5 <- felm(data = df_low_pbl_dummy,
                formula = gdp_hp3 ~ prv_hp + pbl_hp - 1 | country)

res_hp6 <- felm(data = df_high_pbl_dummy,
                formula = gdp_hp3 ~ prv_hp + pbl_hp - 1 | country)

summary(res_hp3)
summary(res_hp4)
summary(res_hp5)
summary(res_hp6)

# Export table (HP(100) results) - [code to copy-paste in latex]
stargazer(res_hp3,res_hp4,res_hp5,res_hp6,
          title = "Replication Table 1 by Batini et al. (2018), HP Filtered",
          align = TRUE)

# Export table (HP(100) results) - [code to copy-paste in latex]
stargazer(res_dif5,res_dif6,res_hp5,res_hp6,
          title = "Replication Table 1 by Batini et al. (2018), HP Filtered",
          align = TRUE)

res_hp7 <- felm(data = df,
                formula = gdp_hp3 ~ prv_hp + prv_hp_sq + pbl_hp + pbl_hp_sq - 1 | country)

res_hp8 <- felm(data = df,
                formula = gdp_hp3 ~ prv_hp + pbl_hp + prv_hp*pbl_hp - 1 | country)

res_hp9 <- felm(data = df,
                formula = gdp_hp3 ~ prv_hp + prv_hp_sq + pbl_hp + pbl_hp_sq + prv_hp*pbl_hp - 1 | country)

summary(res_hp7)
summary(res_hp8)
summary(res_hp9)

res_hp00 <- felm(data = df,
                 formula = gdp_hp ~ prv_hp_lag + pbl_hp_lag - 1 | country)

res_hp000 <- felm(data = df,
                  formula = pbl_hp ~ pbl_hp_lag + prv_hp_lag - 1 | country)

res_hp0000 <- felm(data = df,
                   formula = prv_hp ~ prv_hp_lag + pbl_hp_lag - 1 | country)

summary(res_hp00)
summary(res_hp000)
summary(res_hp0000)

grangertest(df$prv_hp ~ df$pbl_hp, order = 1, na.action = na.omit)

grangertest(df$pbl_hp ~ df$prv_hp, order = 1, na.action = na.omit)


# Export table (HP(100) square variables) - [code to copy-paste in latex]
stargazer(res_hp7,
          title = "Replication Table 1 by Batini et al. (2018), HP Filtered",
          align = TRUE)

#-------------------------------------------
# Replication second table with growth rates
#___________________________________________

df_gr <-read_xlsx("C:/Users/pc/Desktop/Thesis/Data/Long term Annual Dataset/RDATA/DATA growth rate.xlsx") %>%
  setNames(c("country", "code", "year", "gdp_gr", "pbl_gr", "prv_gr","pbl","prv")) %>%
  select(., c("country","code", "year", "gdp_gr", "pbl_gr", "prv_gr","pbl","prv")) %>%
  mutate(., country = as.factor(country)) %>%
  mutate(., year = as.Date(as.character(year), format = "%Y")) %>%
  na.omit(.) %>%
  group_by(country) %>%
  ungroup() %>%
  mutate(., pbl_high_gr = if_else(pbl > 62.5, 1, 0)) %>%
  mutate(., prv_high_gr = if_else(prv > 120.4, 1, 0)) %>%
  na.omit(.)
View(df_gr)

# Baseline panel regression

res_gr0 <- felm(data = df_gr,
                formula = gdp_gr ~ pbl_gr - 1 | country)

res_gr1 <- felm(data = df_gr,
                formula = gdp_gr ~ prv_gr - 1 | country)

res_gr2<- felm(data = df_gr,
               formula = gdp_gr ~ prv_gr + pbl_gr -1 |country)

summary(res_gr0)
summary(res_gr1)
summary(res_gr2)

#Low pudebt countries
df_gr_low_pbl <-subset(df_gr, code %in% c("3","4","5","7","9","11"))

#High pudebt countries
df_gr_high_pbl <-subset(df_gr, code %in% c("1","2","6","8","10"))

# Low-Pudebt dataset with dummy
df_gr_low_pbl_dummy <- df_gr %>%
  filter(., pbl_high_gr == 0)

# High-Pudebt dataset with dummy
df_gr_high_pbl_dummy <- df_gr %>%
  filter(., pbl_high_gr == 1)

res_gr3 <- felm(data = df_gr_low_pbl,
                formula = gdp_gr ~ prv_gr + pbl_gr -1 |country)

res_gr4 <- felm(data = df_gr_high_pbl,
                formula = gdp_gr ~ prv_gr + pbl_gr -1 |country)

res_gr5 <- felm(data = df_gr_low_pbl_dummy,
                formula = gdp_gr ~ prv_gr + pbl_gr -1 |country)

res_gr6 <- felm(data = df_gr_high_pbl_dummy,
                formula = gdp_gr ~ prv_gr + pbl_gr -1 |country)

summary(res_gr3)
summary(res_gr4)
summary(res_gr5)
summary(res_gr6)

# export table (Robustness results) - [code to copy-paste in latex]
stargazer(res_gr1,res_gr2,res_gr3,res_gr4,res_gr5,res_gr6,
          title = "Replication of Table 2 by Batini et al. (2018), Growth Rates",
          align = TRUE)

#--------------------------------------------------------------------
# Replication second table with Average growth rates over three years
#____________________________________________________________________

df_agr <-read_xlsx("C:/Users/pc/Desktop/Thesis/Data/Long term Annual Dataset/RDATA/DATA average growth rate.xlsx") %>%
  setNames(c("country", "code", "year", "gdp_agr", "pbl_agr", "prv_agr","pudebt","prdebt")) %>%
  select(., c("country","code", "year", "gdp_agr", "pbl_agr", "prv_agr","pudebt","prdebt")) %>%
  mutate(., country = as.factor(country)) %>%
  mutate(., year = as.Date(as.character(year), format = "%Y")) %>%
  na.omit(.) %>%
  group_by(country) %>%
  ungroup() %>%
  mutate(., pbl_high_agr = if_else(pudebt > 62.85, 1, 0)) %>%
  mutate(., prv_high_agr = if_else(prdebt > 122.8, 1, 0)) %>%
  na.omit(.)
View(df_agr)

# Baseline panel regression with AGR
res_agr0 <-felm(data = df_agr,
                formula = gdp_agr ~ pbl_agr - 1 | country)

res_agr1 <- felm(data = df_agr,
                 formula = gdp_agr ~ prv_agr - 1 | country)

res_agr2<- felm(data = df_agr,
                formula = gdp_agr ~ prv_agr + pbl_agr -1 |country)

summary(res_agr0)
summary(res_agr1)
summary(res_agr2)

# export table (Robustness results) - [code to copy-paste in latex]
stargazer(res_agr0,res_agr1,res_agr2,
          title = "Replication of Table 2 by Batini et al. (2018), Average Growth Rates",
          align = TRUE)

#Low pudebt countries
df_agr_low_pbl <-subset(df_agr, code %in% c("3","4","5","7","9","11"))

#High pudebt countries
df_agr_high_pbl <-subset(df_agr, code %in% c("1","2","6","8","10"))

# Low-Pudebt dataset with dummy
df_agr_low_pbl_dummy <- df_agr %>%
  filter(., pbl_high_agr == 0)

# High-Pudebt dataset with dummy
df_agr_high_pbl_dummy <- df_agr %>%
  filter(., pbl_high_agr == 1)

res_agr3 <- felm(data = df_agr_low_pbl,
                 formula = gdp_agr ~ pbl_agr +prv_agr -1 |country)

res_agr4 <- felm(data = df_agr_high_pbl,
                 formula = gdp_agr ~ pbl_agr +prv_agr -1 |country)

res_agr5 <- felm(data = df_agr_low_pbl_dummy,
                 formula = gdp_agr ~ pbl_agr +prv_agr -1 |country)

res_agr6 <- felm(data = df_agr_high_pbl_dummy,
                 formula = gdp_agr ~ pbl_agr +prv_agr -1 |country)

summary(res_agr3)
summary(res_agr4)
summary(res_agr5)
summary(res_agr6)

# export table (Robustness results) - [code to copy-paste in latex]
stargazer(res_agr3,res_agr4,res_agr5,res_agr6,
          title = "Replication of Table 2 by Batini et al. (2018), Average Growth Rates",
          align = TRUE)

#-----------------------------------------
# Analysis High-Low Private Debt Countries
#_________________________________________

# Hp Filtered: Prv > 122.4 --> high prv debt country

# Low Prvdebt countries
df_low_prv <-subset(df, code %in% c("1","3","5","6","8"))
View(df_low_prv)

# High Prvdebt countries
df_high_prv <-subset(df, code %in% c("2","4","7","9","10","11"))
View(df_high_prv)

# Low prdebt dataset
df_low_prv_dummy <- df %>%
  filter(., prv_high == 0)

# High prdebt dataset
df_high_prv_dummy <- df %>%
  filter(., prv_high == 1)

# Descriptive Statistics

ggplot(data = df_low_prv, aes(x = year, y = gdp_dif, colour = country)) +
  geom_line() +
  xlab("Year") + ylab("GDP growth") +
  ggtitle("GDP growth, low private debt countries, percentage")

ggplot(data = df_high_prv, aes(x = year, y = gdp_dif, colour = country)) +
  geom_line() +
  xlab("Year") + ylab("GDP growth") +
  ggtitle("GDP growth, high private debt countries, percentage")

ggplot(data = df_low_prv, aes(x = year, y = pbl, colour = country)) +
  geom_line() +
  xlab("Year") + ylab("Public debt-to-GDP") +
  ggtitle("Publi debt-to-GDP, low private debt countries, percentage of GDP")

ggplot(data = df_high_prv, aes(x = year, y = pbl, colour = country)) +
  geom_line() +
  xlab("Year") + ylab("Public debt-to-GDP") +
  ggtitle("Public Debt-to-GDP, high private debt countries, percentage of GDP")

ggplot(data = df_low_prv, aes(x = year, y = pbl_dif, colour = country)) +
  geom_line() +
  xlab("Year") + ylab("Public debt-to-GDP") +
  ggtitle("Publi debt-to-GDP, low private debt countries, percentage of GDP")

ggplot(data = df_high_prv, aes(x = year, y = pbl_dif, colour = country)) +
  geom_line() +
  xlab("Year") + ylab("Public debt-to-GDP") +
  ggtitle("Publi debt-to-GDP, high private debt countries, percentage of GDP")

qplot(pbl,data=df_high_prv,fill=country)
qplot(pbl,data=df_low_prv,fill=country)


res_prv_dif1 <- felm(data = df_low_prv,
                     formula = gdp_dif3 ~ prv + pbl - 1 | country)

res_prv_dif2 <- felm(data = df_high_prv,
                     formula = gdp_dif3 ~ prv + pbl - 1 | country)

res_prv_dif3 <- felm(data = df_low_prv_dummy,
                     formula = gdp_dif3 ~ prv + pbl - 1 | country)

res_prv_dif4 <- felm(data = df_high_prv_dummy,
                     formula = gdp_dif3 ~ prv + pbl - 1 | country)


summary(res_prv_dif1)
summary(res_prv_dif2)
summary(res_prv_dif3)
summary(res_prv_dif4)


# Export table (High-Low Private Debt Countries) - [code to copy-paste in latex]
stargazer(res_prv_dif1,res_prv_dif2,res_prv_dif3,res_prv_dif4,
          title = "Variation of Table 2 by Batini et al. (2018), High-Low Private Debt Countries, HP Filtered",
          align = TRUE)

res_prv_hp1 <- felm(data = df_low_prv,
                    formula = gdp_hp3 ~ prv_hp + pbl_hp - 1 | country)

res_prv_hp2 <- felm(data = df_high_prv,
                    formula = gdp_hp3 ~ prv_hp + pbl_hp - 1 | country)

res_prv_hp3 <- felm(data = df_low_prv_dummy,
                    formula = gdp_hp3 ~ prv_hp + pbl_hp - 1 | country)

res_prv_hp4 <-felm(data = df_high_prv_dummy,
                   formula = gdp_hp3 ~ prv_hp + pbl_hp - 1 | country)


summary(res_prv_hp1)
summary(res_prv_hp2)
summary(res_prv_hp3)
summary(res_prv_hp4)

# Export table (High-Low Private Debt Countries) - [code to copy-paste in latex]
stargazer(res_prv_hp1,res_prv_hp2,res_prv_hp3,res_prv_hp4,
          title = "Variation of Table 2 by Batini et al. (2018), High-Low Private Debt Countries, HP Filtered",
          align = TRUE)

# Average Growth Rates: Prv > 122.8 --> high prv countries

# Low Prvdebt countries
df_agr_low_prv <-subset(df_agr, code %in% c("1","3","5","6","8"))
# AU,FIN,GER,GREE,IT
View(df_agr_prv)

# High Prvdebt countries
df_agr_high_prv <-subset(df_agr, code %in% c("2","4","7","9","10","11"))
# BEL,FRA,IR,NETH,POR,SPA
View(df_agr_prv)

# Low prdebt dataset dummy
df_agr_low_prv_dummy <- df_agr %>%
  filter(., prv_high_agr == 0)

# High prdebt dataset
df_agr_high_prv_dummy <- df_agr %>%
  filter(., prv_high_agr == 1)

res_prv_dif1 <- felm(data = df_low_prv_dummy,
                     formula = gdp_dif3 ~ prv_dif + pbl_dif - 1 | country)

res_prv_dif2 <- felm(data = df_high_prv_dummy,
                     formula = gdp_dif3 ~ prv_dif + pbl_dif - 1 | country)

res_agr_prv1 <- felm(data = df_agr_low_prv_dummy,
                     formula = gdp_agr ~ prv_agr + pbl_agr - 1 | country)

res_agr_prv2 <- felm(data = df_agr_high_prv_dummy,
                     formula = gdp_agr ~ prv_agr + pbl_agr - 1 | country)

summary(res_prv_dif1)
summary(res_prv_dif2)
summary(res_agr_prv1)
summary(res_agr_prv2)

# export table (High-Low Private Debt Countries) - [code to copy-paste in latex]
stargazer(res_prv_hp3,res_prv_hp4,res_agr_prv1,res_agr_prv2,
          title = "Variation of Table 2 by Batini et al. (2018), High-Low Private Debt Countries, Growth Rates",
          align = TRUE)


df_prv <-read_xlsx("C:/Users/pc/Desktop/Thesis/Data/Long term Annual Dataset/RDATA/DATA, Private Debt.xlsx") %>%
  setNames(c("country", "code", "year", "H", "F", "gdp")) %>%
  select(., c("country", "code", "year", "H", "F", "gdp")) %>%
  mutate(., country = as.factor(country)) %>%
  mutate(., year = as.Date(as.character(year), format = "%Y")) %>%
  na.omit(.) %>%
  group_by(country) %>%
  mutate(., gdp_log = log(gdp)) %>%
  mutate(., gdp_dif = c(NA, diff(gdp_log))) %>%
  mutate(., gdp_hp = hpfilter(gdp_log, 100)$cycle) %>% #cyclical components, deviation from the trend
  mutate(., H_lag = Lag(c(H)),shift=1) %>%
  #mutate(., pbl_log = log(pbl)) %>%
  mutate(., H_sq = (H^2)) %>%
  #mutate(., pbl_dif = c(NA, diff(pbl_log))) %>%
  #mutate(., pbl_dif_sq = (pbl_dif^2)) %>%
  #mutate(., pbl_dif_lag = Lag(c(pbl_dif)),shift=1) %>%
  mutate(., F_lag = Lag(c(F)),shift=1) %>%
  #mutate(., prv_log = log(prv)) %>%
  mutate(., F_sq = (F^2)) %>%
  #mutate(., prv_dif = c(NA, diff(prv_log))) %>%
  #mutate(., prv_dif_sq = (prv_dif^2)) %>%
  #mutate(., prv_dif_lag = Lag(c(prv_dif)),shift=1) %>%
  #mutate(., gdp_dif3 = lead(gdp_dif,3)) %>%
  #mutate(., gdp_f3 = lead(gdp_f,3)) %>%
  mutate(., H_hp = hpfilter(H, 100)$cycle) %>%
  mutate(., H_hp_sq = (H_hp^2)) %>%
  mutate(., H_hp_lag = Lag(c(H_hp)),shift=1) %>%
  mutate(., F_hp = hpfilter(F, 100)$cycle) %>%
  mutate(., F_hp_sq = (F_hp^2)) %>%
  mutate(., F_hp_lag = Lag(c(F_hp)),shift=1) %>%
  #mutate(.,prv_pbl = prv_hp*pbl_hp) %>%
  #mutate(., pbl_high = if_else(pbl > 62.8, 1, 0)) %>%
  mutate(., prv_high = if_else(prv > 122.8, 1, 0)) %>%
  ungroup() %>%
  na.omit(.)
View(df)

pdata <- pdata.frame(df_prv, index=c("country","year"), drop.index=TRUE, row.names=TRUE)

gdp_dif3 <-lead(pdata$gdp_dif, k=,3)
gdp_hp3 <-lead(pdata$gdp_hp, k=,3)
df_prv <-cbind(df_prv,gdp_dif3,gdp_hp3)
rownames(df) <- c()  # to delete rownames
df_prv <-na.omit(df_prv)
View(df_prv)

# Low Prvdebt countries
df_prv_low_prv <-subset(df_prv, code %in% c("1","3","5","6","8"))
# AU,FIN,GER,GREE,IT

# High Prvdebt countries
df_prv_high_prv <-subset(df_prv, code %in% c("2","4","7","9","10","11"))
# BEL,FRA,IR,NETH,POR,SPA

res_prv0 <- felm(data = df_prv,
                 formula = gdp_dif3 ~ H - 1| country)

res_prv1 <- felm(data = df_prv,
                 formula = gdp_dif3 ~ F - 1| country)

res_prv2 <- felm(data = df_prv,
                 formula = gdp_dif3 ~ H + F - 1 | country)

res_prv2b <- felm(data = df_prv,
                  formula = gdp_dif3 ~ H_lag + F_lag - 1 | country)

cor(df_prv$H,df_prv$H_lag)
cor(df_prv$F,df_prv$F_lag)

summary(res_prv0)
summary(res_prv1)
summary(res_prv2)
summary(res_prv2b)

# export table (log-difference results) - [code to copy-paste in latex]
stargazer(res_prv0,res_prv1,res_prv2,res_prv2b,
          title = "Variation Table 1 by Batini et al. (2018), Log-difference, gdp",
          align = TRUE)

res_prv3 <- felm(data = df_prv_low_prv,
                 formula = gdp_dif3 ~ H + F - 1 | country)

res_prv4 <- felm(data = df_prv_high_prv,
                 formula = gdp_dif3 ~ H + F - 1 | country)

summary(res_prv3)
summary(res_prv4)

res_prv5 <- felm(data = df_prv,
                 formula = gdp_hp3 ~ H_hp + F_hp - 1 | country)

res_prv6 <- felm(data = df_prv_high_prv,
                 formula = gdp_hp3 ~ H_hp + F_hp - 1 | country)

summary(res_prv5)
summary(res_prv6)

ggplot(data = df_prv, aes(x = year, y = H, colour = country)) +
  geom_line() +
  xlab("Year") + ylab("Real GDP growth") +
  ggtitle("Real GDP growth, Northern countries, percentage")

ggplot(data = df_prv, aes(x = year, y = F, colour = country)) +
  geom_line() +
  xlab("Year") + ylab("Real GDP growth") +
  ggtitle("Real GDP growth, Northern countries, percentage")

#-------------------------------------
# PIIGS vs The Others
#_____________________________________

# PIIGS_dif
piigs_dif <-subset(df, code %in% c("6","7","8","10","11"))

# Others_dif
others_dif <-subset(df, code %in% c("1","2","3","4","5","9"))

# PIIGS_agr
piigs_agr <-subset(df_agr, code %in% c("6","7","8","10","11"))

# Others_agr
others_agr <-subset(df_agr, code %in% c("1","2","3","4","5","9"))


res_piigs_dif <- felm(data = piigs_dif,
                      formula = gdp_dif3 ~ prv + pbl - 1 | country)

res_others_dif <- felm(data = others_dif,
                       formula = gdp_dif3 ~ prv + pbl - 1 | country)

summary(res_piigs_dif)
summary(res_others_dif)

res_piigs_hp <- felm(data = piigs_dif,
                     formula = gdp_hp3 ~ prv_hp + pbl_hp - 1 | country)

res_others_hp <- felm(data = others_dif,
                      formula = gdp_hp3 ~ prv_hp + pbl_hp - 1 | country)

summary(res_piigs_hp)
summary(res_others_hp)

res_piigs_agr <- felm(data = piigs_agr,
                      formula = gdp_agr ~ prv_agr + pbl_agr - 1 | country)

res_others_agr <- felm(data = others_agr,
                       formula = gdp_agr ~ prv_agr + pbl_agr - 1 | country)

summary(res_piigs_agr)
summary(res_others_agr)


# export table (High-Low Private Debt Countries) - [code to copy-paste in latex]
stargazer(res_piigs_dif,res_others_dif,res_piigs_hp,res_others_hp,res_piigs_agr,res_others_agr,
          title = "Variation of Table 2 by Batini et al. (2018), High-Low Private Debt Countries, Growth Rates",
          align = TRUE)

#-------------------------------------
# Analysis Northern-Southern Countries
#_____________________________________

# Northern countries df
df_north <-subset(df, code %in% c("1","2","3","5","9"))
# AU,BEL,FIN,GER,NETH
View(df_north)

# Southern countries df
df_south <-subset(df, code %in% c("4","6","7","8","10","11"))
# FR,GRE,IRE,IT,POR,SPA
View(df_south)

ggplot(data = df_north, aes(x = year, y = gdp_dif, colour = country)) +
  geom_line() +
  xlab("Year") + ylab("Real GDP growth") +
  ggtitle("Real GDP growth, Northern countries, percentage")

ggplot(data = df_south, aes(x = year, y = gdp_dif, colour = country)) +
  geom_line() +
  xlab("Year") + ylab("Real GDP growth") +
  ggtitle("Real GDP growth, Southern countries, percentage")

ggplot(data = df_north, aes(x = year, y = pbl, colour = country)) +
  geom_line() +
  xlab("Year") + ylab("Public debt-to-GDP") +
  ggtitle("Public debt-to-GDP, Northern debt countries, percentage of GDP")

ggplot(data = df_south, aes(x = year, y = pbl, colour = country)) +
  geom_line() +
  xlab("Year") + ylab("Public debt-to-GDP") +
  ggtitle("Public debt-to-GDP, Southern debt countries, percentage of GDP")

ggplot(data = df_north, aes(x = year, y = pbl_dif, colour = country)) +
  geom_line() +
  xlab("Year") + ylab("Public debt-to-GDP change") +
  ggtitle("Public debt-to-GDP change, Northern debt countries, percentage")

ggplot(data = df_south, aes(x = year, y = pbl_dif, colour = country)) +
  geom_line() +
  xlab("Year") + ylab("Public debt-to-GDP change") +
  ggtitle("Public debt-to-GDP change, Southern debt countries, percentage")

ggplot(data = df_north, aes(x = year, y = prv, colour = country)) +
  geom_line() +
  xlab("Year") + ylab("Private debt-to-GDP") +
  ggtitle("Private debt-to-GDP, Northern countries, percentage of GDP")

ggplot(data = df_south, aes(x = year, y = prv, colour = country)) +
  geom_line() +
  xlab("Year") + ylab("Private debt-to-GDP") +
  ggtitle("Private debt-to-GDP, Southern countries, percentage of GDP")

ggplot(data = df_north, aes(x = year, y = prv_dif, colour = country)) +
  geom_line() +
  xlab("Year") + ylab("Private debt-to-GDP change") +
  ggtitle("Private debt-to-GDP change, Northern countries, percentage")

ggplot(data = df_south, aes(x = year, y = prv_dif, colour = country)) +
  geom_line() +
  xlab("Year") + ylab("Private debt-to-GDP change") +
  ggtitle("Private debt-to-GDP change, Southern countries, percentage")

qplot(pbl,data=df_north,fill=country)
qplot(pbl,data=df_south,fill=country)
qplot(pbl_dif,data=df_north,fill=country)
qplot(pbl_dif,data=df_south,fill=country)
qplot(prv,data=df_north,fill=country)
qplot(prv,data=df_south,fill=country)
qplot(prv_dif,data=df_north,fill=country)
qplot(prv_dif,data=df_south,fill=country)

# Baselinee Regressions

# Northern countries df
df_north <-subset(df, code %in% c("1","2","3","5","7","9"))
# AU,BEL,IRE,FIN,GER,NETH
View(df_north)

# Southern countries df
df_south <-subset(df, code %in% c("4","6","8","10","11"))
# FR,GRE,IT,POR,SPA
View(df_south)

res_north_dif <- felm(data = df_north,
                      formula = gdp_dif3 ~ prv + pbl -1 |country)

res_south_dif <-felm(data = df_south,
                     formula = gdp_dif3 ~ prv + pbl - 1 | country)

res_north_hp <- felm(data = df_north,
                     formula = gdp_hp3 ~ prv_hp + pbl_hp - 1 | country)

res_south_hp <- felm(data = df_south,
                     formula = gdp_hp3 ~ prv_hp + pbl_hp - 1 | country)

summary(res_north_dif)
summary(res_south_dif)
summary(res_north_hp)
summary(res_south_hp)

res_north_agr <- felm(data = df_north_agr,
                      formula = gdp_agr ~ prv_agr + pbl_agr -1 |country)

res_south_agr <-felm(data = df_south_agr,
                     formula = gdp_agr ~ prv_agr + pbl_agr - 1 | country)

summary(res_north_agr)
summary(res_south_agr)

# export table (Northern-Southern Countries) - [code to copy-paste in latex]
stargazer(res_north_dif,res_south_dif,res_north_hp,res_south_hp,res_north_agr,res_south_agr,
          title = "Variation of Table 1 by Batini et al. (2018), Northen-Southern Countries, Hp Filtered",
          align = TRUE)

# Northern countries df_gr
df_gr_north <-subset(df_gr, code %in% c("1","2","3","5","9"))
View(df_gr_north)

# Southern countries df_gr
df_gr_south <-subset(df_gr, code %in% c("4","6","7","8","10","11"))
View(df_gr_south)

res_gr_north <- felm(data = df_gr_north,
                     formula = gdp_gr ~ prv_gr + pbl_gr - 1 | country)

res_gr_south <- felm(data = df_gr_south,
                     formula = gdp_gr ~ prv_gr + pbl_gr - 1 | country)

summary(res_gr_north)
summary(res_gr_south)

# export table (Northern-Southern Countries) - [code to copy-paste in latex]
stargazer(res_north_hp,res_south_hp,res_north_dif,res_south_dif,res_gr_north,res_gr_south,
          title = "Variation of Table 1 by Batini et al. (2018), Northen-Southern Countries, Growth Rates",
          align = TRUE)
