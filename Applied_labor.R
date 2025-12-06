library(plotly)
library(lubridate)
library(stringr)
library(tidyr)
library(dplyr)
library(readxl)
library(ggplot2)
library(zoo)
library(stargazer)
library(car)
library(tseries)
library(lmtest)
library(sandwich)

Ledige_stillinger <- read_xlsx("/Users/annaengelbrechtsen/Documents/Københavnsuniversitet Økonomisk videnskab/3. semester/Applied labour economics/R-kode/Ledige_stillinger.xlsx")
Opholdstilladelse <- read_xlsx("/Users/annaengelbrechtsen/Documents/Københavnsuniversitet Økonomisk videnskab/3. semester/Applied labour economics/R-kode/Opholdstillaldelse.xlsx")
Produktion_be <- read_xlsx("/Users/annaengelbrechtsen/Documents/Københavnsuniversitet Økonomisk videnskab/3. semester/Applied labour economics/R-kode/Produktionsbegrænsninger.xlsx")
BNP <- read_xlsx("/Users/annaengelbrechtsen/Documents/Københavnsuniversitet Økonomisk videnskab/3. semester/Applied labour economics/R-kode/BNP.xlsx")
Konjunktur <- read_xlsx("/Users/annaengelbrechtsen/Documents/Københavnsuniversitet Økonomisk videnskab/3. semester/Applied labour economics/R-kode/konjunktur.xlsx")
Inflation <- read_xlsx("/Users/annaengelbrechtsen/Documents/Københavnsuniversitet Økonomisk videnskab/3. semester/Applied labour economics/R-kode/inflation.xlsx")
Vestlige <- read_xlsx("/Users/annaengelbrechtsen/Documents/Københavnsuniversitet Økonomisk videnskab/3. semester/Applied labour economics/R-kode/non_western-kopi.xlsx")

## Forberedelse af data ## 
Ledige_stillinger_filter <- Ledige_stillinger %>%
  pivot_longer(cols = -Kategori,        
               names_to = "ref_date",      
               values_to = "Ledige_stillinger") %>%
  filter(Kategori == "I alt") %>% 
  select(-Kategori) %>% 
  collect()


Opholdstilladelse_filter <- Opholdstilladelse %>%
  filter(!Opholdstilladelse %in% c("Familiesammenføring, Mindreårige børn, Refererer til flygtning", 
                                   "Familiesammenføring, Mindreårige børn, Refererer til andre end flygtning", 
                                   "Familiesammenføring, Mindreårige børn, Uoplyst referenceperson",
                                   "EU/EØS, Uddannelse", "Studie mv., Øvrige grunde", "EU/EØS, Uddannelse",
                                   "Studie mv., Praktikanter", "Studie mv., Uddannelse", "Studie mv., Au pair",
                                   "Det øvrige opholdsområde, Adoption", "EU/EØS, Familiemedlemmer")) %>% 
  pivot_longer(cols = -Opholdstilladelse, names_to = "ref_date", values_to = "Ophold") %>% 
  select(ref_date, Ophold) %>% 
  summarise(Ophold = sum(Ophold, na.rm =T)/1e3, .by = ref_date) %>% collect()

Opholdstilladelse_filter$dif_Ophold <- c(NA, diff(Opholdstilladelse_filter$Ophold))

Opholdstilladelse_filter <- Opholdstilladelse_filter %>% 
  filter(!is.na(dif_Ophold)) %>% 
  arrange(ref_date) %>% 
  mutate(Ophold_lag1 = dplyr::lag(dif_Ophold, 1),
         Ophold_lag2 = dplyr::lag(dif_Ophold, 2),
         Ophold_lag3 = dplyr::lag(dif_Ophold, 3),
         Ophold_lag4 = dplyr::lag(dif_Ophold, 4),
         Ophold_lag5 = dplyr::lag(dif_Ophold, 5),
         Ophold_lag6 = dplyr::lag(dif_Ophold, 6),
         Ophold_lag7 = dplyr::lag(dif_Ophold, 7),
         Ophold_lag8 = dplyr::lag(dif_Ophold, 8)) %>%
  collect()


Produktion_filter <- Produktion_be %>%
  pivot_longer(cols = -Industri,        
               names_to = "ref_date",      
               values_to = "Produktion") %>%
  mutate(ref_date = as.yearmon(ref_date, "%YM%m"),
         ref_date = paste0(format(ref_date, "%Y"), "K", ceiling(as.numeric(format(ref_date, "%m")) / 3))) %>%
  summarise(Produktion = sum(Produktion, na.rm = T), .by = c(ref_date, Industri)) %>% 
  filter(Industri == "BC Råstofindvinding og industri") %>%
  select(-Industri) %>% 
  collect()

Produktion_filter$dif_Produktion <- c(NA, diff(Produktion_filter$Produktion))

Konjunktur_filter <- Konjunktur %>% 
  mutate(ref_date = as.yearmon(ref_date, "%YM%m"),
         ref_date = paste0(format(ref_date, "%Y"), "K", ceiling(as.numeric(format(ref_date, "%m")) / 3))) %>% 
  summarise(konjunktur = sum(konjunktur, na.rm = T)/3, .by = ref_date) %>% 
  collect()
  
Inflation_filter <- Inflation %>% 
  mutate(ref_date = as.yearmon(ref_date, "%YM%m"),
         ref_date = paste0(format(ref_date, "%Y"), "K", ceiling(as.numeric(format(ref_date, "%m")) / 3))) %>% 
  summarise(Inflation = sum(Forbrugerprisindeks, na.rm = T)/3, .by = ref_date) %>% 
  collect()

Inflation_filter$dif_Inflation <- c(NA, diff(Inflation_filter$Inflation))
BNP$dif_BNP <- c(NA, diff(BNP$BNP))

Samlet_produktion <- Inflation_filter %>% 
  left_join(BNP, by = "ref_date") %>% 
  left_join(Konjunktur_filter, by = "ref_date") %>% 
  left_join(Ledige_stillinger_filter, by = "ref_date") %>% 
  left_join(Opholdstilladelse_filter, by = "ref_date") %>% 
  left_join(Produktion_filter, by = "ref_date") %>% 
  mutate(ref_date = as.yearqtr(gsub("K", " Q", ref_date), format = "%Y Q%q")) %>%
  filter(ref_date >= as.yearqtr("2005 Q2"), 
         ref_date <= as.yearqtr("2023 Q3")) %>%
  collect()

Samlet_Ledige <- Inflation_filter %>% 
  left_join(BNP, by = "ref_date") %>% 
  left_join(Konjunktur_filter, by = "ref_date") %>% 
  left_join(Ledige_stillinger_filter, by = "ref_date") %>% 
  left_join(Opholdstilladelse_filter, by = "ref_date") %>% 
  left_join(Produktion_filter, by = "ref_date") %>% 
  mutate(ref_date = as.yearqtr(gsub("K", " Q", ref_date), format = "%Y Q%q")) %>%
  filter(ref_date >= as.yearqtr("2010 Q1"), 
         ref_date <= as.yearqtr("2023 Q3")) %>%
  collect()

### Analyse af alt samlet ###
## Base
OLS_produktion_Base <- lm(dif_Produktion ~ dif_Ophold,
                     data = Samlet_produktion)
coeftest(OLS_produktion_Base, vcov = NeweyWest(OLS_produktion_Base, lag = 0, prewhite = FALSE))
summary(OLS_produktion_Base)
## BIC
OLS_produktion_BIC <- lm(dif_Produktion ~ dif_Ophold + dif_Inflation + konjunktur + dif_BNP ,
                     data = Samlet_produktion)
coeftest(OLS_produktion_BIC, vcov = NeweyWest(OLS_produktion_BIC, lag = 0, prewhite = FALSE))
summary(OLS_produktion_BIC)
## AIC
OLS_produktion_AIC <- lm(dif_Produktion ~ dif_Ophold + Ophold_lag1 + Ophold_lag2 + Ophold_lag3 +
                       Ophold_lag4 + Ophold_lag5 + Ophold_lag6 + Ophold_lag7 
                     + dif_Inflation + konjunktur + dif_BNP ,
                     data = Samlet_produktion)
coeftest(OLS_produktion_AIC, vcov = NeweyWest(OLS_produktion_AIC, lag = 7, prewhite = FALSE))
summary(OLS_produktion_AIC)
# Newey = autocorrelation and heteroskedasticity



# Definer de relevante koefficienter fra AIC-modellen
vars <- c("dif_Ophold", "Ophold_lag1", "Ophold_lag2", "Ophold_lag3",
          "Ophold_lag4", "Ophold_lag5", "Ophold_lag6", "Ophold_lag7")

# Hent punktestimat
coefs <- coef(OLS_produktion_AIC)[vars]
cum_est <- sum(coefs)

# Hent vcov matrix med Newey-West standardfejl (samme som du brugte i coeftest)
vc <- NeweyWest(OLS_produktion_AIC, lag = 7, prewhite = FALSE)
vc_sub <- vc[vars, vars]

# Beregn standardfejl via delta-metoden
w <- rep(1, length(vars))
cum_se <- sqrt(as.numeric(t(w) %*% vc_sub %*% w))

# Beregn t-stat, p-værdi og 95% CI
tstat <- cum_est / cum_se
pval <- 2 * (1 - pnorm(abs(tstat)))
ci_lower <- cum_est - 1.96 * cum_se
ci_upper <- cum_est + 1.96 * cum_se

# Output
list(cumulative_effect = cum_est,
     standard_error = cum_se,
     t_stat = tstat,
     p_value = pval,
     CI_95 = c(ci_lower, ci_upper))
























## Samlet insignifikant?
linearHypothesis(OLS_produktion, c("Ophold_lag1 = 0", "Ophold_lag2 = 0", "Ophold_lag3 = 0", "Ophold_lag4 = 0", 
                                   "Ophold_lag5 = 0", "Ophold_lag6 = 0", "Ophold_lag7 = 0"))

# Sammenlign AIC og BIC
mod_lag0 <- lm(dif_Produktion ~ dif_Ophold + dif_BNP + dif_Inflation + konjunktur, data = Samlet_produktion)
mod_lag1 <- lm(dif_Produktion ~ dif_Ophold + Ophold_lag1 + dif_BNP + dif_Inflation + konjunktur, data = Samlet_produktion)
mod_lag2 <- lm(dif_Produktion ~ dif_Ophold + Ophold_lag1 + Ophold_lag2 + dif_BNP + dif_Inflation + konjunktur, data = Samlet_produktion)
mod_lag3 <- lm(dif_Produktion ~ dif_Ophold + Ophold_lag1 + Ophold_lag2 + Ophold_lag3 + dif_BNP + dif_Inflation + konjunktur, data = Samlet_produktion)
mod_lag4 <- lm(dif_Produktion ~ dif_Ophold + Ophold_lag1 + Ophold_lag2 + Ophold_lag3 + Ophold_lag4 + dif_BNP + dif_Inflation + konjunktur, data = Samlet_produktion)
mod_lag5 <- lm(dif_Produktion ~ dif_Ophold + Ophold_lag1 + Ophold_lag2 + Ophold_lag3 + Ophold_lag4 + Ophold_lag5+ dif_BNP + dif_Inflation + konjunktur, data = Samlet_produktion)
mod_lag6 <- lm(dif_Produktion ~ dif_Ophold + Ophold_lag1 + Ophold_lag2 + Ophold_lag3 + Ophold_lag4 + Ophold_lag5 + Ophold_lag6 + dif_BNP + dif_Inflation + konjunktur, data = Samlet_produktion)
mod_lag7 <- lm(dif_Produktion ~ dif_Ophold + Ophold_lag1 + Ophold_lag2 + Ophold_lag3 + Ophold_lag4 + Ophold_lag5 + Ophold_lag6 + Ophold_lag7 + dif_BNP + dif_Inflation + konjunktur, data = Samlet_produktion)
mod_lag8 <- lm(dif_Produktion ~ dif_Ophold + Ophold_lag1 + Ophold_lag2 + Ophold_lag3 + Ophold_lag4 + Ophold_lag5 + Ophold_lag6 + Ophold_lag7 + Ophold_lag8 + dif_BNP + dif_Inflation + konjunktur, data = Samlet_produktion)

AIC(mod_lag0, mod_lag1, mod_lag2, mod_lag3, mod_lag4, mod_lag5, mod_lag6, mod_lag7, mod_lag8) 
BIC(mod_lag0, mod_lag1, mod_lag2, mod_lag3, mod_lag4, mod_lag5, mod_lag6, mod_lag7, mod_lag8)

# Lav summary statistics tabel
summary_vars <- Samlet_produktion[, c("Produktion", "Ophold", "BNP", "Inflation", "konjunktur")]
summary_vars <- summary_vars[complete.cases(summary_vars), ]
summary_vars <- as.data.frame(summary_vars)
stargazer(summary_vars, type = "text", title = "Summary statistics", digits = 2)
vif(OLS_produktion)

### stationaryity test
adf.test(Samlet_produktion$dif_Produktion, alternative = "stationary") 
adf.test(Samlet_produktion$dif_Ophold, alternative = "stationary") 
adf.test(Samlet_produktion$dif_BNP, alternative = "stationary")  
adf.test(Samlet_produktion$dif_Inflation, alternative = "stationary") 
adf.test(Samlet_produktion$konjunktur, alternative = "stationary")
adf.test(Ledige_stillinger_filter$Ledige_stillinger, alternative = "stationary")

### normality 
resid <- residuals(OLS_produktion_Base)
shapiro.test(resid)
resid <- residuals(OLS_produktion_BIC)
shapiro.test(resid)
resid <- residuals(OLS_produktion_AIC)
shapiro.test(resid)

### Hetero
bptest(OLS_produktion_Base)
bptest(OLS_produktion_BIC)
bptest(OLS_produktion_AIC)

## autocorrelation 
bgtest(OLS_produktion_Base, order = 0)
bgtest(OLS_produktion_BIC, order = 0) 
bgtest(OLS_produktion_AIC, order = 7)


## Analyse med opdeling op grundlag for opholdstilladelse ### 
Refugees <- Opholdstilladelse %>% 
  pivot_longer(cols = -Opholdstilladelse, names_to = "ref_date", values_to = "Ophold") %>% 
  filter(Opholdstilladelse %in% c("Asyl, Flygtningestatus", "Asyl, Andet grundlag", 
                                  "Familiesammenføring, Ægteskab eller fast samlivsforhold, Referer til flygtninge",
                                  "Familiesammenføring, Andre familiemedlemmer, Referer til flygtninge")) %>%
  summarise(Refugees = sum(Ophold, na.rm = T), .by = ref_date) %>% 
  collect()

Refugees$dif_Refugees <- c(NA, diff(Refugees$Refugees))
  
Familiy_exl_refugees <- Opholdstilladelse %>% 
  pivot_longer(cols = -Opholdstilladelse, names_to = "ref_date", values_to = "Ophold") %>% 
  filter(Opholdstilladelse %in% c("Familiesammenføring, Ægteskab eller fast samlivsforhold, Referer til udlæninge, men ikke flygtninge",
                                  "Familiesammenføring, Ægteskab eller fast samlivsforhold, Referer til dansk/nordisk person",
                                  "Familiesammenføring, Ægteskab eller fast samlivsforhold, Uoplyst referenceperson",
                                  "Familiesammenføring, Andre familiemedlemmer, Referer til dansk/nordisk person", 
                                  "Familiesammenføring, Andre familiemedlemmer, Uoplyst referenceperson",
                                  "EU/EØS, Familiemedlemmer")) %>% 
  summarise(Familiy_exl_refugees = sum(Ophold, na.rm = T), .by = ref_date) %>% collect()

Familiy_exl_refugees$dif_Familiy_exl_refugees <- c(NA, diff(Familiy_exl_refugees$Familiy_exl_refugees))

Familiy_exl_refugees <- Familiy_exl_refugees %>% 
  arrange(ref_date) %>% 
  mutate(F_lag1 = dplyr::lag(dif_Familiy_exl_refugees, 1),
         F_lag2 = dplyr::lag(dif_Familiy_exl_refugees, 2),
         F_lag3 = dplyr::lag(dif_Familiy_exl_refugees, 3),
         F_lag4 = dplyr::lag(dif_Familiy_exl_refugees, 4),
         F_lag5 = dplyr::lag(dif_Familiy_exl_refugees, 5),
         F_lag6 = dplyr::lag(dif_Familiy_exl_refugees, 6),
         F_lag7 = dplyr::lag(dif_Familiy_exl_refugees, 7),
         F_lag8 = dplyr::lag(dif_Familiy_exl_refugees, 8)) %>%
  collect()

Work <- Opholdstilladelse %>% 
  pivot_longer(cols = -Opholdstilladelse, names_to = "ref_date", values_to = "Ophold") %>% 
  filter(Opholdstilladelse %in% c("Erhverv", "EU/EØS, Lønarbejde")) %>%
  summarise(Work = sum(Ophold, na.rm = T), .by = ref_date) %>% collect()

Work$dif_Work <- c(NA, diff(Work$Work))

Work <- Work %>% 
  arrange(ref_date) %>% 
  mutate(Work_lag1 = dplyr::lag(dif_Work, 1),
         Work_lag2 = dplyr::lag(dif_Work, 2),
         Work_lag3 = dplyr::lag(dif_Work, 3),
         Work_lag4 = dplyr::lag(dif_Work, 4),
         Work_lag5 = dplyr::lag(dif_Work, 5),
         Work_lag6 = dplyr::lag(dif_Work, 6),
         Work_lag7 = dplyr::lag(dif_Work, 7),
         Work_lag8 = dplyr::lag(dif_Work, 8)) %>%
  collect()

Other <- Opholdstilladelse %>% 
  pivot_longer(cols = -Opholdstilladelse, names_to = "ref_date", values_to = "Ophold") %>% 
  filter(Opholdstilladelse %in% c("Det øvrige opholdsomrøde, Øvrige grunde", "EU/EØS, Øvrige grunde")) %>%
  summarise(Other = sum(Ophold, na.rm = T), .by = ref_date) %>% collect()

Other$dif_Other <- c(NA, diff(Other$Other))

Other <- Other %>% 
  arrange(ref_date) %>% 
  mutate(Other_lag1 = dplyr::lag(dif_Other, 1),
         Other_lag2 = dplyr::lag(dif_Other, 2),
         Other_lag3 = dplyr::lag(dif_Other, 3),
         Other_lag4 = dplyr::lag(dif_Other, 4),
         Other_lag5 = dplyr::lag(dif_Other, 5),
         Other_lag6 = dplyr::lag(dif_Other, 6),
         Other_lag7 = dplyr::lag(dif_Other, 7),
         Other_lag8 = dplyr::lag(dif_Other, 8)) %>%
  collect()

Ukraine <- Opholdstilladelse %>% 
  pivot_longer(cols = -Opholdstilladelse, names_to = "ref_date", values_to = "Ophold") %>% 
  filter(Opholdstilladelse == "Ukraine (særlov)") %>%
  summarise(Ukraine = sum(Ophold, na.rm = T), .by = ref_date) %>% collect()

Ukraine$dif_Ukraine <- c(NA, diff(Ukraine$Ukraine))

Ukraine <- Ukraine %>% 
  left_join(Refugees, by = "ref_date") %>% 
  left_join(Familiy_exl_refugees, by = "ref_date") %>% 
  left_join(Work, by = "ref_date") %>% 
  left_join(Other, by = "ref_date") %>% 
  mutate(ref_date = as.yearqtr(gsub("K", " Q", ref_date), format = "%Y Q%q")) %>%
  collect()


Samlet_produktion_ophold_work <- Samlet_produktion %>% 
  left_join(Ukraine, by = "ref_date") %>% 
  mutate(Work1 = dif_Work + dif_Familiy_exl_refugees + dif_Other,
         Work1_lag1 = Work_lag1 + Other_lag1, F_lag1,
         Work1_lag2 = Work_lag2 + Other_lag2, F_lag2,
         Work1_lag3 = Work_lag3 + Other_lag3, F_lag3,
         Work1_lag4 = Work_lag4 + Other_lag4, F_lag4,
         Work1_lag5 = Work_lag5 + Other_lag5, F_lag5,
         Work1_lag6 = Work_lag6 + Other_lag6, F_lag6,
         Work1_lag7 = Work_lag7 + Other_lag7, F_lag7,
         Work1_lag8 = Work_lag8 + Other_lag8, F_lag8) %>% 
  collect()

adf.test(Samlet_produktion_ophold_work$Work1, alternative = "stationary") 

### hvor mange lags ? 
mod_lag0 <- lm(dif_Produktion ~ Work1 + dif_BNP + dif_Inflation + konjunktur, data = Samlet_produktion_ophold_work)
mod_lag1 <- lm(dif_Produktion ~ Work1 + Work1_lag1 + dif_BNP + dif_Inflation + konjunktur, data = Samlet_produktion_ophold_work)
mod_lag2 <- lm(dif_Produktion ~ Work1 + Work1_lag1 + Work1_lag2 + dif_BNP + dif_Inflation + konjunktur, data = Samlet_produktion_ophold_work)
mod_lag3 <- lm(dif_Produktion ~ Work1 + Work1_lag1 + Work1_lag2 + Work1_lag3 + dif_BNP + dif_Inflation + konjunktur, data = Samlet_produktion_ophold_work)
mod_lag4 <- lm(dif_Produktion ~ Work1 + Work1_lag1 + Work1_lag2 + Work1_lag3 + Work1_lag4 + dif_BNP + dif_Inflation + konjunktur, data = Samlet_produktion_ophold_work)
mod_lag5 <- lm(dif_Produktion ~ Work1 + Work1_lag1 + Work1_lag2 + Work1_lag3 + Work1_lag4 + Work1_lag5+ dif_BNP + dif_Inflation + konjunktur, data = Samlet_produktion_ophold_work)
mod_lag6 <- lm(dif_Produktion ~ Work1 + Work1_lag1 + Work1_lag2 + Work1_lag3 + Work1_lag4 + Work1_lag5 + Work1_lag6 + dif_BNP + dif_Inflation + konjunktur, data = Samlet_produktion_ophold_work)
mod_lag7 <- lm(dif_Produktion ~ Work1 + Work1_lag1 + Work1_lag2 + Work1_lag3 + Work1_lag4 + Work1_lag5 + Work1_lag6 + Work1_lag7 + dif_BNP + dif_Inflation + konjunktur, data = Samlet_produktion_ophold_work)
mod_lag8 <- lm(dif_Produktion ~ Work1 + Work1_lag1 + Work1_lag2 + Work1_lag3 + Work1_lag4 + Work1_lag5 + Work1_lag6 + Work1_lag7 + Work1_lag8 + dif_BNP + dif_Inflation + konjunktur, data = Samlet_produktion_ophold_work)
# Sammenlign AIC og BIC
AIC(mod_lag0, mod_lag1, mod_lag2, mod_lag3, mod_lag4, mod_lag5, mod_lag6, mod_lag7, mod_lag8) 
BIC(mod_lag0, mod_lag1, mod_lag2, mod_lag3, mod_lag4, mod_lag5, mod_lag6, mod_lag7, mod_lag8)

### Analyse af alt samlet ###
## Base
OLS_work_Base <- lm(dif_Produktion ~ Work1,
                          data = Samlet_produktion_ophold_work)
coeftest(OLS_work_Base, vcov = NeweyWest(OLS_work_Base, lag = 0, prewhite = FALSE))
summary(OLS_work_Base)

## controls 
OLS_work_controls <- lm(dif_Produktion ~ Work1 + dif_Inflation + konjunktur + dif_BNP ,
                         data = Samlet_produktion_ophold_work)
coeftest(OLS_work_controls, vcov = NeweyWest(OLS_work_controls, lag = 0, prewhite = FALSE))
summary(OLS_work_controls)

## BIC
OLS_work_BIC <- lm(dif_Produktion ~ Work1 + Work1_lag1 + Work1_lag2 + Work1_lag3 + 
                     Work1_lag4 + Work1_lag5 + Work1_lag6 + 
                     dif_Inflation + konjunktur + dif_BNP,
                         data = Samlet_produktion_ophold_work)
coeftest(OLS_work_BIC, vcov = NeweyWest(OLS_work_BIC, lag = 6, prewhite = FALSE))
summary(OLS_work_BIC)
## AIC
OLS_work_AIC <- lm(dif_Produktion ~ Work1 + Work1_lag1 + Work1_lag2 + Work1_lag3 + 
                     Work1_lag4 + Work1_lag5 + Work1_lag6 + Work1_lag7 +
                     Work1_lag8 + dif_Inflation + konjunktur + dif_BNP,
                         data = Samlet_produktion_ophold_work)
coeftest(OLS_work_AIC, vcov = NeweyWest(OLS_work_AIC, lag = 8, prewhite = FALSE))
summary(OLS_work_AIC)
# Newey = autocorrelation and heteroskedasticity
## Samlet insignifikant?
linearHypothesis(OLS_work_BIC, c("Work1_lag1 = 0", "Work1_lag2 = 0", "Work1_lag3 = 0",
                                 "Work1_lag4 = 0", "Work1_lag5 = 0", "Work1_lag6 = 0"))

linearHypothesis(OLS_work_AIC, c("Work1_lag1 = 0", "Work1_lag2 = 0", "Work1_lag3 = 0",
                                 "Work1_lag4 = 0", "Work1_lag5 = 0", "Work1_lag6 = 0",
                                 "Work1_lag7 = 0", "Work1_lag8 = 0"))
### normality 
resid <- residuals(OLS_work_Base)
shapiro.test(resid)
resid <- residuals(OLS_work_controls)
shapiro.test(resid)
resid <- residuals(OLS_work_BIC)
shapiro.test(resid)
resid <- residuals(OLS_work_AIC)
shapiro.test(resid)

### Hetero
bptest(OLS_work_Base)
bptest(OLS_work_controls)
bptest(OLS_work_BIC)
bptest(OLS_work_AIC)

## autocorrelation 
bgtest(OLS_work_BIC, order = 6) 
bgtest(OLS_work_AIC, order = 8)


### Vacancies ### 
Samlet_ledig <- Inflation_filter %>% 
  left_join(BNP, by = "ref_date") %>% 
  left_join(Konjunktur_filter, by = "ref_date") %>% 
  left_join(Opholdstilladelse_filter, by = "ref_date") %>% 
  left_join(Produktion_filter, by = "ref_date") %>% 
  mutate(ref_date = as.yearqtr(gsub("K", " Q", ref_date), format = "%Y Q%q")) %>%
  filter(ref_date >= as.yearqtr("2010 Q1"), 
         ref_date <= as.yearqtr("2023 Q3")) %>%
  collect()

Ledige_stillinger_filter <- Ledige_stillinger %>%
  pivot_longer(cols = -Kategori,        
               names_to = "ref_date",      
               values_to = "Ledige_stillinger") %>%
  pivot_wider(values_from = Ledige_stillinger, 
              names_from = Kategori) %>%  
  mutate(ref_date = as.yearqtr(gsub("K", " Q", ref_date), format = "%Y Q%q")) %>%
filter(ref_date >= as.yearqtr("2010 Q1"), 
       ref_date <= as.yearqtr("2023 Q3")) %>%collect()

Samlet_ledig$dif_ialt <- c(NA, diff(Ledige_stillinger_filter$`I alt`))
Samlet_ledig$dif_industri <- c(NA, diff(Ledige_stillinger_filter$`2 Industri, råstofindvinding og forsyningsvirksomhed`))
Samlet_ledig$dif_bygge <- c(NA, diff(Ledige_stillinger_filter$`3 Bygge og anlæg`))
Samlet_ledig$dif_Handel <- c(NA, diff(Ledige_stillinger_filter$`4 Handel og transport mv.`))
Samlet_ledig$dif_infor <- c(NA, diff(Ledige_stillinger_filter$`5 Information og kommunikation`))
Samlet_ledig$dif_fin <- c(NA, diff(Ledige_stillinger_filter$`6-7 Finansiering, forsikring og ejendomshandel`))
Samlet_ledig$dif_Erhverv <- c(NA, diff(Ledige_stillinger_filter$`8 Erhvervsservice`))

### Analyse af alt samlet 7 lags ###
## 1
Samlet_ledig_BIC1 <- lm(dif_ialt ~ dif_Ophold + Ophold_lag1 + Ophold_lag2 + Ophold_lag3  +
                       Ophold_lag4 + Ophold_lag5 + Ophold_lag6 + Ophold_lag7 +  dif_Inflation + konjunktur + dif_BNP ,
                         data = Samlet_ledig)
coeftest(Samlet_ledig_BIC1, vcov = NeweyWest(Samlet_ledig_BIC1, lag = 7, prewhite = FALSE))
summary(Samlet_ledig_BIC1)

## 2
Samlet_ledig_BIC2 <- lm(dif_industri ~ dif_Ophold + Ophold_lag1 + Ophold_lag2 + Ophold_lag3  +
                          Ophold_lag4 + Ophold_lag5 + Ophold_lag6 + Ophold_lag7 +
                          dif_Inflation + konjunktur + dif_BNP ,
                       data = Samlet_ledig)
coeftest(Samlet_ledig_BIC2, vcov = NeweyWest(Samlet_ledig_BIC2, lag = 7, prewhite = FALSE))
summary(Samlet_ledig_BIC2)

## 3
Samlet_ledig_BIC3 <- lm(dif_bygge ~ dif_Ophold + Ophold_lag1 + Ophold_lag2 + Ophold_lag3  +
                          Ophold_lag4 + Ophold_lag5 + Ophold_lag6 + Ophold_lag7 +
                          dif_Inflation + konjunktur + dif_BNP ,
                       data = Samlet_ledig)
coeftest(Samlet_ledig_BIC3, vcov = NeweyWest(Samlet_ledig_BIC3, lag = 7, prewhite = FALSE))
summary(Samlet_ledig_BIC3)

## 4
Samlet_ledig_BIC4 <- lm(dif_Handel ~ dif_Ophold + Ophold_lag1 + Ophold_lag2 + Ophold_lag3  +
                          Ophold_lag4 + Ophold_lag5 + Ophold_lag6 + Ophold_lag7 +
                          dif_Inflation + konjunktur + dif_BNP ,
                       data = Samlet_ledig)
coeftest(Samlet_ledig_BIC4, vcov = NeweyWest(Samlet_ledig_BIC4, lag = 7, prewhite = FALSE))
summary(Samlet_ledig_BIC4)

## 5
Samlet_ledig_BIC5 <- lm(dif_infor ~ dif_Ophold + Ophold_lag1 + Ophold_lag2 + Ophold_lag3  +
                          Ophold_lag4 + Ophold_lag5 + Ophold_lag6 + Ophold_lag7 +
                          dif_Inflation + konjunktur + dif_BNP ,
                       data = Samlet_ledig)
coeftest(Samlet_ledig_BIC5, vcov = NeweyWest(Samlet_ledig_BIC5, lag = 7, prewhite = FALSE))
summary(Samlet_ledig_BIC5)

## 6
Samlet_ledig_BIC6 <- lm(dif_fin ~ dif_Ophold + Ophold_lag1 + Ophold_lag2 + Ophold_lag3  +
                          Ophold_lag4 + Ophold_lag5 + Ophold_lag6 + Ophold_lag7 +
                          dif_Inflation + konjunktur + dif_BNP ,
                       data = Samlet_ledig)
coeftest(Samlet_ledig_BIC6, vcov = NeweyWest(Samlet_ledig_BIC6, lag = 7, prewhite = FALSE))
summary(Samlet_ledig_BIC6)

## 7
Samlet_ledig_BIC7 <- lm(dif_Erhverv ~ dif_Ophold + Ophold_lag1 + Ophold_lag2 + Ophold_lag3  +
                          Ophold_lag4 + Ophold_lag5 + Ophold_lag6 + Ophold_lag7 +
                          dif_Inflation + konjunktur + dif_BNP ,
                       data = Samlet_ledig)
coeftest(Samlet_ledig_BIC7, vcov = NeweyWest(Samlet_ledig_BIC7, lag = 7, prewhite = FALSE))
summary(Samlet_ledig_BIC7)


## Samlet insignifikant?
linearHypothesis(Samlet_ledig_BI1, c("Ophold_lag1 = 0", "Ophold_lag2 = 0", "Ophold_lag3 = 0", "Ophold_lag4 = 0", 
                                   "Ophold_lag5 = 0", "Ophold_lag6 = 0", "Ophold_lag7 = 0"))
linearHypothesis(Samlet_ledig_BIC2, c("Ophold_lag1 = 0", "Ophold_lag2 = 0", "Ophold_lag3 = 0", "Ophold_lag4 = 0", 
                                 "Ophold_lag5 = 0", "Ophold_lag6 = 0", "Ophold_lag7 = 0"))
linearHypothesis(Samlet_ledig_BIC3, c("Ophold_lag1 = 0", "Ophold_lag2 = 0", "Ophold_lag3 = 0", "Ophold_lag4 = 0", 
                                 "Ophold_lag5 = 0", "Ophold_lag6 = 0", "Ophold_lag7 = 0"))
linearHypothesis(Samlet_ledig_BIC4, c("Ophold_lag1 = 0", "Ophold_lag2 = 0", "Ophold_lag3 = 0", "Ophold_lag4 = 0", 
                                 "Ophold_lag5 = 0", "Ophold_lag6 = 0", "Ophold_lag7 = 0"))
linearHypothesis(Samlet_ledig_BIC5, c("Ophold_lag1 = 0", "Ophold_lag2 = 0", "Ophold_lag3 = 0", "Ophold_lag4 = 0", 
                                 "Ophold_lag5 = 0", "Ophold_lag6 = 0", "Ophold_lag7 = 0"))
linearHypothesis(Samlet_ledig_BIC6, c("Ophold_lag1 = 0", "Ophold_lag2 = 0", "Ophold_lag3 = 0", "Ophold_lag4 = 0", 
                                 "Ophold_lag5 = 0", "Ophold_lag6 = 0", "Ophold_lag7 = 0"))
linearHypothesis(Samlet_ledig_BIC7, c("Ophold_lag1 = 0", "Ophold_lag2 = 0", "Ophold_lag3 = 0", "Ophold_lag4 = 0", 
                                 "Ophold_lag5 = 0", "Ophold_lag6 = 0", "Ophold_lag7 = 0"))

# Sammenlign AIC og BIC
mod_lag0 <- lm(dif_Erhverv ~ dif_Ophold + dif_BNP + dif_Inflation + konjunktur, data = Samlet_ledig)
mod_lag1 <- lm(dif_Erhverv ~ dif_Ophold + Ophold_lag1 + dif_BNP + dif_Inflation + konjunktur, data = Samlet_ledig)
mod_lag2 <- lm(dif_Erhverv ~ dif_Ophold + Ophold_lag1 + Ophold_lag2 + dif_BNP + dif_Inflation + konjunktur, data = Samlet_ledig)
mod_lag3 <- lm(dif_Erhverv ~ dif_Ophold + Ophold_lag1 + Ophold_lag2 + Ophold_lag3 + dif_BNP + dif_Inflation + konjunktur, data = Samlet_ledig)
mod_lag4 <- lm(dif_Erhverv ~ dif_Ophold + Ophold_lag1 + Ophold_lag2 + Ophold_lag3 + Ophold_lag4 + dif_BNP + dif_Inflation + konjunktur, data = Samlet_ledig)
mod_lag5 <- lm(dif_Erhverv ~ dif_Ophold + Ophold_lag1 + Ophold_lag2 + Ophold_lag3 + Ophold_lag4 + Ophold_lag5+ dif_BNP + dif_Inflation + konjunktur, data = Samlet_ledig)
mod_lag6 <- lm(dif_Erhverv ~ dif_Ophold + Ophold_lag1 + Ophold_lag2 + Ophold_lag3 + Ophold_lag4 + Ophold_lag5 + Ophold_lag6 + dif_BNP + dif_Inflation + konjunktur, data = Samlet_ledig)
mod_lag7 <- lm(dif_Erhverv ~ dif_Ophold + Ophold_lag1 + Ophold_lag2 + Ophold_lag3 + Ophold_lag4 + Ophold_lag5 + Ophold_lag6 + Ophold_lag7 + dif_BNP + dif_Inflation + konjunktur, data = Samlet_ledig)
mod_lag8 <- lm(dif_Erhverv ~ dif_Ophold + Ophold_lag1 + Ophold_lag2 + Ophold_lag3 + Ophold_lag4 + Ophold_lag5 + Ophold_lag6 + Ophold_lag7 + Ophold_lag8 + dif_BNP + dif_Inflation + konjunktur, data = Samlet_ledig)

AIC(mod_lag0, mod_lag1, mod_lag2, mod_lag3, mod_lag4, mod_lag5, mod_lag6, mod_lag7, mod_lag8) 
BIC(mod_lag0, mod_lag1, mod_lag2, mod_lag3, mod_lag4, mod_lag5, mod_lag6, mod_lag7, mod_lag8)

### Analyse af alt samlet forskellige lags ###
## 1
Samlet_ledig_BI1 <- lm(dif_ialt ~ dif_Ophold +  dif_Inflation + konjunktur + dif_BNP ,
                       data = Samlet_ledig)
coeftest(Samlet_ledig_BI1, vcov = NeweyWest(Samlet_ledig_BI1, lag = 0, prewhite = FALSE))
summary(Samlet_ledig_BI1)

## 2
Samlet_ledig_BIC2 <- lm(dif_industri ~ dif_Ophold + dif_Inflation + konjunktur + dif_BNP ,
                        data = Samlet_ledig)
coeftest(Samlet_ledig_BIC2, vcov = NeweyWest(Samlet_ledig_BIC2, lag = 0, prewhite = FALSE))
summary(Samlet_ledig_BIC2)

## 3
Samlet_ledig_BIC3 <- lm(dif_bygge ~ dif_Ophold + dif_Inflation + konjunktur + dif_BNP ,
                        data = Samlet_ledig)
coeftest(Samlet_ledig_BIC3, vcov = NeweyWest(Samlet_ledig_BIC3, lag = 0, prewhite = FALSE))
summary(Samlet_ledig_BIC3)

## 4
Samlet_ledig_BIC4 <- lm(dif_Handel ~ dif_Ophold + Ophold_lag1 + Ophold_lag2 + Ophold_lag3  +
                          Ophold_lag4 + Ophold_lag5 + Ophold_lag6 + Ophold_lag7 +
                          dif_Inflation + konjunktur + dif_BNP ,
                        data = Samlet_ledig)
coeftest(Samlet_ledig_BIC4, vcov = NeweyWest(Samlet_ledig_BIC4, lag = 7, prewhite = FALSE))
summary(Samlet_ledig_BIC4)

## 5
Samlet_ledig_BIC5 <- lm(dif_infor ~ dif_Ophold + dif_Inflation + konjunktur + dif_BNP ,
                        data = Samlet_ledig)
coeftest(Samlet_ledig_BIC5, vcov = NeweyWest(Samlet_ledig_BIC5, lag = 0, prewhite = FALSE))
summary(Samlet_ledig_BIC5)

## 6
Samlet_ledig_BIC6 <- lm(dif_fin ~ dif_Ophold + Ophold_lag1 + Ophold_lag2 + dif_Inflation + konjunktur + dif_BNP ,
                        data = Samlet_ledig)
coeftest(Samlet_ledig_BIC6, vcov = NeweyWest(Samlet_ledig_BIC6, lag = 2, prewhite = FALSE))
summary(Samlet_ledig_BIC6)

## 7
Samlet_ledig_BIC7 <- lm(dif_Erhverv ~ dif_Ophold + dif_Inflation + konjunktur + dif_BNP ,
                        data = Samlet_ledig)
coeftest(Samlet_ledig_BIC7, vcov = NeweyWest(Samlet_ledig_BIC7, lag = 0, prewhite = FALSE))
summary(Samlet_ledig_BIC7)

## Samlet insignifikant?

linearHypothesis(Samlet_ledig_BIC4, c("Ophold_lag1 = 0", "Ophold_lag2 = 0", "Ophold_lag3 = 0", "Ophold_lag4 = 0", 
                                      "Ophold_lag5 = 0", "Ophold_lag6 = 0", "Ophold_lag7 = 0"))
linearHypothesis(Samlet_ledig_BIC6, c("Ophold_lag1 = 0", "Ophold_lag2 = 0"))


### normality 
resid <- residuals(Samlet_ledig_BIC1)
shapiro.test(resid)
resid <- residuals(Samlet_ledig_BIC2)
shapiro.test(resid)
resid <- residuals(Samlet_ledig_BIC3)
shapiro.test(resid)
resid <- residuals(Samlet_ledig_BIC4)
shapiro.test(resid)
resid <- residuals(Samlet_ledig_BIC5)
shapiro.test(resid)
resid <- residuals(Samlet_ledig_BIC6)
shapiro.test(resid)
resid <- residuals(Samlet_ledig_BIC7)
shapiro.test(resid)

### Hetero
bptest(Samlet_ledig_BIC1)
bptest(Samlet_ledig_BIC2)
bptest(Samlet_ledig_BIC3)
bptest(Samlet_ledig_BIC4)
bptest(Samlet_ledig_BIC5)
bptest(Samlet_ledig_BIC6)
bptest(Samlet_ledig_BIC7)

## autocorrelation 
bgtest(Samlet_ledig_BIC1, order = 0)
bgtest(Samlet_ledig_BIC2, order = 0)
bgtest(Samlet_ledig_BIC3, order = 0)
bgtest(Samlet_ledig_BIC4, order = 7)
bgtest(Samlet_ledig_BIC5, order = 0)
bgtest(Samlet_ledig_BIC6, order = 2)
bgtest(Samlet_ledig_BIC7, order = 0)

### Western non Western
Opholdstilladelse2 <- Opholdstilladelse %>%
  filter(!Opholdstilladelse %in% c("Familiesammenføring, Mindreårige børn, Refererer til flygtning", 
                                   "Familiesammenføring, Mindreårige børn, Refererer til andre end flygtning", 
                                   "Familiesammenføring, Mindreårige børn, Uoplyst referenceperson",
                                   "EU/EØS, Uddannelse", "Studie mv., Øvrige grunde", "EU/EØS, Uddannelse",
                                   "Studie mv., Praktikanter", "Studie mv., Uddannelse", "Studie mv., Au pair",
                                   "Det øvrige opholdsområde")) %>% 
  pivot_longer(cols = -Opholdstilladelse, names_to = "ref_date", values_to = "Ophold") %>% 
  filter(ref_date >= "1998K1") %>% 
  select(ref_date, Ophold) %>% 
  summarise(Ophold = sum(Ophold, na.rm =T)/1e3, .by = ref_date) %>% collect()

Vestlige2 <- Vestlige %>%
  pivot_longer(cols = -Land, names_to = "ref_date", values_to = "Western") %>% 
  select(ref_date, Western) %>% 
  summarise(Western = sum(Western, na.rm =T)/1e3, .by = ref_date) %>% collect()

Opholdstilladelse_filter <- Opholdstilladelse2 %>%
  left_join(Vestlige2, by = "ref_date") %>% collect()

Opholdstilladelse_filter <- Opholdstilladelse_filter %>% 
  group_by(ref_date) %>%
  mutate(Non_western = Ophold - Western) %>%
  collect()

Opholdstilladelse_filter$dif_Non_western<- c(NA, diff(Opholdstilladelse_filter$Non_western))
Opholdstilladelse_filter$dif_Western<- c(NA, diff(Opholdstilladelse_filter$Western))

Opholdstilladelse_west <- Opholdstilladelse_filter %>%
  filter(!is.na(dif_Non_western)) %>% 
  arrange(ref_date) %>%
  ungroup() %>% 
  mutate(Non_western = as.numeric(Non_western), 
         Western = as.numeric(Western), 
         dif_Non_western = as.numeric(dif_Non_western), 
         dif_Western = as.numeric(dif_Western),
         West_lag1 = dplyr::lag(dif_Western, 1),
         West_lag2 = dplyr::lag(dif_Western, 2),
         West_lag3 = dplyr::lag(dif_Western, 3),
         West_lag4 = dplyr::lag(dif_Western, 4),
         West_lag5 = dplyr::lag(dif_Western, 5),
         West_lag6 = dplyr::lag(dif_Western, 6),
         West_lag7 = dplyr::lag(dif_Western, 7),
         West_lag8 = dplyr::lag(dif_Western, 8), 
         NWest_lag1 = dplyr::lag(dif_Non_western, 1),
         NWest_lag2 = dplyr::lag(dif_Non_western, 2),
         NWest_lag3 = dplyr::lag(dif_Non_western, 3),
         NWest_lag4 = dplyr::lag(dif_Non_western, 4),
         NWest_lag5 = dplyr::lag(dif_Non_western, 5),
         NWest_lag6 = dplyr::lag(dif_Non_western, 6),
         NWest_lag7 = dplyr::lag(dif_Non_western, 7),
         NWest_lag8 = dplyr::lag(dif_Non_western, 8)) %>%
  mutate(ref_date = as.yearqtr(gsub("K", " Q", ref_date), format = "%Y Q%q")) %>%
  collect()

Samlet_produktion <- Samlet_produktion %>% 
  select(ref_date, dif_BNP, dif_Inflation, dif_Produktion, konjunktur) %>% 
  left_join(Opholdstilladelse_west, by = "ref_date") %>% 
  collect()

### Analyse af alt samlet ###
#### Western
## Base
OLS_produktion_Base <- lm(dif_Produktion ~ dif_Western,
                          data = Samlet_produktion)
coeftest(OLS_produktion_Base, vcov = NeweyWest(OLS_produktion_Base, lag = 0, prewhite = FALSE))
summary(OLS_produktion_Base)
## BIC
OLS_produktion_BIC <- lm(dif_Produktion ~ dif_Western + dif_Inflation + konjunktur + dif_BNP ,
                         data = Samlet_produktion)
coeftest(OLS_produktion_BIC, vcov = NeweyWest(OLS_produktion_BIC, lag = 0, prewhite = FALSE))
summary(OLS_produktion_BIC)
## AIC
OLS_produktion_AIC <- lm(dif_Produktion ~ dif_Western + West_lag1 + West_lag2 + West_lag3 +
                           West_lag4 + West_lag5 + West_lag6 + West_lag7+ West_lag8 
                         + dif_Inflation + konjunktur + dif_BNP ,
                         data = Samlet_produktion)
coeftest(OLS_produktion_AIC, vcov = NeweyWest(OLS_produktion_AIC, lag = 8, prewhite = FALSE))
summary(OLS_produktion_AIC)

#### Non-Western
## Base
OLS_produktion_Base_n <- lm(dif_Produktion ~ dif_Non_western,
                          data = Samlet_produktion)
coeftest(OLS_produktion_Base_n, vcov = NeweyWest(OLS_produktion_Base_n, lag = 0, prewhite = FALSE))
summary(OLS_produktion_Base_n)
## BIC
OLS_produktion_BIC_n <- lm(dif_Produktion ~ dif_Non_western + dif_Inflation + konjunktur + dif_BNP ,
                         data = Samlet_produktion)
coeftest(OLS_produktion_BIC_n, vcov = NeweyWest(OLS_produktion_BIC_n, lag = 0, prewhite = FALSE))
summary(OLS_produktion_BIC_n)
## AIC
OLS_produktion_AIC_n <- lm(dif_Produktion ~ dif_Non_western + NWest_lag1 + NWest_lag2 + NWest_lag3 +
                           NWest_lag4 + NWest_lag5 + NWest_lag6 + dif_Inflation + konjunktur + dif_BNP ,
                         data = Samlet_produktion)
coeftest(OLS_produktion_AIC_n, vcov = NeweyWest(OLS_produktion_AIC_n, lag = 6, prewhite = FALSE))
summary(OLS_produktion_AIC_n)

# Newey = autocorrelation and heteroskedasticity

## Samlet insignifikant?
linearHypothesis(OLS_produktion_AIC_n, c("NWest_lag1 = 0", "NWest_lag2 = 0", "NWest_lag3 = 0", "NWest_lag4 = 0", 
                                   "NWest_lag5 = 0", "NWest_lag6 = 0"))

# Sammenlign AIC og BIC
mod_lag0 <- lm(dif_Produktion ~ dif_Non_western + dif_BNP + dif_Inflation + konjunktur, data = Samlet_produktion)
mod_lag1 <- lm(dif_Produktion ~ dif_Non_western + NWest_lag1 + dif_BNP + dif_Inflation + konjunktur, data = Samlet_produktion)
mod_lag2 <- lm(dif_Produktion ~ dif_Non_western + NWest_lag1 + NWest_lag2 + dif_BNP + dif_Inflation + konjunktur, data = Samlet_produktion)
mod_lag3 <- lm(dif_Produktion ~ dif_Non_western + NWest_lag1 + NWest_lag2 + NWest_lag3 + dif_BNP + dif_Inflation + konjunktur, data = Samlet_produktion)
mod_lag4 <- lm(dif_Produktion ~ dif_Non_western + NWest_lag1 + NWest_lag2 + NWest_lag3 + NWest_lag4 + dif_BNP + dif_Inflation + konjunktur, data = Samlet_produktion)
mod_lag5 <- lm(dif_Produktion ~ dif_Non_western + NWest_lag1 + NWest_lag2 + NWest_lag3 + NWest_lag4 + NWest_lag5+ dif_BNP + dif_Inflation + konjunktur, data = Samlet_produktion)
mod_lag6 <- lm(dif_Produktion ~ dif_Non_western + NWest_lag1 + NWest_lag2 + NWest_lag3 + NWest_lag4 + NWest_lag5 + NWest_lag6 + dif_BNP + dif_Inflation + konjunktur, data = Samlet_produktion)
mod_lag7 <- lm(dif_Produktion ~ dif_Non_western + NWest_lag1 + NWest_lag2 + NWest_lag3 + NWest_lag4 + NWest_lag5 + NWest_lag6 + NWest_lag7 + dif_BNP + dif_Inflation + konjunktur, data = Samlet_produktion)
mod_lag8 <- lm(dif_Produktion ~ dif_Non_western + NWest_lag1 + NWest_lag2 + NWest_lag3 + NWest_lag4 + NWest_lag5 + NWest_lag6 + NWest_lag7 + NWest_lag8 + dif_BNP + dif_Inflation + konjunktur, data = Samlet_produktion)

AIC(mod_lag0, mod_lag1, mod_lag2, mod_lag3, mod_lag4, mod_lag5, mod_lag6, mod_lag7, mod_lag8) 
BIC(mod_lag0, mod_lag1, mod_lag2, mod_lag3, mod_lag4, mod_lag5, mod_lag6, mod_lag7, mod_lag8)

### normality 
resid <- residuals(OLS_produktion_Base)
shapiro.test(resid)
resid <- residuals(OLS_produktion_BIC)
shapiro.test(resid)
resid <- residuals(OLS_produktion_AIC)
shapiro.test(resid)

resid <- residuals(OLS_produktion_Base_n)
shapiro.test(resid)
resid <- residuals(OLS_produktion_BIC_n)
shapiro.test(resid)
resid <- residuals(OLS_produktion_AIC_n)
shapiro.test(resid)
### Hetero
bptest(OLS_produktion_Base)
bptest(OLS_produktion_BIC)
bptest(OLS_produktion_AIC)

bptest(OLS_produktion_Base_n)
bptest(OLS_produktion_BIC_n)
bptest(OLS_produktion_AIC_n)

## autocorrelation 
bgtest(OLS_produktion_Base, order = 0)
bgtest(OLS_produktion_BIC, order = 0) 
bgtest(OLS_produktion_AIC, order = 8)
bgtest(OLS_produktion_Base_n, order = 0)
bgtest(OLS_produktion_BIC_n, order = 0) 
bgtest(OLS_produktion_AIC_n, order = 6)
























