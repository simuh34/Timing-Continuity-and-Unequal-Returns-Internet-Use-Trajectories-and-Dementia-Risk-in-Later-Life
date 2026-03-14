library(dplyr)
library(survival)
library(ggplot2)
library(broom)
library(dagitty)
library(ggdag)
library(lubridate)
library(nnet)
library(sjPlot)
library(haven)
library(tableone)
library(survminer)
library(kableExtra)

df <- read.csv("D:\\R project\\Digital\\HRS\\hrs_imputation.csv")
df <- df[,-1]
df_cognition <- read_dta("D:\\R project\\data\\hrs\\cogfinalimp_9520wide.dta")
table(df_cognition$cogfunction2010)
df$age_sq <- (df$age)^2

#event analysis
df_cognition$hhid <- sub("^0", "", df_cognition$hhid)
df_cognition$hhidpn <- paste(as.character(df_cognition$hhid), as.character(df_cognition$pn), sep = "")

df$hhidpn <- as.numeric(df$hhidpn)
df_cognition$hhidpn <- as.numeric(df_cognition$hhidpn)

df_dementia <- left_join(
  df, df_cognition %>% select("hhidpn", "cogfunction2010", "cogfunction2012","cogfunction2014","cogfunction2016","cogfunction2018"),
  by = "hhidpn")

nrow(df_dementia)
table(df_dementia$cogfunction2010)
df_dementia %>%
  select("cogfunction2010", "cogfunction2012","cogfunction2014","cogfunction2016","cogfunction2018") %>%
  summary()

#delete participants had dimented problem in 2010 (n = 3635)
df_dementia <- subset(df_dementia, cogfunction2010 != 3)

#construct outcome dementia event
for (i in seq(2012, 2019, by = 2)) {
  df_dementia[[paste0("dementia_", i)]] <- ifelse(df_dementia[[paste0("cogfunction", i)]] == 3, 1, 0)
}
df_dementia %>%
  select("dementia_2012","dementia_2014","dementia_2016","dementia_2018") %>%
  summary()

#events_status
df_dementia <- df_dementia %>%
  mutate(
    event_status = case_when(
      (dementia_2012 == 1 | dementia_2014 == 1 | dementia_2016 == 1 | dementia_2018 == 1) ~ 1,
      is.na(dementia_2012) & is.na(dementia_2014) & is.na(dementia_2016) & is.na(dementia_2018) ~ NA_real_,
      TRUE ~ 0  
    )
  )
#drop rows do not have any dementia records (n = 9317)
df_dementia <- df_dementia[!is.na(df_dementia$event_status),]


#NOTE:The logic behind calculating event_date is as follows: For event_status == 0 (no event), if inv_date14 is not missing, the event_date is set to inv_date14. If inv_date14 is missing but death_date exists and is earlier than the minimum of inv_date14, the event_date is set to death_date. If inv_date14 is missing and the death_date condition does not apply, the event_date is imputed using the median of inv_date14 from available non-missing values. If neither condition applies, the event_date is set to NA. For event_status == 1 (event occurred), if dementia_2012 == 1, the event_date is set to inv_date11; if dementia_2014 == 1, the event_date is set to inv_date12; if dementia_2016 == 1, the event_date is set to inv_date13; and if dementia_2018 == 1, the event_date is set to inv_date14. If none of these conditions apply, the event_date is set to NA. This approach ensures the event_date is logically determined based on available data and handles missing values appropriately. Temporary columns used for the calculation of the minimum and median of inv_date14 are removed at the end.
df_dementia <- df_dementia %>%
  mutate(
    min_inv_date14 = min(inv_date14, na.rm = TRUE),
    median_inv_date14 = median(inv_date14, na.rm = TRUE),
    event_date = case_when(
      event_status == 0 & !is.na(inv_date14) ~ as.Date(inv_date14, origin = "1960-01-01"),  
      event_status == 0 & !is.na(death_date) & death_date < min_inv_date14 ~ as.Date(death_date, origin = "1960-01-01"),  
      event_status == 0 & is.na(inv_date14) ~ as.Date(median_inv_date14, origin = "1960-01-01"),  
      event_status == 0 ~ as.Date(NA),  
      event_status == 1 ~ as.Date(
        coalesce(
          ifelse(dementia_2012 == 1, inv_date11, NA), 
          ifelse(dementia_2014 == 1, inv_date12, NA), 
          ifelse(dementia_2016 == 1, inv_date13, NA), 
          ifelse(dementia_2018 == 1, inv_date14, NA)
        ), origin = "1960-01-01"
      ),
      TRUE ~ as.Date(NA)
    )
  ) %>%
  select(-min_inv_date14, -median_inv_date14)


df_dementia <- df_dementia %>%
  mutate(
    event_time = as.numeric(difftime(event_date, as.Date(inv_date10, origin = "1960-01-01"), units = "days")) / 30.44
  )


#reference group
df_dementia$digital_usage <- factor(df_dementia$digital_usage, 
                               levels = c("Never users", "Dropouts", "Intermittent Users", "Adopters", "Consistent users"))
df_dementia$digital_usage <- relevel(df_dementia$digital_usage, ref = "Never users")
df_dementia$Race <- factor(df_dementia$Race,levels = c("Non-Hispanic White" ,"Other"))
df_dementia$Race <- relevel(df_dementia$Race,ref = "Non-Hispanic White" )
df_dementia$H_education <- factor(df_dementia$H_education)
df_dementia$H_education <- relevel(df_dementia$H_education, ref = "Less than upper secondary")
#df_dementia$health_condition <- as.numeric(df_dementia$health_condition)
df_dementia$residence_rural <- factor(df_dementia$residence_rural)
df_dementia$residence_rural <- relevel(df_dementia$residence_rural, ref = "rural")
df_dementia$gender <- relevel(factor(df_dementia$gender), ref = "Men")
df_dementia$equivalized_wealth <- relevel(factor(df_dementia$equivalized_wealth), ref = "Lower wealth")

####table

t1 = CreateTableOne(vars = c("age","gender","Race", "H_education","marital_status", "employment_status", "residence_rural","equivalized_wealth"),   
                    data=df_dementia,strata = "digital_usage")
print(t1, showAllLevels = TRUE, catDigits=2, printToggle = FALSE) %>% 
  knitr::kable(caption = "Descriptive charateristics by hypertension and treatment status",
               booktabs = T, linesep = '') %>% 
  kable_styling(latex_options = "hold_position")

t2 = CreateTableOne(vars = c("age","gender","Race", "H_education","marital_status", "employment_status", "residence_rural","equivalized_wealth"),   
                    data=df_dementia)
print(t2, showAllLevels = TRUE, catDigits=2, printToggle = FALSE) %>% 
  knitr::kable(caption = "Descriptive charateristics by hypertension and treatment status",
               booktabs = T, linesep = '') %>% 
  kable_styling(latex_options = "hold_position")

####model
library(survival)
surv_obj <- Surv(time = df_dementia$event_time, event = df_dementia$event_status)
model_dementia <- coxph(surv_obj ~ digital_usage + age + age_sq + gender + Race + 
                     residence_rural + H_education + marital_status + 
                     equivalized_wealth, data = df_dementia, weights = iptw_weight)
summary(model_dementia)
exp(confint(model_dementia))

model_dementia_unadjusted <- coxph(surv_obj ~ digital_usage, data = df_dementia, weights = iptw_weight)
summary(model_dementia_unadjusted)

model_dementia_nw <- coxph(surv_obj ~ digital_usage + age + age_sq + gender + Race + 
                        residence_rural + H_education + marital_status + 
                        equivalized_wealth, data = df_dementia, weights = iptw_weight)
summary(model_dementia_nw)

####cip####
mean_age <- mean(df_dementia$age, na.rm = TRUE)
mean_age_sq <- mean(df_dementia$age_sq, na.rm = TRUE)
#mean_health_condition <- mean(df_dementia$health_condition, na.rm = TRUE)
mode_gender <- factor('Men')
mode_race <- factor("Non-Hispanic White")
mode_rural <- factor("rural")
mode_education <- factor("Less than upper secondary")
mode_marital <- factor("Married/Parterned")
mode_wealth <- factor("Lower wealth")

newdata <- data.frame(
  digital_usage = factor(levels(df_dementia$digital_usage), levels = levels(df_dementia$digital_usage)),
  age = rep(mean_age, length(levels(df_dementia$digital_usage))),  
  age_sq = rep(mean_age_sq, length(levels(df_dementia$digital_usage))),  
  gender = rep(mode_gender, length(levels(df_dementia$digital_usage))),
  Race = rep(mode_race, length(levels(df_dementia$digital_usage))),
  residence_rural = rep(mode_rural, length(levels(df_dementia$digital_usage))),
  H_education = rep(mode_education, length(levels(df_dementia$digital_usage))),
  marital_status = rep(mode_marital, length(levels(df_dementia$digital_usage))),
  equivalized_wealth = rep(mode_wealth, length(levels(df_dementia$digital_usage)))
)


fit <- survfit(model_dementia_unadjusted, newdata = newdata)
ggsurv_obj <- ggsurvplot(fit,
                         data = df_dementia,
                         fun = "event",
                         conf.int = FALSE,
                         risk.table = FALSE,
                         palette = "Set1",
                         xlab = "Time (Years)",
                         ylab = "Cumulative Incidence (%)",
                         legend.title = "Digital Usage",
                         legend.labs = c("Never users", "Dropouts", "Intermittent Users", "Adopters", "Consistent users"),
                         risk.table.height = 0.25,
                         title = "D.Dementia",
                         break.time.by = 12,
                         xscale = "d_y",
                         censor = FALSE,    
                         size = 0.7)         

p_final <- ggsurv_obj$plot + 
  theme_minimal() +  
  scale_x_continuous(
    breaks = seq(0, 106, by = 12),  
    labels = function(x) paste0(round(x / 12))  
  ) +
  scale_color_manual(
    values = c("Never users" = "#8b0000", "Dropouts" = "#f06d6d", "Intermittent Users" = "#f8c8c8", "Adopters" = "#66cdaa", "Consistent users" = "#006400"),
    labels = c("Never Users", "Dropouts", "Intermittent Users", "Adopters", "Consistent Users") 
  ) +
  theme(
    text = element_text(size = 14),  
    legend.position = c(0.2, 0.8),  
    legend.title = element_text(size = 14),  
    legend.text = element_text(size = 12),  
    legend.key.size = unit(0.5, "cm"),  
    legend.background = element_rect(color = "black", size = 1, linetype = "solid")  
  ) +
  guides(color = guide_legend(
    title = "Internet use",
    title.position = "top",  
    title.hjust = 0.5, 
    label.position = "right",  
    order = 1)) +
  coord_cartesian(xlim = c(0, 106), ylim = c(0,1))  

png(file = 'D:\\R project\\Digital\\HRS\\plot\\dementia.png',
    width = 1800,
    height = 1200,
    units = "px",
    res = 200)
print(p_final)
dev.off()

p_final_b <- ggsurv_obj$plot + 
  theme_minimal() +  
  scale_x_continuous(
    breaks = seq(0, 106, by = 12),  
    labels = function(x) paste0(round(x / 12))  
  ) +
  scale_color_manual(
    values = c("Never users" = "#8b0000", "Dropouts" = "#f06d6d", "Intermittent Users" = "#f8c8c8", "Adopters" = "#66cdaa", "Consistent users" = "#006400"),
    labels = c("Never Users", "Dropouts", "Intermittent Users", "Adopters", "Consistent Users") 
  ) +
  theme(
    text = element_text(size = 14),  
    legend.position = c(0.2, 0.8),  
    legend.title = element_text(size = 14),  
    legend.text = element_text(size = 12),  
    legend.key.size = unit(0.5, "cm"),  
    legend.background = element_rect(color = "black", size = 1, linetype = "solid"),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()   
  ) +
  guides(color = guide_legend(
    title = "Internet use",
    title.position = "top",  
    title.hjust = 0.5, 
    label.position = "right",  
    order = 1)) +
  coord_cartesian(xlim = c(0, 106), ylim = c(0,1))  

png(file = 'D:\\R project\\Digital\\HRS\\plot\\dementia_b.png',
    width = 1800,
    height = 1200,
    units = "px",
    res = 200)
print(p_final_b)
dev.off()

summary(df_dementia$event_status[df_dementia$digital_usage == "Never users"])


####interaction####
#base model
model_dementia <- coxph(Surv(event_time, event_status) ~ 
                                 digital_usage + gender  + age + age_sq + Race + 
                                 residence_rural + H_education + marital_status + 
                                 equivalized_wealth, 
                               data = df_dementia, weights = iptw_weight)
summary(model_dementia)
conf_int <- confint(model_dementia_inter)
round(exp(conf_int), 2)


#gender interaction
model_dementia_gender_inter <- coxph(Surv(event_time, event_status) ~ 
                                        digital_usage*gender  + age + age_sq + Race + 
                                        residence_rural + H_education + marital_status + 
                                        equivalized_wealth, 
                                      data = df_dementia, weights = iptw_weight)
summary(model_dementia_gender_inter)
conf_int <- confint(model_dementia_gender_inter)
round(exp(conf_int), 2)

#race interaction
model_dementia_race_inter <- coxph(Surv(event_time, event_status) ~ 
                                      digital_usage * Race + gender  + age + age_sq + 
                                      residence_rural + H_education + marital_status + 
                                      equivalized_wealth, 
                                    data = df_dementia, weights = iptw_weight)
summary(model_dementia_race_inter)
conf_int <- confint(model_dementia_race_inter)
round(exp(conf_int), 2)

#urbanicity interaction
model_dementia_rural_inter <- coxph(Surv(event_time, event_status) ~ 
                                       digital_usage * residence_rural + Race + gender  + age + age_sq + 
                                       residence_rural + H_education + marital_status + 
                                       equivalized_wealth, 
                                     data = df_dementia, weights = iptw_weight)
summary(model_dementia_rural_inter)
conf_int <- confint(model_dementia_rural_inter)
round(exp(conf_int), 2)

#wealth interaction
model_dementia_wealth_inter <- coxph(Surv(event_time, event_status) ~ 
                                        digital_usage * equivalized_wealth + residence_rural + Race + gender  + age + age_sq + 
                                        residence_rural + H_education + marital_status, 
                                      data = df_dementia, weights = iptw_weight)
summary(model_dementia_wealth_inter)
conf_int <- confint(model_dementia_wealth_inter)
round(exp(conf_int), 2)

#likelihood ratio test
library(lmtest)
lrt_1 <- lrtest(model_dementia_inter, model_dementia_gender_inter)
lrt_2 <- lrtest(model_dementia_inter, model_dementia_race_inter)
lrt_3 <- lrtest(model_dementia_inter, model_dementia_rural_inter)
lrt_4 <- lrtest(model_dementia_inter, model_dementia_wealth_inter)

####stratified analysis####
#gender
##men
model_dementia_men <- coxph(Surv(event_time, event_status) ~ 
                         digital_usage + age + age_sq + Race + 
                         residence_rural + H_education + marital_status + 
                         equivalized_wealth, 
                       data = df_dementia[df_dementia$gender == "Men",], weights = iptw_weight)
summary(model_dementia_men)
conf_int <- confint(model_dementia_men)
round(exp(conf_int), 2)

##women
model_dementia_women <- coxph(Surv(event_time, event_status) ~ 
                           digital_usage  + age + age_sq + Race + 
                           residence_rural + H_education + marital_status + 
                           equivalized_wealth, 
                         data = df_dementia[df_dementia$gender == "Women",], weights = iptw_weight)
conf_int <- confint(model_dementia_women)
round(exp(conf_int), 2)
summary(model_dementia_women)


#race
##white
model_dementia_white <- coxph(Surv(event_time, event_status) ~ 
                           digital_usage + age + age_sq  + 
                           residence_rural + H_education + marital_status + 
                           equivalized_wealth, 
                         data = df_dementia[df_dementia$Race == "Non-Hispanic White",], weights = iptw_weight)
conf_int <- confint(model_dementia_white)
round(exp(conf_int), 2)
confint(model_dementia_white)

##other
model_dementia_other <- coxph(Surv(event_time, event_status) ~ 
                           digital_usage  + age + age_sq + 
                           residence_rural + H_education + marital_status + 
                           equivalized_wealth, 
                         data = df_dementia[df_dementia$Race == "Other",], weights = iptw_weight)
summary(model_dementia_other)
conf_int <- confint(model_dementia_other)
round(exp(conf_int), 2)

summary(model_dementia_white,model_dementia_other)

#rural
##rural
model_dementia_rural <- coxph(Surv(event_time, event_status) ~ 
                                  digital_usage + age + age_sq  + 
                                  H_education + marital_status + 
                                  equivalized_wealth, 
                                data = df_dementia[df_dementia$residence_rural == "rural",], weights = iptw_weight)
summary(model_dementia_rural)
conf_int <- confint(model_dementia_rural)
round(exp(conf_int), 2)
confint(model_dementia_rural)

##other
model_dementia_urban <- coxph(Surv(event_time, event_status) ~ 
                                  digital_usage  + age + age_sq + 
                                  H_education + marital_status + 
                                  equivalized_wealth, 
                                data = df_dementia[df_dementia$residence_rural == "urban",], weights = iptw_weight)
summary(model_dementia_urban)
conf_int <- confint(model_dementia_urban)
round(exp(conf_int), 2)
summary(model_dementia_rural,model_dementia_urban)

#wealth
##lower
model_dementia_low <- coxph(Surv(event_time, event_status) ~ 
                         digital_usage + age + age_sq  + 
                         H_education + marital_status + residence_rural, 
                       data = df_dementia[df_dementia$equivalized_wealth == "Lower wealth",], weights = iptw_weight)
summary(model_adl_low)
conf_int <- confint(model_dementia_low)
round(exp(conf_int), 2)
confint(model_adl_low)

##upper
model_dementia_high <- coxph(Surv(event_time, event_status) ~ 
                          digital_usage  + age + age_sq + 
                          H_education + marital_status + 
                          residence_rural, 
                        data = df_dementia[df_dementia$equivalized_wealth == "Upper wealth",], weights = iptw_weight)
summary(model_adl_high)
conf_int <- confint(model_dementia_high)
round(exp(conf_int), 2)
summary(model_dementia_low,model_dementia_high)

#kp
#kaplan-meier plots
library(event)
library(survminer)
base_surv <- survfit(Surv(event_time, event_status) ~ digital_usage,data = df_dementia)
ggsurvplot(base_surv, data = df_dementia,legend.title = "Digital Usage",
           legend.labs = levels(as.factor(df_dementia$digital_usage)),
           xlab = "event Months",  
           ylab = "Suevival Probability",
           pval = TRUE)

####table####
library(tableone)
library(kableExtra)
dementia_by_year <- df_dementia %>%
  filter(event_status == 1) %>%  
  mutate(event_year = as.numeric(format(event_date, "%Y"))) %>%  
  filter(event_year %in% c(2012, 2014, 2016, 2018)) %>%  
  
  group_by(digital_usage, event_year) %>%
  summarise(events = n()) %>%
  ungroup() %>%
  
  arrange(digital_usage, event_year) %>%
  group_by(digital_usage) %>%
  mutate(cumulative_events = cumsum(events)) %>%
  ungroup()

print(dementia_by_year)

table(df_dementia$digital_usage)

library(emmeans)
#Pairwise Comparisons
emmeans_results <- emmeans(model_dementia, ~ digital_usage)
comparisons <- contrast(emmeans_results, method = "revpairwise")
summary_result <- summary(comparisons, 
                          type = "response", 
                          infer = c(TRUE, TRUE))

summary_result$ratio <- as.numeric(summary_result$ratio)
summary_result$p.value <- as.numeric(summary_result$p.value)

summary_result$lower.CL <- exp(log(summary_result$ratio) - 1.96 * summary_result$SE)
summary_result$upper.CL <- exp(log(summary_result$ratio) + 1.96 * summary_result$SE)

summary_result$ratio <- sprintf("%.2f", round(summary_result$ratio, 2))
summary_result$lower.CL <- sprintf("%.2f", round(summary_result$lower.CL, 2))
summary_result$upper.CL <- sprintf("%.2f", round(summary_result$upper.CL, 2))
summary_result$p.value <- sprintf("%.2f", round(summary_result$p.value, 2))

final_output <- summary_result[, c("contrast", "ratio", "lower.CL", "upper.CL", "p.value")]
print(final_output)


####sensitivity analysis####
#lagged analysis
df_dementia_lag <- df_dementia[df_dementia$inw11 == 1,]
df_dementia_lag <- df_dementia[df_dementia_lag$death_year > 2012 | is.na(df_dementia_lag$death_year), ]

model_dementia_lag <- coxph(Surv(event_time, event_status) ~ 
                               digital_usage + age + age_sq + gender + Race + 
                               residence_rural + H_education + marital_status + 
                               equivalized_wealth, 
                             data = df_dementia_lag, weights = iptw_weight)
summary(model_dementia_lag)
conf_int <- confint(model_dementia_lag)
round(exp(conf_int), 2)

logLik_model_dementia <- as.numeric(logLik(model_dementia))
logLik_model_dementia_lag <- as.numeric(logLik(model_dementia_lag))

lr_stat <- 2 * (logLik_model_dementia_lag - logLik_model_dementia)
df_diff <- df.residual(model_dementia_lag) - df.residual(model_dementia)
p_value <- 1 - pchisq(lr_stat, df_diff)
lr_stat
p_value

#complete information n = 9891
df_complete <- df_dementia[rowSums(is.na(df_dementia[, c("c_w6","c_w7","c_w8","c_w9","c_w10")])) < 1, ]
model_dementia_complete <- coxph(Surv(event_time, event_status) ~ 
                                    digital_usage + age + age_sq + gender + Race + 
                                    residence_rural + H_education + marital_status + 
                                    equivalized_wealth, 
                                  data = df_complete, weights = iptw_weight)
summary(model_dementia_complete)
conf_int <- confint(model_dementia_complete)
round(exp(conf_int), 2)

logLik_model_dementia <- as.numeric(logLik(model_dementia))
logLik_model_dementia_complete <- as.numeric(logLik(model_dementia_complete))

lr_stat <- 2 * (logLik_model_dementia_complete - logLik_model_dementia)
df_diff <- df.residual(model_dementia_complete) - df.residual(model_dementia)
p_value <- 1 - pchisq(lr_stat, df_diff)
lr_stat
p_value

#sampling weight
df_dementia$WEIGHT <- as.numeric(df_dementia$r6weight)
df_dementia$iptw_weight[df_dementia$iptw_weight <= 0] <- 1e-6
df_dementia$WEIGHT[df_dementia$WEIGHT <= 0] <- 1e-6
model_dementia_sample <- coxph(Surv(event_time, event_status) ~ 
                                  digital_usage + age + age_sq + gender + Race + 
                                  residence_rural + H_education + marital_status + 
                                  equivalized_wealth, 
                                data = df_dementia, weights = iptw_weight * WEIGHT)
summary(model_dementia_sample)
conf_int <- confint(model_dementia_sample)
round(exp(conf_int), 2)
round(exp(confint(model_dementia_sample)),2)

logLik_model_dementia <- as.numeric(logLik(model_dementia))
logLik_model_dementia_complete <- as.numeric(logLik(model_dementia_sample))

lr_stat <- 2 * (logLik_model_dementia_complete - logLik_model_dementia)
df_diff <- df.residual(model_dementia_complete) - df.residual(model_dementia)
p_value <- 1 - pchisq(lr_stat, df_diff)
lr_stat
p_value

#iptw < 10
df_dementia_iptw5 <- filter(df_dementia, df_dementia$iptw_weight < 10)
model_dementia_iptw5 <- coxph(Surv(event_time, event_status) ~ 
                                 digital_usage + age + age_sq + gender + Race + 
                                 residence_rural + H_education + marital_status + 
                                 equivalized_wealth, 
                               data = df_dementia_iptw5, weights = iptw_weight * WEIGHT)
summary(model_dementia_iptw5)
conf_int <- confint(model_dementia_iptw5)
round(exp(conf_int), 2)

logLik_model_dementia <- as.numeric(logLik(model_dementia))
logLik_model_dementia_iptw5 <- as.numeric(logLik(model_dementia_iptw5))

lr_stat <- 2 * (logLik_model_dementia_iptw5 - logLik_model_dementia)
df_diff <- df.residual(model_dementia_iptw5) - df.residual(model_dementia)
p_value <- 1 - pchisq(lr_stat, df_diff)
lr_stat
p_value

#ipcw
df_dementia$ipcw_cum_inw14[df_dementia$ipcw_cum_inw14 <= 0] <- 1e-6
df_dementia$iptw_weight[df_dementia$iptw_weight <= 0] <- 1e-6
model_dementia_sample <- coxph(Surv(event_time, event_status) ~ 
                                  digital_usage + age + age_sq + gender + Race + 
                                  residence_rural + H_education + marital_status + 
                                  equivalized_wealth, 
                                data = df_dementia, weights = iptw_weight * ipcw_cum_inw14)
summary(model_dementia_sample)
conf_int <- confint(model_dementia_sample)
round(exp(conf_int), 2)

logLik_model_dementia <- as.numeric(logLik(model_dementia))
logLik_model_dementia_sample <- as.numeric(logLik(model_dementia_sample))

lr_stat <- 2 * (logLik_model_dementia_sample - logLik_model_dementia)
df_diff <- df.residual(model_dementia_sample) - df.residual(model_dementia)
p_value <- 1 - pchisq(lr_stat, df_diff)
lr_stat
p_value

#Pairwise Comparisons
emmeans_results <- emmeans(model_dementia, ~ digital_usage)
comparisons <- contrast(emmeans_results, method = "revpairwise")
summary_result <- summary(comparisons, 
                          type = "response", 
                          infer = c(TRUE, TRUE))

summary_result$ratio <- as.numeric(summary_result$ratio)
summary_result$p.value <- as.numeric(summary_result$p.value)

summary_result$lower.CL <- exp(log(summary_result$ratio) - 1.96 * summary_result$SE)
summary_result$upper.CL <- exp(log(summary_result$ratio) + 1.96 * summary_result$SE)

summary_result$ratio <- sprintf("%.2f", round(summary_result$ratio, 2))
summary_result$lower.CL <- sprintf("%.2f", round(summary_result$lower.CL, 2))
summary_result$upper.CL <- sprintf("%.2f", round(summary_result$upper.CL, 2))
summary_result$p.value <- sprintf("%.2f", round(summary_result$p.value, 2))

final_output <- summary_result[, c("contrast", "ratio", "lower.CL", "upper.CL", "p.value")]
print(final_output)

#E-value
#extract rr
##define function
calc_e_value <- function(rr) {
  if (rr < 1) rr <- 1 / rr  # if rr < 1, take the reciprocal
  return(rr + rr * (rr - 1))
}

rr <- exp(coef(model_dementia))
variable_names <- names(rr)

e_values <- sapply(rr, calc_e_value)
results <- data.frame(Variable = variable_names, RiskRatio = rr, EValue = e_values)
print(results)
