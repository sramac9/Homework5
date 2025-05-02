# Meta --------------------------------------------------------------------
# Author:        Sammy Ramacher
# Date Created:  4/21/2025
# Date Edited:   4/23/2025
# Homework 3-1

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata, scales, fixest, broom, gt, modelsummary, lfe)

#Question 1: Plot the share of the adult population with direct purchase health insurance over time.

ins.data <- read_tsv("data/acs_insurance.txt")
q1 <- ins.data %>%
  group_by(year) %>%
  summarize(
    total_ins_direct = sum(ins_direct, na.rm = TRUE),
    total_adult_pop = sum(adult_pop, na.rm = TRUE)
  ) %>%
  mutate(fraction_insured = total_ins_direct / total_adult_pop) %>%
  ggplot(aes(x = year, y = fraction_insured)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Fraction of Direct Insurance Coverage by Year",
    x = "Year",
    y = "Fraction Insured (Direct)"
  ) +
  theme_minimal()

#Question 2: discussion

#Question 3: Plot the share of the adult population with Medicaid over time.

q3 <- ins.data %>%
  group_by(year) %>%
  summarize(
    total_ins_medicaid = sum(ins_medicaid, na.rm = TRUE),
    total_adult_pop = sum(adult_pop, na.rm = TRUE)
  ) %>%
  mutate(fraction_insured = total_ins_medicaid / total_adult_pop) %>%
  ggplot(aes(x = year, y = fraction_insured)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Fraction of Medicaid Coverage by Year",
    x = "Year",
    y = "Fraction Insured (Medicaid)"
  ) +
  theme_minimal()

#Question 4: Plot the share of uninsured over time, separately by states that expanded Medicaid in 2014 versus those that did not. Drop all states that expanded after 2014.

mcaid.data <- read_tsv("data/acs_medicaid.txt")
ins.plot.dat <- mcaid.data %>% filter(expand_year==2014 | is.na(expand_year), !is.na(expand_ever)) %>%
  mutate(perc_unins=uninsured/adult_pop) %>%
  group_by(expand_ever, year) %>% summarize(mean=mean(perc_unins))

q4 <- ggplot(data=ins.plot.dat, aes(x=year,y=mean,group=expand_ever,linetype=expand_ever)) + 
  geom_line() + geom_point() + theme_bw() +
  geom_vline(xintercept=2013.5, color="red") +
  geom_text(data = ins.plot.dat %>% filter(year == 2016), 
            aes(label = c("Non-expansion","Expansion"),
                x = year + 1,
                y = mean)) +
  guides(linetype="none") +
  labs(
    x="Year",
    y="Fraction Uninsured",
    title="Share of Uninsured over Time"
  )

#Question 5: Calculate the average percent of uninsured individuals in 2012 and 2015, separately for expansion and non-expansion states. Present your results in a basic 2x2 DD table.
expanded <- mcaid.data %>%
  group_by(State) %>%
  summarize(first_expand_year = unique(year(date_adopted))) %>%
  mutate(
    expand_group = case_when(
      is.na(first_expand_year) ~ "Never Expanded",
      first_expand_year == 2014 ~ "Expanded in 2014",
      TRUE ~ NA_character_  )) %>%
  filter(!is.na(expand_group))

final.data.exp <- mcaid.data %>%
  inner_join(expanded, by = "State")

uninsured.share <- final.data.exp %>%
  group_by(year, expand_group) %>%
  summarize(
    total_uninsured = sum(uninsured, na.rm = TRUE),
    total_adult_pop = sum(adult_pop, na.rm = TRUE),
    share_uninsured = total_uninsured / total_adult_pop,
    .groups = "drop")

dd.table <- final.data.exp %>%
  filter(year %in% c(2012, 2015)) %>%
  group_by(expand_group, year) %>%
  summarize(
    avg_uninsured = sum(uninsured, na.rm = TRUE) / sum(adult_pop, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = year, values_from = avg_uninsured)

q5 <- dd.table %>%
  mutate(
    diff = `2015` - `2012`
  )

#Question 6: Estimate the effect of Medicaid expansion on the uninsurance rate using a standard DD regression estimator, again focusing only on states that expanded in 2014 versus those that never expanded.

reg.dat <- mcaid.data %>% filter(expand_year==2014 | is.na(expand_year), !is.na(expand_ever)) %>%
  mutate(perc_unins=uninsured/adult_pop,
         post = (year>=2014), 
         treat=post*expand_ever)

dd.ins.reg <- lm(perc_unins ~ post + expand_ever + post*expand_ever, data=reg.dat)
q6 <- modelsummary(list("DD (2014)"=dd.ins.reg),
             shape=term + statistic ~ model, 
             gof_map=NA,
             coef_omit='Intercept',
             vcov=~State,
             output = "markdown"
         )

#Question 7: Include state and year fixed effects in your estimates. Try using the lfe or fixest package to estimate this instead of directly including the fixed effects.
m.twfe <- feols(perc_unins ~ treat | State + year, data=reg.dat)
q7 <- msummary(list("TWFE"=m.twfe),
         shape=term + statistic ~ model, 
         gof_map=NA,
         coef_omit='Intercept',
         vcov=~State,
         output = "markdown"
         )



rm(list=c("ins.data"))
save.image("submission1/hwk5_workspace.Rdata")