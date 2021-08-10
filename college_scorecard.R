# analyze college outcomes vs debt
library(tidyverse)

# raw_score <- read_csv("data/Most-Recent-Cohorts-All-Data-Elements.csv")

raw_majors_current <- read_csv("data/Most-Recent-Cohorts-Field-of-Study.csv")

raw_majors_2years_ago <- read_csv("data/FieldOfStudyData1415_1516_PP.csv")

majors_current <- raw_majors_current %>%
  select(UNITID,INSTNM,CIPDESC,CIPCODE,CREDLEV,CREDDESC,EARN_MDN_HI_2YR) %>%
  filter(str_detect(EARN_MDN_HI_2YR,"Privacy",negate = TRUE)) %>%
  mutate(EARN_MDN_HI_2YR = as.numeric(EARN_MDN_HI_2YR))

majors_2years_ago <- raw_majors_2years_ago %>%
  select(UNITID,INSTNM,CIPDESC,CIPCODE,CREDLEV,CREDDESC,DEBT_ALL_STGP_EVAL_MDN) %>%
  filter(str_detect(DEBT_ALL_STGP_EVAL_MDN,"Privacy",negate = TRUE)) %>%
  mutate(DEBT_ALL_STGP_EVAL_MDN = as.numeric(DEBT_ALL_STGP_EVAL_MDN))

# Join to get debt at time of graduation and earnings two-years later for the same people
debt_vs_earnings <- inner_join(majors_2years_ago,majors_current) %>%
  mutate(CREDDESC = fct_reorder(as_factor(CREDDESC),CREDLEV)) %>%
  mutate(debt_ratio = EARN_MDN_HI_2YR/DEBT_ALL_STGP_EVAL_MDN) %>%
  mutate(CIPDESC = as.factor(CIPDESC))

colleges = c("Colgate University","Denison University")
subset_field <- debt_vs_earnings %>%
  filter(INSTNM %in% colleges) %>%
  filter(CREDLEV == 3)

limits = c(
  min(subset_field$DEBT_ALL_STGP_EVAL_MDN,subset_field$EARN_MDN_HI_2YR),
  max(subset_field$DEBT_ALL_STGP_EVAL_MDN,subset_field$EARN_MDN_HI_2YR)
)
subset_field %>%
  ggplot(aes(DEBT_ALL_STGP_EVAL_MDN,EARN_MDN_HI_2YR,color=INSTNM,label=CIPDESC)) +
  geom_point() +
#  geom_text() +
  geom_abline(slope = 1,intercept = 0) +
  scale_x_continuous(limits=limits,label=scales::dollar) +
  scale_y_continuous(limits=limits,label=scales::dollar) +
  labs(title = "College Earnings vs. Debt",
       subtitle = "Selected Colleges",
       x= "Debt at Graduation",
       y = "Earnings Two Years After Graduation",
       caption = "Source: U.S. Dept. of Education")

subset_field %>%
  ggplot(aes(CIPDESC,EARN_MDN_HI_2YR,color=INSTNM)) +
#  geom_col(position = "dodge") +
  scale_y_continuous(label=scales::dollar) +
  geom_point() +
  coord_flip() +
  labs(title = "Earnings for 2017 Graduates",
       subtitle = "Only Concentrations With Sufficient Data",
       y="Median Earnings 2 Years After Graduation",
       x= "Concentration",
       caption = "Source: U.S. Dept. of Education")
