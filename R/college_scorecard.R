# analyze college outcomes vs debt
library(tidyverse)
library(ggmulti) # for ggplot glyphs
library(png)

# load data
# raw_score <- read_csv("data/Most-Recent-Cohorts-All-Data-Elements.csv")

raw_majors_current <- read_csv("data/Most-Recent-Cohorts-Field-of-Study.csv")

raw_majors_2years_ago <- read_csv("data/FieldOfStudyData1415_1516_PP.csv")

peers_finance <- read_csv("data/peer list finance.csv")
peers_kb <- read_csv("data/peer list kb.csv")
d_glpyh_file <- "images/denison-big-red-athletics-no-ribbon.png"
d_glyph <- png::readPNG(d_glpyh_file)

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
  mutate(CIPDESC = as.factor(CIPDESC)) %>%
  rename(college = INSTNM,
         concentration = CIPDESC,
         earnings=EARN_MDN_HI_2YR,
         debt=DEBT_ALL_STGP_EVAL_MDN,
         degree=CREDDESC)

colleges = peers_finance$college

denison_majors <- debt_vs_earnings %>%
  filter(college == "Denison University") %>%
  pull(concentration) %>%
  unique() %>%
  as.character()

subset_field <- debt_vs_earnings %>%
  filter(college %in% colleges) %>%
  filter(concentration %in% denison_majors) %>%
  filter(CREDLEV == 3) #Bachelors


plot_limits = c(
  min(subset_field$debt,subset_field$earnings),
  max(subset_field$debt,subset_field$earnings)
)

# Choose highlight schools in plots
highlight_schools = c("Denison University")
subset_highlight = filter(subset_field,college %in% highlight_schools)


# ------------------------------------------------
# PLOTS
subset_field %>%
  ggplot(aes(debt,earnings,color=college,label=concentration)) +
  geom_point() +
#  geom_text() +
  geom_abline(slope = 1,intercept = 0) +
  scale_x_continuous(limits=plot_limits,label=scales::dollar) +
  scale_y_continuous(limits=plot_limits,label=scales::dollar) +
  labs(title = "College Earnings vs. Debt",
       subtitle = "Selected Colleges, Denison Highlighted",
       x= "Debt at Graduation",
       y = "Earnings Two Years After Graduation",
       caption = "Source: U.S. Dept. of Education") +
#  geom_point(data=subset_highlight,size = 5,color = "red") +
  geom_image_glyph(data=subset_highlight,
                   mapping = aes(x = debt, y = earnings),
                   imagewidth = 0.5,
                   imageheight = 0.5,
                   colour = NA,
#                   size = 3,
                   images=d_glyph) +
  theme(legend.position = "none")

# Earnings
subset_field %>%
  ggplot(aes(concentration,earnings,color=college)) +
  scale_y_continuous(label=scales::dollar) +
  geom_point() +
  coord_flip() +
  labs(title = "Earnings for 2017 Graduates",
       subtitle = "Only Concentrations With Sufficient Data and Offered at Denison",
       y="Median Earnings 2 Years After Graduation",
       x= "Concentration",
       caption = "Source: U.S. Dept. of Education") +
#  geom_point(data=subset_highlight,size = 5,color = "red") +
  geom_text(data=subset_highlight,label = "DU",color = "red") +
  theme(legend.position = "none")

# Debt
subset_field %>%
  ggplot(aes(concentration,debt,color=college)) +
  scale_y_continuous(label=scales::dollar) +
  geom_point() +
  coord_flip() +
  labs(title = "Debt for 2017 Graduates",
       subtitle = "Only Concentrations With Sufficient Data and Offered at Denison",
       y="Debt At Time Of Graduation",
       x= "Concentration",
       caption = "Source: U.S. Dept. of Education")

# Earnings/Debt Ratiosubset_field %>%
subset_field %>%
  ggplot(aes(concentration,debt_ratio,color=college)) +
  geom_point() +
  coord_flip() +
  labs(title = "Earnings/Debt Ratio for 2017 Graduates",
       subtitle = "Only Concentrations With Sufficient Data and Offered at Denison",
       y="Ratio of Earnings in 2019 to Debt in 2017",
       x= "Concentration",
       caption = "Source: U.S. Dept. of Education")
