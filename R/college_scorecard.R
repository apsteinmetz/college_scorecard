
# analyze college outcomes vs debt
library(tidyverse)
library(ggmulti) # for ggplot glyphs
library(png)

# load data
raw_score <- read_csv("data/Most-Recent-Cohorts-All-Data-Elements.csv")
profiles <- raw_score %>% transmute(UNITID,college = INSTNM,
                                  type = SCHTYPE,
                                  enrollment = UGDS,
                                  avg_net_priv = NPT4_PRIV,
                                  avg_net_pub = NPT4_PUB,
                                  avg_net_other = NPT4_PROG,
                                  avg_cost = COSTT4_A,
                                  net_cost_low_income_priv = NPT41_PRIV,
                                  net_cost_low_income_pub = NPT41_PUB,
                                  med_fam_income = MD_FAMINC,
                                  middle_income_students = INC_PCT_M1,
                                  debt = DEBT_MDN,
                                  debt_low_income = LO_INC_DEBT_MDN,
                                  adm_rate = ADM_RATE,
                                  sat_test_avg = SAT_AVG,
                                  act_test_mid = ACTMTMID,
                                  pct_white = UGDS_WHITE,
                                  pct_black = UGDS_BLACK,
                                  completion_rate_white = C150_4_WHITE,
                                  completion_rate_black = C150_4_BLACK) %>%
  mutate(type=fct_recode(as.factor(type),public="1",private="2",for_profit="3",unspecified="NULL"))
profiles <- profiles %>% mutate(across(4:ncol(profiles),as.numeric))

#data source: https://data.ed.gov/dataset/college-scorecard-all-data-files-through-6-2020/resources
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

colleges = c(peers_finance$college,"Elon University")

denison_majors <- debt_vs_earnings %>%
  filter(college == "Denison University") %>%
  pull(concentration) %>%
  unique() %>%
  as.character()

subset_field <- debt_vs_earnings %>%
  filter(college %in% colleges) %>%
  left_join(peers_finance) %>%
  filter(concentration %in% denison_majors) %>%
  filter(CREDLEV == 3) %>%  #Bachelors
  mutate(college = str_remove(college," College| University")) %>%
  mutate(college = str_remove(college,"The of "))


plot_limits = c(
  min(subset_field$debt,subset_field$earnings),
  max(subset_field$debt,subset_field$earnings)
)

# Choose highlight schools in plots
highlight_schools = c("Denison")
subset_highlight = filter(subset_field,college %in% highlight_schools)

# -------------------------------------------------
# display profiles
subset_profiles <- profiles %>%
  filter(college %in% colleges) %>%
  select(!contains("pub")) %>%
  select(!contains("other")) %>%
  mutate(college = str_remove(college," College| University")) %>%
  mutate(college = str_remove(college,"The of ")) %>%
  mutate(college = as.factor(college))

subset_profiles %>%
  select(college,avg_cost,avg_net_priv,net_cost_low_income_priv,debt,debt_low_income) %>%
  mutate(across(where(is.double),~scales::dollar(.x,trim=FALSE),.names="{.col}")) %>%
  arrange(avg_cost) %>%
  # kableExtra::kable() %>%
  {.}

# ------------------------------------------------
# profile plots
subset_profiles %>%
  # pivot_longer(c(avg_cost,avg_net_priv,net_cost_low_income_priv),names_to = "cost_type",values_to = "amount") %>%
  # group_by(college) %>%
  ggplot() +
  geom_col(aes(reorder(college,avg_net_priv),avg_cost),fill="blue") +
  geom_col(aes(reorder(college,avg_net_priv),avg_net_priv)) +
  labs(title= "Gross and Net Cost",
       y = "Average Cost",
       x= "",
       caption = "Source: U.S. Dept. of Education") +
  coord_flip()

subset_profiles %>%
  ggplot() +
  geom_col(aes(reorder(college,pct_white),pct_white),fill="tan") +
  geom_col(aes(reorder(college,pct_white),pct_black),fill="black") +
  scale_y_continuous(label=scales::percent) +
  labs(title= "Fraction of Students Who are White and Black",
       y = "",
       x= "",
       caption = "Source: U.S. Dept. of Education") +
  coord_flip()


subset_profiles %>%
  ggplot() +
  geom_point(aes(1-avg_net_priv/avg_cost,pct_white)) +
  #  geom_text(aes(x=1-avg_net_priv/avg_cost,y=pct_white,label = college)) +
  ggrepel::geom_text_repel(aes(x=1-avg_net_priv/avg_cost,y=pct_white,label = college)) +
  scale_y_continuous(label=scales::percent) +
  scale_x_continuous(label=scales::percent) +
  labs(title= "Race Vs. Tuition Discounts",
       y = "Fraction of White Students",
       x= "Tuition Discount",
       caption = "Source: U.S. Dept. of Education")

subset_profiles %>%
  ggplot() +
  geom_point(aes(1-avg_net_priv/avg_cost,med_fam_income)) +
  #  geom_text(aes(x=1-avg_net_priv/avg_cost,y=pct_white,label = college)) +
  ggrepel::geom_text_repel(aes(x=1-avg_net_priv/avg_cost,y=med_fam_income,label = college)) +
  scale_y_continuous(label=scales::dollar) +
  scale_x_continuous(label=scales::percent) +
  labs(title= "Family Income Vs. Tuition Discounts",
       y = "Median Family Income",
       x= "Tuition Discount",
       caption = "Source: U.S. Dept. of Education")

subset_profiles %>%
  ggplot() +
  geom_point(aes(sat_test_avg,adm_rate)) +
  #  geom_text(aes(x=1-avg_net_priv/avg_cost,y=pct_white,label = college)) +
  ggrepel::geom_text_repel(aes(x=sat_test_avg,y=adm_rate,label = college)) +
  scale_y_continuous(label=scales::percent) +
  # scale_x_continuous(label=scales::percent) +
  labs(title= "Test Scores Vs. Selectivity",
       y = "Adminssion Rate",
       x= "SAT Test Average",
       caption = "Source: U.S. Dept. of Education")

subset_profiles %>%
  ggplot() +
  geom_col(aes(reorder(college,debt_low_income),debt),fill="orange") +
  geom_col(aes(reorder(college,debt_low_income),debt_low_income),fill="red") +
  scale_y_continuous(label=scales::dollar) +
  labs(title= "Debt Principal for All and Low-Income-Family Graduates",
       y = "",
       x= "",
       subtitle = "Red is Debt of Low Income Students",
       caption = "Source: U.S. Dept. of Education") +
  coord_flip()
# ------------------------------------------------
# concentration plots
subset_field %>%
  ggplot(aes(comp_fee_2122,earnings,color= college)) +
  geom_point() +
  geom_line() +
  #  geom_text() +
  #  scale_x_continuous(limits=plot_limits,label=scales::dollar) +
  #  scale_y_continuous(limits=plot_limits,label=scales::dollar) +
  scale_x_continuous(label=scales::dollar) +
  scale_y_continuous(limits=c(0,max(subset_field$earnings)),label=scales::dollar) +
  labs(title = "College Price vs. Earnings",
       subtitle = "Only Concentrations at Peer Colleges\nWith Sufficient Data and Offered at Denison",
       x= "Full Price",
       y = "Earnings Two Years After Graduation",
       caption = "Source: U.S. Dept. of Education") +
  #  geom_point(data=subset_highlight,size = 5,color = "red") +
  #  geom_text(data=subset_highlight,label = "DU",color = "red") +
  # geom_image_glyph(data=subset_highlight,
  #                  mapping = aes(x = debt, y = earnings),
  #                  imagewidth = 0.5,
  #                  imageheight = 0.5,
  #                  colour = NA,
  #                  interpolate = TRUE,
  #                  images=d_glyph) +
  geom_text(aes(comp_fee_2122,10000,label=college),color="black",angle=0,alpha = 0.2) +
  theme(legend.position = "none") +
  coord_flip()

# debt
subset_field %>%
  ggplot(aes(debt,earnings,color=college,label=concentration)) +
  geom_point() +
  #  geom_text() +
  geom_abline(slope = 1,intercept = 0) +
  #  scale_x_continuous(limits=plot_limits,label=scales::dollar) +
  #  scale_y_continuous(limits=plot_limits,label=scales::dollar) +
  scale_x_continuous(label=scales::dollar) +
  scale_y_continuous(label=scales::dollar) +
  labs(title = "College Earnings vs. Debt",
       subtitle = "Only Concentrations at Peer Colleges\nWith Sufficient Data and Offered at Denison",
       x= "Debt at Graduation",
       y = "Earnings Two Years After Graduation",
       caption = "Source: U.S. Dept. of Education") +
  #  geom_point(data=subset_highlight,size = 5,color = "red") +
  geom_text(data=subset_highlight,label = "DU",color = "red") +
  # geom_image_glyph(data=subset_highlight,
  #                  mapping = aes(x = debt, y = earnings),
  #                  imagewidth = 0.5,
  #                  imageheight = 0.5,
  #                  colour = NA,
  #                  interpolate = TRUE,
  #                  images=d_glyph) +
  theme(legend.position = "none")

# Earnings
subset_field %>%
  ggplot(aes(concentration,earnings)) +
  scale_y_continuous(label=scales::dollar) +
  geom_point() +
  geom_boxplot() +
  coord_flip() +
  labs(title = "Earnings for 2017 Graduates",
       subtitle = "Only Concentrations at Peer Colleges\nWith Sufficient Data and Offered at Denison",
       y="Median Earnings 2 Years After Graduation",
       x= "Concentration",
       caption = "Source: U.S. Dept. of Education") +
  geom_text(data=subset_highlight,label = "DU",color = "red") +
  # geom_image_glyph(data=subset_highlight,
  #                  mapping = aes(x = debt, y = earnings),
  #                  imagewidth = 0.5,
  #                  imageheight = 0.5,
  #                  colour = NA,
  #                  interpolate = TRUE,
  #                  images=d_glyph) +
  theme(legend.position = "none")

# Debt
subset_field %>%
  ggplot(aes(concentration,debt)) +
  geom_point() +
  geom_boxplot() +
  scale_y_continuous(label=scales::dollar) +
  coord_flip() +
  labs(title = "Debt for 2017 Graduates",
       subtitle = "Only Concentrations at Peer Colleges\n With Sufficient Data and Offered at Denison",
       y="Debt At Time Of Graduation",
       x= "Concentration",
       caption = "Source: U.S. Dept. of Education") +
  geom_text(data=subset_highlight,label = "DU",color = "red") +
  # geom_image_glyph(data=subset_highlight,
  #                  mapping = aes(x = debt, y = earnings),
  #                  imagewidth = 0.5,
  #                  imageheight = 0.5,
  #                  colour = NA,
  #                  interpolate = TRUE,
  #                  images=d_glyph) +
  theme(legend.position = "none")

# Earnings/Debt Ratios
subset_field %>%
  ggplot(aes(concentration,debt_ratio)) +
  geom_point() +
  geom_boxplot() +
  labs(title = "Earnings/Debt Ratio for 2017 Graduates",
       subtitle = "Only Concentrations at Peer Colleges\nWith Sufficient Data and Offered at Denison",
       y="Ratio of Earnings in 2019 to Debt in 2017",
       x= "Concentration",
       caption = "Source: U.S. Dept. of Education") +
  geom_text(data=subset_highlight,label = "DU",color = "red") +
  # geom_image_glyph(data=subset_highlight,
  #                  mapping = aes(x = debt, y = earnings),
  #                  imagewidth = 0.5,
  #                  imageheight = 0.5,
  #                  colour = NA,
  #                  interpolate = TRUE,
  #                  images=d_glyph) +
  coord_flip()

