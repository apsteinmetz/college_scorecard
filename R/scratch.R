#scratch
temp <- subset_profiles %>%
  mutate(pct_other = 1-pct_white-pct_black) %>%
  arrange(pct_white) %>%
  mutate(college = as_factor(college)) %>%
  pivot_longer(starts_with("pct"),names_to = "Race",values_to = "percent") %>%
  group_by(college)

temp %>%
  ggplot(aes(college,percent,fill=Race)) +
  geom_col() +
  scale_y_continuous(label=scales::percent) +
#  labs(title= "Fraction of Students Who are White and Black",
#       y = "",
#       x= "",
#       caption = "Source: U.S. Dept. of Education") +
  coord_flip()

