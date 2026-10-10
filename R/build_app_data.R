# Builds data/hea_app_data.rds for the HEA Earning Scorecard app.
# Sources:
#   - HEA earnings data (data/1.5.26+OBBBA.GE+Data.HEA+Group+.xlsx)
#   - College Scorecard (latitude, via OPEID6)
#   - NCAA conference membership scraped from Wikipedia (D-I, D-II, D-III)

library(dplyr)
library(stringr)
library(readr)
library(readxl)
library(rvest)

norm_name <- function(x) {
  x |>
    str_to_lower() |>
    str_replace_all("&", " and ") |>
    str_replace_all("[\u2013\u2014-]", " ") |>
    str_replace_all("\\bst\\.?\\b", "saint") |>
    str_replace_all("\\bthe\\b", " ") |>
    str_replace_all("\\buniv\\b", "university") |>
    str_replace_all("[^a-z0-9 ]", "") |>
    str_squish()
}

# ---- HEA earnings ----
hea <- read_excel("data/1.5.26+OBBBA.GE+Data.HEA+Group+.xlsx", sheet = "Data") |>
  rename(
    school = Institution, state = State, major = Major, credential = Credential,
    benchmark = `Earnings Benchmark`, earnings = `Earnings 4 Years After Completion`,
    completers = `# of Completers in Earnings Cohort`
  ) |>
  mutate(
    major = str_squish(str_remove(major, "\\.$")),
    earn_ratio = earnings / benchmark
  )

# ---- Latitude from Scorecard (one row per OPEID6; prefer main campus) ----
lat <- read_csv(
  "data/Most-Recent-Cohorts-All-Data-Elements.csv",
  col_select = c(UNITID, OPEID6, MAIN, LATITUDE, LONGITUDE),
  col_types = cols(.default = "c"), na = c("", "NA", "NULL", "PrivacySuppressed")
) |>
  mutate(opeid6 = as.numeric(OPEID6), latitude = as.numeric(LATITUDE),
         longitude = as.numeric(LONGITUDE), MAIN = as.numeric(MAIN)) |>
  filter(!is.na(latitude)) |>
  arrange(opeid6, desc(MAIN == 1), as.numeric(UNITID)) |>
  distinct(opeid6, .keep_all = TRUE) |>
  select(opeid6, latitude, longitude)

# ---- NCAA conferences from Wikipedia ----
wiki <- function(url, idx, state_col, conf_col) {
  t <- read_html(url) |> html_elements("table.wikitable") |> html_table()
  t <- t[[idx]]
  tibble(
    school_wiki = t[["School"]],
    state = t[[state_col]],
    conference = t[[conf_col]]
  )
}

ncaa <- bind_rows(
  wiki("https://en.wikipedia.org/wiki/List_of_NCAA_Division_I_institutions", 2, "State", "Primary") |>
    mutate(division = "D-I"),
  wiki("https://en.wikipedia.org/wiki/List_of_NCAA_Division_II_institutions", 1, "State/Province", "Conference") |>
    mutate(division = "D-II"),
  wiki("https://en.wikipedia.org/wiki/List_of_NCAA_Division_III_institutions", 1, "State", "Conference") |>
    mutate(division = "D-III")
) |>
  mutate(
    school_wiki = str_remove_all(school_wiki, "\\[[^]]*\\]"),
    conference = str_squish(str_remove_all(conference, "\\[[^]]*\\]|\\(.*\\)")),
    key = norm_name(school_wiki)
  ) |>
  filter(conference != "", !is.na(conference)) |>
  distinct(key, state, .keep_all = TRUE)

inst <- hea |> distinct(opeid6, school, state) |> mutate(key = norm_name(school))

ncaa_key <- ncaa |> select(key, state, conference, division)

conf <- inst |>
  left_join(ncaa_key, by = c("key", "state")) |>
  distinct(opeid6, .keep_all = TRUE)

# Second pass for names with a campus suffix (e.g. "Purdue University-Main Campus").
# Accept only when the stripped name matches exactly one HEA institution in the
# state and that NCAA school was not already matched.
fallback <- conf |>
  filter(is.na(conference)) |>
  mutate(key2 = norm_name(str_remove(school, "-.*$"))) |>
  inner_join(
    ncaa_key |> anti_join(conf |> filter(!is.na(conference)), by = c("key", "state")),
    by = c("key2" = "key", "state"), suffix = c("", "_f")
  ) |>
  add_count(key2, state) |>
  filter(n == 1) |>
  select(opeid6, conference_f, division_f)

conf <- conf |>
  left_join(fallback, by = "opeid6") |>
  mutate(conference = coalesce(conference, conference_f),
         division = coalesce(division, division_f)) |>
  select(-conference_f, -division_f)

message("Institutions: ", nrow(conf), "; matched to an NCAA conference: ", sum(!is.na(conf$conference)))
write_csv(conf |> select(opeid6, school, state, division, conference), "data/conferences.csv", na = "")

# Broad major categories from the 2-digit CIP family (first two digits of cip4)
category_map <- tibble::tribble(
  ~category, ~cip2,
  "Agriculture & Natural Resources", c(1, 3),
  "Arts, Design & Architecture", c(4, 50),
  "Business", 52,
  "Communication & Media", c(9, 10),
  "Computer Science & Math", c(11, 27),
  "Education", 13,
  "Engineering & Engineering Technology", c(14, 15),
  "Health & Fitness", c(31, 51),
  "Humanities", c(5, 16, 23, 24, 38, 39, 54),
  "Natural Sciences", c(26, 40, 41),
  "Public Safety & Legal", c(22, 29, 43),
  "Skilled Trades & Services", c(12, 46, 47, 48, 49),
  "Social Sciences & Human Services", c(19, 42, 44, 45),
  "Interdisciplinary & Other", c(25, 30, 36)
) |>
  tidyr::unnest(cip2)

hea <- hea |>
  mutate(cip2 = cip4 %/% 100) |>
  left_join(category_map, by = "cip2")

stopifnot(!anyNA(hea$category))

app_data <- hea |>
  left_join(lat, by = "opeid6") |>
  left_join(conf |> select(opeid6, division, conference), by = "opeid6") |>
  mutate(conference = coalesce(conference, "No NCAA conference found"))

saveRDS(app_data, "data/hea_app_data.rds")
message("Rows: ", nrow(app_data), "; missing latitude: ", sum(is.na(app_data$latitude)))
