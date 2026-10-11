library(shiny)
library(bslib)
library(dplyr)
library(plotly)

# Built by R/build_app_data.R. Shiny runs the app with its directory as the
# working directory, so the data folder is one level up.
d <- readRDS("../data/hea_app_data.rds") |>
  filter(!is.na(latitude), is.finite(earn_ratio)) |>
  rename(ratio = earn_ratio) |>
  mutate(dollars = earnings)

# One dot per school / per major. Ratios and dollars are completer-weighted so
# large programs count more; the major plot's y position is the
# completer-weighted mean latitude of the schools offering that major.
school_pts <- d |>
  summarise(
    ratio = sum(earnings * completers) / sum(benchmark * completers),
    dollars = sum(earnings * completers) / sum(completers),
    latitude = first(latitude),
    .by = c(opeid6, school)
  )

major_pts <- d |>
  summarise(
    ratio = sum(earnings * completers) / sum(benchmark * completers),
    dollars = sum(earnings * completers) / sum(completers),
    latitude = weighted.mean(latitude, completers),
    .by = major
  )

# Highlight colors, one per filter, ordered lowest to highest drawing priority.
pal <- c(
  none = "#CCCCCC",
  credential = "#FF7F0E",
  state = "#E7298A",
  conference = "#D62728",
  category = "#17BECF",
  major = "#2CA02C",
  school = "#1F77B4"
)

# Colors for individual schools when 2+ are selected (recycled beyond 8).
# Avoids the filter colors above (blue, green, red, orange, grey).
school_colors <- c("#9467BD", "#8C564B", "#E377C2", "#666666", "#BCBD22", "#000000", "#003F5C", "#FFD700")

filter_ids <- c("school", "major", "category", "conference", "state", "credential")

legend_item <- function(color, label) {
  span(
    style = "display:inline-flex;align-items:center;margin-right:10px;",
    span(style = sprintf(
      "display:inline-block;width:10px;height:10px;border-radius:50%%;background:%s;margin-right:4px;", color
    )),
    label
  )
}

# metric: "ratio" (earnings / baseline) or "dollars" (median earnings)
dotplot <- function(df, colors, metric = "ratio", size = 5, alpha = 0.6) {
  # Built directly with plot_ly (not ggplotly) so scattergl traces carry no
  # unsupported attributes. One trace per class, drawn in palette order so
  # highlights sit on top. Only highlighted traces get hover tooltips.
  is_ratio <- metric == "ratio"
  p <- plot_ly()
  for (cl in names(colors)) {
    sub <- df[df$cls == cl, , drop = FALSE]
    if (nrow(sub) == 0) next
    highlighted <- cl != "none"
    p <- add_trace(
      p,
      type = "scattergl", mode = "markers",
      data = sub, x = sub[[metric]], y = ~latitude,
      marker = list(color = colors[[cl]], size = size, opacity = alpha),
      text = ~label,
      hoverinfo = if (highlighted) "text" else "skip",
      showlegend = FALSE
    )
  }
  layout(
    p,
    xaxis = if (is_ratio) {
      list(title = "Earnings / baseline", zeroline = FALSE)
    } else {
      list(title = "Median earnings", zeroline = FALSE, tickprefix = "$", tickformat = ",.0f")
    },
    yaxis = list(title = "Latitude", zeroline = FALSE),
    hovermode = "closest",
    # Baseline line only meaningful on the ratio scale
    shapes = if (is_ratio) list(list(
      type = "line", x0 = 1, x1 = 1, y0 = 0, y1 = 1, yref = "paper",
      line = list(color = "grey", dash = "dash", width = 1)
    )) else list()
  )
}

ratio_txt <- function(x) sprintf("%.2f", x)
dollar_txt <- function(x) paste0("$", formatC(round(x), big.mark = ",", format = "d"))
value_txt <- function(ratio, dollars) {
  paste0("Ratio: ", ratio_txt(ratio), "<br>Earnings: ", dollar_txt(dollars))
}

ui <- page_sidebar(
  title = "HEA Earning Scorecard",
  sidebar = sidebar(
    width = 340,
    input_switch("dollars", "Show earnings ($) instead of ratio"),
    selectizeInput("school", "School", choices = NULL, multiple = TRUE,
                   options = list(placeholder = "All")),
    selectizeInput("category", "Major category", choices = NULL, multiple = TRUE,
                   options = list(placeholder = "All")),
    selectizeInput("major", "Major", choices = NULL, multiple = TRUE,
                   options = list(placeholder = "All")),
    selectizeInput("conference", "Athletic conference", choices = NULL, multiple = TRUE,
                   options = list(placeholder = "All")),
    selectizeInput("state", "State", choices = NULL, multiple = TRUE,
                   options = list(placeholder = "All")),
    selectizeInput("credential", "Credential", choices = NULL, multiple = TRUE,
                   options = list(placeholder = "All")),
    actionButton("clear", "Clear Filters", class = "btn-outline-secondary"),
    hr(),
    uiOutput("legend"),
    helpText(
      "Leave a filter empty to ignore it. Each active filter highlights its own",
      "matches in its color; where they overlap, school wins over major, then",
      "major category, conference, state, and credential. With 2+ schools selected, each school gets",
      "its own color. On the major plot, each selected school adds its own dot",
      "per major it offers (at that school's latitude and value).",
      "Hover details appear only on highlighted dots.",
      "Dashed line = baseline (ratio of 1; ratio view only). School and major",
      "dots are completer-weighted. Conferences are NCAA primary",
      "affiliations matched by school name; unmatched schools are labeled",
      "'No NCAA conference found'."
    )
  ),
  layout_columns(
    col_widths = c(12, 6, 6),
    row_heights = c(1.2, 1),
    card(card_header("One dot per school and major"), plotlyOutput("p_both")),
    card(card_header("One dot per school"), plotlyOutput("p_school")),
    card(card_header("One dot per major"), plotlyOutput("p_major"))
  )
)

server <- function(input, output, session) {
  updateSelectizeInput(session, "school", choices = sort(unique(d$school)), server = TRUE)
  updateSelectizeInput(session, "category", choices = sort(unique(d$category)), server = TRUE)
  updateSelectizeInput(session, "major", choices = sort(unique(d$major)), server = TRUE)
  updateSelectizeInput(session, "conference", choices = sort(unique(d$conference)), server = TRUE)
  updateSelectizeInput(session, "state", choices = sort(unique(d$state)), server = TRUE)
  updateSelectizeInput(session, "credential", choices = sort(unique(d$credential)), server = TRUE)

  observeEvent(input$clear, {
    for (id in filter_ids) updateSelectizeInput(session, id, selected = character(0))
  })

  # Row-level flags: TRUE when the row matches that filter's selection.
  # Inactive (empty) filters never highlight.
  flagged <- reactive({
    sel <- list(
      school = input$school, major = input$major, category = input$category,
      conference = input$conference, state = input$state,
      credential = input$credential
    )
    d |>
      mutate(
        f_school = school %in% sel$school,
        f_major = major %in% sel$major,
        f_category = category %in% sel$category,
        f_conference = conference %in% sel$conference,
        f_state = state %in% sel$state,
        f_credential = credential %in% sel$credential
      )
  })

  # Per-school colors apply only when 2+ schools are selected
  school_pal <- reactive({
    sel <- input$school
    if (length(sel) < 2) return(character(0))
    setNames(rep_len(school_colors, length(sel)), paste0("school:", sel))
  })

  # Full palette: lowest to highest drawing priority, per-school colors on top
  all_pal <- reactive(c(pal[names(pal) != "school"], pal["school"], school_pal()))

  output$legend <- renderUI({
    sp <- school_pal()
    school_items <- if (length(sp)) {
      Map(legend_item, unname(sp), sub("^school:", "", names(sp)))
    } else {
      list(legend_item(pal[["school"]], "School"))
    }
    div(
      class = "small",
      school_items,
      legend_item(pal[["major"]], "Major"),
      legend_item(pal[["category"]], "Major category"),
      legend_item(pal[["conference"]], "Conference"),
      legend_item(pal[["state"]], "State"),
      legend_item(pal[["credential"]], "Credential")
    )
  })

  # One display class per dot by priority:
  # school > major > category > conference > state > credential.
  # When per-school colors are active and the dot has a single school, use its name.
  classify <- function(df) {
    per_school <- length(school_pal()) > 0 && "school" %in% names(df)
    school_name <- if ("school" %in% names(df)) df$school else NA_character_
    df |>
      mutate(
        cls = case_when(
          f_school & per_school ~ paste0("school:", school_name),
          f_school ~ "school",
          f_major ~ "major",
          f_category ~ "category",
          f_conference ~ "conference",
          f_state ~ "state",
          f_credential ~ "credential",
          .default = "none"
        )
      )
  }

  flag_cols <- c("f_school", "f_major", "f_category", "f_conference", "f_state", "f_credential")

  metric <- reactive(if (isTRUE(input$dollars)) "dollars" else "ratio")

  output$p_both <- renderPlotly({
    one_school <- length(input$school) == 1
    flagged() |>
      mutate(
        # A selected school's rows highlight only if they match any major/category selection
        f_school = f_school &
          (length(input$major) == 0 | f_major) &
          (length(input$category) == 0 | f_category),
        # With one school selected, other schools' majors aren't highlighted
        f_major = f_major & !one_school,
        f_category = f_category & !one_school
      ) |>
      classify() |>
      mutate(label = paste0(school, "<br>", major, " (", credential, ")<br>", value_txt(ratio, dollars))) |>
      dotplot(all_pal(), metric(), size = 4, alpha = 0.5)
  })

  output$p_school <- renderPlotly({
    # A school dot is highlighted for a filter if any of its rows matches it
    fl <- flagged() |> summarise(across(all_of(flag_cols), any), .by = opeid6)
    school_pts |>
      left_join(fl, by = "opeid6") |>
      classify() |>
      mutate(label = paste0(school, "<br>", value_txt(ratio, dollars))) |>
      dotplot(all_pal(), metric(), size = 6)
  })

  output$p_major <- renderPlotly({
    sel_schools <- input$school
    fl <- flagged() |> summarise(across(all_of(flag_cols), any), .by = major)
    base <- major_pts |>
      left_join(fl, by = "major") |>
      # Selected schools get their own dots below, so the aggregate dot ignores the school filter
      mutate(f_school = FALSE) |>
      # With one school selected, only that school's own major dots are highlighted
      mutate(
        f_major = f_major & length(sel_schools) != 1,
        f_category = f_category & length(sel_schools) != 1
      ) |>
      classify() |>
      mutate(label = paste0(major, "<br>", value_txt(ratio, dollars))) |>
      select(ratio, dollars, latitude, cls, label)

    if (length(sel_schools) == 0) return(dotplot(base, all_pal(), metric(), size = 7))

    # One dot per selected school and major offered, at that school's latitude
    per_school <- length(sel_schools) >= 2
    own <- d |>
      filter(school %in% sel_schools,
             length(input$major) == 0 | major %in% input$major,
             length(input$category) == 0 | category %in% input$category) |>
      summarise(
        ratio = sum(earnings * completers) / sum(benchmark * completers),
        dollars = sum(earnings * completers) / sum(completers),
        latitude = first(latitude),
        .by = c(school, major)
      ) |>
      mutate(
        cls = if (per_school) paste0("school:", school) else "school",
        label = paste0(school, "<br>", major, "<br>", value_txt(ratio, dollars))
      ) |>
      select(ratio, dollars, latitude, cls, label)

    dotplot(bind_rows(base, own), all_pal(), metric(), size = 7)
  })
}

shinyApp(ui, server)
