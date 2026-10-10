library(shiny)
library(bslib)
library(dplyr)
library(plotly)

# Built by R/build_app_data.R
d <- readRDS("data/hea_app_data.rds") |>
  filter(!is.na(latitude), is.finite(earn_ratio)) |>
  rename(ratio = earn_ratio)

# One dot per school / per major. Ratios are completer-weighted so large
# programs count more; the major plot's y position is the completer-weighted
# mean latitude of the schools offering that major.
school_pts <- d |>
  summarise(
    ratio = sum(earnings * completers) / sum(benchmark * completers),
    latitude = first(latitude),
    .by = c(opeid6, school)
  )

major_pts <- d |>
  summarise(
    ratio = sum(earnings * completers) / sum(benchmark * completers),
    latitude = weighted.mean(latitude, completers),
    .by = major
  )

# Highlight colors, one per filter, ordered lowest to highest drawing priority.
pal <- c(
  none = "#CCCCCC",
  credential = "#FF7F0E",
  conference = "#D62728",
  category = "#17BECF",
  major = "#2CA02C",
  school = "#1F77B4"
)

# Colors for individual schools when 2+ are selected (recycled beyond 8).
# Avoids the filter colors above (blue, green, red, orange, grey).
school_colors <- c("#9467BD", "#8C564B", "#E377C2", "#666666", "#BCBD22", "#000000", "#003F5C", "#FFD700")

filter_ids <- c("school", "major", "category", "conference", "credential")

legend_item <- function(color, label) {
  span(
    style = "display:inline-flex;align-items:center;margin-right:10px;",
    span(style = sprintf(
      "display:inline-block;width:10px;height:10px;border-radius:50%%;background:%s;margin-right:4px;", color
    )),
    label
  )
}

dotplot <- function(df, colors, size = 5, alpha = 0.6) {
  # Built directly with plot_ly (not ggplotly) so scattergl traces carry no
  # unsupported attributes. One trace per class, drawn in palette order so
  # highlights sit on top. Only highlighted traces get hover tooltips.
  p <- plot_ly()
  for (cl in names(colors)) {
    sub <- df[df$cls == cl, , drop = FALSE]
    if (nrow(sub) == 0) next
    highlighted <- cl != "none"
    p <- add_trace(
      p,
      type = "scattergl", mode = "markers",
      data = sub, x = ~ratio, y = ~latitude,
      marker = list(color = colors[[cl]], size = size, opacity = alpha),
      text = ~label,
      hoverinfo = if (highlighted) "text" else "skip",
      showlegend = FALSE
    )
  }
  layout(
    p,
    xaxis = list(title = "Earnings / baseline", zeroline = FALSE),
    yaxis = list(title = "Latitude", zeroline = FALSE),
    hovermode = "closest",
    shapes = list(list(
      type = "line", x0 = 1, x1 = 1, y0 = 0, y1 = 1, yref = "paper",
      line = list(color = "grey", dash = "dash", width = 1)
    ))
  )
}

ratio_txt <- function(x) sprintf("%.2f", x)

ui <- page_sidebar(
  title = "HEA Earning Scorecard",
  sidebar = sidebar(
    width = 340,
    selectizeInput("school", "School", choices = NULL, multiple = TRUE,
                   options = list(placeholder = "All")),
    selectizeInput("category", "Major category", choices = NULL, multiple = TRUE,
                   options = list(placeholder = "All")),
    selectizeInput("major", "Major", choices = NULL, multiple = TRUE,
                   options = list(placeholder = "All")),
    selectizeInput("conference", "Athletic conference", choices = NULL, multiple = TRUE,
                   options = list(placeholder = "All")),
    selectizeInput("credential", "Credential", choices = NULL, multiple = TRUE,
                   options = list(placeholder = "All")),
    actionButton("clear", "Clear Filters", class = "btn-outline-secondary"),
    hr(),
    uiOutput("legend"),
    helpText(
      "Leave a filter empty to ignore it. Each active filter highlights its own",
      "matches in its color; where they overlap, school wins over major, then",
      "major category, conference, and credential. With 2+ schools selected, each school gets",
      "its own color. On the major plot, each selected school adds its own dot",
      "per major it offers (at that school's latitude and ratio).",
      "Hover details appear only on highlighted dots.",
      "Dashed line = baseline (ratio of 1). Conferences are NCAA primary",
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
  updateSelectizeInput(session, "credential", choices = sort(unique(d$credential)), server = TRUE)

  observeEvent(input$clear, {
    for (id in filter_ids) updateSelectizeInput(session, id, selected = character(0))
  })

  # Row-level flags: TRUE when the row matches that filter's selection.
  # Inactive (empty) filters never highlight.
  flagged <- reactive({
    sel <- list(
      school = input$school, major = input$major, category = input$category,
      conference = input$conference, credential = input$credential
    )
    d |>
      mutate(
        f_school = school %in% sel$school,
        f_major = major %in% sel$major,
        f_category = category %in% sel$category,
        f_conference = conference %in% sel$conference,
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
      legend_item(pal[["credential"]], "Credential")
    )
  })

  # One display class per dot by priority: school > major > conference > credential.
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
          f_credential ~ "credential",
          .default = "none"
        )
      )
  }

  flag_cols <- c("f_school", "f_major", "f_category", "f_conference", "f_credential")

  output$p_both <- renderPlotly({
    flagged() |>
      classify() |>
      mutate(label = paste0(school, "<br>", major, " (", credential, ")<br>Ratio: ", ratio_txt(ratio))) |>
      dotplot(all_pal(), size = 4, alpha = 0.5)
  })

  output$p_school <- renderPlotly({
    # A school dot is highlighted for a filter if any of its rows matches it
    fl <- flagged() |> summarise(across(all_of(flag_cols), any), .by = opeid6)
    school_pts |>
      left_join(fl, by = "opeid6") |>
      classify() |>
      mutate(label = paste0(school, "<br>Ratio: ", ratio_txt(ratio))) |>
      dotplot(all_pal(), size = 6)
  })

  output$p_major <- renderPlotly({
    sel_schools <- input$school
    fl <- flagged() |> summarise(across(all_of(flag_cols), any), .by = major)
    base <- major_pts |>
      left_join(fl, by = "major") |>
      # Selected schools get their own dots below, so the aggregate dot ignores the school filter
      mutate(f_school = FALSE) |>
      classify() |>
      mutate(label = paste0(major, "<br>Ratio: ", ratio_txt(ratio))) |>
      select(ratio, latitude, cls, label)

    if (length(sel_schools) == 0) return(dotplot(base, all_pal(), size = 7))

    # One dot per selected school and major offered, at that school's latitude
    per_school <- length(sel_schools) >= 2
    own <- d |>
      filter(school %in% sel_schools) |>
      summarise(
        ratio = sum(earnings * completers) / sum(benchmark * completers),
        latitude = first(latitude),
        .by = c(school, major)
      ) |>
      mutate(
        cls = if (per_school) paste0("school:", school) else "school",
        label = paste0(school, "<br>", major, "<br>Ratio: ", ratio_txt(ratio))
      ) |>
      select(ratio, latitude, cls, label)

    dotplot(bind_rows(base, own), all_pal(), size = 7)
  })
}

shinyApp(ui, server)
