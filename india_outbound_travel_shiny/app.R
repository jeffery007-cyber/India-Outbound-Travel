# ============================================================
# Indian Outbound Travel Analytics - R Shiny Application
# Source: Ministry of Tourism, Government of India
# Underlying source: Bureau of Immigration (BOI), Govt. of India
# ============================================================

required <- c("shiny", "bslib", "dplyr", "ggplot2", "plotly", "leaflet", "DT", "scales")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing) > 0) {
  install.packages(missing, repos = "https://cloud.r-project.org")
}

library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(plotly)
library(leaflet)
library(DT)
library(scales)

# ---------- Load data ----------
top15 <- read.csv("data/outbound_india_top15.csv", stringsAsFactors = FALSE)
totals <- read.csv("data/outbound_india_totals.csv", stringsAsFactors = FALSE)
monthly <- read.csv("data/monthly_departures_2023_2024.csv", stringsAsFactors = FALSE)

top15$Year <- as.integer(top15$Year)
totals$Year <- as.integer(totals$Year)
monthly$Year <- as.integer(monthly$Year)

# Approximate country coordinates for visualization.
# These are map display coordinates, not departure-port locations.
coords <- data.frame(
  Country = c("United Arab Emirates", "Saudi Arabia", "United States", "Thailand",
              "Singapore", "Qatar", "Kuwait", "Oman", "United Kingdom",
              "Malaysia", "Canada", "Australia", "Bangladesh", "Sri Lanka", "Bahrain"),
  lat = c(24.45, 23.89, 37.09, 15.87, 1.35, 25.29, 29.31, 21.47, 55.38,
          4.21, 56.13, -25.27, 23.68, 7.87, 26.07),
  lon = c(54.38, 45.08, -95.71, 100.99, 103.82, 51.52, 47.48, 55.98, -3.44,
          101.98, -106.35, 133.78, 90.36, 80.77, 50.56),
  stringsAsFactors = FALSE
)

# ---------- UI ----------
ui <- page_fluid(
  theme = bs_theme(version = 5, bootswatch = "flatly", primary = "#1565C0"),

  tags$h2("Indian Outbound Travel Analytics"),

  layout_sidebar(
    sidebar = sidebar(
      selectInput("year", "Select year", choices = c(2019, 2023, 2024), selected = 2024),
      sliderInput("top_n", "Number of destinations", min = 5, max = 15, value = 10, step = 1),
      checkboxGroupInput("trend_years", "Trend years", choices = c(2019, 2023, 2024), selected = c(2019, 2023, 2024)),
      selectInput("trend_country", "Country", choices = c("All Top 15", sort(unique(top15$Country))), selected = "All Top 15"),
      selectInput("map_year", "Map year", choices = c(2019, 2023, 2024), selected = 2024),
      selectInput("table_year", "Table year", choices = c("All", 2019, 2023, 2024), selected = "All"),
      textInput("country_search", "Country contains", value = ""),
      helpText("The country-level dataset contains the Ministry of Tourism's Top 15 destinations for 2019, 2023 and 2024."),
      hr(),
      strong("Data source"),
      p("Ministry of Tourism, Government of India; source: Bureau of Immigration (BOI).")
    ),

    layout_columns(
      value_box(title = "Total Indian departures", value = textOutput("total_card"), showcase = bsicons::bs_icon("airplane")),
      value_box(title = "Top destination", value = textOutput("top_country_card"), showcase = bsicons::bs_icon("trophy")),
      value_box(title = "Top destination share", value = textOutput("top_share_card"), showcase = bsicons::bs_icon("pie-chart")),
      value_box(title = "Top 15 share", value = textOutput("top15_share_card"), showcase = bsicons::bs_icon("bar-chart"))
    ),

    layout_columns(
      card(card_header("Top destinations"), plotlyOutput("bar_plot", height = "430px")),
      card(card_header("Destination share"), plotlyOutput("donut_plot", height = "430px"))
    ),

    layout_columns(
      card(card_header("Year comparison"), plotlyOutput("year_compare_plot", height = "380px")),
      card(card_header("Monthly departures: 2023 vs 2024"), plotlyOutput("monthly_plot", height = "380px"))
    ),

    layout_columns(
      card(card_header("Outbound travel trend"), plotlyOutput("trend_plot", height = "500px")),
      card(card_header("Interpretation"), htmlOutput("trend_text"))
    ),

    layout_columns(
      card(card_header("Indian outbound destinations"), leafletOutput("travel_map", height = "650px")),
      card(card_header("Destination-level data"), DTOutput("data_table"))
    ),

    card(
      card_header("Project Information"),
      h3("Indian Outbound Travel Analytics"),
      p("This application analyzes Indian Nationals' Departures (INDs) from India to major destination countries."),
      h4("Dataset"),
      p("The country-level dataset is based on Table 3.1.3 of the India Tourism Data Compendium 2025 and covers the Top 15 destination countries for 2019, 2023 and 2024."),
      h4("Why these years?"),
      p("2019 provides a pre-pandemic reference point, while 2023 and 2024 show the post-pandemic recovery and recent outbound travel pattern."),
      h4("Visualizations used"),
      tags$ul(
        tags$li("Bar chart - destination ranking"),
        tags$li("Donut chart - destination share"),
        tags$li("Line chart - trend comparison"),
        tags$li("Monthly line chart - 2023 vs 2024"),
        tags$li("World map - geographic distribution"),
        tags$li("Interactive data table")
      ),
      h4("Official source"),
      p("Ministry of Tourism, Government of India. Underlying source: Bureau of Immigration (BOI).")
    )
  )
)

# ---------- Server ----------
server <- function(input, output, session) {

  selected_data <- reactive({
    top15 %>%
      filter(Year == as.integer(input$year)) %>%
      arrange(desc(Departures)) %>%
      slice_head(n = input$top_n)
  })

  selected_total <- reactive({
    totals %>% filter(Year == as.integer(input$year))
  })

  output$total_card <- renderText({
    comma(selected_total()$TotalDepartures, accuracy = 1)
  })

  output$top_country_card <- renderText({
    selected_data()$Country[1]
  })

  output$top_share_card <- renderText({
    paste0(number(selected_data()$Share[1], accuracy = 0.01), "%")
  })

  output$top15_share_card <- renderText({
    paste0(number(selected_total()$Top15Share, accuracy = 0.01), "%")
  })

  output$bar_plot <- renderPlotly({
    df <- selected_data()
    p <- ggplot(df, aes(x = reorder(Country, Departures), y = Departures,
                        text = paste0(Country, "<br>Departures: ", comma(Departures)))) +
      geom_col() +
      coord_flip() +
      scale_y_continuous(labels = label_number(scale = 1e-6, suffix = " M")) +
      labs(x = NULL, y = "Indian national departures", title = NULL) +
      theme_minimal(base_size = 13)
    ggplotly(p, tooltip = "text")
  })

  output$donut_plot <- renderPlotly({
    df <- selected_data()
    plot_ly(
      df,
      labels = ~Country,
      values = ~Share,
      type = "pie",
      hole = 0.5,
      textinfo = "label+percent",
      hovertemplate = "%{label}<br>Share: %{value}%<extra></extra>",
      marker = list(line = list(color = "white", width = 1))
    ) %>%
      layout(
        showlegend = TRUE,
        legend = list(orientation = "v"),
        margin = list(l = 10, r = 10, t = 10, b = 10)
      )
  })

  output$year_compare_plot <- renderPlotly({
    p <- ggplot(totals, aes(x = factor(Year), y = TotalDepartures,
                            group = 1, text = paste0(Year, ": ", comma(TotalDepartures)))) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 3) +
      scale_y_continuous(labels = label_number(scale = 1e-6, suffix = " M")) +
      labs(x = "Year", y = "Total departures", title = "Total Indian national departures") +
      theme_minimal(base_size = 13)
    ggplotly(p, tooltip = "text")
  })

  output$monthly_plot <- renderPlotly({
    p <- ggplot(monthly, aes(x = MonthNo, y = Departures, color = factor(Year), group = Year,
                             text = paste0(Month, " ", Year, "<br>Departures: ", comma(Departures)))) +
      geom_line(linewidth = 1.1) +
      geom_point() +
      scale_x_continuous(breaks = 1:12, labels = month.abb) +
      scale_y_continuous(labels = label_number(scale = 1e-6, suffix = " M")) +
      labs(x = "Month", y = "Departures", color = "Year") +
      theme_minimal(base_size = 13)
    ggplotly(p, tooltip = "text")
  })

  output$trend_plot <- renderPlotly({
    years <- as.integer(input$trend_years)
    validate(need(length(years) > 0, "Select at least one year."))

    if (input$trend_country == "All Top 15") {
      df <- top15 %>% filter(Year %in% years) %>%
        group_by(Year) %>% summarise(Departures = sum(Departures), .groups = "drop")
      p <- ggplot(df, aes(x = Year, y = Departures, group = 1,
                          text = paste0("Year: ", Year, "<br>Top 15 departures: ", comma(Departures)))) +
        geom_line(linewidth = 1.2) + geom_point(size = 3) +
        scale_y_continuous(labels = label_number(scale = 1e-6, suffix = " M")) +
        labs(x = "Year", y = "Departures", title = "Top 15 destinations combined") +
        theme_minimal(base_size = 14)
    } else {
      df <- top15 %>% filter(Country == input$trend_country, Year %in% years)
      p <- ggplot(df, aes(x = Year, y = Departures, group = 1,
                          text = paste0(Country, " - ", Year, "<br>Departures: ", comma(Departures)))) +
        geom_line(linewidth = 1.2) + geom_point(size = 3) +
        scale_y_continuous(labels = label_number(scale = 1e-6, suffix = " M")) +
        labs(x = "Year", y = "Departures", title = input$trend_country) +
        theme_minimal(base_size = 14)
    }
    ggplotly(p, tooltip = "text")
  })

  output$trend_text <- renderUI({
    y <- as.integer(input$year)
    d <- top15 %>% filter(Year == y) %>% arrange(desc(Departures))
    total <- totals %>% filter(Year == y) %>% pull(TotalDepartures)
    HTML(paste0(
      "<p>In <b>", y, "</b>, the total number of Indian national departures was approximately <b>", comma(total),
      "</b>. Among the Top 15 destination countries, <b>", d$Country[1], "</b> ranked first with <b>",
      comma(d$Departures[1]), "</b> departures.</p>"
    ))
  })

  output$travel_map <- renderLeaflet({
    y <- as.integer(input$map_year)
    df <- top15 %>% filter(Year == y) %>% left_join(coords, by = "Country")

    leaflet(df) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircleMarkers(
        lng = ~lon, lat = ~lat,
        radius = ~pmax(5, sqrt(Departures / 100000)),
        stroke = TRUE, weight = 1,
        fillOpacity = 0.75,
        popup = ~paste0(
          "<b>", Country, "</b><br>",
          "Year: ", Year, "<br>",
          "Indian departures: ", comma(Departures), "<br>",
          "Share: ", Share, "%"
        ),
        label = ~paste0(Country, ": ", comma(Departures))
      ) %>%
      addControl("Top 15 destination countries", position = "topright")
  })

  table_data <- reactive({
    df <- top15
    if (input$table_year != "All") df <- df %>% filter(Year == as.integer(input$table_year))
    if (nzchar(trimws(input$country_search))) {
      df <- df %>% filter(grepl(input$country_search, Country, ignore.case = TRUE))
    }
    df %>% arrange(Year, Rank)
  })

  output$data_table <- renderDT({
    datatable(
      table_data(),
      rownames = FALSE,
      filter = "top",
      options = list(pageLength = 15, scrollX = TRUE),
      colnames = c("Rank", "Country", "Year", "Departures", "Share (%)")
    ) %>%
      formatRound("Departures", 0) %>%
      formatRound("Share", 2)
  })
}

shinyApp(ui, server)
