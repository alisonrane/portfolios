library(shiny)
library(tidyverse)
library(bslib)
library(ggiraph)
library(tidytext)
library(gt)
library(stopwords)

here::i_am("app.R")

contacts <- readr::read_csv(here::here("contacts - contact.csv")) |> 
  mutate(Date = lubridate::as_date(mdy(Date)) + 2000)

prospects <- readr::read_csv(here::here("prospects - prospects.csv"))

progress <- tibble(`Staff Name` = unique(contacts$`Staff Name`),
                   Goal = c(2000000, 2000000, 3000000, 5000000,
                            2000000, 2000000, 3000000, 5000000),
                   Commitments = c(1500000, 1800000, 2500000, 3000000,
                                   1400000, 1900000, 1750000, 4800000)) 

alert_colors <- c(Low = "#533745", Med = "#33658a", High = "#86cb92")
time_colors <- c("Less than 1 month" = "#86cb92", 
                 "1-2 months" = "#33658a", 
                 "2-3 months" = "#533745", 
                 "3+ months" = "#d36135")

# Selectors --------------------------------------------------------------------
manager_selector <- selectInput("manager", "Select a Gift Officer",
                                choices = c("All MGOs", unique(prospects$`Staff Name`)))

# UI  --------------------------------------------------------------------------

ui <- page_navbar(
  title = "Major Gifts Overview: Sample reporting from fictional data",
  theme = bs_theme(version = 5),
  fluidRow(
    column(width = 3, manager_selector), 
    column(width = 3, value_box("Outstanding Proposals", 
                                textOutput("total_proposals"),
                                showcase = bsicons::bs_icon("currency-dollar"))),
    column(width = 3, value_box("Most Recent Visit", 
                                textOutput("most_recent_date"),
                                showcase = bsicons::bs_icon("calendar-check"))),
    column(width = 3, value_box("Total Visits", 
                                textOutput("total_visits"),
                                showcase = bsicons::bs_icon("person")))
    ),
  fluidRow(
    column(width = 6,
           card(
             card_header("Progress to Fundraising Goal"),
             plotOutput("progress_to_goal")
           )),
    column(width = 6,
           card(
             card_header("Recency of Prospect Contact (Hover for Tooltip)"),
             girafeOutput("timesincecontact", height = "400px")
           )
    )
  ),
  fluidRow(
    column(width = 6,
           card(
             card_header("Prospects, Capacity, and Proposals by Stage"),
             gt_output("capacity_by_stage")
           )),
    column(width = 6,
           card(
             card_header("Visit Count and Percent Positive Outcome"),
             gt_output("contact_success")
           )
           )
    ),
  fluidRow(
      column(width = 6, 
             card(
               card_header("Total Visits by Month"),
               plotOutput("visits_by_month", height = "300px")
             )),
    column(width = 6,
    card(
      card_header("Contact Timeline (Hover for Tooltip)"),
      girafeOutput("contact_frequency", height = "300px")
    ))),
  fluidRow(
    column(width = 12,
           card(
             card_header("Frequent Words in Contact Reports by Word Sentiment"),
             plotOutput("termfrequency"))
    )
  )
  )


# Server  ----------------------------------------------------------------------
server <- function(input, output){
  reactprospects <- reactive({
    if (input$manager == "All MGOs"){
      prospects
    } else {
      prospects |> filter(`Staff Name` == input$manager)
    }
  })
  
  reactcontacts <- reactive({
    if (input$manager == "All MGOs"){
      contacts
    } else {
      contacts |> filter(`Staff Name` == input$manager)
    }
  })
  
  reactprogress <- reactive({
    if (input$manager == "All MGOs"){
      progress
    } else {
      progress |> filter(`Staff Name` == input$manager)
    }
  })
  
# Value box values
  output$most_recent_date <- renderText({
    reactcontacts() |> summarize(Date = max(Date)) |> pull(Date) |> as.character()
  })
  
  output$total_visits <- renderText({
    reactcontacts() |> count() |> pull(n)
  })
  
  output$total_proposals <- renderText({
    reactprospects() |> summarize(total = sum(Proposal, na.rm = T)) |> pull(total) |> 
      scales::dollar()
  })
  
# Plots
  output$progress_to_goal <- renderPlot({
    progress <- reactprogress()
    
    progress |> 
      summarize(Goal = sum(Goal),
                Commitments = sum(Commitments)) |> 
      mutate(Pct = Commitments / Goal) |> 
      mutate(alert = case_when(
        Pct >= 0.90 ~ "High", 
        Pct >= 0.75 ~ "Med",
        Pct < 0.75 ~ "Low"
      )) |> 
      ggplot(aes(ymax = Pct, ymin = 0, xmax = 2, xmin = 1, fill = alert)) +
      geom_rect(aes(ymax = 1, ymin = 0, xmax = 2, xmin=1), fill ="lightgrey") +
      geom_rect() + 
      coord_polar(theta = "y",start = -pi/2) + xlim(c(0, 2)) + ylim(c(0,2)) +
      geom_text(aes(x = 0, y = 0, label = scales::percent_format()(Pct), 
                    color = alert), size = 10, vjust = -0.1) +
      geom_text(aes(x = 0, y = 0, label = input$manager, 
                    color = alert), size = 10, vjust = 2) +
      scale_fill_manual(values = alert_colors) +
      scale_color_manual(values = alert_colors) +
      theme_void() +
      theme(strip.background = element_blank(),
            strip.text.x = element_blank(),
            legend.position = "none") 
  })
  
  output$timesincecontact <- renderGirafe({
    contacts <- reactcontacts()
    
    gg <- contacts |> 
      mutate(days_since_contact = lubridate::time_length(today() - Date, "month")) |> 
      mutate(days_since_binned = case_when(
        days_since_contact < 1 ~ "Less than 1 month",
        days_since_contact < 2 ~ "1-2 months",
        days_since_contact < 3 ~ "2-3 months",
        days_since_contact >= 3 ~ "3+ months"
      )) |> 
      mutate(days_since_binned = fct_relevel(days_since_binned, "Less than 1 month")) |> 
      count(days_since_binned) |> 
      mutate(pct = prop.table(n)) |> 
      ggplot(aes(x = "", y = pct, fill = days_since_binned,
                 tooltip = paste(scales::percent_format(accuracy = 1)(pct), 
                                 days_since_binned))) +
      geom_bar_interactive(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      scale_fill_manual(values = time_colors) +
      theme_void(base_size = 10) +
      labs(fill = NULL) +
      theme(legend.position = "bottom")
    
    girafe(ggobj = gg,
           options = list(
             opts_tooltip(offx = 20, offy = 20),
             opts_selection(type = "none")))
  })
  
  output$contact_success <- render_gt({
    contacts <- reactcontacts()
    
    contacts |> 
      count(`Staff Name`, Outcome) |> 
      pivot_wider(names_from = Outcome, values_from = n) |> 
      mutate(`Total Visits` = Positive + Negative,
        `Success Rate` = (Positive / `Total Visits`)) |> 
      select(-Positive, -Negative) |> 
      gt() |> 
      fmt_percent(columns = "Success Rate", decimals = 0) |> 
      # gtExtras::gt_plt_bar(column = `Total Visits`, keep_column = TRUE, color = "darkblue") |> 
      opt_interactive(use_compact_mode = TRUE)
  })
  
  output$termfrequency <- renderPlot({
    contacts <- reactcontacts() 
    
    contacts |> 
      unnest_tokens(word, Summary) |> 
      anti_join(get_stopwords()) |> 
      anti_join(tibble(word = c("university", "meet", "meeting"))) |> 
      count(word, sort = TRUE) |> 
      inner_join(get_sentiments("bing"), by = "word") |> 
      mutate(angle = 90 * sample(c(0, 1), n(), replace = TRUE, prob = c(60, 40))) |> 
      ggplot(aes(label = word, size = n, color = sentiment, angle = angle)) +
      ggwordcloud::geom_text_wordcloud_area(area_corr_power = 1, rm_outside = TRUE) +
      scale_size_area(max_size = 40) +
      scale_color_manual(values = c("#533745", "#86cb92")) +
      theme_minimal()
  })
  
  output$visits_by_month <- renderPlot({
    contacts <- reactcontacts()
    
    contacts |> 
      mutate(month = lubridate::floor_date(Date, "month")) |> 
      count(month) |> 
      ggplot(aes(month, n)) +
      geom_col(fill = "#33658a") +
      scale_x_date(name = NULL) +
      scale_y_continuous(name = NULL, labels = NULL) +
      geom_text(aes(label = n), vjust = -0.1, size = 5) +
      theme_minimal(base_family = "Arial", base_size = 16) 
  })
  
  output$contact_frequency <- renderGirafe({
    contacts <- reactcontacts()
    
    gg <- contacts |> 
      left_join(prospects, by = "Donor") |> 
      mutate(Capacity_dollar = scales::dollar(Capacity, accuracy = 1)) |> 
      mutate(Capacity_dollar = fct_reorder(Capacity_dollar, Capacity)) |> 
      ggplot(aes(Date, runif(n=1, min=0, max=1), color = Outcome,
                 tooltip = paste(Donor, "with", `Staff Name.x`, "\nCapacity:", 
                                 Capacity_dollar))) +
      geom_jitter_interactive(alpha = 0.7, size = 2) +
      scale_x_date(name = NULL) +
      scale_y_continuous(name = NULL, labels = NULL) +
      scale_color_manual(values = c("#533745", "#86cb92")) +
      theme_minimal(base_family = "Arial") +
      theme(legend.position = "top")
    
    girafe(ggobj = gg,
           pointsize = 20,
           width_svg = 8,
           height_svg = 4,
           options = list(
             opts_tooltip(offx = 20, offy = 20),
           opts_selection(type = "none"),    
           opts_hover_inv(css = "opacity:0.1;"),
           opts_hover(css = "stroke-width:2;")
           )
    )
  })

  output$capacity_by_stage <- render_gt({
    prospects <- reactprospects()
    
    prospects |> 
      mutate(Stage = fct_relevel(Stage, "Qualification", "Cultivation",
                                 "Solicitation", "Stewardship")) |> 
      group_by(Stage) |> 
      summarize(Prospects = n(), 
                `Total Capacity` = sum(Capacity),
                `Total Proposals` = sum(Proposal, na.rm = T)) |> 
      ungroup() |> 
      mutate(pct_captured = `Total Proposals` / `Total Capacity`) |> 
      mutate(`Capacity Captured` = ifelse(is.na(pct_captured) | pct_captured == 0, 
                                   "No Proposals", scales::percent(pct_captured,
                                                                   accuracy = 1))) |> 
      select(-pct_captured) |> 
      gt() |> 
      fmt_currency(columns = c("Total Capacity", "Total Proposals"),
                   suffixing = TRUE, decimals = 1) |> 
      opt_interactive(use_compact_mode = TRUE)

  })
  
}

shinyApp(ui, server)