# -------------------------------------------------------------------
# Customer Review Dashboard
# Uses Claude Haiku 4.5 for all LLM calls and local retrieval for review evidence
# -------------------------------------------------------------------

library(shiny)
library(plotly)
library(bslib)
library(querychat)
library(ellmer)
library(shinychat)
library(dplyr)
library(ggplot2)
library(DT)
library(tidytext)
library(tidyr)
library(lubridate)
library(scales)
library(stringr)
library(textstem)
library(topicmodels)
library(tm)
# Just so I can try to publish..
library(broom)
library(magick)
library(RSQLite)
# To fix LDA error when published online
library(reshape2)
#For PDF Creation
library(rmarkdown)
library(knitr)
library(glue)

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
}

read_md_or_default <- function(file, default_text) {
  if (file.exists(file)) paste(readLines(file, warn = FALSE), collapse = "\n") else default_text
}

# Citation: Salminen, J., Kandpal, C., Kamel, A. M., Jung, S., & Jansen, B. J. (2022). 
# Creating and detecting fake reviews of online products. Journal of Retailing and Consumer Services, 64, 102771.
# https://doi.org/10.1016/j.jretconser.2021.102771

# https://www.kaggle.com/datasets/mexwell/fake-reviews-dataset

# -------------------------------------------------------------------
#### - Data Import and Setup ####
# -------------------------------------------------------------------

#reviews <- read.csv("fakereviewsdataset_sample_with_dates.csv")

reviews <- read.csv("fakereviewsdataset.csv")

reviews <- reviews[reviews$label == "OR", ]

reviews$category <- gsub("_5$", "", reviews$category)
reviews$category <- gsub("_", " ", reviews$category)

reviews <- reviews |>
  rename(text = text_)
reviews <- reviews |>
  mutate(primary_id = paste0("A_", row_number()))
reviews <- reviews |>
  mutate(
    primary_id = paste0(
      "A_",
      stringr::str_pad(row_number(),
                       width = nchar(n()),
                       pad = "0")
    )
  )

reviews <- reviews |>
  relocate(primary_id, .before = 1)


start_date <- as.Date("2026-01-01")
end_date   <- as.Date("2026-05-12")

set.seed(6395)

make_uneven_dates <- function(n) {
  all_dates <- seq.Date(start_date, end_date, by = "day")
  
  #   Uneven probability by month
  date_weights <- case_when(
    month(all_dates) == 1 ~ 0.90,
    month(all_dates) == 2 ~ 1.15,
    month(all_dates) == 3 ~ 0.95,
    month(all_dates) == 4 ~ 1.10,
    month(all_dates) == 5 ~ 0.85,
    TRUE ~ 0.01
  )
  
  #   Add day-to-day randomness so it does not look smooth
  date_weights <- date_weights * runif(length(all_dates), 0.2, 2.5)
  
  sample(all_dates, size = n, replace = TRUE, prob = date_weights)
}

reviews <- reviews |>
  group_by(category) |>
  mutate(
    review_date = make_uneven_dates(n())
  ) |>
  ungroup()


data_for_app <- reviews |>
  mutate(
    primary_id = as.character(primary_id),
    review_date = format(as.Date(review_date), "%Y-%m-%d"),
    category = as.factor(category),
    rating = as.numeric(rating),
    text = as.character(text)
  ) |>
  select(primary_id, review_date, category, rating, text)

# ---- Dynamic date context for QueryChat, -----
# -----so that QueryChat knows what today's date is ----
dataset_today <- max(as.Date(data_for_app$review_date), na.rm = TRUE)

date_filter_instructions <- paste0(
  "Important date-filtering instructions:\n",
  "- The review_date column is stored as character text in YYYY-MM-DD format.\n",
  "- When the user says 'today', automatically interpret today as ",
  format(dataset_today, "%Y-%m-%d"), ".\n",
  "- Do not ask the user to clarify today's date.\n",
  "- For requests like 'between April 15th and today', filter review_date from ",
  "2026-04-15 through ", format(dataset_today, "%Y-%m-%d"), ", inclusive.\n",
  "- For date ranges, use review_date >= start date and review_date <= end date.",
  "- For a full year like 2026, use: review_date >= '2026-01-01' AND review_date <= '2026-12-31'."
)

qc <- QueryChat$new(
  data_for_app,
  client = chat_anthropic(model = "claude-haiku-4-5"),
  greeting = read_md_or_default(
    "reviews_greeting.md",
    paste(
      "I can help you filter the customer review data.",
      "Try asking: Show reviews with rating below 3, show reviews about packaging, or show reviews in a specific category.",
      sep = "\n"
    )
  ),
  data_description = read_md_or_default(
    "reviews_description.md",
    "This dataset contains customer reviews with primary_id, review_date, category, rating, and text."
  ),
  extra_instructions = paste(
    read_md_or_default(
      "reviews_instructions.md",
      "Use only the available columns to filter the review dataset."
    ),
    date_filter_instructions,
    sep = "\n\n"
  )
)

# -------------------------------------------------------------------
#### ISSUE DICTIONARY
# -------------------------------------------------------------------

issue_dictionary <- tibble::tribble(
  ~issue, ~word,
  "Price / Value", "price",
  "Price / Value", "cost",
  "Price / Value", "expensive",
  "Price / Value", "cheap",
  "Price / Value", "affordable",
  "Price / Value", "value",
  "Price / Value", "worth",
  "Price / Value", "overpriced",
  "Product Quality", "quality",
  "Product Quality", "material",
  "Product Quality", "premium",
  "Product Quality", "solid",
  "Product Quality", "poor",
  "Product Quality", "defective",
  "Product Quality", "flimsy",
  "Functionality / Performance", "function",
  "Functionality / Performance", "functional",
  "Functionality / Performance", "feature",
  "Functionality / Performance", "work",
  "Functionality / Performance", "performance",
  "Functionality / Performance", "slow",
  "Functionality / Performance", "fast",
  "Usability / Setup", "easy",
  "Usability / Setup", "difficult",
  "Usability / Setup", "simple",
  "Usability / Setup", "use",
  "Usability / Setup", "setup",
  "Usability / Setup", "install",
  "Usability / Setup", "confusing",
  "Fit / Sizing", "fit",
  "Fit / Sizing", "size",
  "Fit / Sizing", "sizing",
  "Fit / Sizing", "small",
  "Fit / Sizing", "large",
  "Fit / Sizing", "tight",
  "Fit / Sizing", "loose",
  "Packaging / Delivery", "packaging",
  "Packaging / Delivery", "package",
  "Packaging / Delivery", "box",
  "Packaging / Delivery", "shipping",
  "Packaging / Delivery", "delivery",
  "Packaging / Delivery", "damage",
  "Packaging / Delivery", "arrive",
  "Packaging / Delivery", "late",
  "Packaging / Delivery", "delayed",
  "Customer Support", "support",
  "Customer Support", "service",
  "Customer Support", "customer",
  "Customer Support", "help",
  "Customer Support", "helpful",
  "Customer Support", "response",
  "Customer Support", "refund",
  "Customer Support", "return",
  "Durability / Broken", "durable",
  "Durability / Broken", "durability",
  "Durability / Broken", "break",
  "Durability / Broken", "broken",
  "Durability / Broken", "last",
  "Durability / Broken", "sturdy",
  "Durability / Broken", "wear",
  "Durability / Broken", "crack",
  "Accuracy / Description", "accurate",
  "Accuracy / Description", "accuracy",
  "Accuracy / Description", "exact",
  "Accuracy / Description", "correct",
  "Accuracy / Description", "wrong",
  "Accuracy / Description", "misleading",
  "Accuracy / Description", "description"
)

# -------------------------------------------------------------------
# CLAUDE | LOCAL RETRIEVAL SETUP
# -------------------------------------------------------------------

tokenize_for_retrieval <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- stringr::str_to_lower(x)
  
  tibble(text = x) |>
    mutate(row_id = row_number()) |>
    tidytext::unnest_tokens(word, text) |>
    mutate(word = textstem::lemmatize_words(word)) |>
    anti_join(tidytext::stop_words, by = "word") |>
    filter(stringr::str_detect(word, "^[a-z]+$"))
}

make_query_terms <- function(user_question) {
  terms <- tokenize_for_retrieval(user_question) |>
    count(word, sort = TRUE) |>
    pull(word)
  
  unique(terms)
}



# -------------------------------------------------------------------
#### UI
# -------------------------------------------------------------------


ui <- page_navbar(
  title = "Customer Review Dashboard",
  theme = bs_theme(version = 5, bootswatch = "superhero"),
  
  tags$style(HTML("
    .card-header { border-bottom: 1px solid #444 !important; }
    .value-box { background-color: #1b1f22 !important; border: 1px solid #444 !important; }
    .med-value-box .value-box-title { font-size: 0.75rem !important; line-height: 1.05 !important; }
    .med-value-box .value-box-value { font-size: 0.95rem !important; line-height: 1.05 !important; word-break: break-word; white-space: normal; }
    .med-value-box .value-box-area { padding: 0.45rem !important; }
    .med-value-box { min-height: 90px !important; }

    #advanced_chat h1,
    #advanced_chat h2,
    #advanced_chat h3 {
      font-size: 1rem !important;
      line-height: 1.2 !important;
      margin-top: 0.4rem !important;
      margin-bottom: 0.3rem !important;
      font-weight: 700 !important;
    }

    #advanced_chat p {
      font-size: 0.90rem !important;
      line-height: 1.25 !important;
    }

    #advanced_chat li {
      font-size: 0.90rem !important;
      line-height: 1.25 !important;
    }

    .table-fill-card { flex: 1 1 45%; min-height: 45%; overflow: visible !important; }
    .table-fill-card .card-body { overflow: visible !important; padding-bottom: 1rem; }
    .table-fill-card .dataTables_wrapper, .table-fill-card table.dataTable { width: 100% !important; }
    .dataTables_wrapper .dataTables_length { float: left !important; margin-bottom: 0.6rem; }
    .dataTables_wrapper .dataTables_filter { float: right !important; margin-bottom: 0.6rem; }
    .dataTables_scroll { clear: both; margin-bottom: 0.6rem; }
    .dataTables_scrollBody { overflow: auto !important; }
    .dataTables_wrapper .dataTables_info { float: left !important; clear: left !important; padding-top: 0.6rem !important; }
    .dataTables_wrapper .dataTables_paginate { float: right !important; padding-top: 0.3rem !important; }
  ")),
  
  nav_panel(
    "1. Data filter and basic summary",
    layout_columns(
      col_widths = c(3, 9),
      card(
        full_screen = TRUE,
        card_header("Filter Data"),
        div(style = "height: calc(100vh - 150px); overflow: hidden;", qc$ui())
      ),
      div(
        style = "height: calc(100vh - 130px); display: flex; flex-direction: column; gap: 0.75rem;",
        div(
          style = "flex: 0 0 50%; min-height: 0; display: grid; grid-template-columns: 1fr 1fr; gap: 0.75rem;",
          card(
            card_header("Summary Charts (for filtered data if applicable)"),
            selectInput(
              "chart_type",
              "Select chart:",
              choices = c(
                "1. Count of reviews by rating" = "rating_bar",
                "2. Count of reviews by review date" = "reviews_by_date",
                "3. Average rating by week" = "avg_rating_week",
                "4. Count by category + rating" = "reviews_by_category_rating",
                "5. Count by category + rating group" = "rating_group_category"
              ),
              selected = "rating_bar"
            ),
            plotly::plotlyOutput("main_chart", height = "230px")
          ),
          div(
            style = "display: grid; grid-template-columns: 1fr 1fr 1fr; grid-template-rows: 1fr 1fr; gap: 0.75rem; min-height: 0;",
            value_box("Average Rating of Filtered Reviews", textOutput("avg_rating"), class = "med-value-box"),
            value_box("Filtered Count of Reviews", textOutput("num_responses"), class = "med-value-box"),
            value_box("Current Month Review Count (from filtered criteria)", textOutput("past_month_reviews"), class = "med-value-box"),
            value_box("Current Filters Applied", uiOutput("filters_applied"), class = "med-value-box"),
            value_box("Top Category (by Review count)", uiOutput("top_category"), class = "med-value-box"),
            value_box("All Available Unfiltered Categories", uiOutput("available_categories"), class = "med-value-box")
          )
        ),
        card(
          full_screen = TRUE,
          class = "table-fill-card",
          card_header("Filtered Reviews"),
          DTOutput("review_table", width = "100%")
        )
      )
    )
  ),
  
  nav_panel(
    "2. Advanced Insights",
    layout_columns(
      col_widths = c(8, 4),
      card(
        card_header("Advanced Insights"),
        downloadButton(
          "download_advanced_pdf",
          "Download Advanced Insights PDF",
          class = "btn-primary"
        ),
        br(),
        br(),
        div(
          style = "display: grid; grid-template-columns: 1fr 1fr; grid-auto-rows: auto; gap: 0.75rem;",
          card(card_header("Positive Sentiment Over Time"), plotly::plotlyOutput("positive_sentiment_time", height = "270px")),
          card(card_header("Negative Sentiment Over Time"), plotly::plotlyOutput("negative_sentiment_time", height = "270px")),
          card(card_header("Most Common Issue by Category"), plotly::plotlyOutput("common_issue_by_category", height = "270px")),
          card(card_header("Sentiment Split"), plotly::plotlyOutput("sentiment_pie", height = "270px")),
          card(
            style = "grid-column: 1 / -1;",
            card_header("LDA Topic Modeling"),
            div(style = "margin-bottom: 0.75rem;", actionButton("run_lda", "Run LDA", class = "btn-primary")),
            plotOutput("lda_topics_plot", height = "420px")
          )
        )
      ),
      card(card_header("Ask About Advanced Insights"), chat_ui("advanced_chat", height = "800px"))
    )
  ),
  
  nav_panel(
    "3. More Information",
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Project Notes"),
        div(
          style = "min-height: 300px; padding: 1rem;",
          "This dashboard analyzes customer review data using tidy text preprocessing, lemmatization, sentiment analysis, issue classification, LDA topic modeling, and retrieval-augmented LLM interpretation."
        )
      ),
      card(
        card_header("Data / Method Details"),
        div(
          style = "min-height: 300px; padding: 1rem;",
          "Methods: Bing lexicon sentiment analysis calculates positive and negative word counts. Common issue classification uses a manually defined issue dictionary and reports the most common issue for each category in the filtered data. LDA topic modeling estimates exploratory topics from filtered review text. The chatbot receives chart context, filtered summaries, spike-date evidence, issue counts, LDA terms, and locally retrieved review evidence from the user's question."
        )
      ),
      card(
        card_header("Limitations"),
        div(
          style = "min-height: 300px; padding: 1rem;",
          "Limitations: Lexicon sentiment may miss negation, sarcasm, and context. Common issue classification depends on the manually defined dictionary. LDA topics are exploratory and may be unstable for small filtered datasets. The retrieval function uses local lexical matching with tf-idf-style scoring. This keeps the app Claude-only, but it may miss semantically similar reviews that do not share words with the user question."
        )
      ),
      card(
        card_header("Expected Dataset Columns Used"),
        div(style = "min-height: 300px; padding: 1rem;", "primary_id, review_date, category, rating, text")
      )
    )
  )
)

# -------------------------------------------------------------------
#### SERVER
# -------------------------------------------------------------------

server <- function(input, output, session) {
  
  # Activate querychat's reactive outputs
  qc_vals <- qc$server()
  
  filtered_reviews <- reactive({
    qc_vals$df()
  })
  
  output$available_categories <- renderUI({
    cats <- data_for_app |>
      distinct(category) |>
      arrange(category) |>
      pull(category)
    
    tags$div(
      style = "font-size: 0.70rem; line-height: 1.1; max-height: 75px; overflow-y: auto;",
      lapply(cats, function(cat) {
        tags$div(as.character(cat))
      })
    )
  })
  
  # ---- Helper: save ggplot as PNG for chatbot vision ----
  save_plot_png <- function(plot_obj) {
    file <- tempfile(fileext = ".png")
    png(filename = file, width = 900, height = 650, res = 120)
    print(plot_obj)
    dev.off()
    file
  }
  
  
  advanced_data <- reactive({
    df <- qc_vals$df()
    req(nrow(df) > 0)
    
    df <- df |>
      mutate(review_date = as.Date(review_date), text = as.character(text)) |>
      filter(!is.na(text), text != "")
    
    req(nrow(df) > 0)
    
    tidy_reviews <- df |>
      select(primary_id, review_date, category, rating, text) |>
      unnest_tokens(word, text) |>
      mutate(word = textstem::lemmatize_words(word))
    
    all_reviews <- df |> distinct(primary_id, review_date)
    
    bing_matches <- tidy_reviews |> inner_join(get_sentiments("bing"), by = "word")
    
    if (nrow(bing_matches) == 0) {
      sentiment_scores <- all_reviews |>
        mutate(positive = 0L, negative = 0L, net_sentiment = 0L, sentiment_group = "Neutral")
    } else {
      sentiment_scores <- bing_matches |>
        count(primary_id, review_date, sentiment) |>
        pivot_wider(names_from = sentiment, values_from = n, values_fill = 0)
      
      if (!"positive" %in% names(sentiment_scores)) sentiment_scores$positive <- 0L
      if (!"negative" %in% names(sentiment_scores)) sentiment_scores$negative <- 0L
      
      sentiment_scores <- sentiment_scores |>
        mutate(
          positive = coalesce(positive, 0L),
          negative = coalesce(negative, 0L),
          net_sentiment = positive - negative,
          sentiment_group = case_when(
            net_sentiment > 0 ~ "Positive",
            net_sentiment < 0 ~ "Negative",
            TRUE ~ "Neutral"
          )
        )
      
      sentiment_scores <- all_reviews |>
        left_join(sentiment_scores, by = c("primary_id", "review_date")) |>
        mutate(
          positive = coalesce(positive, 0L),
          negative = coalesce(negative, 0L),
          net_sentiment = coalesce(net_sentiment, 0L),
          sentiment_group = coalesce(sentiment_group, "Neutral")
        )
    }
    
    issue_hits <- tidy_reviews |> inner_join(issue_dictionary, by = "word")
    
    if (nrow(issue_hits) == 0) {
      issue_counts <- tibble(category = factor("No category"), issue = "No issue matches", word = "none", n = 0L)
      biggest_issue <- tibble(category = factor("No category"), issue = "No issue matches", issue_count = 0L)
    } else {
      issue_counts <- issue_hits |> count(category, issue, word, sort = TRUE)
      biggest_issue <- issue_hits |>
        distinct(primary_id, category, issue) |>
        count(category, issue, name = "issue_count") |>
        group_by(category) |>
        slice_max(issue_count, n = 1, with_ties = FALSE) |>
        ungroup() |>
        arrange(issue_count)
    }
    
    list(
      filtered_reviews = df,
      tidy_reviews = tidy_reviews,
      sentiment_scores = sentiment_scores,
      issue_hits = issue_hits,
      issue_counts = issue_counts,
      biggest_issue = biggest_issue
    )
  })

  ## This function helps with copying the current plots to the PDF report
  save_report_plot <- function(plot_obj, filename, width = 8, height = 5) {
    ggplot2::ggsave(
      filename = filename,
      plot = plot_obj,
      width = width,
      height = height,
      units = "in",
      dpi = 150
    )
  }

  ## This function is designed to help write the PDF report
  make_advanced_report_summary <- function() {
    adv <- advanced_data()
    df <- adv$filtered_reviews
    sentiment_df <- adv$sentiment_scores
    issue_counts <- adv$issue_counts
    biggest_issues <- adv$biggest_issue
    
    daily_sentiment <- sentiment_df |>
      group_by(review_date) |>
      summarise(
        positive_words = sum(positive, na.rm = TRUE),
        negative_words = sum(negative, na.rm = TRUE),
        total_sentiment_words = positive_words + negative_words,
        reviews = n(),
        .groups = "drop"
      )
    
    top_positive_day <- daily_sentiment |>
      arrange(desc(positive_words)) |>
      slice_head(n = 1)
    
    top_negative_day <- daily_sentiment |>
      arrange(desc(negative_words)) |>
      slice_head(n = 1)
    
    sentiment_split <- sentiment_df |>
      count(sentiment_group) |>
      mutate(
        percent = n / sum(n),
        label = paste0(sentiment_group, ": ", n, " reviews (", scales::percent(percent, accuracy = 0.1), ")")
      ) |>
      pull(label)
    
    top_issues <- issue_counts |>
      group_by(issue) |>
      summarise(total = sum(n), .groups = "drop") |>
      arrange(desc(total)) |>
      slice_head(n = 5) |>
      mutate(label = paste0(issue, ": ", total)) |>
      pull(label)
    
    biggest_issue_context <- biggest_issues |>
      mutate(label = paste0(category, ": ", issue, " (", issue_count, " reviews)")) |>
      pull(label)
    
    lda_summary <- if (lda_is_available()) {
      tryCatch(
        {
          lda_topic_data()$topic_terms |>
            group_by(topic) |>
            summarise(terms = paste(term, collapse = ", "), .groups = "drop") |>
            mutate(label = paste0("Topic ", topic, ": ", terms)) |>
            pull(label) |>
            paste(collapse = "\n")
        },
        error = function(e) {
          "LDA was requested, but no valid LDA topic terms were available for the current filter."
        }
      )
    } else {
      "LDA Topic Modeling has not been run for the current filtered data. Click Run LDA to generate topics."
    }
    
    paste(
      "# Advanced Insights Summary",
      "",
      paste0("**Current filter:** ", qc_vals$title() %||% "No filters applied"),
      paste0("**Filtered reviews:** ", scales::comma(nrow(df)), " of ", scales::comma(nrow(data_for_app))),
      paste0("**Date range:** ", min(df$review_date, na.rm = TRUE), " to ", max(df$review_date, na.rm = TRUE)),
      "",
      "## Sentiment Summary",
      paste0("- Highest positive-word day: ", top_positive_day$review_date, " with ", top_positive_day$positive_words, " positive words."),
      paste0("- Highest negative-word day: ", top_negative_day$review_date, " with ", top_negative_day$negative_words, " negative words."),
      paste0("- Sentiment split: ", paste(sentiment_split, collapse = "; ")),
      "",
      "## Most Common Issues",
      paste0("- Top issues overall: ", paste(top_issues, collapse = "; ")),
      paste0("- Most common issue by category: ", paste(biggest_issue_context, collapse = "; ")),
      "",
      "## LDA Topic Modeling",
      lda_summary,
      "",
      "## Method Notes",
      "- Sentiment is calculated using a dictionary-based Bing lexicon approach.",
      "- Issue categories are based on the manually defined issue dictionary.",
      "- LDA topics are exploratory and depend on the current filtered review text.",
      sep = "\n"
    )
  }
  
  lda_topic_data <- eventReactive(input$run_lda, {
    df <- qc_vals$df()
    req(nrow(df) >= 5)
    
    tidy_for_lda <- df |>
      select(primary_id, text) |>
      filter(!is.na(text), text != "") |>
      unnest_tokens(word, text) |>
      mutate(word = textstem::lemmatize_words(word)) |>
      anti_join(stop_words, by = "word") |>
      filter(str_detect(word, "^[a-z]+$")) |>
      count(primary_id, word, sort = TRUE)
    
    shiny::validate(shiny::need(nrow(tidy_for_lda) > 20, "Not enough text after cleaning for LDA topics."))
    
    review_dtm <- tidy_for_lda |> cast_dtm(document = primary_id, term = word, value = n)
    
    shiny::validate(
      shiny::need(nrow(review_dtm) >= 5, "Need at least 5 reviews for LDA topics."),
      shiny::need(ncol(review_dtm) >= 5, "Need at least 5 unique terms for LDA topics.")
    )
    
    k_topics <- min(5, nrow(review_dtm) - 1, ncol(review_dtm) - 1)
    shiny::validate(shiny::need(k_topics >= 2, "Not enough data for topic modeling."))
    
    lda_model <- topicmodels::LDA(review_dtm, k = k_topics, control = list(seed = 1234))
    
    list(
      model = lda_model,
      topic_terms = broom::tidy(lda_model, matrix = "beta") |>
        group_by(topic) |>
        slice_max(beta, n = 8) |>
        ungroup(),
      document_topics = broom::tidy(lda_model, matrix = "gamma")
    )
  })
  
  # -------------------------------------------------------------------
  # Advanced insights plots
  # -------------------------------------------------------------------
  
  advanced_chart_theme <- theme_minimal(base_size = 15) +
    theme(
      plot.title = element_text(size = 17, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 13),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12),
      legend.title = element_text(size = 13),
      legend.text = element_text(size = 12),
      strip.text = element_text(size = 13, face = "bold")
    )
  
  positive_plot <- reactive({
    plot_df <- advanced_data()$sentiment_scores |>
      group_by(review_date) |>
      summarise(
        positive_words = sum(positive, na.rm = TRUE),
        .groups = "drop"
      )
    
    ggplot(plot_df, aes(
      x = review_date,
      y = positive_words,
      group = 1,
      text = paste0(
        "Date: ", review_date,
        "<br>Positive words: ", scales::comma(positive_words)
      )
    )) +
      geom_area(fill = "#2ecc71", alpha = 0.25) +
      geom_line(color = "#27ae60", linewidth = 0.5) +
      geom_point(color = "#27ae60", size = 1.1) +
      advanced_chart_theme +
      labs(
        title = "Positive Sentiment Over Time",
        subtitle = "Based on filtered review data",
        x = "Review Date",
        y = "Positive Word Count"
      )
  })
  
  output$positive_sentiment_time <- plotly::renderPlotly({
    plotly::ggplotly(positive_plot(), tooltip = "text") |>
      plotly::layout(
        font = list(size = 13),
        title = list(font = list(size = 17), x = 0.5),
        margin = list(l = 65, r = 20, t = 55, b = 50)
      ) |>
      plotly::config(displayModeBar = FALSE)
  })
  
  negative_plot <- reactive({
    plot_df <- advanced_data()$sentiment_scores |>
      group_by(review_date) |>
      summarise(
        negative_words = sum(negative, na.rm = TRUE),
        .groups = "drop"
      )
    
    ggplot(plot_df, aes(
      x = review_date,
      y = negative_words,
      group = 1,
      text = paste0(
        "Date: ", review_date,
        "<br>Negative words: ", scales::comma(negative_words)
      )
    )) +
      geom_area(fill = "#e74c3c", alpha = 0.25) +
      geom_line(color = "#c0392b", linewidth = 0.5) +
      geom_point(color = "#c0392b", size = 1.1) +
      advanced_chart_theme +
      labs(
        title = "Negative Sentiment Over Time",
        subtitle = "Based on filtered review data",
        x = "Review Date",
        y = "Negative Word Count"
      )
  })
  
  output$negative_sentiment_time <- plotly::renderPlotly({
    plotly::ggplotly(negative_plot(), tooltip = "text") |>
      plotly::layout(
        font = list(size = 13),
        title = list(font = list(size = 17), x = 0.5),
        margin = list(l = 65, r = 20, t = 55, b = 50)
      ) |>
      plotly::config(displayModeBar = FALSE)
  })
  
  output$download_advanced_pdf <- downloadHandler(
    filename = function() {
      paste0("advanced_insights_report_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      req(nrow(filtered_reviews()) > 0)
      
      temp_dir <- tempfile("advanced_report_")
      dir.create(temp_dir)
      
      positive_png <- file.path(temp_dir, "positive_sentiment.png")
      negative_png <- file.path(temp_dir, "negative_sentiment.png")
      issues_png <- file.path(temp_dir, "common_issues.png")
      pie_png <- file.path(temp_dir, "sentiment_split.png")
      lda_png <- file.path(temp_dir, "lda_topics.png")
      
      save_report_plot(positive_plot(), positive_png)
      save_report_plot(negative_plot(), negative_png)
      save_report_plot(common_issue_plot(), issues_png)
      save_report_plot(sentiment_pie_plot(), pie_png)

      ## A button is built, so it doesnt hog all the resources the moment the tab is visible
      ## Designed to process/run on demand only
      lda_plot_for_report <- if (lda_is_available()) {
        tryCatch(
          current_advanced_plot("lda"),
          error = function(e) {
            unavailable_plot(
              "LDA Topic Modeling",
              "LDA topic chart is not available for the current filtered data."
            )
          }
        )
      } else {
        unavailable_plot(
          "LDA Topic Modeling",
          "LDA topic chart is not available yet. Click Run LDA to generate topics."
        )
      }
      
      save_report_plot(lda_plot_for_report, lda_png, width = 8, height = 5.5)
      
      summary_text <- make_advanced_report_summary()
      
      report_rmd <- file.path(temp_dir, "advanced_report.Rmd")
      
      writeLines(
        c(
          "---",
          "title: 'Customer Review Dashboard - Advanced Insights Report'",
          "output:",
          "  pdf_document:",
          "    toc: false",
          "    number_sections: false",
          "geometry: margin=0.75in",
          "---",
          "",
          "```{r setup, include=FALSE}",
          "knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)",
          "```",
          "",
          summary_text,
          "",
          "\\newpage",
          "",
          "## Positive Sentiment Over Time",
          "",
          paste0("![](", positive_png, "){width=100%}"),
          "",
          "## Negative Sentiment Over Time",
          "",
          paste0("![](", negative_png, "){width=100%}"),
          "",
          "## Most Common Issue by Category",
          "",
          paste0("![](", issues_png, "){width=100%}"),
          "",
          "## Sentiment Split",
          "",
          paste0("![](", pie_png, "){width=85%}"),
          "",
          "## LDA Topic Modeling",
          "",
          paste0("![](", lda_png, "){width=100%}")
        ),
        report_rmd
      )
      
      rendered_pdf <- rmarkdown::render(
        input = report_rmd,
        output_format = "pdf_document",
        output_file = "advanced_report.pdf",
        output_dir = temp_dir,
        quiet = TRUE,
        envir = new.env(parent = globalenv())
      )
      
      file.copy(rendered_pdf, file, overwrite = TRUE)
    }
  )
  
  common_issue_plot <- reactive({
    plot_df <- advanced_data()$biggest_issue
    
    shiny::validate(
      shiny::need(nrow(plot_df) > 0, "No issue data available."),
      shiny::need(sum(plot_df$issue_count) > 0, "No issue keyword matches found for the current filter.")
    )
    
    ggplot(plot_df, aes(
      x = issue_count,
      y = reorder(category, issue_count),
      fill = issue,
      text = paste0(
        "Category: ", category,
        "<br>Issue: ", issue,
        "<br>Matching reviews: ", scales::comma(issue_count)
      )
    )) +
      geom_col() +
      advanced_chart_theme +
      labs(
        title = "Most Common Issue by Category",
        subtitle = "Based on filtered review data",
        x = "Number of matching reviews",
        y = "Category",
        fill = "Most Common Issue"
      )
  })
  
  output$common_issue_by_category <- plotly::renderPlotly({
    plotly::ggplotly(common_issue_plot(), tooltip = "text") |>
      plotly::layout(
        font = list(size = 10),
        title = list(font = list(size = 17), x = 0.5),
        xaxis = list(
          title = list(font = list(size = 10)),
          tickfont = list(size = 9)
        ),
        yaxis = list(
          title = list(font = list(size = 9)),
          tickfont = list(size = 8)
        ),
        legend = list(
          font = list(size = 8),
          title = list(font = list(size = 9))
        ),
        margin = list(l = 115, r = 15, t = 45, b = 40)
      ) |>
      plotly::config(displayModeBar = FALSE)
  })
  
  sentiment_pie_plot <- reactive({
    plot_df <- advanced_data()$sentiment_scores |>
      count(sentiment_group) |>
      mutate(
        percent = n / sum(n),
        label = paste0(sentiment_group, "\n", scales::percent(percent, accuracy = 0.1))
      )
    
    ggplot(plot_df, aes(x = "", y = n, fill = sentiment_group)) +
      geom_col(width = 1, color = "white") +
      geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 4) +
      coord_polar(theta = "y") +
      theme_void(base_size = 15) +
      labs(title = "Sentiment Split of Filtered Reviews", fill = "Sentiment")
  })
  
  output$sentiment_pie <- plotly::renderPlotly({
    plot_df <- advanced_data()$sentiment_scores |>
      count(sentiment_group) |>
      mutate(
        percent = n / sum(n),
        hover_text = paste0(
          "Sentiment: ", sentiment_group,
          "<br>Reviews: ", scales::comma(n),
          "<br>Percent: ", scales::percent(percent, accuracy = 0.1)
        )
      )
    
    plotly::plot_ly(
      data = plot_df,
      labels = ~sentiment_group,
      values = ~n,
      type = "pie",
      textinfo = "label+percent",
      hoverinfo = "text",
      text = ~hover_text,
      textfont = list(size = 11),marker = list(
        colors = c(
          "Positive" = "#2ecc71",
          "Negative" = "#e74c3c",
          "Neutral"  = "#0072B2"
        )[plot_df$sentiment_group]
      ),
      domain = list(
        x = c(0.18, 0.88),
        y = c(0.00, 0.92)
      )
    ) |>
      plotly::layout(
        title = list(
          text = "<b>Sentiment Split of Filtered Reviews</b>",
          font = list(size = 17, color = "black"),
          x = 0.5
        ),
        font = list(size = 10),
        legend = list(
          font = list(size = 10),
          x = 1.02,
          y = 0.5
        ),
        margin = list(l = 10, r = 45, t = 45, b = 10)
      ) |>
      plotly::config(displayModeBar = FALSE)
  })
  
  output$lda_topics_plot <- renderPlot({
    shiny::validate(shiny::need(input$run_lda > 0, "Click 'Run LDA' to generate topics for the currently filtered reviews."))
    
    topic_terms <- lda_topic_data()$topic_terms
    shiny::validate(shiny::need(nrow(topic_terms) > 0, "No LDA topics available for the current filter."))
    
    topic_terms |>
      mutate(term = tidytext::reorder_within(term, beta, topic)) |>
      ggplot(aes(beta, term, fill = factor(topic))) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~topic, scales = "free_y") +
      tidytext::scale_y_reordered() +
      theme_minimal(base_size = 12) +
      labs(
        title = "LDA Topic Modeling",
        subtitle = "Top terms by topic from filtered reviews",
        x = "Topic-Term Probability",
        y = NULL
      )
  })
  
  # -------------------------------------------------------------------
  # Chat helpers, to analyze the plots and provide some insights
  # -------------------------------------------------------------------
  
  infer_advanced_chart <- function(user_question) {
    q <- tolower(user_question %||% "")
    if (grepl("negative|bad|complaint|complaints|downside", q)) return("negative")
    if (grepl("positive|good|praise|upside", q)) return("positive")
    if (grepl("lda|topic|topics", q)) return("lda")
    if (grepl("issue|issues|problem|problems|price|functionality|quality|usability|fit|packaging|support|durability|accuracy", q)) return("issues")
    if (grepl("pie|split|share|percent|percentage|proportion|neutral", q)) return("pie")
    "all"
  }
  
  advanced_chart_title <- function(chart_key) {
    switch(
      chart_key,
      positive = "Positive Sentiment Over Time",
      negative = "Negative Sentiment Over Time",
      issues = "Most Common Issue by Category",
      pie = "Sentiment Split",
      lda = "LDA Topics",
      all = "All Advanced Insights Charts",
      "All Advanced Insights Charts"
    )
  }
  
  # Track the previously selected chart so follow-up questions like
  # "From the charts?" stay focused on the same chart.
  last_advanced_chart <- reactiveVal(NULL)
  
  is_followup_chart_question <- function(user_question) {
    q <- tolower(user_question %||% "")
    grepl("from the charts?|from charts?|from the plot|from the graph|that chart|this chart", q)
  }
  
  is_lda_question <- function(user_question) {
    q <- tolower(user_question %||% "")
    grepl("\\blda\\b|topic modeling|topics?\\b", q)
  }
  
  lda_is_available <- reactive({
    isTruthy(input$run_lda) && input$run_lda > 0
  })
  
  unavailable_plot <- function(title, message) {
    ggplot() +
      theme_void(base_size = 14) +
      annotate(
        "text",
        x = 0,
        y = 0,
        label = message,
        size = 5
      ) +
      labs(title = title)
  }
  
  save_unavailable_chart_png <- function(title, message) {
    file <- tempfile(fileext = ".png")
    png(filename = file, width = 1200, height = 850, res = 120)
    print(unavailable_plot(title, message))
    dev.off()
    file
  }
  
  current_advanced_plot <- function(chart_key) {
    switch(
      chart_key,
      positive = positive_plot(),
      negative = negative_plot(),
      issues = common_issue_plot(),
      pie = sentiment_pie_plot(),
      lda = {
        if (!lda_is_available()) {
          return(unavailable_plot(
            "LDA Topic Modeling",
            "LDA topic chart is not available yet. Click Run LDA to generate topics."
          ))
        }
        topic_terms <- lda_topic_data()$topic_terms
        topic_terms |>
          mutate(term = tidytext::reorder_within(term, beta, topic)) |>
          ggplot(aes(beta, term, fill = factor(topic))) +
          geom_col(show.legend = FALSE) +
          facet_wrap(~topic, scales = "free_y") +
          tidytext::scale_y_reordered() +
          theme_minimal(base_size = 12) +
          labs(title = "LDA Topic Modeling", x = "Topic-Term Probability", y = NULL)
      },
      positive_plot()
    )
  }
  
  save_advanced_chart_png <- function(chart_key) {
    file <- tempfile(fileext = ".png")
    png(filename = file, width = 1200, height = 850, res = 120)
    
    if (identical(chart_key, "all")) {
      grid::grid.newpage()
      grid::pushViewport(grid::viewport(layout = grid::grid.layout(2, 2)))
      print(positive_plot(), vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 1))
      print(negative_plot(), vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 2))
      print(common_issue_plot(), vp = grid::viewport(layout.pos.row = 2, layout.pos.col = 1))
      print(sentiment_pie_plot(), vp = grid::viewport(layout.pos.row = 2, layout.pos.col = 2))
    } else {
      print(current_advanced_plot(chart_key))
    }
    
    dev.off()
    file
  }
  
  make_spike_evidence_context <- function(adv, chart_key) {
    df <- adv$filtered_reviews
    sentiment_df <- adv$sentiment_scores
    tidy_reviews <- adv$tidy_reviews
    issue_hits <- adv$issue_hits
    
    daily_sentiment <- sentiment_df |>
      group_by(review_date) |>
      summarise(
        positive_words = sum(positive, na.rm = TRUE),
        negative_words = sum(negative, na.rm = TRUE),
        total_sentiment_words = positive_words + negative_words,
        review_count = n(),
        .groups = "drop"
      )
    
    if (nrow(daily_sentiment) == 0) return("No daily sentiment data available.")
    
    positive_spike <- daily_sentiment |> arrange(desc(positive_words), desc(review_count)) |> slice_head(n = 1)
    negative_spike <- daily_sentiment |> arrange(desc(negative_words), desc(review_count)) |> slice_head(n = 1)
    total_spike <- daily_sentiment |> arrange(desc(total_sentiment_words), desc(review_count)) |> slice_head(n = 1)
    
    selected_spike_date <- dplyr::case_when(
      identical(chart_key, "positive") ~ positive_spike$review_date[1],
      identical(chart_key, "negative") ~ negative_spike$review_date[1],
      TRUE ~ total_spike$review_date[1]
    )
    
    score_col <- dplyr::case_when(
      identical(chart_key, "positive") ~ "positive",
      identical(chart_key, "negative") ~ "negative",
      TRUE ~ "total"
    )
    
    spike_review_scores <- sentiment_df |>
      filter(review_date == selected_spike_date) |>
      mutate(total = positive + negative) |>
      select(primary_id, positive, negative, net_sentiment, sentiment_group, total)
    
    spike_reviews <- df |>
      filter(review_date == selected_spike_date) |>
      select(primary_id, review_date, category, rating, text) |>
      left_join(spike_review_scores, by = "primary_id") |>
      mutate(
        positive = coalesce(positive, 0L),
        negative = coalesce(negative, 0L),
        total = coalesce(total, 0L),
        net_sentiment = coalesce(net_sentiment, 0L),
        sentiment_group = coalesce(sentiment_group, "Neutral"),
        sort_score = dplyr::case_when(
          score_col == "positive" ~ as.numeric(positive),
          score_col == "negative" ~ as.numeric(negative),
          TRUE ~ as.numeric(total)
        ),
        text_short = stringr::str_trunc(text, 500)
      ) |>
      arrange(desc(sort_score), desc(abs(net_sentiment)))
    
    top_words <- tidy_reviews |>
      filter(review_date == selected_spike_date) |>
      anti_join(stop_words, by = "word") |>
      count(word, sort = TRUE) |>
      slice_head(n = 20) |>
      mutate(txt = paste0(word, "=", n)) |>
      pull(txt)
    
    top_issues <- issue_hits |>
      filter(review_date == selected_spike_date) |>
      count(issue, sort = TRUE) |>
      slice_head(n = 10) |>
      mutate(txt = paste0(issue, "=", n)) |>
      pull(txt)
    
    review_evidence <- spike_reviews |>
      slice_head(n = 20) |>
      mutate(
        row_text = paste0(
          "- ID: ", primary_id,
          " | Date: ", review_date,
          " | Category: ", category,
          " | Rating: ", rating,
          " | Sentiment group: ", sentiment_group,
          " | Positive words: ", positive,
          " | Negative words: ", negative,
          " | Net sentiment: ", net_sentiment,
          " | Review: ", text_short
        )
      ) |>
      pull(row_text)
    
    if (length(top_words) == 0) top_words <- "No non-stopword terms found on spike date."
    if (length(top_issues) == 0) top_issues <- "No issue keyword matches found on spike date."
    if (length(review_evidence) == 0) review_evidence <- "No review excerpts found on spike date."
    
    paste(
      "Spike analysis from filtered review text:",
      "\nPositive spike date:", paste0(positive_spike$review_date[1], " (positive words=", positive_spike$positive_words[1], ", reviews=", positive_spike$review_count[1], ")"),
      "\nNegative spike date:", paste0(negative_spike$review_date[1], " (negative words=", negative_spike$negative_words[1], ", reviews=", negative_spike$review_count[1], ")"),
      "\nOverall sentiment-word spike date:", paste0(total_spike$review_date[1], " (positive+negative words=", total_spike$total_sentiment_words[1], ", reviews=", total_spike$review_count[1], ")"),
      "\nSelected spike date for evidence:", selected_spike_date,
      "\nTop words on selected spike date:", paste(top_words, collapse = ", "),
      "\nTop issues on selected spike date:", paste(top_issues, collapse = ", "),
      "\nActual review excerpts from selected spike date:\n", paste(review_evidence, collapse = "\n")
    )
  }
  
  retrieve_relevant_reviews <- function(user_question, df, top_n = 8) {
    if (is.null(user_question) || !nzchar(user_question)) {
      return("No user question was provided for retrieval.")
    }
    
    if (nrow(df) == 0) {
      return("No filtered reviews are available for retrieval.")
    }
    
    df <- df |>
      mutate(
        review_row = row_number(),
        text = as.character(text),
        text = dplyr::coalesce(text, "")
      )
    
    query_terms <- make_query_terms(user_question)
    
    if (length(query_terms) == 0) {
      return("No usable retrieval terms were found in the user question.")
    }
    
    review_tokens <- df |>
      select(review_row, primary_id, review_date, category, rating, text) |>
      tidytext::unnest_tokens(word, text) |>
      mutate(word = textstem::lemmatize_words(word)) |>
      anti_join(tidytext::stop_words, by = "word") |>
      filter(stringr::str_detect(word, "^[a-z]+$"))
    
    if (nrow(review_tokens) == 0) {
      return("No review tokens were available for retrieval.")
    }
    
    review_word_counts <- review_tokens |>
      count(review_row, word, name = "term_count")
    
    doc_count <- n_distinct(review_word_counts$review_row)
    
    idf <- review_word_counts |>
      distinct(review_row, word) |>
      count(word, name = "doc_freq") |>
      mutate(idf = log((doc_count + 1) / (doc_freq + 1)) + 1)
    
    scored_reviews <- review_word_counts |>
      filter(word %in% query_terms) |>
      left_join(idf, by = "word") |>
      mutate(score_piece = term_count * idf) |>
      group_by(review_row) |>
      summarise(
        retrieval_score = sum(score_piece, na.rm = TRUE),
        matched_terms = paste(unique(word), collapse = ", "),
        .groups = "drop"
      ) |>
      arrange(desc(retrieval_score)) |>
      slice_head(n = top_n) |>
      left_join(df, by = "review_row")
    
    if (nrow(scored_reviews) == 0) {
      # Fallback: return a few filtered reviews instead of failing.
      scored_reviews <- df |>
        slice_head(n = min(top_n, nrow(df))) |>
        mutate(
          retrieval_score = 0,
          matched_terms = "No direct keyword overlap"
        )
    }
    
    scored_reviews |>
      transmute(
        evidence = paste0(
          "- ID: ", primary_id,
          " | Date: ", review_date,
          " | Category: ", category,
          " | Rating: ", rating,
          " | Retrieval score: ", round(retrieval_score, 3),
          " | Matched terms: ", matched_terms,
          " | Review: ", stringr::str_trunc(text, 500)
        )
      ) |>
      pull(evidence) |>
      paste(collapse = "\n")
  }

  ## Build the chatbot promopts
  advanced_chat <- chat_anthropic(
    model = "claude-haiku-4-5",
    system_prompt = paste(
      "You are an NLP dashboard analyst for customer review data.",
      "Answer the user's specific question using the supplied chart image and only the context relevant to the selected chart.",
      "Only answer from the selected chart and the context explicitly tied to that selected chart.",
      "If the selected chart is unavailable, not visible, or not generated, say that directly and stop.",
      "Do not substitute another chart when the user asks about a specific chart.",
      "For follow-up questions like 'from the charts?' or 'from the graph?', continue using the chart from the previous user turn when possible.",
      "If the user asks about LDA and LDA has not been run, say the LDA Topic Modeling chart is not currently available and tell the user to click Run LDA.",
      "When the user asks what happened, what caused a spike, why a metric changed, or what is driving a result, analyze actual review text only when that evidence is relevant to the selected chart.",
      "Use only the evidence relevant to the selected chart; do not use unrelated chart summaries.",
      "Use cautious language: say the reviews suggest something, not that they prove an external cause.",
      "Plain bullets only: normally 3 to 5 bullets, no headings, no bold. Be specific and evidence-based.",
      "Do not use large Markdown headings. Do not start responses with # or ## headings. Use short bold section labels instead."
    )
  )
  
  observeEvent(input$advanced_chat_user_input, {
    adv <- advanced_data()
    df <- adv$filtered_reviews
    sentiment_df <- adv$sentiment_scores
    issue_counts <- adv$issue_counts
    biggest_issues <- adv$biggest_issue
    req(nrow(df) > 0)
    
    inferred_chart <- infer_advanced_chart(input$advanced_chat_user_input)
    
    chart_key <- if (
      is_followup_chart_question(input$advanced_chat_user_input) &&
      !is.null(last_advanced_chart())
    ) {
      last_advanced_chart()
    } else {
      inferred_chart
    }
    
    last_advanced_chart(chart_key)
    chart_title <- advanced_chart_title(chart_key)
    
    plot_file <- tryCatch(
      {
        if (identical(chart_key, "lda") && !lda_is_available()) {
          save_unavailable_chart_png(
            "LDA Topic Modeling",
            "LDA topic chart is not available yet. Click Run LDA to generate topics."
          )
        } else {
          save_advanced_chart_png(chart_key)
        }
      },
      error = function(e) {
        save_unavailable_chart_png(
          chart_title,
          paste("The selected chart is not available:", e$message)
        )
      }
    )
    
    if (identical(chart_key, "lda") && !lda_is_available()) {
      chat_append(
        "advanced_chat",
        advanced_chat$stream_async(
          content_image_file(plot_file),
          sprintf(
            "Current filter: %s (%s of %s rows).",
            qc_vals$title() %||% "none (all data)",
            scales::comma(nrow(df)),
            scales::comma(nrow(data_for_app))
          ),
          paste(
            "The user asked about the LDA Topic Modeling chart.",
            "The LDA chart is not currently available or visible because Run LDA has not been clicked for the current filtered data.",
            "Do not answer from the positive sentiment, negative sentiment, issue, sentiment split, or any other Advanced Insights chart.",
            "Do not infer LDA results from sentiment, issue, or review evidence.",
            "State that the LDA chart is unavailable and tell the user to click Run LDA.",
            "User question:", input$advanced_chat_user_input
          )
        )
      )
      return()
    }
    
    daily_sentiment <- sentiment_df |>
      group_by(review_date) |>
      summarise(
        positive_words = sum(positive, na.rm = TRUE),
        negative_words = sum(negative, na.rm = TRUE),
        total_sentiment_words = positive_words + negative_words,
        reviews = n(),
        .groups = "drop"
      )
    
    top_positive_day <- daily_sentiment |> arrange(desc(positive_words)) |> slice_head(n = 1)
    top_negative_day <- daily_sentiment |> arrange(desc(negative_words)) |> slice_head(n = 1)
    top_overall_day <- daily_sentiment |> arrange(desc(total_sentiment_words)) |> slice_head(n = 1)
    
    top_issues <- issue_counts |>
      group_by(issue) |>
      summarise(total = sum(n), .groups = "drop") |>
      arrange(desc(total)) |>
      slice_head(n = 5) |>
      mutate(txt = paste0(issue, "=", total)) |>
      pull(txt)
    
    biggest_issue_context <- biggest_issues |>
      mutate(txt = paste0(category, ": ", issue, " (", issue_count, " reviews)")) |>
      pull(txt)
    
    top_lda_terms <- if (lda_is_available()) {
      tryCatch({
        lda_topic_data()$topic_terms |>
          group_by(topic) |>
          summarise(terms = paste(term, collapse = ", "), .groups = "drop") |>
          mutate(txt = paste0("Topic ", topic, ": ", terms)) |>
          pull(txt)
      }, error = function(e) {
        "LDA topics were not available for the current filtered data. The user may need to click Run LDA."
      })
    } else {
      "LDA topics were not available because Run LDA has not been clicked for the current filtered data."
    }
    
    sentiment_split <- sentiment_df |>
      count(sentiment_group) |>
      mutate(txt = paste0(sentiment_group, "=", n)) |>
      pull(txt)
    
    filter_context <- sprintf(
      "Current filter: %s (%s of %s rows).",
      qc_vals$title() %||% "none (all data)",
      scales::comma(nrow(df)),
      scales::comma(nrow(data_for_app))
    )
    
    plot_context <- paste(
      "Charts visible in Advanced Insights: Positive Sentiment Over Time, Negative Sentiment Over Time, Most Common Issue by Category, Sentiment Split, LDA Topics",
      "\nChart selected for this answer:", chart_title,
      "\nFiltered review date range:", paste(min(df$review_date, na.rm = TRUE), "to", max(df$review_date, na.rm = TRUE)),
      "\nPositive reviews:", sum(sentiment_df$sentiment_group == "Positive", na.rm = TRUE),
      "\nNegative reviews:", sum(sentiment_df$sentiment_group == "Negative", na.rm = TRUE),
      "\nNeutral reviews:", sum(sentiment_df$sentiment_group == "Neutral", na.rm = TRUE),
      "\nHighest positive-word day:", paste0(top_positive_day$review_date, "=", top_positive_day$positive_words, " positive words"),
      "\nHighest negative-word day:", paste0(top_negative_day$review_date, "=", top_negative_day$negative_words, " negative words"),
      "\nHighest overall sentiment-word day:", paste0(top_overall_day$review_date, "=", top_overall_day$total_sentiment_words, " positive+negative words"),
      "\nTop issues in filtered data:", paste(top_issues, collapse = ", "),
      "\nMost common issue by category:", paste(biggest_issue_context, collapse = " | "),
      "\nLDA topic terms:", paste(top_lda_terms, collapse = " | "),
      "\nSentiment split:", paste(sentiment_split, collapse = ", "),
      "\nUser question:", input$advanced_chat_user_input
    )
    
    spike_evidence_context <- make_spike_evidence_context(adv, chart_key)
    
    rag_context <- paste(
      "Locally retrieved reviews used as evidence for the user's exact question:",
      retrieve_relevant_reviews(input$advanced_chat_user_input, df, top_n = 8),
      sep = "\n"
    )
    
    chat_append(
      "advanced_chat",
      advanced_chat$stream_async(
        content_image_file(plot_file),
        filter_context,
        plot_context,
        spike_evidence_context,
        rag_context
      )
    )
  })
  
  # ============================================================
  # Summary outputs
  # ============================================================
  
  output$avg_rating <- renderText({
    df <- filtered_reviews()
    req(nrow(df) > 0)
    round(mean(df$rating, na.rm = TRUE), 2)
  })
  
  output$num_responses <- renderText({
    df <- filtered_reviews()
    comma(nrow(df))
  })
  
output$past_month_reviews <- renderText({
  df <- filtered_reviews() |>
    mutate(review_date = as.Date(review_date))

  req(nrow(df) > 0)

  actual_today <- Sys.Date()
  current_month_start <- lubridate::floor_date(actual_today, unit = "month")

  scales::comma(sum(
    df$review_date >= current_month_start &
      df$review_date <= actual_today,
    na.rm = TRUE
  ))
})
  
  output$filters_applied <- renderUI({
    tags$div(
      style = "font-size: 0.95rem; line-height: 1.05;",
      tags$strong("Current filter:"),
      tags$br(),
      qc_vals$title() %||% "No filters applied"
    )
  })
  
  output$top_category <- renderUI({
    df <- filtered_reviews()
    req(nrow(df) > 0)
    top_cat <- df |> count(category, sort = TRUE) |> slice_head(n = 1)
    
    tags$div(
      style = "font-size: 0.95rem; line-height: 1.05; word-break: break-word; white-space: normal;",
      tags$div(as.character(top_cat$category)),
      tags$div(style = "font-size: 0.72rem; margin-top: 0.2rem;", paste0("(", scales::comma(top_cat$n), ")"))
    )
  })
  
  output$empty_box <- renderUI(tags$div(""))
  
  # -------------------------------------------------------------------
  # Main chart
  # -------------------------------------------------------------------
  
  # ---- Adjust some themes and font sizes----
  big_chart_theme <- theme_minimal(base_size = 10) +
    theme(
      plot.title = element_text(size = 12, face = "bold", hjust = 0.7),
      plot.subtitle = element_text(size = 11),
      axis.title = element_text(size = 11),
      axis.text = element_text(size = 11),
      legend.title = element_text(size = 11),
      legend.text = element_text(size = 11),
      plot.margin = margin(2, 2, 2, 2)
    )
  
  advanced_chart_theme <- theme_minimal(base_size = 10) +
    theme(
      plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 11),
      axis.text = element_text(size = 11),
      legend.title = element_text(size = 11),
      legend.text = element_text(size = 11),
      strip.text = element_text(size = 11, face = "bold"),
      plot.margin = margin(5, 5, 5, 5)
    )
  
  output$main_chart <- plotly::renderPlotly({
    df <- filtered_reviews()
    req(nrow(df) > 0)
    
    df <- df |>
      mutate(
        review_date = as.Date(review_date),
        rating = as.numeric(rating)
      )
    
    if (input$chart_type == "rating_bar") {
      rating_df <- df |> 
        count(rating) |>
        mutate(percent = n / sum(n))
      
      p <- rating_df |>
        ggplot(aes(
          x = factor(rating),
          y = n,
          text = paste0(
            "Rating: ", rating,
            "<br>Reviews: ", scales::comma(n),
            "<br>Percent: ", scales::percent(percent, accuracy = 0.1)
          )
        )) +
        geom_col() +
        big_chart_theme +
        labs(
          title = "Review Count by Rating",
          x = "Rating",
          y = "Number of Reviews"
        )
    }
    else if (input$chart_type == "reviews_by_date") {
      p <- df |> 
        count(review_date) |>
        ggplot(aes(
          x = review_date,
          y = n,
          group = 1,
          text = paste0(
            "Date: ", review_date,
            "<br>Reviews: ", scales::comma(n)
          )
        )) +
        geom_line(linewidth = 0.4) +
        geom_point(size = 1.2) +
        big_chart_theme +
        labs(
          title = "Number of Reviews Submitted by Date",
          x = "Review Date",
          y = "Number of Reviews"
        )
      
    } else if (input$chart_type == "avg_rating_week") {
      p <- df |>
        filter(!is.na(review_date), !is.na(rating)) |>
        mutate(review_week = lubridate::floor_date(review_date, unit = "week")) |>
        group_by(review_week) |>
        summarise(
          avg_rating = mean(rating, na.rm = TRUE),
          reviews = n(),
          .groups = "drop"
        ) |>
        ggplot(aes(
          x = review_week,
          y = avg_rating,
          group = 1,
          text = paste0(
            "Week: ", review_week,
            "<br>Average rating: ", round(avg_rating, 2),
            "<br>Reviews: ", scales::comma(reviews)
          )
        )) +
        geom_line(linewidth = 0.4) +
        geom_point(size = 1.2) +
        big_chart_theme +
        labs(
          title = "Average Rating by Week",
          x = "Week",
          y = "Average Rating"
        )
      
    } else if (input$chart_type == "reviews_by_category_rating") {
      category_rating_df <- df |> 
        count(category, rating) |>
        group_by(category) |>
        mutate(percent = n / sum(n)) |>
        ungroup() |>
        mutate(
          rating = factor(rating, levels = c("1", "2", "3", "4", "5"))
        )
      
      p <- category_rating_df |>
        ggplot(aes(
          x = category,
          y = n,
          fill = rating,
          text = paste0(
            "Category: ", category,
            "<br>Rating: ", rating,
            "<br>Reviews: ", scales::comma(n),
            "<br>Percent within category: ", scales::percent(percent, accuracy = 0.1)
          )
        )) +
        geom_col(position = position_stack(reverse = TRUE)) +
        coord_flip() +
        scale_y_continuous(labels = scales::comma) +
        scale_fill_manual(
          values = c(
            "1" = "#D55E00",
            "2" = "#E69F00",
            "3" = "#0072B2",
            "4" = "#009E73",
            "5" = "#F0E442"
          ),
          breaks = c("1", "2", "3", "4", "5")
        ) +
        guides(fill = guide_legend(reverse = FALSE)) +
        big_chart_theme +
        labs(
          title = "Reviews by Category and Rating",
          x = NULL,
          y = "Number of Reviews",
          fill = "Rating"
        )
    } else if (input$chart_type == "rating_group_category") {
      # Create rating groups
      df_grouped <- df |>
        mutate(
          rating_group = case_when(
            rating <= 2 ~ "Low (1-2)",
            rating == 3 ~ "Medium (3)",
            rating >= 4 ~ "High (4-5)",
            TRUE ~ "Unknown"
          ),
          rating_group = factor(
            rating_group,
            levels = c("Low (1-2)", "Medium (3)", "High (4-5)", "Unknown")
          )
        ) |>
        count(category, rating_group) |>
        group_by(category) |>
        mutate(percent = n / sum(n)) |>
        ungroup()
      
      # Plot
      p <- df_grouped |>
        ggplot(aes(
          x = category,
          y = percent,
          fill = rating_group,
          text = paste0(
            "Category: ", category,
            "<br>Rating group: ", rating_group,
            "<br>Reviews: ", scales::comma(n),
            "<br>Percent: ", scales::percent(percent, accuracy = 0.1)
          )
        )) +
        geom_col(position = position_stack(reverse = TRUE)) +
        coord_flip() +
        scale_y_continuous(labels = scales::percent_format()) +
        scale_fill_manual(
          values = c(
            "Low (1-2)" = "#D55E00",
            "Medium (3)" = "#0072B2",
            "High (4-5)" = "#F0E442",
            "Unknown" = "gray"
          ),
          breaks = c("Low (1-2)", "Medium (3)", "High (4-5)", "Unknown")
        ) +
        big_chart_theme +
        labs(
          title = "Reviews by Category and Rating Group",
          x = NULL,
          y = "Percent of Reviews",
          fill = "Rating Group"
        )
      
    } else if (input$chart_type == "low_rating_category") {
      low_df <- df |> 
        filter(rating <= 2) |> 
        count(category, sort = TRUE)
      
      shiny::validate(
        shiny::need(nrow(low_df) > 0, "No low-rating reviews in the current filter.")
      )
      
      p <- low_df |>
        ggplot(aes(
          x = n,
          y = reorder(category, n),
          text = paste0(
            "Category: ", category,
            "<br>Low-rating reviews: ", scales::comma(n)
          )
        )) +
        geom_col() +
        big_chart_theme +
        labs(
          title = "Low-Rating Reviews by Category",
          subtitle = "Ratings 1-2 only",
          x = "Number of Low-Rating Reviews",
          y = NULL
        )
    }
    
    plotly::ggplotly(p, tooltip = "text") |>
      plotly::layout(
        font = list(size = 12),
        title = list(
          font = list(size = 15),
          x = 0.5
        ),
        xaxis = list(
          title = list(font = list(size = 12)),
          tickfont = list(size = 12)
        ),
        yaxis = list(
          title = list(font = list(size = 12)),
          tickfont = list(size = 12)
        ),
        legend = list(
          font = list(size = 12),
          title = list(font = list(size = 12))
        ),
        margin = list(l = 70, r = 15, t = 35, b = 45)
      ) |>
      plotly::config(displayModeBar = FALSE)
  })
  
  # -------------------------------------------------------------------
  # Review table, to display the raw data (filtered of course)
  # -------------------------------------------------------------------
  
  output$review_table <- renderDT({
    df <- filtered_reviews()
    req(nrow(df) > 0)
    
    df |>
      transmute(Date = review_date, Category = category, Rating = rating, Review = text) |>
      datatable(
        rownames = FALSE,
        filter = "none",
        width = "100%",
        options = list(
          dom = "lfrtip",
          pageLength = 5,
          lengthMenu = c(5, 8, 10, 15, 25, 50),
          scrollX = TRUE,
          scrollY = "18vh",
          scrollCollapse = FALSE,
          autoWidth = FALSE,
          columnDefs = list(
            list(width = "12%", targets = 0),
            list(width = "18%", targets = 1),
            list(width = "8%", targets = 2),
            list(width = "62%", targets = 3)
          )
        )
      )
  })
}

shinyApp(ui, server)
