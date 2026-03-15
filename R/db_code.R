library(tidyverse)
library(dbplyr)
library(DBI)
library(gt)

con <- dbConnect(RPostgres::Postgres(),
                 dbname = 'db_lexicate', 
                 host = 'pg-lexicate-jonathan-c732.d.aivencloud.com',
                 port = 20727,
                 user = 'lexicate',
                 password = Sys.getenv("POSTGRES_PASS"))

get_users <- function(con) {
  tbl(con, I("lexdata.wordlog")) |>
    select(user) |> 
    distinct() |> 
    collect() |> 
    pull("user")
}

get_wordlist <- function(con) {
  tbl(con, I("lexdata.wordlist")) |>
    collect()
}

get_word_log <- function(con, selected_user) {
  word_log <- tbl(con, I("lexdata.wordlog")) |>
    filter(user == selected_user) |>
    collect()

  word_log
}

user_summary <- function(con, user) {
  word_log <- get_word_log(con, user)

  form_log <- word_log |>
    filter(assistance_level == 0) |>
    mutate(result = if_else(success_indicator == 1, "✅", "❌")) |>
    group_by(word) |>
    arrange(event_datetime) |>
    slice_tail(n = 5) |>
    summarise(form = paste0(result, collapse = ""))

  summary_log <- word_log |>
    filter(assistance_level == 0) |>
    summarise(
      last_attempt = max(event_datetime),
      correct = sum(success_indicator),
      attempts = n(),
      .by = word
    ) |>
    full_join(form_log, by = "word") |>
    arrange(desc(last_attempt))

  summary_log
}


progress_summary <- function(con, summary_log) {
  wordlist <- get_wordlist(con) |>
    select(word, difficulty)
  
  progress <- wordlist |> 
    left_join(summary_log, by="word") |> 
    replace_na(list(correct = 0, attempts = 0, form = "")) |> 
    mutate(progress = case_when(
      form == "✅✅✅✅✅" ~ "Learned",
      correct == 0 ~ "Unlearned",
      .default = "Variable"
    )) |> 
    mutate(progress_numeric = case_when(
      progress == "Learned" ~ 1,
      progress == "Unlearned" ~ 0,
      .default = 0.5
    ))
  
  
  progress
}