## Raw data cleaning for demographics
library(tidyverse)

# Use the latest cumulative export:
files <- c("/Users/lumikang/Downloads/results_prod-3.csv")

read_pcibex <- function(path) {
  lines <- read_lines(path)
  lines <- lines[!str_detect(lines, "^\\s*(#|$)")]
  
  read.csv(
    text = lines,
    header = FALSE,
    col.names = paste0("V", 1:26),
    colClasses = "character",
    fill = TRUE,
    comment.char = ""
  ) |>
    as_tibble() |>
    mutate(source_file = path, source_row = row_number())
}

raw <- map(files, read_pcibex) |>
  list_rbind() |>
  mutate(submission = str_c(V1, V2, sep = ":"))

overlap <- raw |>
  distinct(submission, source_file) |>
  count(submission) |>
  filter(n > 1)

if (nrow(overlap) > 0) {
  stop("Some submissions appear in multiple files. Check which exports you selected.")
}

raw <- raw |>
  mutate(session_id = sprintf("S%03d", match(submission, unique(submission))))

# 1. Demographics ---------------------------------------------------------
demo_answers <- raw |>
  filter(V6 == "participants", V10 == "_Trial_", V11 == "End") |>
  distinct(session_id, .keep_all = TRUE) |>
  transmute(
    session_id,
    age = parse_integer(V17, na = c("", "NULL", "undefined")),
    gender = V16,
    native_english = V15,
    disorders = V14
  ) |>
  mutate(across(
    c(gender, native_english, disorders),
    ~ replace(.x, .x %in% c("", "NULL", "undefined"), NA_character_)
  ))

session_lists <- raw |>
  filter(
    V6 == "experiment",
    V8 == "Controller-SelfPacedReadingParadigmSentence",
    V22 == "critical",
    !is.na(V18),
    !V18 %in% c("", "NULL", "undefined")
  ) |>
  distinct(session_id, list_number = V18) |>
  group_by(session_id) |>
  summarise(
    list_number = str_c(sort(unique(list_number)), collapse = ", "),
    .groups = "drop"
  )

demographics <- raw |>
  distinct(session_id, .keep_all = TRUE) |>
  transmute(
    session_id,
    sona_id = V13,
    submitted_at_pt = format(
      as.POSIXct(as.numeric(V1), origin = "1970-01-01", tz = "UTC"),
      tz = "America/Los_Angeles",
      format = "%Y-%m-%d %H:%M:%S %Z"
    )
  ) |>
  left_join(session_lists, by = "session_id") |>
  left_join(demo_answers, by = "session_id")

# Accuracy on comprehension questions
comprehension <- raw |>
  filter(
    V6 == "experiment",
    V8 == "Selector",
    V9 == "answer",
    V10 == "Selection",
    V11 %in% c("answer_correct", "answer_wrong")
  ) |>
  transmute(
    session_id,
    list_number = V18,
    trial = as.integer(V19),
    item = V20,
    condition = V21,
    item_type = V22,
    correct = V11 == "answer_correct"
  )

accuracy_by_session <- comprehension |>
  group_by(session_id) |>
  summarise(
    n_answered = n(),
    n_correct = sum(correct),
    accuracy_pct = round(100 * n_correct / n_answered, 1),
    .groups = "drop"
  )

demographics <- demographics |>
  left_join(accuracy_by_session, by = "session_id")

write_csv(demographics, "demographics.csv", na = "")
