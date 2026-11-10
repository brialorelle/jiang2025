## Data Prep using demographics
library(tidyverse)

# import the required file
demographics_file <- "/Users/lumikang/Documents/GitHub/-replication/data/pilot_results/demographics.csv"

# Use the same raw export(s), in the same order, as the demographics script.
files <- c(
  "/Users/lumikang/Documents/GitHub/-replication/data/pilot_results/results_prod-3.csv"
)

reference_file <- "/Users/lumikang/Documents/GitHub/-replication/data/pilot_results/rawdata.csv"
output_file <- "analysis_data.csv"

# 1. Keep participants with at least 80% comprehension accuracy.
demographics <- read_csv(
  demographics_file,
  col_types = cols(
    session_id = col_character(),
    sona_id = col_character(),
    n_answered = col_integer(),
    n_correct = col_integer()
  )
)

eligible_session_ids <- demographics |>
  filter(n_answered > 0, n_correct / n_answered >= 0.80) |>
  distinct(session_id) |>
  pull(session_id)

if (!length(eligible_session_ids)) {
  stop("No sessions meet the 80% comprehension accuracy criterion.")
}

eligible_demographics <- demographics |>
  filter(session_id %in% eligible_session_ids)

View(eligible_demographics)

# 2. Read the raw file and recreate the eligible participants list
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
  mutate(
    submission = str_c(V1, V2, sep = ":"),
    session_id = sprintf("S%03d", match(submission, unique(submission)))
  )

overlapping_submissions <- raw |>
  distinct(submission, source_file) |>
  count(submission) |>
  filter(n > 1)

if (nrow(overlapping_submissions) > 0) {
  stop("Some submissions appear in multiple raw exports.")
}

if (!all(eligible_session_ids %in% raw$session_id)) {
  stop("Some eligible session IDs are missing from the raw export.")
}

rt_log_key <- setdiff(paste0("V", 1:26), "V24")

raw <- raw |>
  mutate(
    is_critical_word = coalesce(
      V6 == "experiment" &
        V8 == "Controller-SelfPacedReadingParadigmSentence" &
        V22 == "critical",
      FALSE
    )
  ) |>
  filter(
    !is_critical_word |
      !duplicated(pick(all_of(rt_log_key)))
  ) |>
  select(-is_critical_word)

# 3. Extract the RTs of the critical words and 5 spillover regions
critical <- raw |>
  filter(
    session_id %in% eligible_session_ids,
    V6 == "experiment",
    V8 == "Controller-SelfPacedReadingParadigmSentence",
    V22 == "critical"
  ) |>
  transmute(
    session_id,
    list_number = as.integer(V18),
    trial = as.integer(V19),
    item = as.integer(V20),
    condition = V21,
    item_order = V4,
    inner_element = V5,
    word_position = as.integer(V10),
    sentence_length = lengths(str_split(V25, fixed("*"))),
    words_from_end = sentence_length - word_position + 1L,
    word = map_chr(V11, URLdecode),
    rt_ms = as.numeric(V23),
    source_file,
    source_row
  )

if (!nrow(critical) ||
    anyNA(critical$words_from_end) ||
    any(critical$sentence_length < 6L |
        critical$word_position < 1L |
        critical$words_from_end < 1L)) {
  stop("Check the critical trials and logged word positions.")
}

last6 <- critical |>
  filter(between(words_from_end, 1L, 6L)) |>
  group_by(session_id, item_order, inner_element, word_position) |>
  mutate(repeated_word_record = n() > 1L) |>
  ungroup()

# 4. Trim reading times
rt_bounded <- last6 |>
  filter(between(rt_ms, 100, 5000))

if (nrow(rt_bounded) < 2L) {
  stop("Need at least two word RTs after the 100–5000 ms filter.")
}

grand_mean <- mean(rt_bounded$rt_ms)
grand_sd <- sd(rt_bounded$rt_ms)

rt_prepared <- rt_bounded |>
  filter(between(rt_ms, grand_mean - 2.5 * grand_sd, grand_mean + 2.5 * grand_sd))

trimming_summary <- tibble(
  eligible_sessions = length(eligible_session_ids),
  before_trimming = nrow(last6),
  removed_by_absolute_bounds = nrow(last6) - nrow(rt_bounded),
  removed_by_2_5sd = nrow(rt_bounded) - nrow(rt_prepared),
  retained = nrow(rt_prepared),
  grand_mean_ms = grand_mean,
  grand_sd_ms = grand_sd
)

print(trimming_summary)

# 5. Format rt for data analysis
condition_key <- tribble(
  ~condition, ~context,      ~plausibility,
  "A",        "realistic",   "implausible",
  "B",        "unrealistic", "plausible",
  "C",        "unrealistic", "implausible",
  "D",        "realistic",   "plausible"
)

cl_lookup <- read_csv(reference_file, show_col_types = FALSE) |>
  transmute(item = as.integer(item), context, plausibility, CL) |>
  distinct()

position_labels <- c("critical", paste0("spill", 1:5))

analysis_data <- rt_prepared |>
  mutate(
    subject = session_id,
    list = list_number,
    # Assumes the critical word is sixth from the end.
    position = factor(
      position_labels[7L - words_from_end],
      levels = position_labels
    )
  ) |>
  left_join(condition_key, by = "condition", relationship = "many-to-one") |>
  left_join(
    cl_lookup,
    by = c("item", "context", "plausibility"),
    relationship = "many-to-one"
  ) |>
  arrange(subject, item, position) |>
  select(subject, list, item, position, context, plausibility, CL, RT = rt_ms)

if (anyNA(analysis_data$CL)) {
  stop("Some item/condition combinations did not match the reference CL values.")
}

if (anyDuplicated(analysis_data[c("subject", "item", "position")])) {
  stop("More than one RT remains for a subject/item/position.")
}

write_csv(analysis_data, output_file)
