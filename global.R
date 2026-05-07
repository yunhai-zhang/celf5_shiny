# global.R — CELF-5 Assessment App (tidyverse rebuild + SQLite norms)
# 常范数据从 celf5_norms.db 加载（来自 CELF-5 Examiner's Manual Appendix A/B）

library(shiny)
library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)
library(RSQLite)
library(glue)

# ─────────────────────────────────────────────────────────────
# 0. 常范数据库
# ─────────────────────────────────────────────────────────────
NORMS_DB <- "/home/yzhang/clawfiles/celf5_shiny/celf5_norms.db"

load_norms_db <- function() {
  con <- dbConnect(SQLite(), NORMS_DB)
  on.exit(dbDisconnect(con), add = TRUE)
  list(
    norms      = dbReadTable(con, "norms_table"),
    norms_ci   = dbReadTable(con, "norms_ci"),
    composite  = dbReadTable(con, "composite_table"),
    composite_ci = dbReadTable(con, "composite_ci_table")
  )
}

.normas <- load_norms_db()
NORMS_TABLE      <- .normas$norms
NORMS_CI_TABLE   <- .normas$norms_ci
COMPOSITE_TABLE  <- .normas$composite
COMPOSITE_CI_TABLE <- .normas$composite_ci

# ─────────────────────────────────────────────────────────────
# 0b. 业务数据库
# ─────────────────────────────────────────────────────────────
DB_PATH <- "/home/yzhang/clawfiles/celf5_shiny/celf5_assessments.db"

get_con <- function() {
  dbConnect(SQLite(), DB_PATH)
}

init_db <- function() {
  con <- get_con()
  on.exit(dbDisconnect(con), add = TRUE)

  dbExecute(con, "CREATE TABLE IF NOT EXISTS patients (
    id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT NOT NULL, dob TEXT NOT NULL,
    gender TEXT, examiner TEXT, notes TEXT,
    created_at TEXT DEFAULT (datetime('now','localtime')))")

  dbExecute(con, "CREATE TABLE IF NOT EXISTS assessments (
    id INTEGER PRIMARY KEY AUTOINCREMENT, patient_id INTEGER NOT NULL,
    assessment_date TEXT NOT NULL, age_years INTEGER, age_months INTEGER, age_days INTEGER,
    age_group TEXT, completed_at TEXT DEFAULT (datetime('now','localtime')),
    status TEXT DEFAULT 'in_progress', FOREIGN KEY (patient_id) REFERENCES patients(id))")

  dbExecute(con, "CREATE TABLE IF NOT EXISTS responses (
    id INTEGER PRIMARY KEY AUTOINCREMENT, assessment_id INTEGER NOT NULL,
    subtest TEXT NOT NULL, item_number INTEGER NOT NULL, response_text TEXT, score INTEGER,
    created_at TEXT DEFAULT (datetime('now','localtime')),
    FOREIGN KEY (assessment_id) REFERENCES assessments(id),
    UNIQUE(assessment_id, subtest, item_number))")

  dbExecute(con, "CREATE TABLE IF NOT EXISTS subtest_scores (
    id INTEGER PRIMARY KEY AUTOINCREMENT, assessment_id INTEGER NOT NULL,
    subtest TEXT NOT NULL, raw_score INTEGER, scaled_score INTEGER,
    FOREIGN KEY (assessment_id) REFERENCES assessments(id),
    UNIQUE(assessment_id, subtest))")

  dbExecute(con, "CREATE TABLE IF NOT EXISTS composite_scores (
    id INTEGER PRIMARY KEY AUTOINCREMENT, assessment_id INTEGER NOT NULL,
    composite TEXT NOT NULL, sum_scaled INTEGER, standard_score INTEGER,
    percentile_rank REAL, confidence_68_lo INTEGER, confidence_68_hi INTEGER,
    confidence_90_lo INTEGER, confidence_90_hi INTEGER,
    confidence_95_lo INTEGER, confidence_95_hi INTEGER,
    FOREIGN KEY (assessment_id) REFERENCES assessments(id),
    UNIQUE(assessment_id, composite))")

  invisible(NULL)
}

init_db()

# ─────────────────────────────────────────────────────────────
# 1. 计算年龄（Manual P32：不四舍五入，借月=30天，借年=12个月）
# ─────────────────────────────────────────────────────────────
calculate_age <- function(dob, assessment_date) {
  dob <- as.Date(dob); ad <- as.Date(assessment_date)
  y1 <- year(dob); m1 <- month(dob); d1 <- day(dob)
  y2 <- year(ad);  m2 <- month(ad);  d2 <- day(ad)
  years  <- y2 - y1
  months <- m2 - m1
  days   <- d2 - d1
  if (days < 0)  { months <- months - 1;  days <- days + 30 }
  if (months < 0){ years  <- years - 1;   months <- months + 12 }
  list(years = years, months = months, days = days)
}

format_age <- function(age) glue("{age$years}y {age$months}m {age$days}d")

get_age_group <- function(age) {
  total_months <- age$years * 12 + age$months
  dplyr::case_when(
    total_months <= 65  ~ "5:0-5:5",
    total_months <= 71  ~ "5:6-5:11",
    total_months <= 77  ~ "6:0-6:5",
    total_months <= 83  ~ "6:6-6:11",
    total_months <= 95  ~ "7:0-7:11",
    total_months <= 107 ~ "8:0-8:11",
    total_months <= 119 ~ "9:0-9:11",
    total_months <= 131 ~ "10:0-10:11",
    total_months <= 143 ~ "11:0-11:11",
    total_months <= 155 ~ "12:0-12:11",
    total_months <= 167 ~ "13:0-13:11",
    total_months <= 179 ~ "14:0-14:11",
    total_months <= 191 ~ "15:0-15:11",
    total_months <= 203 ~ "16:0-16:11",
    TRUE                 ~ "17:0-21:11"
  )
}

# ─────────────────────────────────────────────────────────────
# 2. 各 age group 的测试组合（Manual Table 1.2）
# ─────────────────────────────────────────────────────────────
get_test_composition <- function(age_group) {
  compositions <- list(
    "5:0-5:5"   = c("SC","LC","WS","WC","FD","FS","RS","USP","PP"),
    "5:6-5:11"  = c("SC","LC","WS","WC","FD","FS","RS","USP","PP"),
    "6:0-6:5"   = c("SC","LC","WS","WC","FD","FS","RS","USP","PP"),
    "6:6-6:11"  = c("SC","LC","WS","WC","FD","FS","RS","USP","PP"),
    "7:0-7:11"  = c("SC","LC","WS","WC","FD","FS","RS","USP","PP"),
    "8:0-8:11"  = c("SC","LC","WS","WC","FD","FS","RS","USP","PP"),
    "9:0-9:11"  = c("WC","FD","FS","RS","USP","WD","SA","SR","PP","RC","SW"),
    "10:0-10:11"= c("WC","FD","FS","RS","USP","WD","SA","SR","PP","RC","SW"),
    "11:0-11:11"= c("WC","FD","FS","RS","USP","WD","SA","SR","PP","RC","SW"),
    "12:0-12:11"= c("WC","FD","FS","RS","USP","WD","SA","SR","PP","RC","SW"),
    "13:0-13:11"= c("WC","FD","FS","RS","USP","WD","SA","SR","PP","RC","SW"),
    "14:0-14:11"= c("WC","FD","FS","RS","USP","WD","SA","SR","PP","RC","SW"),
    "15:0-15:11"= c("WC","FD","FS","RS","USP","WD","SA","SR","PP","RC","SW"),
    "16:0-16:11"= c("WC","FD","FS","RS","USP","WD","SA","SR","PP","RC","SW"),
    "17:0-21:11"= c("WC","FD","FS","RS","USP","WD","SA","SR","PP","RC","SW")
  )
  compositions[[age_group]] %||% character(0)
}

# ─────────────────────────────────────────────────────────────
# 3. SUBTEST_DEFS + start_points（Manual Chapter 3）
# ─────────────────────────────────────────────────────────────
SUBTEST_DEFS <- tibble(
  subtest = c("SC","LC","WS","WC","FD","FS","RS","WD","SA","SR","RC","SW","USP","PP"),
  full_name = c("Sentence Comprehension","Linguistic Concepts","Word Structure",
                "Word Classes","Following Directions","Formulated Sentences",
                "Recalling Sentences","Word Definitions","Sentence Assembly",
                "Semantic Relationships","Reading Comprehension","Structured Writing",
                "Understanding Spoken Paragraphs","Pragmatics Profile"),
  max_items = c(42L,36L,34L,40L,32L,48L,48L,21L,20L,20L,28L,44L,18L,15L),
  discontinue_rule = c(4L,4L,4L,4L,4L,4L,4L,4L,4L,4L,0L,0L,0L,0L)
)

SUBTEST_START_POINTS <- list(
  SC  = setNames(list(1L,1L,1L,8L,8L,8L), c("5:0-5:5","5:6-5:11","6:0-6:5","6:6-6:11","7:0-7:11","8:0-8:11")),
  LC  = setNames(list(1L,1L,1L,1L,1L,1L), c("5:0-5:5","5:6-5:11","6:0-6:5","6:6-6:11","7:0-7:11","8:0-8:11")),
  WS  = setNames(list(1L,1L,1L,1L,1L,1L), c("5:0-5:5","5:6-5:11","6:0-6:5","6:6-6:11","7:0-7:11","8:0-8:11")),
  WC  = setNames(list(1L,1L,1L,1L,1L,1L,1L,1L,13L,13L,13L,20L,20L,20L,20L),
                 c("5:0-5:5","5:6-5:11","6:0-6:5","6:6-6:11","7:0-7:11","8:0-8:11",
                   "9:0-9:11","10:0-10:11","11:0-11:11","12:0-12:11","13:0-13:11",
                   "14:0-14:11","15:0-15:11","16:0-16:11","17:0-21:11")),
  FD  = setNames(list(1L,1L,1L,1L,1L,1L,1L,1L,1L,1L,1L,1L,1L,1L,1L),
                 c("5:0-5:5","5:6-5:11","6:0-6:5","6:6-6:11","7:0-7:11","8:0-8:11",
                   "9:0-9:11","10:0-10:11","11:0-11:11","12:0-12:11","13:0-13:11",
                   "14:0-14:11","15:0-15:11","16:0-16:11","17:0-21:11")),
  FS  = setNames(list(1L,1L,1L,1L,1L,1L,1L,1L,1L,1L,1L,1L,1L,1L,1L),
                 c("5:0-5:5","5:6-5:11","6:0-6:5","6:6-6:11","7:0-7:11","8:0-8:11",
                   "9:0-9:11","10:0-10:11","11:0-11:11","12:0-12:11","13:0-13:11",
                   "14:0-14:11","15:0-15:11","16:0-16:11","17:0-21:11")),
  RS  = setNames(list(1L,1L,1L,1L,16L,16L,16L,16L,16L,16L,16L,16L,16L,16L,16L),
                 c("5:0-5:5","5:6-5:11","6:0-6:5","6:6-6:11","7:0-7:11","8:0-8:11",
                   "9:0-9:11","10:0-10:11","11:0-11:11","12:0-12:11","13:0-13:11",
                   "14:0-14:11","15:0-15:11","16:0-16:11","17:0-21:11")),
  WD  = setNames(list(1L,1L,1L,1L,1L,1L,1L,1L,1L,1L,1L,1L,1L,1L,1L),
                 c("5:0-5:5","5:6-5:11","6:0-6:5","6:6-6:11","7:0-7:11","8:0-8:11",
                   "9:0-9:11","10:0-10:11","11:0-11:11","12:0-12:11","13:0-13:11",
                   "14:0-14:11","15:0-15:11","16:0-16:11","17:0-21:11")),
  SA  = setNames(list(1L,1L,1L,4L,4L,4L,4L,4L,4L,4L,4L,4L,4L,4L,4L),
                 c("5:0-5:5","5:6-5:11","6:0-6:5","6:6-6:11","7:0-7:11","8:0-8:11",
                   "9:0-9:11","10:0-10:11","11:0-11:11","12:0-12:11","13:0-13:11",
                   "14:0-14:11","15:0-15:11","16:0-16:11","17:0-21:11")),
  SR  = setNames(list(1L,1L,1L,1L,1L,4L,4L,4L,4L,4L,4L,4L,4L,4L,4L),
                 c("5:0-5:5","5:6-5:11","6:0-6:5","6:6-6:11","7:0-7:11","8:0-8:11",
                   "9:0-9:11","10:0-10:11","11:0-11:11","12:0-12:11","13:0-13:11",
                   "14:0-14:11","15:0-15:11","16:0-16:11","17:0-21:11")),
  RC  = setNames(list(1L,1L,1L,1L,1L,1L,1L,1L,1L,1L,1L,1L,1L,1L,1L),
                 c("5:0-5:5","5:6-5:11","6:0-6:5","6:6-6:11","7:0-7:11","8:0-8:11",
                   "9:0-9:11","10:0-10:11","11:0-11:11","12:0-12:11","13:0-13:11",
                   "14:0-14:11","15:0-15:11","16:0-16:11","17:0-21:11")),
  SW  = setNames(list(1L,1L,1L,1L,1L,1L,1L,1L,1L,1L,1L,1L,1L,1L,1L),
                 c("5:0-5:5","5:6-5:11","6:0-6:5","6:6-6:11","7:0-7:11","8:0-8:11",
                   "9:0-9:11","10:0-10:11","11:0-11:11","12:0-12:11","13:0-13:11",
                   "14:0-14:11","15:0-15:11","16:0-16:11","17:0-21:11")),
  USP = setNames(list(1L,1L,1L,1L,1L,1L,1L,1L,1L,1L,1L,1L,1L,1L,1L),
                 c("5:0-5:5","5:6-5:11","6:0-6:5","6:6-6:11","7:0-7:11","8:0-8:11",
                   "9:0-9:11","10:0-10:11","11:0-11:11","12:0-12:11","13:0-13:11",
                   "14:0-14:11","15:0-15:11","16:0-16:11","17:0-21:11")),
  PP  = setNames(list(1L,1L,1L,1L,1L,1L,1L,1L,1L,1L,1L,1L,1L,1L,1L),
                 c("5:0-5:5","5:6-5:11","6:0-6:5","6:6-6:11","7:0-7:11","8:0-8:11",
                   "9:0-9:11","10:0-10:11","11:0-11:11","12:0-12:11","13:0-13:11",
                   "14:0-14:11","15:0-15:11","16:0-16:11","17:0-21:11"))
)

get_start_point <- function(subtest, age_group) {
  sp <- SUBTEST_START_POINTS[[subtest]]
  if (is.null(sp)) return(1L)
  sp[[age_group]] %||% 1L
}

get_discontinue_rule <- function(subtest) {
  SUBTEST_DEFS %>% filter(subtest == !!subtest) %>%
    pull(discontinue_rule) %>% magrittr::extract2(1)
}

# ─────────────────────────────────────────────────────────────
# 3b. age_group 格式转换（questions 表 vs norms 表）
# ─────────────────────────────────────────────────────────────
# questions 表用 "age_5_8" / "age_9_11" / "age_12_14" / "age_15_21"
# norms 表用 "5:0-5:5" / "9:0-9:11" / "12:0-12:11" / "15:0-15:21"
# 两个方向都要转换

age_group_to_questions <- function(age_group) {
  # norms 格式 → questions 格式
  dplyr::case_when(
    age_group %in% c("5:0-5:5","5:6-5:11","6:0-6:5","6:6-6:11","7:0-7:11","8:0-8:11") ~ "age_5_8",
    age_group %in% c("9:0-9:11","10:0-10:11")                                           ~ "age_9_11",
    age_group %in% c("11:0-11:11","12:0-12:11","13:0-13:11","14:0-14:11")               ~ "age_12_14",
    TRUE                                                                                  ~ "age_15_21"
  )
}

age_group_from_questions <- function(q_age_group) {
  # questions 格式 → norms 格式
  dplyr::case_when(
    q_age_group == "age_5_8"  ~ "5:0-5:5",
    q_age_group == "age_9_11" ~ "9:0-9:11",
    q_age_group == "age_12_14" ~ "12:0-12:11",
    q_age_group == "age_15_21" ~ "15:0-15:21",
    TRUE ~ q_age_group  # fallback
  )
}

# ─────────────────────────────────────────────────────────────
# 3c. 题目信息查询（来自 questions 表）
# ─────────────────────────────────────────────────────────────
get_question_info <- function(subtest, item_number, age_group) {
  con <- get_con()
  on.exit(dbDisconnect(con), add = TRUE)
  q_ag <- age_group_to_questions(age_group)
  sql <- "SELECT question_en, prompt_en, stimulus_en, scoring_key, max_score
          FROM questions WHERE subtest = ? AND age_group = ? AND item_number = ? LIMIT 1"
  q <- dbGetQuery(con, sql, params = list(subtest, q_ag, item_number))
  if (nrow(q) == 0) {
    # fallback: try without age_group filter (e.g. PP has age_group='A')
    sql2 <- "SELECT question_en, prompt_en, stimulus_en, scoring_key, max_score
             FROM questions WHERE subtest = ? AND item_number = ? LIMIT 1"
    q <- dbGetQuery(con, sql2, params = list(subtest, item_number))
    if (nrow(q) == 0) {
      return(tibble(
        question_en = NA_character_,
        prompt_en   = NA_character_,
        stimulus_en = NA_character_,
        scoring_key = NA_character_,
        max_score   = NA_integer_
      ))
    }
  }
  q
}

get_max_item <- function(subtest, age_group) {
  con <- get_con()
  on.exit(dbDisconnect(con), add = TRUE)
  q_ag <- age_group_to_questions(age_group)
  # Try with age_group filter first
  n <- dbGetQuery(con,
    "SELECT COUNT(*) FROM questions WHERE subtest = ? AND age_group = ? AND (question_en IS NOT NULL AND question_en != '')",
    params = list(subtest, q_ag))[[1]]
  if (n > 0) return(as.integer(n))
  # Fallback for PP (age_group='A') or generic subtests
  n2 <- dbGetQuery(con,
    "SELECT COUNT(*) FROM questions WHERE subtest = ? AND (question_en IS NOT NULL AND question_en != '')",
    params = list(subtest))[[1]]
  if (n2 > 0) return(as.integer(n2))
  1L
}

# ─────────────────────────────────────────────────────────────
# 4. 查表函数 — raw → scaled（来自 NORMS_TABLE）
# ─────────────────────────────────────────────────────────────
raw_to_scaled <- function(subtest, raw_score, age_group) {
  ents <- NORMS_TABLE %>%
    filter(.data$test == !!subtest,
           .data$age_group == !!age_group,
           .data$raw_lo <= !!raw_score,
           .data$raw_hi >= !!raw_score)

  if (nrow(ents) > 0) {
    return(ents$scaled_score[1])
  }

  # 边界：raw 超出范围
  all_ent <- NORMS_TABLE %>%
    filter(.data$test == !!subtest, .data$age_group == !!age_group) %>%
    arrange(.data$scaled_score)

  if (nrow(all_ent) == 0) return(NA_integer_)

  max_raw <- max(all_ent$raw_lo)
  min_raw <- min(all_ent$raw_hi)

  if (raw_score >= max_raw) return(tail(all_ent$scaled_score, 1))
  if (raw_score <= min_raw) return(head(all_ent$scaled_score, 1))

  # 线性插值
  lo <- all_ent %>% filter(.data$raw_lo <= !!raw_score) %>% tail(1)
  hi <- all_ent %>% filter(.data$raw_hi >= !!raw_score) %>% head(1)
  if (nrow(lo) == 0 || nrow(hi) == 0) return(NA_integer_)

  lo_ss <- lo$scaled_score[1]; hi_ss <- hi$scaled_score[1]
  lo_raw <- lo$raw_lo[1]; hi_raw <- hi$raw_hi[1]
  if (lo_raw == hi_raw) return(lo_ss)

  as.integer(round(lo_ss + (hi_ss - lo_ss) * (raw_score - lo_raw) / (hi_raw - lo_raw)))
}

# ─────────────────────────────────────────────────────────────
# 5. 计算原始分和量表分
# ─────────────────────────────────────────────────────────────
calculate_raw_scores <- function(responses_df) {
  responses_df %>%
    filter(!is.na(.data$score)) %>%
    group_by(.data$subtest) %>%
    summarise(raw_score = sum(.data$score, na.rm = TRUE), .groups = "drop") %>%
    split(.$subtest) %>%
    purrr::map(~.x$raw_score) %>%
    purrr::set_names(nm = .)
}

calculate_scaled_scores <- function(raw_scores_list, age_group) {
  raw_scores_list %>%
    imap_dfr(function(raw, subtest) {
      scaled <- raw_to_scaled(subtest, raw, age_group)
      tibble(subtest = subtest, raw_score = raw, scaled_score = scaled)
    })
}

# ─────────────────────────────────────────────────────────────
# 6. Composite Scores — CLS, RLI, ELI, LCI, LSI, LMI（Manual Appendix B）
# ─────────────────────────────────────────────────────────────
get_index_composition <- function(composite, age_group) {
  young_ages <- c("5:0-5:5","5:6-5:11","6:0-6:5","6:6-6:11","7:0-7:11","8:0-8:11")
  mid_ages   <- c("9:0-9:11","10:0-10:11","11:0-11:11","12:0-12:11")

  if (composite == "CLS") {
    if (age_group %in% young_ages) c("SC","WS","FS","RS")
    else if (age_group %in% mid_ages) c("FS","RS","USP","SR")
    else c("FS","RS","USP","SR")
  } else if (composite == "RLI") {
    if (age_group %in% young_ages) c("SC","WC","FD")
    else if (age_group %in% mid_ages) c("WC","USP","SR")
    else c("WC","USP","SR")
  } else if (composite == "ELI") {
    if (age_group %in% young_ages) c("WS","FS","RS")
    else if (age_group %in% mid_ages) c("FS","RS","SA")
    else c("FS","RS","SA")
  } else if (composite == "LCI") {
    if (age_group %in% young_ages) c("LC","WC","FD")
    else if (age_group %in% mid_ages) c("WC","USP","WD")
    else c("WC","USP","SA")
  } else if (composite == "LSI") {
    c("SC","WS","FS","RS")
  } else if (composite == "LMI") {
    c("FD","FS","RS")
  } else {
    character(0)
  }
}

get_composite_score <- function(scaled_df, composite, age_group) {
  components <- get_index_composition(composite, age_group)
  sum_ss <- scaled_df %>%
    filter(.data$subtest %in% components) %>%
    pull(.data$scaled_score) %>%
    sum(na.rm = TRUE)

  entry <- COMPOSITE_TABLE %>%
    filter(.data$index_name == !!composite,
           .data$age_group  == !!age_group,
           .data$raw_composite <= !!sum_ss) %>%
    arrange(desc(.data$standard_score)) %>%
    slice(1)

  if (nrow(entry) == 0) {
    entry <- COMPOSITE_TABLE %>%
      filter(.data$index_name == !!composite,
             .data$age_group  == !!age_group) %>%
      arrange(.data$raw_composite) %>%
      slice(1)
  }

  tibble(
    composite     = composite,
    sum_scaled    = sum_ss,
    standard_score = entry$standard_score %||% NA_integer_,
    percentile    = entry$percentile %||% NA_real_
  )
}

# Safe percentile formatter — handles "<0.1" / ">99.9" strings from COMPOSITE_TABLE
fmt_pct <- function(p) {
  if (is.null(p) && length(p) == 1) return("—")
  if (length(p) > 1) {
    # Vectorized path: called from case_when with a tibble column
    vapply(p, fmt_pct, FUN.VALUE = character(1), USE.NAMES = FALSE)
  } else {
    # Scalar path
    if (is.na(p)) return("—")
    suppressWarnings(num <- as.numeric(p))
    if (is.na(num)) {
      if (grepl("<", p, fixed = TRUE)) return("<0.1")
      if (grepl(">", p, fixed = TRUE)) return(">99.9")
      return(p)
    }
    sprintf("%.1f", num)
  }
}

get_confidence_intervals <- function(standard_score, composite, age_group) {
  ci_row <- COMPOSITE_CI_TABLE %>%
    filter(.data$age_group  == !!age_group,
           .data$index_name == !!composite) %>%
    slice(1)

  if (nrow(ci_row) == 0) {
    tibble(
      level    = c("68%","90%","95%"),
      score_lo = c(standard_score - 2L, standard_score - 3L, standard_score - 3L),
      score_hi = c(standard_score + 2L, standard_score + 3L, standard_score + 3L)
    )
  } else {
    ci68 <- pull(ci_row, ci_68)
    ci90 <- pull(ci_row, ci_90)
    ci95 <- pull(ci_row, ci_95)
    tibble(
      level    = c("68%","90%","95%"),
      score_lo = c(standard_score - ci68, standard_score - ci90, standard_score - ci95),
      score_hi = c(standard_score + ci68, standard_score + ci90, standard_score + ci95)
    )
  }
}

calculate_index_scores <- function(scaled_df, age_group) {
  composites <- c("CLS","RLI","ELI","LCI","LSI","LMI")
  imap_dfr(setNames(composites, composites), function(comp, idx) {
    result <- tryCatch(
      get_composite_score(scaled_df, comp, age_group),
      error = function(e) tibble(
        composite = comp, sum_scaled = NA_integer_,
        standard_score = NA_integer_, percentile = NA_real_)
    )
    if (nrow(result) == 0 || is.na(result$standard_score[1])) {
      result <- tibble(
        composite = comp, sum_scaled = NA_integer_,
        standard_score = NA_integer_, percentile = NA_real_)
    }
    result
  })
}

# ─────────────────────────────────────────────────────────────
# 7. Recalling Sentences 评分（Manual Table 3.4）
# ─────────────────────────────────────────────────────────────
score_rs <- function(errors) {
  case_when(
    errors == 0 ~ 3L,
    errors == 1 ~ 2L,
    errors == 2 ~ 1L,
    errors == 3 ~ 1L,
    TRUE ~ 0L
  )
}

# ─────────────────────────────────────────────────────────────
# 8. 数据库操作
# ─────────────────────────────────────────────────────────────
upsert_patient <- function(name, dob, gender = NULL, examiner = NULL, notes = NULL) {
  con <- get_con()
  on.exit(dbDisconnect(con))
  gender <- if (is.null(gender) || gender == "") NA_character_ else gender
  examiner <- if (is.null(examiner) || examiner == "") NA_character_ else examiner
  notes <- if (is.null(notes) || notes == "") NA_character_ else notes
  existing <- dbGetQuery(con,
    "SELECT id FROM patients WHERE name = ? AND dob = ?",
    params = list(name, dob))
  if (nrow(existing) > 0) {
    pid <- existing$id[1]
    dbExecute(con,
      "UPDATE patients SET gender=COALESCE(?,gender),examiner=COALESCE(?,examiner),notes=COALESCE(?,notes) WHERE id=?",
      params = list(gender, examiner, notes, pid))
    pid
  } else {
    dbExecute(con,
      "INSERT INTO patients (name,dob,gender,examiner,notes) VALUES (?,?,?,?,?)",
      params = list(name, dob, gender, examiner, notes))
    as.integer(dbGetQuery(con, "SELECT last_insert_rowid() as id")$id)
  }
}

upsert_assessment <- function(patient_id, assessment_date, age_years,
                              age_months, age_days, age_group) {
  con <- get_con()
  on.exit(dbDisconnect(con))
  existing <- dbGetQuery(con,
    "SELECT id FROM assessments WHERE patient_id=? AND assessment_date=? AND status='in_progress'",
    params = list(patient_id, assessment_date))
  if (nrow(existing) > 0) {
    aid <- existing$id[1]
    dbExecute(con,
      "UPDATE assessments SET age_years=?,age_months=?,age_days=?,age_group=? WHERE id=?",
      params = list(age_years, age_months, age_days, age_group, aid))
    aid
  } else {
    dbExecute(con,
      "INSERT INTO assessments (patient_id,assessment_date,age_years,age_months,age_days,age_group) VALUES (?,?,?,?,?,?)",
      params = list(patient_id, assessment_date, age_years, age_months, age_days, age_group))
    as.integer(dbGetQuery(con, "SELECT last_insert_rowid() as id")$id)
  }
}

save_response <- function(assessment_id, subtest, item_number, response_text = NULL, score) {
  con <- get_con()
  on.exit(dbDisconnect(con))
  dbExecute(con,
    "INSERT OR REPLACE INTO responses (assessment_id,subtest,item_number,response_text,score) VALUES (?,?,?,?,?)",
    params = list(assessment_id, subtest, item_number, response_text, score))
}

save_subtest_scores <- function(assessment_id, scores_df) {
  con <- get_con()
  on.exit(dbDisconnect(con))
  scores_df %>%
    iwalk(function(row, .i) {
      dbExecute(con,
        "INSERT OR REPLACE INTO subtest_scores (assessment_id,subtest,raw_score,scaled_score) VALUES (?,?,?,?)",
        params = list(assessment_id, row$subtest, row$raw_score, row$scaled_score))
    })
}

save_composite_scores <- function(assessment_id, indices_df) {
  con <- get_con()
  on.exit(dbDisconnect(con))
  indices_df %>%
    iwalk(function(row, .i) {
      ci  <- get_confidence_intervals(row$standard_score, row$composite,
                                      indices_df$age_group[1])
      if (nrow(ci) < 3) return()
      dbExecute(con,
        "INSERT OR REPLACE INTO composite_scores
         (assessment_id,composite,sum_scaled,standard_score,percentile_rank,
          confidence_68_lo,confidence_68_hi,confidence_90_lo,confidence_90_hi,
          confidence_95_lo,confidence_95_hi)
         VALUES (?,?,?,?,?,?,?,?,?,?,?)",
        params = list(assessment_id, row$composite, row$sum_scaled,
                      row$standard_score, row$percentile,
                      ci$score_lo[1], ci$score_hi[1],
                      ci$score_lo[2], ci$score_hi[2],
                      ci$score_lo[3], ci$score_hi[3]))
    })
}

# ─────────────────────────────────────────────────────────────
# 9. 缺失的数据库查询函数
# ─────────────────────────────────────────────────────────────
list_assessments <- function(limit = 100L) {
  con <- get_con()
  on.exit(dbDisconnect(con))
  dbGetQuery(con,
    "SELECT a.id, p.name AS patient_name, a.assessment_date,
            a.age_years, a.age_months, a.age_days, a.age_group, a.status
     FROM assessments a
     JOIN patients p ON a.patient_id = p.id
     ORDER BY a.assessment_date DESC
     LIMIT ?",
    params = list(limit))
}

get_assessment_full <- function(assessment_id) {
  con <- get_con()
  on.exit(dbDisconnect(con))

  ass <- dbGetQuery(con,
    "SELECT a.*, p.name AS patient_name, p.dob, p.gender, p.examiner
     FROM assessments a
     JOIN patients p ON a.patient_id = p.id
     WHERE a.id = ?",
    params = list(assessment_id))

  if (nrow(ass) == 0) return(NULL)

  resp <- dbGetQuery(con,
    "SELECT subtest, item_number, response_text, score
     FROM responses WHERE assessment_id = ? ORDER BY subtest, item_number",
    params = list(assessment_id))

  # ── 计算各 subtest 的量表分（从 responses 实时计算）─────────
  ag <- ass$age_group[1]
  if (nrow(resp) > 0 && !is.na(ag)) {
    raw_list <- resp %>%
      dplyr::filter(!is.na(.data$score)) %>%
      dplyr::group_by(.data$subtest) %>%
      dplyr::summarise(raw_score = sum(.data$score, na.rm = TRUE), .groups = "drop") %>%
      purrr::set_names(nm = "subtest", "raw_score") %>%
      { x <- .; split(x$raw_score, x$subtest) }
    ss <- calculate_scaled_scores(raw_list, ag)
  } else {
    ss <- tibble::tibble(subtest = character(), raw_score = integer(), scaled_score = integer())
  }

  cs <- dbGetQuery(con,
    "SELECT composite, sum_scaled, standard_score, percentile_rank,
            confidence_68_lo, confidence_68_hi, confidence_90_lo, confidence_90_hi,
            confidence_95_lo, confidence_95_hi
     FROM composite_scores WHERE assessment_id = ?",
    params = list(assessment_id))

  list(assessment = ass, responses = resp, subtest_scores = ss, composite_scores = cs)
}

start_assessment <- function(patient_id, assessment_date, age_years, age_months, age_days, age_group) {
  upsert_assessment(patient_id, assessment_date, age_years, age_months, age_days, age_group)
}

# ── 删除评估 ───────────────────────────────────────────────
delete_assessment <- function(assessment_id) {
  con <- get_con()
  on.exit(dbDisconnect(con))
  # responses, subtest_scores, composite_scores, ors 都要删
  dbExecute(con, "DELETE FROM responses WHERE assessment_id = ?", params = list(assessment_id))
  dbExecute(con, "DELETE FROM subtest_scores WHERE assessment_id = ?", params = list(assessment_id))
  dbExecute(con, "DELETE FROM composite_scores WHERE assessment_id = ?", params = list(assessment_id))
  dbExecute(con, "DELETE FROM observational_rating_scale WHERE assessment_id = ?", params = list(assessment_id))
  dbExecute(con, "DELETE FROM ors_summary WHERE assessment_id = ?", params = list(assessment_id))
  dbExecute(con, "DELETE FROM assessments WHERE id = ?", params = list(assessment_id))
  invisible()
}

# ─────────────────────────────────────────────────────────────
# ORS: Observational Rating Scale
# ─────────────────────────────────────────────────────────────

ORS_SECTIONS <- list(
  listening = list(
    name_zh = "聆听 Listening",
    name_en = "Listening",
    items = 1:9,
    behaviors_zh = c(
      "注意力不集中。",
      "难以听懂口语指令。",
      "难以记住别人说的话。",
      "难以理解别人的意思。",
      "需要别人重复说过的话。",
      "难以理解词语的含义。",
      "难以理解新想法。",
      "说话或聆听时难以保持眼神接触。",
      "难以理解面部表情、手势或身体语言。"
    )
  ),
  speaking = list(
    name_zh = "表达 Speaking",
    name_en = "Speaking",
    items = 10:28,
    behaviors_zh = c(
      "难以回答别人的问题。",
      "回答问题不如其他同学快。",
      "难以在需要时寻求帮助。",
      "难以提出问题。",
      "说话时难以使用多样化的词汇。",
      "难以找到（想起）合适的词。",
      "难以表达自己的想法。",
      "难以向别人描述事物。",
      "说话时难以保持话题。",
      "说话时难以抓住重点。",
      "讲述事情时难以按正确顺序叙述。",
      "说话时语法不规范。",
      "说话时难以使用完整句子。",
      "说话时句子短而不连贯。",
      "说话时难以扩展答案或提供细节。",
      "难以与人进行对话。",
      "难以在一群人面前说话。",
      "当别人听不懂时难以换一种方式表达。",
      "当别人不理解时会感到沮丧。"
    )
  ),
  reading = list(
    name_zh = "阅读 Reading",
    name_en = "Reading",
    items = 29:34,
    behaviors_zh = c(
      "阅读时难以正确拼读单词。",
      "难以理解所读内容。",
      "难以解释所读内容。",
      "难以找出段落大意。",
      "难以记住细节。",
      "难以按书面指令操作。"
    )
  ),
  writing = list(
    name_zh = "书写 Writing",
    name_en = "Writing",
    items = 35:40,
    behaviors_zh = c(
      "难以写下自己的想法。",
      "书写时语法不规范。",
      "难以写出完整句子。",
      "句子短而不连贯。",
      "书写时难以扩展答案或提供细节。",
      "书写时难以将词语按正确顺序排列。"
    )
  )
)

score_label_zh <- function(score) {
  labels <- c("1" = "从不或几乎从不", "2" = "有时", "3" = "经常", "4" = "总是或几乎总是")
  labels[as.character(score)]
}

save_ors_response <- function(assessment_id, rater_role, section, item_number, score) {
  con <- get_con()
  on.exit(dbDisconnect(con))
  dbExecute(con,
    "INSERT OR REPLACE INTO observational_rating_scale
     (assessment_id, rater_role, section, item_number, score, recorded_at)
     VALUES (?, ?, ?, ?, ?, datetime('now', 'localtime'))",
    params = list(assessment_id, rater_role, section, item_number, score))
  # 更新 summary
  update_ors_summary(assessment_id, rater_role)
  invisible()
}

update_ors_summary <- function(assessment_id, rater_role) {
  con <- get_con()
  on.exit(dbDisconnect(con))

  for (sec in c("listening","speaking","reading","writing")) {
    items <- ORS_SECTIONS[[sec]]$items
    qry <- glue_sql("
      SELECT AVG(score) as avg_score FROM observational_rating_scale
      WHERE assessment_id = ? AND rater_role = ? AND section = ?
      AND item_number IN ({items*})",
      .con = con)
    avg <- dbGetQuery(con, qry, params = list(assessment_id, rater_role, sec))$avg_score[1]

    col_score <- paste0(sec, "_score")
    if (is.na(avg)) next

    dbExecute(con, glue_sql("
      INSERT INTO ors_summary (assessment_id, rater_role, {col_score}, updated_at)
      VALUES (?, ?, ?, datetime('now','localtime'))
      ON CONFLICT(assessment_id, rater_role) DO UPDATE SET
        {col_score} = excluded.{col_score},
        updated_at  = excluded.updated_at",
      .con = con),
      params = list(assessment_id, rater_role, avg))
  }

  # 重新算 total
  total_qry <- glue_sql("
    SELECT SUM(
      COALESCE(listening_score,0) + COALESCE(speaking_score,0) +
      COALESCE(reading_score,0)  + COALESCE(writing_score,0)
    ) as total FROM ors_summary WHERE assessment_id = ? AND rater_role = ?",
    .con = con)
  total <- dbGetQuery(con, total_qry, params = list(assessment_id, rater_role))$total[1]
  dbExecute(con,
    "UPDATE ors_summary SET total_score = ? WHERE assessment_id = ? AND rater_role = ?",
    params = list(total, assessment_id, rater_role))
  invisible()
}

get_ors_summary <- function(assessment_id, rater_role) {
  con <- get_con()
  on.exit(dbDisconnect(con))
  dbGetQuery(con,
    "SELECT * FROM ors_summary WHERE assessment_id = ? AND rater_role = ?",
    params = list(assessment_id, rater_role))
}

get_ors_responses <- function(assessment_id, rater_role) {
  con <- get_con()
  on.exit(dbDisconnect(con))
  dbGetQuery(con,
    "SELECT section, item_number, score FROM observational_rating_scale
     WHERE assessment_id = ? AND rater_role = ?
     ORDER BY section, item_number",
    params = list(assessment_id, rater_role))
}

# ─────────────────────────────────────────────────────────────
# Item Analysis: 基于 Test Objectives MD 的错误分类
# ─────────────────────────────────────────────────────────────

ITEM_ANALYSIS <- list(
  SC = list(  # Sentence Comprehension — 11类
    categories = tibble::tibble(
      category_zh = c("否定句", "修饰语（定语/状语）", "介词短语",
                       "直接/间接宾语", "不定式", "动词短语",
                       "关系从句", "从句（状语/宾语）", "疑问句",
                       "被动语态", "直接/间接请求", "并列句"),
      category_en = c("Negation", "Modification", "Prepositional Phrase",
                       "Direct/Indirect Object", "Infinitive", "Verb Phrase",
                       "Relative Clause", "Subordinate Clause", "Interrogative",
                       "Passive Voice", "Direct/Indirect Request", "Compound"),
      items = list(c(8,9,20), c(1,4,10), c(4,6,14,15,17,18),
                   c(5,15,22), c(5,19), 25,
                   c(2,3,11), c(13,20), 12,
                   c(16,21), c(23,24), c(7,10,26))
    ),
    domain_zh = "句子理解",
    domain_en = "Sentence Comprehension",
    subtest = "SC"
  ),
  LC = list(  # Linguistic Concepts — 6类
    categories = tibble::tibble(
      category_zh = c("包含/排除", "位置概念", "数量", "序列", "条件", "时间"),
      category_en = c("Inclusion/Exclusion", "Location", "Quantity",
                       "Sequence", "Conditional", "Temporal"),
      items = list(c(1,3,4,5,6,7,14,15,19,24,25), c(2,8,10,16,17),
                   c(4,9), c(2,12,13,22), c(11,18,20), c(21,23))
    ),
    domain_zh = "语言概念",
    domain_en = "Linguistic Concepts",
    subtest = "LC"
  ),
  WS = list(  # Word Structure — 11类形态规则
    categories = tibble::tibble(
      category_zh = c("规则复数", "不规则复数", "名词所有格",
                       "第三人称单数", "规则过去式", "不规则过去式",
                       "将来时", "名词派生", "比较级/最高级",
                       "助动词+-ing形式", "代词（宾格/所有格/主格/反身）",
                       "系动词（可缩/不可缩）"),
      category_en = c("Regular Plural", "Irregular Plural", "Possessive Noun",
                       "3rd Person Singular", "Regular Past Tense", "Irregular Past Tense",
                       "Future Tense", "Noun Derivation", "Comparative/Superlative",
                       "Auxiliary + -ing", "Pronouns (Obj/Poss/Subj/Reflex)",
                       "Copula (Contractible/Uncontractible)"),
      items = list(c(1,2), c(3,4), c(7,8), c(5,6), 16, 33,
                   c(20,21), 9, c(22,23,24,25),
                   c(11,12,13,14), c(15,17,18,19,29,30,31,32),
                   c(10,26,27,28))
    ),
    domain_zh = "词汇结构",
    domain_en = "Word Structure",
    subtest = "WS"
  ),
  WC = list(  # Word Classes — 6类语义关系
    categories = tibble::tibble(
      category_zh = c("语义类别（具体）", "位置关系", "组成成分", "同义词",
                       "反义词", "宾语功能", "语义类别（抽象）"),
      category_en = c("Semantic Class", "Location", "Composition",
                       "Synonym", "Word Opposites", "Object Function", "Semantic Class (Abstract)"),
      items = list(c(1,2,3,4,5,6,7,8,9,10,11,12,13,16,19,23,35,38),
                   c(14,15), c(17,18),
                   c(16,25,26,27,28,30,32,34,36,37,39,40),
                   c(24,29,31,33), c(11,22),
                   c(20,21))
    ),
    domain_zh = "词汇语义",
    domain_en = "Word Classes",
    subtest = "WC"
  ),
  FD = list(  # Following Directions — 4级指令 + 修饰词
    categories = tibble::tibble(
      category_zh = c("1级指令", "2级指令", "3级指令", "4级指令",
                       "无修饰词", "一个修饰词", "两个修饰词"),
      category_en = c("1-Level Command", "2-Level Command", "3-Level Command", "4-Level Command",
                       "No Modifier", "One Modifier", "Two Modifiers"),
      items = list(c(1,2,5,11,12), c(3,4,6,7,9,10,13,14,15,25),
                   c(8,16,17,18,20,21,22,24,26,32),
                   c(19,27,28,29,30,31,33),
                   c(6,8,19,23), c(1,2,3,4,5,7,9,10,11,13,14,16,21,22,24,25,29,31),
                   c(12,15,17,18,20,26,27,28,30,32,33))
    ),
    domain_zh = "跟随指令",
    domain_en = "Following Directions",
    subtest = "FD"
  ),
  FS = list(  # Formulated Sentences — 9类句法成分
    categories = tibble::tibble(
      category_zh = c("名词", "代词", "动词", "形容词", "副词",
                       "连接副词", "并列连词", "从属连词", "关联连词"),
      category_en = c("Noun", "Pronoun", "Verb", "Adjective", "Adverb",
                       "Conjunctive Adverb", "Coordinating Conjunction",
                       "Subordinating Conjunction", "Correlative Conjunction"),
      items = list(c(2,3), 1, 7, c(8,9), c(5,6,13,16,24),
                   c(15,18,21,23,24), c(11,20,22), c(10,12,13,14,17,19,20,23),
                   22)
    ),
    domain_zh = "造句",
    domain_en = "Formulated Sentences",
    subtest = "FS"
  ),
  RS = list(  # Recalling Sentences
    categories = tibble::tibble(
      category_zh = c("主动句/被动句", "疑问句", "否定句", "并列结构", "关系/状语从句"),
      category_en = c("Active/Passive", "Interrogative", "Negative",
                       "Coordination", "Relative/Subordinate Clause"),
      items = list(c(1,2,3), c(4,5), c(6,7), c(8,9), c(10,11,12))
    ),
    domain_zh = "句子复述",
    domain_en = "Recalling Sentences",
    subtest = "RS"
  ),
  WD = list(  # Word Definitions — 4类主题
    categories = tibble::tibble(
      category_zh = c("科学", "社会科学", "语言/文学/艺术", "体验性知识"),
      category_en = c("Science", "Social Studies", "Language/Literature/Arts", "Experiential"),
      items = list(c(4,15,16,18), c(7,8,9,10,13,14),
                   c(5,11,12,17,19,20,21), c(1,2,3,6))
    ),
    domain_zh = "词汇定义",
    domain_en = "Word Definitions",
    subtest = "WD"
  ),
  SA = list(  # Sentence Assembly
    categories = tibble::tibble(
      category_zh = c("主动/被动语态", "否定句", "疑问句", "从属/关系从句"),
      category_en = c("Active/Passive Voice", "Negative", "Interrogative",
                       "Subordinate/Relative Clause"),
      items = list(c(1,2,3), c(4,5), c(6,7,8), c(9,10,11))
    ),
    domain_zh = "句子重组",
    domain_en = "Sentence Assembly",
    subtest = "SA"
  ),
  SR = list(  # Semantic Relationships
    categories = tibble::tibble(
      category_zh = c("比较关系", "空间关系", "时间关系", "序列关系", "被动语态"),
      category_en = c("Comparison", "Spatial Relations", "Temporal Relations",
                       "Serial Order", "Passive Voice"),
      items = list(c(1,2,3,4), c(5,6,7), c(8,9,10), c(11,12,13,14), c(15,16))
    ),
    domain_zh = "语义关系",
    domain_en = "Semantic Relationships",
    subtest = "SR"
  )
)

# 诊断报告生成函数：输入 subtest + responses tibble，输出分类错误分析
generate_item_analysis <- function(subtest, resp_df, ag) {
  ia <- ITEM_ANALYSIS[[subtest]]
  if (is.null(ia)) return(NULL)

  categories <- ia$categories
  # resp_df: tibble with item_number, score

  # 对每个 category，找出答错的 items
  error_items <- purrr::map(categories$items, function(item_nums) {
    item_nums <- as.integer(item_nums)
    scored <- resp_df %>% dplyr::filter(.data$item_number %in% item_nums, !is.na(.data$score))
    if (nrow(scored) == 0) return(character(0))
    err <- scored %>% dplyr::filter(.data$score == 0) %>% dplyr::pull(.data$item_number)
    as.character(err)
  })

  # 计算每个 category 的正确率
  cat_performance <- purrr::map_dfr(seq_len(nrow(categories)), function(i) {
    item_nums <- as.integer(categories$items[[i]])
    scored <- resp_df %>% dplyr::filter(.data$item_number %in% item_nums, !is.na(.data$score))
    n_total <- nrow(scored)
    if (n_total == 0) {
      pct <- NA_real_
    } else {
      n_correct <- scored %>% dplyr::filter(.data$score > 0) %>% nrow()
      pct <- n_correct / n_total * 100
    }
    tibble::tibble(
      category_zh = categories$category_zh[[i]],
      category_en = categories$category_en[[i]],
      n_items = length(item_nums),
      n_scored = n_total,
      n_correct = scored %>% dplyr::filter(.data$score > 0) %>% nrow(),
      accuracy_pct = pct,
      error_items = paste(error_items[[i]], collapse = ", "),
      flag = if (!is.na(pct) && pct < 60) "⚠️ 需要重点干预"
             else if (!is.na(pct) && pct < 80) "🔶 可进一步提升"
             else if (!is.na(pct)) "✅ 掌握良好"
             else "—"
    )
  })

  list(
    subtest = subtest,
    domain_zh = ia$domain_zh,
    domain_en = ia$domain_en,
    performance = cat_performance
  )
}

# 简洁诊断建议生成
generate_diagnosis_recommendations <- function(subtest, resp_df, scaled_score, ag) {
  ia <- generate_item_analysis(subtest, resp_df, ag)
  if (is.null(ia)) return(list(overall_zh = "暂无详细分析数据。", overall_en = "No detailed analysis data."))

  perf <- ia$performance
  flag_issues <- perf %>% dplyr::filter(stringr::str_starts(flag, "⚠️"))

  if (nrow(flag_issues) == 0) {
    overall_zh <- sprintf("综合语言评估结果显示，孩子在 %s 方面的各项技能表现正常，未发现显著薄弱环节。",
                          ia$domain_zh)
    overall_en <- sprintf("Overall, performance in %s is within normal limits. No significant areas of weakness identified.",
                          ia$domain_en)
  } else {
    weak_zh <- paste0(sprintf("%s（%s准确率%.0f%%）",
                              flag_issues$category_zh,
                              flag_issues$category_en,
                              flag_issues$accuracy_pct),
                      collapse = "、")
    overall_zh <- sprintf("孩子在 %s 方面的以下技能需要重点干预：%s。建议通过针对性练习提升相关能力。",
                          ia$domain_zh, weak_zh)
    overall_en <- sprintf("Significant weaknesses identified in %s: %s. Targeted intervention recommended.",
                          ia$domain_en,
                          paste0(flag_issues$category_en, collapse = ", "))
  }

  list(overall_zh = overall_zh, overall_en = overall_en, ia = ia)
}
