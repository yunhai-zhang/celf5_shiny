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
  case_when(
    total_months <= 71  ~ "5:0-5:5",
    total_months <= 77  ~ "5:6-5:11",
    total_months <= 83  ~ "6:0-6:5",
    total_months <= 89  ~ "6:6-6:11",
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
  subtest = c("SC","LC","WS","WC","FD","FS","RS","WD","SA","SR","RC","SW","USP"),
  full_name = c("Sentence Comprehension","Linguistic Concepts","Word Structure",
                "Word Classes","Formulated Definitions","Formulated Sentences",
                "Recalling Sentences","Word Definitions","Sentence Assembly",
                "Semantic Relationships","Reading Comprehension","Spelling and Writing",
                "Understanding Spoken Paragraphs"),
  max_items = c(42L,36L,34L,40L,32L,48L,48L,21L,20L,20L,28L,44L,18L),
  discontinue_rule = c(4L,4L,4L,4L,4L,4L,4L,4L,4L,4L,0L,0L,0L)
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
    errors <= 3 ~ 1L,
    TRUE ~ 0L
  )
}

# ─────────────────────────────────────────────────────────────
# 8. 数据库操作
# ─────────────────────────────────────────────────────────────
upsert_patient <- function(name, dob, gender = NULL, examiner = NULL, notes = NULL) {
  con <- get_con()
  on.exit(dbDisconnect(con))
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

save_response <- function(assessment_id, subtest, item_number, score) {
  con <- get_con()
  on.exit(dbDisconnect(con))
  dbExecute(con,
    "INSERT OR REPLACE INTO responses (assessment_id,subtest,item_number,score) VALUES (?,?,?,?)",
    params = list(assessment_id, subtest, item_number, score))
}

save_subtest_scores <- function(assessment_id, scores_df) {
  con <- get_con()
  on.exit(dbDisconnect(con))
  for (i in seq_len(nrow(scores_df))) {
    row <- scores_df[i,]
    dbExecute(con,
      "INSERT OR REPLACE INTO subtest_scores (assessment_id,subtest,raw_score,scaled_score) VALUES (?,?,?,?)",
      params = list(assessment_id, row$subtest, row$raw_score, row$scaled_score))
  }
}

save_composite_scores <- function(assessment_id, indices_df) {
  con <- get_con()
  on.exit(dbDisconnect(con))
  for (i in seq_len(nrow(indices_df))) {
    row <- indices_df[i,]
    ci  <- get_confidence_intervals(row$standard_score, row$composite,
                                    indices_df$age_group[1])
    if (nrow(ci) < 3) next
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
  }
}
