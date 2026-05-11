# global.R — CELF-5 Assessment App (tidyverse rebuild + SQLite norms)
# 常范数据从 celf5_norms.db 加载（来自 CELF-5 Examiner's Manual Appendix A/B）

library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)
library(stringr)
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
# 0c. SW topic names (display labels)
# ─────────────────────────────────────────────────────────────
# Returns a named character vector: item_number → display label
get_sw_topic_label <- function(item_number, age_group_db) {
  labels <- list(
    age_8 = c("1" = "Trial: Catching the Bus", "2" = "Field Trip", "3" = "Stuffing the Backpack"),
    age_9_10 = c("1" = "Trial: Catching the Bus", "2" = "Class Schedules", "3" = "Morning Announcements"),
    age_11_12 = c("1" = "Trial: Catching the Bus", "2" = "Summer Break", "3" = "Elsa's Project"),
    age_13_21 = c("1" = "Trial: Catching the Bus", "2" = "School Play", "3" = "Mystery on Route 9")
  )
  l <- labels[[age_group_db]]
  if (is.null(l)) l <- c("1" = "Trial: Catching the Bus", "2" = "Task 2", "3" = "Task 3")
  as.character(l[as.character(item_number)])
}

# Returns data frame of SW topics available for a norms-format age_group
# Columns: item_number, topic_label, question_en, age_group_db
get_sw_topics <- function(age_group_norms) {
  rubric_key <- switch(age_group_norms,
    "5:0-5:5"   = "age_8", "5:6-5:11" = "age_8",
    "6:0-6:5"   = "age_8", "6:6-6:11" = "age_8",
    "7:0-7:11"  = "age_8", "8:0-8:11" = "age_8",
    "9:0-9:11"  = "age_9_10", "10:0-10:11" = "age_9_10",
    "11:0-11:11" = "age_11_12", "12:0-12:11" = "age_11_12",
    "13:0-13:11" = "age_13_21", "14:0-14:11" = "age_13_21",
    "15:0-15:11" = "age_13_21", "16:0-16:11" = "age_13_21",
    "17:0-21:11" = "age_13_21",
    "age_8"  # fallback
  )
  con <- get_con()
  on.exit(dbDisconnect(con))
  topics <- dbGetQuery(con,
    "SELECT item_number, question_en, age_group AS age_group_db
     FROM questions WHERE subtest = 'SW' AND age_group = ?
     ORDER BY item_number",
    params = list(rubric_key))
  if (nrow(topics) == 0) return(tibble(item_number=integer(), topic_label=character(),
                                        question_en=character(), age_group_db=character()))
  topics$topic_label <- get_sw_topic_label(topics$item_number, rubric_key)
  topics
}

# ─────────────────────────────────────────────────────────────
# 0d. SW 多维评分 Rubric（按 age_group 分维度打分）
# ─────────────────────────────────────────────────────────────
# 每个 entry: list(max_struct=结构满分, struct_scale=结构选项向量,
#                  grammar_scale=语规格式, org_scale=组织格式, mech_scale=机械格式)
# structure_complete: 1=句子完整, 0=句子不完整
# grammar: 按 age_group 最高3/2/1分
# organization: 按 age_group 最高3/4/5分
# mechanics: 按 age_group 最高3/2/1/0分（实际0也是选项）
SW_SCORING_RUBRIC <- list(
  age_8 = list(
    struct_scale = c("1（完整）" = 1L, "0（不完整）" = 0L),
    grammar_scale = c("3分" = 3L, "2分" = 2L, "1分" = 1L, "0分" = 0L),
    org_scale = c("3分" = 3L, "2分" = 2L, "1分" = 1L, "0分" = 0L),
    mech_scale = c("3分" = 3L, "2分" = 2L, "1分" = 1L, "0分" = 0L),
    sentence_items = 2L    # 2个句子（S1 + S+）
  ),
  age_9_10 = list(
    struct_scale = c("1分（完整）" = 1L, "0分（不完整）" = 0L),
    grammar_scale = c("3分" = 3L, "2分" = 2L, "1分" = 1L, "0分" = 0L),
    org_scale = c("3分" = 3L, "2分" = 2L, "0分" = 0L),
    mech_scale = c("3分" = 3L, "2分" = 2L, "1分" = 1L, "0分" = 0L),
    sentence_items = 3L    # 3个句子（S1 + S2 + S+）
  ),
  age_11_12 = list(
    struct_scale = c("1分（完整）" = 1L, "0分（不完整）" = 0L),
    grammar_scale = c("3分" = 3L, "2分" = 2L, "1分" = 1L, "0分" = 0L),
    org_scale = c("4分" = 4L, "3分" = 3L, "0分" = 0L),
    mech_scale = c("3分" = 3L, "2分" = 2L, "1分" = 1L, "0分" = 0L),
    sentence_items = 3L
  ),
  age_13_21 = list(
    struct_scale = c("1分（完整）" = 1L, "0分（不完整）" = 0L),
    grammar_scale = c("1分" = 1L, "0分" = 0L),
    org_scale = c("5分" = 5L, "4分" = 4L, "0分" = 0L),
    mech_scale = c("3分" = 3L, "2分" = 2L, "1分" = 1L, "0分" = 0L),
    sentence_items = 5L    # S1+S2+S3+S4+S+
  )
)

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
    "5:0-5:5"   = c("SC","LC","WS","WC","FD","FS","RS","USP"),
    "5:6-5:11"  = c("SC","LC","WS","WC","FD","FS","RS","USP"),
    "6:0-6:5"   = c("SC","LC","WS","WC","FD","FS","RS","USP"),
    "6:6-6:11"  = c("SC","LC","WS","WC","FD","FS","RS","USP"),
    "7:0-7:11"  = c("SC","LC","WS","WC","FD","FS","RS","USP"),
    "8:0-8:11"  = c("SC","LC","WS","WC","FD","FS","RS","USP"),
    "9:0-9:11"  = c("WC","FD","FS","RS","USP","WD","SA","SR","RC","SW"),
    "10:0-10:11"= c("WC","FD","FS","RS","USP","WD","SA","SR","RC","SW"),
    "11:0-11:11"= c("WC","FD","FS","RS","USP","WD","SA","SR","RC","SW"),
    "12:0-12:11"= c("WC","FD","FS","RS","USP","WD","SA","SR","RC","SW"),
    "13:0-13:11"= c("WC","FD","FS","RS","USP","WD","SA","SR","RC","SW"),
    "14:0-14:11"= c("WC","FD","FS","RS","USP","WD","SA","SR","RC","SW"),
    "15:0-15:11"= c("WC","FD","FS","RS","USP","WD","SA","SR","RC","SW"),
    "16:0-16:11"= c("WC","FD","FS","RS","USP","WD","SA","SR","RC","SW"),
    "17:0-21:11"= c("WC","FD","FS","RS","USP","WD","SA","SR","RC","SW")
  )
  result <- compositions[[age_group]]
  if (is.null(result)) character(0) else result
}

# ─────────────────────────────────────────────────────────────
# 3. SUBTEST_DEFS + start_points（Manual Chapter 3）
# ─────────────────────────────────────────────────────────────
SUBTEST_DEFS <- tibble(
  subtest = c("SC","LC","WS","WC","FD","FS","RS","WD","SA","SR","RC","SW","USP"),
  full_name = c("Sentence Comprehension","Linguistic Concepts","Word Structure",
                "Word Classes","Following Directions","Formulated Sentences",
                "Recalling Sentences","Word Definitions","Sentence Assembly",
                "Semantic Relationships","Reading Comprehension","Structured Writing",
                "Understanding Spoken Paragraphs"),
  max_items = c(42L,36L,34L,40L,32L,48L,48L,21L,20L,20L,28L,44L,18L),
  discontinue_rule = c(4L,4L,4L,4L,4L,4L,4L,4L,4L,4L,0L,0L,0L)
)

# 2026-05-07 校正：所有 start point 均来自 Manual 原文
# 注意：WC/FS/WD/SA/SC/RS 有 start point；SR/RC/LC/WS/FD 无 start point（全部从 Item 1）
# trial/demo items 不入库（不记分），start point 是施测起点，入库题目都是正式题
SUBTEST_START_POINTS <- list(
  # Word Classes: MD lines 144-147
  # Ages 9-10 → Item 1; Ages 11-14 → Item 13; Ages 15-21 → Item 20 (MD: Word Classes)
  WC  = setNames(list(1L,1L,1L,1L,1L,1L,1L,1L,13L,13L,13L,13L,20L,20L,20L),
                 c("5:0-5:5","5:6-5:11","6:0-6:5","6:6-6:11","7:0-7:11","8:0-8:11",
                   "9:0-9:11","10:0-10:11","11:0-11:11","12:0-12:11","13:0-13:11",
                   "14:0-14:11","15:0-15:11","16:0-16:11","17:0-21:11")),

  # Following Directions: MD — Ages 5-8→Item1, Ages 9-11→Item 6, Ages 12-14→Item 10, Ages 15-21→Item 14
  FD  = setNames(list(1L,1L,1L,1L,1L,1L,6L,6L,6L,10L,10L,10L,14L,14L,14L),
                 c("5:0-5:5","5:6-5:11","6:0-6:5","6:6-6:11","7:0-7:11","8:0-8:11",
                   "9:0-9:11","10:0-10:11","11:0-11:11","12:0-12:11","13:0-13:11",
                   "14:0-14:11","15:0-15:11","16:0-16:11","17:0-21:11")),

  # Formulated Sentences: MD line 272
  # Ages 9-11 → Item 8; Ages 12-14 → Item 10; Ages 15-21 → Item 13
  FS  = setNames(list(1L,1L,1L,1L,1L,1L,8L,8L,8L,8L,10L,10L,10L,13L,13L),
                 c("5:0-5:5","5:6-5:11","6:0-6:5","6:6-6:11","7:0-7:11","8:0-8:11",
                   "9:0-9:11","10:0-10:11","11:0-11:11","12:0-12:11","13:0-13:11",
                   "14:0-14:11","15:0-15:11","16:0-16:11","17:0-21:11")),

  # Recalling Sentences: MD lines 334-336
  # Trial items 不入库；正式施测全部从 Item 1 开始（无 start point rule）
  RS  = setNames(list(1L,1L,1L,1L,1L,1L,1L,1L,1L,1L,1L,1L,1L,1L,1L),
                 c("5:0-5:5","5:6-5:11","6:0-6:5","6:6-6:11","7:0-7:11","8:0-8:11",
                   "9:0-9:11","10:0-10:11","11:0-11:11","12:0-12:11","13:0-13:11",
                   "14:0-14:11","15:0-15:11","16:0-16:11","17:0-21:11")),

  # Word Definitions: MD line 421
  # Ages 9-16 → Item 1; Ages 17-21 → Item 3
  WD  = setNames(list(1L,1L,1L,1L,1L,1L,1L,1L,1L,1L,1L,1L,1L,1L,3L),
                 c("5:0-5:5","5:6-5:11","6:0-6:5","6:6-6:11","7:0-7:11","8:0-8:11",
                   "9:0-9:11","10:0-10:11","11:0-11:11","12:0-12:11","13:0-13:11",
                   "14:0-14:11","15:0-15:11","16:0-16:11","17:0-21:11")),

  # Sentence Assembly: MD line 465
  # Ages 9-11 → Item 1; Ages 12-21 → Item 4
  SA  = setNames(list(1L,1L,1L,1L,1L,1L,1L,1L,1L,1L,4L,4L,4L,4L,4L),
                 c("5:0-5:5","5:6-5:11","6:0-6:5","6:6-6:11","7:0-7:11","8:0-8:11",
                   "9:0-9:11","10:0-10:11","11:0-11:11","12:0-12:11","13:0-13:11",
                   "14:0-14:11","15:0-15:11","16:0-16:11","17:0-21:11")),

  # Semantic Relationships: MD lines 503-509
  # 无 start point；Trial items 不入库；正式施测从 Item 1 开始
  SR  = setNames(list(1L,1L,1L,1L,1L,1L,1L,1L,1L,1L,1L,1L,1L,1L,1L),
                 c("5:0-5:5","5:6-5:11","6:0-6:5","6:6-6:11","7:0-7:11","8:0-8:11",
                   "9:0-9:11","10:0-10:11","11:0-11:11","12:0-12:11","13:0-13:11",
                   "14:0-14:11","15:0-15:11","16:0-16:11","17:0-21:11")),

  # Sentence Comprehension: MD age 5-8 Record Form line 171
  # Ages 5-6 → Item 1; Ages 7-8 → Item 10
  SC  = setNames(list(1L,1L,1L,10L,10L,10L), c("5:0-5:5","5:6-5:11","6:0-6:5","6:6-6:11","7:0-7:11","8:0-8:11")),

  # Following Directions age 5-8: all start at Item 1
  FD5 = setNames(list(1L,1L,1L,1L,1L,1L), c("5:0-5:5","5:6-5:11","6:0-6:5","6:6-6:11","7:0-7:11","8:0-8:11")),

  # Linguistic Concepts: all start at Item 1
  LC  = setNames(list(1L,1L,1L,1L,1L,1L), c("5:0-5:5","5:6-5:11","6:0-6:5","6:6-6:11","7:0-7:11","8:0-8:11")),

  # Word Structure: all start at Item 1
  WS  = setNames(list(1L,1L,1L,1L,1L,1L), c("5:0-5:5","5:6-5:11","6:0-6:5","6:6-6:11","7:0-7:11","8:0-8:11")),

  # Reading Comprehension: all start at Item 1
  RC  = setNames(list(1L,1L,1L,1L,1L,1L,1L,1L,1L,1L,1L,1L,1L,1L,1L),
                 c("5:0-5:5","5:6-5:11","6:0-6:5","6:6-6:11","7:0-7:11","8:0-8:11",
                   "9:0-9:11","10:0-10:11","11:0-11:11","12:0-12:11","13:0-13:11",
                   "14:0-14:11","15:0-15:11","16:0-16:11","17:0-21:11")),

  # Structured Writing: all start at Item 1 (Trial Task is separate demo, not in DB)
  SW  = setNames(list(1L,1L,1L,1L,1L,1L,1L,1L,1L,1L,1L,1L,1L,1L,1L),
                 c("5:0-5:5","5:6-5:11","6:0-6:5","6:6-6:11","7:0-7:11","8:0-8:11",
                   "9:0-9:11","10:0-10:11","11:0-11:11","12:0-12:11","13:0-13:11",
                   "14:0-14:11","15:0-15:11","16:0-16:11","17:0-21:11")),

  # Understanding Spoken Paragraphs: all start at Item 1 (Trial Paragraph is separate)
  USP = setNames(list(1L,1L,1L,1L,1L,1L,1L,1L,1L,1L,1L,1L,1L,1L,1L),
                 c("5:0-5:5","5:6-5:11","6:0-6:5","6:6-6:11","7:0-7:11","8:0-8:11",
                   "9:0-9:11","10:0-10:11","11:0-11:11","12:0-12:11","13:0-13:11",
                   "14:0-14:11","15:0-15:11","16:0-16:11","17:0-21:11"))
)

get_start_point <- function(subtest, age_group) {
  sp <- SUBTEST_START_POINTS[[subtest]]
  if (is.null(sp)) return(1L)
  if (is.null(sp[[age_group]])) 1L else sp[[age_group]]
}

get_discontinue_rule <- function(subtest) {
  SUBTEST_DEFS %>% filter(subtest == !!subtest) %>%
    pull(discontinue_rule) %>% magrittr::extract2(1)
}

# ─────────────────────────────────────────────────────────────
# max_score_for_subtest — 各 subtest 最高分（reversal 检测用）
# ─────────────────────────────────────────────────────────────
max_score_for_subtest <- function(subtest) {
  con <- get_con()
  on.exit(dbDisconnect(con), add = TRUE)
  # 先按 age_group 查，最大题目数覆盖所有 age_group
  sql <- "SELECT MAX(max_score) FROM questions WHERE subtest = ? LIMIT 1"
  q <- dbGetQuery(con, sql, params = list(subtest))
  if (nrow(q) > 0 && !is.na(q$`MAX(max_score)`)) {
    return(as.integer(q$`MAX(max_score)`[1]))
  }
  1L  # fallback
}

# ─────────────────────────────────────────────────────────────
# 3b. age_group 格式转换（questions 表 vs norms 表）
# ─────────────────────────────────────────────────────────────
# questions 表用 "age_5_8" / "age_9_11" / "age_12_14" / "age_15_21"
# norms 表用 "5:0-5:5" / "9:0-9:11" / "12:0-12:11" / "15:0-15:21"
# 两个方向都要转换

age_group_to_questions <- function(age_group, subtest = NULL) {
  # WC uses a different banding: age_9_21 covers 11:0-21:11 (items 13-40)
  # Other subtests use: age_5_8 (5:0-8:11), age_9_11 (9:0-10:11), age_12_14 (11:0-14:11), age_15_21 (15:0+)
  if (!is.null(subtest) && subtest == "WC") {
    dplyr::case_when(
      age_group %in% c("5:0-5:5","5:6-5:11","6:0-6:5","6:6-6:11","7:0-7:11","8:0-8:11") ~ "age_5_8",
      age_group %in% c("9:0-9:11","10:0-10:11")                                           ~ "age_9_11",
      age_group %in% c("11:0-11:11","12:0-12:11","13:0-13:11","14:0-14:11",
                       "15:0-15:11","16:0-16:11","17:0-17:11","18:0-18:11",
                       "19:0-19:11","20:0-20:11","21:0-21:11")                             ~ "age_9_21",
      TRUE                                                                                  ~ "age_9_21"
    )
  } else {
    dplyr::case_when(
      age_group %in% c("5:0-5:5","5:6-5:11","6:0-6:5","6:6-6:11","7:0-7:11","8:0-8:11") ~ "age_5_8",
      age_group %in% c("9:0-9:11","10:0-10:11")                                           ~ "age_9_11",
      age_group %in% c("11:0-11:11","12:0-12:11","13:0-13:11","14:0-14:11")               ~ "age_12_14",
      TRUE                                                                                  ~ "age_15_21"
    )
  }
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
# norms 格式 (e.g. "5:0-5:5", "6:0-6:5", "9:0-9:11") → USP paragraphs 字母 (A/B/C/D/E/F)
# CELF-5 Manual Table 1.2 / USP test record form age groupings:
#   A = 5:0–8:11  (age_5_8)
#   B = 9:0–10:11 (age_9_11)
#   C = 11:0–14:11 (age_12_14)
#   D = 15:0–17:11 (age_15_21 lower)
#   E = 18:0–19:11 (age_15_21 upper)
#   F = 20:0–21:11 (age_15_21 highest)
# ─────────────────────────────────────────────────────────────
age_group_to_usp_db <- function(age_group) {
  dplyr::case_when(
    age_group %in% c("5:0-5:5","5:6-5:11",
                     "6:0-6:5","6:6-6:11",
                     "7:0-7:11","8:0-8:11") ~ "A",
    age_group %in% c("9:0-9:11","10:0-10:11") ~ "B",
    age_group %in% c("11:0-11:11","12:0-12:11",
                     "13:0-13:11","14:0-14:11") ~ "C",
    age_group %in% c("15:0-15:11","16:0-16:11","17:0-17:11") ~ "D",
    age_group %in% c("18:0-18:11","19:0-19:11") ~ "E",
    TRUE ~ "F"  # 20:0–21:11
  )
}

# ─────────────────────────────────────────────────────────────
# 3c. 题目信息查询（来自 questions 表）
# ─────────────────────────────────────────────────────────────
get_question_info <- function(subtest, item_number, age_group) {
  con <- get_con()
  on.exit(dbDisconnect(con), add = TRUE)

  # ── USP items 8+: fully dynamic from usp_paragraphs ────────────────────────
  if (subtest == "USP" && as.integer(item_number) > 7) {
    para_info <- get_usp_paragraph_for_item(age_group, as.integer(item_number))
    if (!is.null(para_info) && nrow(para_info) > 0) {
      qs <- jsonlite::fromJSON(para_info$questions_json[1], simplifyVector = FALSE)
      # slot = item_number - 7 gives the cumulative question position across all paragraphs
      # e.g. Trial items 8-14 → slot 1-7; Paragraph A items 15-18 → slot 8-11
      # But qs[[]] is indexed within THIS paragraph, so find the relative position
      slot <- as.integer(item_number) - 7   # cumulative slot: item 8 → 1, item 15 → 8
      # Paragraph boundaries (cumulative): Trial=7, Trial+A=11, Trial+A+B=17, ...
      # We need the position within THIS paragraph's qs array
      # The paragraph returned covers items (start_item) to (start_item + len(qs) - 1)
      # For item 15 → paragraph A (qs has 4 items), relative position is 15 - 14 = 1 (first Q of A)
      # For item 19 → paragraph B (qs has 6 items), relative position is 19 - 18 = 1 (first Q of B)
      # Since get_usp_paragraph_for_item already found which paragraph this item falls in,
      # we know the item is somewhere within qs.  The relative slot = slot - (sum of Qs in earlier paras)
      # We can compute earlier paragraphs' total Q count from the cumulative boundary
      # stored in get_usp_paragraph_for_item's loop.  Simpler: just iterate qs and use item_number directly.
      # ── RECALCULATE from scratch: what is item_number's position within this paragraph? ──
      usp_ag <- age_group_to_usp_db(age_group)
      all_paras <- dbGetQuery(con,
        "SELECT id, questions_json FROM usp_paragraphs WHERE age_group = ? ORDER BY id",
        params = list(usp_ag))
      cum <- 7
      rel_slot <- NA_integer_
      for (pi in seq_len(nrow(all_paras))) {
        pqs <- jsonlite::fromJSON(all_paras$questions_json[pi], simplifyVector = FALSE)
        n   <- length(pqs)
        if (as.integer(item_number) >= cum + 1 && as.integer(item_number) <= cum + n) {
          rel_slot <- as.integer(item_number) - cum   # 1-indexed position within this paragraph
          break
        }
        cum <- cum + n
      }
      if (is.na(rel_slot) || rel_slot < 1 || rel_slot > length(qs)) {
        return(tibble(
          question_en = NA_character_, prompt_en = NA_character_,
          stimulus_en = NA_character_,  scoring_key = NA_character_,
          max_score = NA_integer_,     paragraph_en = NA_character_,
          questions_json = NA_character_
        ))
      }
      q_text  <- qs[[rel_slot]]$q
      q_ans   <- qs[[rel_slot]]$a
      mx <- if (grepl("two|Three|two things", q_text, ignore.case = TRUE)) 2L else 1L
      return(tibble(
        question_en    = q_text,
        prompt_en      = NA_character_,
        stimulus_en    = NA_character_,
        scoring_key    = q_ans,
        max_score      = mx,
        paragraph_en   = para_info$paragraph_en[1],
        questions_json = para_info$questions_json[1]
      ))
    }
    return(tibble(
      question_en = NA_character_, prompt_en = NA_character_,
      stimulus_en = NA_character_,  scoring_key = NA_character_,
      max_score = NA_integer_,     paragraph_en = NA_character_,
      questions_json = NA_character_
    ))
  }

  # ── Non-USP, or USP items 1-7 ──────────────────────────────────────────────
  q_ag <- if (subtest == "USP") age_group else age_group_to_questions(age_group, subtest)
  sql  <- "SELECT question_en, prompt_en, stimulus_en, scoring_key, max_score
           FROM questions WHERE subtest = ? AND age_group = ? AND item_number = ? LIMIT 1"
  q    <- dbGetQuery(con, sql, params = list(subtest, q_ag, item_number))

  if (nrow(q) == 0) {
    sql2 <- "SELECT question_en, prompt_en, stimulus_en, scoring_key, max_score
             FROM questions WHERE subtest = ? AND item_number = ? LIMIT 1"
    q    <- dbGetQuery(con, sql2, params = list(subtest, item_number))
    if (nrow(q) == 0) {
      return(tibble(
        question_en = NA_character_, prompt_en   = NA_character_,
        stimulus_en = NA_character_, scoring_key = NA_character_,
        max_score   = NA_integer_
      ))
    }
  }

  # ── USP items 1-7: attach paragraph info ──────────────────────────────────
  if (subtest == "USP") {
    para_info <- get_usp_paragraph_for_item(age_group, as.integer(item_number))
    if (!is.null(para_info) && nrow(para_info) > 0) {
      q$paragraph_en   <- para_info$paragraph_en[1]
      q$questions_json <- para_info$questions_json[1]
    } else {
      q$paragraph_en   <- NA_character_
      q$questions_json <- NA_character_
    }
  }
  q
}

# ── Helper: map USP item_number → paragraph row ──────────────────────────
get_usp_paragraph_for_item <- function(age_group, item_number) {
  con <- get_con()
  on.exit(dbDisconnect(con), add = TRUE)
  # Items 1-7 are option cards (no paragraph data needed)
  if (item_number <= 7) return(data.frame())
  # Convert norms-format age_group (e.g. "7:0-7:11") to USP DB letter (e.g. "A")
  usp_ag <- age_group_to_usp_db(age_group)
  # Get all paragraphs for this age_group in order, compute cumulative Q counts
  paras <- dbGetQuery(con,
    "SELECT paragraph_id, paragraph_en, questions_json
     FROM usp_paragraphs WHERE age_group = ? ORDER BY id",
    params = list(usp_ag))
  if (nrow(paras) == 0) return(data.frame())
  # Cumulative count: first paragraph covers items 8 to (8 + n_1 - 1)
  cum <- 7  # items 1-7 are option cards
  for (i in seq_len(nrow(paras))) {
    qs <- jsonlite::fromJSON(paras$questions_json[i], simplifyVector = FALSE)
    n_q <- length(qs)
    if (item_number <= cum + n_q) {
      return(paras[i, c("paragraph_en", "questions_json"), drop = FALSE])
    }
    cum <- cum + n_q
  }
  data.frame()  # item_number beyond all paragraphs
}

get_max_item <- function(subtest, age_group) {
  con <- get_con()
  on.exit(dbDisconnect(con), add = TRUE)
  q_ag <- age_group_to_questions(age_group, subtest)
  # Try with age_group filter first
  n <- dbGetQuery(con,
    "SELECT COUNT(*) FROM questions WHERE subtest = ? AND age_group = ? AND (question_en IS NOT NULL AND question_en != '')",
    params = list(subtest, q_ag))[[1]]
  if (n > 0) return(as.integer(n))
  # Fallback for generic subtests, or USP (age_group='A')
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
# 计算 SW 多维原始分（汇总所有维度和句子）
# responses_df: 含 structure_complete/grammar/organization/mechanics 列
# age_group: age_8 | age_9_10 | age_11_12 | age_13_21
# ─────────────────────────────────────────────────────────────
calculate_sw_raw_score <- function(responses_df, age_group) {
  sw_responses <- responses_df %>% filter(subtest == "SW", !is.na(score))
  if (nrow(sw_responses) == 0) return(NA_integer_)

  rubric <- SW_SCORING_RUBRIC[[age_group]]
  if (is.null(rubric)) return(NA_integer_)

  # Structure: 每句子满分1分，累加
  struct_score <- sum(sw_responses$structure_complete, na.rm = TRUE)

  # Grammar: 累加所有句子
  grammar_score <- sum(sw_responses$grammar, na.rm = TRUE)

  # Organization: 每篇作文1个组织分
  org_score <- sum(sw_responses$organization, na.rm = TRUE)

  # Mechanics: 每篇作文1个机械分
  mech_score <- sum(sw_responses$mechanics, na.rm = TRUE)

  as.integer(struct_score + grammar_score + org_score + mech_score)
}

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
    standard_score = as.integer(entry$standard_score[1]),
    percentile    = as.character(entry$percentile[1])  # always character; NA→"NA"
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
        standard_score = NA_integer_, percentile = as.character(NA))
    )
    if (nrow(result) == 0 || is.na(result$standard_score[1])) {
      result <- tibble(
        composite = comp, sum_scaled = NA_integer_,
        standard_score = NA_integer_, percentile = as.character(NA))
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

save_response <- function(assessment_id, subtest, item_number, response_text = NULL, score,
                          structure_complete = NULL, grammar = NULL,
                          organization = NULL, mechanics = NULL) {
  con <- get_con()
  on.exit(dbDisconnect(con))
  # NULL → NA_integer_ (dbExecute 不接受 NULL 参数)
  sc <- if (is.null(structure_complete)) NA_integer_ else structure_complete
  gr <- if (is.null(grammar)) NA_integer_ else grammar
  og <- if (is.null(organization)) NA_integer_ else organization
  mc <- if (is.null(mechanics)) NA_integer_ else mechanics
  dbExecute(con,
    "INSERT OR REPLACE INTO responses
     (assessment_id,subtest,item_number,response_text,score,
      structure_complete,grammar,organization,mechanics)
     VALUES (?,?,?,?,?,?,?,?,?)",
    params = list(assessment_id, subtest, item_number, response_text, score,
                  sc, gr, og, mc))
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
update_assessment_status <- function(assessment_id, status) {
  con <- get_con()
  on.exit(dbDisconnect(con))
  dbExecute(con,
    "UPDATE assessments SET status = ? WHERE id = ?",
    params = list(status, assessment_id))
}

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
    "SELECT subtest, item_number, response_text, score,
            structure_complete, grammar, organization, mechanics
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
  # responses, subtest_scores, composite_scores 都要删
  dbExecute(con, "DELETE FROM responses WHERE assessment_id = ?", params = list(assessment_id))
  dbExecute(con, "DELETE FROM subtest_scores WHERE assessment_id = ?", params = list(assessment_id))
  dbExecute(con, "DELETE FROM composite_scores WHERE assessment_id = ?", params = list(assessment_id))
  dbExecute(con, "DELETE FROM assessments WHERE id = ?", params = list(assessment_id))
  invisible()
}

# ─────────────────────────────────────────────────────────────
# ─────────────────────────────────────────────────────────────
# Item Analysis: 基于 Test Objectives 的分类诊断
# ─────────────────────────────────────────────────────────────

ITEM_ANALYSIS <- list(
  SC = list(
    domain_zh = "句子理解",
    domain_en = "Sentence Comprehension",
    categories = list(
      list(cat_zh = "否定结构",     cat_en = "Negation",            items = c(8, 9, 20)),
      list(cat_zh = "修饰语",       cat_en = "Modification",         items = c(1, 4, 10)),
      list(cat_zh = "介词短语",     cat_en = "Prepositional Phrase", items = c(4, 6, 14, 15, 17, 18)),
      list(cat_zh = "直接/间接宾语",cat_en = "Direct/Indirect Object",items = c(5, 15, 22)),
      list(cat_zh = "不定式",       cat_en = "Infinitive",           items = c(5, 19)),
      list(cat_zh = "动词短语",     cat_en = "Verb Phrase",          items = c(25)),
      list(cat_zh = "关系从句",     cat_en = "Relative Clause",      items = c(2, 3, 11)),
      list(cat_zh = "从句",         cat_en = "Subordinate Clause",    items = c(13, 20)),
      list(cat_zh = "疑问句",       cat_en = "Interrogative",         items = c(12)),
      list(cat_zh = "被动语态",     cat_en = "Passive Voice",         items = c(16, 21)),
      list(cat_zh = "直接/间接请求",cat_en = "Direct/Indirect Request",items = c(23, 24)),
      list(cat_zh = "并列句",       cat_en = "Compound",              items = c(7, 10, 26))
    ),
    intervention = list(
      zh = "针对语义/形态/句法结构错误进行分析；靶向提升接受性词汇和明确句子结构意识。",
      en = "Analyze errors by semantic/morphologic/syntactic structures; target receptive vocabulary and explicit sentence structure awareness."
    )
  ),
  LC = list(
    domain_zh = "语言概念",
    domain_en = "Linguistic Concepts",
    categories = list(
      list(cat_zh = "包含/排除",  cat_en = "Inclusion/Exclusion", items = c(1, 3, 4, 5, 6, 7, 14, 15, 19, 24, 25)),
      list(cat_zh = "位置",        cat_en = "Location",             items = c(2, 8, 10, 16, 17)),
      list(cat_zh = "数量",        cat_en = "Quantity",             items = c(4, 9)),
      list(cat_zh = "序列",        cat_en = "Sequence",            items = c(2, 12, 13, 22)),
      list(cat_zh = "条件",        cat_en = "Conditional",        items = c(11, 18, 20)),
      list(cat_zh = "时间",        cat_en = "Temporal",            items = c(21, 23))
    ),
    intervention = list(
      zh = "用课堂操作材料和时间/序列活动重点训练时间概念和位置概念。",
      en = "Target temporal/location concepts with classroom manipulables and sequential activities."
    )
  ),
  WS = list(
    domain_zh = "词汇结构",
    domain_en = "Word Structure",
    categories = list(
      list(cat_zh = "规则复数",         cat_en = "Regular Plural",              items = c(1, 2)),
      list(cat_zh = "不规则复数",       cat_en = "Irregular Plural",             items = c(3, 4)),
      list(cat_zh = "名词所有格",       cat_en = "Possessive Noun",              items = c(7, 8)),
      list(cat_zh = "第三人称单数",     cat_en = "Third Person Singular",        items = c(5, 6)),
      list(cat_zh = "规则过去式",       cat_en = "Regular Past Tense",           items = c(16)),
      list(cat_zh = "不规则过去式",     cat_en = "Irregular Past Tense",         items = c(33)),
      list(cat_zh = "将来时",           cat_en = "Future Tense",                 items = c(20, 21)),
      list(cat_zh = "名词派生",         cat_en = "Noun Derivation",              items = c(9)),
      list(cat_zh = "比较级/最高级",    cat_en = "Comparative/Superlative",     items = c(22, 23, 24, 25)),
      list(cat_zh = "助动词+-ing",      cat_en = "Auxiliary + -ing",             items = c(11, 12, 13, 14)),
      list(cat_zh = "代词",             cat_en = "Pronouns",                    items = c(15, 17, 18, 19, 29, 30, 31, 32)),
      list(cat_zh = "系动词",           cat_en = "Copula",                       items = c(10, 26, 27, 28))
    ),
    intervention = list(
      zh = "通过模仿、图片替换和故事讲述，针对性训练特定形态规则。",
      en = "Target specific morphological rules via imitation, picture substitution, and storytelling."
    )
  ),
  WC = list(
    domain_zh = "词汇语义",
    domain_en = "Word Classes",
    categories = list(
      list(cat_zh = "语义类别",     cat_en = "Semantic Class",       items = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 16, 19, 20, 21, 23, 35, 38)),
      list(cat_zh = "位置关系",     cat_en = "Location",              items = c(14, 15)),
      list(cat_zh = "组成成分",     cat_en = "Composition",           items = c(17, 18)),
      list(cat_zh = "同义词",       cat_en = "Synonym",               items = c(16, 25, 26, 27, 28, 30, 32, 34, 36, 37, 39, 40)),
      list(cat_zh = "反义词",       cat_en = "Word Opposites",        items = c(24, 29, 31, 33)),
      list(cat_zh = "物体功能",     cat_en = "Object Function",       items = c(11, 22))
    ),
    intervention = list(
      zh = "靶向提升语义联想技能和元语言意识。",
      en = "Target semantic association skills and metalinguistic awareness."
    )
  ),
  FD = list(
    domain_zh = "跟随指令",
    domain_en = "Following Directions",
    categories = list(
      list(cat_zh = "1级指令", cat_en = "1-Level Commands", items = c(1, 2, 5, 11, 12)),
      list(cat_zh = "2级指令", cat_en = "2-Level Commands", items = c(3, 4, 6, 7, 9, 10, 13, 14, 15, 25)),
      list(cat_zh = "3级指令", cat_en = "3-Level Commands", items = c(8, 16, 17, 18, 20, 21, 22, 24, 26, 32)),
      list(cat_zh = "4级指令", cat_en = "4-Level Commands", items = c(19, 27, 28, 29, 30, 31, 33)),
      list(cat_zh = "无修饰语", cat_en = "No Modifiers",    items = c(6, 8, 19, 23)),
      list(cat_zh = "一个修饰语", cat_en = "One Modifier",  items = c(1, 2, 3, 4, 5, 7, 9, 10, 11, 13, 14, 16, 21, 22, 24, 25, 29, 31)),
      list(cat_zh = "两个修饰语", cat_en = "Two Modifiers", items = c(12, 15, 17, 18, 20, 26, 27, 28, 30, 32, 33))
    ),
    intervention = list(
      zh = "简化指令、增加冗余度，并教授方向术语。",
      en = "Simplify instructions, add redundancy, and teach orientation terms."
    )
  ),
  FS = list(
    domain_zh = "造句",
    domain_en = "Formulated Sentences",
    categories = list(
      list(cat_zh = "名词",             cat_en = "Noun",                     items = c(2, 3)),
      list(cat_zh = "代词",             cat_en = "Pronoun",                   items = c(1)),
      list(cat_zh = "动词",             cat_en = "Verb",                      items = c(7)),
      list(cat_zh = "形容词",           cat_en = "Adjective",                 items = c(8, 9)),
      list(cat_zh = "副词",             cat_en = "Adverb",                    items = c(5, 6, 13, 16, 24)),
      list(cat_zh = "连接副词",         cat_en = "Conjunctive Adverb",        items = c(15, 18, 21, 23, 24)),
      list(cat_zh = "并列连词",         cat_en = "Coordinating Conjunction",  items = c(11, 20, 22)),
      list(cat_zh = "从属连词",         cat_en = "Subordinating Conjunction", items = c(10, 12, 13, 14, 17, 19, 20, 23)),
      list(cat_zh = "关联连词",         cat_en = "Correlative Conjunction",   items = c(22))
    ),
    intervention = list(
      zh = "靶向提升语法标记和句法整合能力。",
      en = "Target grammatical markers and syntactic integration."
    )
  ),
  RS = list(
    domain_zh = "句子复述",
    domain_en = "Recalling Sentences",
    categories = list(
      list(cat_zh = "主动/被动语态", cat_en = "Active/Passive", items = c()),
      list(cat_zh = "疑问句",         cat_en = "Interrogative", items = c()),
      list(cat_zh = "否定句",         cat_en = "Negative",       items = c()),
      list(cat_zh = "并列结构",       cat_en = "Coordination",   items = c()),
      list(cat_zh = "从句结构",       cat_en = "Clause",         items = c())
    ),
    intervention = list(
      zh = "重点训练复杂从句（从属/关系从句）和句子长度。",
      en = "Target complex clauses (subordinate/relative) and sentence length."
    )
  ),
  WD = list(
    domain_zh = "词汇定义",
    domain_en = "Word Definitions",
    categories = list(
      list(cat_zh = "科学类", cat_en = "Science",        items = c(4, 15, 16, 18)),
      list(cat_zh = "社会科学", cat_en = "Social Studies", items = c(7, 8, 9, 10, 13, 14)),
      list(cat_zh = "语言文学艺术", cat_en = "Language/Literature/Arts", items = c(5, 11, 12, 17, 19, 20, 21)),
      list(cat_zh = "体验性", cat_en = "Experiential",   items = c(1, 2, 3, 6))
    ),
    intervention = list(
      zh = "针对词汇语义特征和概念理解进行训练。",
      en = "Target vocabulary depth through semantic feature analysis."
    )
  ),
  SA = list(
    domain_zh = "句子重组",
    domain_en = "Sentence Assembly",
    categories = list(
      list(cat_zh = "主动/被动语态", cat_en = "Active/Passive",     items = c()),
      list(cat_zh = "否定结构",     cat_en = "Negative",            items = c()),
      list(cat_zh = "疑问句",       cat_en = "Interrogative",        items = c()),
      list(cat_zh = "从句结构",     cat_en = "Subordinate/Relative",items = c())
    ),
    intervention = list(
      zh = "提升句子变化性和写作修订能力。",
      en = "Target sentence variation and writing revision."
    )
  ),
  SR = list(
    domain_zh = "语义关系",
    domain_en = "Semantic Relationships",
    categories = list(
      list(cat_zh = "比较关系",     cat_en = "Comparison",     items = c()),
      list(cat_zh = "空间关系",    cat_en = "Spatial",        items = c()),
      list(cat_zh = "时间关系",    cat_en = "Temporal",       items = c()),
      list(cat_zh = "序列关系",    cat_en = "Serial Order",   items = c()),
      list(cat_zh = "被动语态",    cat_en = "Passive Voice",  items = c())
    ),
    intervention = list(
      zh = "支持指令、序列和逻辑理解的训练。",
      en = "Support directions, sequences, and logical understanding."
    )
  )
)

generate_item_analysis <- function(subtest, responses_df, age_group) {
  ia_def <- ITEM_ANALYSIS[[subtest]]
  if (is.null(ia_def)) return(NULL)

  if (nrow(responses_df) == 0) return(NULL)

  # 只取有 score 的题目
  scored <- responses_df %>% filter(!is.na(score))
  if (nrow(scored) == 0) return(NULL)

  perf_rows <- lapply(ia_def$categories, function(cat) {
    cat_items   <- cat$items
    cat_responses <- scored %>% filter(.data$item_number %in% cat_items)
    n_items   <- length(cat_items)
    n_scored  <- nrow(cat_responses)
    n_correct <- sum(cat_responses$score > 0, na.rm = TRUE)

    if (n_scored == 0) {
      accuracy_pct <- NA_real_
      flag <- "⚠️ 未评分"
      err_items <- ""
    } else {
      accuracy_pct <- n_correct / n_scored * 100
      err_vec <- cat_responses$item_number[cat_responses$score == 0]
      err_items <- paste(sort(err_vec), collapse = ", ")
      flag <- if (accuracy_pct < 60) "⚠️ 重点干预"
              else if (accuracy_pct < 80) "🔶 提升空间"
              else "✅ 掌握良好"
    }

    data.frame(
      category_zh   = cat$cat_zh,
      category_en   = cat$cat_en,
      n_items       = n_items,
      n_scored      = n_scored,
      n_correct     = n_correct,
      accuracy_pct  = accuracy_pct,
      error_items   = err_items,
      flag          = flag,
      stringsAsFactors = FALSE
    )
  })

  perf <- bind_rows(perf_rows)
  list(domain_zh = ia_def$domain_zh,
       domain_en = ia_def$domain_en,
       performance = perf,
       intervention_zh = ia_def$intervention$zh,
       intervention_en = ia_def$intervention$en)
}

item_analysis_panel <- function(subtest, raw_score) {
  ia <- ITEM_ANALYSIS[[subtest]]
  if (is.null(ia)) return(NULL)

  tagList(
    hr(),
    h5(strong("📊 题目分析 Item Analysis — ", ia$domain_zh, " / ", ia$domain_en)),
    p(strong("⚠️ <60% = 重点干预", class = "text-danger"), " | ",
      strong("🔶 60-80% = 提升空间", class = "text-warning"), " | ",
      strong("✅ 80%+ = 掌握良好", class = "text-success"),
      class = "small"),
    p(strong("干预建议: "), ia$intervention$zh,
      class = "small text-muted", style = "font-style: italic;")
  )
}


# ─────────────────────────────────────────────────────────────
# Appendix C: Age Equivalent lookup (subtest → raw_score → age string)
# Source: CELF-5 Manual Appendix C
# Keys are quoted strings to avoid R parse ambiguity
# ─────────────────────────────────────────────────────────────
age_equiv <- list(
  SC = c(
    "1" = "3:0",   "2" = "3:1",   "3" = "3:2",   "4" = "3:4",   "5" = "3:5",
    "6" = "3:7",   "7" = "3:8",   "8" = "3:10",  "9" = "4:0",   "10" = "4:2",
    "11" = "4:4",  "12" = "4:6",  "13" = "4:8",  "14" = "4:10", "15" = "5:0",
    "16" = "5:3",  "17" = "5:5",  "18" = "5:7",  "19" = "5:9",  "20" = "6:0",
    "21" = "6:3",  "22" = "6:6",  "23" = "7:0",  "24" = "8:2",  "25" = "9:2",
    "26" = "9:8",  "27" = "10:1", "28" = "10:4", "29" = "10:10","30" = "11:7",
    "31" = "12:7", "32" = "13:7", "33" = "14:7", "34" = "15:7", "35" = "17:7",
    "36" = "20:1", "37" = ">21:5","38" = ">21:5","39" = ">21:5","40" = ">21:5"
  ),
  LC = c(
    "9" = "3:1",   "10" = "3:4",  "11" = "3:7",  "12" = "3:9",  "13" = "4:0",
    "14" = "4:3", "15" = "4:5",  "16" = "4:7",  "17" = "4:10", "18" = "5:1",
    "19" = "5:3", "20" = "9:0",  "21" = "9:6",  "22" = "9:11", "23" = "10:7",
    "24" = "11:4","25" = "12:4", "26" = "13:4", "27" = "14:7", "28" = "15:7",
    "29" = "17:7","30" = "20:1", "31" = ">21:5","32" = ">21:5","33" = ">21:5"
  ),
  WS = c(
    "8" = "3:0",   "9" = "3:1",   "10" = "3:3",  "11" = "3:5",  "12" = "3:7",
    "13" = "3:9", "14" = "3:11", "15" = "4:1",  "16" = "4:3",  "17" = "4:5",
    "18" = "4:7", "19" = "4:10", "20" = "5:1",  "21" = "5:3",  "22" = "5:6",
    "23" = "5:10","24" = "6:3",  "25" = "6:6",  "26" = "6:11", "27" = "7:4",
    "28" = "7:10","29" = "8:7",  "30" = ">8:11","31" = ">8:11","32" = ">8:11",
    "33" = "9:3", "34" = "9:7",  "35" = "9:10", "36" = "10:1", "37" = "10:7",
    "38" = "11:1","39" = "11:7", "40" = "12:1", "41" = "12:7", "42" = "13:7",
    "43" = "15:1","44" = "18:7", "45" = ">21:5","46" = ">21:5","47" = ">21:5","48" = ">21:5"
  ),
  WC = c(
    "1" = "3:2",  "2" = "3:4",  "3" = "3:6",  "4" = "3:8",  "5" = "3:10",
    "6" = "4:0", "7" = "4:2",  "8" = "4:5",  "9" = "4:8",  "10" = "4:11",
    "11" = "5:3","12" = "5:5", "13" = "5:8", "14" = "5:11","15" = "6:1",
    "16" = "6:4","17" = "6:7", "18" = "6:11","19" = "7:2", "20" = "7:5",
    "21" = "7:10","22" = "8:2", "23" = "8:6", "24" = "8:10","25" = "9:0",
    "26" = "9:3","27" = "9:5", "28" = "9:8", "29" = "9:10","30" = "10:1",
    "31" = "10:4","32" = "10:7","33" = "10:10","34" = "11:1","35" = "11:4",
    "36" = "11:10","37" = "12:1","38" = "12:7","39" = "12:10","40" = "13:1",
    "41" = "13:4","42" = "13:10","43" = "14:4","44" = "14:10","45" = "15:7",
    "46" = "17:1","47" = "18:7","48" = "20:1","49" = ">21:5","50" = ">21:5",
    "51" = ">21:5","52" = ">21:5","53" = ">21:5","54" = ">21:5","55" = ">21:5",
    "56" = ">21:5","57" = ">21:5","58" = ">21:5","59" = ">21:5","60" = ">21:5",
    "61" = ">21:5","62" = ">21:5","63" = ">21:5","64" = ">21:5","65" = ">21:5",
    "66" = ">21:5","67" = ">21:5","68" = ">21:5","69" = ">21:5","70" = ">21:5",
    "71" = ">21:5","72" = ">21:5","73" = ">21:5","74" = ">21:5","75" = ">21:5",
    "76" = ">21:5","77" = ">21:5","78" = ">21:5"
  ),
  FD = c(
    "1" = "3:3",  "2" = "3:6",  "3" = "3:9",  "4" = "4:0",  "5" = "4:3",
    "6" = "9:8", "7" = "10:7", "8" = "11:4", "9" = "12:4", "10" = "13:4",
    "11" = "14:4","12" = "15:7","13" = "17:7","14" = "19:10","15" = ">21:5",
    "16" = ">21:5","17" = ">21:5","18" = ">21:5","19" = ">21:5","20" = ">21:5","21" = ">21:5"
  ),
  FS = c(
    "1" = "4:5",  "2" = "4:6",  "3" = "4:7",  "4" = "4:8",  "5" = "4:9",
    "6" = "4:10", "7" = "4:11", "8" = "9:0",  "9" = "9:7",  "10" = "10:1",
    "11" = "10:10","12" = "11:7","13" = "12:7","14" = "13:7","15" = "14:7",
    "16" = "15:7","17" = "17:4","18" = "19:10","19" = ">21:5","20" = ">21:5",
    "21" = "6:8", "22" = "6:9", "23" = "6:11","24" = "7:2", "25" = "7:4",
    "26" = "7:6", "27" = "7:8", "28" = "7:11","29" = "8:2", "30" = "8:5",
    "31" = "8:8", "32" = "8:11"
  ),
  RS = c(
    "1" = "3:1",  "2" = "3:2",  "3" = "3:3",  "4" = "3:4",  "5" = "3:5",
    "6" = "3:7",  "7" = "3:8",  "8" = "3:9",  "9" = "9:1",  "10" = "9:8",
    "11" = "10:4","12" = "11:4","13" = "12:4","14" = "13:10","15" = "15:4",
    "16" = "17:4","17" = "20:1","18" = ">21:5","19" = ">21:5","20" = ">21:5",
    "21" = "5:3", "22" = "5:4", "23" = "5:6", "24" = "5:7", "25" = "5:8",
    "26" = "5:10","27" = "6:0", "28" = "6:2", "29" = "6:3", "30" = "6:5",
    "31" = "6:7", "32" = "6:8", "33" = "6:10","34" = "7:0", "35" = "7:2",
    "36" = "7:4", "37" = "7:6", "38" = "7:8", "39" = "7:10","40" = "8:0",
    "41" = "8:2", "42" = "8:5", "43" = "8:7", "44" = "8:10"
  ),
  WD = c(
    "1" = "5:7",  "2" = "6:5",  "3" = "7:3",  "4" = "8:1",  "5" = "8:11",
    "6" = "9:8",  "7" = "10:7", "8" = "11:4", "9" = "12:4", "10" = "13:4",
    "11" = "14:4","12" = "15:7","13" = "17:7","14" = "19:10","15" = ">21:5",
    "16" = ">21:5","17" = ">21:5","18" = ">21:5","19" = ">21:5","20" = ">21:5","21" = ">21:5"
  ),
  SA = c(
    "1" = "5:6",  "2" = "5:11", "3" = "6:4",  "4" = "6:10", "5" = "7:3",
    "6" = "7:10", "7" = "8:5"
  ),
  SR = c(
    "1" = "5:2",  "2" = "5:7",  "3" = "6:0",  "4" = "6:5",  "5" = "6:10",
    "6" = "7:4",  "7" = "7:10", "8" = "8:5"
  )
)

# ─────────────────────────────────────────────────────────────
# Appendix G: Growth Scale Value lookup (subtest → raw_score → GSV)
# Source: CELF-5 Manual Appendix G
# ─────────────────────────────────────────────────────────────
gsv <- list(
  SC = c(
    "1" = 402, "2" = 423, "3" = 436, "4" = 446, "5" = 454,
    "6" = 462, "7" = 468, "8" = 474, "9" = 480, "10" = 486,
    "11" = 491, "12" = 496, "13" = 501, "14" = 506, "15" = 511,
    "16" = 517, "17" = 522, "18" = 527, "19" = 533, "20" = 539,
    "21" = 545, "22" = 553, "23" = 562, "24" = 574, "25" = 593, "26" = 611
  ),
  LC = c(
    "1" = 382, "2" = 403, "3" = 418, "4" = 430, "5" = 440,
    "6" = 449, "7" = 458, "8" = 467, "9" = 475, "10" = 483,
    "11" = 491, "12" = 498, "13" = 506, "14" = 513, "15" = 520,
    "16" = 528, "17" = 535, "18" = 542, "19" = 550, "20" = 558, "21" = 568
  ),
  WS = c(
    "1" = 390, "2" = 410, "3" = 423, "4" = 433, "5" = 441,
    "6" = 448, "7" = 454, "8" = 460, "9" = 465, "10" = 470,
    "11" = 475, "12" = 480, "13" = 484, "14" = 488, "15" = 493,
    "16" = 497, "17" = 501, "18" = 505, "19" = 510, "20" = 514, "21" = 519
  ),
  WC = c(
    "1" = 306, "2" = 327, "3" = 341, "4" = 353, "5" = 364,
    "6" = 374, "7" = 383, "8" = 393, "9" = 402, "10" = 412,
    "11" = 421, "12" = 431, "13" = 441, "14" = 451, "15" = 461,
    "16" = 470, "17" = 479, "18" = 488, "19" = 496, "20" = 504, "21" = 511
  ),
  FD = c(
    "1" = 302, "2" = 336, "3" = 363, "4" = 383, "5" = 399,
    "6" = 413, "7" = 425, "8" = 436, "9" = 446, "10" = 455,
    "11" = 465, "12" = 473, "13" = 482, "14" = 491, "15" = 499,
    "16" = 507, "17" = 515, "18" = 522, "19" = 530, "20" = 537, "21" = 544
  ),
  FS = c(
    "1" = 388,  "2" = 405,  "3" = 417,  "4" = 425,  "5" = 433,
    "6" = 439,  "7" = 445,  "8" = 449,  "9" = 454,  "10" = 458,
    "11" = 462, "12" = 466, "13" = 469, "14" = 473, "15" = 476,
    "16" = 479, "17" = 482, "18" = 486, "19" = 489, "20" = 492,
    "21" = 495, "22" = 498, "23" = 501, "24" = 504, "25" = 507,
    "26" = 510, "27" = 513, "28" = 516, "29" = 519, "30" = 521,
    "31" = 524, "32" = 527, "33" = 529, "34" = 532, "35" = 535,
    "36" = 538, "37" = 541, "38" = 543, "39" = 546, "40" = 550,
    "41" = 553, "42" = 557, "43" = 561, "44" = 566, "45" = 571,
    "46" = 579, "47" = 591, "48" = 604
  ),
  RS = c(
    "1" = 331,  "2" = 348,  "3" = 359,  "4" = 368,  "5" = 375,
    "6" = 382,  "7" = 388,  "8" = 394,  "9" = 400,  "10" = 405,
    "11" = 411, "12" = 416, "13" = 420, "14" = 425, "15" = 430,
    "16" = 434, "17" = 439, "18" = 443, "19" = 447, "20" = 451,
    "21" = 455, "22" = 459, "23" = 463, "24" = 467, "25" = 470,
    "26" = 474, "27" = 477, "28" = 480, "29" = 484, "30" = 486,
    "31" = 489, "32" = 492, "33" = 495, "34" = 497, "35" = 500,
    "36" = 502, "37" = 505, "38" = 507, "39" = 509, "40" = 512,
    "41" = 514, "42" = 516, "43" = 518, "44" = 521, "45" = 523,
    "46" = 525, "47" = 527, "48" = 530, "49" = 532, "50" = 534,
    "51" = 536, "52" = 539, "53" = 541, "54" = 543, "55" = 545,
    "56" = 548, "57" = 550, "58" = 552, "59" = 555, "60" = 557,
    "61" = 559, "62" = 562, "63" = 565, "64" = 567, "65" = 570,
    "66" = 573, "67" = 576, "68" = 579, "69" = 583, "70" = 587,
    "71" = 591, "72" = 596, "73" = 602, "74" = 610, "75" = 618,
    "76" = 629, "77" = 646, "78" = 662
  ),
  WD = c(
    "1" = 332, "2" = 364, "3" = 397, "4" = 425, "5" = 444,
    "6" = 459, "7" = 472, "8" = 483, "9" = 495, "10" = 506,
    "11" = 516, "12" = 527, "13" = 537, "14" = 547, "15" = 557,
    "16" = 567, "17" = 578, "18" = 589, "19" = 604, "20" = 625, "21" = 644
  ),
  SA = c(
    "1" = 346, "2" = 379, "3" = 405, "4" = 429, "5" = 450,
    "6" = 467, "7" = 481, "8" = 493, "9" = 503, "10" = 512,
    "11" = 521, "12" = 529, "13" = 538, "14" = 547, "15" = 557,
    "16" = 568, "17" = 580, "18" = 596, "19" = 620, "20" = 640
  ),
  SR = c(
    "1" = 403, "2" = 427, "3" = 443, "4" = 455, "5" = 464,
    "6" = 473, "7" = 480, "8" = 488, "9" = 495, "10" = 501,
    "11" = 508, "12" = 515, "13" = 522, "14" = 530, "15" = 538,
    "16" = 547, "17" = 557, "18" = 570, "19" = 590, "20" = 609
  )
)

# ─────────────────────────────────────────────────────────────
# Helper: look up Age Equivalent and GSV for a subtest raw score
# Returns NA if not available
# ─────────────────────────────────────────────────────────────
get_age_equiv <- function(subtest, raw_score, age_group = NULL) {
  if (is.null(raw_score) || is.na(raw_score) || raw_score < 0) return(NA_character_)
  ae_tbl <- age_equiv[[subtest]]
  if (is.null(ae_tbl)) return(NA_character_)
  coalesce(as.character(ae_tbl[as.character(raw_score)]), NA_character_)
}

get_gsv <- function(subtest, raw_score, age_group = NULL) {
  if (is.null(raw_score) || is.na(raw_score) || raw_score < 0) return(NA_real_)
  gsv_tbl <- gsv[[subtest]]
  if (is.null(gsv_tbl)) return(NA_real_)
  coalesce(as.numeric(gsv_tbl[as.character(raw_score)]), NA_real_)
}

# ─────────────────────────────────────────────────────────────
# 9. AI 临床评估报告生成（MiniMax M2.7）
# ─────────────────────────────────────────────────────────────
.read_minimax_key <- function() {
  key <- Sys.getenv("MINIMAX_CN_API_KEY")
  if (nzchar(key)) return(key)
  # Fallback: read from .env file directly
  env_lines <- readLines(file.path(Sys.getenv("HOME"), ".hermes", ".env"), warn = FALSE)
  matched <- grep("^MINIMAX_CN_API_KEY=", env_lines, value = TRUE)
  if (length(matched) == 0) return("")
  sub("^MINIMAX_CN_API_KEY=", "", matched[1])
}


# ─────────────────────────────────────────────────────────────────
# .clean_narrative_tags — 清除 MiniMax 返回中的思考标签和 prompt 残留
# 统一 .call_minimax / btn_gen_narrative / btn_regen_narrative 三处重复逻辑
# ─────────────────────────────────────────────────────────────────
.clean_narrative_tags <- function(raw_text) {
  tk_patterns <- list(
    "\u3010\u77e5\u9053[\\s\\S]*?\u3010\u60f3\u77e5\u9053",   # 中文【知道...想知道】
    "\u3010\u60f3\u77e5\u9053[\\s\\S]*?\u3010\u77e5\u9053",
    "<thought>[\\s\\S]*?</thought>",                              # English think tags
    "< THOUGHT >[\\s\\S]*?</THOUGHT>",
    "<think>[\\s\\S]*?</planning>",
    "<think>[\\s\\S]*?</thinking>",
    "\\[think\\][\\s\\S]*?\\[/think\\]"
  )
  cleaned <- raw_text
  for (pat in tk_patterns) {
    cleaned <- stringr::str_remove_all(cleaned, pat)
  }
  # 清理 prompt 指令残留行
  cleaned <- stringr::str_remove(cleaned,
    "^[\\s]*?(You are a clinical|Generate a professional|This report|Writing in)[\\s\\S]*?$")
  cleaned <- stringr::str_remove(cleaned,
    "^[\\s]*?(Write in Chinese|Write in English|Please generate)[\\s\\S]*?$")
  stringr::str_trim(cleaned)
}

.call_minimax <- function(prompt, max_tokens = 800L) {
  api_key  <- .read_minimax_key()
  url      <- "https://api.minimaxi.com/v1/chat/completions"
  body     <- list(
    model    = "MiniMax-M2.7",
    messages = list(list(role = "user", content = prompt)),
    max_tokens = max_tokens,
    temperature = 0.7
  )
  resp     <- httr::POST(
    url,
    httr::add_headers(`Content-Type` = "application/json", `Authorization` = paste0("Bearer ", api_key)),
    body     = body,
    httr::content_type_json()
  )
  txt      <- content(resp, as = "text", encoding = "UTF-8")
  if (resp$status_code != 200) {
    stop(sprintf("MiniMax API error %d: %s", resp$status_code, txt))
  }
  parsed   <- fromJSON(txt, simplifyVector = FALSE)
  .clean_narrative_tags(parsed$choices[[1]]$message$content)
}

# ─────────────────────────────────────────────────────────────
# generate_clinical_narrative — 主函数
# 输入: assessment_id (integer)
# 输出: 临床评估报告文字 (character)
# ─────────────────────────────────────────────────────────────
generate_clinical_narrative <- function(assessment_id) {
  con <- get_con()
  on.exit(dbDisconnect(con))

  # ── 基本信息 ───────────────────────────────────────────────
  ass <- dbGetQuery(con, sprintf(
    "SELECT *, strftime('%%Y-%%m-%%d', assessment_date) as date_str
     FROM assessments WHERE id = %d", assessment_id
  ))
  pat <- dbGetQuery(con, sprintf(
    "SELECT * FROM patients WHERE id = %d", ass$patient_id[1]
  ))
  ag  <- ass$age_group

  # ── 评分数据 ───────────────────────────────────────────────
  resp <- dbGetQuery(con, sprintf(
    "SELECT subtest, item_number, score FROM responses
     WHERE assessment_id = %d AND score IS NOT NULL
     ORDER BY subtest, item_number",
    assessment_id
  ))

  raw_scores <- resp %>%
    group_by(subtest) %>%
    summarise(
      raw      = sum(score, na.rm = TRUE),
      n_items  = n(),
      max_item = max(item_number, na.rm = TRUE),
      .groups  = "drop"
    )

  sdf <- calculate_scaled_scores(
    setNames(as.list(raw_scores$raw), raw_scores$subtest),
    ag
  )

  # ── 强项/弱项 ─────────────────────────────────────────────
  available <- sdf$scaled_score
  names(available) <- sdf$subtest
  ss_sorted  <- sort(available, decreasing = TRUE)
  strongest  <- names(ss_sorted)[1]
  weakest    <- names(ss_sorted)[length(ss_sorted)]

  # ── Discontinuation / Reversal ─────────────────────────────
  rev_subs <- c("WC", "FS", "WD", "SA", "SC", "LC")
  disc_subs <- character()
  rev_info  <- character()

  purrr::iwalk(split(raw_scores, raw_scores$subtest), function(sr, sub) {
    dr <- get_discontinue_rule(sub)
    if (dr > 0 && nrow(sr) >= 4 && all(tail(sr$score, 4) == 0)) {
      disc_subs <<- c(disc_subs, sub)
    }
    if (sub %in% rev_subs) {
      ms <- max_score_for_subtest(sub)
      scored <- which(sr$score == ms)
      if (length(scored) >= 2 && any(diff(scored) == 1)) {
        rev_info <<- c(rev_info, sub)
      }
    }
  })

  # ── Discontinuation / Reversal ─────────────────────────────
  # subtest 描述行
  subtest_lines <- map_chr(seq_len(nrow(raw_scores)), function(i) {
    t   <- raw_scores$subtest[i]
    raw <- raw_scores$raw[i]
    ss_val <- available[t]
    lvl <- if (ss_val >= 11) "above_average"
      else if (ss_val >= 8)  "upper_normal"
      else if (ss_val >= 4)  "average"
      else if (ss_val >= 2)  "below_average"
      else "significantly_below"
    lvl_txt <- switch(lvl,
      above_average       = "高于平均 (above average)",
      upper_normal        = "正常偏高 (upper normal range)",
      average             = "在正常范围内 (within average)",
      below_average      = "低于平均 (below average)",
      significantly_below = "显著低于平均 (significantly below average — clinical concern)"
    )
    sprintf("%s raw=%d SS=%d %s", t, raw, ss_val, lvl_txt)
  })
  subtest_details <- paste(subtest_lines, collapse = "; ")

  disc_txt  <- if (length(disc_subs) > 0) paste0("Discontinuation triggered: ", paste(disc_subs, collapse=", ")) else "None"
  rev_txt   <- if (length(rev_info)  > 0) paste0("Reversal triggered: ", paste(rev_info, collapse=", "))   else "None"

  prompt <- sprintf(
    "You are a clinical child psychologist specializing in language assessment.
Generate a professional clinical assessment report in Chinese for the following CELF-5 results.
Write in formal clinical language, 3rd person, as if signing an official report.

PATIENT INFO:
- Name: %s
- Age: %d years %d months
- Gender: %s
- Assessment Date: %s
- Age Group: %s

SUBTEST RESULTS:
%s

ADMINISTRATIVE FLAGS:
- %s
- %s

STRONGEST SUBTEST: %s (SS=%d)
WEAKEST SUBTEST: %s (SS=%d)

Write a comprehensive clinical narrative report with these sections (in Chinese):
1. 总评（Overall Summary — 2-3 sentences of overall clinical impression）
2. 各分测验结果分析（每项 2-3 句话，描述测量内容 + 临床发现 + 意义）
3. 临床画像（Clinical Profile — 强项弱项总结，2-3句话）
4. 建议（Recommendations — 3 bullet points, highest priority first）
5. 注意事项与局限性（Limitations — 1-2 sentences）

Requirements:
- Write in Chinese (简体中文)
- Clinical but compassionate tone
- Each subtest section must include what the test measures, the finding, and clinical implication
- The weakest subtest MUST be highlighted as requiring follow-up
- The recommendations must be specific and actionable
- Do NOT make up any additional data beyond what is provided above
- End with: 报告生成时间: %s | 本报告需经主试评估师审核签字
",
    pat$name[1], ass$age_years[1], ass$age_months[1],
    ifelse(is.na(pat$gender[1]), "未记录", pat$gender[1]),
    ass$date_str[1], ag,
    subtest_details,
    disc_txt, rev_txt,
    strongest, available[strongest],
    weakest,   available[weakest],
    ass$date_str[1]
  )

  # ── 调用 MiniMax ──────────────────────────────────────────
  raw_narrative <- .call_minimax(prompt, max_tokens = 4000L)
  # 去掉思考标记和 prompt 泄露（MiniMax 模型有时会在开头输出多余内容）
  # 策略：找到第一个中文字符作为正文起点
  narrative <- raw_narrative
  # 去掉开头可能的 思考内容（英文描述）
  narrative <- sub("^.*?\\n\\n(?=[\u4e00-\u9fff])", "", narrative, perl = TRUE)
  # 如果仍有残留指令（如 "You are a..."），直接截断
  if (grepl("^You are a", narrative, ignore.case = TRUE)) {
    # 找到第一个真正中文段落的位置
    chinese_start <- regexpr("[\u4e00-\u9fff]", narrative)[1]
    if (chinese_start > 1) {
      narrative <- substr(narrative, chinese_start, nchar(narrative))
    }
  }
  narrative
}



# ─────────────────────────────────────────────────────────────
# generate_clinical_narrative_en — 英文版临床报告
# 输入: assessment_id (integer)
# 输出: 临床评估报告文字 (character, English)
# ─────────────────────────────────────────────────────────────
generate_clinical_narrative_en <- function(assessment_id) {
  con <- get_con()
  on.exit(dbDisconnect(con))

  ass <- dbGetQuery(con, sprintf(
    "SELECT *, strftime('%%Y-%%m-%%d', assessment_date) as date_str
     FROM assessments WHERE id = %d", assessment_id
  ))
  pat <- dbGetQuery(con, sprintf(
    "SELECT * FROM patients WHERE id = %d", ass$patient_id[1]
  ))
  ag  <- ass$age_group

  resp <- dbGetQuery(con, sprintf(
    "SELECT subtest, item_number, score FROM responses
     WHERE assessment_id = %d AND score IS NOT NULL
     ORDER BY subtest, item_number",
    assessment_id
  ))

  raw_scores <- resp %>%
    group_by(subtest) %>%
    summarise(
      raw     = sum(score, na.rm = TRUE),
      n_items = n(),
      max_item= max(item_number, na.rm = TRUE),
      .groups = "drop"
    )

  sdf <- calculate_scaled_scores(
    setNames(as.list(raw_scores$raw), raw_scores$subtest),
    ag
  )

  available <- sdf$scaled_score
  names(available) <- sdf$subtest
  ss_sorted <- sort(available, decreasing = TRUE)
  strongest <- names(ss_sorted)[1]
  weakest   <- names(ss_sorted)[length(ss_sorted)]

  rev_subs  <- c("WC", "FS", "WD", "SA", "SC", "LC")
  disc_subs <- character()
  rev_info  <- character()

  purrr::iwalk(split(raw_scores, raw_scores$subtest), function(sr, sub) {
    dr <- get_discontinue_rule(sub)
    if (dr > 0 && nrow(sr) >= 4 && all(tail(sr$score, 4) == 0)) {
      disc_subs <<- c(disc_subs, sub)
    }
    if (sub %in% rev_subs) {
      ms <- max_score_for_subtest(sub)
      scored <- which(sr$score == ms)
      if (length(scored) >= 2 && any(diff(scored) == 1)) {
        rev_info <<- c(rev_info, sub)
      }
    }
  })

  subtest_lines <- map_chr(seq_len(nrow(raw_scores)), function(i) {
    t    <- raw_scores$subtest[i]
    raw  <- raw_scores$raw[i]
    ss_val <- available[t]
    lvl <- if (ss_val >= 11) "Above Average"
      else if (ss_val >= 8)  "Average"
      else if (ss_val >= 4)  "Average"
      else if (ss_val >= 2)  "Below Average"
      else "Significantly Below Average"
    sprintf("%s raw=%d SS=%d (%s)", t, raw, ss_val, lvl)
  })
  subtest_details <- paste(subtest_lines, collapse = "; ")

  disc_txt <- if (length(disc_subs) > 0) paste0("Discontinuation: ", paste(disc_subs, collapse=", ")) else "None"
  rev_txt  <- if (length(rev_info)  > 0) paste0("Reversal: ", paste(rev_info, collapse=", "))       else "None"

  prompt <- sprintf(
    "You are a clinical child psychologist specializing in language assessment.
Generate a professional clinical assessment report in English for the following CELF-5 results.
Write in formal clinical language, 3rd person, as if signing an official report.

PATIENT INFO:
- Name: %s
- Age: %d years %d months
- Gender: %s
- Assessment Date: %s
- Age Group: %s

SUBTEST RESULTS:
%s

ADMINISTRATIVE FLAGS:
- %s
- %s

STRONGEST SUBTEST: %s (SS=%d)
WEAKEST SUBTEST: %s (SS=%d)

Write a comprehensive clinical narrative report with these sections (in English):
1. Overall Summary (2-3 sentences of overall clinical impression)
2. Subtest Results Analysis (each subtest: what it measures + clinical finding + implication, 2-3 sentences)
3. Clinical Profile (strengths/weaknesses summary, 2-3 sentences)
4. Recommendations (3 bullet points, highest priority first)
5. Limitations and Cautions (1-2 sentences)

Requirements:
- Write in English
- Clinical but compassionate tone
- Each subtest section must include what the test measures, the finding, and clinical implication
- The weakest subtest MUST be highlighted as requiring follow-up
- Recommendations must be specific and actionable
- Do NOT make up any additional data beyond what is provided above
- End with: Report generated: %s | This report must be reviewed and signed by the examining clinician.
",
    pat$name[1], ass$age_years[1], ass$age_months[1],
    ifelse(is.na(pat$gender[1]), "Not recorded", pat$gender[1]),
    ass$date_str[1], ag,
    subtest_details,
    disc_txt, rev_txt,
    strongest, available[strongest],
    weakest,   available[weakest],
    ass$date_str[1]
  )
  # 思考标签已在 .call_minimax() 里统一清理掉，这里无需额外处理
  .call_minimax(prompt, max_tokens = 4000L)
}

# END OF FILE

