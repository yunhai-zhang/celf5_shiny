# slam_report.R — SLAM Clinical Narrative Report Generator
# Generates Chinese clinical narrative reports for SLAM assessments.
# Uses tidyverse + RSQLite + MiniMax API.

library(dplyr)
library(tidyr)
library(purrr)
library(RSQLite)
library(lubridate)
library(stringr)

# ─────────────────────────────────────────────────────────────
# Constants
# ─────────────────────────────────────────────────────────────
DB_PATH <- "/home/yzhang/clawfiles/celf5_shiny/celf5_assessments.db"

get_con <- function() dbConnect(SQLite(), DB_PATH)

# Story metadata (mirrors app_slam.R)
STORIES <- list(
  baseball_troubles = list(
    id   = "BaseballTroubles",
    name = "Baseball Troubles",
    name_zh = "棒球烦恼",
    age_range = "13-17岁",
    n_wf = 6L,
    n_gfa = 4L,
    wf_max = 6L,
    gfa_max = 8L,
    narrative_max = 10L
  ),
  the_best_turkey = list(
    id   = "TheBestTurkey",
    name = "The Best Turkey",
    name_zh = "最好的火鸡",
    age_range = "10-14岁",
    n_wf = 5L,
    n_gfa = 4L,
    wf_max = 5L,
    gfa_max = 8L,
    narrative_max = 10L
  ),
  the_girl_who_loved_horses = list(
    id   = "GirlWhoLovedHorses",
    name = "The Girl Who Loved Horses",
    name_zh = "爱马的女孩",
    age_range = "13-17岁",
    n_wf = 6L,
    n_gfa = 4L,
    wf_max = 6L,
    gfa_max = 8L,
    narrative_max = 10L
  ),
  wallace_and_batty = list(
    id   = "WallaceAndBatty",
    name = "Wallace and Batty",
    name_zh = "华莱士与巴蒂",
    age_range = "7-14岁",
    n_wf = 5L,
    n_gfa = 4L,
    wf_max = 5L,
    gfa_max = 8L,
    narrative_max = 10L
  )
)

# ─────────────────────────────────────────────────────────────
# SLAM Norms (simplified, ages 7-17)
# ─────────────────────────────────────────────────────────────
build_slam_norms <- function() {
  ages   <- rep(7:17, each = 4)
  raw    <- rep(c(0, 5, 10, 15), 11)
  std_wf <- c(
    50,62,74,86,  50,63,75,87,  51,64,76,88,  51,64,77,88,
    52,65,78,89,  52,66,79,90,  53,67,80,91,  54,68,81,92,
    55,69,82,93,  56,70,83,94,  57,71,84,95
  )
  std_gfa <- c(
    50,60,70,82,  50,61,71,83,  51,62,72,84,  52,63,73,85,
    53,64,74,86,  54,65,75,87,  55,66,76,88,  56,67,77,89,
    57,68,78,90,  58,69,79,91,  59,70,80,92
  )
  tibble(age = ages, raw_score = raw, std_word_finding = std_wf, std_gfa = std_gfa)
}

SLAM_NORMS <- build_slam_norms()

get_slam_ss <- function(raw, type = c("word_finding", "gfa"), age) {
  type <- match.arg(type)
  col  <- if (type == "word_finding") "std_word_finding" else "std_gfa"
  row  <- SLAM_NORMS %>%
    filter(.data$age == !!age, .data$raw_score <= !!raw) %>%
    summarise(s = max(.data[[col]]), .groups = "drop")
  if (nrow(row) == 0 || is.infinite(row$s[1])) return(NA_integer_)
  row$s[1]
}

ss_to_pr <- function(ss) {
  # Approximate percentile rank from standard score (mean=100, sd=15)
  if (is.na(ss) || ss < 50) return(NA_real_)
  pnorm((ss - 100) / 15) * 100
}

ss_to_range <- function(ss) {
  # 68% confidence interval (approx ±1 SD = 15 pts for this scale)
  lo <- max(50, ss - 15)
  hi <- min(140, ss + 15)
  c(lo, hi)
}

# ─────────────────────────────────────────────────────────────
# MiniMax helpers (mirrors global.R)
# ─────────────────────────────────────────────────────────────
.read_minimax_key <- function() {
  env_path <- Sys.getenv("MINIMAX_CN_API_KEY", unset = "~/.hermes/.env")
  if (file.exists(env_path)) {
    lines <- readLines(env_path, warn = FALSE, encoding = "UTF-8")
    kv <- lines[str_detect(lines, "=")]
    vals <- sapply(str_split(kv, "=", 2), `[`, 2)
    names(vals) <- sapply(str_split(kv, "="), `[`, 1)
    return(val <- vals["MINIMAX_CN_API_KEY"])
  }
  Sys.getenv("MINIMAX_CN_API_KEY", unset = "")
}

.clean_narrative_tags <- function(raw_text) {
  tk_patterns <- list(
    "【知道[\\s\\S]*?【想知道",
    "【想知道[\\s\\S]*?【知道",
    "<thought>[\\s\\S]*?</thought>",
    "< THOUGHT >[\\s\\S]*?</THOUGHT>",
    "\\[think\\][\\s\\S]*?\\[/think\\]"
  )
  cleaned <- raw_text
  for (pat in tk_patterns) {
    cleaned <- stringr::str_remove_all(cleaned, pat)
  }
  cleaned <- stringr::str_remove(cleaned,
    "^[\\s]*?(You are a clinical|Generate a professional|This report|Writing in)[\\s\\S]*?$")
  cleaned <- stringr::str_remove(cleaned,
    "^[\\s]*?(Write in Chinese|Write in English|Please generate)[\\s\\S]*?$")
  stringr::str_trim(cleaned)
}

.call_minimax <- function(prompt, max_tokens = 800L) {
  api_key <- .read_minimax_key()
  if (nzchar(api_key) == 0) stop("MINIMAX_CN_API_KEY not found")
  url   <- "https://api.minimaxi.com/v1/chat/completions"
  body  <- list(
    model      = "MiniMax-M2.7",
    messages   = list(list(role = "user", content = prompt)),
    max_tokens = max_tokens,
    temperature = 0.7
  )
  resp  <- httr::POST(
    url,
    httr::add_headers(
      `Content-Type`  = "application/json",
      `Authorization` = paste0("Bearer ", api_key)
    ),
    body = body,
    encode = "json"
  )
  txt   <- httr::content(resp, as = "text", encoding = "UTF-8")
  if (resp$status_code != 200) {
    stop(sprintf("MiniMax API error %d: %s", resp$status_code, txt))
  }
  parsed <- jsonlite::fromJSON(txt, simplifyVector = FALSE)
  .clean_narrative_tags(parsed$choices[[1]]$message$content)
}

# ─────────────────────────────────────────────────────────────
# Subtest descriptions
# ─────────────────────────────────────────────────────────────
wf_description <- "Word Finding / 图片命名 — 测量快速命名视觉刺激的能力，反映词汇提取效率"
gfa_description <- "Grammar Fluency Assessment / 语法填空 — 测量句法补全能力，反映语法加工效率"
narr_description <- "Free Narrative / 自由叙事 — 测量叙事结构、复杂句、推论、语用和理论心智"

ss_level_zh <- function(ss) {
  if (is.na(ss)) return("数据不足")
  if (ss >= 110) return("高于平均")
  if (ss >= 100) return("正常偏高")
  if (ss >= 90)  return("在正常范围内")
  if (ss >= 80)  return("低于平均")
  if (ss >= 70)  return("显著低于平均")
  "显著低于平均"
}

ss_level_en <- function(ss) {
  if (is.na(ss)) return("Insufficient data")
  if (ss >= 110) return("Above Average")
  if (ss >= 100) return("High Average")
  if (ss >= 90)  return("Average")
  if (ss >= 80)  return("Low Average")
  if (ss >= 70)  return("Borderline")
  "Very Low"
}

# ─────────────────────────────────────────────────────────────
# generate_slam_report — Main entry point
# Input: student_id (patient id), assessment_id (integer)
# Output: clinical narrative string (Chinese)
# ─────────────────────────────────────────────────────────────
generate_slam_report <- function(student_id, assessment_id, celf5_id = NULL, lang = c("zh", "en")) {
  con <- get_con()
  on.exit(dbDisconnect(con), add = TRUE)
  lang <- match.arg(lang)

  # ── Patient info ─────────────────────────────────────────────
  pat <- dbGetQuery(con, sprintf(
    "SELECT * FROM patients WHERE id = %d", student_id
  ))
  if (nrow(pat) == 0) stop("Patient not found: ", student_id)

  slam_available <- !is.null(assessment_id) && !is.na(assessment_id) && assessment_id > 0

  if (slam_available) {
    ass <- dbGetQuery(con, sprintf(
      "SELECT *, strftime('%%Y-%%m-%%d', assessment_date) as date_str
       FROM assessments WHERE id = %d", assessment_id
    ))
    if (nrow(ass) == 0) stop("Assessment not found: ", assessment_id)
    age_years <- ass$age_years[1]
    age_group <- ass$age_group[1]
    date_str  <- ass$date_str[1]

    scores <- dbGetQuery(con, sprintf(
      "SELECT subtest, raw_score, scaled_score, percentile, story_id
       FROM subtest_scores WHERE assessment_id = %d",
      assessment_id
    ))

    narr_scores <- dbGetQuery(con, sprintf(
      "SELECT story_id, dimension, score
       FROM narrative_scores WHERE assessment_id = %d",
      assessment_id
    ))

    composites <- dbGetQuery(con, sprintf(
      "SELECT * FROM slam_composites WHERE assessment_id = %d",
      assessment_id
    ))
  } else {
    # No SLAM assessment — only CELF-5 data
    ass <- NULL
    age_years <- NA_integer_
    age_group <- NA_character_
    date_str  <- NA_character_
    scores    <- tibble(subtest = character(), raw_score = integer(),
                        scaled_score = integer(), percentile = numeric(),
                        story_id = character())
    narr_scores <- tibble(story_id = character(), dimension = character(),
                          score = integer())
    composites   <- tibble()
  }

  # ── CELF-5 data (if celf5_id provided) ─────────────────────
  celf5_data <- NULL
  if (!is.null(celf5_id) && !is.na(celf5_id)) {
    celf5_ass <- dbGetQuery(con, sprintf(
      "SELECT *, strftime('%%Y-%%m-%%d', assessment_date) as date_str
       FROM assessments WHERE id = %d", celf5_id
    ))
    if (nrow(celf5_ass) > 0) {
      # Get CELF-5 index scores
      celf5_scores <- dbGetQuery(con, sprintf(
        "SELECT composite as index_name, standard_score, percentile_rank
         FROM composite_scores WHERE assessment_id = %d", celf5_id
      ))
      celf5_data <- list(assessment = celf5_ass, scores = celf5_scores)
    }
  }

  # ── Build per-story summary ─────────────────────────────────
  story_ids <- c("BaseballTroubles", "TheBestTurkey", "GirlWhoLovedHorses", "WallaceAndBatty")
  story_keys <- c("baseball_troubles", "the_best_turkey", "the_girl_who_loved_horses", "wallace_and_batty")

  story_summary <- map_dfr(seq_along(story_ids), function(i) {
    sid <- story_ids[i]
    sk  <- story_keys[i]
    sinfo <- STORIES[[sk]]

    wf_row  <- scores %>% filter(grepl(paste0(sid, "_WordFinding"), subtest))
    gfa_row <- scores %>% filter(grepl(paste0(sid, "_GFA"), subtest))
    narr_row <- scores %>% filter(grepl(paste0(sid, "_Narrative"), subtest))

    wf_raw  <- wf_row$raw_score[1]
    gfa_raw <- gfa_row$raw_score[1]
    narr_raw <- if (nrow(narr_row) > 0) narr_row$raw_score[1] else NA_integer_

    wf_ss  <- get_slam_ss(wf_raw,  "word_finding", age_years)
    gfa_ss <- get_slam_ss(gfa_raw, "gfa",          age_years)

    wf_pr  <- ss_to_pr(wf_ss)
    gfa_pr <- ss_to_pr(gfa_ss)

    # Narrative rubric dimensions
    narr_dims <- if (nrow(narr_scores) > 0) {
      nd <- narr_scores %>% filter(story_id == sid)
      setNames(nd$score, nd$dimension)
    } else {
      setNames(integer(), character())
    }

    tibble(
      story_id       = sid,
      story_key      = sk,
      story_name     = sinfo$name,
      story_name_zh  = sinfo$name_zh,
      age_range      = sinfo$age_range,
      wf_raw         = wf_raw %||% NA_integer_,
      wf_max         = sinfo$wf_max,
      wf_ss          = wf_ss %||% NA_integer_,
      wf_pr          = wf_pr %||% NA_real_,
      gfa_raw        = gfa_raw %||% NA_integer_,
      gfa_max        = sinfo$gfa_max,
      gfa_ss         = gfa_ss %||% NA_integer_,
      gfa_pr         = gfa_pr %||% NA_real_,
      narr_raw       = narr_raw %||% NA_integer_,
      narr_max       = sinfo$narrative_max,
      narrative_dims = list(narr_dims)
    )
  })

  # ── Identify strongest / weakest ──────────────────────────────
  wf_data <- story_summary %>% filter(!is.na(wf_ss)) %>% arrange(desc(wf_ss))
  gfa_data <- story_summary %>% filter(!is.na(gfa_ss)) %>% arrange(desc(gfa_ss))

  strongest_wf <- wf_data$story_name_zh[1]
  weakest_wf   <- wf_data$story_name_zh[nrow(wf_data)]
  strongest_gfa <- gfa_data$story_name_zh[1]
  weakest_gfa   <- gfa_data$story_name_zh[nrow(gfa_data)]

  # Overall composite (average SS across all stories)
  avg_wf_ss  <- mean(story_summary$wf_ss,  na.rm = TRUE)
  avg_gfa_ss <- mean(story_summary$gfa_ss, na.rm = TRUE)

  # ── Determine which data is available ────────────────────────
  has_slam  <- slam_available
  has_celf5 <- !is.null(celf5_data) && !is.null(celf5_data$scores) && nrow(celf5_data$scores) > 0

  # ── Gender helpers ────────────────────────────────────────────
  gender_zh_val <- if (is.na(pat$gender[1]) || pat$gender[1] == "") {
    "未记录"
  } else if (pat$gender[1] == "M") {
    "男"
  } else if (pat$gender[1] == "F") {
    "女"
  } else {
    pat$gender[1]
  }
  gender_en_val <- if (is.na(pat$gender[1]) || pat$gender[1] == "") {
    "Not recorded"
  } else if (pat$gender[1] == "M") {
    "Male"
  } else if (pat$gender[1] == "F") {
    "Female"
  } else {
    pat$gender[1]
  }

  # ── Case 1: SLAM only ─────────────────────────────────────────
  if (has_slam && !has_celf5) {

    subtest_lines_zh <- map_chr(seq_len(nrow(story_summary)), function(i) {
      r <- story_summary[i, ]
      wf_lvl  <- ss_level_zh(r$wf_ss)
      gfa_lvl <- ss_level_zh(r$gfa_ss)
      sprintf(
        "%s [Word Finding]: 原始%d/%d, SS=%d(%s), 百分位%.0f; %s [GFA]: 原始%d/%d, SS=%d(%s), 百分位%.0f; 叙事原始%d/%d",
        r$story_name_zh,
        r$wf_raw, r$wf_max, r$wf_ss, wf_lvl, r$wf_pr,
        r$story_name_zh,
        r$gfa_raw, r$gfa_max, r$gfa_ss, gfa_lvl, r$gfa_pr,
        r$narr_raw %||% 0L, r$narr_max
      )
    })
    subtest_details_zh <- paste(subtest_lines_zh, collapse = "\n")

    dim_names_zh <- c(
      "叙事结构" = "Narrative Structure",
      "复杂句"   = "Complex Clauses",
      "推论能力" = "Inferencing",
      "语用"     = "Pragmatic",
      "理论心智" = "Theory of Mind"
    )
    narr_dim_lines_zh <- map_chr(seq_len(nrow(story_summary)), function(i) {
      r <- story_summary[i, ]
      nd <- r$narrative_dims[[1]]
      if (length(nd) == 0) return(sprintf("%s 叙事评分: 未记录", r$story_name_zh))
      lines <- map_chr(seq_along(dim_names_zh), function(d) {
        dim_en <- names(dim_names_zh)[d]
        dim_zh <- dim_names_zh[d]
        scr <- nd[dim_en] %||% NA_integer_
        sprintf("%s(%s): %d/2", dim_zh, dim_en, scr)
      })
      sprintf("%s 叙事维度: %s", r$story_name_zh, paste(lines, collapse = ", "))
    })
    narr_details_zh <- paste(narr_dim_lines_zh, collapse = "\n")

    subtest_lines_en <- map_chr(seq_len(nrow(story_summary)), function(i) {
      r <- story_summary[i, ]
      wf_lvl_en  <- ss_level_en(r$wf_ss)
      gfa_lvl_en <- ss_level_en(r$gfa_ss)
      sprintf(
        "%s [Word Finding]: raw %d/%d, SS=%d(%s), PR=%.0f; %s [GFA]: raw %d/%d, SS=%d(%s), PR=%.0f; Narrative raw %d/%d",
        r$story_name,
        r$wf_raw, r$wf_max, r$wf_ss, wf_lvl_en, r$wf_pr,
        r$story_name,
        r$gfa_raw, r$gfa_max, r$gfa_ss, gfa_lvl_en, r$gfa_pr,
        r$narr_raw %||% 0L, r$narr_max
      )
    })
    subtest_details_en <- paste(subtest_lines_en, collapse = "\n")

    dim_names_en <- c(
      "Narrative Structure" = "Narrative Structure",
      "Complex Clauses"     = "Complex Clauses",
      "Inferencing"         = "Inferencing",
      "Pragmatic"          = "Pragmatic",
      "Theory of Mind"     = "Theory of Mind"
    )
    narr_dim_lines_en <- map_chr(seq_len(nrow(story_summary)), function(i) {
      r <- story_summary[i, ]
      nd <- r$narrative_dims[[1]]
      if (length(nd) == 0) return(sprintf("%s Narrative dims: Not recorded", r$story_name))
      lines_en <- map_chr(seq_along(dim_names_en), function(d) {
        dim_en <- names(dim_names_en)[d]
        scr <- nd[dim_en] %||% NA_integer_
        sprintf("%s: %d/2", dim_en, scr)
      })
      sprintf("%s Narrative dimensions: %s", r$story_name, paste(lines_en, collapse = ", "))
    })
    narr_details_en <- paste(narr_dim_lines_en, collapse = "\n")

    strongest_wf_en  <- wf_data$story_name[1]
    weakest_wf_en    <- wf_data$story_name[nrow(wf_data)]
    strongest_gfa_en <- gfa_data$story_name[1]
    weakest_gfa_en   <- gfa_data$story_name[nrow(gfa_data)]

    if (lang == "zh") {
      prompt <- sprintf(
        "You are a clinical child psychologist specializing in narrative language assessment.
Generate a professional clinical assessment report in Chinese (Simplified Chinese) for the following SLAM (Structured Language Assessment Measures) results.
Write in formal clinical language, 3rd person, as if signing an official report.

PATIENT INFO:
- Name: %s
- Age: %d years
- Gender: %s
- Assessment Date: %s
- Age Group: %s

SUBTEST RESULTS (by story):
%s

NARRATIVE DIMENSIONS:
%s

OVERALL SUMMARY:
- 平均 Word Finding SS: %.0f (68%% CI: %.0f-%.0f)
- 平均 GFA SS: %.0f (68%% CI: %.0f-%.0f)
- Word Finding 最强故事: %s
- Word Finding 最弱故事: %s
- GFA 最强故事: %s
- GFA 最弱故事: %s

Write a comprehensive clinical narrative report with these sections (in Chinese, Simplified):
1. 总评（Overall Summary — 2-3 sentences of overall clinical impression about narrative language abilities）
2. 各故事结果分析（每项故事 2-3 句话，包含测量内容 + 临床发现 + 意义）
3. 叙事能力分析（Free Narrative 5维度总结：叙事结构、复杂句、推论、语用、理论心智）
4. 临床画像（Clinical Profile — 强项弱项总结，2-3句话）
5. 建议（Recommendations — 3 bullet points, highest priority first）
6. 注意事项与局限性（Limitations — 1-2 sentences）

Requirements:
- Write entirely in Simplified Chinese (简体中文)
- Clinical but compassionate tone
- Each story section must include what it measures, the finding, and clinical implication
- The weakest areas MUST be highlighted as requiring follow-up
- The recommendations must be specific and actionable
- Do NOT make up any additional data beyond what is provided above
- End with: 报告生成时间: %s | 本报告需经主试评估师审核签字
",
        pat$name[1], age_years, gender_zh_val, date_str, age_group,
        subtest_details_zh, narr_details_zh,
        avg_wf_ss,  ss_to_range(avg_wf_ss)[1],  ss_to_range(avg_wf_ss)[2],
        avg_gfa_ss, ss_to_range(avg_gfa_ss)[1], ss_to_range(avg_gfa_ss)[2],
        strongest_wf, weakest_wf, strongest_gfa, weakest_gfa,
        date_str
      )
    } else {
      prompt <- sprintf(
        "You are a clinical child psychologist specializing in narrative language assessment.
Generate a professional clinical assessment report in English for the following SLAM (Structured Language Assessment Measures) results.
Write in formal clinical language, 3rd person, as if signing an official report.

PATIENT INFO:
- Name: %s
- Age: %d years
- Gender: %s
- Assessment Date: %s
- Age Group: %s

SUBTEST RESULTS (by story):
%s

NARRATIVE DIMENSIONS:
%s

OVERALL SUMMARY:
- Mean Word Finding SS: %.0f (68%% CI: %.0f-%.0f)
- Mean GFA SS: %.0f (68%% CI: %.0f-%.0f)
- Strongest story (Word Finding): %s
- Weakest story (Word Finding): %s
- Strongest story (GFA): %s
- Weakest story (GFA): %s

Write a comprehensive clinical narrative report with these sections (in English):
1. Overall Summary (2-3 sentences of overall clinical impression about narrative language abilities)
2. Story-by-Story Analysis (2-3 sentences per story, including what was measured, findings, and clinical implications)
3. Narrative Abilities Analysis (5-dimension summary: Narrative Structure, Complex Clauses, Inferencing, Pragmatic, Theory of Mind)
4. Clinical Profile (strengths and weaknesses summary, 2-3 sentences)
5. Recommendations (3 bullet points, highest priority first)
6. Limitations and Caveats (1-2 sentences)

Requirements:
- Write entirely in English
- Clinical but compassionate tone
- Each story section must include what it measures, the finding, and clinical implication
- The weakest areas MUST be highlighted as requiring follow-up
- The recommendations must be specific and actionable
- Do NOT make up any additional data beyond what is provided above
- End with: Report generated: %s | This report must be reviewed and signed by the evaluating clinician.
",
        pat$name[1], age_years, gender_en_val, date_str, age_group,
        subtest_details_en, narr_details_en,
        avg_wf_ss,  ss_to_range(avg_wf_ss)[1],  ss_to_range(avg_wf_ss)[2],
        avg_gfa_ss, ss_to_range(avg_gfa_ss)[1], ss_to_range(avg_gfa_ss)[2],
        strongest_wf_en, weakest_wf_en, strongest_gfa_en, weakest_gfa_en,
        date_str
      )
    }

  # ── Case 2: CELF-5 only ────────────────────────────────────────
  } else if (!has_slam && has_celf5) {

    celf5_scores_zh <- celf5_data$scores
    celf5_scores_en <- celf5_data$scores

    if (lang == "zh") {
      celf5_lines_zh <- apply(celf5_scores_zh, 1, function(row) {
        lvl <- ss_level_zh(as.integer(row["standard_score"]))
        sprintf("%s: SS=%d(%s), 百分位%.0f",
                row["index_name"], as.integer(row["standard_score"]), lvl,
                as.numeric(row["percentile_rank"]))
      })

      prompt <- sprintf(
        "You are a clinical child psychologist specializing in standardized language assessment.
Generate a professional clinical assessment report in Chinese (Simplified Chinese) for the following CELF-5 (Clinical Evaluation of Language Fundamentals – Fifth Edition) results.
Write in formal clinical language, 3rd person, as if signing an official report.

PATIENT INFO:
- Name: %s
- Age: %d years
- Gender: %s
- Assessment Date: %s
- Age Group: %s

CELF-5 INDEX SCORES:
%s

OVERALL SUMMARY:
- Total Language (TL): SS=%d (68%% CI: %d-%d)
- Core Language (CLS): SS=%d (68%% CI: %d-%d)
- For each index, identify whether results are within normal limits or indicate concern

Write a comprehensive clinical narrative report with these sections (in Chinese, Simplified):
1. 总评（Overall Summary — 2-3 sentences of overall clinical impression about language abilities based on CELF-5 results）
2. 各指数分析（每项指数 2-3 句话，包含测量内容 + 临床发现 + 意义）
3. 临床画像（Clinical Profile — 强项弱项总结，2-3句话）
4. 建议（Recommendations — 3 bullet points, highest priority first）
5. 注意事项与局限性（Limitations — 1-2 sentences）

Requirements:
- Write entirely in Simplified Chinese (简体中文)
- Clinical but compassionate tone
- Focus on CELF-5 index scores only (no SLAM subtest data)
- The lowest-scoring indices MUST be highlighted as requiring follow-up
- The recommendations must be specific and actionable
- Do NOT make up any additional data beyond what is provided above
- End with: 报告生成时间: %s | 本报告需经主试评估师审核签字
",
        pat$name[1], age_years, gender_zh_val,
        celf5_data$assessment$date_str[1], age_group,
        paste(celf5_lines_zh, collapse = "\n"),
        # Use first CELF-5 score as representative
        as.integer(celf5_scores_zh$standard_score[1]),
        ss_to_range(as.integer(celf5_scores_zh$standard_score[1]))[1],
        ss_to_range(as.integer(celf5_scores_zh$standard_score[1]))[2],
        as.integer(celf5_scores_zh$standard_score[1]),
        ss_to_range(as.integer(celf5_scores_zh$standard_score[1]))[1],
        ss_to_range(as.integer(celf5_scores_zh$standard_score[1]))[2],
        date_str
      )
    } else {
      celf5_lines_en <- apply(celf5_scores_en, 1, function(row) {
        lvl <- ss_level_en(as.integer(row["standard_score"]))
        sprintf("%s: SS=%d(%s), PR=%.0f",
                row["index_name"], as.integer(row["standard_score"]), lvl,
                as.numeric(row["percentile_rank"]))
      })

      prompt <- sprintf(
        "You are a clinical child psychologist specializing in standardized language assessment.
Generate a professional clinical assessment report in English for the following CELF-5 (Clinical Evaluation of Language Fundamentals – Fifth Edition) results.
Write in formal clinical language, 3rd person, as if signing an official report.

PATIENT INFO:
- Name: %s
- Age: %d years
- Gender: %s
- Assessment Date: %s
- Age Group: %s

CELF-5 INDEX SCORES:
%s

OVERALL SUMMARY:
- Total Language (TL): SS=%d (68%% CI: %d-%d)
- Core Language (CLS): SS=%d (68%% CI: %d-%d)
- For each index, identify whether results are within normal limits or indicate concern

Write a comprehensive clinical narrative report with these sections (in English):
1. Overall Summary (2-3 sentences of overall clinical impression about language abilities based on CELF-5 results)
2. Index-by-Index Analysis (2-3 sentences per index, including what was measured, findings, and clinical implications)
3. Clinical Profile (strengths and weaknesses summary, 2-3 sentences)
4. Recommendations (3 bullet points, highest priority first)
5. Limitations and Caveats (1-2 sentences)

Requirements:
- Write entirely in English
- Clinical but compassionate tone
- Focus on CELF-5 index scores only (no SLAM subtest data)
- The lowest-scoring indices MUST be highlighted as requiring follow-up
- The recommendations must be specific and actionable
- Do NOT make up any additional data beyond what is provided above
- End with: Report generated: %s | This report must be reviewed and signed by the evaluating clinician.
",
        pat$name[1], age_years, gender_en_val,
        celf5_data$assessment$date_str[1], age_group,
        paste(celf5_lines_en, collapse = "\n"),
        as.integer(celf5_scores_en$standard_score[1]),
        ss_to_range(as.integer(celf5_scores_en$standard_score[1]))[1],
        ss_to_range(as.integer(celf5_scores_en$standard_score[1]))[2],
        as.integer(celf5_scores_en$standard_score[1]),
        ss_to_range(as.integer(celf5_scores_en$standard_score[1]))[1],
        ss_to_range(as.integer(celf5_scores_en$standard_score[1]))[2],
        date_str
      )
    }

  # ── Case 3: Both SLAM + CELF-5 ───────────────────────────────
  } else if (has_slam && has_celf5) {

    subtest_lines_zh <- map_chr(seq_len(nrow(story_summary)), function(i) {
      r <- story_summary[i, ]
      wf_lvl  <- ss_level_zh(r$wf_ss)
      gfa_lvl <- ss_level_zh(r$gfa_ss)
      sprintf(
        "%s [Word Finding]: 原始%d/%d, SS=%d(%s), 百分位%.0f; %s [GFA]: 原始%d/%d, SS=%d(%s), 百分位%.0f; 叙事原始%d/%d",
        r$story_name_zh,
        r$wf_raw, r$wf_max, r$wf_ss, wf_lvl, r$wf_pr,
        r$story_name_zh,
        r$gfa_raw, r$gfa_max, r$gfa_ss, gfa_lvl, r$gfa_pr,
        r$narr_raw %||% 0L, r$narr_max
      )
    })
    subtest_details_zh <- paste(subtest_lines_zh, collapse = "\n")

    dim_names_zh <- c(
      "叙事结构" = "Narrative Structure",
      "复杂句"   = "Complex Clauses",
      "推论能力" = "Inferencing",
      "语用"     = "Pragmatic",
      "理论心智" = "Theory of Mind"
    )
    narr_dim_lines_zh <- map_chr(seq_len(nrow(story_summary)), function(i) {
      r <- story_summary[i, ]
      nd <- r$narrative_dims[[1]]
      if (length(nd) == 0) return(sprintf("%s 叙事评分: 未记录", r$story_name_zh))
      lines <- map_chr(seq_along(dim_names_zh), function(d) {
        dim_en <- names(dim_names_zh)[d]
        dim_zh <- dim_names_zh[d]
        scr <- nd[dim_en] %||% NA_integer_
        sprintf("%s(%s): %d/2", dim_zh, dim_en, scr)
      })
      sprintf("%s 叙事维度: %s", r$story_name_zh, paste(lines, collapse = ", "))
    })
    narr_details_zh <- paste(narr_dim_lines_zh, collapse = "\n")

    celf5_lines_zh <- apply(celf5_data$scores, 1, function(row) {
      lvl <- ss_level_zh(as.integer(row["standard_score"]))
      sprintf("%s: SS=%d(%s), 百分位%.0f",
              row["index_name"], as.integer(row["standard_score"]), lvl,
              as.numeric(row["percentile_rank"]))
    })
    celf5_section_zh <- sprintf("\n|CELF-5 RESULTS:\n%s\n", paste(celf5_lines_zh, collapse = "\n"))

    subtest_lines_en <- map_chr(seq_len(nrow(story_summary)), function(i) {
      r <- story_summary[i, ]
      wf_lvl_en  <- ss_level_en(r$wf_ss)
      gfa_lvl_en <- ss_level_en(r$gfa_ss)
      sprintf(
        "%s [Word Finding]: raw %d/%d, SS=%d(%s), PR=%.0f; %s [GFA]: raw %d/%d, SS=%d(%s), PR=%.0f; Narrative raw %d/%d",
        r$story_name,
        r$wf_raw, r$wf_max, r$wf_ss, wf_lvl_en, r$wf_pr,
        r$story_name,
        r$gfa_raw, r$gfa_max, r$gfa_ss, gfa_lvl_en, r$gfa_pr,
        r$narr_raw %||% 0L, r$narr_max
      )
    })
    subtest_details_en <- paste(subtest_lines_en, collapse = "\n")

    dim_names_en <- c(
      "Narrative Structure" = "Narrative Structure",
      "Complex Clauses"     = "Complex Clauses",
      "Inferencing"         = "Inferencing",
      "Pragmatic"          = "Pragmatic",
      "Theory of Mind"     = "Theory of Mind"
    )
    narr_dim_lines_en <- map_chr(seq_len(nrow(story_summary)), function(i) {
      r <- story_summary[i, ]
      nd <- r$narrative_dims[[1]]
      if (length(nd) == 0) return(sprintf("%s Narrative dims: Not recorded", r$story_name))
      lines_en <- map_chr(seq_along(dim_names_en), function(d) {
        dim_en <- names(dim_names_en)[d]
        scr <- nd[dim_en] %||% NA_integer_
        sprintf("%s: %d/2", dim_en, scr)
      })
      sprintf("%s Narrative dimensions: %s", r$story_name, paste(lines_en, collapse = ", "))
    })
    narr_details_en <- paste(narr_dim_lines_en, collapse = "\n")

    celf5_lines_en <- apply(celf5_data$scores, 1, function(row) {
      lvl <- ss_level_en(as.integer(row["standard_score"]))
      sprintf("%s: SS=%d(%s), PR=%.0f",
              row["index_name"], as.integer(row["standard_score"]), lvl,
              as.numeric(row["percentile_rank"]))
    })
    celf5_section_en <- sprintf("\n|CELF-5 RESULTS:\n%s\n", paste(celf5_lines_en, collapse = "\n"))

    strongest_wf_en  <- wf_data$story_name[1]
    weakest_wf_en    <- wf_data$story_name[nrow(wf_data)]
    strongest_gfa_en <- gfa_data$story_name[1]
    weakest_gfa_en   <- gfa_data$story_name[nrow(gfa_data)]

    if (lang == "zh") {
      prompt <- sprintf(
        "You are a clinical child psychologist specializing in narrative and standardized language assessment.
Generate a professional clinical assessment report in Chinese (Simplified Chinese) for the following combined CELF-5 + SLAM (Structured Language Assessment Measures) results.
Write in formal clinical language, 3rd person, as if signing an official report.

PATIENT INFO:
- Name: %s
- Age: %d years
- Gender: %s
- Assessment Dates: CELF-5 %s / SLAM %s
- Age Group: %s

|CELF-5 RESULTS:
%s

|SLAM SUBTEST RESULTS (by story):
%s

|SLAM NARRATIVE DIMENSIONS:
%s

|OVERALL SUMMARY (SLAM):
- 平均 Word Finding SS: %.0f (68%% CI: %.0f-%.0f)
- 平均 GFA SS: %.0f (68%% CI: %.0f-%.0f)
- Word Finding 最强故事: %s / 最弱故事: %s
- GFA 最强故事: %s / 最弱故事: %s

Write a comprehensive clinical narrative report with these sections (in Chinese, Simplified):
1. 总评（Combined Summary — 2-3 sentences integrating both CELF-5 and SLAM results）
2. CELF-5 分析（Core Language + each index: 2-3 sentences per index）
3. SLAM 各故事结果分析（每项故事 2-3 句话，包含测量内容 + 临床发现 + 意义）
4. 叙事能力分析（5维度总结：叙事结构、复杂句、推论、语用、理论心智）
5. 临床画像（综合强项弱项，2-3句话，结合CELF-5与SLAM）
6. 建议（Recommendations — 3 bullet points, highest priority first）
7. 注意事项与局限性（Limitations — 1-2 sentences）

Requirements:
- Write entirely in Simplified Chinese (简体中文)
- Clinical but compassionate tone
- Integrate both CELF-5 and SLAM findings — do not treat them in isolation
- The weakest areas MUST be highlighted as requiring follow-up
- The recommendations must be specific and actionable
- Do NOT make up any additional data beyond what is provided above
- End with: 报告生成时间: %s | 本报告需经主试评估师审核签字
",
        pat$name[1], age_years, gender_zh_val,
        celf5_data$assessment$date_str[1], date_str, age_group,
        paste(celf5_lines_zh, collapse = "\n"),
        subtest_details_zh, narr_details_zh,
        avg_wf_ss,  ss_to_range(avg_wf_ss)[1],  ss_to_range(avg_wf_ss)[2],
        avg_gfa_ss, ss_to_range(avg_gfa_ss)[1], ss_to_range(avg_gfa_ss)[2],
        strongest_wf, weakest_wf, strongest_gfa, weakest_gfa,
        date_str
      )
    } else {
      prompt <- sprintf(
        "You are a clinical child psychologist specializing in narrative and standardized language assessment.
Generate a professional clinical assessment report in English for the following combined CELF-5 + SLAM (Structured Language Assessment Measures) results.
Write in formal clinical language, 3rd person, as if signing an official report.

PATIENT INFO:
- Name: %s
- Age: %d years
- Gender: %s
- Assessment Dates: CELF-5 %s / SLAM %s
- Age Group: %s

|CELF-5 RESULTS:
%s

|SLAM SUBTEST RESULTS (by story):
%s

|SLAM NARRATIVE DIMENSIONS:
%s

|OVERALL SUMMARY (SLAM):
- Mean Word Finding SS: %.0f (68%% CI: %.0f-%.0f)
- Mean GFA SS: %.0f (68%% CI: %.0f-%.0f)
- Strongest story (Word Finding): %s / Weakest: %s
- Strongest story (GFA): %s / Weakest: %s

Write a comprehensive clinical narrative report with these sections (in English):
1. Combined Summary (2-3 sentences integrating both CELF-5 and SLAM results)
2. CELF-5 Analysis (Core Language + each index: 2-3 sentences per index)
3. SLAM Story-by-Story Analysis (2-3 sentences per story, including what was measured, findings, and clinical implications)
4. Narrative Abilities Analysis (5-dimension summary: Narrative Structure, Complex Clauses, Inferencing, Pragmatic, Theory of Mind)
5. Clinical Profile (integrated strengths and weaknesses, 2-3 sentences)
6. Recommendations (3 bullet points, highest priority first)
7. Limitations and Caveats (1-2 sentences)

Requirements:
- Write entirely in English
- Clinical but compassionate tone
- Integrate both CELF-5 and SLAM findings — do not treat them in isolation
- The weakest areas MUST be highlighted as requiring follow-up
- The recommendations must be specific and actionable
- Do NOT make up any additional data beyond what is provided above
- End with: Report generated: %s | This report must be reviewed and signed by the evaluating clinician.
",
        pat$name[1], age_years, gender_en_val,
        celf5_data$assessment$date_str[1], date_str, age_group,
        paste(celf5_lines_en, collapse = "\n"),
        subtest_details_en, narr_details_en,
        avg_wf_ss,  ss_to_range(avg_wf_ss)[1],  ss_to_range(avg_wf_ss)[2],
        avg_gfa_ss, ss_to_range(avg_gfa_ss)[1], ss_to_range(avg_gfa_ss)[2],
        strongest_wf_en, weakest_wf_en, strongest_gfa_en, weakest_gfa_en,
        date_str
      )
    }

  } else {
    # Neither SLAM nor CELF-5 data available
    if (lang == "zh") {
      prompt <- sprintf(
        "You are a clinical child psychologist.
Generate a professional clinical assessment report in Chinese (Simplified Chinese) stating that no assessment data is available for this patient.
Write in formal clinical language, 3rd person, as if signing an official report.
Requirements:
- Write entirely in Simplified Chinese (简体中文)
- State clearly that no CELF-5 or SLAM assessment data was found for this patient
- End with: 报告生成时间: %s | 本报告需经主试评估师审核签字
",
        date_str
      )
    } else {
      prompt <- sprintf(
        "You are a clinical child psychologist.
Generate a professional clinical assessment report in English stating that no assessment data is available for this patient.
Write in formal clinical language, 3rd person, as if signing an official report.
Requirements:
- Write entirely in English
- State clearly that no CELF-5 or SLAM assessment data was found for this patient
- End with: Report generated: %s | This report must be reviewed and signed by the evaluating clinician.
",
        date_str
      )
    }
  }

  # ── Call MiniMax ────────────────────────────────────────────
  # ── Call MiniMax ────────────────────────────────────────────
  raw_narrative <- .call_minimax(prompt, max_tokens = 4000L)

  # Post-process: find first Chinese character as true start
  narrative <- raw_narrative
  narrative <- sub("^.*?\n\n(?=[一-鿿])", "", narrative, perl = TRUE)
  if (grepl("^You are a", narrative, ignore.case = TRUE)) {
    chinese_start <- regexpr("[一-鿿]", narrative)[1]
    if (chinese_start > 1) {
      narrative <- substr(narrative, chinese_start, nchar(narrative))
    }
  }
  narrative
}

# ─────────────────────────────────────────────────────────────
# generate_slam_report_en — English version
# ─────────────────────────────────────────────────────────────
generate_slam_report_en <- function(student_id, assessment_id) {
  con <- get_con()
  on.exit(dbDisconnect(con), add = TRUE)

  pat <- dbGetQuery(con, sprintf(
    "SELECT * FROM patients WHERE id = %d", student_id
  ))
  if (nrow(pat) == 0) stop("Patient not found: ", student_id)

  ass <- dbGetQuery(con, sprintf(
    "SELECT *, strftime('%%Y-%%m-%%d', assessment_date) as date_str
     FROM assessments WHERE id = %d", assessment_id
  ))
  if (nrow(ass) == 0) stop("Assessment not found: ", assessment_id)

  age_years <- ass$age_years[1]
  age_group <- ass$age_group[1]
  date_str  <- ass$date_str[1]

  scores <- dbGetQuery(con, sprintf(
    "SELECT subtest, raw_score, scaled_score, percentile, story_id
     FROM subtest_scores
     WHERE assessment_id = %d",
    assessment_id
  ))

  narr_scores <- dbGetQuery(con, sprintf(
    "SELECT story_id, dimension, score
     FROM narrative_scores
     WHERE assessment_id = %d",
    assessment_id
  ))

  story_ids  <- c("BaseballTroubles", "TheBestTurkey", "GirlWhoLovedHorses", "WallaceAndBatty")
  story_keys <- c("baseball_troubles", "the_best_turkey", "the_girl_who_loved_horses", "wallace_and_batty")

  story_summary <- map_dfr(seq_along(story_ids), function(i) {
    sid <- story_ids[i]
    sk  <- story_keys[i]
    sinfo <- STORIES[[sk]]

    wf_row  <- scores %>% filter(grepl(paste0(sid, "_WordFinding"), subtest))
    gfa_row <- scores %>% filter(grepl(paste0(sid, "_GFA"), subtest))
    narr_row <- scores %>% filter(grepl(paste0(sid, "_Narrative"), subtest))

    wf_raw   <- wf_row$raw_score[1]
    gfa_raw  <- gfa_row$raw_score[1]
    narr_raw <- if (nrow(narr_row) > 0) narr_row$raw_score[1] else NA_integer_

    wf_ss  <- get_slam_ss(wf_raw,  "word_finding", age_years)
    gfa_ss <- get_slam_ss(gfa_raw, "gfa",          age_years)

    wf_pr  <- ss_to_pr(wf_ss)
    gfa_pr <- ss_to_pr(gfa_ss)

    tibble(
      story_id      = sid,
      story_key     = sk,
      story_name    = sinfo$name,
      story_name_zh = sinfo$name_zh,
      age_range     = sinfo$age_range,
      wf_raw        = wf_raw %||% NA_integer_,
      wf_max        = sinfo$wf_max,
      wf_ss         = wf_ss %||% NA_integer_,
      wf_pr         = wf_pr %||% NA_real_,
      gfa_raw       = gfa_raw %||% NA_integer_,
      gfa_max       = sinfo$gfa_max,
      gfa_ss        = gfa_ss %||% NA_integer_,
      gfa_pr        = gfa_pr %||% NA_real_,
      narr_raw      = narr_raw %||% NA_integer_,
      narr_max      = sinfo$narrative_max
    )
  })

  wf_data  <- story_summary %>% filter(!is.na(wf_ss))  %>% arrange(desc(wf_ss))
  gfa_data <- story_summary %>% filter(!is.na(gfa_ss)) %>% arrange(desc(gfa_ss))

  strongest_wf  <- wf_data$story_name[1]
  weakest_wf    <- wf_data$story_name[nrow(wf_data)]
  strongest_gfa <- gfa_data$story_name[1]
  weakest_gfa   <- gfa_data$story_name[nrow(gfa_data)]

  avg_wf_ss  <- mean(story_summary$wf_ss,  na.rm = TRUE)
  avg_gfa_ss <- mean(story_summary$gfa_ss, na.rm = TRUE)

  subtest_lines <- map_chr(seq_len(nrow(story_summary)), function(i) {
    r <- story_summary[i, ]
    sprintf(
      "%s [Word Finding]: raw %d/%d, SS=%d, PR=%.0f; %s [GFA]: raw %d/%d, SS=%d, PR=%.0f; Narrative: %d/%d",
      r$story_name, r$wf_raw, r$wf_max, r$wf_ss, r$wf_pr,
      r$story_name, r$gfa_raw, r$gfa_max, r$gfa_ss, r$gfa_pr,
      r$narr_raw %||% 0L, r$narr_max
    )
  })
  subtest_details <- paste(subtest_lines, collapse = "\n")

  gender_en <- if (is.na(pat$gender[1]) || pat$gender[1] == "") {
    "Not recorded"
  } else if (pat$gender[1] == "M") {
    "Male"
  } else if (pat$gender[1] == "F") {
    "Female"
  } else {
    pat$gender[1]
  }

  prompt <- sprintf(
    "You are a clinical child psychologist specializing in narrative language assessment.
Generate a professional clinical assessment report in English for the following SLAM (Structured Language Assessment Measures) results.
Write in formal clinical language, 3rd person, as if signing an official report.

PATIENT INFO:
- Name: %s
- Age: %d years
- Gender: %s
- Assessment Date: %s
- Age Group: %s

SUBTEST RESULTS:
%s

OVERALL SUMMARY:
- Average Word Finding SS: %.0f (68%% CI: %.0f–%.0f)
- Average GFA SS: %.0f (68%% CI: %.0f–%.0f)
- Strongest story (Word Finding): %s
- Weakest story (Word Finding): %s
- Strongest story (GFA): %s
- Weakest story (GFA): %s

Write a comprehensive clinical narrative report with these sections (in English):
1. Overall Summary (2-3 sentences of overall clinical impression)
2. Story-by-Story Results Analysis (each story: what it measures + clinical finding + implication, 2-3 sentences)
3. Narrative Abilities Analysis (Free Narrative 5 dimensions: Structure, Complex Clauses, Inferencing, Pragmatic, Theory of Mind)
4. Clinical Profile (strengths/weaknesses summary, 2-3 sentences)
5. Recommendations (3 bullet points, highest priority first)
6. Limitations and Cautions (1-2 sentences)

Requirements:
- Write in English
- Clinical but compassionate tone
- Each story section must include what it measures, the finding, and clinical implication
- The weakest areas MUST be highlighted as requiring follow-up
- Recommendations must be specific and actionable
- Do NOT make up any additional data beyond what is provided above
- End with: Report generated: %s | This report must be reviewed and signed by the examining clinician.
",
    pat$name[1],
    age_years,
    gender_en,
    date_str,
    age_group,
    subtest_details,
    avg_wf_ss,
    ss_to_range(avg_wf_ss)[1], ss_to_range(avg_wf_ss)[2],
    avg_gfa_ss,
    ss_to_range(avg_gfa_ss)[1], ss_to_range(avg_gfa_ss)[2],
    strongest_wf, weakest_wf,
    strongest_gfa, weakest_gfa,
    celf5_section,
    date_str
  )

  raw_narrative <- .call_minimax(prompt, max_tokens = 4000L)
  narrative <- raw_narrative
  narrative <- sub("^.*?\n\n(?=[一-鿿])", "", narrative, perl = TRUE)
  if (grepl("^You are a", narrative, ignore.case = TRUE)) {
    chinese_start <- regexpr("[一-鿿]", narrative)[1]
    if (chinese_start > 1) {
      narrative <- substr(narrative, chinese_start, nchar(narrative))
    }
  }
  narrative
}

# ─────────────────────────────────────────────────────────────
# get_slam_summary — Returns a data frame summary without AI
# ─────────────────────────────────────────────────────────────
get_slam_summary <- function(student_id, assessment_id) {
  con <- get_con()
  on.exit(dbDisconnect(con), add = TRUE)

  pat <- dbGetQuery(con, sprintf(
    "SELECT * FROM patients WHERE id = %d", student_id
  ))
  ass <- dbGetQuery(con, sprintf(
    "SELECT *, strftime('%%Y-%%m-%%d', assessment_date) as date_str
     FROM assessments WHERE id = %d", assessment_id
  ))

  age_years <- ass$age_years[1]

  scores <- dbGetQuery(con, sprintf(
    "SELECT subtest, raw_score, scaled_score, percentile, story_id
     FROM subtest_scores
     WHERE assessment_id = %d",
    assessment_id
  ))

  story_ids  <- c("BaseballTroubles", "TheBestTurkey", "GirlWhoLovedHorses", "WallaceAndBatty")
  story_keys <- c("baseball_troubles", "the_best_turkey", "the_girl_who_loved_horses", "wallace_and_batty")

  story_summary <- map_dfr(seq_along(story_ids), function(i) {
    sid <- story_ids[i]
    sk  <- story_keys[i]
    sinfo <- STORIES[[sk]]

    wf_row   <- scores %>% filter(grepl(paste0(sid, "_WordFinding"), subtest))
    gfa_row  <- scores %>% filter(grepl(paste0(sid, "_GFA"), subtest))
    narr_row <- scores %>% filter(grepl(paste0(sid, "_Narrative"), subtest))

    wf_raw   <- wf_row$raw_score[1]
    gfa_raw  <- gfa_row$raw_score[1]
    narr_raw <- if (nrow(narr_row) > 0) narr_row$raw_score[1] else NA_integer_

    wf_ss  <- get_slam_ss(wf_raw,  "word_finding", age_years)
    gfa_ss <- get_slam_ss(gfa_raw, "gfa",          age_years)

    tibble(
      story_id       = sid,
      story_name_zh  = sinfo$name_zh,
      age_range      = sinfo$age_range,
      wf_raw         = wf_raw %||% NA_integer_,
      wf_max         = sinfo$wf_max,
      wf_ss          = wf_ss %||% NA_integer_,
      wf_pr          = ss_to_pr(wf_ss) %||% NA_real_,
      gfa_raw        = gfa_raw %||% NA_integer_,
      gfa_max        = sinfo$gfa_max,
      gfa_ss         = gfa_ss %||% NA_integer_,
      gfa_pr         = ss_to_pr(gfa_ss) %||% NA_real_,
      narr_raw       = narr_raw %||% NA_integer_,
      narr_max       = sinfo$narrative_max
    )
  })

  list(
    patient    = pat,
    assessment = ass,
    stories    = story_summary,
    avg_wf_ss  = mean(story_summary$wf_ss,  na.rm = TRUE),
    avg_gfa_ss = mean(story_summary$gfa_ss, na.rm = TRUE)
  )
}
