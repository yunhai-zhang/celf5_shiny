# ─────────────────────────────────────────────────────────────────────────────
# ocr_score_sw.R — CELF-5 Structured Writing: Vision AI Scoring Pipeline
# ─────────────────────────────────────────────────────────────────────────────
# Deps: httr, jsonlite, base64enc
# Usage: source("ocr_score_sw.R"); vision_and_score(image_path, age_group)
# Strategy: MiniMax VLM (vision) instead of Tesseract OCR — much better at
# reading children's handwriting. Falls back gracefully on error.
# ─────────────────────────────────────────────────────────────────────────────

# 1. Vision (MiniMax VLM) — replaces old Tesseract OCR ───────────────────────

#' Call MiniMax VLM to analyze an image + prompt
#' @param image_path Local path to image (JPEG/PNG)
#' @param prompt_text Prompt to send to the vision model
#' @return Raw text response from VLM
.call_minimax_vlm <- function(image_path, prompt_text) {
  api_key <- .read_minimax_key()
  url     <- "https://api.minimaxi.com/v1/coding_plan/vlm"

  b64 <- base64enc::base64encode(image_path)
  ext <- tolower(tools::file_ext(image_path))
  if (ext == "jpg") ext <- "jpeg"
  data_url <- sprintf("data:image/%s;base64,%s", ext, b64)

  body <- list(
    prompt    = prompt_text,
    image_url = data_url
  )

  resp <- httr::POST(
    url,
    httr::add_headers(
      `Content-Type` = "application/json",
      `Authorization` = paste0("Bearer ", api_key)
    ),
    body  = body,
    httr::content_type_json(),
    encode = "json"
  )

  txt <- httr::content(resp, as = "text", encoding = "UTF-8")
  if (resp$status_code != 200) {
    stop(sprintf("VLM API error %d: %s", resp$status_code, substr(txt, 1, 300)))
  }

  parsed <- jsonlite::fromJSON(txt, simplifyVector = FALSE)
  content <- parsed$content
  if (is.null(content) || !nzchar(content)) {
    stop(sprintf("No content in VLM response: %s", substr(txt, 1, 300)))
  }
  content
}

# Legacy OCR stubs (kept for compatibility with existing code) ──────────────

#' Run Tesseract OCR on an image file (stub — VLM now handles recognition)
#' @param image_path Local path to image (JPEG/PNG)
#' @param lang Language code — ignored, kept for API compat
#' @return Extracted text (raw string)
run_ocr <- function(image_path, lang = "eng+chi_sim") {
  NA_character_
}

#' Clean OCR output (stub — no OCR output to clean)
clean_ocr_text <- function(raw_text) {
  NA_character_
}

# 2. SW Scoring Rubric (mirrors global.R, self-contained) ──────────────────

SW_RUBRIC <- list(
  age_8 = list(
    struct_max  = 1L,
    grammar_max = 3L,
    org_max     = 3L,
    mech_max    = 3L,
    n_sentences = 2L,
    prompt_desc = "8岁（2个句子：S1 + S+）"
  ),
  age_9_10 = list(
    struct_max  = 1L,
    grammar_max = 3L,
    org_max     = 3L,
    mech_max    = 3L,
    n_sentences = 3L,
    prompt_desc = "9-10岁（3个句子：S1 + S2 + S+）"
  ),
  age_11_12 = list(
    struct_max  = 1L,
    grammar_max = 3L,
    org_max     = 4L,
    mech_max    = 3L,
    n_sentences = 3L,
    prompt_desc = "11-12岁（3个句子：S1 + S2 + S+）"
  ),
  age_13_21 = list(
    struct_max  = 1L,
    grammar_max = 1L,
    org_max     = 5L,
    mech_max    = 3L,
    n_sentences = 5L,
    prompt_desc = "13-21岁（5个句子：S1 + S2 + S3 + S4 + S+）"
  )
)

# 3. Vision Prompt Builder ─────────────────────────────────────────────────

build_sw_vision_prompt <- function(rubric) {
  paste0(
    "You are a clinical child psychologist specializing in language assessment.\n",
    "You will receive an image of a child handwriting sample from CELF-5 Structured Writing test.\n",
    "Your task:\n",
    "1. Read ALL handwritten text in the image carefully — be thorough, children's handwriting can be messy\n",
    "2. Score according to the rubric below\n",
    "3. Return ONLY valid JSON (no thinking tags, no extra text, no markdown code blocks)\n\n",
    "Return EXACTLY this JSON structure, starting with { and ending with }:\n",
    '{"recognized_text": "the complete text you read from the handwriting (be precise, capture every word)", ',
    '"structure": {"score": N, "comment": "brief comment about sentence completeness and logical flow"}, ',
    '"grammar": {"score": N, "comment": "brief comment about sentence structure and word order"}, ',
    '"organization": {"score": N, "comment": "brief comment about overall piece organization"}, ',
    '"mechanics": {"score": N, "comment": "brief comment about spelling, capitalization, punctuation"}, ',
    '"summary": "2 sentence clinical summary in Chinese"}\n\n',
    "Scoring rubric (age group: ", rubric$prompt_desc, "):\n",
    "- Structure (0-", rubric$struct_max * rubric$n_sentences, "): Complete sentences, logical flow between sentences\n",
    "- Grammar (0-", rubric$grammar_max * rubric$n_sentences, "): Sentence structure, verb usage, word order accuracy\n",
    "- Organization (0-", rubric$org_max, "): Overall piece organization and coherence\n",
    "- Mechanics (0-", rubric$mech_max, "): Spelling accuracy, capitalization, punctuation\n\n",
    "IMPORTANT:\n",
    "- recognized_text must capture EVERY word/sentence the child wrote\n",
    "- Do NOT score the stimulus/prompt text at the top of the page — only the child's own writing\n",
    "- Return ONLY the raw JSON, no markdown formatting\n"
  )
}

# 4. LLM API Call ──────────────────────────────────────────────────────────

.read_minimax_key <- function() {
  key <- Sys.getenv("MINIMAX_CN_API_KEY")
  if (nzchar(key)) return(key)
  # fallback: read from ~/.hermes/.env (where Hermes stores it)
  # NOTE: shiny-server may run as root so ~=/root — try USER home as well
  possible_homes <- unique(c(
    Sys.getenv("HOME"),
    Sys.getenv("USER"),
    "/home/yzhang"
  ))
  for (home in possible_homes) {
    env_file <- file.path(home, ".hermes", ".env")
    if (file.exists(env_file)) {
      lines <- readLines(env_file, warn = FALSE)
      pat   <- grep("^MINIMAX_CN_API_KEY=", lines, value = TRUE)
      if (length(pat) > 0) return(sub("^MINIMAX_CN_API_KEY=", "", pat[1]))
    }
  }
  stop("MINIMAX_CN_API_KEY not found in environment or ~/.hermes/.env")
}

#' Strip think/reasoning tags from MiniMax JSON responses
#' MiniMax models prepend <think>...</think> or 【思考】... blocks before JSON
.clean_json_tags <- function(raw_text) {
  # Remove <think>...</think> (English think blocks)
  cleaned <- gsub("<think>[\\s\\S]*?</think>", "", raw_text, perl = TRUE)
  # Remove 【思考】...【思考】 or 【思考】... blocks
  cleaned <- gsub("【思考】[\\s\\S]*?【思考】", "", cleaned, perl = TRUE)
  # Remove remaining 【思考】... singletons
  cleaned <- gsub("【思考】[\\s\\S]*?$", "", cleaned, perl = TRUE)
  # Remove leading whitespace / newlines before the first {
  cleaned <- gsub("^[\\s\\n]+", "", cleaned)
  trimmed <- trimws(cleaned)
  # If we still have no braces, return as-is
  if (!grepl("\\{", trimmed)) return(raw_text)
  trimmed
}

.call_minimax <- function(prompt, max_tokens = 600L) {
  api_key <- .read_minimax_key()
  url <- "https://api.minimaxi.com/v1/chat/completions"
  body <- list(
    model    = "MiniMax-M2.7",
    messages = list(list(role = "user", content = prompt)),
    max_tokens = max_tokens,
    temperature = 0.3
  )
  resp <- httr::POST(
    url,
    httr::add_headers(`Content-Type` = "application/json", `Authorization` = paste0("Bearer ", api_key)),
    body  = body,
    httr::content_type_json(),
    encode = "json"
  )
  txt <- httr::content(resp, as = "text", encoding = "UTF-8")
  if (resp$status_code != 200) {
    stop(sprintf("API error %d: %s", resp$status_code, substr(txt, 1, 300)))
  }
  parsed   <- jsonlite::fromJSON(txt, simplifyVector = FALSE)
  .clean_json_tags(parsed$choices[[1]]$message$content)
}

# 5. Main Pipeline ─────────────────────────────────────────────────────────

# Helper: null-coalescing
`%||%` <- function(a, b) if (is.null(a)) b else a

#' Vision AI score a child's SW handwritten response (replaces old OCR pipeline)
#' @param image_path Path to photo of handwritten work
#' @param age_group Age group key: "age_8", "age_9_10", "age_11_12", "age_13_21"
#' @return Named list with: recognized_text, structure, grammar, organization,
#'         mechanics, total_score, summary, raw_json, error
vision_and_score <- function(image_path, age_group = "age_9_10") {
  rubric <- SW_RUBRIC[[age_group]]
  if (is.null(rubric)) stop("Unknown age_group: ", age_group)

  # Check file exists
  if (!file.exists(image_path)) {
    return(list(
      recognized_text = "",
      structure   = list(score = NA_integer_, comment = "图片文件未找到"),
      grammar     = list(score = NA_integer_, comment = ""),
      organization= list(score = NA_integer_, comment = ""),
      mechanics   = list(score = NA_integer_, comment = ""),
      total_score = NA_integer_,
      summary     = "图片文件未找到，请手动评分",
      raw_json    = "{}",
      error       = "Image file not found"
    ))
  }

  # Step 1: Call MiniMax VLM
  prompt  <- build_sw_vision_prompt(rubric)
  content <- .call_minimax_vlm(image_path, prompt)

  # Step 2: Extract JSON from VLM content
  # VLM may return: (a) plain JSON, (b) markdown-wrapped ```json...```,
  # or (c) other text. Strip markdown code fences first.
  content_clean <- gsub("```json\\s*", "", content, fixed = TRUE)
  content_clean <- gsub("```", "", content_clean, fixed = TRUE)

  # Find JSON block — first { and matching last }
  first_brace <- which(charToRaw(content_clean) == charToRaw("{"))[1]
  last_brace  <- which(rev(charToRaw(content_clean)) == charToRaw("}"))[1]

  json_str <- NULL
  if (!is.na(first_brace) && !is.na(last_brace)) {
    n <- nchar(content_clean)
    last_pos <- n - last_brace + 1
    if (last_pos >= first_brace) {
      json_str <- substr(content_clean, first_brace, last_pos)
    }
  }

  if (is.null(json_str) || length(grep("{", json_str, fixed = TRUE)) == 0) {
    return(list(
      recognized_text = "",
      structure   = list(score = NA_integer_, comment = "VLM返回格式异常，请手动评分"),
      grammar     = list(score = NA_integer_, comment = ""),
      organization= list(score = NA_integer_, comment = ""),
      mechanics   = list(score = NA_integer_, comment = ""),
      total_score = NA_integer_,
      summary     = paste0("VLM未返回有效JSON，已识别内容：", substr(content, 1, 100)),
      raw_json    = content,
      error       = "No JSON found in VLM response"
    ))
  }

  # Step 3: Parse JSON
  parsed <- tryCatch(
    jsonlite::fromJSON(json_str, simplifyVector = FALSE, check.names = FALSE),
    error = function(e) NULL
  )

  if (is.null(parsed)) {
    return(list(
      recognized_text = "",
      structure   = list(score = NA_integer_, comment = "JSON解析失败，请手动评分"),
      grammar     = list(score = NA_integer_, comment = ""),
      organization= list(score = NA_integer_, comment = ""),
      mechanics   = list(score = NA_integer_, comment = ""),
      total_score = NA_integer_,
      summary     = paste0("JSON解析失败，已识别内容：", substr(content, 1, 100)),
      raw_json    = content,
      error       = "JSON parse failed"
    ))
  }

  # Step 4: Validate and cap scores within rubric limits
  cap <- function(n, max_val) pmin(as.integer(n), as.integer(max_val))

  structure_max <- rubric$struct_max * rubric$n_sentences
  grammar_max    <- rubric$grammar_max * rubric$n_sentences

  recognized_text <- as.character(parsed$recognized_text %||% "")

  list(
    recognized_text = recognized_text,
    structure   = list(
      score   = cap(parsed$structure$score, structure_max),
      comment = as.character(parsed$structure$comment %||% "")
    ),
    grammar     = list(
      score   = cap(parsed$grammar$score, grammar_max),
      comment = as.character(parsed$grammar$comment %||% "")
    ),
    organization = list(
      score   = cap(parsed$organization$score, rubric$org_max),
      comment = as.character(parsed$organization$comment %||% "")
    ),
    mechanics    = list(
      score   = cap(parsed$mechanics$score, rubric$mech_max),
      comment = as.character(parsed$mechanics$comment %||% "")
    ),
    total_score  = sum(c(
      cap(parsed$structure$score    %||% 0, structure_max),
      cap(parsed$grammar$score      %||% 0, grammar_max),
      cap(parsed$organization$score%||% 0, rubric$org_max),
      cap(parsed$mechanics$score    %||% 0, rubric$mech_max)
    )),
    summary  = as.character(parsed$summary %||% ""),
    raw_json = content,
    error    = NULL
  )
}

#' Legacy wrapper — ocr_and_score now delegates to vision_and_score
#' Kept for backwards compatibility with app.R
ocr_and_score <- function(image_path, age_group = "age_9_10") {
  vision_and_score(image_path, age_group)
}
