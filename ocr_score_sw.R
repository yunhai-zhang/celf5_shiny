# ─────────────────────────────────────────────────────────────────────────────
# ocr_score_sw.R — CELF-5 Structured Writing: OCR + LLM AI Scoring Pipeline
# ─────────────────────────────────────────────────────────────────────────────
# Deps: tesseract (apt), pytesseract (pip), httr, jsonlite
# Usage: source("ocr_score_sw.R"); ocr_and_score(image_path, age_group)
# ─────────────────────────────────────────────────────────────────────────────

# 1. OCR ────────────────────────────────────────────────────────────────────

#' Run Tesseract OCR on an image file
#' @param image_path Local path to image (JPEG/PNG)
#' @param lang Language code: "eng", "chi_sim", "eng+chi_sim"
#' @return Extracted text (raw string)
run_ocr <- function(image_path, lang = "eng+chi_sim") {
  cmd <- "tesseract"
  args <- c(shQuote(image_path), "stdout", "-l", lang)
  result <- system2(cmd, args = args, stdout = TRUE, stderr = FALSE)
  paste(result, collapse = "\n")
}

#' Clean OCR output — remove tesseract artifacts and empty lines
clean_ocr_text <- function(raw_text) {
  # Remove confidence lines and artifact patterns
  lines <- strsplit(raw_text, "\n")[[1]]
  lines <- trimws(lines)
  # Remove lines that are mostly confidence scores or empty
  lines <- lines[nchar(lines) > 0]
  # Remove lines that look like tesseract debug output
  lines <- lines[!grepl("^(Warning|Page\\s+\\d+|Detected\\s+\\d+|Boxane)", lines, ignore.case = TRUE)]
  paste(lines, collapse = "\n")
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

# 3. LLM Prompt Builder ───────────────────────────────────────────────────

build_sw_scoring_prompt <- function(ocr_text, age_group, rubric) {
  paste0(
    "You are a CELF-5 Structured Writing (SW) scorer.\n",
    "Child's age group: ", rubric$prompt_desc, "\n",
    "Child's handwritten response (OCR extracted):\n",
    "----------------------------------------\n",
    ocr_text, "\n",
    "----------------------------------------\n",
    "CELF-5 SW Scoring Rubric:\n",
    "- Structure (complete sentences): 0 or 1 per sentence\n",
    "- Grammar accuracy: 0-", rubric$grammar_max, " per sentence\n",
    "- Organization (whole piece): 0-", rubric$org_max, "\n",
    "- Mechanics (spelling/capitalization/punctuation): 0-", rubric$mech_max, "\n",
    "Important: The half-written sentence at the top of the page is a STIMULUS prompt (not scored).\n",
    "Only score the child's self-written sentences.\n\n",
    "Your response must begin with { and end with } — no other text. Only output valid JSON.\n",
    "{\n",
    "  \"structure\": {\"score\": N, \"comment\": \"...\"},\n",
    "  \"grammar\":   {\"score\": N, \"comment\": \"...\"},\n",
    "  \"organization\": {\"score\": N, \"comment\": \"...\"},\n",
    "  \"mechanics\":    {\"score\": N, \"comment\": \"...\"},\n",
    "  \"summary\":      \"2-3 sentence clinical summary\"\n",
    "}\n",
    "Score ranges: structure=0-", rubric$struct_max * rubric$n_sentences,
    ", grammar=0-", rubric$grammar_max * rubric$n_sentences,
    ", organization=0-", rubric$org_max,
    ", mechanics=0-", rubric$mech_max,
    ", total=0-", (rubric$struct_max * rubric$n_sentences) + (rubric$grammar_max * rubric$n_sentences) + rubric$org_max + rubric$mech_max
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

#' OCR + AI score a child's SW handwritten response
#' @param image_path Path to photo of handwritten work
#' @param age_group Age group key: "age_8", "age_9_10", "age_11_12", "age_13_21"
#' @return Named list with: ocr_text, structure, grammar, organization, mechanics, total_score, summary, raw_json
ocr_and_score <- function(image_path, age_group = "age_9_10") {
  rubric <- SW_RUBRIC[[age_group]]
  if (is.null(rubric)) stop("Unknown age_group: ", age_group)

  # Step 1: OCR
  raw_ocr   <- run_ocr(image_path)
  ocr_text  <- clean_ocr_text(raw_ocr)

  if (nchar(ocr_text) < 5) {
    return(list(
      ocr_text    = "",
      structure   = list(score = NA_integer_, comment = "OCR failed to extract text"),
      grammar     = list(score = NA_integer_, comment = ""),
      organization= list(score = NA_integer_, comment = ""),
      mechanics   = list(score = NA_integer_, comment = ""),
      total_score = NA_integer_,
      summary     = "无法识别手写内容，请手动输入",
      raw_json    = "{}",
      error       = "OCR returned empty/too short text"
    ))
  }

  # Step 2: Build prompt + call LLM
  prompt    <- build_sw_scoring_prompt(ocr_text, age_group, rubric)
  raw_json  <- .call_minimax(prompt, max_tokens = 2000L)

  # Step 3: Parse JSON — the actual JSON is always at the END of the response
  # (after all <think>...思考 thinking). Strategy: scan RIGHT-TO-LEFT from the end,
  # find the last "}" (end of JSON), then walk backward to find matching "{".
  n <- nchar(raw_json)
  # Find last "}"
  last_close <- NA
  for (i in n:1) {
    if (substr(raw_json, i, i) == "}") { last_close <- i; break }
  }
  if (is.na(last_close)) stop("No } found in response")

  # Walk backward from last_close to find matching "{"
  depth <- 0; json_end <- last_close
  for (i in last_close:1) {
    ch <- substr(raw_json, i, i)
    if (ch == "}") depth <- depth + 1
    else if (ch == "{") {
      depth <- depth - 1
      if (depth == 0) { json_start <- i; break }
    }
  }
  if (is.na(json_start)) stop("No matching { found for JSON")
  json_str <- substr(raw_json, json_start, json_end)

  parsed <- tryCatch(
    jsonlite::fromJSON(json_str, simplifyVector = FALSE),
    error = function(e) NULL
  )

  if (is.null(parsed)) {
    # Fallback: try to extract numbers via regex
    return(list(
      ocr_text     = ocr_text,
      structure    = list(score = NA_integer_, comment = "解析失败，请手动评分"),
      grammar      = list(score = NA_integer_, comment = ""),
      organization = list(score = NA_integer_, comment = ""),
      mechanics    = list(score = NA_integer_, comment = ""),
      total_score  = NA_integer_,
      summary      = paste0("LLM返回格式异常，已识别文本：", substr(ocr_text, 1, 100)),
      raw_json     = raw_json,
      error        = "JSON parse failed"
    ))
  }

  # Step 4: Validate and cap scores within rubric limits
  cap <- function(n, max) pmin(as.integer(n), as.integer(max))

  structure_max   <- rubric$struct_max   * rubric$n_sentences
  grammar_max     <- rubric$grammar_max   * rubric$n_sentences

  list(
    ocr_text     = ocr_text,
    structure    = list(
      score   = cap(parsed$structure$score, structure_max),
      comment = as.character(parsed$structure$comment %||% "")
    ),
    grammar      = list(
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
      cap(parsed$structure$score   %||% 0, structure_max),
      cap(parsed$grammar$score     %||% 0, grammar_max),
      cap(parsed$organization$score%||% 0, rubric$org_max),
      cap(parsed$mechanics$score   %||% 0, rubric$mech_max)
    )),
    summary      = as.character(parsed$summary %||% ""),
    raw_json     = raw_json,
    error        = NULL
  )
}
