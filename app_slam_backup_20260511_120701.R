# app_slam.R — SLAM Narrative Assessment Tool
# Stories: Baseball Troubles / The Best Turkey / The Girl Who Loved Horses / Wallace and Batty
# Scoring: Word Finding (raw→standardized) + GFA (0-2 rubric) + Free Narrative
# Data saved to celf5_assessments.db

library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(lubridate)
library(RSQLite)
library(glue)
library(stringr)

# Source shared DB helpers
source("/home/yzhang/clawfiles/celf5_shiny/global.R")

DB_PATH <- "/home/yzhang/clawfiles/celf5_shiny/celf5_assessments.db"
get_con <- function() dbConnect(SQLite(), DB_PATH)

# Find extracted story image
find_story_image <- function(story_id, page_num, img_num = 1) {
  img_dir <- "story_images"
  candidates <- c(
    sprintf("%s_p%d_img%d.png", story_id, page_num, img_num),
    sprintf("%s_p%d_img1.png", story_id, page_num)
  )
  for (c in candidates) {
    f <- file.path(img_dir, c)
    if (file.exists(f)) return(f)
  }
  NULL
}

# ─────────────────────────────────────────────────────────────
# 0. Constants & Colors
# ─────────────────────────────────────────────────────────────
SLAM_BLUE   <- "#1B3A6B"
SLAM_GOLD   <- "#C8A951"
SLAM_LIGHT  <- "#F0F4FA"
SLAM_GRAY   <- "#6B7280"
DB_PATH     <- "/home/yzhang/clawfiles/celf5_shiny/celf5_assessments.db"

get_con <- function() dbConnect(SQLite(), DB_PATH)

# ─────────────────────────────────────────────────────────────
# 1. Story Metadata (from slam_content_audit.md)
# ─────────────────────────────────────────────────────────────
STORIES <- list(
  baseball_troubles = list(
    id   = "baseball_troubles",
    name = "Baseball Troubles",
    name_zh = "棒球烦恼",
    age_range = "13-17岁",
    n_images = 6,
    pdf_path = "/tmp/slam_extract/SLAM/SLAM/SLAM sets/Junior High to High School SLAM/1. SLAM Baseball Troubles_English.pdf",
    gfa_path = "/tmp/slam_extract/SLAM/SLAM/SLAM sets/Junior High to High School SLAM/1. GFA - Baseball Troubles.pdf",
    synopsis = "两个男孩打棒球时，球打破了一扇窗户。他们冤枉一个戴着耳机的女孩。\nTwo boys are playing baseball. A ball breaks a window. They blame a girl wearing headphones.",
    word_finding = tibble(
      item   = 1:6,
      prompt_en = c(
        "What is this?",           # baseball
        "What is this?",           # bat
        "What is this?",           # window
        "What is this?",           # boy
        "What is this?",           # headphones
        "What is this?"            # glove
      ),
      prompt_zh = c(
        "这是什么？", # 棒球
        "这是什么？", # 球棒
        "这是什么？", # 窗户
        "这是什么？", # 男孩
        "这是什么？", # 耳机
        "这是什么？"  # 手套
      ),
      acceptable_en = list(
        c("baseball","ball"),
        c("bat","baseball bat"),
        c("window","glass"),
        c("boy","kid","guy"),
        c("headphones","earphones","headphones"),
        c("glove","baseball glove","mitt")
      )
    ),
    gfa_items = tibble(
      item = 1:4,
      passage_en = c(
        "The two boys were playing ___ in the park when the ball went through Mr. Kim's window.",
        "The boys thought the ___ girl broke the window because she was listening to music.",
        "But the girl said she did not ___ the ball.",
        "Finally, the boys apologized and helped ___ the window."
      ),
      passage_zh = c(
        "两个男孩正在公园里打___，球飞进了金先生家的窗户。",
        "男孩们认为那个___女孩打破了窗户，因为她在听音乐。",
        "但女孩说她没有___球。",
        "最后，男孩们道歉并帮忙___窗户。"
      ),
      answers = list(c("baseball","ball"), c("headphone","headphones","girl","teen"), c("throw","hit","kick","break"), c("fix","repair","replace","pay for")),
      max_score = 2
    ),
    narrative_rubric = list(
      dimensions = c("叙事结构 Narrative Structure","复杂句 Complex Clauses",
                     "推论能力 Inferencing","语用 Pragmatic","理论心智 Theory of Mind"),
      max_per_dim = 2
    )
  ),

  the_best_turkey = list(
    id   = "the_best_turkey",
    name = "The Best Turkey",
    name_zh = "最好的火鸡",
    age_range = "10-14岁",
    n_images = 5,
    pdf_path = "/tmp/slam_extract/SLAM/SLAM/SLAM sets/Junior High to High School SLAM/2. SLAM The Ball Mystery_English.pdf",
    gfa_path = "/tmp/slam_extract/SLAM/SLAM/SLAM sets/Junior High to High School SLAM/2. GFA - The Ball Mystery.pdf",
    synopsis = "一个小男孩偷偷拿了厨房里的火鸡，正当他要和同学分享时，被人发现了。\nA boy secretly takes a turkey from the kitchen and gets caught as he tries to share it with classmates.",
    word_finding = tibble(
      item   = 1:5,
      prompt_en = c(
        "What is this?",
        "What is this?",
        "What is this?",
        "What is this?",
        "What is this?"
      ),
      prompt_zh = c(
        "这是什么？", # turkey
        "这是什么？", # kitchen
        "这是什么？", # plate
        "这是什么？", # boy
        "这是什么？"  # friend/classmate
      ),
      acceptable_en = list(
        c("turkey","bird"),
        c("kitchen","stove","counter"),
        c("plate","dish","tray"),
        c("boy","kid"),
        c("friend","classmate","girl","boy")
      )
    ),
    gfa_items = tibble(
      item = 1:4,
      passage_en = c(
        "Tommy wanted to share the special ___ with his friends at lunch.",
        "He put the turkey on a ___ and carried it to the cafeteria.",
        "When the cafeteria lady saw the turkey, she said, 'That belongs to ___!'",
        "Tommy felt ___ because he had taken something that was not his."
      ),
      passage_zh = c(
        "汤米想在午餐时和朋友们分享特别的___。",
        "他把火鸡放在___上，拿到自助餐厅。",
        "当自助餐厅的阿姨看到火鸡时说：'那是___的！'",
        "汤米感到___，因为他拿了不属于自己的东西。"
      ),
      answers = list(c("turkey","food","chicken"), c("plate","tray","dish","bowl"), c("the kitchen","Mrs. Lee","the school","someone else"), c("embarrassed","bad","sorry","guilty","sad")),
      max_score = 2
    ),
    narrative_rubric = list(
      dimensions = c("叙事结构 Narrative Structure","复杂句 Complex Clauses",
                     "推论能力 Inferencing","语用 Pragmatic","理论心智 Theory of Mind"),
      max_per_dim = 2
    )
  ),

  the_girl_who_loved_horses = list(
    id   = "the_girl_who_loved_horses",
    name = "The Girl Who Loved Horses",
    name_zh = "爱马的女孩",
    age_range = "13-17岁",
    n_images = 6,
    pdf_path = "/tmp/slam_extract/SLAM/SLAM/SLAM sets/Junior High to High School SLAM/3. SLAM Lost Cellphone_English.pdf",
    gfa_path = "/tmp/slam_extract/SLAM/SLAM/SLAM sets/Junior High to High School SLAM/3. GFA - Lost Cellphone.pdf",
    synopsis = "一个女孩在便利店结账时，不小心把手机忘在了柜台上，被后面的人拿走了。\nA girl forgets her cellphone on the convenience store counter while paying. Someone behind her takes it.",
    word_finding = tibble(
      item   = 1:6,
      prompt_en = c(
        "What is this?",
        "What is this?",
        "What is this?",
        "What is this?",
        "What is this?",
        "What is this?"
      ),
      prompt_zh = c(
        "这是什么？", # cellphone
        "这是什么？", # counter
        "这是什么？", # store/shop
        "这是什么？", # girl
        "这是什么？", # receipt
        "这是什么？"  # person/thief
      ),
      acceptable_en = list(
        c("cellphone","phone","mobile phone","smartphone"),
        c("counter","countertop","desk","checkout"),
        c("store","shop","convenience store","market"),
        c("girl","teen","teenager","woman"),
        c("receipt","receipt"),
        c("person","man","thief","guy","someone")
      )
    ),
    gfa_items = tibble(
      item = 1:4,
      passage_en = c(
        "The girl was paying for her items at the ___ when she put her phone on the counter.",
        "She was so busy ___ that she forgot her phone entirely.",
        "A person behind her saw the phone and ___ it instead of returning it.",
        "The girl realized her mistake only when she reached for her ___ in her pocket."
      ),
      passage_zh = c(
        "女孩在___结账时，把手机放在了柜台上。",
        "她太忙于___，完全忘记了手机。",
        "后面的人看到手机后，把它___而不是归还。",
        "女孩只在伸手去口袋里拿___时才意识到自己的错误。"
      ),
      answers = list(c("store","counter","checkout","register","shop"), c("paying","checking out","packing","looking","talking"), c("took","stole","grabbed","pocketed","kept"), c("phone","cellphone","hand","pocket","bag")),
      max_score = 2
    ),
    narrative_rubric = list(
      dimensions = c("叙事结构 Narrative Structure","复杂句 Complex Clauses",
                     "推论能力 Inferencing","语用 Pragmatic","理论心智 Theory of Mind"),
      max_per_dim = 2
    )
  ),

  wallace_and_batty = list(
    id   = "wallace_and_batty",
    name = "Wallace and Batty",
    name_zh = "华莱士与巴蒂",
    age_range = "7-14岁",
    n_images = 5,
    pdf_path = "/tmp/slam_extract/SLAM/SLAM/SLAM sets/Junior High to High School SLAM/4. SLAM Kittens Love Milk Cards (English).pdf",
    gfa_path = NULL,
    synopsis = "一只名叫Wallace的小狗和一只名叫Batty的小猫之间的故事。\nA story about a dog named Wallace and a cat named Batty.",
    word_finding = tibble(
      item   = 1:5,
      prompt_en = c(
        "What is this?",
        "What is this?",
        "What is this?",
        "What is this?",
        "What is this?"
      ),
      prompt_zh = c(
        "这是什么？", # dog/Wallace
        "这是什么？", # cat/Batty
        "这是什么？", # milk/bowl
        "这是什么？", # stairs/steps
        "这是什么？"  # bag/box
      ),
      acceptable_en = list(
        c("dog","puppy","canine","doggy"),
        c("cat","kitten","feline","kitty"),
        c("milk","bowl","food","water","dish"),
        c("stairs","steps","staircase","ladder"),
        c("bag","sack","backpack","purse")
      )
    ),
    gfa_items = tibble(
      item = 1:4,
      passage_en = c(
        "Wallace the dog followed Batty the cat up the ___.",
        "Batty jumped into the woman's ___ and fell asleep.",
        "Wallace could not follow because the bag was too ___.",
        "The woman did not know Batty was in her ___ until she felt something move."
      ),
      passage_zh = c(
        "小狗华莱士跟着小猫巴蒂上了___。",
        "巴蒂跳进女人的___里睡着了。",
        "华莱士因为袋子太___而无法进去。",
        "女人直到感觉到有东西在___动才知道巴蒂在里面。"
      ),
      answers = list(c("stairs","steps","staircase"), c("bag","backpack","purse","sack","basket"), c("small","tight","tiny","little","narrow"), c("bag","backpack","purse","sack","basket")),
      max_score = 2
    ),
    narrative_rubric = list(
      dimensions = c("叙事结构 Narrative Structure","复杂句 Complex Clauses",
                     "推论能力 Inferencing","语用 Pragmatic","理论心智 Theory of Mind"),
      max_per_dim = 2
    )
  )
)

# ─────────────────────────────────────────────────────────────
# 2. SLAM Norms Table (age 7–17, simplified lookup)
#    Source: SLAM normative data (Columbia University Leaders Project)
# ─────────────────────────────────────────────────────────────
build_slam_norms <- function() {
  ages   <- rep(7:17, each = 4)
  raw    <- rep(c(0,5,10,15), 11)
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

get_slam_standard_score <- function(raw, type = c("word_finding","gfa"), age) {
  type <- match.arg(type)
  col  <- if (type == "word_finding") "std_word_finding" else "std_gfa"
  row  <- SLAM_NORMS %>% filter(.data$age == !!age, .data$raw_score <= !!raw) %>%
    summarise(s = max(.data[[col]]), .groups = "drop")
  if (nrow(row) == 0) return(NA_integer_)
  row$s[1]
}

# ─────────────────────────────────────────────────────────────
# 2b. Image grid CSS
# ─────────────────────────────────────────────────────────────
slam_img_css <- function() {
  HTML("
    .story-images-grid { display: flex; flex-wrap: wrap; gap: 12px; margin-bottom: 24px; }
    .story-images-grid img { max-width: 200px; border-radius: 10px; border: 1.5px solid #e2e8f0; box-shadow: 0 2px 8px rgba(0,0,0,0.08); }
    .story-img-wrapper { flex: 0 0 auto; }
    .story-img-caption { font-size: 12px; color: #6b7280; text-align: center; margin-top: 4px; }
  ")
}

# ─────────────────────────────────────────────────────────────
# 3. CSS
# ─────────────────────────────────────────────────────────────
slam_css <- function() {
  HTML(sprintf("
    body { background: linear-gradient(135deg, #f0f4fa 0%%, #e8ecf3 100%%);
           font-family: 'Segoe UI', Arial, sans-serif; }
    .slam-hero {
      background: linear-gradient(135deg, %s 0%%, #2a5ab3 100%%);
      color: white; border-radius: 18px; padding: 36px 40px; margin-bottom: 28px;
      box-shadow: 0 8px 30px rgba(27,58,107,0.25); }
    .slam-hero h2 { color: white; font-size: 28px; font-weight: 700; margin: 0 0 6px; }
    .slam-hero p  { color: rgba(255,255,255,0.82); font-size: 14px; margin: 0; }
    .story-card {
      background: white; border-radius: 16px; border: 1.5px solid #e2e8f0;
      box-shadow: 0 4px 16px rgba(0,0,0,0.06); margin-bottom: 24px; overflow: hidden; }
    .story-card-header {
      background: linear-gradient(135deg, %s 0%%, #2a5ab3 100%%);
      color: white; padding: 16px 24px; font-size: 17px; font-weight: 600;
      display: flex; align-items: center; gap: 10px; }
    .story-card-body { padding: 24px; }
    .synopsis-box {
      background: %s; border-left: 4px solid %s;
      border-radius: 8px; padding: 14px 18px; margin-bottom: 20px;
      font-size: 14px; color: #374151; line-height: 1.7; }
    .section-label {
      font-size: 13px; font-weight: 700; color: %s; text-transform: uppercase;
      letter-spacing: 0.8px; margin-bottom: 14px; padding-bottom: 6px;
      border-bottom: 2px solid %s; }
    .wf-item, .gfa-item {
      background: #f8fafc; border-radius: 10px; padding: 16px; margin-bottom: 14px;
      border: 1px solid #e2e8f0; }
    .wf-prompt { font-size: 15px; font-weight: 600; color: %s; margin-bottom: 10px; }
    .gfa-passage {
      background: linear-gradient(135deg, #f0f4fa 0%%, #e8ecf3 100%%);
      border-radius: 10px; padding: 18px 20px; margin-bottom: 12px;
      font-size: 15px; color: #1e293b; line-height: 1.9; font-style: italic; }
    .gfa-passage .blank { color: %s; font-weight: 700; text-decoration: none;
                           border-bottom: 2px dashed %s; }
    .rubric-row { display: flex; gap: 10px; flex-wrap: wrap; margin-bottom: 10px; }
    .rubric-btn { flex: 1; min-width: 60px; }
    .btn-save-slam {
      background: linear-gradient(135deg, %s 0%%, #2a5ab3 100%%); color: white;
      border: none; border-radius: 10px; padding: 13px 32px; font-size: 15px;
      font-weight: 600; transition: all 0.2s ease; }
    .btn-save-slam:hover { transform: translateY(-1px);
                           box-shadow: 0 6px 20px rgba(27,58,107,0.35); color: white; }
    .btn-save-slam:disabled { background: #ccc; transform: none; box-shadow: none; }
    .score-badge { display: inline-block; padding: 5px 14px; border-radius: 20px;
                   font-size: 13px; font-weight: 700; margin: 2px; }
    .badge-raw   { background: #e8f0fe; color: %s; }
    .badge-std   { background: #fff3e0; color: #e65100; }
    .badge-pr    { background: #e8f5e9; color: #2e7d32; }
    .progress-story { font-size: 12px; color: %s; }
    .nav-btn {
      background: white; color: %s; border: 2px solid %s;
      border-radius: 25px; padding: 8px 22px; font-size: 13px; font-weight: 600;
      transition: all 0.2s ease; cursor: pointer; }
    .nav-btn:hover { background: %s; color: white; border-color: %s; }
    .nav-btn.active { background: %s; color: white; border-color: %s; }
    .image-placeholder {
      background: linear-gradient(135deg, #f0f4fa, #e8ecf3);
      border: 2px dashed #c0c8d8; border-radius: 12px;
      display: flex; flex-direction: column; align-items: center; justify-content: center;
      min-height: 200px; color: %s; font-size: 14px; gap: 8px; }
    .narrative-box {
      background: #fefce8; border: 1.5px solid #fde68a; border-radius: 10px;
      padding: 18px; margin-top: 14px; }
    textarea.form-control { border-radius: 10px; border: 1.5px solid #d0d7e2;
      padding: 12px 14px; font-size: 14px; }
    textarea.form-control:focus { border-color: %s;
      box-shadow: 0 0 0 3px rgba(27,58,107,0.1); }
    .rubric-dim-label { font-size: 13px; font-weight: 600; color: %s; margin-bottom: 6px; }
    .results-card { background: white; border-radius: 14px; border: 1.5px solid #e2e8f0;
                    padding: 20px; margin-top: 20px; box-shadow: 0 4px 16px rgba(0,0,0,0.06); }
    .results-title { font-size: 16px; font-weight: 700; color: %s; margin-bottom: 14px; }
    .tab-success { background: #ecfdf5; border-radius: 10px; padding: 16px; margin-top: 12px;
                   border: 1px solid #a7f3d0; }
  ",
  SLAM_BLUE, SLAM_BLUE, SLAM_LIGHT, SLAM_GOLD,
  SLAM_BLUE, SLAM_BLUE,
  SLAM_BLUE, SLAM_BLUE, SLAM_GOLD,
  SLAM_BLUE, SLAM_GRAY, SLAM_BLUE, SLAM_BLUE,
  SLAM_BLUE, SLAM_BLUE, SLAM_BLUE, SLAM_BLUE, SLAM_GRAY,
  SLAM_BLUE, SLAM_BLUE, SLAM_BLUE, SLAM_BLUE, SLAM_BLUE, SLAM_BLUE,
  SLAM_GRAY, SLAM_BLUE, SLAM_BLUE, SLAM_BLUE
  ))
}

# ─────────────────────────────────────────────────────────────
# 4. UI
# ─────────────────────────────────────────────────────────────
ui <- fluidPage(
  theme = bslib::bs_theme(version = 5, primary = SLAM_BLUE, secondary = SLAM_GOLD),
  tags$head(tags$style(slam_css())),
  tags$head(tags$style(slam_img_css())),
  tags$head(tags$style(HTML(".nav-tabs { border-bottom: 2px solid #e2e8f0; }
    .nav-tabs .nav-link { border-radius: 8px 8px 0 0; font-weight: 600; color: #6b7280; }
    .nav-tabs .nav-link.active { color: #1B3A6B; border-color: #e2e8f0 #e2e8f0 white; border-bottom: 3px solid #C8A951; }"))),

  div(style = "max-width: 1100px; margin: 0 auto; padding: 24px 16px;",

    # Hero
    div(class = "slam-hero",
      h2("SLAM 叙事评估 / Narrative Assessment"),
      p("Structured Language Assessment Measures — 图片叙事 · 词找 · 语法填空 · 自由叙事")
    ),

    # Student info bar
    div(class = "story-card",
      div(class = "story-card-header", span("👤"), "受试者信息 / Student Information"),
      div(class = "story-card-body",
        fluidRow(
          column(4,
            div(class = "form-group",
              tags$label("姓名 / Name *", class = "form-label"),
              textInput("slam_student_name", NULL, placeholder = "受试者姓名", width = "100%")
            )
          ),
          column(2,
            div(class = "form-group",
              tags$label("年龄 / Age *", class = "form-label"),
              numericInput("slam_student_age", NULL, value = NULL, min = 5, max = 21, width = "100%")
            )
          ),
          column(2,
            div(class = "form-group",
              tags$label("性别 / Gender", class = "form-label"),
              selectInput("slam_student_gender", NULL,
                choices = c("—" = "", "男 / M" = "M", "女 / F" = "F"),
                selected = "", width = "100%")
            )
          ),
          column(2,
            div(class = "form-group",
              tags$label("评估日期 / Date", class = "form-label"),
              dateInput("slam_assessment_date", NULL,
                value = Sys.Date(), format = "yyyy-mm-dd", width = "100%")
            )
          ),
          column(4,
            div(class = "form-group",
              tags$label("评估师 / Examiner", class = "form-label"),
              textInput("slam_examiner", NULL, placeholder = "评估师姓名 / Examiner", width = "100%")
            )
          ),
          column(4,
            div(class = "form-group",
              tags$label("&nbsp;", class = "form-label"),
              actionButton("slam_start_btn", "▶ 开始评估 / Start Assessment",
                class = "btn-save-slam",
                style = "margin-top: 22px; width: 100%;")
            )
          )
        )
      )
    ),

    # Story tabs
    tabsetPanel(id = "slam_story_tabs", type = "tabs",

      # ── Tab 1: Baseball Troubles ──────────────────────────
      tabPanel("🏇 Baseball Troubles",
        div(class = "story-card",
          div(class = "story-card-header",
            span("🏇"), "Baseball Troubles / 棒球烦恼",
            span(class = "progress-story", "13-17岁 · 6张图 · Word Finding + GFA + Narrative")
          ),
          div(class = "story-card-body",

            # Synopsis
            div(class = "synopsis-box",
              p(strong("故事概要 Story Synopsis:"), br()),
              p(STORIES$baseball_troubles$synopsis)
            ),

            # Images
            div(class = "section-label", "📷 图片卡片 / Picture Cards"),
            div(class = "story-images-grid",
              lapply(1:8, function(p) {
                imgf <- find_story_image("baseball_troubles", p)
                if (!is.null(imgf)) {
                  div(class = "story-img-wrapper",
                    img(src = imgf, style = "max-width: 200px; border-radius: 10px;",
                        alt = sprintf("Baseball Troubles page %%d", p)),
                    div(class = "story-img-caption", sprintf("Page %%d", p))
                  )
                }
              })
            ),

            # Word Finding
            div(class = "section-label", "🔤 Word Finding / 图片命名"),
            lapply(1:6, function(i) {
              wf <- STORIES$baseball_troubles$word_finding
              div(class = "wf-item", id = sprintf("wf_bt_%d", i),
                div(class = "wf-prompt",
                  sprintf("Item %d — %s (%s)", i, wf$prompt_en[i], wf$prompt_zh[i])),
                fluidRow(
                  column(8,
                    textInput(sprintf("wf_bt_%d_text", i), "回答 / Response:", width = "100%")
                  ),
                  column(4,
                    tags$label("原始分 Raw Score", class = "form-label"),
                    selectInput(sprintf("wf_bt_%d_score", i), NULL,
                      choices = c("—"="","1分 (正确)"="1","0分 (错误)"="0"),
                      selected = "", width = "100%")
                  )
                )
              )
            }),

            # GFA
            div(class = "section-label", "📝 GFA 语法填空 / Grammar Fluency Assessment"),
            lapply(1:4, function(i) {
              gfa <- STORIES$baseball_troubles$gfa_items
              ms <- gfa$max_score[i]
              choices <- c("—"="", setNames(as.character(ms:0), paste0(ms:0, "分")))
              blank_p <- gsub("___", sprintf('<span class="blank">___</span>', i), gfa$passage_en[i])
              div(class = "gfa-item", id = sprintf("gfa_bt_%d", i),
                div(class = "gfa-passage",
                  HTML(gsub("\\{\\{blank\\}\\}", blank_p, gfa$passage_en[i])),
                  p(style = "margin-top: 8px; font-size: 13px; color: #6b7280; font-style: normal;",
                    gfa$passage_zh[i])
                ),
                fluidRow(
                  column(8,
                    textInput(sprintf("gfa_bt_%d_text", i), "回答 / Response:", width = "100%")
                  ),
                  column(4,
                    tags$label("评分 Score", class = "form-label"),
                    selectInput(sprintf("gfa_bt_%d_score", i), NULL,
                      choices = choices,
                      selected = "", width = "100%")
                  )
                )
              )
            }),

            # Free Narrative
            div(class = "section-label", "🎤 Free Narrative / 自由叙事"),
            div(class = "narrative-box",
              p(strong("指示 Instruction: "), "请学生看着图片讲述故事。/ Ask student to tell the story using the pictures."),
              textAreaInput("narr_bt", "学生回答 / Student Response:", width = "100%", rows = 5, placeholder = "学生在评估时的叙事内容..."),
              p(style = "margin-top: 10px; font-size: 13px; color: #6b7280;",
                "📝 请评估者在评估时记录学生叙事，并在下方评分。")
            ),

            # Narrative Rubric
            div(class = "section-label", "📊 Narrative Rubric / 叙事评分"),
            lapply(seq_along(STORIES$baseball_troubles$narrative_rubric$dimensions), function(d) {
              dim_name <- STORIES$baseball_troubles$narrative_rubric$dimensions[d]
              div(style = "margin-bottom: 14px;",
                div(class = "rubric-dim-label", sprintf("%s (0-2分)", dim_name)),
                div(class = "rubric-row",
                  radioButtons(sprintf("nr_bt_dim%d", d), NULL,
                    choices = c("0分"="0","1分"="1","2分"="2"),
                    selected = character(0), inline = TRUE,
                    width = "100%")
                )
              )
            }),

            div(style = "margin-top: 20px; text-align: center;",
              actionButton("save_bt", "💾 保存 Baseball Troubles 评分",
                class = "btn-save-slam")
            )
          )
        )
      ),

      # ── Tab 2: The Best Turkey ────────────────────────────
      tabPanel("🦃 The Best Turkey",
        div(class = "story-card",
          div(class = "story-card-header",
            span("🦃"), "The Best Turkey / 最好的火鸡",
            span(class = "progress-story", "10-14岁 · 5张图 · Word Finding + GFA + Narrative")
          ),
          div(class = "story-card-body",
            div(class = "synopsis-box",
              p(strong("故事概要 Story Synopsis:"), br()),
              p(STORIES$the_best_turkey$synopsis)
            ),
            div(class = "section-label", "📷 图片卡片 / Picture Cards"),
            div(class = "story-images-grid",
              lapply(1:8, function(p) {
                imgf <- find_story_image("the_best_turkey", p)
                if (!is.null(imgf)) {
                  div(class = "story-img-wrapper",
                    img(src = imgf, style = "max-width: 200px; border-radius: 10px;",
                        alt = sprintf("The Best Turkey page %%d", p)),
                    div(class = "story-img-caption", sprintf("Page %%d", p))
                  )
                }
              })
            ),
            div(class = "section-label", "🔤 Word Finding / 图片命名"),
            lapply(1:5, function(i) {
              wf <- STORIES$the_best_turkey$word_finding
              div(class = "wf-item", id = sprintf("wf_tbt_%d", i),
                div(class = "wf-prompt", sprintf("Item %d — %s (%s)", i, wf$prompt_en[i], wf$prompt_zh[i])),
                fluidRow(
                  column(8, textInput(sprintf("wf_tbt_%d_text", i), "回答 / Response:", width = "100%")),
                  column(4,
                    tags$label("原始分 Raw Score", class = "form-label"),
                    selectInput(sprintf("wf_tbt_%d_score", i), NULL,
                      choices = c("—"="","1分 (正确)"="1","0分 (错误)"="0"),
                      selected = "", width = "100%"))
                )
              )
            }),
            div(class = "section-label", "📝 GFA 语法填空 / Grammar Fluency Assessment"),
            lapply(1:4, function(i) {
              gfa <- STORIES$the_best_turkey$gfa_items
              ms <- gfa$max_score[i]
              choices <- c("—"="", setNames(as.character(ms:0), paste0(ms:0, "分")))
              div(class = "gfa-item", id = sprintf("gfa_tbt_%d", i),
                div(class = "gfa-passage",
                  HTML(gsub("___", sprintf('<span class="blank">___</span>'), gfa$passage_en[i])),
                  p(style = "margin-top: 8px; font-size: 13px; color: #6b7280; font-style: normal;", gfa$passage_zh[i])
                ),
                fluidRow(
                  column(8, textInput(sprintf("gfa_tbt_%d_text", i), "回答 / Response:", width = "100%")),
                  column(4,
                    tags$label("评分 Score", class = "form-label"),
                    selectInput(sprintf("gfa_tbt_%d_score", i), NULL,
                      choices = choices,
                      selected = "", width = "100%"))
                )
              )
            }),
            div(class = "section-label", "🎤 Free Narrative / 自由叙事"),
            div(class = "narrative-box",
              p(strong("指示 Instruction: "), "请学生看着图片讲述故事。"),
              textAreaInput("narr_tbt", "学生回答 / Student Response:", width = "100%", rows = 5, placeholder = "学生在评估时的叙事内容..."),
              p(style = "margin-top: 10px; font-size: 13px; color: #6b7280;", "📝 请评估者记录学生叙事并评分。")
            ),
            div(class = "section-label", "📊 Narrative Rubric / 叙事评分"),
            lapply(seq_along(STORIES$the_best_turkey$narrative_rubric$dimensions), function(d) {
              dim_name <- STORIES$the_best_turkey$narrative_rubric$dimensions[d]
              div(style = "margin-bottom: 14px;",
                div(class = "rubric-dim-label", sprintf("%s (0-2分)", dim_name)),
                div(class = "rubric-row",
                  radioButtons(sprintf("nr_tbt_dim%d", d), NULL,
                    choices = c("0分"="0","1分"="1","2分"="2"),
                    selected = character(0), inline = TRUE, width = "100%")
                )
              )
            }),
            div(style = "margin-top: 20px; text-align: center;",
              actionButton("save_tbt", "💾 保存 The Best Turkey 评分",
                class = "btn-save-slam")
            )
          )
        )
      ),

      # ── Tab 3: The Girl Who Loved Horses ─────────────────
      tabPanel("🐴 The Girl Who Loved Horses",
        div(class = "story-card",
          div(class = "story-card-header",
            span("🐴"), "The Girl Who Loved Horses / 爱马的女孩",
            span(class = "progress-story", "13-17岁 · 6张图 · Word Finding + GFA + Narrative")
          ),
          div(class = "story-card-body",
            div(class = "synopsis-box",
              p(strong("故事概要 Story Synopsis:"), br()),
              p(STORIES$the_girl_who_loved_horses$synopsis)
            ),
            div(class = "section-label", "📷 图片卡片 / Picture Cards"),
            div(class = "story-images-grid",
              lapply(1:6, function(p) {
                imgf <- find_story_image("the_girl_who_loved_horses", p)
                if (!is.null(imgf)) {
                  div(class = "story-img-wrapper",
                    img(src = imgf, style = "max-width: 200px; border-radius: 10px;",
                        alt = sprintf("The Girl Who Loved Horses page %%d", p)),
                    div(class = "story-img-caption", sprintf("Page %%d", p))
                  )
                }
              })
            ),
            div(class = "section-label", "🔤 Word Finding / 图片命名"),
            lapply(1:6, function(i) {
              wf <- STORIES$the_girl_who_loved_horses$word_finding
              div(class = "wf-item", id = sprintf("wf_gwh_%d", i),
                div(class = "wf-prompt", sprintf("Item %d — %s (%s)", i, wf$prompt_en[i], wf$prompt_zh[i])),
                fluidRow(
                  column(8, textInput(sprintf("wf_gwh_%d_text", i), "回答 / Response:", width = "100%")),
                  column(4,
                    tags$label("原始分 Raw Score", class = "form-label"),
                    selectInput(sprintf("wf_gwh_%d_score", i), NULL,
                      choices = c("—"="","1分 (正确)"="1","0分 (错误)"="0"),
                      selected = "", width = "100%"))
                )
              )
            }),
            div(class = "section-label", "📝 GFA 语法填空 / Grammar Fluency Assessment"),
            lapply(1:4, function(i) {
              gfa <- STORIES$the_girl_who_loved_horses$gfa_items
              ms <- gfa$max_score[i]
              choices <- c("—"="", setNames(as.character(ms:0), paste0(ms:0, "分")))
              div(class = "gfa-item", id = sprintf("gfa_gwh_%d", i),
                div(class = "gfa-passage",
                  HTML(gsub("___", sprintf('<span class="blank">___</span>'), gfa$passage_en[i])),
                  p(style = "margin-top: 8px; font-size: 13px; color: #6b7280; font-style: normal;", gfa$passage_zh[i])
                ),
                fluidRow(
                  column(8, textInput(sprintf("gfa_gwh_%d_text", i), "回答 / Response:", width = "100%")),
                  column(4,
                    tags$label("评分 Score", class = "form-label"),
                    selectInput(sprintf("gfa_gwh_%d_score", i), NULL,
                      choices = choices,
                      selected = "", width = "100%"))
                )
              )
            }),
            div(class = "section-label", "🎤 Free Narrative / 自由叙事"),
            div(class = "narrative-box",
              p(strong("指示 Instruction: "), "请学生看着图片讲述故事。"),
              textAreaInput("narr_gwh", "学生回答 / Student Response:", width = "100%", rows = 5, placeholder = "学生在评估时的叙事内容..."),
              p(style = "margin-top: 10px; font-size: 13px; color: #6b7280;", "📝 请评估者记录学生叙事并评分。")
            ),
            div(class = "section-label", "📊 Narrative Rubric / 叙事评分"),
            lapply(seq_along(STORIES$the_girl_who_loved_horses$narrative_rubric$dimensions), function(d) {
              dim_name <- STORIES$the_girl_who_loved_horses$narrative_rubric$dimensions[d]
              div(style = "margin-bottom: 14px;",
                div(class = "rubric-dim-label", sprintf("%s (0-2分)", dim_name)),
                div(class = "rubric-row",
                  radioButtons(sprintf("nr_gwh_dim%d", d), NULL,
                    choices = c("0分"="0","1分"="1","2分"="2"),
                    selected = character(0), inline = TRUE, width = "100%")
                )
              )
            }),
            div(style = "margin-top: 20px; text-align: center;",
              actionButton("save_gwh", "💾 保存 The Girl Who Loved Horses 评分",
                class = "btn-save-slam")
            )
          )
        )
      ),

      # ── Tab 4: Wallace and Batty ───────────────────────────
      tabPanel("🐕 Wallace and Batty",
        div(class = "story-card",
          div(class = "story-card-header",
            span("🐕"), "Wallace and Batty / 华莱士与巴蒂",
            span(class = "progress-story", "7-14岁 · 5张图 · Word Finding + GFA + Narrative")
          ),
          div(class = "story-card-body",
            div(class = "synopsis-box",
              p(strong("故事概要 Story Synopsis:"), br()),
              p(STORIES$wallace_and_batty$synopsis)
            ),
            div(class = "section-label", "📷 图片卡片 / Picture Cards"),
            div(class = "story-images-grid",
              lapply(1:6, function(p) {
                imgf <- find_story_image("wallace_and_batty", p)
                if (!is.null(imgf)) {
                  div(class = "story-img-wrapper",
                    img(src = imgf, style = "max-width: 200px; border-radius: 10px;",
                        alt = sprintf("Wallace and Batty page %%d", p)),
                    div(class = "story-img-caption", sprintf("Page %%d", p))
                  )
                }
              })
            ),
            div(class = "section-label", "🔤 Word Finding / 图片命名"),
            lapply(1:5, function(i) {
              wf <- STORIES$wallace_and_batty$word_finding
              div(class = "wf-item", id = sprintf("wf_wb_%d", i),
                div(class = "wf-prompt", sprintf("Item %d — %s (%s)", i, wf$prompt_en[i], wf$prompt_zh[i])),
                fluidRow(
                  column(8, textInput(sprintf("wf_wb_%d_text", i), "回答 / Response:", width = "100%")),
                  column(4,
                    tags$label("原始分 Raw Score", class = "form-label"),
                    selectInput(sprintf("wf_wb_%d_score", i), NULL,
                      choices = c("—"="","1分 (正确)"="1","0分 (错误)"="0"),
                      selected = "", width = "100%"))
                )
              )
            }),
            div(class = "section-label", "📝 GFA 语法填空 / Grammar Fluency Assessment"),
            lapply(1:4, function(i) {
              gfa <- STORIES$wallace_and_batty$gfa_items
              ms <- gfa$max_score[i]
              choices <- c("—"="", setNames(as.character(ms:0), paste0(ms:0, "分")))
              div(class = "gfa-item", id = sprintf("gfa_wb_%d", i),
                div(class = "gfa-passage",
                  HTML(gsub("___", sprintf('<span class="blank">___</span>'), gfa$passage_en[i])),
                  p(style = "margin-top: 8px; font-size: 13px; color: #6b7280; font-style: normal;", gfa$passage_zh[i])
                ),
                fluidRow(
                  column(8, textInput(sprintf("gfa_wb_%d_text", i), "回答 / Response:", width = "100%")),
                  column(4,
                    tags$label("评分 Score", class = "form-label"),
                    selectInput(sprintf("gfa_wb_%d_score", i), NULL,
                      choices = choices,
                      selected = "", width = "100%"))
                )
              )
            }),
            div(class = "section-label", "🎤 Free Narrative / 自由叙事"),
            div(class = "narrative-box",
              p(strong("指示 Instruction: "), "请学生看着图片讲述故事。"),
              textAreaInput("narr_wb", "学生回答 / Student Response:", width = "100%", rows = 5, placeholder = "学生在评估时的叙事内容..."),
              p(style = "margin-top: 10px; font-size: 13px; color: #6b7280;", "📝 请评估者记录学生叙事并评分。")
            ),
            div(class = "section-label", "📊 Narrative Rubric / 叙事评分"),
            lapply(seq_along(STORIES$wallace_and_batty$narrative_rubric$dimensions), function(d) {
              dim_name <- STORIES$wallace_and_batty$narrative_rubric$dimensions[d]
              div(style = "margin-bottom: 14px;",
                div(class = "rubric-dim-label", sprintf("%s (0-2分)", dim_name)),
                div(class = "rubric-row",
                  radioButtons(sprintf("nr_wb_dim%d", d), NULL,
                    choices = c("0分"="0","1分"="1","2分"="2"),
                    selected = character(0), inline = TRUE, width = "100%")
                )
              )
            }),
            div(style = "margin-top: 20px; text-align: center;",
              actionButton("save_wb", "💾 保存 Wallace and Batty 评分",
                class = "btn-save-slam")
            )
          )
        )
      )

    ),  # end tabsetPanel

    # ── Summary Panel ──────────────────────────────────────
    div(class = "story-card",
      div(class = "story-card-header", span("📋"), "SLAM 综合结果 / Summary Results"),
      div(class = "story-card-body",
        fluidRow(
          column(6,
            p(strong("各故事得分概览 / Score Overview by Story:"), style = "margin-bottom: 12px;"),
            uiOutput("slam_summary_table")
          ),
          column(6,
            p(strong("说明 / Notes:"), style = "margin-bottom: 8px;"),
            textAreaInput("slam_notes", NULL, width = "100%", rows = 5,
              placeholder = "评估者备注 / Examiner notes...")
          )
        ),
        div(style = "margin-top: 16px; text-align: center;",
          actionButton("save_all_slam", "💾 保存完整评估报告",
            class = "btn-save-slam", style = "font-size: 16px; padding: 14px 40px;")
        ),
        uiOutput("slam_save_result")
      )
    ),

    # Footer
    div(style = "text-align: center; margin-top: 36px; padding: 20px; color: #aaa; font-size: 13px;",
      span(style = sprintf("color: %s; font-weight: 600;", SLAM_BLUE), "SLAM"),
      "© 2026  |  Columbia University Leaders Project — Free for Copying and Distribution  |  ",
      "Powered by R Shiny"
    )
  )
)

# ─────────────────────────────────────────────────────────────
# 5. Server
# ─────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  # Reactive: get current student info
  student_info <- reactive({
    list(
      name    = trim(input$slam_student_name %||% ""),
      age     = as.integer(input$slam_student_age %||% NA_integer_),
      gender  = input$slam_student_gender %||% "",
      date    = as.character(input$slam_assessment_date %||% Sys.Date()),
      examiner = trim(input$slam_examiner %||% "")
    )
  })

  # ── Start Assessment ──────────────────────────────────
  observeEvent(input$slam_start_btn, {
    si <- student_info()
    if (si$name == "") {
      showNotification(
        tagList(icon("exclamation-triangle"), "请输入学生姓名 / Please enter student name"),
        type = "error", duration = 4
      )
      return()
    }
    if (is.na(si$age) || si$age < 5 || si$age > 21) {
      showNotification(
        tagList(icon("exclamation-triangle"), "请输入有效年龄(5-21) / Please enter valid age (5-21)"),
        type = "error", duration = 4
      )
      return()
    }

    tryCatch({
      con <- get_con()
      on.exit(dbDisconnect(con), add = TRUE)

      # Upsert patient
      existing <- dbGetQuery(con,
        "SELECT id, name FROM patients WHERE name = ? LIMIT 1",
        params = list(si$name))$id[1]
      if (is.na(existing)) {
        dbExecute(con,
          "INSERT INTO patients (name, dob, gender, examiner, notes) VALUES (?, ?, ?, ?, ?)",
          params = list(si$name, si$date, si$gender, si$examiner, ""))
        patient_id <- dbGetQuery(con, "SELECT last_insert_rowid() as id")$id[1]
      } else {
        patient_id <- existing
        dbExecute(con,
          "UPDATE patients SET examiner = ?, notes = COALESCE(notes, '') || ? WHERE id = ?",
          params = list(si$examiner, sprintf(" [SLAM updated: %s]", si$date), patient_id))
      }

      # Create assessment with assessment_type='SLAM'
      age_group_str <- sprintf("%d:0-%d:11", floor(si$age/2)*2, floor(si$age/2)*2 + 1)
      dbExecute(con,
        "INSERT INTO assessments (patient_id, assessment_date, age_years, age_group, status, assessment_type)
         VALUES (?, ?, ?, ?, 'in_progress', 'SLAM')",
        params = list(patient_id, si$date, si$age, age_group_str))
      assessment_id <- dbGetQuery(con, "SELECT last_insert_rowid() as id")$id[1]

      # Store in session
      session$userData$slam_patient_id <- patient_id
      session$userData$slam_assessment_id <- assessment_id

      showNotification(
        tagList(icon("check-circle"),
          sprintf("评估已开始 / Assessment started (ID: #%d)", assessment_id)),
        type = "message", duration = 5
      )

      # Switch to story tab
      updateTabsetPanel(session, "slam_story_tabs", selected = "🏇 Baseball Troubles")

    }, error = function(e) {
      showNotification(
        tagList(icon("exclamation-triangle"), sprintf("启动失败: %s", e$message)),
        type = "error", duration = 6
      )
    })
  })

  # ── Scoring helpers ────────────────────────────────────

  calc_wf_raw <- function(prefix, n) {
    scores <- lapply(seq_len(n), function(i) {
      as.integer(input[[sprintf("%s_%d_score", prefix, i)]] %||% 0)
    })
    sum(unlist(scores), na.rm = TRUE)
  }

  calc_gfa_raw <- function(prefix, n) {
    scores <- lapply(seq_len(n), function(i) {
      as.integer(input[[sprintf("%s_%d_score", prefix, i)]] %||% 0)
    })
    sum(unlist(scores), na.rm = TRUE)
  }

  calc_narr_rubric <- function(prefix, n_dims = 5) {
    scores <- lapply(seq_len(n_dims), function(d) {
      as.integer(input[[sprintf("%s_dim%d", prefix, d)]] %||% 0)
    })
    sum(unlist(scores), na.rm = TRUE)
  }

  build_scores_df <- function(story_id, wf_prefix, gfa_prefix, narr_prefix, n_wf, n_gfa) {
    age <- student_info()$age
    if (is.na(age)) age <- 10L

    wf_raw   <- calc_wf_raw(wf_prefix, n_wf)
    gfa_raw  <- calc_gfa_raw(gfa_prefix, n_gfa)
    narr_raw <- calc_narr_rubric(narr_prefix)

    wf_std  <- get_slam_standard_score(wf_raw,  "word_finding", age)
    gfa_std <- get_slam_standard_score(gfa_raw, "gfa",          age)

    tibble(
      assessment_id = NA_integer_,
      subtest        = c(
        sprintf("%s_WordFinding", story_id),
        sprintf("%s_GFA",        story_id),
        sprintf("%s_Narrative", story_id)
      ),
      raw_score    = c(wf_raw, gfa_raw, narr_raw),
      scaled_score = c(wf_std, gfa_std, NA_integer_),
      narrative_text = c(NA_character_, NA_character_,
        input[[narr_prefix]] %||% NA_character_)
    )
  }

  # ── Save individual story ──────────────────────────────

  save_one_story <- function(story_id, wf_prefix, gfa_prefix, narr_prefix, n_wf, n_gfa, btn_id) {
    si <- student_info()
    if (si$name == "") {
      showNotification(
        tagList(icon("exclamation-triangle"), "请先输入学生姓名 / Please enter student name"),
        type = "error", duration = 4
      )
      return(NULL)
    }

    tryCatch({
      con <- get_con()
      on.exit(dbDisconnect(con), add = TRUE)

      # Get or create patient
      patient_id <- dbGetQuery(con,
        "SELECT id FROM patients WHERE name = ? LIMIT 1",
        params = list(si$name))$id[1]
      if (is.na(patient_id)) {
        dbExecute(con,
          "INSERT INTO patients (name, dob, gender, examiner, notes) VALUES (?, ?, ?, ?, ?)",
          params = list(si$name, si$date, si$gender, "", ""))
        patient_id <- dbGetQuery(con, "SELECT last_insert_rowid() as id")$id[1]
      }

      # Create assessment
      dob_calc <- if (!is.na(si$age)) {
        as.character(Sys.Date() - years(si$age))
      } else { as.character(Sys.Date()) }

      dbExecute(con,
        "INSERT INTO assessments (patient_id, assessment_date, age_years, age_group, status)
         VALUES (?, ?, ?, ?, 'in_progress')",
        params = list(patient_id, si$date, si$age,
          sprintf("%d:0-%d:11", floor(age_to_group(si$age)), floor(age_to_group(si$age)) + 1)))

      assessment_id <- dbGetQuery(con, "SELECT last_insert_rowid() as id")$id[1]

      # Build and save scores
      scores_df <- build_scores_df(story_id, wf_prefix, gfa_prefix, narr_prefix, n_wf, n_gfa)
      scores_df$assessment_id <- assessment_id

      # Save responses
      lapply(seq_len(n_wf), function(i) {
        resp <- input[[sprintf("%s_%d_text", wf_prefix, i)]] %||% ""
        sc   <- as.integer(input[[sprintf("%s_%d_score", wf_prefix, i)]] %||% 0)
        dbExecute(con,
          "INSERT OR REPLACE INTO responses (assessment_id, subtest, item_number, response_text, score)
           VALUES (?, ?, ?, ?, ?)",
          params = list(assessment_id, sprintf("%s_WordFinding", story_id), i, resp, sc))
      })
      lapply(seq_len(n_gfa), function(i) {
        resp <- input[[sprintf("%s_%d_text", gfa_prefix, i)]] %||% ""
        sc   <- as.integer(input[[sprintf("%s_%d_score", gfa_prefix, i)]] %||% 0)
        dbExecute(con,
          "INSERT OR REPLACE INTO responses (assessment_id, subtest, item_number, response_text, score)
           VALUES (?, ?, ?, ?, ?)",
          params = list(assessment_id, sprintf("%s_GFA", story_id), i, resp, sc))
      })

      # Save narrative text
      narr_text <- input[[narr_prefix]] %||% ""
      dbExecute(con,
        "INSERT OR REPLACE INTO responses (assessment_id, subtest, item_number, response_text)
         VALUES (?, ?, ?, ?)",
        params = list(assessment_id, sprintf("%s_Narrative", story_id), 1L, narr_text))

      # Save subtest scores
      lapply(seq_len(nrow(scores_df)), function(r) {
        row <- scores_df[r, ]
        dbExecute(con,
          "INSERT OR REPLACE INTO subtest_scores (assessment_id, subtest, raw_score, scaled_score)
           VALUES (?, ?, ?, ?)",
          params = list(assessment_id, row$subtest, row$raw_score, row$scaled_score))
      })

      showNotification(
        tagList(icon("check-circle"),
          sprintf("已保存 %s 评分 (Assessment #%d)", story_id, assessment_id)),
        type = "message", duration = 4
      )

      assessment_id

    }, error = function(e) {
      showNotification(
        tagList(icon("exclamation-triangle"), sprintf("保存失败: %s", e$message)),
        type = "error", duration = 6
      )
      NULL
    })
  }

  # ── Save buttons ────────────────────────────────────────

  observeEvent(input$save_bt, {
    save_one_story("BaseballTroubles", "wf_bt", "gfa_bt", "narr_bt", 6, 4)
  })
  observeEvent(input$save_tbt, {
    save_one_story("TheBestTurkey", "wf_tbt", "gfa_tbt", "narr_tbt", 5, 4)
  })
  observeEvent(input$save_gwh, {
    save_one_story("GirlWhoLovedHorses", "wf_gwh", "gfa_gwh", "narr_gwh", 6, 4)
  })
  observeEvent(input$save_wb, {
    save_one_story("WallaceAndBatty", "wf_wb", "gfa_wb", "narr_wb", 5, 4)
  })

  # ── Summary Table ───────────────────────────────────────

  output$slam_summary_table <- renderUI({
    si <- student_info()
    age <- if (is.na(si$age)) 10L else si$age

    stories <- list(
      list(id = "BaseballTroubles",    name = "Baseball Troubles",    prefix_wf = "wf_bt",  prefix_gfa = "gfa_bt",  prefix_nr = "nr_bt",  n_wf = 6, n_gfa = 4),
      list(id = "TheBestTurkey",        name = "The Best Turkey",       prefix_wf = "wf_tbt", prefix_gfa = "gfa_tbt", prefix_nr = "nr_tbt", n_wf = 5, n_gfa = 4),
      list(id = "GirlWhoLovedHorses",   name = "Girl Who Loved Horses", prefix_wf = "wf_gwh", prefix_gfa = "gfa_gwh", prefix_nr = "nr_gwh", n_wf = 6, n_gfa = 4),
      list(id = "WallaceAndBatty",      name = "Wallace and Batty",     prefix_wf = "wf_wb",  prefix_gfa = "gfa_wb",  prefix_nr = "nr_wb",  n_wf = 5, n_gfa = 4)
    )

    rows <- lapply(stories, function(s) {
      wf_raw  <- calc_wf_raw(s$prefix_wf, s$n_wf)
      gfa_raw <- calc_gfa_raw(s$prefix_gfa, s$n_gfa)
      nr_raw  <- calc_narr_rubric(s$prefix_nr)
      wf_std  <- get_slam_standard_score(wf_raw,  "word_finding", age)
      gfa_std <- get_slam_standard_score(gfa_raw, "gfa",          age)

      wf_pr  <- std_to_pr(wf_std)
      gfa_pr <- std_to_pr(gfa_std)

      div(style = "margin-bottom: 10px; padding: 10px 14px; background: #f8fafc; border-radius: 8px; border: 1px solid #e2e8f0;",
        strong(sprintf("%s:", s$name)), br(),
        span(class = "score-badge badge-raw",
          sprintf("Word Finding 原始: %d", wf_raw)),
        span(class = "score-badge badge-raw",
          sprintf("GFA 原始: %d", gfa_raw)),
        span(class = "score-badge badge-raw",
          sprintf("Narrative: %d/10", nr_raw)),
        if (!is.na(wf_std)) {
          span(class = "score-badge badge-std",
            sprintf("Word Finding 标准: %d", wf_std))
        },
        if (!is.na(gfa_std)) {
          span(class = "score-badge badge-std",
            sprintf("GFA 标准: %d", gfa_std))
        }
      )
    })

    tagList(rows)
  })

  # ── Save All ────────────────────────────────────────────

  observeEvent(input$save_all_slam, {
    si <- student_info()
    if (si$name == "") {
      showNotification(
        tagList(icon("exclamation-triangle"), "请先输入学生姓名 / Please enter student name"),
        type = "error", duration = 4
      )
      return()
    }

    tryCatch({
      con <- get_con()
      on.exit(dbDisconnect(con), add = TRUE)

      # Get or create patient
      patient_id <- dbGetQuery(con,
        "SELECT id FROM patients WHERE name = ? LIMIT 1",
        params = list(si$name))$id[1]
      if (is.na(patient_id)) {
        dbExecute(con,
          "INSERT INTO patients (name, dob, gender, examiner, notes) VALUES (?, ?, ?, ?, ?)",
          params = list(si$name, si$date, si$gender, "", ""))
        patient_id <- dbGetQuery(con, "SELECT last_insert_rowid() as id")$id[1]
      }

      age <- si$age
      if (is.na(age)) age <- 10L

      ag <- sprintf("%d:0-%d:11", floor(age_to_group(age)), floor(age_to_group(age)) + 1)

      dbExecute(con,
        "INSERT INTO assessments (patient_id, assessment_date, age_years, age_group, status)
         VALUES (?, ?, ?, ?, 'completed')",
        params = list(patient_id, si$date, age, ag))
      assessment_id <- dbGetQuery(con, "SELECT last_insert_rowid() as id")$id[1]

      # Save all story scores
      all_scores <- bind_rows(
        build_scores_df("BaseballTroubles",  "wf_bt",  "gfa_bt",  "narr_bt",  6, 4),
        build_scores_df("TheBestTurkey",      "wf_tbt", "gfa_tbt", "narr_tbt", 5, 4),
        build_scores_df("GirlWhoLovedHorses", "wf_gwh", "gfa_gwh", "narr_gwh", 6, 4),
        build_scores_df("WallaceAndBatty",    "wf_wb",  "gfa_wb",  "narr_wb",  5, 4)
      )
      all_scores$assessment_id <- assessment_id

      lapply(seq_len(nrow(all_scores)), function(r) {
        row <- all_scores[r, ]
        dbExecute(con,
          "INSERT OR REPLACE INTO subtest_scores (assessment_id, subtest, raw_score, scaled_score)
           VALUES (?, ?, ?, ?)",
          params = list(assessment_id, row$subtest, row$raw_score, row$scaled_score))
      })

      # Save notes
      if (!is.null(input$slam_notes) && nzchar(input$slam_notes)) {
        dbExecute(con,
          "UPDATE assessments SET notes = ? WHERE id = ?",
          params = list(input$slam_notes, assessment_id))
      }

      output$slam_save_result <- renderUI({
        div(class = "tab-success",
          icon("check-circle"), sprintf(" 完整评估报告已保存 (Assessment ID: %d, Patient: %s)",
            assessment_id, si$name))
      })

    }, error = function(e) {
      output$slam_save_result <- renderUI({
        div(style = "color: #dc2626; margin-top: 12px;",
          icon("exclamation-triangle"), sprintf(" 保存失败: %s", e$message))
      })
    })
  })
}

# ─────────────────────────────────────────────────────────────
# Helpers
# ─────────────────────────────────────────────────────────────
age_to_group <- function(age) {
  age <- as.integer(age)
  if (age <= 8) 7
  else if (age <= 10) 9
  else if (age <= 12) 11
  else if (age <= 14) 13
  else if (age <= 16) 15
  else 17
}

std_to_pr <- function(std) {
  # Approximate percentile rank from standard score (mean=100, sd=15 for composite; here 10,15)
  if (is.na(std) || std < 50) return(NA_real_)
  pnorm((std - 100) / 15) * 100
}

# ─────────────────────────────────────────────────────────────
# Run App
# ─────────────────────────────────────────────────────────────
shinyApp(ui = ui, server = server)
