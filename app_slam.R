# app_slam.R — SLAM Narrative Assessment Tool
# Stories: Baseball Troubles / The Best Turkey / The Girl Who Loved Horses / Wallace and Batty
# Scoring: Word Finding (raw→standardized) + GFA (0-2 rubric) + Free Narrative
# Data saved to celf5_assessments.db
# Layout: Matches CELF-5 tabPanel pattern with Subject Info tab, 4 story tabs, Report tab

library(shiny)
library(bslib)
library(purrr)
library(dplyr)
library(tidyr)
library(lubridate)
library(RSQLite)
library(glue)

source("/home/yzhang/clawfiles/celf5_shiny/global.R")

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
# 0b. Story Image Carousel (served from www/story_images/)
# ─────────────────────────────────────────────────────────────
story_img_carousel <- function(story_id, n_images) {
  img_tags <- lapply(seq_len(n_images), function(p) {
    page_num <- sprintf("p%d", p)
    img_path <- sprintf("story_images/%s_%s_img1.png", story_id, page_num)
    tags$div(class = "carousel-slide",
      tags$img(src = img_path, class = "story-img",
                alt = sprintf("Page %d of %s", p, story_id))
    )
  })
  tags$div(class = "story-carousel", id = sprintf("carousel_%s", story_id),
    tagList(img_tags)
  )
}

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
# 3. CSS — CELF-5 brand style
# ─────────────────────────────────────────────────────────────
slam_css <- function() {
  HTML(paste0(
"body { background: #ffffff; font-family: 'Segoe UI', Arial, sans-serif; }
.container-fluid { padding: 0; }
.tab-content { padding: 20px; background: #ffffff; min-height: 100vh; }
.nav-tabs { border-bottom: 2px solid #e2e8f0; }
.nav-tabs > li > a { border-radius: 8px 8px 0 0; font-weight: 600; color: ", SLAM_GRAY, "; }
.nav-tabs > li.active > a { color: ", SLAM_BLUE, "; border-color: #e2e8f0 #e2e8f0 white; border-bottom: 3px solid ", SLAM_GOLD, "; }
.nav-tabs > li > a:hover { color: ", SLAM_BLUE, "; }
.slam-hero { background: linear-gradient(135deg, ", SLAM_BLUE, " 0%, #2a5ab3 100%); color: white; border-radius: 18px; padding: 36px 40px; margin-bottom: 28px; box-shadow: 0 8px 30px rgba(27,58,107,0.25); }
.slam-hero h2 { color: white; font-size: 28px; font-weight: 700; margin: 0 0 6px; }
.slam-hero p  { color: rgba(255,255,255,0.82); font-size: 14px; margin: 0; }
.story-card { background: white; border-radius: 16px; border: 1.5px solid #e2e8f0; box-shadow: 0 4px 16px rgba(0,0,0,0.06); margin-bottom: 24px; overflow: hidden; }
.story-card-header { background: linear-gradient(135deg, ", SLAM_BLUE, " 0%, #2a5ab3 100%); color: white; padding: 16px 24px; font-size: 17px; font-weight: 600; display: flex; align-items: center; gap: 10px; }
.story-card-body { padding: 24px; }
.synopsis-box { background: #F0F4FA; border-left: 4px solid #C8A951; border-radius: 8px; padding: 14px 18px; margin-bottom: 20px; font-size: 14px; color: #374151; line-height: 1.7; }
.section-label { font-size: 13px; font-weight: 700; color: ", SLAM_BLUE, "; text-transform: uppercase; letter-spacing: 0.8px; margin-bottom: 14px; padding-bottom: 6px; border-bottom: 2px solid ", SLAM_BLUE, "; }
.wf-item, .gfa-item { background: #f8fafc; border-radius: 10px; padding: 16px; margin-bottom: 14px; border: 1px solid #e2e8f0; }
.wf-prompt { font-size: 15px; font-weight: 600; color: ", SLAM_BLUE, "; margin-bottom: 10px; }
.gfa-passage { background: linear-gradient(135deg, #f0f4fa 0%, #e8ecf3 100%); border-radius: 10px; padding: 18px 20px; margin-bottom: 12px; font-size: 15px; color: #1e293b; line-height: 1.9; font-style: italic; }
.gfa-passage .blank { color: ", SLAM_BLUE, "; font-weight: 700; text-decoration: none; border-bottom: 2px dashed #C8A951; }
.rubric-row { display: flex; gap: 10px; flex-wrap: wrap; margin-bottom: 10px; }
.rubric-btn { flex: 1; min-width: 60px; }
.btn-save-slam { background: linear-gradient(135deg, ", SLAM_BLUE, " 0%, #2a5ab3 100%); color: white; border: none; border-radius: 10px; padding: 13px 32px; font-size: 15px; font-weight: 600; transition: all 0.2s ease; }
.btn-save-slam:hover { transform: translateY(-1px); box-shadow: 0 6px 20px rgba(27,58,107,0.35); color: white; }
.btn-save-slam:disabled { background: #ccc; transform: none; box-shadow: none; }
.score-badge { display: inline-block; padding: 5px 14px; border-radius: 20px; font-size: 13px; font-weight: 700; margin: 2px; }
.badge-raw   { background: #e8f0fe; color: ", SLAM_BLUE, "; }
.badge-std   { background: #fff3e0; color: #e65100; }
.badge-pr    { background: #e8f5e9; color: #2e7d32; }
.progress-story { font-size: 12px; color: rgba(255,255,255,0.7); }
.nav-btn { background: white; color: ", SLAM_BLUE, "; border: 2px solid ", SLAM_BLUE, "; border-radius: 25px; padding: 8px 22px; font-size: 13px; font-weight: 600; transition: all 0.2s ease; cursor: pointer; }
.nav-btn:hover { background: ", SLAM_BLUE, "; color: white; border-color: ", SLAM_BLUE, "; }
.nav-btn.active { background: ", SLAM_BLUE, "; color: white; border-color: ", SLAM_BLUE, "; }
.story-carousel { display: flex; gap: 12px; overflow-x: auto; padding: 10px 0; scroll-snap-type: x mandatory; -webkit-overflow-scrolling: touch; }
.carousel-slide { flex: 0 0 auto; scroll-snap-align: start; }
.story-img { height: 260px; width: auto; border-radius: 10px; border: 2px solid #e2e8f0; object-fit: cover; box-shadow: 0 4px 12px rgba(0,0,0,0.1); }
.narrative-box { background: #fefce8; border: 1.5px solid #fde68a; border-radius: 10px; padding: 18px; margin-top: 14px; }
textarea.form-control { border-radius: 10px; border: 1.5px solid #d0d7e2; padding: 12px 14px; font-size: 14px; }
textarea.form-control:focus { border-color: ", SLAM_BLUE, "; box-shadow: 0 0 0 3px rgba(27,58,107,0.1); }
.rubric-dim-label { font-size: 13px; font-weight: 600; color: ", SLAM_BLUE, "; margin-bottom: 6px; }
.results-card { background: white; border-radius: 14px; border: 1.5px solid #e2e8f0; padding: 20px; margin-top: 20px; box-shadow: 0 4px 16px rgba(0,0,0,0.06); }
.results-title { font-size: 16px; font-weight: 700; color: ", SLAM_BLUE, "; margin-bottom: 14px; }
.tab-success { background: #ecfdf5; border-radius: 10px; padding: 16px; margin-top: 12px; border: 1px solid #a7f3d0; }
.panel { border-radius: 10px; border: 1px solid #ddd; box-shadow: 0 2px 4px rgba(0,0,0,0.05); }
.panel-heading { background: ", SLAM_BLUE, "; color: white; border-radius: 10px 10px 0 0; font-weight: 600; padding: 12px 16px; }
.panel-body { background: white; padding: 16px; }
.form-control { border-radius: 6px; border: 1px solid #ccc; }
.btn-primary { background: ", SLAM_BLUE, "; border-color: ", SLAM_BLUE, "; font-weight: 600; }
.btn-primary:hover { background: #1452A3; border-color: #1452A3; }
::-webkit-scrollbar { width: 8px; }
::-webkit-scrollbar-track { background: ", SLAM_GRAY, "; }
::-webkit-scrollbar-thumb { background: ", SLAM_BLUE, "; border-radius: 4px; }
"))}

# ─────────────────────────────────────────────────────────────
# 4. UI — CELF-5 tabPanel layout pattern
# ─────────────────────────────────────────────────────────────
ui <- fluidPage(
  theme = bslib::bs_theme(version = 5, primary = SLAM_BLUE, secondary = SLAM_GOLD),
  tags$head(tags$style(slam_css())),

  # Title
  titlePanel(
    div(h2("SLAM 叙事评估 / Narrative Assessment",
            style = sprintf("color:%s; margin:0; font-weight:700;", SLAM_BLUE)),
        p("Structured Language Assessment Measures — 图片叙事 · 词找 · 语法填空 · 自由叙事",
          style = "color:#888; font-size:14px; margin:0;")),
    windowTitle = "SLAM"
  ),

  # Back-to-home link
  div(
    style = "padding: 10px 20px 0;",
    actionLink("slam_btn_back_home", "‹ Back to SLP Homepage",
               style = sprintf("color:%s; font-weight:600; font-size:14px; text-decoration:none; cursor:pointer;", SLAM_BLUE),
               onclick = "window.location.href='http://www.zhangyunhai.com:3838/slp/';"),
    hr(style = sprintf("margin:8px 0 0; border-top:1px solid %s;", SLAM_BLUE))
  ),

  # Main tabset — matches CELF-5 pattern
  tabsetPanel(id = "slam_main_tabs", type = "tabs",

    # ═══════════════════════════════════════════════════════════
    # TAB 1: Subject Info — form + DT assessment history
    # ═══════════════════════════════════════════════════════════
    tabPanel("受试者信息 / Subject Info",
      fluidRow(
        # Left: Subject info form (4 cols)
        column(4,
          div(class = "panel",
            div(class = "panel-heading", "基本信息 / Basic Info"),
            div(class = "panel-body",
              textInput("slam_patient_name", "姓名 * / Name *", placeholder = "受试者姓名"),
              selectInput("slam_patient_gender", "性别 / Gender",
                choices = c("—" = "",
                            "男 / Male"   = "M",
                            "女 / Female" = "F"),
                selected = "", width = "100%"),
              textInput("slam_school_name", "学校 / School", placeholder = "就读学校"),
              textInput("slam_grade_level", "年级 / Grade", placeholder = "如：小一、初二、高一"),
              textInput("slam_examiner", "评估师 / Examiner", placeholder = "评估师姓名"),
              dateInput("slam_dob", "出生日期 * / Date of Birth *", format = "yyyy-mm-dd", value = NA),
              dateInput("slam_assessment_date", "评估日期 * / Assessment Date *",
                format = "yyyy-mm-dd", value = Sys.Date()),
              actionButton("slam_start_assessment", "▶ 开始评估 / Start Assessment",
                class = "btn-primary",
                style = sprintf("width:100%%; background:%s; border-color:%s;", SLAM_BLUE, SLAM_BLUE))
            )
          )
        ),
        # Right: Assessment history table (8 cols)
        column(8,
          div(class = "panel",
            div(class = "panel-heading", "SLAM 历史评估记录 / SLAM Assessment History"),
            div(class = "panel-body",
              selectInput("slam_filter_status", "筛选状态 / Filter Status",
                choices = c("全部 / All" = "all",
                            "进行中 / In Progress" = "in_progress",
                            "已完成 / Complete" = "complete"),
                selected = "all", width = "40%"),
              DT::dataTableOutput("slam_assessments_dt"),
              uiOutput("slam_load_assessment_btn")
            )
          )
        )
      )
    ),

    # ═══════════════════════════════════════════════════════════
    # TAB 2: Baseball Troubles
    # ═══════════════════════════════════════════════════════════
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
          story_img_carousel("baseball_troubles", 6),

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
            blank_p <- gsub("___", sprintf('<span class="blank">___%d</span>', i), gfa$passage_en[i])
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

    # ═══════════════════════════════════════════════════════════
    # TAB 3: The Best Turkey
    # ═══════════════════════════════════════════════════════════
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
          story_img_carousel("the_best_turkey", 5),
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
          div(class = "section-label", "📝 GFA 语法填空"),
          lapply(1:4, function(i) {
            gfa <- STORIES$the_best_turkey$gfa_items
            ms <- gfa$max_score[i]
            choices <- c("—"="", setNames(as.character(ms:0), paste0(ms:0, "分")))
            div(class = "gfa-item", id = sprintf("gfa_tbt_%d", i),
              div(class = "gfa-passage",
                HTML(gsub("___", sprintf('<span class="blank">___%d</span>', i), gfa$passage_en[i])),
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
          div(class = "section-label", "🎤 Free Narrative"),
          div(class = "narrative-box",
            p(strong("指示 Instruction: "), "请学生看着图片讲述故事。"),
            textAreaInput("narr_tbt", "学生回答 / Student Response:", width = "100%", rows = 5, placeholder = "学生在评估时的叙事内容..."),
            p(style = "margin-top: 10px; font-size: 13px; color: #6b7280;", "📝 请评估者记录学生叙事并评分。")
          ),
          div(class = "section-label", "📊 Narrative Rubric"),
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

    # ═══════════════════════════════════════════════════════════
    # TAB 4: The Girl Who Loved Horses
    # ═══════════════════════════════════════════════════════════
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
          story_img_carousel("the_girl_who_loved_horses", 6),
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
          div(class = "section-label", "📝 GFA 语法填空"),
          lapply(1:4, function(i) {
            gfa <- STORIES$the_girl_who_loved_horses$gfa_items
            ms <- gfa$max_score[i]
            choices <- c("—"="", setNames(as.character(ms:0), paste0(ms:0, "分")))
            div(class = "gfa-item", id = sprintf("gfa_gwh_%d", i),
              div(class = "gfa-passage",
                HTML(gsub("___", sprintf('<span class="blank">___%d</span>', i), gfa$passage_en[i])),
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
          div(class = "section-label", "🎤 Free Narrative"),
          div(class = "narrative-box",
            p(strong("指示 Instruction: "), "请学生看着图片讲述故事。"),
            textAreaInput("narr_gwh", "学生回答 / Student Response:", width = "100%", rows = 5, placeholder = "学生在评估时的叙事内容..."),
            p(style = "margin-top: 10px; font-size: 13px; color: #6b7280;", "📝 请评估者记录学生叙事并评分。")
          ),
          div(class = "section-label", "📊 Narrative Rubric"),
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

    # ═══════════════════════════════════════════════════════════
    # TAB 5: Wallace and Batty
    # ═══════════════════════════════════════════════════════════
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
          story_img_carousel("wallace_and_batty", 5),
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
          div(class = "section-label", "📝 GFA 语法填空"),
          lapply(1:4, function(i) {
            gfa <- STORIES$wallace_and_batty$gfa_items
            ms <- gfa$max_score[i]
            choices <- c("—"="", setNames(as.character(ms:0), paste0(ms:0, "分")))
            div(class = "gfa-item", id = sprintf("gfa_wb_%d", i),
              div(class = "gfa-passage",
                HTML(gsub("___", sprintf('<span class="blank">___%d</span>', i), gfa$passage_en[i])),
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
          div(class = "section-label", "🎤 Free Narrative"),
          div(class = "narrative-box",
            p(strong("指示 Instruction: "), "请学生看着图片讲述故事。"),
            textAreaInput("narr_wb", "学生回答 / Student Response:", width = "100%", rows = 5, placeholder = "学生在评估时的叙事内容..."),
            p(style = "margin-top: 10px; font-size: 13px; color: #6b7280;", "📝 请评估者记录学生叙事并评分。")
          ),
          div(class = "section-label", "📊 Narrative Rubric"),
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
    ),

    # ═══════════════════════════════════════════════════════════
    # TAB 6: AI Report
    # ═══════════════════════════════════════════════════════════
    tabPanel("🤖 AI 综合报告 / AI Report",
      div(class = "story-card",
        div(class = "story-card-header",
          span("🤖"), "AI 综合报告 / Hybrid CELF-5 + SLAM Report",
          span(class = "progress-story", "选择学生 → 生成综合报告")
        ),
        div(class = "story-card-body",
          p(strong("选择学生 / Select Student:"), " 点击下方表格选择一位有SLAM记录的学生",
            style = "margin-bottom: 16px; font-size: 14px; color: #374151;"),
          DT::dataTableOutput("slam_patient_dt"),
          hr(),
          uiOutput("slam_ai_report_content")
        )
      )
    )

  ),  # end tabsetPanel

  # Footer
  div(style = "text-align: center; margin-top: 36px; padding: 20px; color: #aaa; font-size: 13px;",
    span(style = sprintf("color: %s; font-weight: 600;", SLAM_BLUE), "SLAM"),
    "© 2026  |  Columbia University Leaders Project — Free for Copying and Distribution  |  ",
    "Powered by R Shiny"
  )
)

# ─────────────────────────────────────────────────────────────
# 5. Server
# ─────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  # ── Reactive: current student info from Subject Info tab ────
  student_info <- reactive({
    list(
      name   = trim(input$slam_patient_name %||% ""),
      gender = input$slam_patient_gender %||% "",
      dob    = input$slam_dob,
      date   = as.character(input$slam_assessment_date %||% Sys.Date())
    )
  })

  # ── SLAM Assessment History DT (Subject Info tab) ────────────
  output$slam_assessments_dt <- DT::renderDataTable({
    filter_status <- input$slam_filter_status %||% "all"
    con <- get_con()
    on.exit(dbDisconnect(con), add = TRUE)

    if (filter_status == "all") {
      status_clause <- ""
    } else {
      status_clause <- sprintf("AND a.status = '%s'", filter_status)
    }

    sql <- sprintf("
      SELECT a.id, p.name, p.dob, p.gender,
             a.assessment_date, a.age_years, a.status, a.assessment_type
      FROM assessments a
      JOIN patients p ON a.patient_id = p.id
      WHERE a.assessment_type = 'SLAM' %s
      ORDER BY a.assessment_date DESC", status_clause)

    df <- dbGetQuery(con, sql)
    if (nrow(df) == 0) {
      return(DT::datatable(data.frame(
        Message = "暂无SLAM记录 / No SLAM records yet"
      ), options = list(dom = "t")))
    }

    df$gender_display <- sapply(df$gender, function(g) {
      switch(g, M = "男 / M", F = "女 / F", "—")
    })

    DT::datatable(
      df[, c("name","dob","gender_display","assessment_date","age_years","status")],
      colnames = c("姓名" = "name", "出生日期" = "dob", "性别" = "gender_display",
                   "评估日期" = "assessment_date", "年龄" = "age_years", "状态" = "status"),
      selection = "single",
      options = list(
        pageLength = 10,
        dom = "frtip",
        language = list(emptyTable = "暂无SLAM记录 / No SLAM records yet")
      )
    )
  })

  # ── Load assessment button (when row selected) ───────────────
  output$slam_load_assessment_btn <- renderUI({
    req(input$slam_assessments_dt_rows_selected)
    tagList(
      hr(),
      actionButton("slam_load_btn", "📂 加载选中评估 / Load Selected Assessment",
        class = "btn-primary",
        style = sprintf("background:%s; border-color:%s;", SLAM_BLUE, SLAM_BLUE))
    )
  })

  # ── Load assessment — populate form ──────────────────────────
  observeEvent(input$slam_load_btn, {
    req(input$slam_assessments_dt_rows_selected)
    con <- get_con()
    on.exit(dbDisconnect(con), add = TRUE)

    filter_status <- input$slam_filter_status %||% "all"
    if (filter_status == "all") {
      status_clause <- ""
    } else {
      status_clause <- sprintf("AND a.status = '%s'", filter_status)
    }
    sql <- sprintf("
      SELECT a.id, p.name, p.dob, p.gender, p.school, p.grade, p.examiner,
             a.assessment_date, a.age_years, a.status
      FROM assessments a
      JOIN patients p ON a.patient_id = p.id
      WHERE a.assessment_type = 'SLAM' %s
      ORDER BY a.assessment_date DESC", status_clause)
    df <- dbGetQuery(con, sql)

    row_idx <- input$slam_assessments_dt_rows_selected[1]
    if (row_idx > nrow(df)) return()

    row <- df[row_idx, ]
    updateTextInput(session, "slam_patient_name", value = row$name %||% "")
    updateSelectInput(session, "slam_patient_gender", selected = row$gender %||% "")
    updateTextInput(session, "slam_school_name", value = row$school %||% "")
    updateTextInput(session, "slam_grade_level", value = row$grade %||% "")
    updateTextInput(session, "slam_examiner", value = row$examiner %||% "")

    if (!is.na(row$dob) && nzchar(row$dob)) {
      updateDateInput(session, "slam_dob", value = as.Date(row$dob))
    }
    if (!is.na(row$assessment_date) && nzchar(row$assessment_date)) {
      updateDateInput(session, "slam_assessment_date", value = as.Date(row$assessment_date))
    }

    showNotification(
      tagList(icon("check-circle"), sprintf(" 已加载评估 ID %d — %s", row$id, row$name)),
      type = "message", duration = 3
    )
  })

  # ── Start Assessment button ──────────────────────────────────
  observeEvent(input$slam_start_assessment, {
    si <- student_info()
    if (si$name == "") {
      showNotification(
        tagList(icon("exclamation-triangle"), "请输入学生姓名 / Please enter student name"),
        type = "error", duration = 4
      )
      return()
    }
    if (is.null(si$dob) || is.na(si$dob)) {
      showNotification(
        tagList(icon("exclamation-triangle"), "请输入出生日期 / Please enter date of birth"),
        type = "error", duration = 4
      )
      return()
    }
    # Switch to first story tab
    updateTabsetPanel(session, "slam_main_tabs", selected = "🏇 Baseball Troubles")
    showNotification(
      tagList(icon("check-circle"), sprintf(" 已开始评估: %s", si$name)),
      type = "message", duration = 3
    )
  })

  # ── Scoring helpers ──────────────────────────────────────────

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
    si <- student_info()
    age_years <- if (!is.null(input$slam_assessment_date) && !is.na(si$dob)) {
      calc_age <- function(dob, ad) {
        dob <- as.Date(dob); ad <- as.Date(ad)
        years <- year(ad) - year(dob)
        months <- month(ad) - month(dob)
        days <- day(ad) - day(dob)
        if (days < 0) { months <- months - 1; days <- days + 30 }
        if (months < 0) { years <- years - 1; months <- months + 12 }
        as.integer(years)
      }
      calc_age(si$dob, si$date)
    } else {
      10L
    }
    if (is.na(age_years)) age_years <- 10L

    wf_raw   <- calc_wf_raw(wf_prefix, n_wf)
    gfa_raw  <- calc_gfa_raw(gfa_prefix, n_gfa)
    narr_raw <- calc_narr_rubric(narr_prefix)

    wf_std  <- get_slam_standard_score(wf_raw,  "word_finding", age_years)
    gfa_std <- get_slam_standard_score(gfa_raw, "gfa",          age_years)

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

  # ── Get or create patient ────────────────────────────────────
  get_or_create_patient <- function(si) {
    con <- get_con()
    on.exit(dbDisconnect(con), add = TRUE)
    dob_str <- if (!is.null(si$dob) && !is.na(si$dob)) {
      as.character(si$dob)
    } else {
      as.character(Sys.Date())
    }
    pid <- dbGetQuery(con,
      "SELECT id FROM patients WHERE name = ? AND dob = ? LIMIT 1",
      params = list(si$name, dob_str))$id[1]
    if (is.na(pid)) {
      dbExecute(con,
        "INSERT INTO patients (name, dob, gender, examiner, notes) VALUES (?, ?, ?, ?, ?)",
        params = list(si$name, dob_str, si$gender, input$slam_examiner %||% "", ""))
      pid <- dbGetQuery(con, "SELECT last_insert_rowid() as id")$id[1]
    }
    pid
  }

  # ── Save individual story ────────────────────────────────────
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

      pid <- get_or_create_patient(si)

      # Calculate age
      age_years <- if (!is.null(si$dob) && !is.na(si$dob)) {
        calc_age <- function(dob, ad) {
          dob <- as.Date(dob); ad <- as.Date(ad)
          years <- year(ad) - year(dob)
          months <- month(ad) - month(dob)
          days <- day(ad) - day(dob)
          if (days < 0) { months <- months - 1; days <- days + 30 }
          if (months < 0) { years <- years - 1; months <- months + 12 }
          as.integer(years)
        }
        calc_age(si$dob, si$date)
      } else { 10L }
      if (is.na(age_years)) age_years <- 10L

      ag <- sprintf("%d:0-%d:11", floor(age_years), floor(age_years) + 1)

      dbExecute(con,
        "INSERT INTO assessments (patient_id, assessment_date, age_years, age_group, status, assessment_type)
         VALUES (?, ?, ?, ?, 'in_progress', 'SLAM')",
        params = list(pid, si$date, age_years, ag))

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

  # ── Save buttons ─────────────────────────────────────────────
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

  # ── AI Report — Patient DT ──────────────────────────────────
  output$slam_patient_dt <- DT::renderDataTable({
    con <- get_con()
    on.exit(dbDisconnect(con), add = TRUE)
    patients_df <- dbGetQuery(con, "
      SELECT DISTINCT p.id, p.name, p.dob, p.gender, p.examiner,
             MAX(a.assessment_date) AS most_recent_date
      FROM patients p
      LEFT JOIN assessments a ON a.patient_id = p.id
      GROUP BY p.id, p.name, p.dob
      ORDER BY most_recent_date DESC")
    if (nrow(patients_df) == 0) {
      return(DT::datatable(data.frame(
        Message = "暂无SLAM记录 / No SLAM records yet"
      ), options = list(dom = "t")))
    }
    DT::datatable(patients_df[, c("name", "dob", "most_recent_date")],
      colnames = c("姓名 / Name" = "name", "出生日期 / DOB" = "dob",
                   "最近评估日期 / Most Recent" = "most_recent_date"),
      selection = "single",
      options = list(
        pageLength = 10,
        dom = "frtip",
        language = list(emptyTable = "暂无SLAM记录 / No SLAM records yet")
      ))
  })

  # ── AI Report — Selected Patient Report ─────────────────────
  selected_slam_pid <- reactive({
    input$slam_patient_dt_rows_selected
  })

  output$slam_ai_report_content <- renderUI({
    req(selected_slam_pid())
    con <- get_con()
    on.exit(dbDisconnect(con), add = TRUE)
    patients_df <- dbGetQuery(con, "
      SELECT DISTINCT p.id, p.name, p.dob
      FROM patients p
      JOIN assessments a ON a.patient_id = p.id
      WHERE a.assessment_type = 'SLAM'
      ORDER BY MAX(a.assessment_date) DESC")
    if (length(selected_slam_pid()) == 0 || selected_slam_pid()[1] > nrow(patients_df)) {
      return(div("请在上方表格中选择一位学生 / Please select a student from the table above."))
    }
    pid <- patients_df$id[selected_slam_pid()[1]]
    pname <- patients_df$name[selected_slam_pid()[1]]
    pdob <- patients_df$dob[selected_slam_pid()[1]]

    all_data <- dbGetQuery(con, "
      SELECT a.*, p.name AS patient_name, p.dob, p.gender
      FROM assessments a
      JOIN patients p ON a.patient_id = p.id
      WHERE a.patient_id = ? AND a.assessment_type IN ('CELF5','SLAM')
      ORDER BY a.assessment_date DESC",
      params = list(pid))

    if (nrow(all_data) == 0) {
      return(div("该患者没有评估记录 / No assessment records found for this patient."))
    }

    slam_data <- all_data[all_data$assessment_type == "SLAM", ]
    celf_data <- all_data[all_data$assessment_type == "CELF5", ]

    report_parts <- list()

    report_parts[[length(report_parts) + 1]] <- div(
      style = sprintf("background: linear-gradient(135deg, %s 0%%, #2a5ab3 100%%); color: white; border-radius: 14px; padding: 24px; margin-bottom: 20px;"),
      h3(sprintf("综合评估报告 / Comprehensive Assessment Report: %s", pname), style = "margin:0 0 8px; color: white;"),
      p(sprintf("出生日期 DOB: %s | Patient ID: %d", pdob, pid), style = "margin:0; opacity: 0.85;")
    )

    if (nrow(slam_data) > 0) {
      slam_ids <- slam_data$id
      slam_scores <- dbGetQuery(con, "
        SELECT ss.* FROM subtest_scores ss
        WHERE ss.assessment_id IN ({paste(slam_ids, collapse=',')})",
        params = list())
      slam_narratives <- dbGetQuery(con, "
        SELECT r.* FROM responses r
        WHERE r.assessment_id IN ({paste(slam_ids, collapse=',')}) AND r.subtest LIKE '%_Narrative'",
        params = list())

      slam_block <- div(style = "margin-bottom: 24px;",
        h4(sprintf("📊 SLAM 叙事评估 / SLAM Narrative Assessment (%d次评估)", nrow(slam_data)),
           style = sprintf("color: %s; border-bottom: 2px solid %s; padding-bottom: 8px;", SLAM_BLUE, SLAM_GOLD)),
        lapply(seq_len(nrow(slam_data)), function(i) {
          aid <- slam_data$id[i]
          adate <- slam_data$assessment_date[i]
          scores_i <- slam_scores[slam_scores$assessment_id == aid, ]
          narratives_i <- slam_narratives[slam_narratives$assessment_id == aid, ]
          div(style = "background: #f8fafc; border-radius: 10px; padding: 16px; margin-bottom: 12px; border: 1px solid #e2e8f0;",
            strong(sprintf("评估日期: %s | 年龄: %s岁", adate, slam_data$age_years[i] %||% "—")), br(),
            if (nrow(scores_i) > 0) {
              tagList(
                lapply(seq_len(nrow(scores_i)), function(si) {
                  row <- scores_i[si, ]
                  div(style = "display: inline-block; margin-right: 16px;",
                    span(class = "score-badge badge-raw",
                      sprintf("%s: %d分", sub("^[^_]*_", "", row$subtest), row$raw_score %||% 0)),
                    if (!is.na(row$scaled_score)) {
                      span(class = "score-badge badge-std",
                        sprintf("标准分: %d", row$scaled_score))
                    }
                  )
                })
              )
            },
            if (nrow(narratives_i) > 0 && nzchar(narratives_i$response_text[1] %||% "")) {
              div(style = "margin-top: 10px; padding: 10px; background: #fefce8; border-radius: 8px; border-left: 3px solid #C8A951;",
                strong("自由叙事文本 / Narrative: "), br(),
                span(style = "font-style: italic; color: #374151;", substr(narratives_i$response_text[1] %||% "", 1, 300))
              )
            }
          )
        })
      )
      report_parts[[length(report_parts) + 1]] <- slam_block
    }

    if (nrow(celf_data) > 0) {
      celf_block <- div(style = "margin-bottom: 24px;",
        h4(sprintf("📋 CELF-5 语言评估 / CELF-5 Language Assessment (%d次评估)", nrow(celf_data)),
           style = sprintf("color: %s; border-bottom: 2px solid %s; padding-bottom: 8px;", SLAM_BLUE, SLAM_GOLD)),
        lapply(seq_len(nrow(celf_data)), function(i) {
          adate <- celf_data$assessment_date[i]
          div(style = "background: #f8fafc; border-radius: 10px; padding: 16px; margin-bottom: 12px; border: 1px solid #e2e8f0;",
            strong(sprintf("评估日期: %s | 年龄: %s岁", adate, celf_data$age_years[i] %||% "—"))
          )
        })
      )
      report_parts[[length(report_parts) + 1]] <- celf_block
    }

    if (length(report_parts) == 1) {
      report_parts[[length(report_parts) + 1]] <- div(
        style = "color: #6b7280; font-style: italic;",
        "暂无详细分数记录 / No detailed score records found."
      )
    }

    tagList(report_parts)
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
  if (is.na(std) || std < 50) return(NA_real_)
  pnorm((std - 100) / 15) * 100
}

# ─────────────────────────────────────────────────────────────
# Run App
# ─────────────────────────────────────────────────────────────
shinyApp(ui = ui, server = server)
