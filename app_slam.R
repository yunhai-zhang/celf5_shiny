# app_slam.R — SLAM Narrative Assessment Tool
# Stories: Baseball Troubles / The Ball Mystery / Lost Cellphone / Kittens Love Milk Cards
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
SLAM_BLUE   <- "#EA580C"
SLAM_GOLD   <- "#F59E0B"
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
      item = 1:7,
      passage_en = c(
        "Will the big boys share their ball with the little boys? How do you know?",
        "What are the little boys thinking here?",
        "Why don't the big boys see that the little boys are playing with the ball?",
        "What are the big boys thinking now?",
        "What do you think the big boys will do if the ball falls out of the shirt?",
        "What would you say to try to get out of trouble if you were the little boy?",
        "Have you ever gotten into big trouble like this? What happened? How did you get out of it?"
      ),
      passage_zh = c(
        "大男孩会把球分享给小男孩吗？你怎么知道？",
        "小男孩在这里想什么？",
        "为什么大男孩没注意到小男孩在玩球？",
        "大男孩现在在想什么？",
        "你觉得如果球从衣服里掉出来，大男孩会怎么做？",
        "如果你是那个被冤枉的小男孩，你会怎么说来摆脱麻烦？",
        "你有没有遇到过类似的冤枉？你是怎么解决的？"
      ),
      answers = lapply(1:7, function(i) character(0)),
      max_score = 2
    ),
    narrative_rubric = list(
      dimensions = c("叙事结构 Narrative Structure","复杂句 Complex Clauses",
                     "推论能力 Inferencing","语用 Pragmatic","理论心智 Theory of Mind"),
      max_per_dim = 2
    )
  ),

  the_ball_mystery = list(
    id   = "the_ball_mystery",
    name = "The Ball Mystery",
    name_zh = "神秘小球",
    age_range = "10-14岁",
    n_images = 5,
    pdf_path = "/tmp/slam_extract/SLAM/SLAM/SLAM sets/Junior High to High School SLAM/2. SLAM The Ball Mystery_English.pdf",
    gfa_path = "/tmp/slam_extract/SLAM/SLAM/SLAM sets/Junior High to High School SLAM/2. GFA - The Ball Mystery.pdf",
    synopsis = "几个大男孩把球塞进衣服里假装肚子大，小男孩们偷偷拿球玩了起来。\nSome big boys tuck a ball under their shirts pretending they have big bellies. The little boys secretly take the ball to play.",
    word_finding = tibble(
      item   = 1:5,
      prompt_en = c(
        "What is this?",  # ball
        "What is this?",  # shirt
        "What is this?",  # boy
        "What is this?",  # big/belly
        "What is this?"   # stairs/ground
      ),
      prompt_zh = c(
        "这是什么？", # ball
        "这是什么？", # shirt
        "这是什么？", # boy
        "这是什么？", # 大/肚子
        "这是什么？"  # 楼梯/地面
      ),
      acceptable_en = list(
        c("ball","soccer ball","football"),
        c("shirt","clothing","top"),
        c("boy","kid","little boy"),
        c("big","large","belly","stomach"),
        c("stairs","steps","ground","floor")
      )
    ),
    gfa_items = tibble(
      item = 1:7,
      passage_en = c(
        "Will the big boys share their ball with the little boys? How do you know?",
        "What are the little boys thinking here?",
        "Why don't the big boys see that the little boys are playing with the ball?",
        "What are the big boys thinking now?",
        "What do you think the big boys will do if the ball falls out of the shirt?",
        "What would you say to try to get out of trouble if you were the little boy?",
        "Have you ever gotten into big trouble like this? What happened?"
      ),
      passage_zh = c(
        "大男孩会把球分享给小男孩吗？你怎么知道？",
        "小男孩们在这里想什么？",
        "为什么大男孩没看到小男孩们在玩球？",
        "现在大男孩在想什么？",
        "如果球从衣服里掉出来，你觉得大男孩会怎么做？",
        "如果你是那个小男孩，你会说什么来摆脱麻烦？",
        "你有没有遇到过这样的麻烦？发生了什么？"
      ),
      answers = list(c("no","won't share"), c("they want to play","excited","the ball is fun"), c("they are distracted","looking elsewhere","not paying attention"), c("they think they still have the ball","they don't know it's gone"), c("they will be embarrassed","they will look for it","they will be surprised"), c("make an excuse","blame someone else","tell the truth"), c("personal narrative","yes or no with explanation")),
      max_score = 2
    ),
    narrative_rubric = list(
      dimensions = c("叙事结构 Narrative Structure","复杂句 Complex Clauses",
                     "推论能力 Inferencing","语用 Pragmatic","理论心智 Theory of Mind"),
      max_per_dim = 2
    )
  ),

  lost_cellphone = list(
    id   = "lost_cellphone",
    name = "Lost Cellphone",
    name_zh = "丢失的手机",
    age_range = "13-17岁",
    n_images = 3,
    pdf_path = "/tmp/slam_extract/SLAM/SLAM/SLAM sets/Junior High to High School SLAM/3. SLAM Lost Cellphone_English.pdf",
    gfa_path = "/tmp/slam_extract/SLAM/SLAM/SLAM sets/Junior High to High School SLAM/3. GFA - Lost Cellphone.pdf",
    synopsis = "一个男孩在便利店结账时被女孩分散了注意力，手机忘在柜台上被后面的人拿走。\nA boy gets distracted by a girl at the store and leaves his cellphone on the counter. Someone behind him takes it.",
    word_finding = tibble(
      item   = 1:6,
      prompt_en = c(
        "What is this?",  # cellphone
        "What is this?",  # counter
        "What is this?",  # store/shop
        "What is this?",  # girl/woman
        "What is this?",  # receipt
        "What is this?"   # person behind
      ),
      prompt_zh = c(
        "这是什么？", # cellphone
        "这是什么？", # counter
        "这是什么？", # store/shop
        "这是什么？", # girl/woman
        "这是什么？", # receipt
        "这是什么？"  # person behind
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
      item = 1:9,
      passage_en = c(
        "Can you put these in order?",
        "Tell me the story of what happened.",
        "How did the boy lose his cellphone?",
        "Why did he leave his cellphone?",
        "What made him remember he forgot his cellphone?",
        "What is he thinking here?",
        "What does he think will happen when he goes back to the store?",
        "What is going to happen when he goes back to the store?",
        "Did anything like this ever happen to you?"
      ),
      passage_zh = c(
        "你能把这些按顺序排好吗？",
        "告诉我发生了什么。",
        "男孩是怎么丢失手机的？",
        "他为什么把手机落在那里？",
        "什么让他想起自己忘了带手机？",
        "他现在在想什么？",
        "他觉得回到店里会发生什么？",
        "当他回到店里时会发生什么？",
        "你有没有遇到过类似的事情？"
      ),
      answers = list(c("ordering task - 0 pts"), c("narrative retelling"), c("he put it down and forgot it","distracted by girl"), c("he was distracted","looking at girl","paying attention to something else"), c("reached for it","couldn't find it","checked pocket"), c("worried","upset","I forgot my phone"), c("he might get it back","the person will be gone","he hopes"), c("he won't get it back","phone is gone","the thief leaves"), c("personal narrative")),
      max_score = 2
    ),
    narrative_rubric = list(
      dimensions = c("叙事结构 Narrative Structure","复杂句 Complex Clauses",
                     "推论能力 Inferencing","语用 Pragmatic","理论心智 Theory of Mind"),
      max_per_dim = 2
    )
  ),

  kittens_love_milk = list(
    id   = "kittens_love_milk",
    name = "Kittens Love Milk Cards",
    name_zh = "小猫爱牛奶",
    age_range = "7-14岁",
    n_images = 3,
    pdf_path = "/tmp/slam_extract/SLAM/SLAM/SLAM sets/Junior High to High School SLAM/4. SLAM Kittens Love Milk Cards (English).pdf",
    gfa_path = NULL,
    synopsis = "一个女人买完东西上楼时，口袋里装了一只小猫她自己却不知道。\nA woman carrying groceries goes upstairs when a little kitten secretly jumps into her bag.",
    word_finding = tibble(
      item   = 1:5,
      prompt_en = c(
        "What is this?",  # kitten/cat
        "What is this?",  # milk
        "What is this?",  # woman
        "What is this?",  # bag
        "What is this?"   # stairs
      ),
      prompt_zh = c(
        "这是什么？", # kitten/cat
        "这是什么？", # milk
        "这是什么？", # woman
        "这是什么？", # bag
        "这是什么？"  # stairs
      ),
      acceptable_en = list(
        c("kitten","cat","kitty","feline"),
        c("milk","spilled milk","milk puddle"),
        c("woman","lady","mom","mother"),
        c("bag","grocery bag","shopping bag","sack"),
        c("stairs","steps","staircase")
      )
    ),
    gfa_items = tibble(
      item = 1:8,
      passage_en = c(
        "Can you put these in order with me?",
        "Tell me the story",
        "What are the kittens thinking here?",
        "Why don't these cats (the ones with filled bellies) follow the woman up the stairs?",
        "What was the little kitten's idea when she followed the woman up the stairs and then jumped into her bag?",
        "Why doesn't the woman know that the kitten is in her bag?",
        "What do you think the woman is going to do now that she sees the kitten in her bag?",
        "What would you do if you found a kitten in your grocery bag?"
      ),
      passage_zh = c(
        "你能和我一起把这些按顺序排好吗？",
        "给我讲讲这个故事",
        "小猫们在这里想什么？",
        "为什么这些猫（吃饱了的）不跟着女人上楼？",
        "当小猫跟着女人上楼然后跳进她的包里时，它有什么主意？",
        "为什么女人不知道小猫在她的包里？",
        "当她看到包里有小猫时，你觉得她会怎么做？",
        "如果你在购物袋里发现一只小猫，你会怎么做？"
      ),
      answers = list(c("ordering task - 0 pts"), c("narrative"), c("they want to drink milk","milk is spilling","hungry for milk"), c("they are full","they already ate","satisfied"), c("hide in bag","get free food","go for a ride","sneak out"), c("she wasn't looking","she didn't see","she didn't feel it","bag is dark"), c("surprised","laugh","take kitten out","keep it","give it milk"), c("keep it","feed it","return it","call owner","take to shelter")),
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
.slam-hero { background: linear-gradient(135deg, ", SLAM_BLUE, " 0%, #b45309 100%); color: white; border-radius: 18px; padding: 36px 40px; margin-bottom: 28px; box-shadow: 0 8px 30px rgba(217,119,6,0.25); }
.slam-hero h2 { color: white; font-size: 28px; font-weight: 700; margin: 0 0 6px; }
.slam-hero p  { color: rgba(255,255,255,0.82); font-size: 14px; margin: 0; }
.story-card { background: white; border-radius: 16px; border: 1.5px solid #e2e8f0; box-shadow: 0 4px 16px rgba(0,0,0,0.06); margin-bottom: 24px; overflow: hidden; }
.story-card-header { background: linear-gradient(135deg, ", SLAM_BLUE, " 0%, #b45309 100%); color: white; padding: 16px 24px; font-size: 17px; font-weight: 600; display: flex; align-items: center; gap: 10px; }
.story-card-body { padding: 24px; }
.synopsis-box { background: #FEF3C7; border-left: 4px solid #F59E0B; border-radius: 8px; padding: 14px 18px; margin-bottom: 20px; font-size: 14px; color: #374151; line-height: 1.7; }
.section-label { font-size: 13px; font-weight: 700; color: ", SLAM_BLUE, "; text-transform: uppercase; letter-spacing: 0.8px; margin-bottom: 14px; padding-bottom: 6px; border-bottom: 2px solid ", SLAM_BLUE, "; }
.wf-item, .gfa-item { background: #f8fafc; border-radius: 10px; padding: 16px; margin-bottom: 14px; border: 1px solid #e2e8f0; }
.wf-prompt { font-size: 15px; font-weight: 600; color: ", SLAM_BLUE, "; margin-bottom: 10px; }
.gfa-passage { background: linear-gradient(135deg, #f0f4fa 0%, #e8ecf3 100%); border-radius: 10px; padding: 18px 20px; margin-bottom: 12px; font-size: 15px; color: #1e293b; line-height: 1.9; font-style: italic; }
.gfa-passage .blank { color: ", SLAM_BLUE, "; font-weight: 700; text-decoration: none; border-bottom: 2px dashed #F59E0B; }
.rubric-row { display: flex; gap: 10px; flex-wrap: wrap; margin-bottom: 10px; }
.rubric-btn { flex: 1; min-width: 60px; }
.btn-save-slam { background: linear-gradient(135deg, ", SLAM_BLUE, " 0%, #b45309 100%); color: white; border: none; border-radius: 10px; padding: 13px 32px; font-size: 15px; font-weight: 600; transition: all 0.2s ease; }
.btn-save-slam:hover { transform: translateY(-1px); box-shadow: 0 6px 20px rgba(27,58,107,0.35); color: white; }
.btn-save-slam:disabled { background: #ccc; transform: none; box-shadow: none; }
.score-badge { display: inline-block; padding: 5px 14px; border-radius: 20px; font-size: 13px; font-weight: 700; margin: 2px; }
.badge-raw   { background: #FEF3C7; color: ", SLAM_BLUE, "; }
.badge-std   { background: #fff3e0; color: #e65100; }
.badge-pr    { background: #e8f5e9; color: #2e7d32; }
.progress-story { font-size: 12px !important; color: rgba(255,255,255,0.7) !important; }
.nav-btn { background: white; color: ", SLAM_BLUE, "; border: 2px solid ", SLAM_BLUE, "; border-radius: 25px; padding: 8px 22px; font-size: 13px; font-weight: 600; transition: all 0.2s ease; cursor: pointer; }
.nav-btn:hover { background: ", SLAM_BLUE, "; color: white; border-color: ", SLAM_BLUE, "; }
.nav-btn.active { background: ", SLAM_BLUE, "; color: white; border-color: ", SLAM_BLUE, "; }
.story-carousel { display: flex; gap: 12px; overflow-x: auto; padding: 10px 0; scroll-snap-type: x mandatory; -webkit-overflow-scrolling: touch; }
.carousel-slide { flex: 0 0 auto; scroll-snap-align: start; }
.story-img { height: 260px; width: auto; border-radius: 10px; border: 2px solid #e2e8f0; object-fit: cover; box-shadow: 0 4px 12px rgba(0,0,0,0.1); }
.narrative-box { background: #FEF3C7; border: 1.5px solid #FDE68A; border-radius: 10px; padding: 18px; margin-top: 14px; }
textarea.form-control { border-radius: 10px; border: 1.5px solid #d0d7e2; padding: 12px 14px; font-size: 14px; }
textarea.form-control:focus { border-color: ", SLAM_BLUE, "; box-shadow: 0 0 0 3px rgba(217,119,6,0.1); }
.rubric-dim-label { font-size: 13px; font-weight: 600; color: ", SLAM_BLUE, "; margin-bottom: 6px; }
.results-card { background: white; border-radius: 14px; border: 1.5px solid #e2e8f0; padding: 20px; margin-top: 20px; box-shadow: 0 4px 16px rgba(0,0,0,0.06); }
.results-title { font-size: 16px; font-weight: 700; color: ", SLAM_BLUE, "; margin-bottom: 14px; }
.tab-success { background: #ecfdf5; border-radius: 10px; padding: 16px; margin-top: 12px; border: 1px solid #a7f3d0; }
.panel { border-radius: 10px; border: 1px solid #ddd; box-shadow: 0 2px 4px rgba(0,0,0,0.05); }
.panel-heading { background: ", SLAM_BLUE, "; color: white; border-radius: 10px 10px 0 0; font-weight: 600; padding: 12px 16px; }
.panel-body { background: white; padding: 16px; }
.form-control { border-radius: 6px; border: 1px solid #ccc; }
.btn-primary { background: ", SLAM_BLUE, "; border-color: ", SLAM_BLUE, "; font-weight: 600; }
.btn-primary:hover { background: #B45309; border-color: #B45309; }
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
        # Right: SLAM assessments history (8 cols)
        column(8,
          div(class = "panel",
            div(class = "panel-heading", "选择受试者 / Select Subject"),
            div(class = "panel-body",
              selectInput("slam_filter_status", "筛选状态 / Filter Status",
                choices = c("全部 / All" = "all",
                            "进行中 / In Progress" = "in_progress",
                            "已完成 / Complete" = "complete"),
                selected = "all", width = "40%"),
              DT::dataTableOutput("slam_patient_dt"),
              uiOutput("slam_load_btn_ui")
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
          span(class = "progress-story", "13-17岁 · 6张图 · GFA + Narrative")
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

          # GFA
          div(class = "section-label", "📝 GFA 语法填空 / Grammar Fluency Assessment"),
          lapply(seq_len(nrow(STORIES$baseball_troubles$gfa_items)), function(i) {
            gfa <- STORIES$baseball_troubles$gfa_items
            ms <- gfa$max_score[i]
            choices <- c("—"="", setNames(as.character(ms:0), paste0(ms:0, "分")))
            passage_display <- if (grepl("___", gfa$passage_en[i])) {
              blank_p <- gsub("___", sprintf('<span class="blank">___%d</span>', i), gfa$passage_en[i])
              HTML(gsub("\\{\\{blank\\}\\}", blank_p, gfa$passage_en[i]))
            } else {
              HTML(gsub("\\{\\{blank\\}\\}", gfa$passage_en[i], gfa$passage_en[i]))
            }
            div(class = "gfa-item", id = sprintf("gfa_bt_%d", i),
              div(class = "gfa-passage",
                passage_display,
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
    # TAB 3: The Ball Mystery
    # ═══════════════════════════════════════════════════════════
    tabPanel("🔵 The Ball Mystery",
      div(class = "story-card",
        div(class = "story-card-header",
          span("🔵"), "The Ball Mystery / 神秘小球",
          span(class = "progress-story", "10-14岁 · 5张图 · GFA + Narrative")
        ),
        div(class = "story-card-body",
          div(class = "synopsis-box",
            p(strong("故事概要 Story Synopsis:"), br()),
            p(STORIES$the_ball_mystery$synopsis)
          ),
          div(class = "section-label", "📷 图片卡片 / Picture Cards"),
          story_img_carousel("the_ball_mystery", 5),
          div(class = "section-label", "📝 GFA 语法填空"),
          lapply(seq_along(STORIES$the_ball_mystery$gfa_items$item), function(i) {
            gfa <- STORIES$the_ball_mystery$gfa_items
            ms <- gfa$max_score[i]
            choices <- c("—"="", setNames(as.character(ms:0), paste0(ms:0, "分")))
            passage_display <- if (grepl("___", gfa$passage_en[i])) {
              blank_p <- gsub("___", sprintf('<span class="blank">___%d</span>', i), gfa$passage_en[i])
              HTML(gsub("\\{\\{blank\\}\\}", blank_p, gfa$passage_en[i]))
            } else {
              HTML(gsub("\\{\\{blank\\}\\}", gfa$passage_en[i], gfa$passage_en[i]))
            }
            div(class = "gfa-item", id = sprintf("gfa_tbt_%d", i),
              div(class = "gfa-passage",
                passage_display,
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
          lapply(seq_along(STORIES$the_ball_mystery$narrative_rubric$dimensions), function(d) {
            dim_name <- STORIES$the_ball_mystery$narrative_rubric$dimensions[d]
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
            actionButton("save_tbt", "💾 保存 The Ball Mystery 评分",
              class = "btn-save-slam")
          )
        )
      )
    ),

    # ═══════════════════════════════════════════════════════════
    # TAB 4: Lost Cellphone
    # ═══════════════════════════════════════════════════════════
    tabPanel("📱 Lost Cellphone",
      div(class = "story-card",
        div(class = "story-card-header",
          span("📱"), "Lost Cellphone / 丢失的手机",
          span(class = "progress-story", "13-17岁 · 6张图 · GFA + Narrative")
        ),
        div(class = "story-card-body",
          div(class = "synopsis-box",
            p(strong("故事概要 Story Synopsis:"), br()),
            p(STORIES$lost_cellphone$synopsis)
          ),
          div(class = "section-label", "📷 图片卡片 / Picture Cards"),
          story_img_carousel("lost_cellphone", 3),
          div(class = "section-label", "📝 GFA 语法问答"),
          lapply(seq_along(STORIES$lost_cellphone$gfa_items$item), function(i) {
            gfa <- STORIES$lost_cellphone$gfa_items
            ms <- gfa$max_score[i]
            choices <- c("—"="", setNames(as.character(ms:0), paste0(ms:0, "分")))
            passage_display <- if (grepl("___", gfa$passage_en[i])) {
              blank_p <- gsub("___", sprintf('<span class="blank">___%d</span>', i), gfa$passage_en[i])
              HTML(gsub("\\{\\{blank\\}\\}", blank_p, gfa$passage_en[i]))
            } else {
              HTML(gsub("\\{\\{blank\\}\\}", gfa$passage_en[i], gfa$passage_en[i]))
            }
            div(class = "gfa-item", id = sprintf("gfa_gwh_%d", i),
              div(class = "gfa-passage",
                passage_display,
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
          lapply(seq_along(STORIES$lost_cellphone$narrative_rubric$dimensions), function(d) {
            dim_name <- STORIES$lost_cellphone$narrative_rubric$dimensions[d]
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
            actionButton("save_gwh", "💾 保存 Lost Cellphone 评分",
              class = "btn-save-slam")
          )
        )
      )
    ),

    # ═══════════════════════════════════════════════════════════
    # TAB 5: Kittens Love Milk Cards
    # ═══════════════════════════════════════════════════════════
    tabPanel("🐱 Kittens Love Milk Cards",
      div(class = "story-card",
        div(class = "story-card-header",
          span("🐱"), "Kittens Love Milk Cards / 小猫爱牛奶",
          span(class = "progress-story", "7-14岁 · 5张图 · GFA + Narrative")
        ),
        div(class = "story-card-body",
          div(class = "synopsis-box",
            p(strong("故事概要 Story Synopsis:"), br()),
            p(STORIES$kittens_love_milk$synopsis)
          ),
          div(class = "section-label", "📷 图片卡片 / Picture Cards"),
          story_img_carousel("kittens_love_milk", 3),
          div(class = "section-label", "📝 GFA 语法问答"),
          lapply(seq_along(STORIES$kittens_love_milk$gfa_items$item), function(i) {
            gfa <- STORIES$kittens_love_milk$gfa_items
            ms <- gfa$max_score[i]
            choices <- c("—"="", setNames(as.character(ms:0), paste0(ms:0, "分")))
            passage_display <- if (grepl("___", gfa$passage_en[i])) {
              blank_p <- gsub("___", sprintf('<span class="blank">___%d</span>', i), gfa$passage_en[i])
              HTML(gsub("\\{\\{blank\\}\\}", blank_p, gfa$passage_en[i]))
            } else {
              HTML(gsub("\\{\\{blank\\}\\}", gfa$passage_en[i], gfa$passage_en[i]))
            }
            div(class = "gfa-item", id = sprintf("gfa_wb_%d", i),
              div(class = "gfa-passage",
                passage_display,
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
          lapply(seq_along(STORIES$kittens_love_milk$narrative_rubric$dimensions), function(d) {
            dim_name <- STORIES$kittens_love_milk$narrative_rubric$dimensions[d]
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
            actionButton("save_wb", "💾 保存 Kittens Love Milk Cards 评分",
              class = "btn-save-slam")
          )
        )
      )
  ),  # end tabsetPanel

  # Footer
  div(style = "text-align: center; margin-top: 36px; padding: 20px; color: #aaa; font-size: 13px;",
    span(style = sprintf("color: %s; font-weight: 600;", SLAM_BLUE), "SLAM"),
    "© 2026  |  Columbia University Leaders Project — Free for Copying and Distribution  |  ",
    "Powered by R Shiny"
  ))
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

  # ── Reactive values for SLAM ────────────────────────────────────
  rv <- reactiveValues(slam_status_version = 0L)

  # ── SLAM Patients DT (Subject Info tab — shared patients table) ─
  slam_patients_df <- reactive({
    con <- get_con()
    on.exit(dbDisconnect(con))
    patients <- dbGetQuery(con, "
      SELECT p.id, p.name, p.dob, p.gender, p.examiner,
             (SELECT COUNT(*) FROM assessments a WHERE a.patient_id = p.id) as n_assessments
      FROM patients p
      ORDER BY p.id DESC")
    patients
  }) %>% bindEvent(input$slam_filter_status, rv$slam_status_version)

  output$slam_patient_dt <- DT::renderDataTable({
    con <- get_con()
    on.exit(dbDisconnect(con))
    patients <- dbGetQuery(con, "
      SELECT p.id, p.name, p.dob, p.gender, p.examiner,
             (SELECT COUNT(*) FROM assessments a WHERE a.patient_id = p.id) as n_assessments
      FROM patients p
      ORDER BY p.id DESC
    ")

    if (nrow(patients) == 0) {
      return(data.frame(
        ID = integer(), Name = character(), DOB = character(),
        Gender = character(), Examiner = character(), Assessments = integer()
      ))
    }

    dt_df <- data.frame(
      ID          = patients$id,
      Name        = patients$name,
      DOB         = ifelse(is.na(patients$dob), "-", as.character(patients$dob)),
      Gender      = ifelse(is.na(patients$gender), "-",
                    ifelse(patients$gender == "M", "男 / M", "女 / F")),
      Examiner    = ifelse(is.na(patients$examiner), "-", patients$examiner),
      Assessments = patients$n_assessments,
      stringsAsFactors = FALSE
    )

    # Filter by slam_filter_status if not "all"
    fs <- input$slam_filter_status
    if (!is.null(fs) && fs == "in_progress") {
      dt_df <- dt_df[dt_df$Assessments > 0, ]
    }

    DT::datatable(dt_df, selection = "single", escape = FALSE,
      options = list(
        pageLength = 10,
        lengthMenu = c(10, 25, 50),
        dom = 'frtip',
        language = list(
          emptyTable = "暂无受试者 / No subjects found",
          search = "搜索：",
          lengthMenu = "每页 _MENU_ 条",
          info = "显示第 _START_ 至 _END_ 条，共 _TOTAL_ 条"
        ),
        columnDefs = list(
          list(className = 'dt-center', targets = c(0, 2, 3, 4, 5)),
          list(visible = FALSE, targets = 0)  # hide ID col
        ),
        initComplete = htmlwidgets::JS(
          "function(settings, json) {",
          "  $(this.api().table().body()).css('font-size','13px');",
          "  $(this.api().table().header()).css('background','#f8f9fa');",
          "  $(this.api().table().header()).css('font-weight','600');",
          "}")
      ),
      rownames = FALSE,
      class = "stripe hover compact")
  }, server = FALSE)

  # ── Load/Delete buttons (when patient row selected) ──────────────
  output$slam_load_btn_ui <- renderUI({
    req(!is.null(input$slam_patient_dt_rows_selected))
    tagList(
      hr(),
      fluidRow(
        column(6,
          actionButton("slam_btn_load_patient", "📂 加载 / Load",
            class = "btn-primary",
            style = sprintf("width:100%%; background:%s;", SLAM_BLUE))
        ),
        column(6,
          actionButton("slam_btn_delete_confirm", "🗑 删除 / Delete",
            class = "btn-danger",
            style = "width:100%;"))
      )
    )
  })

  # ── Load patient — populate form fields ─────────────────────────
  observeEvent(input$slam_btn_load_patient, {
    req(input$slam_patient_dt_rows_selected)
    con <- get_con()
    on.exit(dbDisconnect(con), add = TRUE)

    patients <- dbGetQuery(con, "
      SELECT p.id, p.name, p.dob, p.gender, p.examiner
      FROM patients p
      ORDER BY p.id DESC
    ")

    row_idx <- input$slam_patient_dt_rows_selected[1]
    if (row_idx > nrow(patients)) return()

    row <- patients[row_idx, ]
    rv$patient_id <- as.integer(row$id)

    updateTextInput(session, "slam_patient_name", value = row$name %||% "")
    updateTextInput(session, "slam_examiner", value = row$examiner %||% "")
    updateSelectInput(session, "slam_patient_gender", selected = row$gender %||% "")
    updateDateInput(session, "slam_dob", value = ifelse(is.na(row$dob), NA, as.Date(row$dob)))

    showNotification(
      tagList(icon("check-circle"), sprintf(" 已加载受试者: %s (ID#%d)", row$name, row$id)),
      type = "message", duration = 3
    )
  })

  # ── Delete patient (via modal) ──────────────────────────────────
  observeEvent(input$slam_btn_delete_confirm, {
    req(input$slam_patient_dt_rows_selected)
    showModal(modalDialog(
      title = "确认删除 / Confirm Delete",
      "确定要删除这位受试者吗？所有关联的评估记录也将被删除，此操作不可撤销。",
      footer = tagList(
        modalButton("取消 / Cancel"),
        actionButton("slam_btn_delete_patient_do", "🗑 确定删除",
          class = "btn-danger")
      )
    ))
  })

  observeEvent(input$slam_btn_delete_patient_do, {
    req(input$slam_patient_dt_rows_selected)
    con <- get_con()
    on.exit(dbDisconnect(con), add = TRUE)

    patients <- dbGetQuery(con, "
      SELECT p.id, p.name
      FROM patients p
      ORDER BY p.id DESC
    ")
    row_idx <- input$slam_patient_dt_rows_selected[1]
    pid <- as.integer(patients$id[row_idx])

    # Delete related assessments first
    dbExecute(con, "DELETE FROM assessments WHERE patient_id=?", params = list(pid))
    dbExecute(con, "DELETE FROM patients WHERE id=?", params = list(pid))

    removeModal()
    rv$slam_status_version <- rv$slam_status_version + 1L
    showNotification("受试者已删除 / Subject deleted", type = "message")
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
  # (removed — AI tab was removed)

  # ── AI Report — Selected Patient Report ─────────────────────
  # (removed — AI tab was removed)
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
