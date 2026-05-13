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
story_img_carousel <- function(story_id, n_images, images_per_page = 1) {
  if (images_per_page == 1) {
    # Portrait: one image per page
    img_tags <- lapply(seq_len(n_images), function(p) {
      page_num <- sprintf("p%d", p)
      img_path <- sprintf("story_images/%s_%s_img1.png", story_id, page_num)
      tags$div(class = "carousel-slide",
        tags$img(src = img_path, class = "story-img",
                  alt = sprintf("Page %d of %s", p, story_id))
      )
    })
  } else {
    # Landscape / 2-up: two images per page
    n_pages <- ceiling(n_images / images_per_page)
    img_tags <- lapply(seq_len(n_images), function(idx) {
      page_num <- ceiling(idx / images_per_page)
      img_num  <- ((idx - 1) %% images_per_page) + 1
      page_str <- sprintf("p%d", page_num)
      img_path <- sprintf("story_images/%s_%s_img%d.png", story_id, page_str, img_num)
      tags$div(class = "carousel-slide",
        tags$img(src = img_path, class = "story-img",
                  alt = sprintf("Page %d image %d", page_num, img_num))
      )
    })
  }
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
    age_norms = list(
      range_label = "Late elementary thru high school",
      typical     = "12-16",
      probing     = "9-11",
      support     = "Below 9"
    ),
    n_images = 7,
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
    n_images = 7,
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
    n_images = 6,
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
    n_images = 6,
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
# 2. PreK-Elementary Stories (ages 3-12)
# ─────────────────────────────────────────────────────────────
STORIES_ELEM <- list(
  dog_comes_home = list(
    id   = "dog_comes_home",
    name = "Dog Comes Home",
    name_zh = "狗回家",
    age_range = "3-6岁",
    age_norms = list(
      range_label = "4 thru early elementary",
      typical     = "11-14",
      probing     = "8-10",
      support     = "Below 8"
    ),
    n_images = 7,
    pdf_path = "story_images/dog_comes_home",
    gfa_path = NULL,
    synopsis = "一个小女孩偷偷把一只脏狗带回家藏进包里，被发现后给狗洗澡，妈妈回来看到又惊又气。\nA girl secretly hides a dirty dog in her bag to take it home. When discovered, she bathes the dog. Mom returns shocked and upset.",
    word_finding = tibble(
      item   = 1:6,
      prompt_en = c(
        "What is this?",  # dog
        "What is this?",  # girl
        "What is this?",  # bag
        "What is this?",  # mother/mom
        "What is this?",  # bathtub
        "What is this?"   # water
      ),
      prompt_zh = c(
        "这是什么？", # dog
        "这是什么？", # girl
        "这是什么？", # bag
        "这是什么？", # mother/mom
        "这是什么？", # bathtub
        "这是什么？"  # water
      ),
      acceptable_en = list(
        c("dog","puppy","doggy"),
        c("girl","kid","child"),
        c("bag","backpack","purse"),
        c("mom","mother","mama"),
        c("bathtub","bath","tub"),
        c("water")
      )
    ),
    gfa_items = tibble(
      item = 1:6,
      passage_en = c(
        "What is the girl thinking here?",
        "Why is she putting the dog in her bag?",
        "Why is the girl getting so dirty?",
        "Why is she in the bathtub with a white dog now?",
        "What is the mother going to do now?",
        "What would you say to your mom if you were the girl?"
      ),
      passage_zh = c(
        "小女孩在想什么？",
        "她为什么把小狗放进包里？",
        "小女孩为什么变脏了？",
        "她为什么现在和一只小白狗一起在浴缸里？",
        "妈妈现在会做什么？",
        "如果你是小女孩，你会对妈妈说什么？"
      ),
      answers = lapply(1:6, function(i) character(0)),
      max_score = 2
    ),
    narrative_rubric = list(
      dimensions = c("叙事结构 Narrative Structure","复杂句 Complex Clauses",
                     "推论能力 Inferencing","语用 Pragmatic","理论心智 Theory of Mind"),
      max_per_dim = 2
    )
  ),

  bunny_goes_school = list(
    id   = "bunny_goes_school",
    name = "Bunny Goes to School",
    name_zh = "小兔子去学校",
    age_range = "7-12岁",
    age_norms = list(
      range_label = "4 thru early elementary",
      typical     = "12-16",
      probing     = "9-11",
      support     = "Below 9"
    ),
    n_images = 6,
    pdf_path = "story_images/bunny_goes_school",
    gfa_path = NULL,
    synopsis = "一只小兔子偷偷钻进书包跟男孩去学校，被发现后全班惊慌，男孩用胡萝卜引诱抓住兔子，妈妈被叫到学校。\nA bunny secretly hops into a boy's backpack to follow him to school. Discovered, the class panics. The boy lures it with a carrot. Mom is called to school.",
    word_finding = tibble(
      item   = 1:6,
      prompt_en = c(
        "What is this?",  # bunny/rabbit
        "What is this?",  # backpack
        "What is this?",  # teacher
        "What is this?",  # carrot
        "What is this?",  # desk
        "What is this?"   # phone
      ),
      prompt_zh = c(
        "这是什么？", # bunny/rabbit
        "这是什么？", # backpack
        "这是什么？", # teacher
        "这是什么？", # carrot
        "这是什么？", # desk
        "这是什么？"  # phone
      ),
      acceptable_en = list(
        c("bunny","rabbit","rabbit"),
        c("backpack","bag","schoolbag"),
        c("teacher","teacher"),
        c("carrot","carrot"),
        c("desk","table"),
        c("phone","cellphone","mobile")
      )
    ),
    gfa_items = tibble(
      item = 1:9,
      passage_en = c(
        "Why did the bunny jump out of the backpack?",
        "Why are some students afraid? Why are some laughing?",
        "What would you do if a bunny started hopping around your classroom?",
        "What was the boy's idea?",
        "How did the mom know she had to come to school?",
        "Why did she come to school?",
        "What do you think will happen when the boy goes home?",
        "What is the teacher thinking now?",
        "Have you ever been in trouble like this?"
      ),
      passage_zh = c(
        "小兔子为什么从书包里跳出来？",
        "为什么有的学生害怕，有的在笑？",
        "如果有一只兔子在你们教室里跳，你会怎么做？",
        "男孩想了什么办法？",
        "妈妈怎么知道要来学校？",
        "妈妈为什么要来学校？",
        "你觉得男孩回家后会发生什么？",
        "老师现在在想什么？",
        "你有没有遇到过类似的麻烦？"
      ),
      answers = lapply(1:9, function(i) character(0)),
      max_score = 2
    ),
    narrative_rubric = list(
      dimensions = c("叙事结构 Narrative Structure","复杂句 Complex Clauses",
                     "推论能力 Inferencing","语用 Pragmatic","理论心智 Theory of Mind"),
      max_per_dim = 2
    )
  ),

  the_crayons = list(
    id   = "the_crayons",
    name = "The Crayons",
    name_zh = "蜡笔大战",
    age_range = "4-12岁",
    age_norms = list(
      range_label = "Kindergarten thru high school",
      typical     = "6-8",
      probing     = "4-5",
      support     = "Below 4"
    ),
    n_images = 1,
    pdf_path = "story_images/the_crayons",
    gfa_path = NULL,
    synopsis = "红色蜡笔在墙上乱画后嫁祸给小蓝蜡笔，紫色蜡笔很生气。\nThe red crayon draws on the wall and blames the little blue crayon. The purple crayon is angry at blue.",
    word_finding = tibble(
      item   = 1:5,
      prompt_en = c(
        "What is this?",  # red crayon
        "What is this?",  # blue crayon
        "What is this?",  # purple crayon
        "What is this?",  # wall
        "What is this?"   # coloring/drawing
      ),
      prompt_zh = c(
        "这是什么？", # red crayon
        "这是什么？", # blue crayon
        "这是什么？", # purple crayon
        "这是什么？", # wall
        "这是什么？"  # coloring/drawing
      ),
      acceptable_en = list(
        c("red crayon","red","crayon"),
        c("blue crayon","blue","crayon"),
        c("purple crayon","purple","crayon"),
        c("wall"),
        c("coloring","drawing","picture")
      )
    ),
    gfa_items = tibble(
      item = 1:5,
      passage_en = c(
        "What happened here?",
        "Why is the red crayon pointing to the little blue crayon?",
        "What would you say to the big crayon if you were the little blue crayon?",
        "What do you think is going to happen next?",
        "Were you ever blamed for something you didn't do?"
      ),
      passage_zh = c(
        "这里发生了什么事？",
        "红色蜡笔为什么指着小蓝蜡笔？",
        "如果你是小蓝蜡笔，你会对大红蜡笔说什么？",
        "你觉得接下来会发生什么？",
        "你有没有被冤枉过？"
      ),
      answers = lapply(1:5, function(i) character(0)),
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
# 2b. SLAM Norms Table (age 7–17, simplified lookup)
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

# PreK-Elementary norms (ages 3-12)
build_slam_norms_elem <- function() {
  # Ages 3-6: PreK norms (approximate from clinical experience)
  ages_preschool <- rep(3:6, each = 4)
  raw_scores <- rep(c(0, 3, 6, 9), 4)
  std_wf_elem <- c(
    50,60,70,82,  # age 3
    50,61,72,83,  # age 4
    51,62,73,84,  # age 5
    52,63,74,85   # age 6
  )
  std_gfa_elem <- c(
    50,58,66,78,  # age 3
    50,59,68,80,  # age 4
    51,60,70,82,  # age 5
    52,61,71,83   # age 6
  )
  # Ages 7-12: merge with JH-HS norms
  ages_jh <- rep(7:12, each = 4)
  raw_jh <- rep(c(0,5,10,15), 6)
  std_wf_jh <- c(
    50,62,74,86,  # 7
    51,64,76,88,  # 8
    52,65,78,89,  # 9
    53,66,79,90,  # 10
    54,67,80,91,  # 11
    55,68,81,92   # 12
  )
  std_gfa_jh <- c(
    50,60,70,82,  # 7
    51,62,72,84,  # 8
    52,63,73,85,  # 9
    53,64,74,86,  # 10
    54,65,75,87,  # 11
    55,66,76,88   # 12
  )
  tibble(
    age = c(ages_preschool, ages_jh),
    raw_score = c(raw_scores, raw_jh),
    std_word_finding = c(std_wf_elem, std_wf_jh),
    std_gfa = c(std_gfa_elem, std_gfa_jh)
  )
}

SLAM_NORMS_ELEM <- build_slam_norms_elem()

get_slam_standard_score_elem <- function(raw, type = c("word_finding","gfa"), age) {
  type <- match.arg(type)
  col  <- if (type == "word_finding") "std_word_finding" else "std_gfa"
  row  <- SLAM_NORMS_ELEM %>% filter(.data$age == !!age, .data$raw_score <= !!raw) %>%
    summarise(s = max(.data[[col]]), .groups = "drop")
  if (nrow(row) == 0) return(NA_integer_)
  row$s[1]
}

get_slam_standard_score <- function(raw, type = c("word_finding","gfa"), age) {
  type <- match.arg(type)
  if (age <= 6) {
    get_slam_standard_score_elem(raw, type, age)
  } else {
    get_slam_standard_score_jh <- function(raw, type, age) {
      col  <- if (type == "word_finding") "std_word_finding" else "std_gfa"
      row  <- SLAM_NORMS %>% filter(.data$age == !!age, .data$raw_score <= !!raw) %>%
        summarise(s = max(.data[[col]]), .groups = "drop")
      if (nrow(row) == 0) return(NA_integer_)
      row$s[1]
    }
    get_slam_standard_score_jh(raw, type, age)
  }
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

    # ═══════════════════════════════════════════════════════════════════
    # TAB: PreK-Elementary (PreK-小学，3-12岁)
    # ═══════════════════════════════════════════════════════════════════
    tabPanel("🧒 PreK-小学 / PreK-Elem",
      tabsetPanel(id = "preK_elem_tabs", type = "tabs",

        # ── Dog Comes Home (3-6岁) ─────────────────────────────────
        tabPanel("🏠 Dog Comes Home",
          div(class = "story-card",
            div(class = "story-card-header",
              span("🏠"), "Dog Comes Home / 狗回家",
              span(class = "progress-story", "3-6岁 · 6张图 · GFA + Narrative")
            ),
            div(class = "story-card-body",
              div(class = "synopsis-box",
                p(strong("故事概要 Story Synopsis:"), br()),
                p(STORIES_ELEM$dog_comes_home$synopsis)
              ),

              div(class = "scoring-guide",
                tags$details(
                  tags$summary("Scoring Guide / 评分参考 (click to expand)"),
                  tags$h4("Language Characteristics / 语言特点 (3-6 yrs)"),
                  tags$ul(
                    tags$li("Short, simple sentences; basic connectors: then/and/but"),
                    tags$li("Describes what happened in basic sequence"),
                    tags$li("Limited mental state vocabulary; simple cause-effect")
                  ),
                  tags$h4("Narrative Example / 叙事范例"),
                  tags$p(tags$i("One day, a little girl found a puppy outside. She really liked the puppy, and then they played together. Then she held the puppy and thought her mom said no dogs at home. But she still put the puppy in her bag to bring home. Then her mom saw her because she was dirty, a little surprised. Then mom told her to take a bath. Finally, her mom saw her and the white dog taking a bath together and was very confused.")),
                  tags$h4("Sample Q&A / 问答示例"),
                  tags$ul(
                    tags$li(tags$strong("Q: What is the girl thinking?"), " She likes the puppy but mom says no."),
                    tags$li(tags$strong("Q: Why put the dog in the bag?"), " She loves the puppy and wants to take it home."),
                    tags$li(tags$strong("Q: Why is the girl dirty?"), " The puppy was dirty, she played with it and got dirty too.")
                  )
                )
              ),
              div(class = "section-label", "📷 图片卡片 / Picture Cards"),
              story_img_carousel("dog_comes_home", 7, images_per_page = 2),
              div(class = "section-label", "📝 GFA 语法问答 / Grammar Fluency Assessment"),
              lapply(seq_len(nrow(STORIES_ELEM$dog_comes_home$gfa_items)), function(i) {
                gfa <- STORIES_ELEM$dog_comes_home$gfa_items
                ms <- gfa$max_score[i]
                choices <- c("—"="", setNames(as.character(ms:0), paste0(ms:0, "分")))
                div(class = "gfa-item",
                  div(class = "gfa-passage",
                    p(gfa$passage_en[i]),
                    p(style = "margin-top: 8px; font-size: 13px; color: #6b7280; font-style: normal;", gfa$passage_zh[i])
                  ),
                  fluidRow(
                    column(8, textInput(sprintf("gfa_dog_%d_text", i), "回答 / Response:", width = "100%")),
                    column(4, tags$label("评分 Score", class = "form-label"),
                      selectInput(sprintf("gfa_dog_%d_score", i), NULL,
                        choices = choices, selected = "", width = "100%"))
                  )
                )
              }),
              div(class = "section-label", "🎤 Free Narrative / 自由叙事"),
              div(class = "narrative-box",
                p(strong("指示 Instruction: "), "请学生看着图片讲述故事。/ Ask student to tell the story using the pictures."),
                textAreaInput("narr_dog", "学生回答 / Student Response:", width = "100%", rows = 5, placeholder = "学生在评估时的叙事内容..."),
                p(style = "margin-top: 10px; font-size: 13px; color: #6b7280;", "📝 请评估者在评估时记录学生叙事，并在下方评分。")
              ),
              div(class = "section-label", "📊 Narrative Rubric / 叙事评分"),
              lapply(seq_along(STORIES_ELEM$dog_comes_home$narrative_rubric$dimensions), function(d) {
                dim_name <- STORIES_ELEM$dog_comes_home$narrative_rubric$dimensions[d]
                div(style = "margin-bottom: 14px;",
                  div(class = "rubric-dim-label", sprintf("%s (0-2分)", dim_name)),
                  div(class = "rubric-row",
                    radioButtons(sprintf("nr_dog_dim%d", d), NULL,
                      choices = c("0分"="0","1分"="1","2分"="2"),
                      selected = character(0), inline = TRUE, width = "100%")
                  )
                )
              }),
              div(style = "margin-top: 20px; text-align: center;",
                actionButton("save_dog", "💾 保存 Dog Comes Home 评分", class = "btn-save-slam")
              )
            )
          )
        ),

        # ── Bunny Goes to School (7-12岁) ───────────────────────────
        tabPanel("🐰 Bunny Goes to School",
          div(class = "story-card",
            div(class = "story-card-header",
              span("🐰"), "Bunny Goes to School / 小兔子去学校",
              span(class = "progress-story", "7-12岁 · 6张图 · GFA + Narrative")
            ),
            div(class = "story-card-body",
              div(class = "synopsis-box",
                p(strong("故事概要 Story Synopsis:"), br()),
                p(STORIES_ELEM$bunny_goes_school$synopsis)
              ),

              div(class = "scoring-guide",
                tags$details(
                  tags$summary("Scoring Guide / 评分参考 (click to expand)"),
                  tags$h4("Language Characteristics / 语言特点 (7-12 yrs)"),
                  tags$ul(
                    tags$li("Complete narrative structure: beginning, middle, end, clear sequence"),
                    tags$li("Richer connectors: then, so, because, but, after"),
                    tags$li("Describes characters' thoughts, feelings, and motivations"),
                    tags$li("Vocabulary expansion: sneakily, anxiously, confused"),
                    tags$li("Simple causal reasoning")
                  ),
                  tags$h4("Narrative Example / 叙事范例"),
                  tags$p(tags$i("One day, a boy was heading to school with his lunchbox and backpack, secretly hiding a little bunny inside. During English class, the teacher was teaching the letter C and gave the example of carrot. Just then, the bunny jumped out of the backpack and was discovered by everyone. The bunny hopped around the classroom causing chaos. Some students were scared and躲开, some laughed, and one student was so frightened he climbed onto the desk crying. The teacher had no choice but to call the boy's parent. The boy thought quickly - he remembered his lunchbox had carrots and decided to lure the bunny. He took out the carrot and fed it to the bunny, and the bunny calmed down. Then mom and the teacher walked in together...")),
                  tags$h4("Sample Q&A / 问答示例"),
                  tags$ul(
                    tags$li(tags$strong("Q: Why did the bunny jump out?"), " It wanted to come out and play, or it was too hot in the backpack."),
                    tags$li(tags$strong("Q: Why are some scared, some laughing?"), " Some are afraid of rabbits, others think it is cute and funny."),
                    tags$li(tags$strong("Q: What was the boy's idea?"), " He thought to use the carrot to lure the bunny to come to him.")
                  )
                )
              ),
              div(class = "section-label", "📷 图片卡片 / Picture Cards"),
              story_img_carousel("bunny_goes_school", 6, images_per_page = 2),
              div(class = "section-label", "📝 GFA 语法问答 / Grammar Fluency Assessment"),
              lapply(seq_len(nrow(STORIES_ELEM$bunny_goes_school$gfa_items)), function(i) {
                gfa <- STORIES_ELEM$bunny_goes_school$gfa_items
                ms <- gfa$max_score[i]
                choices <- c("—"="", setNames(as.character(ms:0), paste0(ms:0, "分")))
                div(class = "gfa-item",
                  div(class = "gfa-passage",
                    p(gfa$passage_en[i]),
                    p(style = "margin-top: 8px; font-size: 13px; color: #6b7280; font-style: normal;", gfa$passage_zh[i])
                  ),
                  fluidRow(
                    column(8, textInput(sprintf("gfa_bunny_%d_text", i), "回答 / Response:", width = "100%")),
                    column(4, tags$label("评分 Score", class = "form-label"),
                      selectInput(sprintf("gfa_bunny_%d_score", i), NULL,
                        choices = choices, selected = "", width = "100%"))
                  )
                )
              }),
              div(class = "section-label", "🎤 Free Narrative / 自由叙事"),
              div(class = "narrative-box",
                p(strong("指示 Instruction: "), "请学生看着图片讲述故事。/ Ask student to tell the story using the pictures."),
                textAreaInput("narr_bunny", "学生回答 / Student Response:", width = "100%", rows = 5, placeholder = "学生在评估时的叙事内容..."),
                p(style = "margin-top: 10px; font-size: 13px; color: #6b7280;", "📝 请评估者在评估时记录学生叙事，并在下方评分。")
              ),
              div(class = "section-label", "📊 Narrative Rubric / 叙事评分"),
              lapply(seq_along(STORIES_ELEM$bunny_goes_school$narrative_rubric$dimensions), function(d) {
                dim_name <- STORIES_ELEM$bunny_goes_school$narrative_rubric$dimensions[d]
                div(style = "margin-bottom: 14px;",
                  div(class = "rubric-dim-label", sprintf("%s (0-2分)", dim_name)),
                  div(class = "rubric-row",
                    radioButtons(sprintf("nr_bunny_dim%d", d), NULL,
                      choices = c("0分"="0","1分"="1","2分"="2"),
                      selected = character(0), inline = TRUE, width = "100%")
                  )
                )
              }),
              div(style = "margin-top: 20px; text-align: center;",
                actionButton("save_bunny", "💾 保存 Bunny Goes to School 评分", class = "btn-save-slam")
              )
            )
          )
        ),

        # ── The Crayons (4-12岁) ────────────────────────────────────
        tabPanel("🖍️ The Crayons",
          div(class = "story-card",
            div(class = "story-card-header",
              span("🖍️"), "The Crayons / 蜡笔大战",
              span(class = "progress-story", "4-12岁 · 4张图 · GFA + Narrative")
            ),
            div(class = "story-card-body",
              div(class = "synopsis-box",
                p(strong("故事概要 Story Synopsis:"), br()),
                p(STORIES_ELEM$the_crayons$synopsis)
              ),

              div(class = "scoring-guide",
                tags$details(
                  tags$summary("Scoring Guide / 评分参考 (click to expand)"),
                  tags$h4("Language Characteristics / 语言特点"),
                  tags$ul(
                    tags$li(tags$span(class = "tier-label", "4-6 yrs"), "Short sentences, repetitive, uses and/then/but; describes what happened"),
                    tags$li(tags$span(class = "tier-label", "7-9 yrs"), "Cause-effect, because/so/after; describes emotions"),
                    tags$li(tags$span(class = "tier-label", "10-12 yrs"), "Longer sentences, to avoid blame / innocent / blames / lies; complete logic and mental state words")
                  ),
                  tags$h4("Narrative Examples / 叙事范例"),
                  tags$p(tags$span(class = "tier-label", "4-6"), tags$i(" Version 1 (short, simple): One day the crayons were in the classroom. The red crayon drew on the wall. Then he pointed at the blue crayon and said it was him. The blue crayon felt sad. The purple crayon was angry.")),
                  tags$p(tags$span(class = "tier-label", "7-9"), tags$i(" Version 2 (cause-effect): One day, the red crayon drew on the wall when no one was looking. After that, he pointed at the blue crayon and said the blue crayon did it. The blue crayon did not do anything wrong, so he looks very upset. The purple crayon is the teacher and she is very angry.")),
                  tags$p(tags$span(class = "tier-label", "10-12"), tags$i(" Version 3 (complete narrative): To avoid getting in trouble, the red crayon quickly pointed at the innocent blue crayon and lied to the teacher, saying the blue crayon made the mess. The blue crayon feels very sad and unfair because he is blamed for something he did not do.")),
                  tags$h4("Sample Q&A / 问答示例"),
                  tags$ul(
                    tags$li(tags$strong("Q: What happened?"), " The red crayon drew on the wall, then blamed the blue crayon."),
                    tags$li(tags$strong("Q: Why is red pointing at blue?"), " Red drew on the wall and wants to blame someone else."),
                    tags$li(tags$strong("Q: What would you say if you were blue?"), " I would say: I did not draw on the wall! You did it! Do not blame me!")
                  )
                )
              ),
              div(class = "section-label", "📷 图片卡片 / Picture Cards"),
              story_img_carousel("the_crayons", 1),
              div(class = "section-label", "📝 GFA 语法问答 / Grammar Fluency Assessment"),
              lapply(seq_len(nrow(STORIES_ELEM$the_crayons$gfa_items)), function(i) {
                gfa <- STORIES_ELEM$the_crayons$gfa_items
                ms <- gfa$max_score[i]
                choices <- c("—"="", setNames(as.character(ms:0), paste0(ms:0, "分")))
                div(class = "gfa-item",
                  div(class = "gfa-passage",
                    p(gfa$passage_en[i]),
                    p(style = "margin-top: 8px; font-size: 13px; color: #6b7280; font-style: normal;", gfa$passage_zh[i])
                  ),
                  fluidRow(
                    column(8, textInput(sprintf("gfa_crayons_%d_text", i), "回答 / Response:", width = "100%")),
                    column(4, tags$label("评分 Score", class = "form-label"),
                      selectInput(sprintf("gfa_crayons_%d_score", i), NULL,
                        choices = choices, selected = "", width = "100%"))
                  )
                )
              }),
              div(class = "section-label", "🎤 Free Narrative / 自由叙事"),
              div(class = "narrative-box",
                p(strong("指示 Instruction: "), "请学生看着图片讲述故事。/ Ask student to tell the story using the pictures."),
                textAreaInput("narr_crayons", "学生回答 / Student Response:", width = "100%", rows = 5, placeholder = "学生在评估时的叙事内容..."),
                p(style = "margin-top: 10px; font-size: 13px; color: #6b7280;", "📝 请评估者在评估时记录学生叙事，并在下方评分。")
              ),
              div(class = "section-label", "📊 Narrative Rubric / 叙事评分"),
              lapply(seq_along(STORIES_ELEM$the_crayons$narrative_rubric$dimensions), function(d) {
                dim_name <- STORIES_ELEM$the_crayons$narrative_rubric$dimensions[d]
                div(style = "margin-bottom: 14px;",
                  div(class = "rubric-dim-label", sprintf("%s (0-2分)", dim_name)),
                  div(class = "rubric-row",
                    radioButtons(sprintf("nr_crayons_dim%d", d), NULL,
                      choices = c("0分"="0","1分"="1","2分"="2"),
                      selected = character(0), inline = TRUE, width = "100%")
                  )
                )
              }),
              div(style = "margin-top: 20px; text-align: center;",
                actionButton("save_crayons", "💾 保存 The Crayons 评分", class = "btn-save-slam")
              )
            )
          )
        )

      )  # end preK_elem_tabs
    ),    # end PreK-Elem tabPanel

    # ═══════════════════════════════════════════════════════════════════
    # TAB: Junior High-High School (初中-高中，13-17岁)
    # ═══════════════════════════════════════════════════════════════════
    tabPanel("📚 初中-高中 / JH-HS",
      tabsetPanel(id = "jh_hs_tabs", type = "tabs",

        # ── Baseball Troubles ────────────────────────────────────────
        tabPanel("🏇 Baseball Troubles", value = "baseball_troubles_tab",
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
          story_img_carousel("baseball_troubles", 7),

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

              div(class = "scoring-guide",
                tags$details(
                  tags$summary("Scoring Guide / 评分参考 (click to expand)"),
                  tags$h4("Language Characteristics / 语言特点"),
                  tags$ul(
                    tags$li(tags$span(class = "tier-label", "JH"), "Clear structure, logical sequence, describes events coherently"),
                    tags$li(tags$span(class = "tier-label", "HS"), "Mature expression, smooth transitions, complete psychological description")
                  ),
                  tags$h4("Narrative Examples / 叙事范例"),
                  tags$p(tags$span(class = "tier-label", "JH"), tags$i("Two big boys were playing soccer happily on the playground. Two little boys sat on the steps nearby, watching eagerly but too afraid to join. The boy in yellow told the boy in blue that he had gotten the soccer ball first, but the big boys took it away because they said he was not good. Then a parent called the big boys away. The two little boys were happy and started playing with the ball when the big boys were not looking. But they kicked too hard and the ball flew into the air and popped. The little boys were very scared. When the big boys came back and found the ball was gone, they looked confused. The little boys got nervous quickly.")),
                  tags$p(tags$span(class = "tier-label", "HS"), tags$i("On the playground, two older boys were playing soccer happily, while two younger boys on the nearby steps watched longingly but dared not join. The boy in yellow explained to his friend that he had gotten the soccer ball first, but the older boys had taken it away saying he was too bad. Just then, an adult called the big boys away, leaving the ball behind. Seizing the chance, the two little boys took the ball and played happily while the big boys were distracted. Unfortunately, they kicked too hard and the ball flew up and burst. The two boys were immediately filled with panic.")),
                  tags$h4("Sample Q&A / 问答示例"),
                  tags$ul(
                    tags$li(tags$strong("Q: Will the big boys share?"), " No, they took the ball away and said the little boys were not good at playing."),
                    tags$li(tags$strong("Q: What are the little boys thinking?"), " They really want to play soccer. They think it is unfair and hope they can get a chance."),
                    tags$li(tags$strong("Q: What would you say to get out of trouble?"), " I would say sorry and explain I did not mean to break it.")
                  )
                )
              ),
          div(class = "section-label", "📷 图片卡片 / Picture Cards"),
          story_img_carousel("the_ball_mystery", 7),
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

              div(class = "scoring-guide",
                tags$details(
                  tags$summary("Scoring Guide / 评分参考 (click to expand)"),
                  tags$h4("Language Characteristics / 语言特点"),
                  tags$ul(
                    tags$li(tags$span(class = "tier-label", "JH"), "Clear sequence, natural connectors, describes events in order"),
                    tags$li(tags$span(class = "tier-label", "HS"), "Complete logic, natural psychological description, mature expression")
                  ),
                  tags$h4("Narrative Examples / 叙事范例"),
                  tags$p(tags$span(class = "tier-label", "JH"), tags$i("After school, two boys met a girl sitting on a bench reading a book. The boy in orange looked at the girl and both blushed a little. He was holding his cellphone and a red folder. Later, he went into a convenience store to buy snacks and put his cellphone on the counter while paying. Just then, the same girl walked into the store. He took his food and went out to greet her, completely forgetting his cellphone on the counter. He walked and talked with the girl and did not remember his phone. When the girl took out her cellphone and shyly asked for his number, he looked everywhere but could not find it. He suddenly remembered he left it on the counter. He felt very worried and hurried back to the store to get it.")),
                  tags$p(tags$span(class = "tier-label", "HS"), tags$i("After school, two boys encountered a female classmate sitting on a bench reading. The boy in orange made eye contact with her, and both seemed a little shy. He was holding his cellphone and a red folder. Soon after, he went into a convenience store to buy snacks and placed his cellphone on the counter while checking out. Coincidentally, the girl he had just met also entered the store. Thinking only of greeting her, he took his food and left, completely leaving his cellphone behind. He walked and talked with the girl, totally unaware that he had lost his phone. It was not until the girl took out her cellphone and shyly asked for his contact information that he nervously checked his pockets but could not find it. He immediately realized he had left it on the counter.")),
                  tags$h4("Sample Q&A / 问答示例"),
                  tags$ul(
                    tags$li(tags$strong("Q: How did he lose his phone?"), " He put his phone on the counter at the store and got distracted, so he forgot to take it."),
                    tags$li(tags$strong("Q: Why did he leave his phone?"), " He got distracted by the girl and was focused on greeting her, so he forgot about his phone."),
                    tags$li(tags$strong("Q: What made him remember?"), " When the girl took out her phone to ask for his number, he realized his phone was missing.")
                  )
                )
              ),
          div(class = "section-label", "📷 图片卡片 / Picture Cards"),
          story_img_carousel("lost_cellphone", 6, images_per_page = 2),
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
          story_img_carousel("kittens_love_milk", 6, images_per_page = 2),
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

      )  # end jh_hs_tabs
    ),    # end JH-HS tabPanel

    # ── Report Tab ──────────────────────────────────────────────────
    tabPanel("📋 Report / 报告", value = "slam_report_tab",
      uiOutput("slam_report")
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

  # Proxy for server=FALSE DT refresh after delete
  proxy <- DT::dataTableProxy("slam_patient_dt")

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

    # Refresh the DT with fresh data
    fresh_patients <- dbGetQuery(con, "
      SELECT p.id, p.name, p.dob, p.gender, p.examiner,
             (SELECT COUNT(*) FROM assessments a WHERE a.patient_id = p.id) as n_assessments
      FROM patients p
      ORDER BY p.id DESC
    ")
    dt_df <- data.frame(
      ID          = fresh_patients$id,
      Name        = fresh_patients$name,
      DOB         = ifelse(is.na(fresh_patients$dob), "-", as.character(fresh_patients$dob)),
      Gender      = ifelse(is.na(fresh_patients$gender), "-",
                    ifelse(fresh_patients$gender == "M", "男 / M", "女 / F")),
      Examiner    = ifelse(is.na(fresh_patients$examiner), "-", fresh_patients$examiner),
      Assessments = fresh_patients$n_assessments,
      stringsAsFactors = FALSE
    )
    DT::replaceData(proxy, dt_df, resetPaging = FALSE)

    removeModal()
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
    # Switch to first story tab (JH-HS section, first story)
    updateTabsetPanel(session, "slam_main_tabs", selected = "📚 初中-高中 / JH-HS")
    updateTabsetPanel(session, "jh_hs_tabs", selected = "baseball_troubles_tab")
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

  # PreK-Elementary save handlers
  observeEvent(input$save_dog, {
    save_one_story("DogComesHome", "wf_dog", "gfa_dog", "narr_dog", 6, 6)
  })
  observeEvent(input$save_bunny, {
    save_one_story("BunnyGoesSchool", "wf_bunny", "gfa_bunny", "narr_bunny", 6, 9)
  })
  observeEvent(input$save_crayons, {
    save_one_story("TheCrayons", "wf_crayons", "gfa_crayons", "narr_crayons", 5, 5)
  })

  # ── AI Report — Patient DT ──────────────────────────────────
  # (removed — AI tab was removed)

  # ── AI Report — Selected Patient Report ─────────────────────
  # (removed — AI tab was removed)

  # ── SLAM Report Tab ─────────────────────────────────────────────
  output$slam_report <- renderUI({
    # Get most recent SLAM assessment for current patient
    pid <- rv$patient_id
    if (is.null(pid) || is.na(pid)) {
      return(div(
        style = "text-align:center; padding:60px 20px; color:#6b7280;",
        icon("clipboard-list", class = "fa-3x"),
        h3("暂无评估数据 / No Assessment Data"),
        p('请在"受试者信息"标签页选择或创建受试者，然后完成至少一个故事的评分保存。')
      ))
    }

    con <- get_con()
    on.exit(dbDisconnect(con))

    # Get most recent SLAM assessment for this patient
    aid_row <- dbGetQuery(con, "
      SELECT id, assessment_date FROM assessments
      WHERE patient_id = ? AND assessment_type = 'SLAM'
      ORDER BY assessment_date DESC LIMIT 1",
      params = list(pid))
    if (nrow(aid_row) == 0) {
      return(div(style="text-align:center; padding:40px;",
        p("该受试者暂无SLAM评估记录 / No SLAM assessments yet")))
    }
    aid <- aid_row$id[1]

    # Helper: GFA rubric description
    gfa_desc <- function(score, item_num, story_id) {
      # 0-2 scale with qualitative descriptions
      desc <- switch(as.character(score),
        "0" = "无回应或回答完全不符题意 / No response or entirely off-topic",
        "0.5" = "部分相关但语法/内容错误明显 / Partially relevant with significant errors",
        "1" = "基本正确但有轻微语法问题 / Generally correct with minor grammar issues",
        "1.5" = "较完整，偶有细节缺失 / Mostly complete, occasional missing details",
        "2" = "完整准确 / Complete and accurate",
        "未评分"
      )
      score_label <- if (score %% 1 == 0) paste0(score, ".0") else as.character(score)
      sprintf("<b>%s分</b> — %s", score_label, desc)
    }

    # Helper: age-normed interpretation for GFA total
    gfa_age_interp <- function(gfa_total, story_id, age_years) {
      norms <- switch(story_id,
        "dog_comes_home"   = list(typical = 11:14, probing = 8:10,  support_max = 7),
        "bunny_goes_school"= list(typical = 12:16, probing = 9:11,  support_max = 8),
        "the_crayons"      = list(typical = 6:8,   probing = 4:5,   support_max = 3),
        "baseball_troubles"= list(typical = 12:16, probing = 9:11,  support_max = 8),
        "lost_cellphone"   = list(typical = 12:16, probing = 9:11,  support_max = 8),
        "the_ball_mystery" = list(typical = 12:16, probing = 9:11,  support_max = 8),
        "kittens_love_milk"= list(typical = 12:16, probing = 9:11,  support_max = 8),
        NULL
      )
      if (is.null(norms)) return(list(level = "unknown", label_zh = "未知", color = "#6b7280"))

      level <- if (gfa_total %in% norms$typical) {
        list("typical", "典型发展", "#16a34a")
      } else if (gfa_total %in% norms$probing) {
        list("probing", "需进一步探查", "#ea580c")
      } else if (gfa_total <= norms$support_max) {
        list("support", "可能需要支持", "#dc2626")
      } else {
        list("unknown", "待评估", "#6b7280")
      }
      list(level = level[[1]], label_zh = level[[2]], color = level[[3]])
    }

    # Fetch responses
    res <- dbGetQuery(con, "
      SELECT subtest, item_number, response_text, score
      FROM responses WHERE assessment_id = ? ORDER BY subtest, item_number",
      params = list(aid))

    # Fetch subtest scores
    sts <- dbGetQuery(con, "
      SELECT subtest, raw_score, scaled_score
      FROM subtest_scores WHERE assessment_id = ?",
      params = list(aid))

    # Build story blocks — PreK-Elem stories
    stories_prek <- list(
      list(id = "dog_comes_home",   db_id = "DogComesHome",   name = "🏠 Dog Comes Home",      name_zh = "狗回家"),
      list(id = "bunny_goes_school",db_id = "BunnyGoesSchool",name = "🐰 Bunny Goes to School",  name_zh = "小兔子去学校"),
      list(id = "the_crayons",       db_id = "TheCrayons",     name = "🖍️ The Crayons",          name_zh = "蜡笔大战")
    )
    # JH-HS stories
    stories_jh <- list(
      list(id = "baseball_troubles",  db_id = "BaseballTroubles",  name = "🏇 Baseball Troubles",   name_zh = "棒球烦恼"),
      list(id = "the_ball_mystery",   db_id = "TheBestTurkey",    name = "🔵 The Ball Mystery",     name_zh = "神秘小球"),
      list(id = "lost_cellphone",      db_id = "GirlWhoLovedHorses",name = "📱 Lost Cellphone",       name_zh = "丢失的手机"),
      list(id = "kittens_love_milk",  db_id = "WallaceAndBatty",   name = "🐱 Kittens Love Milk",   name_zh = "小猫爱牛奶")
    )

    # Report group selector
    report_group <- input$slam_report_group %||% "elem"

    stories <- if (report_group == "elem") stories_prek else stories_jh

    story_blocks <- lapply(stories, function(s) {
      wf_sub  <- paste0(s$db_id, "_WordFinding")
      gfa_sub <- paste0(s$db_id, "_GFA")
      nar_sub <- paste0(s$db_id, "_Narrative")

      # Raw scores
      wf_raw  <- sts$raw_score[sts$subtest == wf_sub][1]
      gfa_raw <- sts$raw_score[sts$subtest == gfa_sub][1]
      wf_std  <- sts$scaled_score[sts$subtest == wf_sub][1]
      gfa_std <- sts$scaled_score[sts$subtest == gfa_sub][1]

      wf_pr <- std_to_pr(wf_std)
      gfa_pr <- std_to_pr(gfa_std)

      # GFA responses
      gfa_rows <- res[res$subtest == gfa_sub, ]
      gfa_items <- if (report_group == "elem") {
        STORIES_ELEM[[s$id]]$gfa_items
      } else {
        STORIES[[s$id]]$gfa_items
      }
      n_gfa <- nrow(gfa_items)

      # WF responses
      wf_rows <- res[res$subtest == wf_sub, ]

      # Narrative
      nar_row <- res[res$subtest == nar_sub, ]
      nar_text <- if (nrow(nar_row) > 0) nar_row$response_text[1] else ""

      # Build GFA item list
      gfa_item_html <- lapply(seq_len(n_gfa), function(i) {
        item_score_val <- if (i <= nrow(gfa_rows)) as.numeric(gfa_rows$score[i]) else NA
        item_text_val <- if (i <= nrow(gfa_rows)) gfa_rows$response_text[i] else ""
        score_display <- if (!is.na(item_score_val)) {
          if (item_score_val %% 1 == 0) paste0(as.integer(item_score_val), ".0") else as.character(item_score_val)
        } else "—"
        item_desc <- if (!is.na(item_score_val)) {
          switch(as.character(item_score_val),
            "0" = "无回应或明显错误",
            "0.5" = "部分相关，语法/内容错误",
            "1" = "基本正确，轻微问题",
            "1.5" = "较完整，偶有缺失",
            "2" = "完整准确",
            "未评分"
          )
        } else ""
        tags$div(class = "gfa-item",
          tags$div(class = "gfa-passage",
            HTML(gsub("\\{\\{blank\\}\\}", sprintf("<b>___%d</b>", i),
              if (grepl("___", gfa_items$passage_en[i])) gfa_items$passage_en[i]
              else gfa_items$passage_en[i])),
            p(style = "margin-top:4px; font-size:12px; color:#6b7280;", gfa_items$passage_zh[i])
          ),
          fluidRow(
            column(8,
              textInput(sprintf("rpt_gfa_%s_%d_text", s$db_id, i),
                "学生回答 / Response:", value = item_text_val, width = "100%"))
          ),
          tags$div(style = "margin-top:4px;",
            span("评分: ", class = "badge", score_display),
            span(if (!is.na(item_score_val)) item_desc, style = "font-size:12px; color:#374151;")
          )
        )
      })

      tags$div(class = "story-card", style = "margin-bottom:30px;",
        tags$div(class = "story-card-header",
          span(s$name), paste0(s$name_zh, " / ", s$id)
        ),
        tags$div(class = "story-card-body",
          # Score summary row
          fluidRow(
            column(4,
              tags$div(style = "text-align:center; padding:12px; background:#f0f9ff; border-radius:10px;",
                p("Word Finding", style = "font-weight:700; margin:0; color:#EA580C;"),
                p(sprintf("原始分: %s", if (length(wf_raw)) wf_raw else "—"), style = "font-size:13px; margin:4px 0;"),
                p(sprintf("标准化: %s", if (length(wf_std) && !is.na(wf_std)) wf_std else "—"), style = "font-size:13px; margin:4px 0;"),
                p(sprintf("百分位: %s%%", if (length(wf_pr) && !is.na(wf_pr)) round(wf_pr, 1) else "—"), style = "font-size:13px; margin:4px 0;")
              )
            ),
            column(4,
              tags$div(style = "text-align:center; padding:12px; background:#fff7ed; border-radius:10px;",
                p("GFA", style = "font-weight:700; margin:0; color:#EA580C;"),
                p(sprintf("原始分: %s", if (length(gfa_raw)) gfa_raw else "—"), style = "font-size:13px; margin:4px 0;"),
                p(sprintf("标准化: %s", if (length(gfa_std) && !is.na(gfa_std)) gfa_std else "—"), style = "font-size:13px; margin:4px 0;"),
                p(sprintf("百分位: %s%%", if (length(gfa_pr) && !is.na(gfa_pr)) round(gfa_pr, 1) else "—"), style = "font-size:13px; margin:4px 0;"),
                # Age-normed interpretation
                if (length(gfa_raw) && !is.na(gfa_raw)) {
                  interp <- gfa_age_interp(as.numeric(gfa_raw), s$id, NA)
                  tags$div(style = sprintf("margin-top:8px; padding:6px 8px; background:%s22; border-radius:6px; border:1.5px solid %s;",
                    substr(interp$color, 2, 7), interp$color),
                    tags$span(style = sprintf("color:%s; font-weight:700; font-size:12px;", interp$color),
                      interp$label_zh)
                  )
                }
              )
            ),
            column(4,
              tags$div(style = "text-align:center; padding:12px; background:#f5f3ff; border-radius:10px;",
                p("Narrative", style = "font-weight:700; margin:0; color:#7c3aed;"),
                p(sprintf("叙事字数: %d字", nchar(nar_text)), style = "font-size:13px; margin:4px 0;"),
                p(sprintf("评估日期: %s", aid_row$assessment_date[1]), style = "font-size:12px; margin:4px 0; color:#6b7280;")
              )
            )
          ),

          # GFA Items
          tags$div(class = "section-label", "📝 GFA 语法问答 — Scoring Commentary"),
          gfa_item_html,

          # Narrative text
          tags$div(class = "section-label", "🎤 自由叙事 / Free Narrative"),
          tags$div(class = "narrative-box",
            textAreaInput(sprintf("rpt_narr_%s", s$db_id), NULL,
              value = nar_text, width = "100%", rows = 4,
              placeholder = "学生叙事内容...")
          )
        )
      )
    })

    # Feedback section
    feedback_section <- tags$div(
      tags$div(class = "section-label", "💬 评估师反馈 / Examiner Feedback"),
      textAreaInput("slam_report_feedback", NULL,
        value = "", width = "100%", rows = 4,
        placeholder = "在此输入评估师评语、观察、建议..."),
      div(style = "margin-top:20px; text-align:center;",
        actionButton("save_slam_report", "💾 保存报告 / Save Report",
          class = "btn-save-slam")
      )
    )

    tagList(
      div(style = "max-width:900px; margin:0 auto; padding:20px;",
        tags$div(style = "text-align:center; margin-bottom:30px;",
          h2("📋 SLAM 评估报告 / Assessment Report"),
          p(sprintf("评估日期: %s | Assessment ID: %d", aid_row$assessment_date[1], aid),
            style = "color:#6b7280;"),
          # Group selector
          div(style = "margin-top: 15px;",
            radioButtons("slam_report_group", NULL,
              choices = c("🧒 PreK-小学 / PreK-Elem" = "elem",
                          "📚 初中-高中 / JH-HS" = "jh"),
              selected = input$slam_report_group %||% "elem",
              inline = TRUE
            )
          )
        ),
        story_blocks,
        feedback_section
      )
    )
  })

  # Save report feedback
  observeEvent(input$save_slam_report, {
    pid <- rv$patient_id
    if (is.null(pid)) {
      showNotification("请先选择受试者 / Please select a subject first", type = "error")
      return()
    }
    feedback <- input$slam_report_feedback
    if (nzchar(feedback)) {
      tryCatch({
        con <- get_con()
        on.exit(dbDisconnect(con))
        aid_row <- dbGetQuery(con, "
          SELECT id FROM assessments
          WHERE patient_id = ? AND assessment_type = 'SLAM'
          ORDER BY assessment_date DESC LIMIT 1",
          params = list(pid))
        if (nrow(aid_row) > 0) {
          dbExecute(con,
            "UPDATE assessments SET notes = ? WHERE id = ?",
            params = list(feedback, aid_row$id[1]))
          showNotification("报告已保存 / Report saved", type = "message")
        }
      }, error = function(e) {
        showNotification(sprintf("保存失败: %s", e$message), type = "error")
      })
    }
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
