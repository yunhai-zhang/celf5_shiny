# app.R — CELF-5 Assessment Shiny App
# tidyverse rebuild + SQLite 持久化
# 评估人员自带 Stimulus Books，本 app 仅录入分数、自动计算

library(shiny)
library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)
library(glue)
library(rlang)
library(DT)
library(stringr)

source("global.R")

# ─────────────────────────────────────────────────────────────
# UI — CELF-5 配色：深蓝 #1B3A6B + 白 + 浅灰
# ─────────────────────────────────────────────────────────────
celf5_blue   <- "#1B3A6B"
celf5_gold  <- "#C8A951"
celf5_gray  <- "#F5F5F5"
celf5_white <- "#FFFFFF"

ui <- fluidPage(
  tags$head(
    tags$script(HTML("
      // Clear dateInput browser-cache values on page load
      // DOB needs manual entry so clear it; assessment_date defaults to today so leave it
      window.addEventListener('load', function() {
        setTimeout(function() {
          var dob = document.querySelector('#dob input');
          if (dob) { dob.value = ''; dob.dispatchEvent(new Event('change', {bubbles: true})); }
        }, 200);
      });
    ")),
    tags$style(HTML(sprintf("
      body { background: %s; font-family: 'Segoe UI', Arial, sans-serif; }
      .container-fluid { padding: 0; }
      .tab-content { padding: 20px; background: %s; min-height: 100vh; }
      .nav-tabs > li > a { color: %s; font-weight: 600; }
      .nav-tabs > li.active > a { background: %s !important; color: %s !important; }
      .nav-tabs > li > a:hover { color: %s; }
      .form-control { border-radius: 6px; border: 1px solid #ccc; }
      .btn-primary { background: %s; border-color: %s; font-weight: 600; }
      .btn-primary:hover { background: %s; border-color: %s; }
      .btn-default { border-radius: 6px; }
      h1, h2, h3, h4 { color: %s; }
      .panel { border-radius: 10px; border: 1px solid #ddd; box-shadow: 0 2px 4px rgba(0,0,0,0.05); }
      .panel-heading { background: %s; color: %s; border-radius: 10px 10px 0 0; font-weight: 600; }
      .panel-body { background: %s; }
      .well { background: %s; border-radius: 8px; border: none; }
      .alert-info { background: #E8F0FE; border-color: %s; color: %s; }
      .alert-success { background: #E8F5E9; border-color: #4CAF50; color: #2E7D32; }
      .alert-warning { background: #FFF8E1; border-color: %s; color: #F57F17; }
      .shiny-table { width: 100%%; }
      . shiny-table th { background: %s; color: %s; }
      . shiny-table tr:nth-child(even) { background: #FAFAFA; }
      ::-webkit-scrollbar { width: 8px; }
      ::-webkit-scrollbar-track { background: %s; }
      ::-webkit-scrollbar-thumb { background: %s; border-radius: 4px; }
    ",
    celf5_white, celf5_white,
    celf5_blue, celf5_blue, celf5_white,
    celf5_blue,
    celf5_blue, celf5_blue, "#1452A3", "#1452A3",
    celf5_blue,
    celf5_blue, celf5_white,
    celf5_white, celf5_gray,
    celf5_blue, celf5_blue,
    celf5_blue,
    celf5_blue, celf5_white,
    celf5_gray, celf5_blue
    )))
  ),

  titlePanel(
    div(h1("CELF-5 语言评估系统", style = sprintf("color:%s; margin:0;", celf5_blue)),
        p("Clinical Evaluation of Language Fundamentals — Fifth Edition",
          style = "color:#888; font-size:14px; margin:0;")),
    windowTitle = "CELF-5"
  ),

  tabsetPanel(id = "main_tabs",

    # ── Tab 1: 受试者信息 ─────────────────────────────
    tabPanel("受试者信息 / Subject Info",
      fluidRow(
        column(4,
          div(class = "panel",
            div(class = "panel-heading", "基本信息 / Basic Info"),
            div(class = "panel-body",
              textInput("patient_name", "姓名 * / Name *", placeholder = "受试者姓名"),
              selectInput("patient_gender", "性别 / Gender",
                          choices = c("— 请选择 / Select —" = "",
                                      "男 / Male"   = "M",
                                      "女 / Female" = "F"),
                          selected = "", width = "100%"),
              textInput("school_name", "学校 / School", placeholder = "就读学校"),
              textInput("grade_level", "年级 / Grade", placeholder = "如：小一、初二、高一"),
              textInput("examiner", "评估师 / Examiner", placeholder = "评估师姓名"),
              dateInput("dob", "出生日期 * / Date of Birth *", format = "yyyy-mm-dd", value = character(0)),
              dateInput("assessment_date", "评估日期 * / Assessment Date *",
                        format = "yyyy-mm-dd", value = Sys.Date()),
              actionButton("btn_start", "▶ 开始评估 / Start Assessment", class = "btn-primary",
                           style = sprintf("width:100%%; background:%s; border-color:%s;", celf5_blue, celf5_blue))
            )
          )
        ),
        column(8,
          div(class = "panel",
            div(class = "panel-heading", "历史评估记录 / Assessment History"),
            div(class = "panel-body",
              dataTableOutput("assessments_table"),
              uiOutput("load_btn_ui")
            )
          )
        )
      )
    ),

    # ── Tab 2: 评估进度 ─────────────────────────────
    tabPanel("评估进度 / Progress",
      fluidRow(
        column(12,
          h3(textOutput("current_patient")),
          h4(textOutput("current_age"))
        )
      ),
      fluidRow(
        column(12, uiOutput("subtest_progress_ui"))
      )
    ),

    # ── Tab 3: 测试题目 ─────────────────────────────
    tabPanel("测试题目 / Test Items",
      fluidRow(
        column(3,
          wellPanel(
            uiOutput("subtest_selector"),
            uiOutput("item_calculator_ui")
          )
        ),
        column(9,
          wellPanel(
            uiOutput("question_ui"),
            fluidRow(
              column(4, actionButton("btn_prev", "◀ 上一题 / Prev", style = "width:100%")),
              column(4, actionButton("btn_save_score", "💾 保存并下一题 / Save & Next",
                          class = "btn-primary",
                          style = sprintf("width:100%%; background:%s;", celf5_blue))),
              column(4, actionButton("btn_next", "▶ 仅下一题 / Next Only", style = "width:100%"))
            ),
            hr(),
            fluidRow(column(12, h5("已打分 / Scored: "), textOutput("subtest_progress_text")))
          )
        )
      )
    ),

    # ── Tab 3b: 行为观察 ──────────────────────────────
    tabPanel("行为观察 / ORS",
      fluidRow(
        column(12,
          uiOutput("ors_ui")
        )
      )
    ),

    # ── Tab 4: 评分报告 ─────────────────────────────
    tabPanel("评分报告 / Report",
      fluidRow(
        column(12, uiOutput("report_ui"))
      )
    )
  )
)

# ─────────────────────────────────────────────────────────────
# Server
# ─────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  rv <- reactiveValues(
    patient_id = NULL,
    assessment_id = NULL,
    dob = NULL,
    assessment_date = NULL,
    age = NULL,
    age_group = NULL,
    test_list = character(0),
    current_subtest = NULL,
    current_item = 1L,
    start_point = 1L,
    responses = tibble(subtest=character(), item_number=integer(),
                       response_text=character(), score=integer()),
    completed_subtests = character(0),
    discontinue_triggered = FALSE
  )

  # ── 历史评估列表（renderDataTable 模式 — server=TRUE 启用 rows_selected）──
  output$assessments_table <- renderDataTable({
    df <- tryCatch({
      list_assessments() %>%
        mutate(age_str = glue("{age_years}y {age_months}m"),
               date = as.character(assessment_date)) %>%
        select(姓名=patient_name, 评估日期=date, 年龄=age_str, 状态=status)
    }, error = function(e) {
      data.frame(姓名=character(), 评估日期=character(), 年龄=character(), 状态=character())
    })
    if (nrow(df) == 0) {
      return(df)
    }
    DT::datatable(df, selection = "single",
                  options = list(pageLength = 10,
                    language = list(emptyTable = "暂无历史评估记录")),
                  rownames = FALSE)
  }, server = FALSE)

  # dataTableProxy 用于 server=FALSE 模式下刷新表格
  proxy <- dataTableProxy("assessments_table")

  # 加载按钮（独立 output，选中行后才显示）
  output$load_btn_ui <- renderUI({
    req(!is.null(input$assessments_table_rows_selected))
    tagList(
      hr(),
      fluidRow(
        column(6,
          actionButton("btn_load_assessment", "📂 加载 / Load",
                       class = "btn-primary",
                       style = sprintf("width:100%%; background:%s;", celf5_blue))
        ),
        column(6,
          actionButton("btn_delete_confirm", "🗑 删除 / Delete",
                       class = "btn-danger",
                       style = "width:100%;")
        )
      )
    )
  })

  # ── 开始新评估 ─────────────────────────────────────────
  observeEvent(input$btn_start, {
    req(input$patient_name, input$dob, input$assessment_date)

    dob_str <- as.character(input$dob)
    assess_str <- as.character(input$assessment_date)

    if (dob_str == "" || is.na(input$dob) || is.null(input$dob)) {
      showNotification("请填写有效的出生日期 / Please enter a valid date of birth",
                        type = "error"); return()
    }
    if (assess_str == "" || is.na(input$assessment_date) || is.null(input$assessment_date)) {
      showNotification("请填写有效的评估日期 / Please enter a valid assessment date",
                        type = "error"); return()
    }

    age <- calculate_age(input$dob, input$assessment_date)
    age_group <- get_age_group(age)

    if (age_group == "out_of_range") {
      showModal(modalDialog(
        title = "年龄超出范围",
        "CELF-5 适用于 5:0–21:11 岁。",
        easyClose = TRUE
      ))
      return()
    }

    pid <- upsert_patient(input$patient_name, as.character(input$dob),
                          input$patient_gender, input$examiner)
    aid <- start_assessment(pid, as.character(input$assessment_date),
                           age$years, age$months, age$days, age_group)

    rv$patient_id <- pid
    rv$assessment_id <- aid
    rv$dob <- input$dob
    rv$assessment_date <- input$assessment_date
    rv$age <- age
    rv$age_group <- age_group
    rv$test_list <- get_test_composition(age_group)
    rv$completed_subtests <- character(0)
    rv$discontinue_triggered <- FALSE
    rv$responses <- tibble(subtest=character(), item_number=integer(),
                          response_text=character(), score=integer())
    # Auto-select first subtest to avoid NULL current_subtest on question tab
    first_test <- rv$test_list[1]
    rv$current_subtest <- first_test
    rv$current_item <- 1L
    rv$start_point <- get_start_point(first_test, age_group)
    updateTabsetPanel(session, "main_tabs", selected = "评估进度 / Progress")
  })

  # ── 加载已有评估 ───────────────────────────────────────
  observeEvent(input$btn_load_assessment, {
    row <- input$assessments_table_rows_selected
    if (is.null(row) || length(row) == 0) {
      showNotification("请先在表格中选择一行", type = "warning"); return()
    }
    assessments <- tryCatch(list_assessments() %>%
      mutate(age_str = glue("{age_years}y {age_months}m"),
             date = as.character(assessment_date)) %>%
      select(姓名=patient_name, 评估日期=date, 年龄=age_str, 状态=status),
      error = function(e) NULL)
    if (is.null(assessments)) { showNotification("加载失败", type = "error"); return() }
    sel <- assessments[row, ]
    all_assessments <- list_assessments()
    full <- get_assessment_full(all_assessments$id[row])

    rv$assessment_id <- all_assessments$id[row]
    rv$patient_id <- full$assessment$patient_id
    rv$dob <- as.Date(full$assessment$dob)
    rv$assessment_date <- as.Date(full$assessment$assessment_date)
    rv$age <- calculate_age(rv$dob, rv$assessment_date)
    rv$age_group <- full$assessment$age_group
    rv$test_list <- get_test_composition(rv$age_group)
    rv$completed_subtests <- character(0)
    rv$discontinue_triggered <- FALSE
    # Initialize current_subtest to avoid NULL on question tab
    first_test <- rv$test_list[1]
    rv$current_subtest <- first_test
    rv$current_item <- 1L
    rv$start_point <- get_start_point(first_test, rv$age_group)

    if (nrow(full$responses) > 0) {
      rv$responses <- full$responses %>%
        select(subtest, item_number, response_text, score) %>%
        mutate(across(everything(), ~replace_na(as.character(.), ""))) %>%
        mutate(item_number = as.integer(item_number),
               score = as.integer(score))
    }

    updateTabsetPanel(session, "main_tabs", selected = "评估进度 / Progress")
  })

  # ── 删除评估（两步确认）────────────────────────────────
  observeEvent(input$btn_delete_confirm, {
    row <- input$assessments_table_rows_selected
    if (is.null(row) || length(row) == 0) {
      showNotification("请先在表格中选择一行", type = "warning"); return()
    }
    all_assessments <- list_assessments()
    sel_id   <- all_assessments$id[row]
    sel_name <- all_assessments$patient_name[row]
    sel_date <- as.character(all_assessments$assessment_date[row])
    msg <- paste0(
      "确定要删除以下评估记录吗？此操作不可撤销。\n\n",
      "受试者：", sel_name, "\n",
      "评估日期：", sel_date, "\n",
      "评估编号：#", sel_id
    )
    showModal(modalDialog(
      title = "⚠️ 确认删除 / Confirm Delete",
      HTML(str_replace_all(msg, "\n", "<br>")),
      footer = tagList(
        actionButton("btn_delete_confirmed",
                     "🗑 确认删除 / Confirm Delete",
                     class = "btn-danger"),
        modalButton("取消 / Cancel")
      ),
      easyClose = FALSE
    ))
  })

  observeEvent(input$btn_delete_confirmed, {
    row <- input$assessments_table_rows_selected
    if (is.null(row) || length(row) == 0) { removeModal(); return() }
    all_assessments <- list_assessments()
    sel_id <- all_assessments$id[row]
    tryCatch({
      delete_assessment(sel_id)
      showNotification(paste0("已删除评估 #", sel_id), type = "message")
      rv$assessment_id <- NULL
      rv$patient_id    <- NULL
      # server=FALSE 的 DT 无法用 proxy 增量刷新，直接 reload 整页
      session$reload()
      removeModal()
    }, error = function(e) {
      showNotification(paste0("删除失败: ", e$message), type = "error")
      removeModal()
    })
  })

  # ── Header 信息 ─────────────────────────────────────────
  output$current_patient <- renderText({
    req(rv$assessment_id)
    con <- get_con(); on.exit(dbDisconnect(con))
    name <- dbGetQuery(con, "SELECT name FROM patients WHERE id=?",
                       params = list(rv$patient_id))$name
    glue("受试者: {name}  |  评估日期: {rv$assessment_date}  |  编号: #{rv$assessment_id}")
  })

  output$current_age <- renderText({
    req(rv$age)
    glue("年龄: {format_age(rv$age)} ({rv$age_group})  |  测试组合: {paste(rv$test_list, collapse=', ')}")
  })

  # ── 测试进度（可点击卡片） ──────────────────────────────
  output$subtest_progress_ui <- renderUI({
    req(rv$test_list)
    map(rv$test_list, function(t) {
      is_done <- t %in% rv$completed_subtests
      max_i <- get_max_item(t, rv$age_group)
      n_done <- sum(rv$responses$subtest == t, na.rm = TRUE)
      badge <- if (is_done) "✓ 完成" else glue("{n_done}/{max_i} 题")
      bg <- if (is_done) "#d4edda" else "#f8f9fa"
      col <- if (is_done) "#155724" else "#1B3A6B"
      box <- SUBTEST_DEFS %>% filter(subtest==t) %>% pull(full_name) %>% .[[1]]
      cursor <- if (is_done) "default" else "pointer"
      tags$a(
        href = "#",
        onclick = sprintf("Shiny.setInputValue('jump_to_subtest', '%s', {priority: 'event'});", t),
        style = glue("background:{bg}; border-radius:8px; padding:12px; margin:6px; display:inline-block; width:200px; text-decoration:none; cursor:{cursor}; border:1px solid #ddd;"),
        strong(style = glue("color:{col}; display:block;"), t),
        span(style = glue("color:{col}; font-size:12px;"), box),
        br(),
        span(style = glue("color:{col}; font-size:13px; font-weight:600;"), badge)
      )
    }) %>% tagList()
  })

  # 进度卡片点击 → 跳转到对应 subtest 的评分页面
  observeEvent(input$jump_to_subtest, {
    t <- input$jump_to_subtest
    updateTabsetPanel(session, "main_tabs", selected = "测试题目 / Test Items")
    rv$current_subtest <- t
    sub_resp <- rv$responses %>% filter(subtest == t)
    sp <- get_start_point(t, rv$age_group)
    all_items <- seq_len(get_max_item(t, rv$age_group))
    done_items <- sub_resp$item_number
    next_item <- min(setdiff(all_items, done_items), na.rm = TRUE)
    rv$current_item <- next_item
    rv$start_point <- sp
    rv$discontinue_triggered <- FALSE
  })

  # ── Subtest 选择 ─────────────────────────────────────────
  output$subtest_selector <- renderUI({
    req(rv$test_list)
    opts <- setNames(rv$test_list, map_chr(rv$test_list, ~{
      val <- SUBTEST_DEFS %>% filter(subtest==.x) %>% pull(full_name)
      if (length(val)==0) .x else val[[1]]
    }))
    selectInput("selected_subtest", "选择测试 / Select Subtest", choices = opts, selectize=FALSE)
  })

  observeEvent(input$selected_subtest, {
    rv$current_subtest <- input$selected_subtest
    sub_resp <- rv$responses %>% filter(subtest == rv$current_subtest)
    max_done <- if (nrow(sub_resp) > 0) max(sub_resp$item_number) else 0L
    sp <- get_start_point(rv$current_subtest, rv$age_group)
    all_items <- seq_len(get_max_item(rv$current_subtest, rv$age_group))
    done_items <- sub_resp$item_number
    next_item <- min(setdiff(all_items, done_items), na.rm=TRUE)
    rv$current_item <- next_item
    rv$start_point <- sp
    rv$discontinue_triggered <- FALSE
  })

  # ── 题目 UI ─────────────────────────────────────────────
  output$question_ui <- renderUI({
    req(rv$current_subtest, rv$current_item)

    t <- rv$current_subtest
    item_n <- rv$current_item
    sp <- rv$start_point
    max_item <- get_max_item(t, rv$age_group)

    if (rv$discontinue_triggered) {
      return(div(class="alert alert-warning", style="margin-top:20px",
                 h3("⏹ Discontinue: 连续4题0分，该测试结束 / 4 consecutive 0s — subtest ended")))
    }

    if (item_n > max_item) {
      return(div(class="alert alert-success", style="margin-top:20px",
                 h3(glue("✓ {t} 完成（共 {max_item} 题）/ Complete ({max_item} items)"))))
    }

    # Reversal 检查
    sub_resp <- rv$responses %>% filter(subtest==t)
    items_before <- (sp:(item_n-1)) %>% .[.>=1]
    if (length(items_before) >= 2) {
      last_two <- tail(items_before, 2)
      scores_b <- sub_resp %>% filter(item_number %in% last_two) %>%
        arrange(item_number) %>% pull(score)
      if (length(scores_b)==2 && all(scores_b==max_score_for_subtest(t))) {
      items_before %>%
        iwalk(function(.x, .y) {
          if (!any(rv$responses$subtest==t & rv$responses$item_number==.x)) {
            rv$responses <- rv$responses %>% add_row(
              subtest=t, item_number=.x, response_text="Reversal满分",
              score=max_score_for_subtest(t))
          }
        }) %>% invisible()
        showNotification(glue("Reversal 触发！{sp}-{item_n-1}题记满分"), type="message")
      }
    }

    box_title <- SUBTEST_DEFS %>% filter(subtest==t) %>% pull(full_name) %>% .[[1]]
    qi <- get_question_info(t, item_n, rv$age_group)

    # 题目文字优先用 question_en，其次 prompt_en
    stimulus_txt <- if (!is.na(qi$question_en) && nzchar(qi$question_en)) qi$question_en[1] else ""
    prompt_txt   <- if (!is.na(qi$prompt_en)   && nzchar(qi$prompt_en))   qi$prompt_en[1]   else ""
    scoring_txt  <- if (!is.na(qi$scoring_key) && nzchar(qi$scoring_key)) qi$scoring_key[1] else ""

    # Trial / Demo 提示（起始点前的引导题不显示题目卡片）
    is_trial <- item_n < sp

    tagList(
      h3(glue("{box_title} — 第 {item_n} / {max_item} 题 / Item {item_n} of {max_item}")),
      if (item_n == sp) div(class="alert alert-info", "★ 起始点题号 / Start Point Item"),
      if (is_trial) div(class="alert alert-secondary", "引导题（不记分）Trial Item (not scored)"),
      hr(),

      # ── 题目/刺激物显示（trial 也显示题目，只是不打分）────────
      if (nzchar(stimulus_txt)) {
        div(class="card mb-3", style="background:#f8f9fa",
          div(class="card-body",
            p(strong("题目 Stimulus: "), HTML(stimulus_txt)),
            if (nzchar(prompt_txt))
              p(strong("施测说明 Prompt: "), HTML(prompt_txt)),
            if (!is_trial && nzchar(scoring_txt))
              p(strong("评分标准 Scoring: "), HTML(scoring_txt))
          )
        )
      } else {
        div(class="alert alert-secondary", "题目加载中 / Question not yet available for this item")
      },

      hr(),

      # trial items 只展示题目，不渲染打分控件
      if (!is_trial) uiOutput("score_input_ui"),
      if (is_trial) div(class="alert alert-light", "← 引导题无需打分 / Trial — no score needed")
    )
  })

  # Manual Chapter 3 评分类型（全部核实自原文）：
  # FD/SA/SR/RC/SW: 1分制（1=正确, 0=错误）
  # FS/WD/USP: 2分制（2=正确, 1=部分, 0=错误）
  # RS: 3分制（3=0错误, 2=1错误, 1=2-3错误, 0=4+错误）
  max_score_for_subtest <- function(t) {
    case_when(t %in% c("SC","LC","WS","WC","FD","SA","SR","RC","SW") ~ 1L,
              t %in% c("FS","WD","USP") ~ 2L,
              t == "RS" ~ 3L,
              TRUE ~ 1L)
  }

  # ── 评分输入 UI ─────────────────────────────────────────
  output$score_input_ui <- renderUI({
    req(rv$current_subtest, rv$current_item)
    t <- rv$current_subtest
    cur_resp <- rv$responses %>% filter(subtest==!!t, item_number==!!rv$current_item)
    cur_score <- if (nrow(cur_resp)>0) cur_resp$score[1] else NA_integer_
    max_s <- max_score_for_subtest(t)

    if (t == "RS") {
      # UI shows error count; stored score is scaled (3=0err, 2=1err, 1=2-3err, 0=4+err)
      err_val <- if (!is.na(cur_score) && !is.null(cur_score)) max_s - as.integer(cur_score) else NA_integer_
      tagList(
        numericInput("input_score", "错误数量（0=3分, 1=2分, 2-3=1分, 4+=0分）",
                     value=err_val, min=0, max=99, step=1)
      )
    } else if (max_s == 1L) {
      tagList(
        radioButtons("input_score", "得分",
                     choices=c("1分（正确）"=1L, "0分（错误）"=0L),
                     selected=cur_score)
      )
    } else {
      opts <- setNames(as.character(2:0), c("2分", "1分", "0分"))
      tagList(
        radioButtons("input_score", "得分",
                     choices=list("2分"=2L, "1分"=1L, "0分"=0L),
                     selected=cur_score)
      )
    }
  })

  output$subtest_progress_text <- renderText({
    req(rv$current_subtest)
    sub_r <- rv$responses %>% filter(subtest==!!rv$current_subtest) %>% arrange(item_number)
    if (nrow(sub_r)==0) return("暂无 / None")
    paste(tail(sub_r$score, 20), collapse=" ")
  })

  # ── 按钮交互（移到 tab 3 外部，点击事件始终有效）──────────

  observeEvent(input$btn_save_score, {
    t <- rv$current_subtest
    i_n <- rv$current_item
    sp <- rv$start_point

    # Trial items have no score input — skip silently
    if (i_n < sp) { showNotification("引导题无需保存 / Trial item — not saved", type="message"); return() }

    captured_score <- input$input_score
    captured_resp  <- input$response_text %||% ""

    if (is.null(captured_score) || is.na(captured_score)) {
      showNotification("请先打分 / Please score first", type = "warning"); return()
    }
    sv <- captured_score
    if (t=="RS" && !is.na(sv)) sv <- score_rs(as.integer(sv))
    rt <- captured_resp

    rv$responses <- rv$responses %>% filter(!(subtest==!!t & item_number==!!i_n)) %>%
      add_row(subtest=t, item_number=i_n, response_text=as.character(rt),
              score=as.integer(sv))
    save_response(rv$assessment_id, t, i_n, as.character(rt), as.integer(sv))
    check_discontinue(t)
    showNotification(glue("已保存 / Saved: {t} 第{i_n}题 = {sv}分"), type="message")

    # ── 导航：保存后自动前进到下一题 ───────────────────────────
    max_i <- get_max_item(t, rv$age_group)
    if (rv$discontinue_triggered || i_n >= max_i) {
      # 本测验结束，切换到下一个
      rv$completed_subtests <- c(rv$completed_subtests, t) %>% unique()
      next_t <- setdiff(rv$test_list, rv$completed_subtests)[1]
      if (!is.na(next_t)) {
        updateSelectInput(session, "selected_subtest", selected = next_t)
      }
    } else {
      # 清除当前打分控件，避免下一题残留
      if (t == "RS") {
        updateNumericInput(session, "input_score", value = NA_integer_)
      } else {
        updateRadioButtons(session, "input_score", selected = NA_integer_)
      }
      rv$current_item <- i_n + 1L
    }
  })

  observeEvent(input$btn_prev, {
    if (rv$current_item > 1) rv$current_item <- rv$current_item - 1L
  })

  observeEvent(input$btn_next, {
    t <- rv$current_subtest
    i_n <- rv$current_item
    max_i <- get_max_item(t, rv$age_group)

    # 仅导航：不自动保存（用户必须点"保存并下一题"）
    if (i_n < max_i) {
      rv$current_item <- i_n + 1L
    } else {
      # 已到最后一题，切换到下一测验
      rv$completed_subtests <- c(rv$completed_subtests, t) %>% unique()
      next_t <- setdiff(rv$test_list, rv$completed_subtests)[1]
      if (!is.na(next_t)) {
        updateSelectInput(session, "selected_subtest", selected = next_t)
      }
    }
  })

  # ── Discontinue 检查 ─────────────────────────────────────
  check_discontinue <- function(subtest) {
    t <- subtest
    disc_rule <- get_discontinue_rule(t)
    if (disc_rule == 0) return()
    sub_r <- rv$responses %>% filter(subtest==!!t) %>% arrange(item_number)
    if (nrow(sub_r) < 4) return()
    last_4 <- tail(sub_r, 4)
    if (all(last_4$score == 0)) {
      rv$discontinue_triggered <- TRUE
      rv$completed_subtests <- c(rv$completed_subtests, t) %>% unique()
      showNotification(glue("⚠ Discontinue 触发！{t} 在第{last_4$item_number[1]}题停止"),
                       type="warning", duration=10)
    }
  }

  # ── ORS: Observational Rating Scale ────────────────────────
  output$ors_ui <- renderUI({
    if (is.null(rv$assessment_id)) {
      return(div(class = "alert alert-info", style = "margin: 40px;",
                 h3("请先加载或创建评估 / Load or create an assessment first"),
                 p('在左侧表格选择一行，点击"📂 加载"按钮加载评估记录。')))
    }
    tagList(
      div(class = "page-header", style = "margin-bottom: 24px;",
        h2("行为观察评分量表 / Observational Rating Scale (ORS)", style = "color: #003A6C;"),
        p("由教师 / 家长 / 学生本人填写。评分：从不或几乎从不(1) → 有时(2) → 经常(3) → 总是或几乎总是(4)",
          class = "text-muted")
      ),
      fluidRow(
        column(4,
          wellPanel(
            h4("基本信息 / Basic Info"),
            p("评分: 从不或几乎从不(1) → 有时(2) → 经常(3) → 总是或几乎总是(4)",
              class = "small text-muted"),
            p("Section 1 Listening (1-9)   → 9题", class = "small text-muted"),
            p("Section 2 Speaking (10-28) → 19题", class = "small text-muted"),
            p("Section 3 Reading  (29-34) → 6题", class = "small text-muted"),
            p("Section 4 Writing  (35-40) → 6题", class = "small text-muted"),
            hr(),
            uiOutput("ors_summary_cards")
          )
        ),
        column(8,
          wellPanel(
            uiOutput("ors_section_tabs")
          )
        )
      )
    )
  })

  output$ors_section_tabs <- renderUI({
    req(rv$assessment_id)

    role <- "teacher"
    existing <- get_ors_responses(rv$assessment_id, role)
    existing_vec <- if (nrow(existing) == 0) {
      character(0)
    } else {
      setNames(existing$score, paste0(existing$section, "_", existing$item_number))
    }

    tabBox(width = 12,
      tabPanel("📖 Listening (1-9)",
        ors_section_table("listening", 1:9, existing_vec, role)
      ),
      tabPanel("🗣 Speaking (10-28)",
        ors_section_table("speaking", 10:28, existing_vec, role)
      ),
      tabPanel("📖 Reading (29-34)",
        ors_section_table("reading", 29:34, existing_vec, role)
      ),
      tabPanel("✏️ Writing (35-40)",
        ors_section_table("writing", 35:40, existing_vec, role)
      )
    )
  })

  ors_section_table <- function(section, items, existing_vec, role) {
    sec_info <- ORS_SECTIONS[[section]]
    item_zh  <- sec_info$behaviors_zh
    n <- length(items)

    tagList(
      p(strong(sec_info$name_zh, " / ", sec_info$name_en, " — ", n, "题"), class = "text-primary", style = "margin-bottom: 12px;"),
      fluidRow(
        column(1, ""),
        column(3, strong("从不", class = "text-center"), p("Never(1)", class = "small text-muted text-center")),
        column(3, strong("有时", class = "text-center"), p("Sometimes(2)", class = "small text-muted text-center")),
        column(3, strong("经常", class = "text-center"), p("Often(3)", class = "small text-muted text-center")),
        column(3, strong("总是", class = "text-center"), p("Always(4)", class = "small text-muted text-center"))
      ),
      lapply(seq_along(items), function(i) {
        item_num <- items[i]
        key <- paste0(section, "_", item_num)
        cur_val <- existing_vec[key]
        radioId <- paste0("ors_s", section, "_i", item_num)

        div(class = "ors-item-row",
          style = "display: flex; align-items: center; padding: 6px 0; border-bottom: 1px solid #eee;",
          column(1, strong(item_num), style = "text-align: center;"),
          column(3, p(item_zh[i], style = "margin: 0; font-size: 13px;")),
          column(3, div(style = "text-align: center;",
            radioButtons(radioId, label = NULL,
              choices = c("1" = "1", "2" = "2", "3" = "3", "4" = "4"),
              selected = cur_val %||% character(0),
              inline = TRUE,
              width = "120px")
          )),
          column(5, "")
        )
      })
    )
  }

  # ── ORS 保存：实时保存每题的评分 ─────────────────────
  ors_save_observer <- function(section, items) {
    lapply(items, function(item_num) {
      radioId <- paste0("ors_s", section, "_i", item_num)
      observeEvent(input[[radioId]], {
        req(rv$assessment_id)
        score <- as.integer(input[[radioId]])
        save_ors_response(rv$assessment_id, "teacher", section, item_num, score)
        # 更新 summary cards
        invalidateLater(500)
      }, ignoreInit = TRUE)
    })
  }
  ors_save_observer("listening", 1:9)
  ors_save_observer("speaking",  10:28)
  ors_save_observer("reading",   29:34)
  ors_save_observer("writing",   35:40)

  output$ors_summary_cards <- renderUI({
    req(rv$assessment_id)
    sumry <- tryCatch(get_ors_summary(rv$assessment_id, "teacher"), error = function(e) NULL)
    if (is.null(sumry)) {
      return(div(class = "alert alert-warning", style = "margin: 20px;", "无法加载 ORS 数据"))
    }

    make_card <- function(sec, label, icon) {
      val <- sumry[[paste0(sec, "_score")]]
      if (is.null(val) || length(val) == 0 || is.na(val)) {
        bg <- "bg-secondary"; val_disp <- "—"
      } else if (val >= 3.5) {
        bg <- "bg-danger text-white"; val_disp <- sprintf("%.2f / 4.0", val)
      } else if (val >= 3.0) {
        bg <- "bg-warning"; val_disp <- sprintf("%.2f / 4.0", val)
      } else if (val >= 2.5) {
        bg <- "bg-info"; val_disp <- sprintf("%.2f / 4.0", val)
      } else {
        bg <- "bg-success"; val_disp <- sprintf("%.2f / 4.0", val)
      }
      # 低分=问题，高分=正常（ORS 越高问题越多）
      tagList(
        div(class = paste0("card mb-2"),
          div(class = paste0("card-header ", bg), strong(icon, " ", label)),
          div(class = "card-body text-center",
            h4(val_disp),
            p(if (is.na(val) || is.null(val)) "未填写" else if (val >= 3.5) "⚠ 需关注" else if (val >= 3.0) "轻微关注" else "正常",
              class = "small")
          )
        )
      )
    }

    tagList(
      make_card("listening", "聆听", "👂"),
      make_card("speaking",  "表达", "🗣"),
      make_card("reading",   "阅读", "📖"),
      make_card("writing",   "书写", "✏️")
    )
  })

  # ── 评分报告 ─────────────────────────────────────────────
  output$report_ui <- renderUI({
    if (is.null(rv$assessment_id)) {
      return(div(class = "alert alert-info", style = "margin: 40px;",
                 h3("请先加载或创建评估 / Load or create an assessment first"),
                 p('在左侧表格选择一行，点击"📂 加载"按钮加载评估记录。')))
    }
    full <- tryCatch(get_assessment_full(rv$assessment_id), error = function(e) NULL)
    if (is.null(full)) {
      return(div(class = "alert alert-danger", style = "margin: 40px;",
                 "加载评估数据失败 / Failed to load assessment data"))
    }
    scaled_df <- full$subtest_scores

    if (nrow(scaled_df) == 0) {
      return(div(class = "alert alert-warning", role = "alert",
                 "尚无打分数据。请先完成至少一个子测试的打分并保存。"))
    }

    ag <- rv$age_group

    # ── 子测试分页签定义 ────────────────────────────────
    subtest_display <- function(st, ss_row) {
      # ss_row: nrow(scaled_df[st]) = 1
      raw    <- ss_row$raw_score
      scaled <- ss_row$scaled_score
      score_lbl <- if (is.na(scaled)) "—" else as.character(scaled)

      rng <- if (is.na(scaled)) {
        "No Score"
      } else if (scaled >= 13) {
        "bg-success text-white"
      } else if (scaled >= 8) {
        "bg-primary text-white"
      } else if (scaled >= 7) {
        "bg-warning"
      } else {
        "bg-danger text-white"
      }

      interpretation <- if (is.na(scaled)) {
        list(en = "No score available for this subtest.",
             zh = "该子测试暂无分数。")
      } else if (scaled >= 13) {
        list(en = "Above Average — Performance is significantly above the expected level for the student's age. This suggests strong competency in this skill area.",
             zh = "高于平均 — 表现显著高于同龄预期水平，提示该技能领域能力较强。")
      } else if (scaled >= 8) {
        list(en = "Average — Performance is within the expected range for the student's age. No significant difficulty identified in this skill area.",
             zh = "平均范围 — 表现符合同龄预期水平，该技能领域未发现显著困难。")
      } else if (scaled >= 7) {
        list(en = "Borderline — Performance is slightly below the expected range. There may be mild difficulty in this skill area; monitoring and follow-up are recommended.",
             zh = "边缘/临界 — 表现略低于预期范围，该技能领域可能存在轻度困难，建议持续监测或跟进。")
      } else if (scaled >= 5) {
        list(en = "Below Average — Performance is below the expected range for the student's age. This indicates a language disorder that warrants further assessment and intervention.",
             zh = "低于平均 — 表现低于同龄预期水平，提示存在语言障碍，需要进一步评估和干预。")
      } else {
        list(en = "Very Low — Performance is significantly below the expected level. This strongly suggests a language disorder requiring immediate intervention.",
             zh = "非常低 — 表现显著低于预期水平，强烈提示存在语言障碍，需要立即进行干预。")
      }

      # Subtest 中文名
      subtest_names <- c(
        SC  = "句子理解 / Sentence Comprehension",
        LC  = "语言概念 / Linguistic Concepts",
        WS  = "词汇结构 / Word Structure",
        WC  = "词汇语义 / Word Classes",
        FD  = "跟随指令 / Following Directions",
        FS  = "造句 / Formulated Sentences",
        RS  = "句子复述 / Recalling Sentences",
        USP = "段落理解 / Understanding Spoken Paragraphs",
        PP  = "语用观察 / Pragmatics Profile",
        WD  = "词汇定义 / Word Definitions",
        SA  = "句子重组 / Sentence Assembly",
        SR  = "语义关系 / Semantic Relationships",
        RC  = "阅读理解 / Reading Comprehension",
        SW  = "命题写作 / Structured Writing"
      )

      subtest_desc_en <- c(
        SC  = "Ability to understand spoken sentences of varying syntactic complexity, including passive voice, conditional statements, and embedded clauses.",
        LC  = "Ability to understand basic linguistic concepts (e.g., comparatives, spatial relations, temporal terms) presented orally.",
        WS  = "Knowledge of morphological word-structure rules (plurals, verb tense, comparatives, derivational morphemes).",
        WC  = "Ability to identify semantic relationships between words (synonymy, antonymy, hierarchical, and functional relationships).",
        FD  = "Ability to follow multi-step oral directions of varying syntactic complexity.",
        FS  = "Ability to formulate complete, semantically and syntactically correct sentences using target vocabulary.",
        RS  = "Ability to recall and accurately reproduce sentences of varying length and syntactic complexity.",
        USP = "Ability to understand main ideas and relevant details in spoken paragraphs; assessing listening comprehension.",
        PP  = "Pragmatic language skills as observed across social communication situations.",
        WD  = "Ability to define words by describing their semantic features and relevant attributes.",
        SA  = "Ability to construct grammatically well-formed sentences using specified word classes.",
        SR  = "Ability to interpret sentences involving semantic relationships such as comparison, location, time, serial order, and passive voice.",
        RC  = "Ability to understand written passages and answer comprehension questions.",
        SW  = "Ability to produce organized narrative writing following structural conventions."
      )

      div(class = "card mb-3",
        div(class = "card-header d-flex justify-content-between align-items-center",
          strong(subtest_names[[st]] %||% st),
          span(class = paste0("badge ", rng), score_lbl)
        ),
        div(class = "card-body",
          p(strong("原始分 Raw Score: "), as.character(raw %||% "—")),
          p(strong("量表分 Scaled Score (M=10, SD=3): "), score_lbl),
          hr(),
          p(strong("评估说明 Assessment: ")),
          p(interpretation$zh),
          p(strong("Interpretation: "), span(interpretation$en, class = "text-muted small")),
          # ── Item Analysis ───────────────────────────────
          {
            ia_result <- generate_item_analysis(st, full$responses[full$responses$subtest == st, ], ag)
            ia_def    <- ITEM_ANALYSIS[[st]]
            if (!is.null(ia_def) && !is.null(ia_result)) {
              ia   <- ia_result
              perf <- ia$performance
              perf_rows <- purrr::pmap_chr(perf, function(category_zh, category_en, n_items, n_scored, n_correct, accuracy_pct, error_items, flag) {
                pct_disp  <- if (is.na(accuracy_pct)) "—" else sprintf("%.0f%%", accuracy_pct)
                tr_clz    <- if (grepl("⚠️", flag)) "table-danger"
                             else if (grepl("🔶", flag)) "table-warning"
                             else if (grepl("✅", flag)) "table-success"
                             else ""
                bg_clr    <- if (is.na(accuracy_pct)) "bg-secondary text-white"
                             else if (accuracy_pct < 60) "bg-danger text-white"
                             else if (accuracy_pct < 80) "bg-warning"
                             else "bg-success text-white"
                err_disp  <- if (error_items == "") "无" else error_items
                sprintf('<tr class="%s"><td>%s<br><small class="text-muted">%s</small></td><td class="text-center">%d</td><td class="text-center">%d</td><td class="text-center"><span class="badge %s">%s</span></td><td class="text-center">%s</td><td class="text-center small">%s</td></tr>',
                  tr_clz, category_zh, category_en, n_items, n_scored, bg_clr, pct_disp, flag, err_disp)
              })
              header_row <- '<tr><th>技能类别</th><th>总题</th><th>已评</th><th>正确率</th><th>状态</th><th>错题</th></tr>'
              tbl_html   <- paste0('<table class="table table-sm table-bordered">', header_row, paste(perf_rows, collapse = ""), '</table>')
              tagList(
                hr(),
                h5(strong("📊 题目分析 Item Analysis — ", ia$domain_zh, " / ", ia$domain_en)),
                p(HTML("⚠️ <strong class='text-danger'>&lt;60% = 重点干预</strong> | 🔶 <strong class='text-warning'>60–80% = 提升空间</strong> | ✅ <strong class='text-success'>80%+ = 掌握良好</strong>")),
                HTML(tbl_html),
                p(strong("干预建议: "), ia$intervention_zh,
                  class = "small text-muted", style = "font-style:italic;")
              )
            } else if (!is.null(ia_def)) {
              tagList(
                hr(),
                h5(strong("Item Analysis — ", ia_def$domain_zh, " / ", ia_def$domain_en)),
                p(HTML("&lt;60% = red | 60-80% = yellow | 80%+ = green")),
                p(em("（完成打分后显示各技能类别的正确率）"),
                  class = "small text-muted text-center"),
                p(strong("干预建议: "), ia_def$intervention$zh,
                  class = "small text-muted", style = "font-style:italic;")
              )
            }
          }
        )
      )
    }

    # ── 复合分数 ─────────────────────────────────────────
    comp_list <- c("CLS","RLI","ELI","LCI")
    if (ag %in% c("5:0-5:5","5:6-5:11","6:0-6:5","6:6-6:11","7:0-7:11","8:0-8:11")) {
      comp_list <- c(comp_list, "LSI")
    } else {
      comp_list <- c(comp_list, "LMI")
    }

    comp_display <- lapply(comp_list, function(comp) {
      cs  <- get_composite_score(scaled_df, comp, ag)
      cis <- get_confidence_intervals(cs$standard_score[1], comp, ag)
      std <- cs$standard_score[1]
      pct <- cs$percentile[1]
      ss_sum <- cs$sum_scaled[1]

      rng_css <- if (is.na(std) || std < 70) "bg-danger text-white"
        else if (std < 80)  "bg-warning"
        else if (std < 90)  "bg-info text-white"
        else if (std < 110) "bg-primary text-white"
        else if (std < 120) "bg-info text-white"
        else if (std < 130) "bg-success text-white"
        else                "bg-success text-white"

      comp_names <- c(
        CLS = "核心语言分数 / Core Language Score",
        RLI = "接受性语言指数 / Receptive Language Index",
        ELI = "表达性语言指数 / Expressive Language Index",
        LCI = "语言内容指数 / Language Content Index",
        LSI = "语言结构指数 / Language Structure Index",
        LMI = "语言记忆指数 / Language Memory Index"
      )

      comp_desc_en <- c(
        CLS = "Core Language Score (CLS) is the most clinically sensitive composite score. It is calculated from the sum of the highest-performing subtests across core language domains and is the best single indicator of overall language ability.",
        RLI = "Receptive Language Index (RLI) reflects the student's ability to understand spoken and written language. Subtests include Sentence Comprehension, Linguistic Concepts, and Word Structure.",
        ELI = "Expressive Language Index (ELI) reflects the student's ability to use language expressively. Subtests include Word Classes, Formulated Sentences, and Naming.",
        LCI = "Language Content Index (LCI) measures vocabulary and semantic knowledge. For ages 5–8 it includes Word Classes, Understanding Spoken Paragraphs, and Word Definitions; for ages 9–21 it includes Word Classes, Understanding Spoken Paragraphs, and Sentence Assembly.",
        LSI = "Language Structure Index (LSI) measures morphological and syntactic skills. It includes Word Structure, Linguistic Concepts, and Recalling Sentences.",
        LMI = "Language Memory Index (LMI) measures verbal memory and sentence recall. It includes Recalling Sentences, Sentence Assembly, and Understanding Spoken Paragraphs."
      )

      pct_num  <- as.numeric(pct)
      pct_disp <- if (is.na(pct_num)) {
        if (grepl("<", pct, fixed = TRUE)) "<0.1" else if (grepl(">", pct, fixed = TRUE)) ">99.9" else pct
      } else {
        sprintf("%.1f", pct_num)
      }

      int_en <- if (is.na(std)) {
        "No score available."
      } else if (std >= 130) {
        paste0("Very Superior — A score of ", std, " (", pct_disp, "th percentile) indicates exceptional language ability well above age-level expectations.")
      } else if (std >= 120) {
        paste0("Superior — A score of ", std, " (", pct_disp, "th percentile) is well above average, suggesting strong language skills.")
      } else if (std >= 110) {
        paste0("High Average — A score of ", std, " (", pct_disp, "th percentile) is above average, within normal limits.")
      } else if (std >= 90) {
        paste0("Average — A score of ", std, " (", pct_disp, "th percentile) is within the average range, consistent with age-level expectations.")
      } else if (std >= 80) {
        paste0("Low Average — A score of ", std, " (", pct_disp, "th percentile) is slightly below average. The student may benefit from targeted language support.")
      } else if (std >= 70) {
        paste0("Borderline — A score of ", std, " (", pct_disp, "th percentile) is significantly below average and suggests a language disorder. Intervention is strongly recommended.")
      } else {
        paste0("Extremely Low — A score of ", std, " (", pct_disp, "th percentile) is far below average, indicating a significant language disorder requiring immediate intervention.")
      }

      int_zh <- if (is.na(std)) {
        "暂无分数。"
      } else if (std >= 130) {
        sprintf("非常优秀 — 分数 %d（第 %s 百分位）表明语言能力远超同龄预期。", std, pct_disp)
      } else if (std >= 120) {
        sprintf("优秀 — 分数 %d（第 %s 百分位）表明语言能力明显高于平均水平。", std, pct_disp)
      } else if (std >= 110) {
        sprintf("高于平均 — 分数 %d（第 %s 百分位）在正常范围内，显著高于平均。", std, pct_disp)
      } else if (std >= 90) {
        sprintf("平均范围 — 分数 %d（第 %s 百分位）符合同龄预期，在正常范围内。", std, pct_disp)
      } else if (std >= 80) {
        sprintf("低于平均 — 分数 %d（第 %s 百分位）略低于平均水平，建议提供针对性语言支持。", std, pct_disp)
      } else if (std >= 70) {
        sprintf("边缘/临界 — 分数 %d（第 %s 百分位）显著低于平均水平，提示存在语言障碍，强烈建议干预。", std, pct_disp)
      } else {
        sprintf("非常低 — 分数 %d（第 %s 百分位）远低于平均水平，提示显著语言障碍，需要立即干预。", std, pct_disp)
      }

      div(class = "card mb-3",
        div(class = "card-header d-flex justify-content-between align-items-center",
          strong(comp_names[[comp]] %||% comp),
          span(class = paste0("badge ", rng_css),
               if (is.na(std)) "—" else paste0(std, " (", pct_disp, "%)"))
        ),
        div(class = "card-body",
          if (!is.na(ss_sum)) p(strong("量表分合计 Scaled Sum: "), ss_sum),
          if (!is.na(std))    p(strong("标准分数 Standard Score (M=100, SD=15): "), std),
          if (nrow(cis) >= 1 && !is.na(cis$score_lo[1])) {
            p(strong("68% 置信区间: "), glue("[{cis$score_lo[1]}, {cis$score_hi[1]}]"))
          },
          p(strong("解读 Interpretation: ")),
          p(int_zh),
          p(strong("Clinical Note: "), p(int_en, class = "text-muted small mb-0"))
        )
      )
    })

    # ── 语言能力总评 ─────────────────────────────────────
    cls_score <- {
      cs <- get_composite_score(scaled_df, "CLS", ag)
      if (nrow(cs) == 0) NA else cs$standard_score[1]
    }
    overall_en <- if (is.null(cls_score) || length(cls_score) == 0 || is.na(cls_score)) {
      "Overall language ability could not be determined due to insufficient subtest data."
    } else if (cls_score >= 90) {
      "Overall, the student's language abilities are within the average range for their age. No significant language disorder was identified on the CELF-5."
    } else if (cls_score >= 80) {
      "Overall, the student's language abilities are in the low-average range. There may be mild difficulties that warrant monitoring and potentially targeted intervention."
    } else {
      "Overall, the student's performance on the CELF-5 suggests the presence of a language disorder. Results should be interpreted in the context of all available information. Comprehensive intervention is recommended."
    }

    overall_zh <- if (is.null(cls_score) || length(cls_score) == 0 || is.na(cls_score)) {
      "由于子测试数据不足，无法确定整体语言能力水平。"
    } else if (cls_score >= 90) {
      "整体而言，该学生的语言能力处于同龄正常（平均）范围内。CELF-5 未发现显著语言障碍。"
    } else if (cls_score >= 80) {
      "整体而言，该学生的语言能力处于低于平均范围。可能存在轻度困难，建议持续监测并在适当时提供针对性干预。"
    } else {
      "整体而言，该学生在 CELF-5 上的表现提示存在语言障碍。结果应结合所有可用信息进行解读。建议进行综合干预。"
    }

    # 存入 rv 供 downloadHandler 使用
    rv$overall_en <- overall_en
    rv$overall_zh <- overall_zh

    # ── 下载按钮行 ───────────────────────────────────────
    tagList(
      h2("CELF-5 评估报告 / Assessment Report"),
      hr(),
      h3("基本信息 / Student Information"),
      fluidRow(
        column(6, p(strong("姓名 Name: "), full$assessment$patient_name)),
        column(6, p(strong("性别 Sex: "), if (full$assessment$gender == "F") "女 / Female" else "男 / Male"))
      ),
      fluidRow(
        column(6, p(strong("年龄 Age: "), glue("{full$assessment$age_years}y {full$assessment$age_months}m ({ag})"))),
        column(6, p(strong("评估日期 Date: "), full$assessment$assessment_date))
      ),
      fluidRow(
        column(6, p(strong("评估师 Examiner: "), full$assessment$examiner %||% "—")),
        column(6, p(strong("评估编号 ID: "), rv$assessment_id))
      ),
      hr(),
      h3("整体评估结论 / Overall Assessment"),
      div(class = "alert alert-info", role = "alert",
        p(strong("总评 Summary: "), overall_zh),
        p(strong("Overall: "), p(overall_en, class = "text-muted small mb-0"))
      ),
      hr(),
      h3("各测试量表分 / Subtest Scaled Scores"),
      p("量表分以10为均值（Mean），标准差为3，范围1-19。8-12为平均范围，低于7提示存在困难。",
        class = "small text-muted"),
      lapply(seq_len(nrow(scaled_df)), function(i) {
        st <- scaled_df$subtest[i]
        subtest_display(st, scaled_df[i, ])
      }),
      hr(),
      h3("复合分数 / Composite Scores"),
      p("复合分数以100为均值，标准差为15。90-110为平均范围。",
        class = "small text-muted"),
      comp_display,
      hr(),
      h3("下载报告 / Download Report"),
      fluidRow(
        column(4, downloadButton("download_report_en",  "📄 Download (English)")),
        column(4, downloadButton("download_report_zh",  "📄 下载中文 (Chinese)")),
        column(4, downloadButton("download_report_pdf", "📋 Download PDF"))
      ),
      hr(),
      p("© 2013 NCS Pearson, Inc. All rights reserved. CELF-5 may not be reproduced without written permission from Pearson.",
        class = "small text-muted text-center")
    )
  })
  # ── 下载处理器：英文报告 ───────────────────────────────
  output$download_report_en <- downloadHandler(
    filename = function() glue("CELF5_Report_{rv$assessment_id}_{Sys.Date()}_EN.docx"),
    content = function(file) {
      full <- get_assessment_full(rv$assessment_id)
      scaled_df <- full$subtest_scores
      ag <- rv$age_group

      # Build indices list for params (tidyverse)
      comps <- c("CLS", "RLI", "ELI", "LCI", "LSI", "LMI")
      idx_list <- setNames(
        lapply(comps, function(comp) {
          cs <- get_composite_score(scaled_df, comp, ag)
          cs$standard_score[1]
        }),
        comps
      )
      idx_list <- c(idx_list, setNames(
        lapply(comps, function(comp) {
          cs <- get_composite_score(scaled_df, comp, ag)
          fmt_pct(cs$percentile[1])
        }),
        paste0(comps, "_pct")
      ))

      # Build raw_scores named list
      raw_list <- if (nrow(scaled_df) > 0) {
        setNames(as.list(scaled_df$raw_score), scaled_df$subtest)
      } else {
        list()
      }

      showNotification("Generating English report...", type = "message", duration = 3)

      tryCatch({
        rmarkdown::render(
          input = "report_celf5_en.Rmd",
          output_file = file,
          params = list(
            assessment_id   = rv$assessment_id,
            student_name     = full$assessment$patient_name,
            student_sex      = full$assessment$gender,
            age_years        = full$assessment$age_years,
            age_months       = full$assessment$age_months,
            age_days         = full$assessment$age_days,
            age_group        = ag,
            assessment_date  = as.character(full$assessment$assessment_date),
            examiner_name    = full$assessment$examiner %||% NA,
            scaled_scores    = scaled_df,
            indices          = idx_list,
            raw_scores       = raw_list,
            overall_en        = isolate(rv$overall_en %||% "No assessment data available."),
            overall_zh       = isolate(rv$overall_zh %||% "无评估数据。")
          ),
          envir = new.env(parent = globalenv()),
          quiet = TRUE
        )
        showNotification("English report downloaded successfully!", type = "message")
      }, error = function(e) {
        showNotification(paste0("Report error: ", e$message), type = "error")
        cat(file = stderr(), "download_report_en error:", e$message, "\n")
      })
    }
  )

  # ── 下载处理器：中文报告 ──────────────────────────────
  output$download_report_zh <- downloadHandler(
    filename = function() glue("CELF5_报告_{rv$assessment_id}_{Sys.Date()}_ZH.docx"),
    content = function(file) {
      full <- get_assessment_full(rv$assessment_id)
      scaled_df <- full$subtest_scores
      ag <- rv$age_group

      comps <- c("CLS", "RLI", "ELI", "LCI", "LSI", "LMI")
      idx_list <- setNames(
        lapply(comps, function(comp) {
          cs <- get_composite_score(scaled_df, comp, ag)
          cs$standard_score[1]
        }),
        comps
      )
      idx_list <- c(idx_list, setNames(
        lapply(comps, function(comp) {
          cs <- get_composite_score(scaled_df, comp, ag)
          fmt_pct(cs$percentile[1])
        }),
        paste0(comps, "_pct")
      ))

      raw_list <- if (nrow(scaled_df) > 0) {
        setNames(as.list(scaled_df$raw_score), scaled_df$subtest)
      } else {
        list()
      }

      showNotification("正在生成中文报告...", type = "message", duration = 3)

      tryCatch({
        rmarkdown::render(
          input = "report_celf5_zh.Rmd",
          output_file = file,
          params = list(
            assessment_id   = rv$assessment_id,
            student_name     = full$assessment$patient_name,
            student_sex      = full$assessment$gender,
            age_years        = full$assessment$age_years,
            age_months       = full$assessment$age_months,
            age_days         = full$assessment$age_days,
            age_group        = ag,
            assessment_date  = as.character(full$assessment$assessment_date),
            examiner_name    = full$assessment$examiner %||% NA,
            scaled_scores    = scaled_df,
            indices          = idx_list,
            raw_scores       = raw_list,
            overall_en       = isolate(rv$overall_en %||% "No assessment data available."),
            overall_zh       = isolate(rv$overall_zh %||% "无评估数据。")
          ),
          envir = new.env(parent = globalenv()),
          quiet = TRUE
        )
        showNotification("中文报告下载成功！", type = "message")
      }, error = function(e) {
        showNotification(paste0("报告错误: ", e$message), type = "error")
        cat(file = stderr(), "download_report_zh error:", e$message, "\n")
      })
    }
  )

  # ── 下载处理器：PDF 报告 ───────────────────────────────
  output$download_report_pdf <- downloadHandler(
    filename = function() glue("CELF5_Report_{rv$assessment_id}_{Sys.Date()}.pdf"),
    content = function(file) {
      full <- get_assessment_full(rv$assessment_id)
      scaled_df <- full$subtest_scores
      ag <- rv$age_group

      comps <- c("CLS", "RLI", "ELI", "LCI", "LSI", "LMI")
      idx_list <- setNames(
        lapply(comps, function(comp) {
          cs <- get_composite_score(scaled_df, comp, ag)
          cs$standard_score[1]
        }),
        comps
      )
      idx_list <- c(idx_list, setNames(
        lapply(comps, function(comp) {
          cs <- get_composite_score(scaled_df, comp, ag)
          fmt_pct(cs$percentile[1])
        }),
        paste0(comps, "_pct")
      ))

      raw_list <- if (nrow(scaled_df) > 0) {
        setNames(as.list(scaled_df$raw_score), scaled_df$subtest)
      } else {
        list()
      }

      showNotification("Generating PDF report...", type = "message", duration = 3)

      tryCatch({
        rmarkdown::render(
          input = "report_celf5_en.Rmd",
          output_format = "pdf_document",
          output_file = file,
          params = list(
            assessment_id   = rv$assessment_id,
            student_name     = full$assessment$patient_name,
            student_sex      = full$assessment$gender,
            age_years        = full$assessment$age_years,
            age_months       = full$assessment$age_months,
            age_days         = full$assessment$age_days,
            age_group        = ag,
            assessment_date  = as.character(full$assessment$assessment_date),
            examiner_name    = full$assessment$examiner %||% NA,
            scaled_scores    = scaled_df,
            indices          = idx_list,
            raw_scores       = raw_list,
            overall_en       = isolate(rv$overall_en %||% "No assessment data available."),
            overall_zh       = isolate(rv$overall_zh %||% "无评估数据。")
          ),
          envir = new.env(parent = globalenv()),
          quiet = TRUE
        )
        showNotification("PDF report downloaded successfully!", type = "message")
      }, error = function(e) {
        showNotification(paste0("Report error: ", e$message), type = "error")
        cat(file = stderr(), "download_report_pdf error:", e$message, "\n")
      })
    }
  )
}

shinyApp(ui, server)