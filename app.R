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
      window.addEventListener('load', function() {
        setTimeout(function() {
          var dob = document.querySelector('[data-datepicker-id=dob] input');
          var ad  = document.querySelector('[data-datepicker-id=assessment_date] input');
          if (dob) { dob.value = ''; dob.dispatchEvent(new Event('change', {bubbles: true})); }
          if (ad)  { ad.value  = ''; ad.dispatchEvent(new Event('change',  {bubbles: true})); }
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
              textInput("patient_gender", "性别 / Gender", placeholder = "男 M / 女 F"),
              textInput("school_name", "学校 / School", placeholder = "就读学校"),
              textInput("grade_level", "年级 / Grade", placeholder = "如：小一、初二、高一"),
              textInput("examiner", "评估师 / Examiner", placeholder = "评估师姓名"),
              dateInput("dob", "出生日期 * / Date of Birth *", format = "yyyy-mm-dd", value = character(0)),
              dateInput("assessment_date", "评估日期 * / Assessment Date *", format = "yyyy-mm-dd", value = character(0)),
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
              column(4, actionButton("btn_save_score", "💾 保存打分 / Save",
                          class = "btn-primary",
                          style = sprintf("width:100%%; background:%s;", celf5_blue))),
              column(4, actionButton("btn_next", "下一题 ▶ / Next", style = "width:100%"))
            ),
            hr(),
            fluidRow(column(12, h5("已打分 / Scored: "), textOutput("subtest_progress_text")))
          )
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

  # 加载按钮（独立 output，选中行后才显示）
  output$load_btn_ui <- renderUI({
    req(!is.null(input$assessments_table_rows_selected))
    tagList(
      hr(),
      actionButton("btn_load_assessment", "加载选中评估 / Load Selected",
                   class = "btn-primary",
                   style = sprintf("background:%s;", celf5_blue))
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

  # ── 测试进度 ────────────────────────────────────────────
  output$subtest_progress_ui <- renderUI({
    req(rv$test_list)
    map(rv$test_list, function(t) {
      is_done <- t %in% rv$completed_subtests
      max_i <- get_max_item(t)
      n_done <- sum(rv$responses$subtest == t, na.rm = TRUE)
      badge <- if (is_done) "✓ 完成" else glue("{n_done}/{max_i} 题")
      bg <- if (is_done) "#d4edda" else "#f8f9fa"
      col <- if (is_done) "#155724" else "#212529"
      box <- SUBTEST_DEFS %>% filter(subtest==t) %>% pull(full_name) %>% .[[1]]
      div(style = glue("background:{bg}; border-radius:8px; padding:12px; margin:6px; display:inline-block; width:200px;"),
          strong(style=glue("color:{col}"), t), br(), box, br(),
          span(style=glue("color:{col}"), badge))
    }) %>% tagList()
  })

  # ── Subtest 选择 ─────────────────────────────────────────
  output$subtest_selector <- renderUI({
    req(rv$test_list)
    opts <- setNames(rv$test_list, map(rv$test_list, ~{
      SUBTEST_DEFS %>% filter(subtest==.) %>% pull(full_name) %>% .[[1]]
    }))
    selectInput("selected_subtest", "选择测试 / Select Subtest", choices = opts, selectize=FALSE)
  })

  observeEvent(input$selected_subtest, {
    rv$current_subtest <- input$selected_subtest
    sub_resp <- rv$responses %>% filter(subtest == rv$current_subtest)
    max_done <- if (nrow(sub_resp) > 0) max(sub_resp$item_number) else 0L
    sp <- get_start_point(rv$current_subtest, rv$age_group)
    all_items <- seq_len(get_max_item(rv$current_subtest))
    done_items <- sub_resp$item_number
    next_item <- min(setdiff(all_items, done_items), na.rm=TRUE)
    rv$current_item <- next_item
    rv$start_point <- sp
    rv$discontinue_triggered <- FALSE
  })

  get_max_item <- function(subtest) {
    row <- SUBTEST_DEFS %>% filter(subtest==!!subtest)
    if (nrow(row) == 0) return(1L)
    row %>% pull(max_items) %>% .[[1]]
  }

  # ── 题目 UI ─────────────────────────────────────────────
  output$question_ui <- renderUI({
    req(rv$current_subtest, rv$current_item)

    t <- rv$current_subtest
    item_n <- rv$current_item
    sp <- rv$start_point
    max_item <- get_max_item(t)

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
        for (i_n in items_before) {
          if (!any(rv$responses$subtest==t & rv$responses$item_number==i_n)) {
            rv$responses <- rv$responses %>% add_row(
              subtest=t, item_number=i_n, response_text="Reversal满分",
              score=max_score_for_subtest(t))
          }
        }
        showNotification(glue("Reversal 触发！{sp}-{item_n-1}题记满分"), type="message")
      }
    }

    box_title <- SUBTEST_DEFS %>% filter(subtest==t) %>% pull(full_name) %>% .[[1]]

    tagList(
      h3(glue("{box_title} — 第 {item_n} / {max_item} 题 / Item {item_n} of {max_item}")),
      if (item_n == sp) div(class="alert alert-info", "★ 起始点题号 / Start Point Item"),
      hr(),
      uiOutput("score_input_ui")
    )
  })

  max_score_for_subtest <- function(t) {
    case_when(t %in% c("SC","LC","WS","WC","RC","SW") ~ 1L,
              t %in% c("FD","FS","RS","WD","SA","SR","USP") ~ 2L,
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
      err_val <- if (!is.na(cur_score)) max_s - cur_score else NA_integer_
      tagList(
        numericInput("input_score", "错误数量（0=3分, 1=2分, 2-3=1分, 4+=0分）",
                     value=err_val, min=0, max=99, step=1),
        textInput("response_text", "受试者回答", value=cur_resp$response_text%||%"")
      )
    } else if (max_s == 1L) {
      tagList(
        radioButtons("input_score", "得分",
                     choices=c("1分（正确）"=1L, "0分（错误）"=0L),
                     selected=cur_score),
        textInput("response_text", "受试者回答", value=cur_resp$response_text%||%"")
      )
    } else {
      opts <- setNames(as.character(2:0), c("2分", "1分", "0分"))
      tagList(
        radioButtons("input_score", "得分",
                     choices=list("2分"=2L, "1分"=1L, "0分"=0L),
                     selected=cur_score),
        textInput("response_text", "受试者回答", value=cur_resp$response_text%||%"")
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
    req(rv$current_subtest, rv$assessment_id)
    if (is.null(input$input_score) || is.na(input$input_score)) {
      showNotification("请先打分 / Please score first", type = "warning"); return()
    }
    t <- rv$current_subtest
    i_n <- rv$current_item
    sv <- input$input_score
    if (t=="RS" && !is.na(sv)) sv <- score_rs(as.integer(sv))
    rt <- input$response_text %||% ""

    rv$responses <- rv$responses %>% filter(!(subtest==!!t & item_number==!!i_n)) %>%
      add_row(subtest=t, item_number=i_n, response_text=as.character(rt),
              score=as.integer(sv))
    save_response(rv$assessment_id, t, i_n, as.character(rt), as.integer(sv))
    check_discontinue(t)
    showNotification(glue("已保存 / Saved: {t} 第{i_n}题 = {sv}分"), type="message")
  })

  observeEvent(input$btn_prev, {
    if (rv$current_item > 1) rv$current_item <- rv$current_item - 1L
  })

  observeEvent(input$btn_next, {
    req(rv$current_subtest, rv$assessment_id)
    t <- rv$current_subtest
    i_n <- rv$current_item
    max_i <- get_max_item(t)

    if (is.null(input$input_score) || is.na(input$input_score)) {
      showNotification("请先打分 / Please score first", type = "warning"); return()
    }

    sv <- input$input_score
    if (t=="RS" && !is.na(sv)) sv <- score_rs(as.integer(sv))
    rt <- input$response_text %||% ""

    rv$responses <- rv$responses %>% filter(!(subtest==!!t & item_number==!!i_n)) %>%
      add_row(subtest=t, item_number=i_n, response_text=as.character(rt),
              score=as.integer(sv))
    save_response(rv$assessment_id, t, i_n, as.character(rt), as.integer(sv))

    if (!rv$discontinue_triggered) check_discontinue(t)

    if (!rv$discontinue_triggered && i_n < max_i) {
      rv$current_item <- i_n + 1L
    } else {
      rv$completed_subtests <- c(rv$completed_subtests, t) %>% unique()
      next_t <- setdiff(rv$test_list, rv$completed_subtests)[1]
      if (!is.na(next_t)) updateSelectInput(session, "selected_subtest", selected=next_t)
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

  # ── 评分报告 ─────────────────────────────────────────────
  output$report_ui <- renderUI({
    req(rv$assessment_id)
    full <- get_assessment_full(rv$assessment_id)
    scaled_df <- full$subtest_scores

    if (nrow(scaled_df) == 0) return(p("尚无打分数据"))

    ag <- rv$age_group
    comp_list <- c("CLS","RLI","ELI","LCI")
    if (ag %in% c("5:0-5:5","5:6-5:11","6:0-6:5","6:6-6:11","7:0-7:11","8:0-8:11")) {
      comp_list <- c(comp_list, "LSI")
    } else {
      comp_list <- c(comp_list, "LMI")
    }

    comp_rows <- map(comp_list, function(comp) {
      cs <- get_composite_score(scaled_df, comp, ag)
      cis <- get_confidence_intervals(cs$standard_score[1], comp, ag)
      tibble(
        Composite=comp,
        `Scaled Sum`=cs$sum_scaled[1],
        `Standard Score`=cs$standard_score[1],
        Percentile=glue("{cs$percentile[1]}%"),
        `68% CI`=glue("[{cis$score_lo[1]}, {cis$score_hi[1]}]"),
        `90% CI`=glue("[{cis$score_lo[2]}, {cis$score_hi[2]}]"),
        `95% CI`=glue("[{cis$score_lo[3]}, {cis$score_hi[3]}]")
      )
    }) %>% bind_rows()

    tagList(
      h2("CELF-5 评估报告"),
      hr(),
      h3("基本信息"),
      p(strong("受试者: "), full$assessment$patient_name),
      p(strong("年龄: "), glue("{full$assessment$age_years}y {full$assessment$age_months}m {full$assessment$age_days}d ({ag})")),
      p(strong("评估日期: "), full$assessment$assessment_date),
      p(strong("评估师: "), full$assessment$examiner %||% "—"),
      hr(),
      h3("各测试量表分"),
      renderTable({ scaled_df %>% select(Test=subtest, Raw=raw_score, Scaled=scaled_score) }),
      hr(),
      h3("复合分数（Composite Scores）"),
      renderTable({ comp_rows }),
      hr(),
      downloadButton("download_report", "下载完整报告")
    )
  })

  output$download_report <- downloadHandler(
    filename = function() glue("CELF5_{rv$assessment_id}_{Sys.Date()}.pdf"),
    content = function(file) {
      showNotification("报告生成中...", type="message")
    }
  )
}

shinyApp(ui, server)