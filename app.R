# app.R — CELF-5 Assessment Shiny App
# tidyverse rebuild + SQLite 持久化
# 评估人员自带 Stimulus Books，本 app 仅录入分数、自动计算

library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)
library(glue)
library(rlang)
library(DT)
library(stringr)

source("global.R")
source("ocr_score_sw.R")

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
              selectInput("filter_status", "筛选状态 / Filter Status",
                choices = c("全部 / All" = "all",
                            "进行中 / In Progress" = "in_progress",
                            "已完成 / Complete" = "complete"),
                selected = "all", width = "40%"),
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
        column(8,
          h3(textOutput("current_patient")),
          h4(textOutput("current_age"))
        ),
        column(4,
          uiOutput("assessment_status_ui"),
          uiOutput("mark_complete_ui")
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

    # ── Tab 3b: Structured Writing（SW）──────────────────────
    # Standalone topic-based flow — NOT part of the normal item-by-item navigation
    tabPanel("📝 写作任务 / Writing",
      uiOutput("sw_standalone_ui")
    ),


    # ── Tab 4: AI 报告 ────────────────────────────────────
    tabPanel("AI 报告 / AI Report",
      fluidRow(
        column(12,
          h3("🤖 AI 临床叙事报告 / AI Clinical Narrative"),
          fluidRow(
            column(3,
              selectInput("report_lang", "语言 / Language",
                choices = c("中文" = "zh", "English" = "en"),
                selected = "zh", width = "100%")
            ),
            column(3,
              actionButton("btn_gen_narrative", "生成报告 / Generate",
                icon = icon("brain"), class = "btn btn-primary",
                style = "margin-top: 18px;", width = "100%")
            ),
            column(6,
              uiOutput("narrative_status", style = "padding-top: 22px;")
            )
          ),
          uiOutput("narrative_preview")
        )
      )
    ),

    # ── Tab 5: 评分报告 ─────────────────────────────
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
                       response_text=character(), score=integer(),
                       structure_complete=integer(), grammar=integer(),
                       organization=integer(), mechanics=integer()),
    completed_subtests = character(0),
    discontinue_triggered = FALSE,
    reversal_triggered = FALSE,
    reversal_item = 0L,
    wc_reversal_depth = 0L,  # WC 两级 Reversal 状态追踪
    sw_current_topic = NULL,  # 当前选中的 SW topic (item_number)
    sw_completed_topics = integer(0),  # 已完成的 topic 列表
    status_version = 0L
  )

  # ── 历史评估列表（bindEvent filter，下拉切换时自动刷新）──
  assessments_df <- reactive({
    df <- tryCatch({
      list_assessments() %>%
        mutate(age_str = glue("{age_years}y {age_months}m"),
               date = as.character(assessment_date),
               状态 = ifelse(status == "in_progress", "🔄 进行中 / In Progress",
                             "✅ 已完成 / Complete")) %>%
        select(姓名=patient_name, 评估日期=date, 年龄=age_str, 状态=状态)
    }, error = function(e) {
      data.frame(姓名=character(), 评估日期=character(), 年龄=character(), 状态=character())
    })
    fs <- input$filter_status
    if (!is.null(fs) && fs != "all") {
      df <- df[grepl(if (fs == "in_progress") "进行中" else "已完成", df$状态), ]
    }
    df
  }) %>% bindEvent(input$filter_status, input$btn_mark_complete)

  output$assessments_table <- renderDataTable({
    df <- assessments_df()
    if (nrow(df) == 0) return(df)
    # 去掉 emoji（DT 里 emoji 显示丑），改用 HTML 彩色标签
    df$状态 <- ifelse(grepl("进行中", df$状态),
                      '<span style="color:#e67e22;font-weight:600;">● 进行中</span>',
                      '<span style="color:#27ae60;font-weight:600;">● 已完成</span>')
    DT::datatable(df, selection = "single", escape = FALSE,
                  options = list(
                    pageLength = 10,
                    lengthMenu = c(10, 25, 50),
                    dom = 'frtip',
                    language = list(
                      emptyTable = "暂无历史评估记录",
                      search = "搜索：",
                      lengthMenu = "每页 _MENU_ 条",
                      info = "显示第 _START_ 至 _END_ 条，共 _TOTAL_ 条"
                    ),
                    columnDefs = list(
                      list(className = 'dt-center', targets = c(1, 2, 3))
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
    rv$reversal_triggered <- FALSE
    rv$reversal_item <- 0L
    rv$wc_reversal_depth <- 0L
    rv$sw_completed_topics <- integer(0)
    rv$responses <- tibble(subtest=character(), item_number=integer(),
                          response_text=character(), score=integer(),
                          structure_complete=integer(), grammar=integer(),
                          organization=integer(), mechanics=integer())
    # Auto-select first subtest to avoid NULL current_subtest on question tab
    first_test <- rv$test_list[1]
    rv$current_subtest <- first_test
    rv$start_point <- get_start_point(first_test, age_group)
    rv$current_item <- rv$start_point  # ✅ 从 start_point 开始，不是第1题
    updateSelectInput(session, "selected_subtest", selected = first_test)
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
    rv$reversal_triggered <- FALSE
    rv$reversal_item <- 0L
    rv$wc_reversal_depth <- 0L
    rv$sw_completed_topics <- integer(0)
    # Initialize current_subtest to avoid NULL on question tab
    first_test <- rv$test_list[1]
    rv$current_subtest <- first_test
    rv$start_point <- get_start_point(first_test, rv$age_group)
    rv$current_item <- rv$start_point  # ✅ 从 start_point 开始

    if (nrow(full$responses) > 0) {
      rv$responses <- full$responses %>%
        select(subtest, item_number, response_text, score,
               structure_complete, grammar, organization, mechanics) %>%
        mutate(across(everything(), ~replace_na(as.character(.), ""))) %>%
        mutate(item_number = as.integer(item_number),
               score = as.integer(score),
               structure_complete = as.integer(structure_complete),
               grammar = as.integer(grammar),
               organization = as.integer(organization),
               mechanics = as.integer(mechanics))
    }

    updateSelectInput(session, "selected_subtest", selected = first_test)
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

  # ── 当前状态徽章 + 更改状态按钮 ───────────────────────
  output$assessment_status_ui <- renderUI({
    req(rv$assessment_id)
    # Depend on status_version so this re-renders when status changes
    invisible(rv$status_version)
    status <- tryCatch({
      con <- get_con(); on.exit(dbDisconnect(con))
      dbGetQuery(con, "SELECT status FROM assessments WHERE id=?",
                 params = rv$assessment_id)$status[1]
    }, error = function(e) "in_progress")
    badge <- if (status == "complete") {
      span(class = "badge bg-success", style = "font-size:14px; padding:6px 14px;",
           "✅ 已完成 / Complete")
    } else {
      span(class = "badge bg-warning text-dark", style = "font-size:14px; padding:6px 14px;",
           "🔄 进行中 / In Progress")
    }
    tagList(
      badge,
      tags$button(
        type = "button", class = "btn btn-sm btn-outline-secondary",
        style = "margin-left:8px;",
        onclick = "Shiny.setInputValue('toggle_status_edit', Math.random());",
        "更改状态 \u00bb"
      )
    )
  })

  # 更改状态面板（点击按钮才出现）
  output$mark_complete_ui <- renderUI({
    req(input$toggle_status_edit)
    req(rv$assessment_id)
    invisible(rv$status_version)  # re-render when status changes
    status <- tryCatch({
      con <- get_con(); on.exit(dbDisconnect(con))
      dbGetQuery(con, "SELECT status FROM assessments WHERE id=?",
                 params = rv$assessment_id)$status[1]
    }, error = function(e) "in_progress")
    # Always show BOTH options so user can switch either way
    tagList(
      selectInput("new_status", NULL,
        choices = c(
          "进行中 / In Progress" = "in_progress",
          "已完成 / Complete"     = "complete"
        ),
        selected = status,
        width = "200px"),
      actionButton("btn_save_status", "\u2702 保存",
        class = "btn btn-primary btn-sm", style = "margin-top:4px;")
    )
  })

  observeEvent(input$btn_save_status, {
    req(rv$assessment_id, input$new_status)
    update_assessment_status(rv$assessment_id, input$new_status)
    showNotification("状态已更新 / Status updated", type = "message")
    # Force the status UI reactive to re-run by toggling a dummy reactive value
    rv$status_version <- rv$status_version + 1L
  })

  # ── 测试进度（可点击卡片） ──────────────────────────────
  output$subtest_progress_ui <- renderUI({
    req(rv$test_list)
    map(rv$test_list, function(t) {
      is_done <- t %in% rv$completed_subtests
      n_done <- sum(rv$responses$subtest == t, na.rm = TRUE)
      badge <- if (is_done) "✓ 完成" else glue("{n_done} 题已评")
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
    if (t == "SW") {
      updateTabsetPanel(session, "main_tabs", selected = "📝 写作任务 / Writing")
    } else {
      updateTabsetPanel(session, "main_tabs", selected = "测试题目 / Test Items")
      rv$current_subtest <- t
      updateSelectInput(session, "selected_subtest", selected = t)
    }
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
    sp <- get_start_point(rv$current_subtest, rv$age_group)
    sub_resp <- rv$responses %>% filter(subtest == rv$current_subtest)
    # 找下一个未打分的题（允许从 Item 1 导航到全部题目，没有 end point）
    # 注意：start_point 之前的题（1~sp-1）在 reversal 触发时已被 backfill 为满分，
    #       但施测流程中仍可通过 btn_prev 回退查看（打过分但不参与正常施测流程）
    real_max <- SUBTEST_DEFS %>% filter(subtest==rv$current_subtest) %>% pull(max_items) %>% .[[1]]
    scored_items <- sub_resp$item_number
    candidates <- setdiff(seq(1, real_max), scored_items)
    next_item <- if (length(candidates) > 0) min(candidates) else (real_max + 1L)
    # 避免 btn_start/init 时覆盖 current_item（btn_start 在此之前已正确设置）
    if (length(scored_items) > 0 || next_item >= sp) {
      rv$current_item <- next_item
    }
    # 仅在真正切换 subtest 时才重置状态标志（不在 btn_start 初始化阶段）
    if (!isTRUE(get0("._initializing_."))) {
      rv$start_point <- sp
      rv$discontinue_triggered <- FALSE
      rv$reversal_triggered <- FALSE
      rv$reversal_item <- 0L
      rv$wc_reversal_depth <- 0L
    }
  })

  # ── 题目 UI ─────────────────────────────────────────────
  output$question_ui <- renderUI({
    req(rv$current_subtest, rv$current_item)

    t <- rv$current_subtest
    item_n <- rv$current_item
    sp <- rv$start_point
    # 使用 SUBTEST_DEFS 的固定最大题数（没有 end point，按 start_point 施测，做完或触发 discontinue 为止）
    real_max <- SUBTEST_DEFS %>% filter(subtest==t) %>% pull(max_items) %>% .[[1]]

    if (rv$discontinue_triggered) {
      return(div(class="alert alert-warning", style="margin-top:20px",
                 h3("⏹ Discontinue: 连续4题0分，该测试结束 / 4 consecutive 0s — subtest ended")))
    }

    box_title <- SUBTEST_DEFS %>% filter(subtest==t) %>% pull(full_name) %>% .[[1]]
    qi <- get_question_info(t, item_n, rv$age_group)

    # 题目文字优先用 question_en，其次 prompt_en
    stimulus_txt <- if (!is.na(qi$question_en) && nzchar(qi$question_en)) qi$question_en[1] else ""
    prompt_txt   <- if (!is.na(qi$prompt_en)   && nzchar(qi$prompt_en))   qi$prompt_en[1]   else ""
    scoring_txt  <- if (!is.na(qi$scoring_key) && nzchar(qi$scoring_key)) qi$scoring_key[1] else ""

    tagList(
      h3(
        glue("{box_title} — 第 {item_n} 题 / Item {item_n} (共 {real_max} 题 total)"),
        if (item_n == sp) span(class = "badge bg-danger", style = "margin-left:10px;vertical-align:middle;font-size:16px;padding:4px 10px;", "★ 起始题 Start")
      ),

      # ── 题目/刺激物显示 ─────────────────────────────────────
      if (nzchar(stimulus_txt)) {
        div(class="card mb-3", style="background:#f8f9fa",
          div(class="card-body",
            p(strong("题目 Stimulus: "), HTML(stimulus_txt)),
            if (nzchar(prompt_txt))
              p(strong("施测说明 Prompt: "), HTML(prompt_txt)),
            if (nzchar(scoring_txt))
              p(strong("评分标准 Scoring: "), HTML(scoring_txt))
          )
        )
      } else {
        # DEBUG: show raw qi columns
        div(class="alert alert-danger", style="font-size:11px;word-break:break-all",
          "题目加载中 / Question not yet available",
          br(), "DEBUG: stimulus_txt empty — qi$question_en:",
          I(paste0(capture.output(print(qi)), collapse="\n"))
        )
      },

      hr(),

      uiOutput("score_input_ui")
    )
  })

  # ── 评分输入 UI ─────────────────────────────────────────
  output$score_input_ui <- renderUI({
    req(rv$current_subtest, rv$current_item, rv$assessment_id)
    t <- rv$current_subtest

    # SW uses standalone tab — redirect if user navigates to SW via Test Items
    if (t == "SW") {
      return(tagList(
        div(class="alert alert-info", style="margin-top:20px",
          h4("📝 Structured Writing 使用独立标签页"),
          p("请点击上方「📝 写作任务 / Writing」标签页进入写作评分。"),
          p("Structured Writing uses its own standalone tab above.")
        ),
        updateTabsetPanel(session, "main_tabs", selected = "📝 写作任务 / Writing")
      ))
    }
    i_n <- rv$current_item
    cur_resp <- rv$responses %>% filter(subtest==!!t, item_number==!!i_n)
    cur_score <- if (nrow(cur_resp)>0) cur_resp$score[1] else NA_integer_

    # Get actual max_score from DB for this specific item
    q_info <- get_question_info(t, i_n, rv$age_group)
    max_s <- as.integer(q_info$max_score[1])
    if (is.na(max_s) || max_s < 1) max_s <- 1L

    if (t == "RS") {
      # UI shows error count; stored score is scaled (3=0err, 2=1err, 1=2-3err, 0=4+err)
      err_val <- if (!is.na(cur_score) && !is.null(cur_score)) max_s - as.integer(cur_score) else NA_integer_
      tagList(
        numericInput("input_score", "错误数量（0=3分, 1=2分, 2-3=1分, 4+=0分）",
                     value=err_val, min=0, max=99, step=1)
      )
    } else if (t == "USP") {
      # ── USP: show paragraph + questions + per-question scoring ──
      qi <- get_question_info("USP", i_n, rv$age_group)
      para_text <- if (is.character(qi$paragraph_en[1]) && nzchar(qi$paragraph_en[1])) qi$paragraph_en[1] else ""
      questions_json_str <- if (is.character(qi$questions_json[1]) && nzchar(qi$questions_json[1])) qi$questions_json[1] else "[]"
      questions_list <- tryCatch(jsonlite::fromJSON(questions_json_str, simplifyVector = FALSE)[[1]],
                                  error = function(e) list())
      cur_resp_df <- rv$responses %>% filter(subtest=="USP", item_number==!!i_n)
      saved_scores <- if (nrow(cur_resp_df) > 0) {
        cur_resp_df$score
      } else integer(0)

      tagList(
        if (nzchar(para_text)) {
          div(class="card mb-3", style="background:#e8f4f8",
            div(class="card-body",
              h5("📖 段落文本 / Paragraph"),
              p(strong(para_text))))
        } else div(),
        if (length(questions_list) > 0) {
          lapply(seq_along(questions_list), function(qi_idx) {
            q_item <- questions_list[[qi_idx]]
            # Guard: skip atomic vectors or items without $q/$a fields
            if (!is.list(q_item) || is.null(q_item$q)) return(NULL)
            q_text <- q_item$q
            q_ans  <- q_item$a
            q_score_val <- if (qi_idx <= length(saved_scores)) saved_scores[qi_idx] else NA_integer_
            q_name <- paste0("usp_q", i_n, "_", qi_idx)
            fluidRow(column(12,
              wellPanel(
                h6(paste0("Q", qi_idx, ". ", q_text)),
                p(em(strong("参考答案: "), code(q_ans)), style="color:#555"),
                radioButtons(q_name, "得分",
                  choices = c("1分"=1L, "0分"=0L),
                  selected = q_score_val,
                  inline = TRUE)
              )
            ))
          })
        } else div(class="alert alert-secondary", "题目加载中...")
      )
    } else if (max_s == 1L) {
      tagList(
        radioButtons("input_score", "得分",
                     choices=c("1分（正确）"=1L, "0分（错误）"=0L),
                     selected=cur_score)
      )
    } else if (max_s == 2L) {
      tagList(
        radioButtons("input_score", "得分",
                     choices=list("2分（完全正确）"=2L, "1分（部分正确）"=1L, "0分（错误）"=0L),
                     selected=cur_score)
      )
    } else {
      # max_s >= 3: 从 max_s 到 1（不是到 0！）
      choice_vec <- setNames(as.integer(max_s:1), sapply(as.integer(max_s:1), function(s) {
        if (s == max_s) paste0(s, "分（最高）")
        else if (s == 1L) paste0(s, "分（最低）")
        else paste0(s, "分")
      }))
      tagList(
        radioButtons("input_score", "得分",
                     choices = choice_vec,
                     selected = cur_score)
      )
    }
  })

  output$subtest_progress_text <- renderText({
    req(rv$current_subtest)
    sub_r <- rv$responses %>% filter(subtest==!!rv$current_subtest) %>% arrange(item_number)
    if (nrow(sub_r)==0) return("暂无 / None")
    paste(tail(sub_r$score, 20), collapse=" ")
  })

  # ═══════════════════════════════════════════════════════════════
  # SW STANDALONE TAB — Topic-based flow (NOT item-number based)
  # ═══════════════════════════════════════════════════════════════

  # Reactive: current rubric key for SW
  sw_rubric_key <- reactive({
    ag_raw <- rv$age_group
    dplyr::case_when(
      ag_raw %in% c("5:0-5:5","5:6-5:11","6:0-6:5","6:6-6:11","7:0-7:11","8:0-8:11") ~ "age_8",
      ag_raw %in% c("9:0-9:11","10:0-10:11")                                                   ~ "age_9_10",
      ag_raw %in% c("11:0-11:11","12:0-12:11")                                                 ~ "age_11_12",
      TRUE                                                                                      ~ "age_13_21"
    )
  })

  # Reactive: available SW topics for current age group
  sw_topics_r <- reactive({
    req(rv$age_group)
    get_sw_topics(rv$age_group)
  })

  # Reactive: currently selected topic item_number (NULL if none selected)
  sw_selected_item_r <- reactive({
    if (is.null(input$sw_topic_select)) return(NULL)
    as.integer(input$sw_topic_select)
  })

  # Reactive: rubric for current age group
  sw_rubric_r <- reactive({
    req(sw_rubric_key())
    rub <- SW_SCORING_RUBRIC[[sw_rubric_key()]]
    if (is.null(rub)) {
      showNotification(glue("未知 age_group rubric: {sw_rubric_key()}"), type="error")
    }
    rub
  })

  # Main SW standalone UI
  output$sw_standalone_ui <- renderUI({
    req(rv$assessment_id)
    topics <- sw_topics_r()
    if (nrow(topics) == 0) {
      return(div(class="alert alert-warning", "当前年龄组无可用写作任务 / No writing tasks for this age group"))
    }

    # Determine selected item_number
    sel_item <- sw_selected_item_r()
    if (is.null(sel_item) && !is.null(rv$sw_current_topic)) {
      sel_item <- rv$sw_current_topic
    }
    # Default to first uncompleted topic, or first topic
    if (is.null(sel_item)) {
      remaining <- setdiff(topics$item_number, rv$sw_completed_topics)
      sel_item <- if (length(remaining) > 0) remaining[1] else topics$item_number[1]
    }

    # Build topic dropdown choices
    topic_choices <- setNames(as.character(topics$item_number), topics$topic_label)
    # Mark completed topics
    topic_choices_display <- sapply(names(topic_choices), function(n) {
      inum <- as.integer(topic_choices[n])
      if (inum %in% rv$sw_completed_topics) {
        paste0(n, " ✓")
      } else {
        n
      }
    })
    names(topic_choices) <- topic_choices_display

    # Show previously saved scores for each topic (summary badges)
    saved_summary <- lapply(topics$item_number, function(inum) {
      r_row <- rv$responses %>% filter(subtest=="SW", item_number==!!inum)
      if (nrow(r_row) > 0) {
        row <- r_row[1,]
        score_str <- if (!is.na(row$score)) paste0(row$score, "分") else "未打分"
        fluidRow(column(12,
          wellPanel(
            h5(topics$topic_label[topics$item_number == inum], style="margin:0"),
            tags$span(style="float:right", strong(score_str)),
            if (!is.na(row$structure_complete)) {
              p(em(paste0("结构:", row$structure_complete,
                          " 语法:", row$grammar,
                          " 组织:", row$organization,
                          " 机械:", row$mechanics)),
                style="margin:4px 0 0 0;color:#555;font-size:12px")
            }
          )
        ))
      } else NULL
    }) %>% compact()

    tagList(
      fluidRow(
        column(12,
          h3("📝 Structured Writing / 结构化写作"),
          p(glue("年龄组 Age Group: {rv$age_group}  |  评分量表: {sw_rubric_key()}"))
        )
      ),
      fluidRow(
        column(12,
          if (length(saved_summary) > 0) {
            div(class="card mb-3", style="background:#f0f7ff",
              div(class="card-body",
                h4("已完成Topics / Completed Topics"),
                saved_summary
              )
            )
          }
        )
      ),
      fluidRow(
        column(4,
          wellPanel(
            h4("选择写作任务 / Select Topic"),
            selectInput("sw_topic_select", "任务 / Task",
              choices = topic_choices,
              selected = as.character(sel_item),
              selectize = FALSE),
            br(),
            if (length(rv$sw_completed_topics) > 0) {
              p(em(paste0("已完成 ", length(rv$sw_completed_topics), "/", nrow(topics), " 个任务")))
            }
          )
        ),
        column(8,
          if (!is.null(sel_item) && sel_item %in% topics$item_number) {
            sw_topic_scoring_ui(sel_item, sw_rubric_r())
          } else {
            div(class="alert alert-info", "请从左侧选择写作任务 / Please select a writing task from the left")
          }
        )
      )
    )
  })

  # Per-topic scoring UI builder
  sw_topic_scoring_ui <- function(item_number, rubric) {
    req(item_number, !is.null(rubric))
    topics <- sw_topics_r()
    topic_row <- topics[topics$item_number == as.integer(item_number), ]
    if (nrow(topic_row) == 0) return(div("Topic not found"))

    prompt_text <- topic_row$question_en[1]
    is_trial <- (as.integer(item_number) == 1L)

    # Read existing scores for this topic
    cur_resp <- rv$responses %>% filter(subtest=="SW", item_number==!!as.integer(item_number))
    cur_sc <- if (nrow(cur_resp)>0) cur_resp$structure_complete[1] else NA_integer_
    cur_gr <- if (nrow(cur_resp)>0) cur_resp$grammar[1] else NA_integer_
    cur_or <- if (nrow(cur_resp)>0) cur_resp$organization[1] else NA_integer_
    cur_me <- if (nrow(cur_resp)>0) cur_resp$mechanics[1] else NA_integer_

    tagList(
      wellPanel(
        h4(topic_row$topic_label[1]),
        if (is_trial) {
          div(class="alert alert-secondary", "Trial Task（不计分 / Not Scored）")
        },
        div(class="card mb-3", style="background:#f8f9fa",
          div(class="card-body",
            h5("📋 写作提示 / Writing Prompt"),
            p(strong(prompt_text))
          )
        ),

        if (!is_trial) {
          tagList(
            h5("1. 结构完整性 Structure（每句完整=1，不完整=0）"),
            radioButtons("sw_struct", "句子是否完整写出？",
              choices = rubric$struct_scale,
              selected = cur_sc),
            hr(),
            h5("2. 语法准确性 Grammar"),
            p(em("对应当前句子语法正确性")),
            radioButtons("sw_grammar", "语法评分",
              choices = rubric$grammar_scale,
              selected = cur_gr),
            hr(),
            h5("3. 组织 Organization（整体逻辑与衔接）"),
            radioButtons("sw_org", "组织评分",
              choices = rubric$org_scale,
              selected = cur_or),
            hr(),
            h5("4. 写作机械 Mechanics（拼写/大小写/标点）"),
            p(em("按对应年龄组标准")),
            radioButtons("sw_mech", "机械评分",
              choices = rubric$mech_scale,
              selected = cur_me),
            hr()
          )
        },

        # ── AI 辅助评分区 ────────────────────────────────
        h5("🤖 AI 辅助评分 AI-Assisted Scoring"),
        p(em("上传小朋友写作照片，AI 自动识别内容并给出评分建议")),
        fileInput("sw_image_upload", "上传写作照片",
          accept = c("image/jpeg","image/png","image/jpg","image/webp"),
          buttonLabel = "选择图片", placeholder = "未选择文件"),
        fluidRow(
          column(6, actionButton("btn_run_ai_score", "🔍 AI 分析",
            icon = icon("brain"), class = "btn-primary")),
          column(6, actionButton("btn_clear_ai", "🗑️ 清除",
            icon = icon("trash"), class = "btn-outline-secondary"))
        ),
        uiOutput("sw_ai_result"),
        hr(),
        if (!is_trial) {
          fluidRow(
            column(6, actionButton("btn_sw_save", "💾 保存当前任务 / Save Topic",
              class = "btn-primary",
              style = sprintf("width:100%%; background:%s;", celf5_blue))),
            column(6, actionButton("btn_sw_done", "✅ 标记完成并选择下一任务 / Done",
              class = "btn-success", style = "width:100%;"))
          )
        } else {
          fluidRow(
            column(6, actionButton("btn_sw_trial_done", "✅ Trial完成 / Trial Done",
              class = "btn-success", style = sprintf("width:100%%; background:%s;", celf5_blue)))
          )
        }
      )
    )
  }

  # ── 保存当前 SW topic ─────────────────────────────────────
  observeEvent(input$btn_sw_save, {
    item_n <- sw_selected_item_r()
    req(item_n)
    rubric <- sw_rubric_r()
    if (is.null(rubric)) { showNotification("Rubric 未找到", type="error"); return() }

    sw_struct  <- input$sw_struct
    sw_grammar <- input$sw_grammar
    sw_org     <- input$sw_org
    sw_mech    <- input$sw_mech

    if (is.null(sw_struct) || is.null(sw_grammar) || is.null(sw_org) || is.null(sw_mech)) {
      showNotification("请完成所有维度评分 / Please complete all dimension scores", type = "warning"); return()
    }

    total_score <- as.integer(sw_struct) + as.integer(sw_grammar) + as.integer(sw_org) + as.integer(sw_mech)

    rv$responses <- rv$responses %>% filter(!(subtest=="SW" & item_number==!!item_n)) %>%
      add_row(subtest="SW", item_number=item_n, response_text="",
              score=total_score,
              structure_complete=as.integer(sw_struct),
              grammar=as.integer(sw_grammar),
              organization=as.integer(sw_org),
              mechanics=as.integer(sw_mech))
    save_response(rv$assessment_id, "SW", item_n, "", total_score,
                  structure_complete=as.integer(sw_struct),
                  grammar=as.integer(sw_grammar),
                  organization=as.integer(sw_org),
                  mechanics=as.integer(sw_mech))
    showNotification(glue("已保存 / Saved: SW Topic {item_n} = {total_score}分"), type="message")
  })

  # ── 标记 SW topic 完成并选下一任务 ───────────────────────
  observeEvent(input$btn_sw_done, {
    item_n <- sw_selected_item_r()
    req(item_n)
    # Save first (same logic as btn_sw_save)
    rubric <- sw_rubric_r()
    if (!is.null(rubric)) {
      sw_struct  <- input$sw_struct
      sw_grammar <- input$sw_grammar
      sw_org     <- input$sw_org
      sw_mech    <- input$sw_mech
      if (!is.null(sw_struct) && !is.null(sw_grammar) && !is.null(sw_org) && !is.null(sw_mech)) {
        total_score <- as.integer(sw_struct) + as.integer(sw_grammar) + as.integer(sw_org) + as.integer(sw_mech)
        rv$responses <- rv$responses %>% filter(!(subtest=="SW" & item_number==!!item_n)) %>%
          add_row(subtest="SW", item_number=item_n, response_text="",
                  score=total_score,
                  structure_complete=as.integer(sw_struct),
                  grammar=as.integer(sw_grammar),
                  organization=as.integer(sw_org),
                  mechanics=as.integer(sw_mech))
        save_response(rv$assessment_id, "SW", item_n, "", total_score,
                      structure_complete=as.integer(sw_struct),
                      grammar=as.integer(sw_grammar),
                      organization=as.integer(sw_org),
                      mechanics=as.integer(sw_mech))
      }
    }
    # Mark as completed
    rv$sw_completed_topics <- c(rv$sw_completed_topics, item_n) %>% unique()
    topics <- sw_topics_r()
    remaining <- setdiff(topics$item_number, rv$sw_completed_topics)
    if (length(remaining) > 0) {
      next_topic <- remaining[1]
      rv$sw_current_topic <- next_topic
      updateSelectInput(session, "sw_topic_select", selected = as.character(next_topic))
      showNotification(glue("已切换到下一任务 / Next topic: {topics$topic_label[topics$item_number==next_topic]}"), type="message")
    } else {
      showNotification("🎉 所有写作任务已完成！/ All writing tasks completed!", type="message", duration=5)
    }
  })

  # ── Trial task done ─────────────────────────────────────────
  observeEvent(input$btn_sw_trial_done, {
    item_n <- sw_selected_item_r()
    req(item_n)
    # Mark trial (item 1) as completed in responses with 0 score (not counted)
    rv$responses <- rv$responses %>% filter(!(subtest=="SW" & item_number==!!item_n)) %>%
      add_row(subtest="SW", item_number=item_n, response_text="Trial",
              score=0L, structure_complete=0L, grammar=0L, organization=0L, mechanics=0L)
    save_response(rv$assessment_id, "SW", item_n, "Trial", 0L,
                  structure_complete=0L, grammar=0L, organization=0L, mechanics=0L)
    rv$sw_completed_topics <- c(rv$sw_completed_topics, item_n) %>% unique()
    topics <- sw_topics_r()
    remaining <- setdiff(topics$item_number, rv$sw_completed_topics)
    if (length(remaining) > 0) {
      next_topic <- remaining[1]
      rv$sw_current_topic <- next_topic
      updateSelectInput(session, "sw_topic_select", selected = as.character(next_topic))
    }
    showNotification("Trial 完成，请继续正式任务 / Trial done, proceed to scored tasks", type="message")
  })

  # ── AI scoring (same logic as before, scoped to current topic) ──
  ai_result_rv <- reactiveValues(status = "idle", data = NULL)

  output$sw_ai_result <- renderUI({
    req(input$sw_image_upload)
    NULL
  })

  observeEvent(input$btn_run_ai_score, {
    req(input$sw_image_upload)
    img_path <- input$sw_image_upload$datapath
    if (!file.exists(img_path)) {
      showNotification("图片文件未找到", type = "error"); return()
    }

    ai_result_rv$status <- "running"
    ai_result_rv$data   <- NULL

    output$sw_ai_result <- renderUI({
      div(class = "alert alert-info",
          h5("🤖 AI 识别中..."),
          p(em("正在 OCR 识别 + AI 评分，请稍候（约 10-20 秒）"))
      )
    })

    tryCatch({
      result <- ocr_and_score(img_path, sw_rubric_key())
      ai_result_rv$data   <- result
      ai_result_rv$status <- if (is.null(result$error)) "done" else "error"
    }, error = function(e) {
      ai_result_rv$status <<- "error"
      ai_result_rv$data   <<- list(error = conditionMessage(e))
    })

    if (ai_result_rv$status == "done") {
      r <- ai_result_rv$data
      output$sw_ai_result <- renderUI({
        tagList(
          div(class = "alert alert-success",
              h5("✅ AI 评分建议"), br(),
              fluidRow(
                column(3, strong("结构 Structure"),  p(r$structure$score, "/ 结构满分")),
                column(3, strong("语法 Grammar"),     p(r$grammar$score,   "/ 语法满分")),
                column(3, strong("组织 Organization"),p(r$organization$score,"/ 组织满分")),
                column(3, strong("机械 Mechanics"),   p(r$mechanics$score,  "/ 机械满分"))
              ),
              fluidRow(
                column(12, strong("临床评语 Clinical Comment:")),
                column(12, p(r$summary))
              ),
              if (!is.null(r$structure$comment) && r$structure$comment != "") {
                fluidRow(column(12, p(em("结构: ", r$structure$comment))))
              },
              if (!is.null(r$grammar$comment) && r$grammar$comment != "") {
                fluidRow(column(12, p(em("语法: ", r$grammar$comment))))
              },
              if (!is.null(r$organization$comment) && r$organization$comment != "") {
                fluidRow(column(12, p(em("组织: ", r$organization$comment))))
              },
              if (!is.null(r$mechanics$comment) && r$mechanics$comment != "") {
                fluidRow(column(12, p(em("机械: ", r$mechanics$comment))))
              },
              hr(),
              p(strong("🔽 识别文本（可复制到 response 框）:")),
              pre(style = "font-size:12px; background:#f8f9fa; padding:8px;",
                  r$recognized_text)
          ),
          div(class = "alert alert-warning",
              p(strong("📋 临床医生确认:"), " 请审核 AI 评分并手动调整下方评分后保存。"),
              actionButton("btn_apply_ai_scores", "✅ 采纳 AI 建议分数",
                           icon = icon("check"), class = "btn-success btn-sm")
          )
        )
      })
    } else {
      err_msg <- if (!is.null(ai_result_rv$data$error)) ai_result_rv$data$error else "未知错误"
      output$sw_ai_result <- renderUI({
        div(class = "alert alert-danger",
            h5("❌ AI 评分失败"),
            p(err_msg),
            p("请手动评分或重试。")
        )
      })
    }
  })

  # Apply AI scores to the rating UI
  observeEvent(input$btn_apply_ai_scores, {
    r <- ai_result_rv$data
    req(!is.null(r))
    rubric <- sw_rubric_r()
    if (is.null(rubric)) return()
    updateRadioButtons(session, "sw_struct",  selected = r$structure$score)
    updateRadioButtons(session, "sw_grammar", selected = r$grammar$score)
    updateRadioButtons(session, "sw_org",     selected = r$organization$score)
    updateRadioButtons(session, "sw_mech",    selected = r$mechanics$score)
    showNotification("✅ AI 分数已填入，请确认后保存", type = "message")
  })

  # Clear AI results
  observeEvent(input$btn_clear_ai, {
    ai_result_rv$status <- "idle"
    ai_result_rv$data   <- NULL
    output$sw_ai_result <- renderUI(NULL)
  })

  # ═══════════════════════════════════════════════════════════════
  # END SW STANDALONE TAB
  # ═══════════════════════════════════════════════════════════════

  # ── 按钮交互（移到 tab 3 外部，点击事件始终有效）──────────

  observeEvent(input$btn_save_score, {
    t <- rv$current_subtest
    i_n <- rv$current_item
    sp <- rv$start_point
    rt <- if (is.null(input$response_text) || is.na(input$response_text)) "" else input$response_text

    # SW uses standalone tab — nothing to do here
    if (t == "SW") {
      showNotification("请使用「📝 写作任务」标签页进行 Structured Writing 评分",
                       type = "warning")
      return()
    } else if (t == "USP") {
      # ── USP per-question score save ──────────────────────────
      qi <- get_question_info("USP", i_n, rv$age_group)
      questions_json_str <- if (is.character(qi$questions_json[1]) && nzchar(qi$questions_json[1])) qi$questions_json[1] else "[]"
      questions_list <- tryCatch(jsonlite::fromJSON(questions_json_str, simplifyVector = FALSE)[[1]], error = function(e) list())
      n_q <- length(questions_list)

      q_scores <- lapply(seq_len(n_q), function(qi_idx) {
        q_name <- paste0("usp_q", i_n, "_", qi_idx)
        input[[q_name]]
      })
      q_scores_vec <- as.integer(q_scores)

      if (any(is.na(q_scores_vec))) {
        showNotification("请完成所有题目评分 / Please score all questions", type = "warning"); return()
      }

      # Sum over all questions for this paragraph item
      total_score <- sum(q_scores_vec, na.rm = TRUE)
      # Store response_text as JSON of per-question scores
      resp_json <- jsonlite::toJSON(setNames(q_scores_vec, paste0("Q", seq_len(n_q))), auto_unbox = TRUE)

      # Clear old responses for this item (may have multiple rows for per-question) and re-add
      rv$responses <- rv$responses %>% filter(!(subtest=="USP" & item_number==!!i_n)) %>%
        add_row(subtest="USP", item_number=i_n, response_text=resp_json, score=total_score)

      save_response(rv$assessment_id, "USP", i_n, resp_json, total_score)
      showNotification(glue("已保存 / Saved: USP 第{i_n}题 = {total_score}/{n_q} 分"), type="message")

    } else {
      captured_score <- input$input_score
      if (is.null(captured_score) || is.na(captured_score)) {
        showNotification("请先打分 / Please score first", type = "warning"); return()
      }
      sv <- captured_score
      if (t=="RS" && !is.na(sv)) sv <- score_rs(as.integer(sv))

      rv$responses <- rv$responses %>% filter(!(subtest==!!t & item_number==!!i_n)) %>%
        add_row(subtest=t, item_number=i_n, response_text=as.character(rt),
                score=as.integer(sv))
      save_response(rv$assessment_id, t, i_n, as.character(rt), as.integer(sv))
      showNotification(glue("已保存 / Saved: {t} 第{i_n}题 = {sv}分"), type="message")
    }

    check_discontinue(t)
    check_reversal(t)

    # ── 导航：保存后前进到下一题 ───────────────────────────
    # 没有 end point：只有 discontinue 才会结束 subtest；否则一直做到题目库最后
    real_max <- SUBTEST_DEFS %>% filter(subtest==t) %>% pull(max_items) %>% .[[1]]
    if (rv$discontinue_triggered) {
      # 连续4×0 → 结束当前 subtest，跳到下一个
      rv$completed_subtests <- c(rv$completed_subtests, t) %>% unique()
      next_t <- setdiff(rv$test_list, rv$completed_subtests)[1]
      if (!is.na(next_t)) {
        updateSelectInput(session, "selected_subtest", selected = next_t)
      }
    } else {
      # 继续前进：没有 end point，永远前进一题
      rv$current_item <- i_n + 1L
      # 清除打分控件
      if (t == "RS") {
        updateNumericInput(session, "input_score", value = NA_integer_)
      } else {
        updateRadioButtons(session, "input_score", selected = NA_integer_)
      }
    }
  })

  observeEvent(input$btn_prev, {
    if (rv$current_subtest == "SW") return()  # SW uses standalone tab
    if (rv$current_item > 1) rv$current_item <- rv$current_item - 1L
    if (rv$current_item < 1) rv$current_item <- 1L
  })

  observeEvent(input$btn_next, {
    if (rv$current_subtest == "SW") return()  # SW uses standalone tab
    t <- rv$current_subtest
    i_n <- rv$current_item
    # 没有 end point：仅 discontinue 触发时才切换 subtest；否则永远前进一题
    rv$current_item <- i_n + 1L
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

    # ── Reversal 检查（Manual Chapter 3 原文）──────────────────
    # 有 reversal: SC / LC / WC / FS / RS / WD / SA
    # 无 reversal: FD / WS / SR / RC / SW / USP
    #
    # Continue Rule（所有 subtest 通用）：
    #   start_point 只是施测起点，不是题库上限。
    #   学生做对就一路往下做到 max_item 或 discontinue。
    #   绝对不能在到达 max_item 之前提前停止。
    #
    # WC 特殊 Reversal 规则（Manual P46-48）：
    #   Ages 5-10:   start_point=1 → Reversal 不适用
    #   Ages 11-14:  前两题(sp=13, sp+1=14) 未满分 → 回到 Demo+Trials → Item 1
    #                 (我们无 Trial Items → 直接跳 Item 1)
    #   Ages 15-21:  两级检查：
    #     第一级：前两题(sp=20, sp+1=21) 未满分 → 跳回 Item 13
    #     第二级：在 Item 13 检测 items 13+14：
    #       - 都满分 → 继续从 Item 15 往下做
    #       - 未满分 → 回到 Demo+Trials → Item 1（我们无 Trial → 直接 Item 1）
    check_reversal <- function(subtest) {
      t <- subtest
      sp <- rv$start_point

      reversal_subs <- c("SC", "LC", "WC", "FS", "RS", "WD", "SA")
      if (!(t %in% reversal_subs)) return()

      if (sp <= 1L) return()              # start_point=1 → 不适用
      if (rv$reversal_triggered) return()   # 已触发过 → 不重复

      sub_r <- rv$responses %>% filter(subtest==!!t) %>% arrange(item_number)

      max_score_t <- max_score_for_subtest(t)

    wc_reversal_depth <- rv$wc_reversal_depth
    # ── WC 15-21 岁两级 Reversal ──────────────────────────────
    if (t == "WC" && sp == 20L) {
      first_two <- sub_r %>% filter(item_number >= 20L) %>% slice(1:2)
      if (nrow(first_two) < 2) return()
      if (first_two$score[1] == max_score_t && first_two$score[2] == max_score_t) {
        rv$wc_reversal_depth <- 0L; return()  # 两题都满分，不触发
      }
      # level 1 触发（items 20+21 未满分）→ 跳 Item 13 做 level 2
      rv$wc_reversal_depth <- 1L
      rv$reversal_triggered <- TRUE   # ← Bug 1 fix: 阻止 generic reversal 继续乱触发
      to_backfill <- setdiff(seq(1, 19), sub_r$item_number)
      purrr::walk(to_backfill, function(i_n) {
        if (!any(rv$responses$subtest == t & rv$responses$item_number == i_n)) {
          rv$responses <- rv$responses %>% add_row(
            subtest = t, item_number = i_n,
            response_text = "(backfill)", score = 1L,
            structure_complete = NA_integer_, grammar = NA_integer_,
            organization = NA_integer_, mechanics = NA_integer_
          )
          save_response(rv$assessment_id, t, i_n, "(backfill)", 1L)
        }
      })
      rv$current_item <- 13L
      showNotification(
        glue("↩ Reversal Level 1！WC 15-21：items 20+21 未满分 → 跳回 Item 13 做 level 2"),
        type = "warning", duration = 15
      )
      return()
    }

    # ── WC 15-21 level 2：items 13+14 检查 ─────────────────
    if (t == "WC" && sp == 20L && rv$wc_reversal_depth == 1L) {
      first_two <- sub_r %>% filter(item_number >= 13L) %>% slice(1:2)
      if (nrow(first_two) < 2) return()
      if (first_two$score[1] == max_score_t && first_two$score[2] == max_score_t) {
        # items 13+14 都满分 → level 2 不触发，level 1 继续正常做
        rv$wc_reversal_depth <- 2L; return()
      }
      # level 2 触发（items 13+14 未满分）→ 跳 Item 1
      rv$wc_reversal_depth <- 2L
      rv$reversal_triggered <- TRUE   # ← Bug 1 fix: 阻止 generic reversal 继续乱触发
      # Backfill items 1-12（items 13-19 已在 level 1 做过且满分）
      to_backfill <- setdiff(seq(1, 12), sub_r$item_number)
      purrr::walk(to_backfill, function(i_n) {
        if (!any(rv$responses$subtest == t & rv$responses$item_number == i_n)) {
          rv$responses <- rv$responses %>% add_row(
            subtest = t, item_number = i_n,
            response_text = "(backfill)", score = 1L,
            structure_complete = NA_integer_, grammar = NA_integer_,
            organization = NA_integer_, mechanics = NA_integer_
          )
          save_response(rv$assessment_id, t, i_n, "(backfill)", 1L)
        }
      })
      rv$current_item <- 1L
      showNotification(
        glue("↩ Reversal Level 2！WC 15-21：items 13+14 未满分 → 回到 Item 1"),
        type = "warning", duration = 15
      )
      return()
    }

    # ── WC 11-14 岁特殊 Reversal（跳 Item 1）─────────────────
    if (t == "WC" && sp == 13L) {
      first_two <- sub_r %>% filter(item_number >= 13L) %>% slice(1:2)
      if (nrow(first_two) < 2) return()
      if (first_two$score[1] == max_score_t && first_two$score[2] == max_score_t) {
        return()
      }
      rv$wc_reversal_depth <- 2L
      rv$reversal_triggered <- TRUE   # ← Bug 1 fix: 阻止 generic reversal 继续乱触发
      to_backfill <- setdiff(seq(1, 12), sub_r$item_number)
      purrr::walk(to_backfill, function(i_n) {
        if (!any(rv$responses$subtest == t & rv$responses$item_number == i_n)) {
          rv$responses <- rv$responses %>% add_row(
            subtest = t, item_number = i_n,
            response_text = "(backfill)", score = 1L,
            structure_complete = NA_integer_, grammar = NA_integer_,
            organization = NA_integer_, mechanics = NA_integer_
          )
          save_response(rv$assessment_id, t, i_n, "(backfill)", 1L)
        }
      })
      rv$current_item <- 1L
      showNotification(
        glue("↩ Reversal！WC 11-14：items 13+14 未满分 → 回到 Item 1"),
        type = "warning", duration = 15
      )
      return()
    }

    # ── 通用 Reversal（SC/LC/FS/RS/WD/SA）───────────────────
    first_two <- sub_r %>% filter(item_number >= sp) %>% slice(1:2)
    if (nrow(first_two) < 2) return()
    if (first_two$score[1] == max_score_t && first_two$score[2] == max_score_t) {
      return()  # 前两题都满分 → 不触发
    }
    rv$reversal_triggered <- TRUE
    rv$reversal_item <- sp
    to_backfill <- setdiff(seq(1, sp - 1), sub_r$item_number)
    purrr::walk(to_backfill, function(i_n) {
      if (!any(rv$responses$subtest == t & rv$responses$item_number == i_n)) {
        rv$responses <- rv$responses %>% add_row(
          subtest = t, item_number = i_n,
          response_text = "(backfill)", score = 1L,
          structure_complete = NA_integer_, grammar = NA_integer_,
          organization = NA_integer_, mechanics = NA_integer_
        )
        save_response(rv$assessment_id, t, i_n, "(backfill)", 1L)
      }
    })
    rv$current_item <- 1L
    showNotification(
      glue("↩ Reversal！{t} items {sp},{sp+1} 未满分 → 回到 Item 1（补 {length(to_backfill)} 题满分）"),
      type = "warning", duration = 15
    )
  }

  # ── 施测开始 / 切换 subtest 时重置 reversal 状态 ─────────────
  observeEvent(rv$current_subtest, {
    rv$reversal_triggered <- FALSE
    rv$reversal_item <- 0L
  }, ignoreInit = TRUE)


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
        WD  = "Ability to define words by describing their semantic features and relevant attributes.",
        SA  = "Ability to construct grammatically well-formed sentences using specified word classes.",
        SR  = "Ability to interpret sentences involving semantic relationships such as comparison, location, time, serial order, and passive voice.",
        RC  = "Ability to understand written passages and answer comprehension questions.",
        SW  = "Ability to produce organized narrative writing following structural conventions."
      )

      div(class = "card mb-3",
        div(class = "card-header d-flex justify-content-between align-items-center",
          strong(if (is.null(subtest_names[[st]])) st else subtest_names[[st]]),
          span(class = paste0("badge ", rng), score_lbl)
        ),
        div(class = "card-body",
          p(strong("原始分 Raw Score: "), as.character(if (is.null(raw) || is.na(raw)) "—" else raw)),
          p(strong("量表分 Scaled Score (M=10, SD=3): "), score_lbl),
          {
            ae  <- get_age_equiv(st, raw, ag)
            tagList(
              p(strong("语言年龄 Age Equivalent: "), if (is.na(ae)) "—" else ae)
            )
          },
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

      pct_disp <- fmt_pct(pct)

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
          strong(if (is.null(comp_names[[comp]])) comp else comp_names[[comp]]),
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
        column(6, p(strong("性别 Sex: "), if (is.na(full$assessment$gender) || full$assessment$gender == "F") "女 / Female" else "男 / Male"))
      ),
      fluidRow(
        column(6, p(strong("年龄 Age: "), glue("{full$assessment$age_years}y {full$assessment$age_months}m ({ag})"))),
        column(6, p(strong("评估日期 Date: "), full$assessment$assessment_date))
      ),
      fluidRow(
        column(6, p(strong("评估师 Examiner: "), if (is.null(full$assessment$examiner) || is.na(full$assessment$examiner)) "—" else full$assessment$examiner)),
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
      )
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
            examiner_name    = if (is.null(full$assessment$examiner) || is.na(full$assessment$examiner)) NA else full$assessment$examiner,
            scaled_scores    = scaled_df,
            indices          = idx_list,
            overall_en        = isolate(if (is.null(rv$overall_en) || is.na(rv$overall_en)) "No assessment data available." else rv$overall_en),
            overall_zh       = isolate(if (is.null(rv$overall_zh) || is.na(rv$overall_zh)) "无评估数据。" else rv$overall_zh)
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
            examiner_name    = if (is.null(full$assessment$examiner) || is.na(full$assessment$examiner)) NA else full$assessment$examiner,
            scaled_scores    = scaled_df,
            indices          = idx_list,
            overall_en        = isolate(if (is.null(rv$overall_en) || is.na(rv$overall_en)) "No assessment data available." else rv$overall_en),
            overall_zh       = isolate(if (is.null(rv$overall_zh) || is.na(rv$overall_zh)) "无评估数据。" else rv$overall_zh)
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
        Sys.setenv(PATH = paste("/home/yzhang/.TinyTeX/bin/x86_64-linux", Sys.getenv("PATH"), sep = ":"))
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
            examiner_name    = if (is.null(full$assessment$examiner) || is.na(full$assessment$examiner)) NA else full$assessment$examiner,
            scaled_scores    = scaled_df,
            indices          = idx_list,
            overall_en        = isolate(if (is.null(rv$overall_en) || is.na(rv$overall_en)) "No assessment data available." else rv$overall_en),
            overall_zh       = isolate(if (is.null(rv$overall_zh) || is.na(rv$overall_zh)) "无评估数据。" else rv$overall_zh)
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

  # ── AI 临床叙事报告 ─────────────────────────────────────────
  # Phase: "idle" | "generating" | "done" | "error"
  narrative_phase <- reactiveVal("idle")
  # narrative_text removed — was dead state (never read in UI)

  output$narrative_status <- renderUI({ NULL })
  output$narrative_preview <- renderUI({ NULL })

  # observe 驱动 UI：phase 一切换，spinner/报告立刻画出来
  observe({
narrative_phase <- narrative_phase()
    if (narrative_phase == "generating") {
      output$narrative_status <- renderUI({
        tags$div(
          tags$style(HTML("
            @keyframes ai-spin { to { transform: rotate(360deg); } }
            .ai-spin { width:18px; height:18px; border:2px solid #dee2e6;
                       border-top:2px solid #1B3A6B; border-radius:50%;
                       display:inline-block; animation:ai-spin 0.7s linear infinite; }
            .ai-msg  { display:inline; margin-left:8px; color:#6c757d; }
          ")),
          tags$div(style="margin-top:6px",
            tags$span(class="ai-spin"),
            tags$span(class="ai-msg", "🤖 正在生成报告，请稍候...")
          )
        )
      })
      output$narrative_preview <- renderUI({ NULL })
    }
  })

  observeEvent(input$btn_gen_narrative, {
    req(rv$assessment_id)
    lang <- input$report_lang
    aid <- rv$assessment_id  # capture before later::later()

    # 第一步：立刻切换 phase → 触发上面的 observe 立即画 spinner
    narrative_phase("generating")

    # 第二步：推迟 API 调用到下一个 tick，让 reactive flush 先跑完
    later::later(function() {

    tryCatch({
      narrative <- if (lang == "zh") {
        generate_clinical_narrative(aid)
      } else {
        generate_clinical_narrative_en(aid)
      }

      # ── 清理 think tags + 残余 prompt 前缀 ──────────────────
              narrative <- .clean_narrative_tags(narrative)

narrative_phase("done")

      output$narrative_status <- renderUI({
        div(class = "alert alert-success mb-0", role = "alert",
          icon("check-circle"),
          if (lang == "zh") "中文报告已生成！" else "English report generated!",
          " ",
          actionLink("btn_regen_narrative",
            if (lang == "zh") " 重新生成" else " Regenerate",
            icon = icon("refresh"), class = "btn btn-sm btn-outline-success"))
      })

      md_html <- markdown::markdownToHTML(text = narrative, fragment.only = TRUE)
      output$narrative_preview <- renderUI({
        tags$div(
          tags$style(HTML("
            .ai-report-card { background:#fafafa; border:1px solid #e9ecef;
                             border-radius:8px; padding:20px 24px; margin-top:12px;
                             font-size:14px; line-height:1.75; max-height:600px;
                             overflow-y:auto; }
            .ai-report-card h1,.ai-report-card h2,.ai-report-card h3 { color:#1B3A6B; margin-top:14px; }
            .ai-report-card h1:first-child,.ai-report-card h2:first-child { margin-top:0; }
            .ai-report-card ul,.ai-report-card ol { padding-left:22px; }
            .ai-report-card li { margin-bottom:5px; }
            .ai-report-card strong { color:#1B3A6B; }
            .ai-report-card em { color:#666; font-style:italic; }
            .ai-report-card hr { border-top:1px solid #ddd; margin:12px 0; }
          ")),
          div(class = "card mb-3",
            div(class = "card-header d-flex justify-content-between align-items-center",
              strong(if (lang == "zh") "中文临床叙事报告" else "English Clinical Narrative"),
              span(class = "badge bg-secondary", "AI")
            ),
            div(class = "card-body p-0",
              div(class = "ai-report-card", HTML(md_html))
            )
          )
        )
      })
    }, error = function(e) {
      # 失败 → 切换 phase 为 error
      narrative_phase("error")
      output$narrative_status <- renderUI({
        div(class = "alert alert-danger mb-0", role = "alert",
          icon("exclamation-triangle"),
          if (lang == "zh") "生成失败: " else "Generation failed: ",
          e$message)
      })
      cat(file = stderr(), "[narrative error]", e$message, "\n")
    })  # closes tryCatch
  })  # closes later::later
  })  # closes observeEvent(input$btn_gen_narrative, ...)

  # 重新生成按钮
  observeEvent(input$btn_regen_narrative, {
    req(rv$assessment_id)
    lang <- input$report_lang
    aid <- rv$assessment_id  # capture before later::later()

    narrative_phase("generating")

    later::later(function() {
      tryCatch({
        narrative <- if (lang == "zh") {
          generate_clinical_narrative(aid)
        } else {
          generate_clinical_narrative_en(aid)
        }

                narrative <- .clean_narrative_tags(narrative)

narrative_phase("done")

        output$narrative_status <- renderUI({
          div(class = "alert alert-success mb-0", role = "alert",
            icon("check-circle"),
            if (lang == "zh") "中文报告已生成！" else "English report generated!",
            " ",
            actionLink("btn_regen_narrative",
              if (lang == "zh") " 重新生成" else " Regenerate",
              icon = icon("refresh"), class = "btn btn-sm btn-outline-success"))
        })

        md_html <- markdown::markdownToHTML(text = narrative, fragment.only = TRUE)
        output$narrative_preview <- renderUI({
          tags$div(
            tags$style(HTML("
              .ai-report-card { background:#fafafa; border:1px solid #e9ecef;
                               border-radius:8px; padding:20px 24px; margin-top:12px;
                               font-size:14px; line-height:1.75; max-height:600px;
                               overflow-y:auto; }
              .ai-report-card h1,.ai-report-card h2,.ai-report-card h3 { color:#1B3A6B; margin-top:14px; }
              .ai-report-card h1:first-child,.ai-report-card h2:first-child { margin-top:0; }
              .ai-report-card ul,.ai-report-card ol { padding-left:22px; }
              .ai-report-card li { margin-bottom:5px; }
              .ai-report-card strong { color:#1B3A6B; }
              .ai-report-card em { color:#666; font-style:italic; }
              .ai-report-card hr { border-top:1px solid #ddd; margin:12px 0; }
            ")),
            div(class = "card mb-3",
              div(class = "card-header d-flex justify-content-between align-items-center",
                strong(if (lang == "zh") "中文临床叙事报告" else "English Clinical Narrative"),
                span(class = "badge bg-secondary", "AI")
              ),
              div(class = "card-body p-0",
                div(class = "ai-report-card", HTML(md_html))
              )
            )
          )
        })
      }, error = function(e) {
        narrative_phase("error")
        output$narrative_status <- renderUI({
          div(class = "alert alert-danger mb-0", role = "alert",
            icon("exclamation-triangle"),
            if (lang == "zh") "生成失败: " else "Generation failed: ",
            e$message)
        })
        cat(file = stderr(), "[narrative error]", e$message, "\n")
      })
    })  # closes later::later
  })    # closes observeEvent(input$btn_regen_narrative, ...)
}      # closes server
shinyApp(ui, server)
