# app_homepage.R — CELF5 2.0 SLP 共同首頁
# 三大評估入口：CELF5、SLAM、聯合報告

library(shiny)
library(bslib)
library(dplyr)
library(RSQLite)
library(lubridate)
library(stringr)
library(DT)
library(shinyjs)

# ─────────────────────────────────────────────────────────────
# DB helpers
# ─────────────────────────────────────────────────────────────
DB_PATH <- "/home/yzhang/clawfiles/celf5_shiny/celf5_assessments.db"
get_con <- function() dbConnect(SQLite(), DB_PATH)

get_patient_assessments <- function(student_id) {
  con <- get_con()
  on.exit(dbDisconnect(con), add = TRUE)
  dbGetQuery(con, sprintf(
    "SELECT id, assessment_type, strftime('%%Y-%%m-%%d', assessment_date) as date_str,
            age_years, age_group FROM assessments WHERE patient_id = %d ORDER BY assessment_date DESC",
    student_id
  ))
}

get_patient_name <- function(student_id) {
  con <- get_con()
  on.exit(dbDisconnect(con), add = TRUE)
  dbGetQuery(con, sprintf("SELECT name FROM patients WHERE id = %d", student_id))$name[1]
}

# ─────────────────────────────────────────────────────────────
# 配色常量
# ─────────────────────────────────────────────────────────────
celf5_blue   <- "#1B3A6B"
celf5_gold  <- "#C8A951"
celf5_gray  <- "#F5F5F5"
celf5_white <- "#FFFFFF"

# ─────────────────────────────────────────────────────────────
# UI
# ─────────────────────────────────────────────────────────────
ui <- fluidPage(
  theme = bslib::bs_theme(
    version = 5,
    primary = celf5_blue,
    secondary = celf5_gold
  ),

  tags$head(
    tags$style(HTML("
      body {
        background: linear-gradient(135deg, #f8f9fa 0%, #e8ecf3 100%);
        font-family: 'Segoe UI', Arial, sans-serif;
        min-height: 100vh;
      }
      .main-container {
        max-width: 1100px;
        margin: 0 auto;
        padding: 32px 20px;
      }
      /* Hero Section */
      .home-hero {
        background: linear-gradient(135deg, #1B3A6B 0%, #2a5ab3 100%);
        color: white;
        border-radius: 20px;
        padding: 56px 40px;
        margin-bottom: 40px;
        text-align: center;
        box-shadow: 0 12px 40px rgba(27,58,107,0.3);
      }
      .home-hero h1 {
        color: white;
        font-size: 36px;
        font-weight: 700;
        margin-bottom: 10px;
      }
      .home-hero .subtitle {
        color: rgba(255,255,255,0.85);
        font-size: 16px;
        margin: 0;
        letter-spacing: 0.5px;
      }
      .home-hero .gold-accent {
        color: #C8A951;
        font-weight: 600;
      }
      /* Entry Cards */
      .cards-row {
        display: flex;
        gap: 24px;
        margin-bottom: 36px;
      }
      .entry-card {
        flex: 1;
        border-radius: 18px;
        border: 2px solid #e8eaf0;
        padding: 32px 24px;
        text-align: center;
        transition: all 0.3s ease;
        background: white;
        cursor: pointer;
        text-decoration: none;
        display: flex;
        flex-direction: column;
        align-items: center;
        min-height: 320px;
      }
      .entry-card:hover {
        border-color: #1B3A6B;
        box-shadow: 0 10px 32px rgba(27,58,107,0.18);
        transform: translateY(-4px);
        text-decoration: none;
      }
      .entry-card:active {
        transform: translateY(-1px);
      }
      .entry-icon {
        font-size: 56px;
        margin-bottom: 18px;
        display: block;
      }
      .entry-title {
        font-size: 22px;
        font-weight: 700;
        color: #1B3A6B;
        margin-bottom: 12px;
      }
      .entry-desc {
        font-size: 14px;
        color: #666;
        line-height: 1.7;
        margin-bottom: 20px;
        flex-grow: 1;
      }
      .entry-btn {
        display: inline-block;
        padding: 10px 28px;
        border-radius: 25px;
        font-size: 14px;
        font-weight: 600;
        transition: all 0.2s ease;
      }
      .btn-celf5 {
        background: #1B3A6B;
        color: white;
        border: none;
      }
      .btn-celf5:hover {
        background: #1452a3;
        color: white;
        box-shadow: 0 4px 12px rgba(27,58,107,0.35);
      }
      .btn-slam {
        background: white;
        color: #1B3A6B;
        border: 2px solid #1B3A6B;
      }
      .btn-slam:hover {
        background: #1B3A6B;
        color: white;
      }
      .btn-combined {
        background: linear-gradient(135deg, #1B3A6B 0%, #2a5ab3 100%);
        color: white;
        border: none;
      }
      .btn-combined:hover {
        box-shadow: 0 4px 12px rgba(27,58,107,0.35);
        color: white;
      }
      .entry-badge {
        display: inline-block;
        padding: 5px 16px;
        border-radius: 20px;
        font-size: 12px;
        font-weight: 600;
        margin-bottom: 16px;
      }
      .badge-celf5  { background: #e8f0fe; color: #1B3A6B; }
      .badge-slam   { background: #fff3e0; color: #e65100; }
      .badge-combined { background: #e8f5e9; color: #2e7d32; }
      /* Student Panel */
      .student-panel {
        background: white;
        border-radius: 18px;
        border: 1px solid #e0e4ee;
        box-shadow: 0 4px 16px rgba(0,0,0,0.06);
        overflow: hidden;
      }
      .student-panel-header {
        background: linear-gradient(135deg, #1B3A6B 0%, #2a5ab3 100%);
        color: white;
        padding: 18px 28px;
        font-size: 18px;
        font-weight: 600;
        display: flex;
        align-items: center;
        gap: 10px;
      }
      .student-panel-body {
        padding: 28px;
      }
      .form-label {
        font-size: 13px;
        font-weight: 600;
        color: #1B3A6B;
        margin-bottom: 5px;
      }
      .home-footer {
        text-align: center;
        margin-top: 40px;
        padding: 20px;
        color: #aaa;
        font-size: 13px;
      }
      .footer-brand {
        color: #1B3A6B;
        font-weight: 600;
      }
      /* Responsive */
      @media (max-width: 768px) {
        .cards-row { flex-direction: column; }
        .entry-card { min-height: auto; }
        .home-hero h1 { font-size: 26px; }
      }
    "))
  ),

  div(class = "main-container",

    # ── Hero ──────────────────────────────────────────────
    div(class = "home-hero",
      h1("CELF-5 2.0 共同评估平台"),
      p(class = "subtitle",
        "CELF-5 2.0 Integrated Assessment Platform",
        br(),
        span(class = "gold-accent", "三大评估入口  |  Unified Assessment Hub")
      )
    ),

    # ── Entry Cards ────────────────────────────────────────
    div(class = "cards-row",

      # Card 1: CELF-5
      div(class = "entry-card",
        onclick = "Shiny.setInputValue('home_goto', 'celf5', {priority: 'event'});",
        span(class = "entry-icon", "📋"),
        div(class = "entry-title", "CELF-5"),
        div(class = "entry-desc",
          "语言评估基础工具",
          br(),
          "Language Assessment",
          br(),
          br(),
          "核心语言分数、复合量表分、临床叙事报告"
        ),
        span(class = "entry-badge badge-celf5", "进行中 / In Use"),
        span(class = "entry-btn btn-celf5", "进入评估  ›")
      ),

      # Card 2: SLAM
      div(class = "entry-card",
        onclick = "window.location.href = '/slam/';",
        span(class = "entry-icon", "📖"),
        div(class = "entry-title", "SLAM"),
        div(class = "entry-desc",
          "叙事评估工具",
          br(),
          "Narrative Assessment",
          br(),
          br(),
          "图片叙事情境，评估叙事能力与语用"
        ),
        span(class = "entry-badge badge-slam", "进行中 / In Use"),
        span(class = "entry-btn btn-slam", "进入评估  ›")
      ),

    ),

    # ── Combined Report Panel ───────────────────────────────
    div(class = "student-panel",
      div(class = "student-panel-header",
        span("📊"), "联合报告 / Combined Report"
      ),
      div(class = "student-panel-body",

        fluidRow(
          column(12,
            div(class = "form-group",
              tags$label(class = "form-label", "选择学生 / Select Student"),
              DT::dataTableOutput("patient_list_dt"),
              tags$style(HTML("
                #patient_list_dt { font-size: 13px; }
                #patient_list_dt tbody tr { cursor: pointer; }
                #patient_list_dt tbody tr:hover { background-color: #e8f0fe !important; }
              "))
            )
          )
        ),

        fluidRow(
          column(12,
            uiOutput("patient_load_delete_btns")
          )
        ),

        hr(style = sprintf("border-color: %s; opacity: 0.3;", celf5_blue)),

        fluidRow(
          column(12,
            div(id = "combined_report_actions",
              uiOutput("combined_report_ui")
            )
          )
        ),

        hr(style = sprintf("border-color: %s; opacity: 0.3;", celf5_blue)),

        fluidRow(
          column(12,
            uiOutput("combined_report_output")
          )
        )
      )
    ),

    # ── Footer ─────────────────────────────────────────────
    div(class = "home-footer",
      span(class = "footer-brand", "CELF-5 2.0"), " © 2026  |  Powered by Shiny + bslib  |  ",
      "如需帮助请联系评估系统管理员"
    )
  )
)

# ─────────────────────────────────────────────────────────────
# Server
# ─────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  # ── Card Navigation ────────────────────────────────────
  observeEvent(input$home_goto, {
    msg <- input$home_goto
    if (msg == "celf5") {
      showNotification(
        tagList(icon("check-circle"), "正在进入 CELF-5 评估系统..."),
        type = "message", duration = 2
      )
    } else if (msg == "combined") {
      showNotification(
        tagList(icon("info-circle"), "正在打开联合报告..."),
        type = "message", duration = 2
      )
    }
  })

  # ── Patient DT ────────────────────────────────────────
  output$patient_list_dt <- DT::renderDataTable({
    query <- "SELECT p.id, p.name, p.dob, p.gender,
      SUM(CASE WHEN a.assessment_type IN ('CELF5','CELF-5') THEN 1 ELSE 0 END) as celf5_count,
      SUM(CASE WHEN a.assessment_type = 'SLAM' THEN 1 ELSE 0 END) as slam_count
    FROM patients p
    LEFT JOIN assessments a ON p.id = a.patient_id
    GROUP BY p.id
    ORDER BY p.id DESC"
    con <- get_con()
    on.exit(dbDisconnect(con), add = TRUE)
    df <- dbGetQuery(con, query)
    colnames(df) <- c("ID", "姓名/Name", "出生日期/DOB", "性别/Gender", "# CELF-5", "# SLAM")
    df$`性别/Gender` <- ifelse(df$`性别/Gender` == "M", "男/M",
                        ifelse(df$`性别/Gender` == "F", "女/F", "—"))
    df
  }, selection = "single", escape = FALSE,
     options = list(
       pageLength = 10,
       dom = "frtip",
       columnDefs = list(
         list(visible = FALSE, targets = 0),
         list(className = "dt-center", targets = c(2, 3, 4, 5))
       ),
       language = list(
         emptyTable = "暂无学生记录",
         search = "搜索：",
         info = "显示第 _START_ 至 _END_ 条，共 _TOTAL_ 条"
       )
     ))

  # ── Store selected patient on DT row click ────────────
  observeEvent(input$patient_list_dt_rows_selected, {
    req(input$patient_list_dt_rows_selected)
    query <- "SELECT p.id, p.name, p.dob, p.gender,
      SUM(CASE WHEN a.assessment_type IN ('CELF5','CELF-5') THEN 1 ELSE 0 END) as celf5_count,
      SUM(CASE WHEN a.assessment_type = 'SLAM' THEN 1 ELSE 0 END) as slam_count
    FROM patients p
    LEFT JOIN assessments a ON p.id = a.patient_id
    GROUP BY p.id
    ORDER BY p.id DESC"
    con <- get_con()
    on.exit(dbDisconnect(con), add = TRUE)
    df <- dbGetQuery(con, query)
    row_idx <- input$patient_list_dt_rows_selected[1]
    combined_rv$selected_patient_id    <- df$id[row_idx]
    combined_rv$selected_patient_name <- df$name[row_idx]
    celf5_cnt <- as.integer(df$celf5_count[row_idx])
    slam_cnt  <- as.integer(df$slam_count[row_idx])
    combined_rv$selected_celf5_count  <- celf5_cnt
    combined_rv$selected_slam_count   <- slam_cnt
  })

  # ── Deselect: clear everything ────────────────────────
  observeEvent(input$patient_list_dt_rows_selected, {
    if (is.null(input$patient_list_dt_rows_selected) || length(input$patient_list_dt_rows_selected) == 0) {
      combined_rv$selected_patient_id    <<- NULL
      combined_rv$selected_patient_name <<- NULL
      combined_rv$selected_celf5_count  <<- 0
      combined_rv$selected_slam_count   <<- 0
      combined_rv$patient_name          <<- NULL
      combined_rv$student_id            <<- NULL
      output$combined_report_ui    <<- renderUI(NULL)
      output$combined_report_output <<- renderUI(NULL)
    }
  }, ignoreInit = TRUE)

  # ── Load/Delete buttons (show when row selected) ──────
  output$patient_load_delete_btns <- renderUI({
    req(!is.null(input$patient_list_dt_rows_selected))
    tagList(
      fluidRow(
        column(6,
          actionButton("btn_load_assessment", "📂 加载 / Load",
            class = "btn-primary",
            style = sprintf("width:100%%; background:%s; border:none;", celf5_blue))
        ),
        column(6,
          actionButton("btn_delete_patient_confirm", "🗑 删除 / Delete",
            class = "btn-danger",
            style = "width:100%%; background:#c0392b; border:none;")
        )
      )
    )
  })

  # ── Load: query assessments, build report UI ────────────
  observeEvent(input$btn_load_assessment, {
    req(combined_rv$selected_patient_id)
    sid   <- combined_rv$selected_patient_id
    pname <- combined_rv$selected_patient_name
    tryCatch({
      assessments <- get_patient_assessments(sid)
      celf5_ass  <- assessments[grepl("CELF5|celf5", assessments$assessment_type, ignore.case = TRUE), ]
      slam_ass   <- assessments[grepl("SLAM|slam",  assessments$assessment_type, ignore.case = TRUE), ]
      combined_rv$celf5_assessments <- celf5_ass
      combined_rv$slam_assessments  <- slam_ass
      combined_rv$patient_name      <- pname
      combined_rv$student_id        <- sid
      has_celf5 <- nrow(celf5_ass) > 0
      has_slam  <- nrow(slam_ass)  > 0

      if (has_celf5 && has_slam) {
        celf5_opts <- sprintf("<option value='%d'>CELF5 #%d (%s)</option>",
                              celf5_ass$id, celf5_ass$id, celf5_ass$date_str)
        slam_opts  <- sprintf("<option value='%d'>SLAM #%d (%s)</option>",
                              slam_ass$id, slam_ass$id, slam_ass$date_str)
        ui_html <- sprintf(
          '<div style="background: linear-gradient(135deg, #e8f0fe 0%%, #fff8e1 100%%); border-radius: 12px; padding: 16px;">
            <p style="margin:0 0 10px 0; font-size:13px; color:#2c5aa0;">
              <span style="font-size:16px;">🎉</span> 已找到 %s 的 CELF-5 和 SLAM 评估记录。
            </p>
            <div class="form-group" style="margin-bottom:8px;">
              <select id="combined_celf5_id" class="form-control" style="border-radius:8px;">
                <option value="">— 选择 CELF-5 评估 —</option>
                %s
              </select>
            </div>
            <div class="form-group" style="margin-bottom:8px;">
              <select id="combined_slam_id" class="form-control" style="border-radius:8px;">
                <option value="">— 选择 SLAM 评估 —</option>
                %s
              </select>
            </div>
            <button id="btn_generate_combined" type="button" class="btn btn-primary" style="width:100%%; background:%s; border:none; border-radius:8px; font-weight:600;">
              📊 生成联合临床报告
            </button>
          </div>',
          pname, paste(celf5_opts, collapse=""), paste(slam_opts, collapse=""), celf5_blue)
      } else if (has_celf5) {
        celf5_opts <- sprintf("<option value='%d'>CELF5 #%d (%s)</option>",
                              celf5_ass$id, celf5_ass$id, celf5_ass$date_str)
        ui_html <- sprintf(
          '<div style="background:#e8f0fe; border-radius:12px; padding:16px;">
            <p style="margin:0 0 10px 0; font-size:13px; color:#2c5aa0;">
              <span style="font-size:16px;">📋</span> 已找到 %s 的 CELF-5 评估记录。
            </p>
            <div class="form-group" style="margin-bottom:8px;">
              <select id="combined_celf5_id" class="form-control" style="border-radius:8px;">
                <option value="">— 选择 CELF-5 评估 —</option>
                %s
              </select>
            </div>
            <input type="hidden" id="combined_slam_id" value="">
            <button id="btn_generate_combined" type="button" class="btn btn-primary" style="width:100%%; background:%s; border:none; border-radius:8px; font-weight:600;">
              📋 生成 CELF-5 临床报告
            </button>
          </div>',
          pname, paste(celf5_opts, collapse=""), celf5_blue)
      } else if (has_slam) {
        slam_opts <- sprintf("<option value='%d'>SLAM #%d (%s)</option>",
                             slam_ass$id, slam_ass$id, slam_ass$date_str)
        ui_html <- sprintf(
          '<div style="background:#fff3e0; border-radius:12px; padding:16px;">
            <p style="margin:0 0 10px 0; font-size:13px; color:#e65100;">
              <span style="font-size:16px;">📖</span> 已找到 %s 的 SLAM 评估记录。
            </p>
            <div class="form-group" style="margin-bottom:8px;">
              <select id="combined_slam_id" class="form-control" style="border-radius:8px;">
                <option value="">— 选择 SLAM 评估 —</option>
                %s
              </select>
            </div>
            <input type="hidden" id="combined_celf5_id" value="">
            <button id="btn_generate_combined" type="button" class="btn btn-primary" style="width:100%%; background:#e65100; border:none; border-radius:8px; font-weight:600;">
              📖 生成 SLAM 叙事报告
            </button>
          </div>',
          pname, paste(slam_opts, collapse=""))
      } else {
        ui_html <- '<div style="background:#f5f5f5; border-radius:12px; padding:16px; text-align:center;">
            <p style="margin:0; font-size:14px; color:#888;">该学生暂无评估记录</p></div>'
      }
      output$combined_report_ui <<- renderUI(HTML(ui_html))
      showNotification(
        tagList(icon("check-circle"), sprintf("已加载: %s", pname)),
        type = "message", duration = 2
      )
    }, error = function(e) {
      showNotification(
        tagList(icon("exclamation-triangle"), "加载失败 / Load failed"),
        type = "error", duration = 3
      )
    })
  })

  # ── Delete patient ────────────────────────────────────
  observeEvent(input$btn_delete_patient_confirm, {
    req(combined_rv$selected_patient_id)
    showModal(modalDialog(
      title = "⚠️ 确认删除 / Confirm Delete",
      sprintf("删除学生ID %d 及其所有评估记录？此操作不可撤销。", combined_rv$selected_patient_id),
      easyClose = FALSE,
      footer = tagList(
        actionButton("btn_delete_patient_confirmed", "🗑 确认删除",
          style = "background:#c0392b; color:white; border:none;"),
        modalButton("取消 / Cancel")
      )
    ))
  })

  observeEvent(input$btn_delete_patient_confirmed, {
    req(combined_rv$selected_patient_id)
    sid <- combined_rv$selected_patient_id
    con <- get_con()
    on.exit(dbDisconnect(con), add = TRUE)
    tryCatch({
      dbExecute(con, sprintf("DELETE FROM subtest_scores WHERE assessment_id IN (SELECT id FROM assessments WHERE patient_id = %d)", sid))
      dbExecute(con, sprintf("DELETE FROM narrative_scores WHERE assessment_id IN (SELECT id FROM assessments WHERE patient_id = %d)", sid))
      dbExecute(con, sprintf("DELETE FROM slam_composites WHERE assessment_id IN (SELECT id FROM assessments WHERE patient_id = %d)", sid))
      dbExecute(con, sprintf("DELETE FROM celf5_index_scores WHERE assessment_id IN (SELECT id FROM assessments WHERE patient_id = %d)", sid))
      dbExecute(con, sprintf("DELETE FROM assessments WHERE patient_id = %d", sid))
      dbExecute(con, sprintf("DELETE FROM patients WHERE id = %d", sid))
      combined_rv$selected_patient_id    <<- NULL
      combined_rv$selected_patient_name <<- NULL
      combined_rv$selected_celf5_count  <<- 0
      combined_rv$selected_slam_count   <<- 0
      combined_rv$patient_name          <<- NULL
      combined_rv$student_id            <<- NULL
      output$combined_report_ui    <<- renderUI(NULL)
      output$combined_report_output <<- renderUI(NULL)
      showNotification(tagList(icon("trash"), "已删除 / Deleted"), type = "message")
    }, error = function(e) {
      showNotification(tagList(icon("exclamation-triangle"), "删除失败 / Delete failed"), type = "error")
    })
    removeModal()
  })

  # ── Report spinner (same as CELF-5) ───────────────────
  report_phase <- reactiveVal("idle")

  output$sw_ai_spinner <- renderUI({
    if (report_phase() == "running") {
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
          tags$span(class="ai-msg", "🤖 AI 报告中，请稍候...")
        )
      )
    } else {
      NULL
    }
  })

  # ── Generate Combined Report ───────────────────────────
  observeEvent(input$btn_generate_combined, {
    showNotification(
      tagList(icon("info-circle"), "按钮点击成功，开始处理..."),
      type = "message", duration = 3
    )
    celf5_id <- input$combined_celf5_id
    slam_id  <- input$combined_slam_id
    celf5_ok <- !is.null(celf5_id) && celf5_id != ""
    slam_ok  <- !is.null(slam_id)  && slam_id  != ""

    if (!celf5_ok && !slam_ok) {
      showNotification(
        tagList(icon("exclamation-triangle"), "请至少选择一种评估 / Select at least one assessment"),
        type = "error", duration = 3
      )
      return()
    }

    report_phase("running")

    output$combined_report_output <- renderUI({
      div(style = "padding: 20px; text-align: center;",
        div(class = "spinner-border", role = "status",
            style = "width: 2rem; height: 2rem; color: #1B3A6B;"),
        br(), br(),
        strong("正在生成报告，请稍候... / Generating report...", style = "color: #1B3A6B;"),
        br(),
        span("这可能需要 30-60 秒，请勿关闭页面", style = "font-size: 13px; color: #666;"),
        br(), br(),
        uiOutput("sw_ai_spinner")
      )
    })

    tryCatch({
      source("/home/yzhang/clawfiles/celf5_shiny/slam_report.R", local = TRUE)
      slam_narrative <- generate_slam_report(
        combined_rv$student_id,
        assessment_id = ifelse(slam_ok,  as.integer(slam_id),  NA_integer_),
        celf5_id     = ifelse(celf5_ok, as.integer(celf5_id), NA_integer_)
      )

      if (celf5_ok && slam_ok) {
        report_title <- "CELF-5 + SLAM 联合临床报告"
        report_ids   <- sprintf("CELF5#%s + SLAM#%s", celf5_id, slam_id)
      } else if (celf5_ok) {
        report_title <- "CELF-5 临床报告"
        report_ids   <- sprintf("CELF5#%s", celf5_id)
      } else {
        report_title <- "SLAM 叙事报告"
        report_ids   <- sprintf("SLAM#%s", slam_id)
      }

      report_phase("idle")
      output$combined_report_output <- renderUI({
        div(class = "combined-report-container",
          style = sprintf("background: #f8f9fa; border-radius: 16px; padding: 24px; border: 2px solid %s;", celf5_gold),
          div(style = "text-align: center; margin-bottom: 20px;",
            h3(report_title, style = sprintf("color: %s; margin:0;", celf5_blue)),
            h4(sprintf("学生: %s | %s", combined_rv$patient_name, report_ids),
               style = "color: #666; font-size: 14px; margin: 8px 0 0 0;")
          ),
          hr(style = sprintf("border-color: %s;", celf5_gold)),
          div(style = "background: white; border-radius: 12px; padding: 20px; font-size: 14px; line-height: 1.8;",
            HTML(gsub("\n", "<br>", slam_narrative))
          ),
          div(style = "text-align: center; margin-top: 16px;",
            span(style = sprintf("font-size: 12px; color: %s;", celf5_blue),
                 "本报告由 AI 辅助生成，需经主试评估师审核签字后方可使用。")
          )
        )
      })
      showNotification(tagList(icon("check-circle"), "报告生成成功！"), type = "message", duration = 5)
    }, error = function(e) {
      report_phase("idle")
      output$combined_report_output <- renderUI({
        div(style = "padding: 20px;",
          div(class = "alert alert-danger", role = "alert",
              sprintf("报告生成失败 / Failed: %s", e$message))
        )
      })
      showNotification(tagList(icon("exclamation-triangle"), "报告生成失败"), type = "error", duration = 5)
    })
  })

  # ── Reactive values ─────────────────────────────────────
  combined_rv <- reactiveValues(
    celf5_assessments      = NULL,
    slam_assessments       = NULL,
    patient_name           = NULL,
    student_id             = NULL,
    selected_patient_id    = NULL,
    selected_patient_name  = NULL,
    selected_celf5_count   = 0,
    selected_slam_count    = 0
  )
}

# ─────────────────────────────────────────────────────────────
# Run App
# ─────────────────────────────────────────────────────────────
shinyApp(ui = ui, server = server)
