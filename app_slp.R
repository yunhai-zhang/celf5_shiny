# app.R — SLP AI Report Platform (Complete Version)
# Features: DT Student Table + CELF5/SLAM Checkbox Selectors + 3-case AI Report
# Canonical: /srv/shiny-server/slp/app.R

library(shiny)
library(bslib)
library(dplyr)
library(DT)
library(RSQLite)
library(lubridate)
library(stringr)

# ─────────────────────────────────────────────────────────────
# 加载 global.R 和 slam_report.R
# ─────────────────────────────────────────────────────────────
source("/home/yzhang/clawfiles/celf5_shiny/global.R")
source("/home/yzhang/clawfiles/celf5_shiny/slam_report.R", local = TRUE)

# ─────────────────────────────────────────────────────────────
# 配色常量
# ─────────────────────────────────────────────────────────────
celf5_blue <- "#1B3A6B"
celf5_gold <- "#C8A951"

# ─────────────────────────────────────────────────────────────
# UI
# ─────────────────────────────────────────────────────────────
ui <- fluidPage(
  theme = bslib::bs_theme(version = 5, primary = celf5_blue, secondary = celf5_gold),

  tags$head(
    tags$style(HTML(sprintf("
      body {
        background: linear-gradient(135deg, #f8f9fa 0%%, #e8ecf3 100%%);
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
        background: linear-gradient(135deg, %s 0%%, #2a5ab3 100%%);
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
        color: %s;
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
        border-color: %s;
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
        color: %s;
        margin-bottom: 12px;
      }
      .entry-desc {
        font-size: 14px;
        color: #666;
        line-height: 1.7;
        margin-bottom: 20px;
        flex-grow: 1;
      }
      .entry-badge {
        display: inline-block;
        padding: 5px 16px;
        border-radius: 20px;
        font-size: 12px;
        font-weight: 600;
        margin-bottom: 16px;
      }
      .badge-celf5  { background: #e8f0fe; color: %s; }
      .badge-slam   { background: #fff3e0; color: #e65100; }
      .entry-btn {
        display: inline-block;
        padding: 10px 28px;
        border-radius: 25px;
        font-size: 14px;
        font-weight: 600;
        transition: all 0.2s ease;
      }
      .btn-celf5 {
        background: %s;
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
        color: %s;
        border: 2px solid %s;
      }
      .btn-slam:hover {
        background: %s;
        color: white;
      }
      .home-footer {
        text-align: center;
        margin-top: 40px;
        padding: 20px;
        color: #aaa;
        font-size: 13px;
      }
      .footer-brand { color: %s; font-weight: 600; }
      /* Student Info Panel */
      .student-panel {
        background: white;
        border-radius: 18px;
        border: 1px solid #e0e4ef;
        box-shadow: 0 4px 16px rgba(0,0,0,0.06);
        overflow: hidden;
      }
      .student-panel-header {
        background: linear-gradient(135deg, %s 0%%, #2a5ab3 100%%);
        color: white;
        padding: 18px 28px;
        font-size: 18px;
        font-weight: 600;
        display: flex;
        align-items: center;
        gap: 10px;
      }
      .student-panel-body { padding: 28px; }
      .form-group {
        border-radius: 8px;
        border: 1.5px solid #d0d7e2;
        padding: 10px 14px;
        font-size: 14px;
        transition: border-color 0.2s ease;
      }
      .form-group:focus-within {
        border-color: %s;
        box-shadow: 0 0 0 3px rgba(27,58,107,0.1);
      }
      .form-label {
        font-size: 13px;
        font-weight: 600;
        color: %s;
        margin-bottom: 5px;
      }
      /* Assessment Checkboxes */
      .assessment-checkboxes {
        display: flex;
        gap: 16px;
        margin-top: 8px;
      }
      .assessment-chk {
        display: flex;
        align-items: center;
        gap: 6px;
        font-size: 13px;
        cursor: pointer;
      }
      .assessment-chk input[type='checkbox'] {
        width: 16px;
        height: 16px;
        cursor: pointer;
      }
      /* Student Selected Info */
      .student-selected-info {
        background: #f8f9fa;
        border-radius: 10px;
        padding: 14px 18px;
        margin-bottom: 18px;
        font-size: 14px;
        border-left: 4px solid %s;
      }
      .student-selected-info .student-name {
        font-weight: 700;
        color: %s;
        font-size: 16px;
      }
      .student-selected-info .student-meta {
        color: #666;
        font-size: 13px;
        margin-top: 2px;
      }
      /* Spinner & Report Card */
      .ai-spin { width:18px; height:18px; border:2px solid #dee2e6;
                  border-top:2px solid %s; border-radius:50%%;
                  display:inline-block; animation:ai-spin 0.7s linear infinite; }
      @keyframes ai-spin { to { transform: rotate(360deg); } }
      .ai-msg  { display:inline; margin-left:8px; color:#6c757d; }
      .ai-report-card { background:#fafafa; border:1px solid #e9ecef;
                        border-radius:8px; padding:20px 24px; margin-top:12px;
                        font-size:14px; line-height:1.75; max-height:600px;
                        overflow-y:auto; }
      .ai-report-card h1,.ai-report-card h2,.ai-report-card h3 { color:%s; margin-top:14px; }
      .ai-report-card h1:first-child,.ai-report-card h2:first-child { margin-top:0; }
      .ai-report-card ul,.ai-report-card ol { padding-left:22px; }
      .ai-report-card li { margin-bottom:5px; }
      .ai-report-card strong { color:%s; }
      .ai-report-card em { color:#666; font-style:italic; }
      .ai-report-card hr { border-top:1px solid #ddd; margin:12px 0; }
      /* DT table styling */
      .dataTables_wrapper {
        font-size: 13px;
      }
      .student-dt-table {
        border-radius: 10px;
        overflow: hidden;
        border: 1px solid #e0e4ef;
      }
      .student-dt-table thead {
        background: %s;
        color: white;
      }
      .student-dt-table thead th {
        font-weight: 600;
        border: none;
        padding: 12px 14px;
      }
      .student-dt-table tbody tr:hover {
        background: #f0f4ff !important;
      }
      .student-dt-table tbody tr.selected {
        background: #dce8ff !important;
      }
      .dt-btn-select {
        background: %s;
        color: white;
        border: none;
        border-radius: 16px;
        padding: 4px 14px;
        font-size: 12px;
        font-weight: 600;
        cursor: pointer;
        transition: background 0.2s;
      }
      .dt-btn-select:hover {
        background: #1452a3;
      }
      /* Responsive */
      @media (max-width: 768px) {
        .cards-row { flex-direction: column; }
        .entry-card { min-height: auto; }
        .home-hero h1 { font-size: 26px; }
      }
    ",
    celf5_blue, celf5_gold,     # hero gradient / gold-accent
    celf5_gold,                 # card hover
    celf5_blue,                 # entry title
    celf5_blue,                 # badge-celf5
    celf5_blue,                 # btn-celf5
    celf5_gold, celf5_gold, celf5_gold,  # btn-slam / btn-slam hover
    celf5_blue,                 # footer brand
    celf5_blue,                 # student panel header
    celf5_blue,                 # form focus
    celf5_blue,                 # form label
    celf5_blue,                 # student-selected border
    celf5_blue,                 # student-name
    celf5_blue,                 # spinner
    celf5_blue, celf5_blue,    # report card headings / strong
    celf5_blue,                 # DT thead
    celf5_blue                  # dt-btn-select
    )))
  ),

  div(class = "main-container",

    # ── Hero ──────────────────────────────────────────────────
    div(class = "home-hero",
      h1("SLP 评估平台"),
      p(class = "subtitle",
        "SLP Assessment Platform",
        br(),
        span(class = "gold-accent", "CELF-5 | SLAM | AI Report  三大评估统一入口")
      )
    ),

    # ── Entry Cards ───────────────────────────────────────────
    div(class = "cards-row",

      # Card 1: CELF-5
      div(class = "entry-card",
        tags$a(href = "http://www.zhangyunhai.com:3838/celf5",
          span(class = "entry-icon", "📋"),
          div(class = "entry-title", "CELF-5"),
          div(class = "entry-desc",
            "语言评估基础工具",
            br(),
            "Language Assessment",
            br(), br(),
            "核心语言分数、复合量表分、临床叙事报告"
          ),
          span(class = "entry-badge badge-celf5", "进行中 / In Use"),
          span(class = "entry-btn btn-celf5", "进入评估  ›")
        )
      ),

      # Card 2: SLAM
      div(class = "entry-card",
        tags$a(href = "http://www.zhangyunhai.com:3838/slam",
          span(class = "entry-icon", "📖"),
          div(class = "entry-title", "SLAM"),
          div(class = "entry-desc",
            "叙事评估工具",
            br(),
            "Narrative Assessment",
            br(), br(),
            "图片叙事情境，评估叙事能力与语用"
          ),
          span(class = "entry-badge badge-slam", "进行中 / In Use"),
          span(class = "entry-btn btn-slam", "进入评估  ›")
        )
      )
    ),

    # ── AI Report Panel ───────────────────────────────────────
    div(class = "student-panel",
      div(class = "student-panel-header",
        span("🤖"), "AI 报告 / AI Report"
      ),
      div(class = "student-panel-body",

        # Step 1: DT Student Table
        fluidRow(
          column(12,
            tags$h5("第一步：选择学生 / Step 1: Select Student", 
                    style = sprintf("color:%s; font-weight:700; margin-bottom:12px;", celf5_blue)),
            div(class = "student-dt-table",
              DT::dataTableOutput("student_dt")
            )
          )
        ),

        # Step 2: Student Info + Assessment Selection
        fluidRow(
          column(6,
            uiOutput("student_selected_ui")
          ),
          column(6,
            uiOutput("assessment_checkboxes_ui")
          )
        ),

        # Step 3: Generate Button + Language
        fluidRow(
          column(4,
            div(class = "form-group",
              tags$label(class = "form-label", "语言 / Language"),
              selectInput("report_lang", NULL,
                choices = c("中文" = "zh", "English" = "en"),
                selected = "zh", width = "100%")
            )
          ),
          column(4,
            div(class = "form-group",
              tags$label(class = "form-label", " "),
              actionButton("btn_gen_narrative", "生成报告 / Generate Report",
                icon = icon("brain"), class = "btn btn-primary",
                style = sprintf("margin-top: 18px; width: 100%%; background: %s; border: none;", celf5_blue),
                width = "100%")
            )
          ),
          column(4,
            div(class = "form-group",
              tags$label(class = "form-label", "状态 / Status"),
              uiOutput("narrative_status", style = "padding-top: 22px;")
            )
          )
        ),

        # Step 4: Report Output
        fluidRow(
          column(12,
            uiOutput("narrative_preview")
          )
        )
      )
    ),

    # ── Footer ────────────────────────────────────────────────
    div(class = "home-footer",
      span(class = "footer-brand", "SLP 评估平台"), " © 2026  |  Powered by Shiny + bslib  |  ",
      "如需帮助请联系评估系统管理员"
    )
  )
)

# ─────────────────────────────────────────────────────────────
# Server
# ─────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  # ── Reactive values ────────────────────────────────────────
  rv <- reactiveValues(
    patient_id   = NULL,
    patient_name = NULL,
    celf5_ids    = character(),
    slam_ids     = character()
  )

  # ─────────────────────────────────────────────────────────────
  # Render DT Student Table
  # ─────────────────────────────────────────────────────────────
  output$student_dt <- DT::renderDataTable({
    con <- get_con()
    on.exit(dbDisconnect(con))

    # Get all patients with their latest assessment info
    patients <- dbGetQuery(con, "
      SELECT p.id, p.name, p.class, p.teacher_id,
             (SELECT COUNT(*) FROM assessments a WHERE a.patient_id = p.id) as n_assessments
      FROM patients p
      ORDER BY p.id DESC
    ")

    if (nrow(patients) == 0) {
      return(data.frame(
        ID = integer(),
        Name = character(),
        Class = character(),
        `Teacher ID` = character(),
        Assessments = integer()
      ))
    }

    # Build display table
    dt_df <- data.frame(
      ID           = patients$id,
      Name         = patients$name,
      Class        = ifelse(is.na(patients$class), "-", patients$class),
      `Teacher ID` = ifelse(is.na(patients$teacher_id), "-", patients$teacher_id),
      Assessments  = patients$n_assessments,
      stringsAsFactors = FALSE
    )

    # Use callback to add button
    DT::datatable(
      dt_df,
      selection   = 'single',
      rownames    = FALSE,
      escape      = FALSE,   # Allow HTML in cells
      options     = list(
        pageLength  = 10,
        lengthMenu  = c(10, 25, 50),
        ordering    = TRUE,
        class       = "stripe hover",
        language    = list(
          search      = "搜索 / Search:",
          lengthMenu   = "显示 _MENU_ 条",
          info        = "显示第 _START_ 至 _END_ 条，共 _TOTAL_ 条",
          paginate    = list(first = "首页", previous = "‹", `next` = "›", last = "末页"),
          zeroRecords = "无匹配学生 / No matching students"
        ),
        columnDefs  = list(
          list(className = 'dt-center', targets = c(0, 2, 3, 4)),
          list(visible  = FALSE, targets  = 0)  # hide ID column, use data instead
        )
      )
    )
  })

  # ─────────────────────────────────────────────────────────────
  # When a DT row is selected → load patient + assessments
  # ─────────────────────────────────────────────────────────────
  observeEvent(input$student_dt_rows_selected, {
    selected_row <- input$student_dt_rows_selected
    if (length(selected_row) == 0) {
      rv$patient_id   <- NULL
      rv$patient_name <- NULL
      rv$celf5_ids    <- character()
      rv$slam_ids     <- character()
      return()
    }

    con <- get_con()
    on.exit(dbDisconnect(con))

    # Get patient info
    patients <- dbGetQuery(con, "
      SELECT p.id, p.name, p.class, p.teacher_id
      FROM patients p
      ORDER BY p.id DESC
    ")

    pid      <- as.integer(patients$id[selected_row])
    pname    <- patients$name[selected_row]
    pclass   <- patients$class[selected_row]
    pteacher <- patients$teacher_id[selected_row]

    # Get CELF-5 and SLAM assessments
    celf5_df <- dbGetQuery(con, sprintf(
      "SELECT id, strftime('%%Y-%%m-%%d', assessment_date) as date_str,
              assessment_type, status
       FROM assessments
       WHERE patient_id = %d AND assessment_type = 'CELF5'
       ORDER BY assessment_date DESC", pid))

    slam_df <- dbGetQuery(con, sprintf(
      "SELECT id, strftime('%%Y-%%m-%%d', assessment_date) as date_str,
              assessment_type, status
       FROM assessments
       WHERE patient_id = %d AND assessment_type = 'SLAM'
       ORDER BY assessment_date DESC", pid))

    rv$patient_id   <- pid
    rv$patient_name <- pname
    rv$celf5_ids    <- as.character(celf5_df$id)
    rv$slam_ids     <- as.character(slam_df$id)
  })

  # ─────────────────────────────────────────────────────────────
  # Student Selected Info UI
  # ─────────────────────────────────────────────────────────────
  output$student_selected_ui <- renderUI({
    if (is.null(rv$patient_id)) {
      return(tags$div(
        class = "student-selected-info",
        style = "border-left-color: #ccc; color: #999; text-align: center; padding: 20px;",
        "请在上方表格中选择学生 / Please select a student from the table above"
      ))
    }

    con <- get_con()
    on.exit(dbDisconnect(con))

    pclass   <- dbGetQuery(con, sprintf("SELECT class FROM patients WHERE id = %d", rv$patient_id))$class
    pteacher <- dbGetQuery(con, sprintf("SELECT teacher_id FROM patients WHERE id = %d", rv$patient_id))$teacher_id
    if (is.na(pclass))   pclass   <- "-"
    if (is.na(pteacher))  pteacher <- "-"

    celf5_count <- length(rv$celf5_ids)
    slam_count  <- length(rv$slam_ids)

    tags$div(class = "student-selected-info",
      div(class = "student-name", paste("🎓", rv$patient_name)),
      div(class = "student-meta",
        paste0("ID: ", rv$patient_id, "  |  Class: ", pclass,
               "  |  Teacher: ", pteacher, "  |  ",
               "CELF-5: ", celf5_count, "次  |  SLAM: ", slam_count, "次"))
    )
  })

  # ─────────────────────────────────────────────────────────────
  # Assessment Checkboxes UI
  # ─────────────────────────────────────────────────────────────
  output$assessment_checkboxes_ui <- renderUI({
    if (is.null(rv$patient_id)) {
      return(tags$div(style = "color: #999; font-size: 13px; margin-top: 20px;",
                      "选择学生后显示评估选项"))
    }

    celf5_count <- length(rv$celf5_ids)
    slam_count  <- length(rv$slam_ids)

    tagList(
      tags$div(style = sprintf("margin-bottom: 12px; color: %s; font-weight: 600; font-size: 14px;", celf5_blue),
               "选择评估类型 / Select Assessment Type"),

      if (celf5_count == 0) {
        tags$div(style = "color: #999; font-size: 13px;", "⚪ 无 CELF-5 评估记录")
      } else {
        tags$div(class = "assessment-checkboxes",
          tags$div(class = "assessment-chk",
            checkboxInput("chk_celf5", paste0("CELF-5 (", celf5_count, "次评估)"),
                          value = TRUE, width = NULL),
            if (celf5_count > 0) {
              # Show first CELF-5 as selected by default
              tagList(
                tags$input(type = "hidden", id = "celf5_selected_id",
                           value = rv$celf5_ids[1])
              )
            }
          )
        )
      },

      if (slam_count == 0) {
        tags$div(style = "color: #999; font-size: 13px; margin-top: 8px;", "⚪ 无 SLAM 评估记录")
      } else {
        tags$div(class = "assessment-checkboxes", style = "margin-top: 8px;",
          tags$div(class = "assessment-chk",
            checkboxInput("chk_slam", paste0("SLAM (", slam_count, "次评估)"),
                          value = TRUE, width = NULL),
            if (slam_count > 0) {
              tagList(
                tags$input(type = "hidden", id = "slam_selected_id",
                           value = rv$slam_ids[1])
              )
            }
          )
        )
      },

      if (celf5_count == 0 && slam_count == 0) {
        tags$div(style = "color: #e00; font-size: 13px; margin-top: 10px;",
                 "⚠️ 该学生暂无任何评估记录")
      }
    )
  })

  # ─────────────────────────────────────────────────────────────
  # AI 临床叙事报告 (3-case: SLAM only / CELF5 only / both)
  # ─────────────────────────────────────────────────────────────
  report_phase <- reactiveVal("idle")

  output$narrative_status <- renderUI({ NULL })
  output$narrative_preview <- renderUI({ NULL })

  observeEvent(input$btn_gen_narrative, {
    pid <- rv$patient_id
    if (is.null(pid)) {
      showNotification(tags$div(
        icon("exclamation-triangle"), " 请先在表格中选择学生 / Please select a student first"
      ), type = "error", duration = 4)
      return()
    }

    has_celf5 <- isTRUE(input$chk_celf5) && length(rv$celf5_ids) > 0
    has_slam  <- isTRUE(input$chk_slam)  && length(rv$slam_ids)  > 0

    if (!has_celf5 && !has_slam) {
      showNotification(tags$div(
        icon("exclamation-triangle"),
        " 请至少选择一个评估类型（CELF-5 或 SLAM）"
      ), type = "error", duration = 4)
      return()
    }

    celf5_id <- if (has_celf5) as.integer(rv$celf5_ids[1]) else NULL
    slam_id  <- if (has_slam)  as.integer(rv$slam_ids[1])  else NULL
    lang     <- input$report_lang %||% "zh"

    report_phase("generating")
    output$narrative_status <- renderUI({
      tags$div(style = "margin-top:6px",
        tags$span(class = "ai-spin"),
        tags$span(class = "ai-msg",
          if (lang == "zh") "正在生成报告，请稍候..." else "Generating report, please wait...")
      )
    })
    output$narrative_preview <- renderUI({ NULL })

    later::later(function() {
      tryCatch({
        narrative <- generate_slam_report(
          student_id     = pid,
          assessment_id  = slam_id,
          celf5_id      = celf5_id,
          lang           = lang
        )
        narrative <- gsub("\n", "<br>", narrative)
        report_phase("idle")

        output$narrative_status <- renderUI({
          tags$div(class = "alert alert-success mb-0", role = "alert",
            icon("check-circle"),
            if (lang == "zh") " 报告生成完成！" else " Report generated! ",
            actionLink("btn_regen_narrative",
              if (lang == "zh") "重新生成" else "Regenerate",
              icon = icon("refresh"), class = "btn btn-sm btn-outline-success",
              style = "margin-left: 12px;")
          )
        })

        output$narrative_preview <- renderUI({
          div(class = "card mb-3",
            div(class = "card-header d-flex justify-content-between align-items-center",
              strong(if (lang == "zh")
                "AI 临床叙事报告 / Clinical Narrative Report"
                else "AI Clinical Narrative Report"),
              span(class = "badge bg-secondary", "AI")
            ),
            div(class = "card-body p-0",
              div(class = "ai-report-card", HTML(narrative))
            )
          )
        })
      }, error = function(e) {
        report_phase("error")
        output$narrative_status <- renderUI({
          tags$div(class = "alert alert-danger mb-0", role = "alert",
            icon("exclamation-triangle"),
            if (lang == "zh") " 生成失败: " else " Generation failed: ",
            e$message)
        })
        cat(file = stderr(), "[narrative error]", e$message, "\n")
      })
    }, 0.1)
  })

  # Regenerate
  observeEvent(input$btn_regen_narrative, {
    session$sendCustomMessage("resetGenerateBtn", list())
  })
}

shinyApp(ui, server)
