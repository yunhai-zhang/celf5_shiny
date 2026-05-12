# app.R — SLP (Shared Landing Page)
# 入口：CELF-5 | SLAM | AI Report
# 三大评估统一入口，AI Report 在会话内完成

library(shiny)
library(bslib)
library(dplyr)
library(RSQLite)
library(lubridate)
library(stringr)

# ─────────────────────────────────────────────────────────────
# 加载 global.R 中的所有共享函数和常量
# ─────────────────────────────────────────────────────────────
source("/home/yzhang/clawfiles/celf5_shiny/global.R")

# ─────────────────────────────────────────────────────────────
# 配色常量
# ─────────────────────────────────────────────────────────────
celf5_blue  <- "#1B3A6B"
celf5_gold  <- "#C8A951"

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
      .badge-ai     { background: #e8f5e9; color: #2e7d32; }
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
        border: 1px solid #e0e4ee;
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
      .form-control {
        border-radius: 8px;
        border: 1.5px solid #d0d7e2;
        padding: 10px 14px;
        font-size: 14px;
        transition: border-color 0.2s ease;
      }
      .form-control:focus {
        border-color: %s;
        box-shadow: 0 0 0 3px rgba(27,58,107,0.1);
      }
      .form-label {
        font-size: 13px;
        font-weight: 600;
        color: %s;
        margin-bottom: 5px;
      }
      @media (max-width: 768px) {
        .cards-row { flex-direction: column; }
        .entry-card { min-height: auto; }
        .home-hero h1 { font-size: 26px; }
      }
    ",
    celf5_blue, celf5_gold,    # hero gradient
    celf5_gold,                # card hover
    celf5_blue,                # entry title
    celf5_blue,                # btn-celf5
    celf5_blue, celf5_blue,   # btn-slam
    celf5_gold,                # btn-slam hover
    celf5_blue,                # badge-celf5
    celf5_blue,                # footer brand
    celf5_blue,                # student panel header
    celf5_blue,                # form focus
    celf5_blue                 # form label
    )))
  ),

  div(class = "main-container",

    # ── Hero ──────────────────────────────────────────────────
    div(class = "home-hero",
      h1("CELF-5 2.0 共同评估平台"),
      p(class = "subtitle",
        "CELF-5 2.0 Integrated Assessment Platform",
        br(),
        span(class = "gold-accent", "三大评估入口  |  Unified Assessment Hub")
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
        fluidRow(
          column(3,
            div(class = "form-group",
              tags$label(class = "form-label", "学生ID / Student ID *"),
              textInput("ai_patient_id", NULL, placeholder = "输入学生ID")
            )
          ),
          column(3,
            div(class = "form-group",
              tags$label(class = "form-label", "选择评估 / Select Assessment"),
              uiOutput("ai_assessment_selector")
            )
          ),
          column(2,
            div(class = "form-group",
              tags$label(class = "form-label", "语言 / Language"),
              selectInput("report_lang", NULL,
                choices = c("中文" = "zh", "English" = "en"),
                selected = "zh", width = "100%")
            )
          ),
          column(2,
            div(class = "form-group",
              tags$label(class = "form-label", " "),
              actionButton("btn_gen_narrative", "生成报告 / Generate",
                icon = icon("brain"), class = "btn btn-primary",
                style = sprintf("margin-top: 18px; width: 100%%; background: %s; border: none;", celf5_blue),
                width = "100%")
            )
          ),
          column(2,
            div(class = "form-group",
              tags$label(class = "form-label", "状态 / Status"),
              uiOutput("narrative_status", style = "padding-top: 22px;")
            )
          )
        ),
        fluidRow(
          column(12,
            uiOutput("narrative_preview")
          )
        )
      )
    ),

    # ── Footer ────────────────────────────────────────────────
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

  # ── Reactive values ────────────────────────────────────────
  rv <- reactiveValues(
    patient_id = NULL,
    assessment_id = NULL
  )

  # ── Patient/Assessment selector ─────────────────────────────
  observeEvent(input$ai_patient_id, {
    pid <- trim(input$ai_patient_id)
    if (pid == "" || is.na(as.integer(pid))) {
      rv$patient_id <- NULL
      rv$assessment_id <- NULL
      return()
    }
    pid_num <- as.integer(pid)
    con <- get_con()
    on.exit(dbDisconnect(con))
    name <- dbGetQuery(con, "SELECT name FROM patients WHERE id = ?",
                       params = list(pid_num))$name
    if (length(name) == 0 || is.na(name)) {
      rv$patient_id <- NULL
      return()
    }
    rv$patient_id <- pid_num
    rv$assessment_id <- NULL
  })

  output$ai_assessment_selector <- renderUI({
    req(rv$patient_id)
    con <- get_con()
    on.exit(dbDisconnect(con))
    assessments <- dbGetQuery(con,
      sprintf("SELECT id, assessment_date, age_group FROM assessments
              WHERE patient_id = %d ORDER BY assessment_date DESC",
              rv$patient_id))
    if (nrow(assessments) == 0) {
      return(tags$span(style = "color:#aaa;font-size:13px;", "无评估记录"))
    }
    opts <- setNames(assessments$id,
                     paste0("#", assessments$id, " (", assessments$assessment_date, ")"))
    selectInput("ai_assessment_id", NULL, choices = c("— 选择评估 —" = "", opts),
                width = "100%")
  })

  observeEvent(input$ai_assessment_id, {
    rv$assessment_id <- as.integer(input$ai_assessment_id)
  })

  # ── AI 临床叙事报告 (3-case: SLAM only / CELF5 only / both) ─
  report_phase <- reactiveVal("idle")

  output$narrative_status <- renderUI({ NULL })
  output$narrative_preview <- renderUI({ NULL })

  # Spinner CSS (static, injected once)
  tags$style(HTML("
    @keyframes ai-spin { to { transform: rotate(360deg); } }
    .ai-spin { width:18px; height:18px; border:2px solid #dee2e6;
               border-top:2px solid #1B3A6B; border-radius:50%%;
               display:inline-block; animation:ai-spin 0.7s linear infinite; }
    .ai-msg  { display:inline; margin-left:8px; color:#6c757d; }
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
  "))

  # ── Load saved CELF5 + SLAM assessments for patient ───────────
  output$ai_assessment_selector <- renderUI({
    req(rv$patient_id)
    pid <- rv$patient_id
    con <- get_con()
    on.exit(dbDisconnect(con))

    # CELF-5 assessments
    celf5_ass <- dbGetQuery(con, sprintf(
      "SELECT id, strftime('%%Y-%%m-%%d', assessment_date) as date_str,
              assessment_type FROM assessments
       WHERE patient_id = %d AND assessment_type = 'CELF5'
       ORDER BY assessment_date DESC", pid))

    # SLAM assessments
    slam_ass <- dbGetQuery(con, sprintf(
      "SELECT id, strftime('%%Y-%%m-%%d', assessment_date) as date_str,
              assessment_type FROM assessments
       WHERE patient_id = %d AND assessment_type = 'SLAM'
       ORDER BY assessment_date DESC", pid))

    tagList(
      fluidRow(
        column(6,
          tags$label(class = "form-label", "CELF-5 评估 / CELF-5 Assessment"),
          if (nrow(celf5_ass) == 0) {
            tags$span(style = "color:#aaa;font-size:13px;", "无 CELF-5 记录")
          } else {
            lapply(seq_len(nrow(celf5_ass)), function(i) {
              div(
                style = "display:flex;align-items:center;gap:8px;margin-bottom:6px;",
                tags$input(type = "checkbox", class = "form-check-input",
                           id = paste0("celf5_chk_", celf5_ass$id[i]),
                           value = as.character(celf5_ass$id[i])),
                tags$label(class = "form-check-label", style = "font-size:13px;",
                           paste0("CELF5 #", celf5_ass$id[i], " (", celf5_ass$date_str[i], ")"))
              )
            })
          }
        ),
        column(6,
          tags$label(class = "form-label", "SLAM 评估 / SLAM Assessment"),
          if (nrow(slam_ass) == 0) {
            tags$span(style = "color:#aaa;font-size:13px;", "无 SLAM 记录")
          } else {
            lapply(seq_len(nrow(slam_ass)), function(i) {
              div(
                style = "display:flex;align-items:center;gap:8px;margin-bottom:6px;",
                tags$input(type = "checkbox", class = "form-check-input",
                           id = paste0("slam_chk_", slam_ass$id[i]),
                           value = as.character(slam_ass$id[i])),
                tags$label(class = "form-check-label", style = "font-size:13px;",
                           paste0("SLAM #", slam_ass$id[i], " (", slam_ass$date_str[i], ")"))
              )
            })
          }
        )
      )
    )
  })

  observeEvent(input$ai_patient_id, {
    rv$assessment_id <- NULL
  })

  observeEvent(input$btn_gen_narrative, {
    pid <- rv$patient_id
    if (is.null(pid)) {
      showNotification(tagList(icon("exclamation-triangle"), "请先输入学生ID"),
                      type = "error", duration = 4)
      return()
    }

    # Collect selected CELF5 and SLAM IDs
    celf5_ids <- grep("^celf5_chk_", names(input), value = TRUE)
    slam_ids  <- grep("^slam_chk_",  names(input), value = TRUE)

    has_celf5 <- length(celf5_ids) > 0
    has_slam  <- length(slam_ids)  > 0

    if (!has_celf5 && !has_slam) {
      showNotification(tagList(icon("exclamation-triangle"),
                               "请至少选择一个 CELF-5 或 SLAM 评估"),
                       type = "error", duration = 4)
      return()
    }

    celf5_id <- if (has_celf5) as.integer(input[[celf5_ids[1]]]) else NULL
    slam_id  <- if (has_slam)  as.integer(input[[slam_ids[1]]])  else NULL
    lang     <- input$report_lang %||% "zh"

    report_phase("generating")
    output$narrative_status <- renderUI({
      tags$div(style = "margin-top:6px",
        tags$span(class = "ai-spin"),
        tags$span(class = "ai-msg", "🤖 正在生成报告，请稍候...")
      )
    })
    output$narrative_preview <- renderUI({ NULL })

    later::later(function() {
      tryCatch({
        source("/home/yzhang/clawfiles/celf5_shiny/slam_report.R", local = TRUE)
        narrative <- generate_slam_report(
          student_id    = pid,
          assessment_id = slam_id,
          celf5_id      = celf5_id,
          lang          = lang
        )
        narrative <- gsub("\n", "<br>", narrative)
        report_phase("idle")

        output$narrative_status <- renderUI({
          div(class = "alert alert-success mb-0", role = "alert",
            icon("check-circle"),
            if (lang == "zh") "报告已生成！" else "Report generated! ",
            actionLink("btn_regen_narrative",
              if (lang == "zh") " 重新生成" else " Regenerate",
              icon = icon("refresh"), class = "btn btn-sm btn-outline-success"))
        })

        output$narrative_preview <- renderUI({
          div(class = "card mb-3",
            div(class = "card-header d-flex justify-content-between align-items-center",
              strong(if (lang == "zh") "临床叙事报告 / Clinical Narrative Report" else "Clinical Narrative Report"),
              span(class = "badge bg-secondary", "AI")),
            div(class = "card-body p-0",
              div(class = "ai-report-card", HTML(narrative))))
        })
      }, error = function(e) {
        report_phase("error")
        output$narrative_status <- renderUI({
          div(class = "alert alert-danger mb-0", role = "alert",
            icon("exclamation-triangle"),
            if (lang == "zh") "生成失败: " else "Generation failed: ",
            e$message)
        })
        cat(file = stderr(), "[narrative error]", e$message, "\n")
      })
    })
  })
}

shinyApp(ui, server)
