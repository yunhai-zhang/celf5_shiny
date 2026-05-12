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
library(shinyjs)

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
    tags$style(HTML(paste0(
    "body { background: #e8ecf3 !important; font-family: 'Segoe UI', Arial, sans-serif; min-height: 100vh; }",
    ".main-container { max-width: 1100px; margin: 0 auto; padding: 32px 20px; }",
    ".home-hero { background: linear-gradient(135deg, #1B3A6B 0%, #2a5ab3 100%) !important; color: white !important; border-radius: 20px; padding: 56px 40px; margin-bottom: 40px; text-align: center; box-shadow: 0 12px 40px rgba(27,58,107,0.3); }",
    ".home-hero h1 { color: #1B3A6B !important; font-size: 36px; font-weight: 700; margin-bottom: 10px; text-shadow: 0 1px 3px rgba(255,255,255,0.8); }",
    ".home-hero .subtitle { color: rgba(255,255,255,0.85) !important; font-size: 16px; margin: 0; letter-spacing: 0.5px; }",
    ".home-hero .gold-accent { color: #C8A951 !important; font-weight: 600; }",
    ".cards-row { display: flex; gap: 24px; margin-bottom: 36px; }",
    ".entry-card { background: white !important; flex: 1; border-radius: 18px; border: 2px solid #e8eaf0; padding: 32px 24px; text-align: center; transition: all 0.3s ease; cursor: pointer; text-decoration: none; display: flex; flex-direction: column; align-items: center; min-height: 320px; }",
    ".entry-card:hover { border-color: #C8A951 !important; box-shadow: 0 10px 32px rgba(27,58,107,0.18); transform: translateY(-4px); text-decoration: none; }",
    ".entry-card:active { transform: translateY(-1px); }",
    ".entry-icon { font-size: 56px; margin-bottom: 18px; display: block; }",
    ".entry-title { font-size: 22px; font-weight: 700; color: #1B3A6B !important; margin-bottom: 12px; }",
    ".entry-desc { font-size: 14px; color: #666 !important; line-height: 1.7; margin-bottom: 20px; flex-grow: 1; }",
    ".entry-badge { display: inline-block; padding: 5px 16px; border-radius: 20px; font-size: 12px; font-weight: 600; margin-bottom: 16px; }",
    ".badge-celf5  { background: #e8f0fe !important; color: #1B3A6B !important; }",
    ".badge-slam   { background: #fff3e0 !important; color: #e65100 !important; }",
    ".entry-btn { display: inline-block; padding: 10px 28px; border-radius: 25px; font-size: 14px; font-weight: 600; transition: all 0.2s ease; color: #1B3A6B !important; }",
    ".btn-celf5 { background: #1B3A6B !important; color: white !important; border: none; }",
    ".btn-celf5:hover { background: #1452a3 !important; color: white !important; box-shadow: 0 4px 12px rgba(27,58,107,0.35); }",
    ".btn-slam { background: white !important; color: #C8A951 !important; border: 2px solid #C8A951 !important; }",
    ".btn-slam:hover { background: #C8A951 !important; color: #1B3A6B !important; }",
    ".home-footer { text-align: center; margin-top: 40px; padding: 20px; color: #666 !important; font-size: 13px; }",
    ".footer-brand { color: #1B3A6B !important; font-weight: 600; }",
    ".student-panel { background: white !important; border-radius: 18px; border: 1px solid #e0e4ef; box-shadow: 0 4px 16px rgba(0,0,0,0.06); overflow: hidden; }",
    ".student-panel-header { background: linear-gradient(135deg, #1B3A6B 0%, #2a5ab3 100%) !important; color: white !important; padding: 18px 28px; font-size: 18px; font-weight: 600; display: flex; align-items: center; gap: 10px; }",
    ".student-panel-body { padding: 28px; }",
    ".form-group { border-radius: 8px; border: 1.5px solid #d0d7e2; padding: 10px 14px; font-size: 14px; transition: border-color 0.2s ease; }",
    ".form-group:focus-within { border-color: #1B3A6B !important; box-shadow: 0 0 0 3px rgba(27,58,107,0.1); }",
    ".form-label { font-size: 13px; font-weight: 600; color: #1B3A6B !important; margin-bottom: 5px; }",
    ".assessment-checkboxes { display: flex; gap: 16px; margin-top: 8px; }",
    ".assessment-chk { display: flex; align-items: center; gap: 6px; font-size: 13px; cursor: pointer; }",
    ".assessment-chk input[type='checkbox'] { width: 16px; height: 16px; cursor: pointer; }",
    ".student-selected-info { background: #f8f9fa !important; border-radius: 10px; padding: 14px 18px; margin-bottom: 18px; font-size: 14px; border-left: 4px solid #1B3A6B !important; }",
    ".student-selected-info .student-name { font-weight: 700; color: #1B3A6B !important; font-size: 16px; }",
    ".student-selected-info .student-meta { color: #444 !important; font-size: 13px; margin-top: 2px; }",
    ".new-student-form input::placeholder { color: #999 !important; opacity: 1; }",
    ".new-student-form select option[value=''] { color: #999 !important; }",
    ".ai-spin { width:18px; height:18px; border:2px solid #dee2e6; border-top:2px solid #1B3A6B; border-radius:50%; display:inline-block; animation:ai-spin 0.7s linear infinite; }",
    "@keyframes ai-spin { to { transform: rotate(360deg); } }",
    ".ai-msg  { display:inline; margin-left:8px; color:#6c757d; }",
    ".ai-report-card { background:#fafafa !important; border:1px solid #e9ecef; border-radius:8px; padding:20px 24px; margin-top:12px; font-size:14px; line-height:1.75; max-height:600px; overflow-y:auto; }",
    ".ai-report-card h1,.ai-report-card h2,.ai-report-card h3 { color:#1B3A6B !important; margin-top:14px; }",
    ".ai-report-card h1:first-child,.ai-report-card h2:first-child { margin-top:0; }",
    ".ai-report-card ul,.ai-report-card ol { padding-left:22px; }",
    ".ai-report-card li { margin-bottom:5px; }",
    ".ai-report-card strong { color:#1B3A6B !important; }",
    ".ai-report-card em { color:#666 !important; font-style:italic; }",
    ".ai-report-card hr { border-top:1px solid #ddd; margin:12px 0; }",
    ".dataTables_wrapper { font-size: 13px; }",
    ".student-dt-table { border-radius: 10px; overflow: hidden; border: 1px solid #e0e4ef; }",
    ".student-dt-table thead { background: #1B3A6B !important; color: white !important; }",
    ".student-dt-table thead th { font-weight: 600; border: none; padding: 12px 14px; }",
    ".student-dt-table tbody tr:hover { background: #f0f4ff !important; }",
    ".student-dt-table tbody tr.selected { background: #dce8ff !important; }",
    ".dt-btn-select { background: #1B3A6B !important; color: white !important; border: none; border-radius: 16px; padding: 4px 14px; font-size: 12px; font-weight: 600; cursor: pointer; transition: background 0.2s; }",
    ".dt-btn-select:hover { background: #1452a3 !important; }",
    ".new-student-form { background: #f8f9fa !important; border-radius: 12px; padding: 20px; border: 1px solid #e0e4ef; }",
    ".new-student-form .form-heading { font-size: 15px; font-weight: 700; color: #1B3A6B !important; background: #f0f4fa !important; margin: -20px -20px 18px -20px; padding: 14px 20px; border-radius: 12px 12px 0 0; border-bottom: 2px solid #C8A951 !important; }",
    ".new-student-form .form-group { margin-bottom: 12px; }",
    ".new-student-form .form-group input,.new-student-form .form-group select { border-radius: 6px; border: 1.5px solid #d0d7e2; padding: 8px 12px; font-size: 13px; width: 100%; color: #333; }",
    ".new-student-form .form-group input:focus,.new-student-form .form-group select:focus { border-color: #1B3A6B !important; box-shadow: 0 0 0 3px rgba(27,58,107,0.1); outline: none; }",
    ".new-student-form label { font-size: 12px; font-weight: 600; color: #1B3A6B !important; margin-bottom: 4px; display: block; }",
    ".new-student-form .btn-add { width: 100%; margin-top: 8px; background: #1B3A6B !important; color: white !important; border: none; border-radius: 8px; padding: 10px 16px; font-size: 14px; font-weight: 600; transition: background 0.2s; }",
    ".new-student-form .btn-add:hover { background: #1452a3 !important; color: white !important; }",
    "@media (max-width: 768px) { .cards-row { flex-direction: column; } .entry-card { min-height: auto; } .home-hero h1 { font-size: 26px; } }",
    ""
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

        # ── Two-column layout: New Student Form (LEFT) + DT Table (RIGHT)
        fluidRow(
          # LEFT: New Student Form (4 cols)
          column(4,
            div(class = "new-student-form",
              div(class = "form-heading", style = "font-size:15px;font-weight:700;color:#1B3A6B;background:#f0f4fa;padding:14px 20px;border-bottom:2px solid #C8A951;", "📝 新学生注册 / New Student Registration"),
              div(class = "form-group",
                tags$label("姓名 * / Name *"),
                textInput("slp_patient_name", NULL, placeholder = "受试者姓名 / Student name")
              ),
              div(class = "form-group",
                tags$label("性别 / Gender"),
                selectInput("slp_patient_gender", NULL,
                  choices = c("请选择 / Select" = "",
                              "男 / Male" = "M",
                              "女 / Female" = "F"),
                  selected = "", width = "100%")
              ),
              div(class = "form-group",
                tags$label("评估师 * / Examiner *"),
                textInput("slp_examiner", NULL, placeholder = "评估师姓名 / Examiner name")
              ),
              div(class = "form-group",
                tags$label("出生日期 * / DOB *"),
                dateInput("slp_dob", NULL, format = "yyyy-mm-dd",
                          value = Sys.Date(), startview = "decade")
              ),
              div(class = "form-group",
                tags$label("评估日期 / Assessment Date"),
                dateInput("slp_assessment_date", NULL, format = "yyyy-mm-dd",
                          value = Sys.Date())
              ),

              div(class = "form-group",
                tags$label(" "),
                actionButton("slp_btn_add_patient",
                  icon = icon("user-plus"),
                  label = "添加学生 / Add Student",
                  class = "btn-add"
                )
              )
            )
          ),

          # RIGHT: Existing DT Student Table + rest of panel (8 cols)
          column(8,
            # Step 1: DT Student Table
            div(class = "student-dt-table",
              DT::dataTableOutput("student_dt")
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

            # Load + Delete buttons (shown after selecting a student)
            fluidRow(
              column(12,
                uiOutput("slp_load_btn_ui")
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
      SELECT p.id, p.name, p.dob, p.gender, p.examiner,
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
      ID            = patients$id,
      Name          = patients$name,
      DOB           = ifelse(is.na(patients$dob), "-", patients$dob),
      Gender        = ifelse(is.na(patients$gender), "-", patients$gender),
      Examiner       = ifelse(is.na(patients$examiner), "-", patients$examiner),
      Assessments   = patients$n_assessments,
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
          list(className = 'dt-center', targets = c(0, 2, 3, 4, 5)),
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
      SELECT p.id, p.name, p.dob, p.gender, p.examiner
      FROM patients p
      ORDER BY p.id DESC
    ")

    pid   <- as.integer(patients$id[selected_row])
    pname <- patients$name[selected_row]
    pdob  <- patients$dob[selected_row]
    pgender <- patients$gender[selected_row]
    pexaminer <- patients$examiner[selected_row]

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
  # Load + Delete Buttons UI (shown after selecting a student)
  # ─────────────────────────────────────────────────────────────
  output$slp_load_btn_ui <- renderUI({
    req(!is.null(input$student_dt_rows_selected) && length(input$student_dt_rows_selected) > 0)
    tagList(
      hr(),
      fluidRow(
        column(4,
          actionButton("btn_slp_load", "📂 加载 / Load",
                       class = "btn-primary",
                       style = sprintf("width:100%%; background:%s;", celf5_blue))
        ),
        column(4,
          actionButton("btn_slp_delete_confirm", "🗑 删除 / Delete",
                       class = "btn-danger",
                       style = "width:100%;")
        ),
        column(4,
          span(style = "color:#999;font-size:12px;padding-top:8px;display:inline-block;",
               "⚠️ 删除学生将同时删除所有关联评估 / Deleting removes all linked assessments")
        )
      )
    )
  })

  # ─────────────────────────────────────────────────────────────
  # Two-step delete confirmation
  # ─────────────────────────────────────────────────────────────
  observeEvent(input$btn_slp_delete_confirm, {
    req(!is.null(rv$patient_id))
    pid  <- rv$patient_id
    pname <- rv$patient_name
    celf5_n <- length(rv$celf5_ids)
    slam_n  <- length(rv$slam_ids)

    showModal(modalDialog(
      title = span(icon("exclamation-triangle"), "⚠️ 确认删除 / Confirm Delete"),
      p(strong(paste0("学生：", pname, " (ID: ", pid, ")"))),
      p(paste0("将同时删除所有关联评估（CELF-5: ", celf5_n, "套, SLAM: ", slam_n, "套）。此操作不可撤销。")),
      p(strong("确定要继续吗？/ Are you sure?"), style = "color:#d00;"),
      easyClose = FALSE,
      footer = tagList(
        actionButton("btn_slp_delete_confirmed",
                     "🗑 确认删除 / Confirm Delete",
                     class = "btn-danger"),
        modalButton("取消 / Cancel")
      )
    ))
  })

  observeEvent(input$btn_slp_delete_confirmed, {
    req(!is.null(rv$patient_id))
    pid <- rv$patient_id
    tryCatch({
      delete_patient(pid)
      rv$patient_id   <- NULL
      rv$patient_name <- NULL
      rv$celf5_ids    <- character()
      rv$slam_ids     <- character()
      showNotification(paste0("已删除学生 #", pid, " 及其所有评估"), type = "message")
    }, error = function(e) {
      showNotification(paste0("删除失败: ", e$message), type = "error")
    })
  })

  # ─────────────────────────────────────────────────────────────
  # Add New Student from form
  # ─────────────────────────────────────────────────────────────
  observeEvent(input$slp_btn_add_patient, {
    # Validation
    patient_name   <- trim(input$slp_patient_name)
    examiner       <- trim(input$slp_examiner)
    dob            <- input$slp_dob
    assessment_date <- input$slp_assessment_date
    gender         <- input$slp_patient_gender
    if (is.null(patient_name) || patient_name == "") {
      showNotification("请填写学生姓名 / Please enter student name", type = "error")
      return()
    }
    if (is.null(dob) || is.na(dob)) {
      showNotification("请选择出生日期 / Please select date of birth", type = "error")
      return()
    }
    if (is.null(examiner) || examiner == "") {
      showNotification("请填写评估师姓名 / Please enter examiner name", type = "error")
      return()
    }

    if (is.null(assessment_date) || is.na(assessment_date)) {
      assessment_date <- Sys.Date()
    }

    tryCatch({
      # Calculate age
      age_calc   <- calculate_age(dob, assessment_date)
      age_years  <- age_calc$years
      age_months <- age_calc$months
      age_days   <- age_calc$days
      age_group  <- get_age_group(age_years)

      # Map gender display to code
      gender_code <- gender
      if (identical(gender, "M")) gender_code <- "M"
      else if (identical(gender, "F")) gender_code <- "F"
      else gender_code <- NA

      # Get or create patient
      con <- get_con()
      on.exit(dbDisconnect(con))
      patient_id <- get_or_create_patient(con, patient_name, dob, gender_code, examiner)

      # Start assessment
      start_assessment(con, patient_id, assessment_date,
                       age_years, age_months, age_days,
                       age_group)

      # Success
      showNotification("✅ 学生已添加 / Student added successfully", type = "message", duration = 3)

      # Reset form fields
      updateTextInput(session, "slp_patient_name", value = "")
      updateSelectInput(session, "slp_patient_gender", selected = "")
      updateTextInput(session, "slp_examiner", value = "")
      updateDateInput(session, "slp_dob", value = Sys.Date())
      updateDateInput(session, "slp_assessment_date", value = Sys.Date())

      # Reload page to refresh DT table
      shinyjs::delay(500, session$reload())

    }, error = function(e) {
      showNotification(paste0("❌ 添加失败 / Error: ", e$message), type = "error")
      cat(file = stderr(), "[add_patient error]", e$message, "\n")
    })
  })

  # ─────────────────────────────────────────────────────────────
  # Load: navigate to CELF-5 or SLAM app
  # ─────────────────────────────────────────────────────────────
  observeEvent(input$btn_slp_load, {
    req(!is.null(rv$patient_id))
    pid <- rv$patient_id
    has_celf5 <- isTRUE(input$chk_celf5) && length(rv$celf5_ids) > 0
    has_slam  <- isTRUE(input$chk_slam)  && length(rv$slam_ids)  > 0
    if (has_celf5) {
      shinyjs::runjs(sprintf("window.location.href = 'http://www.zhangyunhai.com:3838/celf5?patient=%d'", pid))
    } else if (has_slam) {
      shinyjs::runjs(sprintf("window.location.href = 'http://www.zhangyunhai.com:3838/slam?patient=%d'", pid))
    } else {
      showNotification("请先在右侧选择要加载的评估类型（CELF-5 或 SLAM）", type = "warning")
    }
  })

  # ─────────────────────────────────────────────────────────────
  # Student Selected Info UI
  # ─────────────────────────────────────────────────────────────
  output$student_selected_ui <- renderUI({
    if (is.null(rv$patient_id)) {
      return(tags$div(
        class = "student-selected-info",
        style = "border-left-color: #ccc; color: #555; text-align: center; padding: 20px; background: #f0f0f0;",
        "请在上方表格中选择学生 / Please select a student from the table above"
      ))
    }

    con <- get_con()
    on.exit(dbDisconnect(con))

    pdob_out   <- dbGetQuery(con, sprintf("SELECT dob FROM patients WHERE id = %d", rv$patient_id))$dob
    pgender_out <- dbGetQuery(con, sprintf("SELECT gender FROM patients WHERE id = %d", rv$patient_id))$gender
    pexaminer_out <- dbGetQuery(con, sprintf("SELECT examiner FROM patients WHERE id = %d", rv$patient_id))$examiner
    if (is.na(pdob_out))   pdob_out   <- "-"
    if (is.na(pgender_out)) pgender_out <- "-"
    if (is.na(pexaminer_out)) pexaminer_out <- "-"

    celf5_count <- length(rv$celf5_ids)
    slam_count  <- length(rv$slam_ids)

    tags$div(class = "student-selected-info",
      tags$div(class = "student-name", paste0("👤 ", rv$patient_name)),
      tags$div(class = "student-meta",
        paste0("ID: ", rv$patient_id, "  |  DOB: ", pdob_out,
               "  |  Gender: ", pgender_out, "  |  Examiner: ", pexaminer_out, "  |  ",
               "CELF-5: ", celf5_count, "套  |  SLAM: ", slam_count, "套")
      )
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
