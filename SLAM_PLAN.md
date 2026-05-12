# SLAM Platform Improvement Plan

## Context & Current State

### Files
- **SLAM app**: `/srv/shiny-server/slam/app.R` (1129 lines, fully bilingual UI already)
- **SLP app**: `/srv/shiny-server/slp/app.R` (CELF-5 based, 2053 lines)
- **global.R**: `/home/yzhang/clawfiles/celf5_shiny/global.R` (2293 lines, shared DB functions)
- **DB**: `/home/yzhang/clawfiles/celf5_shiny/celf5_assessments.db` (SQLite, patients + assessments + responses tables)
- **Story images**: `/srv/shiny-server/slam/www/story_images/` — **30 PNG images already extracted**

### Already Bilingual
The SLAM app already has bilingual labels (Chinese + English) throughout. The main gaps are:
1. **No Subject Info tab** — student info is collected at top but NOT saved to patients table
2. **No image display** — placeholder divs shown instead of actual images from story_images/
3. **SLP AI Report DT** — needs verification

### Key DB Schema (from global.R)
```sql
patients(id, name, dob, gender, examiner, notes)
assessments(id, patient_id, assessment_date, age_years, age_group, status, notes)
responses(assessment_id, subtest, item_number, response_text, score, ...)
subtest_scores(assessment_id, subtest, raw_score, scaled_score, ...)
```

---

## TASK 1: SLAM App — Add Subject Info Tab + Make Fully Bilingual + Display Images

### Step 1: Restructure Student Info Bar into Subject Info Tab

**File**: `/srv/shiny-server/slam/app.R`

**Problem**: The student info bar (lines 403-436) collects name/age/gender/date but:
- Does NOT save to `patients` table on "Start Assessment" 
- Uses `age` not `DOB` (no age auto-calculation)
- Has no "Start Assessment" button to trigger DB saves

**Changes**:
1. Move student info from permanent top bar into a proper first tab `tabPanel("👤 受试者信息 / Subject Info", ...)`
2. Change `numericInput("slam_student_age")` → `dateInput("slam_dob")` for DOB
3. Add auto-computed age display using `calculate_age()` (from global.R)
4. Add `actionButton("slam_start_assessment", "▶ 开始评估 / Start Assessment")`
5. Store `patient_id` and `assessment_id` in reactiveValues on "Start"
6. Remove the old top-bar student info section (lines 403-436)

**Lines affected**: UI section (lines 387–810), Server section (lines 815–1105)

**Implementation**:
- Wrap entire student info section in `tabPanel("👤 受试者信息 / Subject Info")` 
- Add `dateInput("slam_dob")` at line ~414 (replacing numeric age)
- Add age calculation: `age <- as.integer(floor(interval(dob, assessment_date) / years(1)))`
- Add `actionButton("slam_start_assessment", "▶ 开始评估 / Start Assessment")` after date input
- Create server-side `observeEvent(input$slam_start_assessment)` that:
  - Calls `upsert_patient()` (from global.R) with name, dob, gender
  - Calls `start_assessment()` with assessment_type='SLAM'
  - Sets `rv$slam_patient_id` and `rv$slam_assessment_id`

---

### Step 2: Make ALL UI Text Fully Bilingual

**File**: `/srv/shiny-server/slam/app.R`

The UI is ~80% bilingual but some sections need fixing:

| Location | Issue | Fix |
|---|---|---|
| Lines 537, 615, 693, 771 | Save button text only Chinese | Add bilingual: `"💾 保存 [StoryName] 评分 / Save [StoryName] Scores"` |
| Lines 576, 596, 654, 674, 732, 752 | Section headers mostly Chinese | Add EN: `"📝 GFA 语法填空 / Grammar Fluency Assessment"` |
| Lines 577, 654, 732 | Chinese-only GFA section labels | Add EN suffix |
| Lines 602, 680, 758 | Chinese-only Narrative Rubric labels | `"📊 叙事评分 / Narrative Rubric"` |
| Summary panel (lines 781-800) | Partial bilingual | Make fully bilingual throughout |
| Save All button (line 796) | Chinese only | `"💾 保存完整评估报告 / Save Complete Report"` |

**Button label changes** (4 save buttons at lines 537, 615, 693, 771):
```r
# Before:
actionButton("save_bt", "💾 保存 Baseball Troubles 评分")
# After:
actionButton("save_bt", "💾 保存 / Save Baseball Troubles Scores")
```

**GFA section headers** (lines 576, 654, 732):
```r
# Before:
div(class = "section-label", "📝 GFA 语法填空")
# After:
div(class = "section-label", "📝 GFA 语法填空 / Grammar Fluency Assessment")
```

---

### Step 3: Display Extracted Story Images

**File**: `/srv/shiny-server/slam/app.R`

**Images already extracted** to `/srv/shiny-server/slam/www/story_images/`:
```
baseball_troubles_p1_img1.png through p8_img1.png (8 images)
the_best_turkey_p1_img1.png through p8_img1.png (8 images)
the_girl_who_loved_horses_p1_img1.png through p3_img2.png (various)
wallace_and_batty_p1_img1.png through p3_img2.png (various)
```

**Current placeholder** (lines 458-462, 557-559, 635-637, 713-715):
```r
div(class = "image-placeholder", ...,
  span("📄 PDF: 1. SLAM Baseball Troubles_English.pdf"),
  span("图片路径: /tmp/slam_extract/..."),
  span("(需要PDF图片提取) — 请在评估时展示给受试者")
)
```

**Replace with actual image display**:
```r
div(class = "story-images-grid",
  lapply(1:n_images, function(i) {
    img_path <- sprintf("story_images/%s_p%%d_img%%d.png", story_id)
    # Try each page/image combo until found
    img_file <- find_story_image(story_id, i)
    if (!is.null(img_file)) {
      img(src = img_file, style = "max-width: 100%%; border-radius: 8px; margin: 4px;",
          alt = sprintf("Story image page %%d", i))
    }
  })
)
```

**Implementation approach** (server-side helper):
```r
find_story_image <- function(story_id, page_num) {
  img_dir <- "story_images"
  # Pattern: {story_id}_p{page}_img1.png (most are img1)
  candidates <- c(
    sprintf("%%s_p%%d_img1.png", story_id, page_num),
    sprintf("%%s_p%%d_img2.png", story_id, page_num)
  )
  for (c in candidates) {
    f <- file.path(img_dir, sprintf(c, story_id, page_num))
    if (file.exists(f)) return(f)
  }
  NULL
}
```

**CSS for image grid**:
```css
.story-images-grid { display: flex; flex-wrap: wrap; gap: 10px; margin-bottom: 20px; }
.story-images-grid img { max-width: 100%%; border-radius: 8px; border: 1px solid #e2e8f0; }
```

**Image placeholder locations** (replace these 4 blocks):
- Line ~458: Baseball Troubles placeholder
- Line ~557: The Best Turkey placeholder
- Line ~635: The Girl Who Loved Horses placeholder  
- Line ~713: Wallace and Batty placeholder

---

## TASK 2: SLP App — Verify AI Report DT is Complete

### File: `/srv/shiny-server/slp/app.R`

**Verification checklist**:

1. **App loads without errors**: `curl http://www.zhangyunhai.com:3838/slp/` → 200 ✓ (already confirmed)

2. **DT shows combined CELF-5 + SLAM assessments**:
   - Check for a `dataTableOutput` in the AI Report tab
   - Check if it queries both assessment types from DB
   - Look for `list_assessments()` or similar function

3. **Row selection shows info box**: Find `input$..._table_rows_selected` handler

4. **Patient detail panel shows all assessments**: Look for `renderUI` outputting patient history

5. **Generate button produces AI narrative**: Search for `generate_narrative` or similar AI function

**If broken — common fixes needed**:
- Missing `source("global.R")` 
- Wrong DB path
- DT selection not wired up
- AI generation function missing

**Specific sections to check in SLP app**:
- Search for "DT" or "dataTable" → find the AI Report tab
- Search for "generate" → find AI narrative function
- Search for "patient" → find patient detail panel

---

## TASK 3: Copy & Verify After Each Change

### Deployment steps after modifying `/srv/shiny-server/slam/app.R`:
```bash
# 1. Copy modified file
cp /home/yzhang/clawfiles/celf5_shiny/SLAM_PLAN.md /srv/shiny-server/slam/app.R

# Wait — PLAN.md is not the app. Copy actual modified app:
# (Will be done after implementation)

# 2. Check syntax
Rscript -e "source('/srv/shiny-server/slam/app.R', echo=FALSE)"

# 3. Verify deployment
curl -s -o /dev/null -w "%{http_code}" http://www.zhangyunhai.com:3838/slam/
```

---

## Detailed File Changes

### `/srv/shiny-server/slam/app.R` — Change Log

| Change | Lines | Type |
|---|---|---|
| Add `library(lubridate)` for age calc | ~6 | Add |
| Add `source("global.R")` for DB helpers | ~15 | Add |
| Add `find_story_image()` helper | ~260-280 | Add function |
| Add image grid CSS | ~372-380 | Add CSS |
| Wrap student info in tabPanel | ~403-436 | Restructure |
| Replace age with DOB input | ~414-416 | Replace |
| Add "Start Assessment" button | ~433-434 | Add |
| Remove old top-bar (unneeded after tab) | ~403-436 | Delete |
| Replace 4x image placeholders | ~458,557,635,713 | Replace |
| Fix GFA section labels bilingual | ~576,654,732 | Replace |
| Fix save button labels bilingual | ~537,615,693,771 | Replace |
| Fix narrative rubric labels | ~602,680,758 | Replace |
| Add `slam_start_assessment` observer | ~815-850 | Add server |
| Add patient/assessment ID reactive storage | ~193-216 | Add rv |

---

## Risks & Dependencies

| Risk | Impact | Mitigation |
|---|---|---|
| Breaking SLP/CELF-5 while modifying SLAM | HIGH | Only modify SLAM; deploy separately |
| Story images not matching story order | MEDIUM | Use fallback placeholder if file not found |
| DB schema mismatch for `assessment_type` | LOW | Check `assessments` table has `assessment_type` column |
| `global.R` functions not available in SLAM | LOW | SLAM has its own `get_con()` and DB helpers |

### DB Column Check
Need to verify `assessments` table has `assessment_type` column for SLAM vs CELF-5 distinction:
```sql
PRAGMA table_info(assessments);
```
If `assessment_type` column doesn't exist, add it via ALTER TABLE.

---

## Implementation Order

1. **Write plan** → `/home/yzhang/clawfiles/celf5_shiny/SLAM_PLAN.md` ✓
2. **Verify DB schema** — check `assessment_type` column exists
3. **Task 2**: Modify SLAM app.R (Subject Info tab + bilingual + images)
4. **Deploy SLAM** — copy + syntax check + curl
5. **Task 3**: Verify SLP AI Report DT — test all 5 criteria
6. **Fix SLP** if anything broken
7. **Deploy SLP** if modified
