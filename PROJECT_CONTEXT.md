# CELF-5 / SLAM / SLP — Project Constants

**Last updated:** 2026-05-11

## Paths
```
SLP app:       /srv/shiny-server/slp/app.R
SLAM app:      /srv/shiny-server/slam/app.R
CELF-5 app:    /srv/shiny-server/celf5/app.R (symlink)
global.R:      /home/yzhang/clawfiles/celf5_shiny/global.R
DB:            /home/yzhang/clawfiles/celf5_shiny/celf5_assessments.db
ocr_score:     /srv/shiny-server/slp/ocr_score_sw.R
Source dir:    /home/yzhang/clawfiles/celf5_shiny/
```

## Brand Colors
```
navy:  #1B3A6B
gold:  #C8A951
```

## DB Schema
- `patients(id, name, dob, gender, examiner, notes)`
- `assessments(id, patient_id, assessment_date, age_years, age_months, age_days, age_group, status, assessment_type)`
  - `assessment_type` = 'CELF5' or 'SLAM'
- `responses(id, assessment_id, subtest, item, response_text, scored_value)`
- `subtest_scores(id, assessment_id, subtest, raw_score, standard_score, percentile_rank)`

## Patient Linkage Rule
**Same patient = same patient_id.** Do NOT link by name. When showing DT of patients, query by `patient_id`. When generating report for a patient, read ALL assessments for that `patient_id` across both `assessment_type` values.

## Verified Apps (200 OK)
```
www.zhangyunhai.com:3838/slp/
www.zhangyunhai.com:3838/slam/
www.zhangyunhai.com:3838/celf5/
```

## Common Commands
```bash
# Verify R syntax
Rscript -e "source('/home/yzhang/clawfiles/celf5_shiny/app.R', echo=FALSE)"

# Check app deployed
curl -s -o /dev/null -w "%{http_code}" www.zhangyunhai.com:3838/slp/

# DB query
sqlite3 /home/yzhang/clawfiles/celf5_shiny/celf5_assessments.db "SELECT COUNT(*) FROM patients;"
```

## Kanban
```
hermes kanban list           # show all tasks
hermes kanban complete <id>  # mark done
hermes kanban create "title" # create task
```
