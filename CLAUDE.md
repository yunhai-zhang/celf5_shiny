# CLAUDE.md — CELF-5 / SLAM / SLP Shiny Apps

## Project Overview
Clinical language assessment platform with 3 Shiny apps sharing one SQLite DB.

**Apps:**
- `www.zhangyunhai.com:3838/slp/` — SLP unified entry point (app.R)
- `www/zhangyunhai.com:3838/celf5/` — CELF-5 assessment tool
- `www.zhangyunhai.com:3838/slam/` — SLAM narrative assessment tool

**Shared DB:** `/home/yzhang/clawfiles/celf5_shiny/celf5_assessments.db`
**Shared global.R:** `/home/yzhang/clawfiles/celf5_shiny/global.R`

## Architecture
- **Patient linkage:** By `patient_id` (patients table). CELF-5 and SLAM assessments for the same person share the same `patient_id`. Query by `patient_id` to get ALL data across both assessment types.
- **assessment_type:** Either `'CELF5'` or `'SLAM'` in the `assessments` table.

## Brand Colors
- Navy: `#1B3A6B`
- Gold: `#C8A951`

## FILE EDITING RULES (CRITICAL)

### NEVER DO THIS
- ❌ `write_file()` or full file rewrite
- ❌ `replace_file()` or similar full-file operations
- ❌ Ask Claude Code to "rewrite the entire file"

### ALWAYS DO THIS
- ✅ Use `patch()` tool for ALL file edits — targeted find-and-replace
- ✅ After any edit, verify: `Rscript -e "source('app.R', echo=FALSE)"`
- ✅ If syntax error found, fix with another `patch()` — never rewrite from scratch

### Before Patching
1. Read the exact section you want to change with `read_file(offset=X, limit=Y)`
2. Provide enough surrounding context in `old_string` to make the match unique
3. Do NOT use regex or sed from terminal — use `patch()`

## After Any Code Change
```bash
# Verify syntax
Rscript -e "source('app.R', echo=FALSE)"

# Verify deployed app
curl -s -o /dev/null -w "%{http_code}" www.zhangyunhai.com:3838/slam/app.R
curl -s -o /dev/null -w "%{http_code}" www.zhangyunhai.com:3838/slp/
```

## Communication
- Write ALL prompts to Claude Code in **English**
- Use Python for file operations (extract PDFs, DB queries, string manipulation)
- Use R only within Rscript/shiny context

## Required Helper Files (must exist in /srv/shiny-server/slp/)
- `ocr_score_sw.R` — sourced by app.R at startup
- `global.R` — shared functions
