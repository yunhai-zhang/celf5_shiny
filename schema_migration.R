# schema_migration.R — Shared DB schema for CELF5 + SLAM
# Run this to migrate celf5_assessments.db to support both assessment types.
# Uses tidyverse/dplyr + RSQLite.

library(dplyr)
library(RSQLite)

DB_PATH <- "/home/yzhang/clawfiles/celf5_shiny/celf5_assessments.db"

get_con <- function() {
  dbConnect(SQLite(), DB_PATH)
}

# ─────────────────────────────────────────────────────────────
# 1. assessments: add assessment_type column
# ─────────────────────────────────────────────────────────────
migrate_assessments_type <- function(con) {
  cols <- dbListFields(con, "assessments")
  if (!"assessment_type" %in% cols) {
    dbExecute(con, "
      ALTER TABLE assessments ADD COLUMN assessment_type TEXT
      CHECK (assessment_type IN ('CELF5', 'SLAM'))
      DEFAULT 'CELF5'
    ")
    message("[migrate] Added assessment_type to assessments")
  } else {
    message("[migrate] assessment_type already exists in assessments")
  }
  invisible(NULL)
}

# ─────────────────────────────────────────────────────────────
# 2. subtest_scores: add SLAM columns
# ─────────────────────────────────────────────────────────────
migrate_subtest_scores_slam <- function(con) {
  cols <- dbListFields(con, "subtest_scores")

  if (!"story_id" %in% cols) {
    dbExecute(con, "ALTER TABLE subtest_scores ADD COLUMN story_id TEXT")
    message("[migrate] Added story_id to subtest_scores")
  }
  if (!"percentile" %in% cols) {
    dbExecute(con, "ALTER TABLE subtest_scores ADD COLUMN percentile REAL")
    message("[migrate] Added percentile to subtest_scores")
  }
  invisible(NULL)
}

# ─────────────────────────────────────────────────────────────
# 3. responses: add optional story_id column
# ─────────────────────────────────────────────────────────────
migrate_responses_story_id <- function(con) {
  cols <- dbListFields(con, "responses")
  if (!"story_id" %in% cols) {
    dbExecute(con, "ALTER TABLE responses ADD COLUMN story_id TEXT")
    message("[migrate] Added story_id to responses")
  } else {
    message("[migrate] story_id already exists in responses")
  }
  invisible(NULL)
}

# ─────────────────────────────────────────────────────────────
# 4. narrative_scores: new table for free narrative scoring
#    5 dimensions per story: Narrative Structure, Complex Clauses,
#    Inferencing, Pragmatic, Theory of Mind — each 0-2
# ─────────────────────────────────────────────────────────────
create_narrative_scores <- function(con) {
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS narrative_scores (
      id              INTEGER PRIMARY KEY AUTOINCREMENT,
      assessment_id   INTEGER NOT NULL,
      story_id        TEXT    NOT NULL,
      dimension       TEXT    NOT NULL,
      score           INTEGER NOT NULL CHECK (score BETWEEN 0 AND 2),
      created_at      TEXT    DEFAULT (datetime('now','localtime')),
      FOREIGN KEY (assessment_id) REFERENCES assessments(id),
      UNIQUE(assessment_id, story_id, dimension)
    )
  ")
  message("[migrate] Created/verified narrative_scores table")
  invisible(NULL)
}

# ─────────────────────────────────────────────────────────────
# 5. SLAM stories: new table for story-level metadata
# ─────────────────────────────────────────────────────────────
create_slam_stories <- function(con) {
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS slam_stories (
      id              INTEGER PRIMARY KEY AUTOINCREMENT,
      story_key       TEXT    NOT NULL UNIQUE,
      story_name      TEXT    NOT NULL,
      story_name_zh   TEXT,
      age_range       TEXT,
      word_finding_n  INTEGER NOT NULL DEFAULT 0,
      gfa_n           INTEGER NOT NULL DEFAULT 0,
      gfa_max_score   INTEGER NOT NULL DEFAULT 2,
      created_at      TEXT    DEFAULT (datetime('now','localtime'))
    )
  ")
  message("[migrate] Created/verified slam_stories table")
  invisible(NULL)
}

# ─────────────────────────────────────────────────────────────
# 6. SLAM composite: optional aggregate table
# ─────────────────────────────────────────────────────────────
create_slam_composites <- function(con) {
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS slam_composites (
      id                INTEGER PRIMARY KEY AUTOINCREMENT,
      assessment_id     INTEGER NOT NULL,
      composite_name    TEXT    NOT NULL,
      sum_raw           INTEGER,
      standard_score    INTEGER,
      percentile        REAL,
      confidence_68_lo  INTEGER,
      confidence_68_hi  INTEGER,
      created_at        TEXT    DEFAULT (datetime('now','localtime')),
      FOREIGN KEY (assessment_id) REFERENCES assessments(id),
      UNIQUE(assessment_id, composite_name)
    )
  ")
  message("[migrate] Created/verified slam_composites table")
  invisible(NULL)
}

# ─────────────────────────────────────────────────────────────
# Run all migrations
# ─────────────────────────────────────────────────────────────
run_migration <- function() {
  con <- get_con()
  on.exit(dbDisconnect(con), add = TRUE)

  message("=== Starting SLAM schema migration ===")

  migrate_assessments_type(con)
  migrate_subtest_scores_slam(con)
  migrate_responses_story_id(con)
  create_narrative_scores(con)
  create_slam_stories(con)
  create_slam_composites(con)

  message("=== Migration complete ===")
  verify_schema(con)

  invisible(NULL)
}

# ─────────────────────────────────────────────────────────────
# Verify: list all tables and their columns
# ─────────────────────────────────────────────────────────────
verify_schema <- function(con) {
  tables <- dbListTables(con)
  message("\n=== DB Schema Verification ===")
  for (t in tables) {
    cols <- dbListFields(con, t)
    message(sprintf("  Table: %s", t))
    for (c in cols) {
      message(sprintf("    - %s", c))
    }
  }
  message("=== Verification complete ===\n")
}

# ─────────────────────────────────────────────────────────────
# Seed slam_stories with known story metadata
# ─────────────────────────────────────────────────────────────
seed_slam_stories <- function() {
  con <- get_con()
  on.exit(dbDisconnect(con), add = TRUE)

  stories <- tibble(
    story_key     = c("baseball_troubles", "the_best_turkey",
                      "the_girl_who_loved_horses", "wallace_and_batty"),
    story_name    = c("Baseball Troubles", "The Best Turkey",
                      "The Girl Who Loved Horses", "Wallace and Batty"),
    story_name_zh = c("棒球烦恼", "最好的火鸡",
                      "爱马的女孩", "华莱士与巴蒂"),
    age_range     = c("13-17岁", "10-14岁", "13-17岁", "7-14岁"),
    word_finding_n = c(6L, 5L, 6L, 5L),
    gfa_n          = c(4L, 4L, 4L, 4L),
    gfa_max_score  = c(2L, 2L, 2L, 2L)
  )

  for (i in seq_len(nrow(stories))) {
    row <- stories[i, ]
    dbExecute(con,
      "INSERT OR IGNORE INTO slam_stories
       (story_key, story_name, story_name_zh, age_range,
        word_finding_n, gfa_n, gfa_max_score)
       VALUES (?, ?, ?, ?, ?, ?, ?)",
      params = list(row$story_key, row$story_name, row$story_name_zh,
                    row$age_range, row$word_finding_n, row$gfa_n, row$gfa_max_score))
  }

  message("[seed] SLAM story metadata seeded")
  invisible(NULL)
}

# ─────────────────────────────────────────────────────────────
# Test: run migration and verify
# ─────────────────────────────────────────────────────────────
test_migration <- function() {
  con <- get_con()
  on.exit(dbDisconnect(con), add = TRUE)

  message("\n=== Running Migration Tests ===")

  # Test 1: assessments has assessment_type
  cols <- dbListFields(con, "assessments")
  stopifnot("assessment_type" %in% cols)
  message("  [PASS] assessment_type column exists in assessments")

  # Test 2: subtest_scores has SLAM columns
  cols <- dbListFields(con, "subtest_scores")
  stopifnot("story_id" %in% cols)
  stopifnot("percentile" %in% cols)
  message("  [PASS] story_id and percentile columns exist in subtest_scores")

  # Test 3: responses has story_id
  cols <- dbListFields(con, "responses")
  stopifnot("story_id" %in% cols)
  message("  [PASS] story_id column exists in responses")

  # Test 4: narrative_scores exists
  stopifnot("narrative_scores" %in% dbListTables(con))
  cols <- dbListFields(con, "narrative_scores")
  stopifnot(all(c("assessment_id","story_id","dimension","score") %in% cols))
  message("  [PASS] narrative_scores table exists with required columns")

  # Test 5: slam_stories exists
  stopifnot("slam_stories" %in% dbListTables(con))
  message("  [PASS] slam_stories table exists")

  # Test 6: slam_composites exists
  stopifnot("slam_composites" %in% dbListTables(con))
  message("  [PASS] slam_composites table exists")

  # Test 7: Seed stories
  seed_slam_stories()
  n_stories <- dbGetQuery(con, "SELECT COUNT(*) as n FROM slam_stories")$n
  stopifnot(n_stories == 4)
  message(sprintf("  [PASS] slam_stories seeded with %d stories", n_stories))

  message("=== All Tests Passed ===\n")
  verify_schema(con)

  invisible(TRUE)
}

# ─────────────────────────────────────────────────────────────
# Main: run migration + tests when sourced
# ─────────────────────────────────────────────────────────────
if (sys.nframe() == 0L) {
  run_migration()
  test_migration()
}
