# CELF-5 Shiny Assessment App

## 项目位置
/home/yzhang/clawfiles/celf5_shiny/

## 技术栈
- R Shiny (app.R + global.R)
- SQLite 数据库 (celf5_assessments.db, celf5_norms.db)
- shiny-server → www.zhangyunhai.com:3838/celf5/
- tesseract OCR + MiniMax LLM API（AI 评分）

## 数据库
- `celf5_assessments.db`: patients, subjects, responses, usp_paragraphs, questions
- `celf5_norms.db`: norms 表（年龄格式 "Y:M-Y:M"）
- 年龄组格式：norms 用 "Y:M-Y:M"（如 "7:0-7:11"），USP paragraphs DB 用 "A/B/C/D/E/F"

## 已完成
- 基础框架、登录、患者管理
- 大部分子测试题目加载
- USP 子测试修复（age_group 格式转换 bug，line 411 已修）
- AI 评分 `.read_minimax_key()` fallback 路径修复（ocr_score_sw.R → ~/.hermes/.env）

## 剩余工作（按优先级）

### P0 — AI 写作评分功能（Structured Writing）
**Bug 现象**：点击"🔍 AI 分析"后报"MINIMAX_CN_API_KEY not found"或返回格式异常

**已知问题（已修复 1/？）**：
1. ✅ `ocr_score_sw.R` 的 `.read_minimax_key()` fallback 路径错误：`~/.env`（无 key）→ 已改为 `~/.hermes/.env`
2. ⚠️ `.call_minimax()` model 名可能需调整（global.R 用 "MiniMax/M2.7"，ocr_score_sw.R 用 "MiniMax-M2.7"）
3. ⚠️ MiniMax API 返回的 JSON 可能在 think tags 之后，需验证 JSON 解析逻辑
4. ⚠️ shiny-server 以 root 运行，Sys.getenv("MINIMAX_CN_API_KEY") 可能返回空

**测试命令**：
```R
source("/home/yzhang/clawfiles/celf5_shiny/ocr_score_sw.R")
key <- .read_minimax_key()  # 应返回 125 字符的 sk-cp-... key
test_result <- .call_minimax("Reply with exactly {\"test\": 1}", max_tokens=200L)
# 检查返回是否干净（无 think tags）
```

**关键文件**：
- `app.R:944-1024` — btn_run_ai_score observeEvent
- `app.R:924-932` — sw_rubric_key() 转换（norms格式 → "age_8"/"age_9_10"等）
- `ocr_score_sw.R` — OCR + LLM 评分完整 pipeline
- `global.R:1420-1478` — 另一个版本的 .call_minimax（临床报告生成用）

### P1 — max_score=2 评分 UI
**问题**：2分题（如 USP item 18）目前 UI 只显示 0/1 选项
**位置**：app.R 评分 UI 生成逻辑，需根据 max_score 动态生成 radioButtons

### P2 — AI 临床报告生成
**文件**：`global.R:generate_clinical_narrative()`（1485行开始）
**依赖**：各 subtest 的 raw score 数据

### P3 — Discontinue Rules
各 subtest 的 discontinue 规则实现（已在 `celf5_skip_rules.md` 记录）

## 常用命令
```bash
# 重启 shiny-server
echo 'Callofjj1989' | sudo -S systemctl restart shiny-server

# 测试 app
curl http://www.zhangyunhai.com:3838/celf5/

# 测试 OCR+AI 评分（Rscript 环境）
Rscript -e '
source("/home/yzhang/clawfiles/celf5_shiny/ocr_score_sw.R")
# 需要一张测试图片路径
result <- ocr_and_score("/path/to/image.jpg", "age_9_10")
str(result)
'
```

## Git
项目有 git 管理。每次完成功能后自动 commit：
```bash
cd /home/yzhang/clawfiles/celf5_shiny
git add -A
git commit -m "描述改动的具体内容"
```
