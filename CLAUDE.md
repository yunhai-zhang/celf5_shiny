# CELF-5 Shiny Assessment App

## 项目位置
`/home/yzhang/clawfiles/celf5_shiny/`

## 技术栈
- R Shiny (app.R 2209行 + global.R 1814行)
- SQLite 数据库 (`celf5_assessments.db`, `celf5_norms.db`)
- shiny-server → www.zhangyunhai.com:3838/celf5/
- tesseract OCR + MiniMax LLM API（AI 写作评分）
- MiniMax Speech 2.8 TTS（临床报告语音合成）

## 数据库
- `celf5_assessments.db`: patients, assessments, responses, subtest_scores, questions
- `celf5_norms.db`: composite_ci_table, composite_table, norms_ci, norms_table
- 年龄组格式：norms 用 "Y:M-Y:M"（如 "7:0-7:11"），questions 表用 "age_5_8" / "age_9_11" / "age_12_14" / "age_15_21"
- 年龄组转换：`global.R:370-386` 的 `age_group_to_questions()` / `age_group_from_questions()`

## 核心文件

| 文件 | 说明 |
|------|------|
| `app.R` | 主应用逻辑（2209行），含所有 UI render/observeEvent |
| `global.R` | 全局数据/函数（1814行），Rubric、常模、helper 函数 |
| `ocr_score_sw.R` | Structured Writing AI 评分 pipeline（OCR + MiniMax LLM） |
| `report_celf5*.Rmd` | 报告 R Markdown 模板 |

## SW_SCORING_RUBRIC（按 age_group）

```r
SW_SCORING_RUBRIC <- list(
  age_8     = list(org_scale=c("3分"=3,"2分"=2,"1分"=1,"0分"=0),  grammar=c(3,2,1,0), mech=c(3,2,1,0)),
  age_9_10  = list(org_scale=c("3分"=3,"2分"=2,"0分"=0),          grammar=c(3,2,1,0), mech=c(3,2,1,0)),
  age_11_12 = list(org_scale=c("4分"=4,"3分"=3,"0分"=0),          grammar=c(3,2,1,0), mech=c(3,2,1,0)),
  age_13_21 = list(org_scale=c("5分"=5,"4分"=4,"0分"=0),          grammar=c(1,0),     mech=c(3,2,1,0))
)
```

**重要**：Manual Table 3.7 确认 age_9_10/11_12/13_21 的 org_scale 没有中间档（1分/2分/3分），这是按"句子数"评分的设计，不是 bug。

## 关键代码位置

| 功能 | 位置 |
|------|------|
| sw_rubric_key（age_group → rubric key） | app.R:787-795 |
| sw_rubric_r（读 SW_SCORING_RUBRIC） | app.R:810-817 |
| sw_topic_scoring_ui（评分 UI 构建） | app.R:918-1012 |
| btn_run_ai_score handler | app.R:1137-1181 |
| ai_score_phase reactive（spinner 驱动） | app.R:1109 |
| sw_ai_spinner 输出 | app.R:1111-1130 |
| btn_apply_ai_scores | app.R:1195+ |
| generate_clinical_narrative | global.R:1500+ |
| .call_minimax（临床报告用） | global.R:1420-1478 |
| MiniMax API key 读取 | ocr_score_sw.R:30-50 |

## AI 评分原理（ocr_score_sw.R）

```
ocr_and_score(img_path, rubric_key)
  ├─ tesseract OCR → recognized_text
  ├─ build_sw_vision_prompt(rubric) → prompt（含评分规则）
  ├─ .call_minimax(prompt) → JSON 响应
  ├─ .parse_sw_response(raw, rubric) → 结构化结果
  │   └─ cap(score, org_max) 限制分数范围
  └─ list(recognized_text, structure, grammar, organization, mechanics)
```

**已知问题**：
- `.call_minimax()` 的 model 名需与 global.R 一致（"MiniMax/M2.7"）
- JSON 响应可能含 think tags，需清理

## 已完成功能

| 功能 | 状态 | 备注 |
|------|------|------|
| spinner（AI 分析按钮） | ✅ | app.R:1109-1130，复用 narrative_phase 模式 |
| AI 评分端到端 | ✅ | 需要真实图片测试 |
| MiniMax TTS 临床报告 | ✅ | global.R:1500+，speech-2.8-hd |
| SW_SCORING_RUBRIC | ✅ | 与 Manual Table 3.7 一致 |

## 剩余工作（按优先级）

### P0
- **SW AI 评分端到端测试**：用真实儿童手写图片测试 `btn_run_ai_score`
- **model 名一致性**：ocr_score_sw.R 和 global.R 的 MiniMax model 名需统一
- **JSON think tags 清理**：MiniMax API 返回可能含 `<think>...</think>` 标签

### P1
- **max_score=2 评分 UI**：USP item 18 等 2分题 UI 只显示 0/1，需动态生成 radioButtons
- **Discontinue Rules**：已在 `celf5_skip_rules.md` 记录，实现到 app.R

### P2
- **临床报告 AI 生成**：`generate_clinical_narrative()` 需对接真实数据

### P3
- **临床叙事报告增强**：多语言、临床术语优化

## 常用命令

```bash
# 重启 shiny-server
echo 'Callofjj1989' | sudo -S systemctl restart shiny-server

# 测试 app
curl http://www.zhangyunhai.com:3838/celf5/

# 本地测试（端口 3847）
cd /home/yzhang/clawfiles/celf5_shiny
Rscript -e "shiny::runApp('.', launch.browser=FALSE, port=3847)"

# 测试 AI 评分
Rscript -e '
source("/home/yzhang/clawfiles/celf5_shiny/ocr_score_sw.R")
result <- ocr_and_score("/path/to/image.jpg", "age_9_10")
str(result)
'
```

## Git

```bash
cd /home/yzhang/clawfiles/celf5_shiny
git add -A
git commit -m "描述改动"
git push
```

## MiniMax API（事实）
- endpoint: `https://api.minimaxi.com`（北京节点）
- TTS model: `speech-2.8-hd`，voice_id: `danya_xuejie`
- API key 路径: `~/.hermes/.env`（环境变量 `MINIMAX_CN_API_KEY=sk-cp-08I1...`）
- Token Plan Plus 重置时间：每日 20:00 北京时间
