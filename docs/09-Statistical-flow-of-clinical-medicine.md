# 临床医学统计流 {#Statistical-flow-of-clinical-medicine}
## COMMON.R
### 加载R包和配置全局环境

```R
## 1 加载R包和配置全局环境：----
install.packages("pacman")
library(pacman)
pacman::p_load(purrr,tidyverse,scales,dplyr,flextable,gt,gtsummary,rstatix,forcats,janitor,table1,tableone,arrow,
     data.table,datacleanr,stringr,tidyr,lubridate,openxlsx)
# Global options
options(scipen = 1000)
options(scipen = 1, digits = 2)
options(encoding = 'UTF-8')

# getOption("digits")

Sys.setlocale("LC_ALL","Chinese")
```

### 使用officer来启动函数参数：

### Table of content

```R
center_par <- fp_par(text.align = "center",padding = 10)
bold_face <- shortcuts$fp_bold(font.size = 20)
toc <- fpar(
  ftext("目  录", prop = bold_face ),
  fp_p = center_par)
```


### Cover template -封面

```R
## 基于officer包生成项目封面：

bold_face1 <- fp_text(font.size = 25, bold = TRUE,
                      font.family = "Times New Roma")
bold_face2 <- fp_text(font.size = 20, font.family = "宋体")

cover <- fpar(
  
  run_linebreak(), run_linebreak(), run_linebreak(),
  run_linebreak(), run_linebreak(),
  
  # logo
  external_img(here("figures", "palan.png"),
               height = 1.045, width = 4.975),
  
  run_linebreak(), run_linebreak(), run_linebreak(), run_linebreak(), 
  run_linebreak(), run_linebreak(), run_linebreak(), run_linebreak(), 
  run_linebreak(), run_linebreak(), run_linebreak(),
  
  #TITLE
  ftext("...", 
        prop = bold_face1),
  
  run_linebreak(), run_linebreak(), run_linebreak(), run_linebreak(), 
  run_linebreak(), run_linebreak(), run_linebreak(), run_linebreak(),
  run_linebreak(), run_linebreak(), run_linebreak(),
  
  # AUTHOR AND DATE
  ftext("athorA", prop = bold_face2),
  run_linebreak(),
  ftext("athorB", prop = bold_face2),
  run_linebreak(),
  ftext("athorC", prop = bold_face2),
  
  run_linebreak(), run_linebreak(),
  
  ftext(paste("日期：",Sys.Date(),sep = ""),
        prop = bold_face2),
  
  fp_p = fp_par(text.align = "center"))
```

### 快捷辅助函数：

```R
vi = function(data){
  View(data)}
# vi(iris)

## 2.1.2 计算唯一患者人数：
pe = function(data){
  return(n_distinct(data$patient_id))
}

## 2.1.3 转换格式：num/cha/fac/dat
## 需要注意as_date默认的时间设置时分秒为早晨八点；
tr = function(data,lab,funss){
  if(funss == "num"){
    return(
      data %>% mutate(across(.cols = lab, .fns = as.numeric)))
  }else if(funss == "dat"){
    return(
      data %>% mutate(across(.cols = lab, .fns = as_date)))
  }else if(funss == "cha"){
    return(
      data %>% mutate(across(.cols = lab, .fns = as.character)))
  }else if(funss == "fac"){
    return(
      data %>% mutate(across(.cols = lab, .fns = as.factor)))
  }else{
    return("input function fasle")
  }
}

# iris %>% tr(.,"Sepal.Length","cha")

## 2.1.4 查询文件名：
na = function(data){return(names(data))}

## 2.1.5转化频次 
fr = function(data){
  data = as.numeric(data)
  return(
    paste0(round(data*100,2),"%",sep="")
  )
}
# fr(5)

## 2.1.6 unique():
un = function(data){return(unique(data))}

## 2.1.7 计算平均值和标准差：
## 需要注意是全局已经设置小数为2位；
## data；输入数据，可适配tidyvrse：
## select1: 输入数据，并提取指定行计算平均值和标准差：
mean_sd = function(data,select1){
  data %>% select({{select1}}) %>% 
    summarise(mean = mean({{select1}}),
              sd = sd({{select1}})) %>% 
    mutate(across(where(is.numeric),~ round(.,2)))
}
# iris %>% ms(.,Sepal.Length) %>% as.data.frame()
mean_sd_union = function(data,select1){
  t1 = data %>% select({{select1}}) %>% 
    summarise(mean = mean({{select1}}),
              sd = sd({{select1}})) %>% 
    mutate(across(where(is.numeric),~ round(.,2)))
  return(paste0(t1$mean,"(",t1$sd,")"))
}


## 2.1.8 人数比例：
## 分母：denomination：den
pop_percent = function(data,den){
  out =list()
  out$pat = pe(data)
  out$per = fr(pe(data)/pe(den))
  return(out)
}

pop_per_union = function(data,den){
  union_t = paste0(pe(data),"(",fr(pe(data)/pe(den)),")")
  return(union_t)
}
## prec_bp %>% pp(.,pe(prec))

## 2.1.9 round(2):
round2 = function(data,n=2){
  data %>% 
    mutate(across(where(is.numeric),~ round(.,n)))
}

## 2.1.10 统计visit_id的数量：
vis = function(data){
  data %>% select(visit_id) %>% n_distinct()
}

```

### 快捷统计函数：

```R
## 2.1.7 计算平均值和标准差：
## 需要注意是全局已经设置小数为2位；
## data；输入数据，可适配tidyvrse：
## select1: 输入数据，并提取指定行计算平均值和标准差：
mean_sd = function(data,select1){
  data %>% select({{select1}}) %>% 
    summarise(mean = mean({{select1}}),
              sd = sd({{select1}})) %>% 
    mutate(across(where(is.numeric),~ round(.,2)))
}
# iris %>% ms(.,Sepal.Length) %>% as.data.frame()

## 将平均值和标准差以括号进行合并：
mean_sd_union = function(data,select1){
  t1 = data %>% select({{select1}}) %>% 
    summarise(mean = mean({{select1}}),
              sd = sd({{select1}})) %>% 
    mutate(across(where(is.numeric),~ round(.,2)))
  
  return(paste0(t1$mean,"(",t1$sd,")"))}

## 2.1.8 人数比例：
## 分母：denomination：den
pop_percent = function(data,den){
  out =list()
  out$pat = pe(data)
  out$per = fr(pe(data)/pe(den))
  return(out)
}

pop_per_union = function(data,den){
  union_t = paste0(pe(data),"(",fr(pe(data)/pe(den)),")")
  return(union_t)
}
## prec_bp %>% pp(.,pe(prec))
```

### 描述性分析统计函数

#### 年龄计算

```R
## birthDate: 出生日期：
## refDate： 默认为当前时间，项目需要index_date:
## 输出患者年龄：
calc_age <- function(birthDate, refDate = Sys.Date(), unit = "year") {
  require(lubridate)
  if (grepl(x = unit, pattern = "year")) {
    as.period(interval(birthDate, refDate), unit = 'year')$year
  } else if (grepl(x = unit, pattern = "month")) {
    as.period(interval(birthDate, refDate), unit = 'month')$month
  } else if (grepl(x = unit, pattern = "week")) {
    floor(as.period(interval(birthDate, refDate), unit = 'day')$day / 7)
  } else if (grepl(x = unit, pattern = "day")) {
    as.period(interval(birthDate, refDate), unit = 'day')$day
  } else {
    print("Argument 'unit' must be one of 'year', 'month', 'week', or 'day'")
    NA  }}

```





## Rawdata cleaning pipeline
```R
## 常用线性清理范式（1）
linelist <- linelist_raw %>%
    # standardize column name syntax
    janitor::clean_names() %>% 
    # define class
    mutate(across(contains("date"), as.Date), 
           generation = as.numeric(generation),
           age        = as.numeric(age)) %>% 
    # mutation
    mutate(days_onset_hosp = as.numeric(date_hospitalisation - date_onset)) %>% 
    mutate(hospital = recode(hospital,
	"Mitylira Hopital"  = "Military Hospital",
	"other"             = "Other")) %>% 
    mutate(hospital = replace_na(hospital, "Missing")) %>% 
   	 mutate(age_years = case_when(
          age_unit == "years" ~ age,
          age_unit == "months" ~ age/12,
          is.na(age_unit) ~ age,
          TRUE ~ NA_real_)) %>% 
    mutate(
          age_cat = epikit::age_categories(age_years, breakers = c(0, 5, 10, 15, 20, 30, 50, 70))) %>% 
```

## target_groups
### 加载集成数据集
#### Vroom

```R
library(here)
source(here("R", "common.R"), encoding = "utf-8")

patient_filepath <- fs::dir_ls(here("data", "preprocess", "patient_clean"),
                               glob = "*/*.csv")

diagnosis_filepath <- fs::dir_ls(here("data", "preprocess", "diagnosis_clean"),
                               glob = "*/*.csv")

patient <- vroom(patient_filepath) %>% 
  select(-"...1") %>% 
  distinct()

diagnosis <- vroom(diagnosis_filepath) %>% 
  select(-"...1") %>% 
  distinct()


```

#### Arrow

```R
library(here)
source(here("R", "prepare and analyse data", "prepare.R"), encoding = "UTF-8")

patient <- open_dataset(
  here("data", "preprocess", "lan", "patient_clean")) %>% 
  collect() 

visit <- open_dataset(
  here("data", "preprocess", "lan", "visit_clean")) %>% 
  collect()


```

### 保存数据集

Make sure only patient_id, visit_id and provider_id are kept in the end.

```R
overall_target <- diagnosis %>% 
  filter(patient_type == "住院患者") %>% 
  filter(!sub_group == "other")

target_group_1 <- overall_target %>% 
  filter(sub_group == "非内分泌科就诊") %>% 
  left_join(select(visit, patient_id, visit_id, admission_datetime)) %>%
  group_by(patient_id) %>%
  arrange(admission_datetime) %>% 
  slice_head() %>%
  ungroup() %>%
  select(patient_id, visit_id, provider_id) %>% 
  distinct()


```

### 患者平衡匹配前后信息比较 

This part is for comparing the differences in baseline characteristics of the dataset after using PSM.  Make sure only baseline characteristics are kept in the end. 

```R
table_psm_before <- CreateTableOne(vars = c('年龄', '年龄分组', '性别',
                                       '医院', '医保', '医院等级'),
                              data = data_psm_before,
                              strata = 'strata',
                              factorVars = c('年龄分组', '医保', '性别',
                                             '医院等级'),
                              smd = TRUE
) 

table_psm_before <- print(
  table_psm_before,
  printToggle = FALSE, 
  noSpaces = TRUE, 
  smd = TRUE # add smd
)


table_psm_after <- CreateTableOne(vars = c('年龄', '年龄分组', '性别',
                                       '医院', '医保', '医院等级'),
                              data = data_psm_after,
                              strata = 'strata',
                              factorVars = c('年龄分组', '医保', '性别',
                                             '医院等级'),
                              smd = TRUE
) 

table_psm_after <- print(
  table_psm_before,
  printToggle = FALSE, 
  noSpaces = TRUE, 
  smd = TRUE # add smd
)
```




## dataset_for_analysis

Here is an example:

```R
library(here)
source(here("R", "prepare and analyse data", "lan", "target_groups.R"), encoding = "UTF-8")

data_endpoint3 <- group_endpoint3 %>% 
  left_join(patient) %>% 
  left_join(
    select(visit, patient_id, visit_id, insurance_type)
  ) %>% 
  left_join(
    select(lab, patient_id, visit_id, if_ctn, ctn, if_ckmb, ckmb, if_ck, ck)
  ) %>% 
  left_join(
    select(diagnosis, patient_id, visit_id, "diag1", "diag1", "diag1",  
           "diag1", "diag1", "diag1", "diag1")
  ) %>% 
  left_join(
    select(prescribing, patient_id, visit_id, "diag1", "diag1", "diag1",
           "diag1", "diag1（ACEI）", 
           "diag1（ARB）", start_kangban, end_kangban,
           kangban)
  ) %>% 
  distinct() %>% 
  mutate_at(vars(if_ctn, if_ckmb, if_ck), ~ replace_na(., 0)) 

secondary5 <- rbind(subgroup, overall) %>% 
  apply_labels(fbg_diff = "空腹血糖变化差值",
               fbg_reg = "末次空腹血糖<6.1m/mol",
               age = "年龄",
               sex = "性别",
               insurance_type = "医疗保险",
               region = "就诊地区",
               cad = "冠心病",
               hbp = "高血压",
               stroke = "卒中",
               ckd = "慢性肾功能不全",
               disease_num = "合并疾病数目",
               group_1 = "基础vs.预混",
               treatment_plan = "糖尿病用药方案",
               inpatient_time = "住院时长",
               total_fee = "总花费",
               trt_fee = "治疗费",
               service_fee = "服务费",
               sub_group = "亚组"
               )

write_dataset(
  data_endpoint3, 
  here("data", "analysis", "lan", "endpoint3"),
  format = "parquet")
```


## final_analysis
此部分主要用于深层进阶分析
## present
使用rmarkdown批量输出
```
---
output: 
  officedown::rdocx_document
---

```

```r
knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE)
library(here)
source(here("R", "prepare and analyse data", "lan", "endpoint1.R"), encoding = "utf-8")
```


```r
cover
```

`\newpage`使用其进行换页

```r
toc
block_toc()
```

endpoint one


```r
table_1 %>% 
  as_flex_table() %>% 
  set_table_properties(layout = "autofit")
```

endpoint two


```r
table_3 %>% 
  as_flex_table() %>% 
  set_table_properties(layout = "autofit")
```

<!---BLOCK_LANDSCAPE_START---> 
endpoint three

```r
table_4 %>% 
  as_flex_table() %>% 
  set_table_properties(layout = "autofit")
```
