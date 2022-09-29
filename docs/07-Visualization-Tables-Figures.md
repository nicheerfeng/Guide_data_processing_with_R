# 可视化（表与图）{#Visualization-Tables-Figures}

## 表格可视化

### 快速图表可视化

```
## 这里是生成优化表格的三件套；-好用
iris %>% 
  flextable::flextable() %>%    # convert to pretty image
  flextable::autofit() %>%          # format to one line per row 
  flextable::save_as_docx(path = "tabyl.docx")

## 快速查看表格：- 好用
library(knitr)
library(kableExtra)
kable(iris) %>%
kable_styling(latex_options = "striped")

## 快速生成数据报告：-- 好用
# install.packages("rrtable")
library(rrtable)
## 自动化报表输出：
## 生成报表图片：
df2flextable2( sampleData3 ,vanilla= FALSE )
## 生成描述性统计图片：
mytable2flextable( mytable(Dx~.,data=acs) ,vanilla= FALSE )
## 生成对应统计分类的html格式：
data2HTML(sampleData3) 


## 快速统计建模结果可视化：- 模型；
library(sjPlot)
library(sjmisc)
library(sjlabelled)
tab_model(m1) ## 注意仅支持统计结果，不支持表格；

#################### shiny表格的可视化: #################################
## DT 查看：--复杂使用参见R-data-shiny.md
library(DT)
datatable(iris)

## reactable --复杂使用参见R-data-shiny.md
library(reactable)
reactable(iris)

## datacleanr -- 快速生成数据报告，需要联网环境
它可以处理嵌套的表格，以及空间和时间序列数据。
install.packages("datacleanr")
library(datacleanr)
dcr_app(iris) ## 第一个功能很好用，可以用可视化的方法展示数据内部的结构；


```

### 编辑表
**方法1：**
不仅能够编辑表，还可以实现表数据的增删，以及筛选后导出；

```r
mtcars_new <- DataEditR::data_edit(mtcars,
                        save_as = "mtcars_new.csv")
```

**方法2：**

```r
edit()
```

**方法3：**

```r
# DT::datatable(head(iris), editable = 'cell')
```

### 静态表格优化展示

#### knitr:kableExtra

```
## knitr:kableExtra #### 
library(knitr)
library(kableExtra)
iris2 <- head(iris)
knitr::kable(iris2, col.names = gsub("[.]", " ", names(iris)))

## 指定对齐：
knitr::kable(iris2, align = "lccrr")
## 添加标题：
knitr::kable(iris2, caption = "An example table caption.")
## 添加范式：
kable(iris) %>%
  kable_styling(latex_options = "striped")

## 设置字体：
kable(head(iris, 5), booktabs = TRUE) %>%
  kable_styling(font_size = 8)

## 分组行/列
iris2 <- iris[1:5, c(1, 3, 2, 4, 5)]
names(iris2) <- gsub('[.].+', '', names(iris2))
kable(iris2, booktabs = TRUE) %>%
  add_header_above(c("Length" = 2, "Width" = 2, " " = 1)) %>% 
  add_header_above(c("Measurements" = 4, "More attributes" = 1))

## 进阶应用：
(ipip50_nested <- ipip50_nested %>%
  mutate(model = map(data, ~lm(o.value ~ t.value, data = .))))

(ipip50_nested <- ipip50_nested %>%
  mutate(tidy = map(model, broom::tidy)))

(tab <- ipip50_nested %>%
  unnest(tidy, .drop = T) %>%
  select(Trait:std.error) %>%
  rename(b = estimate, SE = std.error) %>%
  gather(key = tmp, value = value, b, SE) %>%
  unite(tmp, outcome, tmp, sep = ".") %>%
  spread(key = tmp, value = value))

tab %>% select(-Trait) %>%
  kable(., "html", booktabs = T, escape = F, digits = 2,
        col.names = c("Term", rep(c("b", "SE"), times = 3))) %>%
  kable_styling(full_width = F) %>%
  column_spec(2:7, width = "2cm") %>%
  group_rows("Agreeableness",1,2) %>% ## 添加新行，分组，并重命名：
  group_rows("Conscientiousness",3,4) %>%
  group_rows("Extraversion",5,6) %>%
  group_rows("Neuroticism",7,8) %>%
  group_rows("Openness",9,10) %>%
## 指定标题行的分类位置；1和2表示占位符，按顺序向后推；
  add_header_above(c(" " = 1, "BMI" = 2, "Exercise" = 2, "Log Median Income" = 2))

## 生成多表：
knitr::kable(
  list(
    head(iris[, 1:2], 3),
    head(mtcars[, 1:3], 5)
  ),
  caption = 'A Tale of Two Tables.', booktabs = TRUE
)

```

#### flextable() 静态动态均支持

```
data6 %>%
  tabyl(insurance_type,sex,) %>%
  adorn_totals(where="row") %>%
  adorn_totals(where="col") %>%
  adorn_percentages(denominator = "col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position="front") %>%
  adorn_title(
    row_name = "insurance_type",
    col_name = "Gender",
    placement="combined"
  )->table2
mytable2<-flextable(table2)
mytable2
```

#### 高度自定义gt()

```
## 参见;
https://themockup.blog/posts/2020-09-04-10-table-rules-in-r/
https://themockup.blog/posts/2020-09-26-functions-and-themes-for-gt-tables/#gt-tables

## 这个博客主人的博客原文代码：
## 很有意义，可以用来学习写日常的rmarkdown；
https://github.com/jthomasmock/themockup-blog/blob/master/posts/2020-09-26-functions-and-themes-for-gt-tables/index.qmd
```



### 动态表格优化展示

#### DT-reactable

```r
## 参见资料：
https://thinkr.fr/tableaux-interactifs-avec-r-pour-shiny-et-vos-pages-web/
## data-clean-table
https://clarewest.github.io/blog/post/making-tables-shiny/

## DT包：- - -专注于展示表格相关的数据结构-使用shiny；
## 参见：https://rstudio.github.io/DT/
## 其他复杂用法参见：
library(DT)
datatable(iris)
## 使用论文线表来可视化数据结构；
datatable(head(iris), class = 'cell-border stripe')
## 表格编辑：
DT::datatable(head(iris), editable = 'cell')

## 表格筛选可视化
DT::datatable(head(iris), editable = list(
  target = 'row', disable = list(columns = c(1, 3, 4))
))
## 结合shiny来使用datatale()
library(shiny)
ui <- fluidPage(titlePanel("DT table in Shiny"),
                mainPanel(width = 12,
                          DT::dataTableOutput("mytable")))
server <- function(input, output) {
  output$mytable <- DT::renderDataTable(villagers,
                                        options = list(scrollX = TRUE),
                                        rownames = FALSE)
}
# Run the application
shinyApp(ui = ui, server = server)
## DT中相关参数：
cell-border：用于单元格的实心边框
compact: 减少行间距
hover: 悬停在光标上时突出显示行
nowrap：删除单元格中的换行符
order-column：突出显示表格排序所依据的列
row-bordercell-border: 仅用于列顶部和底部的边框（出于明显原因，只能同时使用）
stripe：用于“条纹”线，即两种交替的颜色
display: 对于集合stripe, hover,row-border和order-column
# 组合使用：
iris %>%
  datatable(class = "cell-border compact hover order-column")
```


#### reactable 包 -- - - -专注于展示表格相关的数据结构-使用shiny；

```r
## install.packages("reactable")
## 参见；https://glin.github.io/reactable/
library(reactable)

## 直接使用：
reactable(iris)


## 结合shiny来使用reactable；
library(shiny)
library(reactable)

ui <- fluidPage(
  reactableOutput("table")
)
server <- function(input, output) {
  output$table <- renderReactable({
    reactable(iris)
  })
}
shinyApp(ui, server)

## 在Rmarkdwon中使用reactable()

library(reactable)
reactable(iris)
```



#### sparkline包生成交互式小型图表：

```r
## 这个功能很有用：
library(sparkline)
iris_spark <- iris %>%
  group_by(Species) %>%
  summarise(Sepal.Length = list(Sepal.Length),
            Sepal.Width = list(Sepal.Width), 
            Petal.Length = list(Petal.Length),
            Petal.Width = list(Petal.Width)) 
iris_spark %>%  
  reactable(
    columns = list(
      Sepal.Length = colDef(cell = function(values) {
        sparkline(values, type = "bar")
      }),
      Sepal.Width = colDef(cell = function(values, index) {
        sparkline(iris_spark$Sepal.Width[[index]])
      }),
      Petal.Length = colDef(cell = function(values, index) {
        sparkline(iris_spark$Petal.Length[[index]], type = "box")
      }),
      Petal.Width = colDef(cell = function(values, index) {
        sparkline(iris_spark$Petal.Width[[index]])
      })
    )
  )

## 在表格的默认添加交互式图表：
iris %>% 
  reactable(
    defaultPageSize = 5, ## 指定显图表行数；
    defaultColDef = colDef(footer = function(values) {
      if (!is.numeric(values)) return()
        ## 添加末尾的交互式图表：
      sparkline(values, type = "box", width = 100, height = 30)
    })
  )
```

#### formattable()添加颜色


```r
## formattable ########
install.packages("formattable")
library(formattable)

df <- data.frame(
  id = 1:10,
  name = c("Bob", "Ashley", "James", "David", "Jenny",
           "Hans", "Leo", "John", "Emily", "Lee"),
  age = c(28, 27, 30, 28, 29, 29, 27, 27, 31, 30),
  grade = c("C", "A", "A", "C", "B", "B", "B", "A", "C", "C"),
  test1_score = c(8.9, 9.5, 9.6, 8.9, 9.1, 9.3, 9.3, 9.9, 8.5, 8.6),
  test2_score = c(9.1, 9.1, 9.2, 9.1, 8.9, 8.5, 9.2, 9.3, 9.1, 8.8),
  final_score = c(9, 9.3, 9.4, 9, 9, 8.9, 9.25, 9.6, 8.8, 8.7),
  registered = c(TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE),
  stringsAsFactors = FALSE)

###  动态结合表筛选过程展示建模结果
formattable(df, list(
  age = color_tile("white", "orange"),
  grade = formatter("span", style = x ~ ifelse(x == "A",
                                               style(color = "green", font.weight = "bold"), NA)),
  area(col = c(test1_score, test2_score)) ~ normalize_bar("pink", 0.2),
  final_score = formatter("span",
                          style = x ~ style(color = ifelse(rank(-x) <= 3, "green", "gray")),
                          x ~ sprintf("%.2f (rank: %02d)", x, rank(-x))),
  registered = formatter("span",
                         style = x ~ style(color = ifelse(x, "green", "red")),
                         x ~ icontext(ifelse(x, "ok", "remove"), ifelse(x, "Yes", "No")))
))

## 补充，formattable() 还可以结合DT的结果来输出形成阅读性较高的动态表格；
formattable(
  pic_items,
  list(
    `sell_value` = color_tile("white", "pink"),
    `buy_value` = color_bar("lightblue"),
    orderable = true_false_formatter
  )
) %>%
  as.datatable(escape = FALSE,
               options = list(scrollX = TRUE),
               rownames = FALSE)

## 另外一种颜色梯度的表格用法：
library(formattable)
products <- data.frame(id = 1:5, 
                       price = c(10, 15, 12, 8, 9),
                       rating = c(5, 4, 4, 3, 4),
                       market_share = percent(c(0.1, 0.12, 0.05, 0.03, 0.14)),
                       revenue = accounting(c(55000, 36400, 12000, -25000, 98100)),
                       profit = accounting(c(25300, 11500, -8200, -46000, 65000)))
sign_formatter <- formatter("span", 
                            style = x ~ style(color = ifelse(x > 0, "green", 
                                                             ifelse(x < 0, "red", "black"))))
products %>%
  formattable(list(
    price = color_tile("transparent", "lightpink"),
    rating = color_bar("lightgreen"),
    market_share = color_bar("lightblue"),
    revenue = sign_formatter,
    profit = sign_formatter))
```



### 描述性统计表格分析
#### table1

**table1包：**
参见网页：https://cran.r-project.org/web/packages/table1/vignettes/table1-examples.html

特色：既可以指定对应的函数，也可以利用现成的分类数据进行平局值和标准差计算；


```r
library(table1)
library(boot) 
melanoma2 <- melanoma

# Factor the basic variables that
# we're interested in
melanoma2$status <- 
  factor(melanoma2$status, 
         levels=c(2,1,3),
         labels=c("Alive", # Reference
                  "Melanoma death", 
                  "Non-melanoma death"))
melanoma2$sex <- 
  factor(melanoma2$sex, levels=c(1,0),
         labels=c("Male", 
                  "Female"))
 
melanoma2$ulcer <- 
  factor(melanoma2$ulcer, levels=c(0,1),
         labels=c("Absent", 
                  "Present"))

label(melanoma2$sex)       <- "Sex"
label(melanoma2$age)       <- "Age"
label(melanoma2$ulcer)     <- "Ulceration"
label(melanoma2$thickness) <- "Thickness"

## 这两个unit就很有用；在输出表格中很有用；
units(melanoma2$age)       <- "years"
units(melanoma2$thickness) <- "mm"

##  注意这里的分组变量使用的|status 。另外，像sex之类的变量，还可以设计为factor(sex)
table1(~ sex + age + ulcer + thickness | status, data=melanoma2, overall="Total")

## 还可以指定函数，用于表格中参数的计算：
## 指定函数另外参见：https://zhuanlan.zhihu.com/p/466024679

my.render.cont <- function(x) {
    with(stats.apply.rounding(stats.default(x), digits=2), c("",
        "Mean (SD)"=sprintf("%s (&plusmn; %s)", MEAN, SD)))
}
my.render.cat <- function(x) {
    c("", sapply(stats.default(x), function(y) with(y,
        sprintf("%d (%0.0f %%)", FREQ, PCT))))
}
                 
 table1(strata, labels, groupspan=c(1, 1, 2),
       render.continuous=my.render.cont, render.categorical=my.render.cat) 
                 
## table1还支持输出表格样式：
 table1(~ age + sex + wt | treat, data=dat, topclass="Rtable1-zebra")
```

#### janitor包`tabyl()`功能

```r
### janitor包提供了生成表格和交叉表格的tabyl()功能，可以“装饰”或使用辅助功能进行修改，以显示百分比、比例、计数等。
pacman::p_load(
  rio,          # File import
  here,         # File locator
  skimr,        # get overview of data
  tidyverse,    # data management + ggplot2 graphics 
  gtsummary,    # summary statistics and tests
  rstatix,      # summary statistics and statistical tests
  janitor,      # adding totals and percents to tables
  scales,       # easily convert proportions to percents  
  flextable     # converting tables to pretty images
  )
linelist %>%                                  
  tabyl(age_cat, gender) %>%   ## 交叉统计             
  adorn_totals(where = "row") %>%     ## 添加汇总列：    
  adorn_percentages(denominator = "row") %>%   ## 将计数转为比例；按行或列
  adorn_pct_formatting(digits = 1)   %>%  ## 将比例转为百分比；        
  adorn_ns(position = "front") %>%            # display as: "count (percent)"
  adorn_title(                                # adjust titles
    row_name = "Age Category", ## 这里是汇总命名的方法；
    col_name = "Gender",
    placement = "combined") %>% 
  flextable::flextable() %>%    # convert to pretty image
  flextable::autofit() %>%          # format to one line per row 
  flextable::save_as_docx(path = "tabyl.docx")
```

#### tableone


```r
library(survival)
library(tableone)
library(skimr)
## Load data

data(pbc)
pbc=pbc %>% as.tbl() %>% 
  mutate(trt=factor(trt,labels = c("yes","no")),
         status=factor(status, labels =c("status","edema","stage")))

df=pbc %>% as.tbl() %>% 
  mutate(trt=factor(trt,labels = c("yes","no")),
         status=factor(status, labels =c("status","edema","stage"))) %>% select(time,age,sex,status,trt,bili,platelet)

df
# A tibble: 418 x 7
    time   age sex   status trt    bili platelet
   <int> <dbl> <fct> <fct>  <fct> <dbl>    <int>
 1   400  58.8 f     stage  yes    14.5      190
 2  4500  56.4 f     status yes     1.1      221

# … with 408 more rows
## 构建统计展示：
T1=CreateTableOne(data=df,strata = c("trt"))
print(T1,showAllLevels = TRUE)

## 指定部分检验结果：
# 有时候，并不是所有数据都是服从正态分布，频数分布有时候也需要用到Fisher 检验，这时候需要我们自定义了。
# 这里在Print需要用nonnormal来指定哪些变量为非正态分布及exact 来指定确切概率法检验
T3=print(T1,showAllLevels = TRUE,nonnormal = c("bili","platelet"),
         exact = "sex")
```

#### compareGroups包


```r
## 参见：https://www.jianshu.com/p/8879d49064ae

## 功能：
可支持多种数据导入，如haven、readxl、readr等，也接受Tibble类型数据集。
内置descrTable的新函数，只需一步就可以构建描述性表。
支持R-markdown文档，支持HTML的分层表。
内置strataTable的新功能，可以按层(变量的值或级别)构建描述性表。
日期变量被视为连续非正态，执行中位数、四分位数和非参数检验。
在compareGroups和descrTable中添加新的参数var.equal。这允许在比较两组以上的比较。

## 基础函数数据值展示：
library(compareGroups)
library(tidyverse)
data(predimed)
head(predimed)
# ALL data
descrTable( ~ ., data = predimed)

res <- compareGroups(group ~ age + sex + smoke + waist + hormo, 
    data = predimed)
res
createTable(res) ### 结果输出展示；


## 函数输出默认为正态分布：
如果有非正态分布；我们需要对非正态分布进行指定，使用下面方法进行指定。
这里method 变量=1表示比较使用正态分布，
变量=2表示使用四分位间距，
变量=3表示使用分类变量比较，
变量=NA表示自动根据Shapiro-Wilks检测，做出是正态还是非正态方法
createTable(res)
## add non-normal test
res=compareGroups(group ~ age + smoke + waist + hormo, data = predimed, 
                  method = c(waist = 2, age = 2))
createTable(res)

## OR或HR的展示
res1 <- compareGroups(htn ~ age + sex + bmi + smoke, data = predimed, 
    ref = 1)
createTable(res1, show.ratio = TRUE)

# HR展示：
library(survival)
predimed$tmain <- with(predimed, Surv(toevent, event == "Yes"))
createTable(compareGroups(tmain ~ group + age + sex, data = predimed), 
            show.ratio = TRUE)

## 输出xlxs格式：
export2xls(createTable(res), file='table1.xlsx')
```

#### gtsummary()

##### 基础配置


```r
### 常用默认主题配置：
reset_gtsummary_theme()
theme_gtsummary_language(
  language =  "zh-cn",
  decimal.mark = NULL,
  set_theme = TRUE)
```

##### 描述性统计

###### 5.5.2.1 基础描述性统计


```r
### tbl_summary式样，使用by分组统计：
### 输出HTML格式的表格报告：
t1 <- data_table4  %>%
  tbl_summary(by=druggroup,
              type = list(all_continuous() ~ 'continuous2',
                          all_dichotomous() ~ "dichotomous"),
              statistic = all_continuous()~c(
                "{mean} ({sd})",
                "{median} ({p25}, {p75})",
                "{min}, {max}"),
              ## 设定表格中小数的位数
              digits = list(c(sex,age_cat,insurance_type)~2),
              label=list(
                sex~"性别",
                age~"入组年龄",
                age_cat~"年龄组分布",
                insurance_type~"医保类型",
                lab_item_name.x~"肌钙蛋白cTn检查  [人数(比例)]",
              ),
              missing_text = "Missing") %>% 
              as_gt() %>% 
              tab_spanner(
             label="DRUG GROUP",
             columns = starts_with("stat_")
) %>% 
  gtsave("t1.html",path="E:/projects/RX021PD0908/coronary/yunxian.report")

## 细节- 添加表格的次级合并分组：
as_gt() %>%  ## 还可以进行分组生成一级标题：
  tab_spanner(
    label = "用药分组",
    columns = starts_with("stat_")  )
```


```r
tbl_strata_ex1 <-
  trial %>%
  select(age, grade, stage, trt) %>%
  mutate(grade = paste("Grade", grade)) %>%
  tbl_strata(
    strata = grade,
    .tbl_fun =
      ~ .x %>%
      tbl_summary(by = trt, missing = "no") %>%
      add_n(),
    .header = "**{strata}**, N = {n}"
  )


# Example 2 ----------------------------------
tbl_strata_ex2 <-
  trial %>% 
  select(grade, response) %>%
  mutate(grade = paste("Grade", grade)) %>%
  tbl_strata2(
    strata = grade,
    .tbl_fun =
      ~.x %>%
      tbl_summary(
      ## 这里的response
        label = list(response = .y),
        missing = "no",
        statistic = response ~ "{p}%") %>%
      add_ci(pattern = "{stat} ({ci})") %>%
      modify_header(stat_0 = "**Rate (95% CI)**") %>% 
    ## 这里上面一步的%>% 也可以使用逗号来替换，但会增加一个百分号注释；
      modify_footnote(stat_0 = NA),
    ## 这个函数的作用是提取及合并数据：
    .combine_with = "tbl_stack",
    ## 这一步是删除空白行：
    .combine_args = list(group_header = NULL),
    .quiet = TRUE) %>%
  modify_caption("**Response Rate by Grade**")
```

###### 5.5.2.2 组间描述性统计（P值）


```r
 trial %>%
  tbl_summary(
    by = trt,
    ## type = all_continuous() ~ "continuous2" ：将连续变量统计描述结果展示为多行，type设置为"continuous2"；  
    type = all_continuous() ~ "continuous2",
    statistic =all_continuous() ~ c( "{mean} ({sd})",
                                     "{median} ({p25}, {p75})",  "{min}, {max}"),
    missing="always", ## 强制展示缺失值是否存在
    missing_text='missing')%>%  ## 修改缺失值的显示标签
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 2),
        ## 定义检验形式：，组间比较，连续变量默认检验方法为秩和检验，分类变量为卡方检验，
        list(all_continuous() ~ "t.test",
             all_categorical() ~ "fisher.test")) %>%
  add_n() %>% ## 添加每个样本的数量描述性统计 
  add_overall %>%  
  modify_header(label = "**Variable**")  %>% ## 更新列名；
  ## 添加表格标题
  modify_caption("Patient Characteristics")  %>%
  ## 添加尾注：还有一种方法是直接使用modify_footnote
  # as_gt() %>% #使用gt存在的问题是无法使用flex_table()
  #  gt::tab_source_note(gt::md("*This data is simulated*")) 
  ## 表格导出保存;
  as_flex_table() %>%
  flextable::save_as_docx(.,path='C:/Users/DELL/tab_2.docx') 	
```

##### 回归分析

###### 5.5.3.1 线性回归/逻辑回归


```r
##  Linear regression
table_35 <- glm(inpatient_time ~ fbg_diff + fbg_reg + age + sex  + region + cad + hbp + stroke + ckd +  group_1 + treatment_plan,
               family = "gaussian", data = secondary5) %>% 
  tbl_regression(
    label = list(
      fbg_diff ~ "空腹血糖变化差值",
      # pbg_diff ~ "餐后血糖变化差值",
      fbg_reg ~ "末次空腹血糖<6.1mmol/l",
      age ~ "年龄",
      sex ~ "性别",
      # insurance_type ~ "医疗保险",
      region ~ "地区",
      cad ~ "冠心病",
      # disease_num ~ "合并疾病数目",
      group_1 ~ "基础vs.预混",
      treatment_plan ~ "糖尿病用药方案")) %>% 
  bold_p(0.05) %>% 
  modify_header(label = "**变量**")

table_35

## 逻辑回归：
mod1 <- glm(response ~ trt + age + grade, trial, family = binomial)
t1 <- tbl_regression(mod1, exponentiate = TRUE)

## tab_regression参数解析：
tbl_regression(
  x,
  label = NULL, ## 添加标签：
  exponentiate = FALSE,
  include = everything(),
  show_single_row = NULL,  ## 单行或多行展示，例如性别是否要分组展示等情况；
  conf.level = NULL,
  intercept = FALSE,
  estimate_fun = NULL,
  pvalue_fun = NULL,
  tidy_fun = NULL,
  add_estimate_to_reference_rows = FALSE,
  show_yesno = NULL,
  exclude = NULL,
  ...
)

## 使用add_glance_table来输出函数结果中更多的参数：
## add_glance_table()来自一个补充的R包：
包括模型的R方、AIC信息、样本量等，仅能适用于lm函数，不适合glm
mod_va<-lm(retinol_μmolL~sex+age_cut+Region+SBP+DBP,data=my2) %>% tbl_regression()
mod_va%>% add_glance_table(
  label = list(sigma ~ "\U03C3"),
   include = c(r.squared, AIC, sigma,nobs))
```

###### 5.5.3.2 生存分析


```r
library(survival)
mod1 <- glm(response ~ trt + age + grade, trial, family = binomial)

t1 <- tbl_regression(mod1, exponentiate = TRUE)
# build survival model table
t2 <-
  coxph(Surv(ttdeath, death) ~ trt + grade + age, trial) %>%
  tbl_regression(exponentiate = TRUE)

# merge tables 
tbl_merge_ex1 <-
  tbl_merge(
    tbls = list(t1, t2),
    tab_spanner = c("**Tumor Response**", "**Time to Death**")
  )
```

##### 自定义函数输出

###### 5.5.4.1 自定义相关系数


```r
作者：一个柒
链接：https://zhuanlan.zhihu.com/p/506580074
来源：知乎
著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。

# 示例中，vars为所有自变量，`SUA`为因变量，数据集为ds
# vars <- c("age","BMI")
 
fun_spearman_r <- function(data, variable, ...) {
  r = cor.test(data[[variable]],data$`SUA`,method = "spearman")$estimate
  dplyr::tibble(r = r)
}
# 此处tibble(r=r)很必要
fun_spearman_p <- function(data, variable, ...) {
  cor.test(data[[variable]],data$`SUA`,method = "spearman")$p.value
}
# p不需要(p=p)，因为r为主体自定义函数，p为add_stat
 
ds[vars]<- sapply(ds[vars],as.numeric)
tbl_custom_summary_ex <- ds %>%
    select(all_of(vars),SUA) %>%
  ## tbl_custom_summary:输入自定义函数并且能解析tidyr的结果:
    tbl_custom_summary(include = all_of(vars),
                       stat_fns = everything() ~ fun_spearman_r,
                       statistic = everything() ~ "{r}",
                       digits = everything() ~ 3, # 保留3位小数
                       type = list(everything() ~ "continuous"),
                       missing = "no",
                       label = list(`age` ~ "年龄",
                                    `BMI` ~ "身体质量指数(BMI)")) %>%
    add_stat(fns = everything() ~ fun_spearman_p) %>%
    ## 不太理解，这里也要使用自定义的函数来输入数据：
    modify_fmt_fun(add_stat_1 ~ function(x) style_pvalue(x, digits = 3)) %>% 
	# p值如果先保留小数再style_pvalue会出现本来应`<0.001`但变成`0.001`的情况
    modify_header(label = '**变量**',stat_0 ~ "**r**", add_stat_1 ~ "**P值**") %>%
    bold_labels() %>%
    modify_footnote(all_stat_cols() ~ NA) # 不要脚注
```

###### 5.4.4.2 效应值计算

```r
### 效应值计算函数方法1： 
library(gtsummary)
library(tidyverse)

# function that returns either Cohen's D or the 1988 interpretation of its size
CohenD <- function(data, variable, by, interpret_d = FALSE, ...) {
  # Cohen's d, Hedges's g (correction=TRUE or FALSE) and Glass’s delta
  ES <- effectsize::cohens_d(data[[variable]], factor(data[[by]]),
                             ci=.90,
                             pooled_sd=TRUE,
                             paired=FALSE,
                             correction=TRUE)

  if (interpret_d == TRUE)
    return(stringr::str_glue("{effectsize::interpret_d(ES$Cohens_d, rules = 'cohen1988')}"))
  
  # Formatting statistic with CI
  est <- style_sigfig(ES$Cohens_d)
  ci_lower <- style_sigfig(ES$CI_low)
  ci_upper <- style_sigfig(ES$CI_high)

  # Returning estimate with CI together
  stringr::str_glue("{est} ({ci_lower}, {ci_upper})")
}
CohenD_interpret_d <- purrr::partial(CohenD, interpret_d = TRUE)

trial %>%
  select(trt, age, marker) %>%
  tbl_summary(by = trt, missing = "no", statistic = all_continuous() ~ "{mean} ± {sd}") %>%
  add_p(test = everything() ~ t.test) %>%
  add_stat(fns = everything() ~ CohenD, header = "**ES (90% CI)**") %>%
  add_stat(fns = everything() ~ CohenD_interpret_d, header = "**Interpretation**")
```


```r
## 效应值计算函数方法2：
## 使用使用该add_stat()函数的方法：增加额外的函数或者置信区间：效应大小[90％CI]
## 关于效应大小的计算：
效应量可以解决P值无法刻画相关程度大小和差异大小的问题，也可以避免“P值操控”现象。
效应量衡量实验真实效果大小或者变量关联强度的指标, 它不受样本容量大小的影响 。
依据效应量的大小, 能够判断具有显著差异的研究结果是否具有实际意义或重要性。
P值代表的是统计学上的意义，而效应量是能反映实际上的意义，有时候即使有显著的统计学意义，但是效应量却可以很小。



library(tidyverse)
library(gtsummary)

my_EStest <- function(data, variable, by, ...) {
  # Cohen's D
  d <- effsize::cohen.d(data[[variable]] ~ as.factor(data[[by]]), 
                   conf.level=.90, pooled=TRUE, paired=FALSE, 
                   hedges.correction=TRUE)
  # Formatting statistic with CI
  est <- style_sigfig(d$estimate)
  ci <- style_sigfig(d$conf.int) %>% paste(collapse = ", ")
  # returning estimate with CI together
  str_glue("{est} ({ci})")
}

add_ES <-
  trial %>%
  select(trt, age) %>%
  tbl_summary(by = trt, missing = "no",
              statistic = list(all_continuous() ~ "{mean} ({sd})"),
              digits = list(all_continuous() ~ c(1,1))) %>%
  add_p(test = everything() ~ t.test) %>%
  add_stat(
    fns = everything() ~ my_EStest,
    fmt_fun = NULL,
    header = "**ES (90% CI)**") %>%
  ## 添加尾注：
  modify_footnote(add_stat_1 ~ "Cohen's D (90% CI)")
```

##### 辅助/ 参数化


```r
 # 合并表格后：remove spanning headers
  modify_spanning_header(everything() ~ NA)

# 控制试验组和安慰剂组的位置顺序：
trial %>%
  select(trt, age, marker) %>%  #
  mutate(trt = forcats::fct_rev(trt)) %>%   # reverse the ordering of trt variable
  tbl_summary(by = trt)

## 两种合并表格的方法：
## 横向合并表格：
table_11 <-
  tbl_merge(tbls = list(a, b, c),
    tab_spanner = c("**非内分泌科就诊**", "**内分泌科就诊**", "**总共**") ) 
## 纵向合并表格：
tbl_stack(list(tbl1, tbl2), group_header = c("versicolor", "virginica"))

## gtsummary自动化参数命名：
使用attr(.,"label")给参数变量命名，然后gtsummary会优先从列的label中读取描述。

## 控制列宽：
library(gt)
library(gtExtras)
library(dplyr)

cols_fn <- function(data, y) {
  data %>%
    select(1:4) %>%
    gt() %>%
    gt_theme_espn() %>%
    cols_width(as.formula(glue::glue("4~px({y})")))}

cols_fn(head(mtcars), y = 900)

## gtsmmary的主题设计：
https://cran.r-project.org/web/packages/gtsummary/vignettes/themes.html
```

#### finalfit(简易高效)


```r
library(finalfit)
dependent <- "ulcer.factor"
explanatory <- c("age", "sex.factor", "year", "t_stage.factor")
melanoma %>% 
## 自动对分组变量和连续变量求p值，类似于tableone;
  summary_factorlist(dependent, explanatory, p = TRUE,
                     add_dependent_label = TRUE)
```



### 统计结果表格可视化

#### sjplot()


```r
## 参见：https://strengejacke.github.io/sjPlot/index.html
sjPlot - Data Visualization for Statistics in Social Science

# install.packages("sjPlot") ####
## 支持类型出表：
# 经典回归、贝叶斯回归、混合效应回归；

## 支持类型出图：
## 支持回归系数、边际效应和交互式出图；

## 支持表格和图形的论文化出图：
## 支持灰度图：
使用colors = "gs"，展示数据灰度；
使用colors = "bw"，展示数据线条类型；

## 支持论文发表为主题的自定义表格输出图
# 可以在一定程度上优化现有的ggplot2体系；
# 比调整ggplot2的代码更便捷；

# load package
library(sjPlot)
library(sjmisc)
library(sjlabelled)

# sample data
data("efc")
efc <- as_factor(efc, c161sex, c172code)

m1 <- lm(barthtot ~ c160age + c12hour + c161sex + c172code, data = efc)
m2 <- lm(neg_c_7 ~ c160age + c12hour + c161sex + e17age, data = efc)

## 自动聚类分组出统计表：太牛皮了吧。。。
tab_model(m1)
tab_model(m1, m2) ## 多组统计表

## 支持各种统计过程汇总参数再导出：
## 添加se/sts
tab_model(m1, show.se = TRUE, show.std = TRUE, show.stat = TRUE)
##  关闭p值等操作；
tab_model(m3, m4, show.ci = FALSE, show.p = FALSE, auto.label = FALSE)
```

#### mtable


```r
# we will define a fake model which includes all the IVs
mtables <- mtable("Model fake" = lm(sr ~ dpi + ddpi + log(dpi) + log(ddpi), LifeCycleSavings),
                  "Model 1" = lm(sr ~ dpi + pop15 + pop75, LifeCycleSavings),
                  "Model 2" = lm(sr ~ ddpi + pop15 + pop75, LifeCycleSavings),
                  "Model 3" = lm(sr ~ log(dpi) + pop15 + pop75, LifeCycleSavings),
                  "Model 4" = lm(sr ~ log(ddpi) + pop15 + pop75, LifeCycleSavings))
# we will display mtables without the fake model
mtables[2:5]
# output
Calls:
Model 1: lm(formula = sr ~ dpi + pop15 + pop75, data = LifeCycleSavings)
Model 2: lm(formula = sr ~ ddpi + pop15 + pop75, data = LifeCycleSavings)
Model 3: lm(formula = sr ~ log(dpi) + pop15 + pop75, data = LifeCycleSavings)
Model 4: lm(formula = sr ~ log(ddpi) + pop15 + pop75, data = LifeCycleSavings)

=====================================================================
                    Model 1      Model 2     Model 3      Model 4    
---------------------------------------------------------------------
  (Intercept)       31.457***    28.125***    36.304**    26.118***  
                    (7.482)      (7.184)     (10.511)     (7.416)    
  dpi               -0.001                                           
                    (0.001)                                          
  ddpi                            0.428*                             
                                 (0.188)                             
  log(dpi)                                    -0.779                 
                                              (1.018)                
  log(ddpi)                                                1.584*    
                                                          (0.722)    
  pop15             -0.492**     -0.452**     -0.506**    -0.408**   
                    (0.149)      (0.141)      (0.154)     (0.144)    
  pop75             -1.568       -1.835       -1.649      -1.663     
                    (1.121)      (0.998)      (1.110)     (1.009)    
---------------------------------------------------------------------
  R-squared          0.274        0.337        0.271       0.332     
  adj. R-squared     0.227        0.293        0.223       0.288     
  sigma              3.939        3.767        3.948       3.780     
  F                  5.797        7.778        5.700       7.608     
  p                  0.002        0.000        0.002       0.000     
  Log-likelihood  -137.410     -135.171     -137.525    -135.355     
  Deviance         713.767      652.606      717.054     657.424     
  AIC              284.821      280.341      285.050     280.709     
  BIC              294.381      289.902      294.610     290.269     
  N                 50           50           50          50         
=====================================================================
```

#### stargazer


```r
# stargazer can be an option
library(stargazer)
stargazer(lm(sr ~ dpi + pop15 + pop75, LifeCycleSavings),
          lm(sr ~ ddpi + pop15 + pop75, LifeCycleSavings),
          lm(sr ~ log(dpi) + pop15 + pop75, LifeCycleSavings),
          lm(sr ~ log(ddpi) + pop15 + pop75, LifeCycleSavings),
          type = "text", column.labels = c("Model 1", "Model 2", "Model 3", "Model 4"),
          model.numbers = FALSE)
# output
=====================================================================
                                        Dependent variable:          
                              ---------------------------------------
                                                sr                   
                               Model 1   Model 2   Model 3   Model 4 
---------------------------------------------------------------------
dpi                            -0.001                                
                               (0.001)                               

ddpi                                     0.428**                     
                                         (0.188)                     

log(dpi)                                           -0.779            
                                                   (1.018)           

log(ddpi)                                                    1.584** 
                                                             (0.722) 

pop15                         -0.492*** -0.452*** -0.506*** -0.408***
                               (0.149)   (0.141)   (0.154)   (0.144) 

pop75                          -1.568    -1.835*   -1.649    -1.663  
                               (1.121)   (0.998)   (1.110)   (1.009) 

Constant                      31.457*** 28.125*** 36.304*** 26.118***
                               (7.482)   (7.184)  (10.511)   (7.416) 

---------------------------------------------------------------------
Observations                     50        50        50        50    
R2                              0.274     0.337     0.271     0.332  
Adjusted R2                     0.227     0.293     0.223     0.288  
Residual Std. Error (df = 46)   3.939     3.767     3.948     3.780  
F Statistic (df = 3; 46)      5.797***  7.778***  5.700***  7.608*** 
=====================================================================
Note:                                     *p<0.1; **p<0.05; ***p<0.01
```





### 补充表格包
#### CROSS-TABLE()

```r
# 不太好用，暂存：
library(crosstable)
# 参见网址：https://cran.r-project.org/web/packages/crosstable/vignettes/crosstable.html
# 需要指定分类变量后再批量转为表格导出：
crosstable(mtcars2, c(mpg, cyl), by=am) %>%
  as_flextable(keep_id=TRUE)
```
#### tableone

```r
library(tableone)
install.packages("tableone")
tab2 <- CreateTableOne(data=iris)  
print(tab2) ## 返回(mean (SD))
```

## 图形可视化

### 画图参考网站

```
https://r-graph-gallery.com/

https://chart-studio.plotly.com/feed/?q=plottype:violin#/

## 配色：
https://c.runoob.com/front-end/55/


## 动态地图可视化：
https://rstudio.github.io/leaflet/markers.html

## 其他类型的可视化方案：
https://vultr.ywbb.com/4186.html
```

### 免代码绘制

#### esquisse包

```r
免代码ggplot 生成器，鼠标点点点就能在R-studio里面画图。
类似BI工具，支持代码导出：
esquisse::esquisser() ## 有局限性；
```

#### ggThemeAssist包
调节ggplot对象的神器, 免代码调节ggplot的细节参数，比如字体，背景颜色等

````r
library(ggplot2)
library(ggThemeAssist)
使用mtcars生成一个点图示例
注意这里必须指定定义为gg输出后，再输入；
```gg <- ggplot(mtcars, aes(x = hp, y = mpg, colour = as.factor(cyl))) + geom_point()  ```
开始调整主题
**ggThemeAssistGadget(gg)** ##这个更好用一些
````
#### ggplotgui包
免代码绘图，导入数据，根据已有类型图绘图！
**提供R包包括：**
仅实现了广泛使用的图表：箱线图、密度图、点 + 误差图、点图、直方图、散点图和小提# 琴图。只能更改选定数量的美学特征。并且支持对应代码的导出；

```r
install.packages("devtools")
devtools::install_github("gertstulp/ggplotgui")
library("ggplotgui")
ggplot_shiny()
ggplot_shiny(mpg)                                                   
```

### 统计辅助绘图

#### smplot/trelliscopejs


```r
### 可视化优化R 包 -- smplot ####
install.packages('devtools')
devtools::install_github('smin95/smplot')
## 参见；https://smin95.github.io/dataviz/manual-and-examples-of-smplot.html

library(smplot)
## 简单来说，这个程序优化了以往写ggplot2的不便之处，
## 加强了统计可视化的便利性，以及增加了画图的易用性：

## 改变主题：
p1 + sm_corr_theme()

## 去除边框和图例：
p2 <- p1 + sm_corr_theme(borders = FALSE, legends = FALSE)
p2

## 改变颜色：
p2 + scale_color_manual(values = sm_palette(7))

## 相关图：-添加R和P值；
p1 + sm_corr_theme() + 
  sm_statCorr(color = sm_color('green'))

## 箱线图+散点图：
p1 + sm_corr_theme() + 
  sm_statCorr(color = sm_color('green'))
```

#### 可视化优化facet_trelliscope 

```r
优化分图显示太多造成的困扰，使用滑窗展示的方法：
library(trelliscopejs)
library(ggplot2)
library(gapminder)
## 参见；https://cran.r-project.org/web/packages/trelliscopejs/vignettes/trelliscopejs.html
## 支持行列数量控制、标签控制、筛选控制和排序控制；


## 作为参数参与ggplot2的相关下游可视化：
qplot(year, lifeExp, data = gapminder) +
  xlim(1948, 2011) + ylim(10, 95) + theme_bw() +
  facet_trelliscope(~ country + continent, nrow = 2, ncol = 7, width = 300)

## 作为管道，直接接受上游ggplot2的图层，再优化：
library(trelliscopejs)
country_plot <- function(data, model) {
  plot_ly(data = data, x = ~year, y = ~lifeExp,
          type = "scatter", mode = "markers", name = "data") %>%
    add_trace(data = data, x = ~year, y = ~predict(model),
              mode = "lines", name = "lm") %>%
    layout(
      xaxis = list(range = c(1948, 2011)),
      yaxis = list(range = c(10, 95)),
      showlegend = FALSE)
}

## 需要注意这里使用purrr包对应的map_系列的绘图函数；
by_country <- by_country %>%
  mutate(data_plot = map2_plot(data, model, country_plot))
## 然后输入后进行管道运算优化：
by_country %>%
  arrange(-resid_mad) %>%
  trelliscope(name = "by_country_lm", nrow = 2, ncol = 4)
```

#### ggpubr

```
## 描述：一次绘制一个或一组变量。
内容：
基因表达数据
箱线图
小提琴情节
条形图和点图
密度图
直方图
经验累积密度函数
分位数 - 分位数图
将 P 值和显着性水平添加到 ggplots

## 描述：计算并自动将 p 值和显着性水平添加到 ggplots。
内容：
比较均值的方法
R函数添加p值
比较均值（）
stat_compare_means()
比较两个独立的组
比较两个配对样本
比较两个以上的组
多个分组变量
具有相关性和边际直方图的完美散点图

## 描述：使用相关系数和边际直方图/密度创建漂亮的散点图。
内容：
基本地块
按组着色
添加浓度椭圆
添加点标签
气泡图
按连续变量着色
添加边际图
添加二维密度估计
应用于基因表达数据
绘制均值/中位数和误差线

描述：用误差线轻松绘制均值或中位数。
内容：
误差图
线图
条形图
添加标签
应用于基因表达数据
条形图和现代替代方案

描述：轻松创建基本和有序的条形图，以及条形图的一些现代替代品，包括棒棒糖图和克利夫兰的点图。
内容：
基本条形图
多个分组变量
有序条形图
偏差图
条形图的替代方案
棒棒糖图表
克利夫兰的散点图
将文本标签添加到直方图和密度图

## 描述：创建直方图/密度图并突出显示图中的一些关键元素。
ggplot2 - 在同一页面上混合多个图形的简单方法
描述：在同一页面上以及在多个页面上组合多个 ggplot 的分步指南。
内容：
创建一些图
在一页上排列
注释排列的图形
对齐绘图面板
更改绘图的列/行跨度
为组合 ggplots 使用公共图例
带有边际密度图的散点图
混合表格、文本和 ggplot2 图表
在 ggplot 中插入图形元素
在 ggplot 中放置一个表格
在 ggplot 中放置一个箱形图
将背景图像添加到 ggplot2 图形
排列多个页面
使用 ggarrange() 的嵌套布局
导出地块
ggplot2 - 更改图形参数的简便方法

# 描述：描述函数 ggpar() [in ggpubr]，可用于简单轻松地自定义任何基于 ggplot2 的图形。
内容:
更改标题和轴标签
更改图例位置和外观
更改调色板
组颜色
渐变色
更改轴限制和比例
自定义轴文本和刻度
旋转绘图
更改主题
移除 ggplot 组件
创建和自定义多面板 ggplots：Facet 简单指南

# 描述：通过一个或多个变量拆分您的数据，并将数据的子集可视化在一起。
内容：        
按一个分组变量分面
由两个分组变量分面
修改面板标签外观
```




```r
# 参见网页：
https://rpkgs.datanovia.com/ggpubr/
## ggpubr全参数详解：
http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/

## 密度图： 
ggdensity(wdata, x = "weight",
   add = "mean", rug = TRUE,
   color = "sex", fill = "sex",
   palette = c("#00AFBB", "#E7B800"))

## 柱状图：
gghistogram(wdata, x = "weight",
   add = "mean", rug = TRUE,
   color = "sex", fill = "sex",
   palette = c("#00AFBB", "#E7B800"))

## 箱线图病添加统计p值：
ggboxplot(df, x = "dose", y = "len",
                color = "dose", palette =c("#00AFBB", "#E7B800", "#FC4E07"),
                add = "jitter", shape = "dose")

my_comparisons <- list( c("0.5", "1"), c("1", "2"), c("0.5", "2") )
p + stat_compare_means(comparisons = my_comparisons)+ # Add pairwise comparisons p-value
  stat_compare_means(label.y = 50)    

## 小提琴图
ggviolin(df, x = "dose", y = "len", fill = "dose",
         palette = c("#00AFBB", "#E7B800", "#FC4E07"),
         add = "boxplot", add.params = list(fill = "white"))+
  stat_compare_means(comparisons = my_comparisons, label = "p.signif")+ # Add significance levels
  stat_compare_means(label.y = 50)     

## 条线图：
ggbarplot(dfm, x = "name", y = "mpg",
          fill = "cyl",               # change fill color by cyl
          color = "white",            # Set bar border colors to white
          palette = "jco",            # jco journal color palett. see ?ggpar
          sort.val = "desc",          # Sort the value in dscending order
          sort.by.groups = FALSE,     # Don't sort inside each group
          x.text.angle = 90           # Rotate vertically x axis texts
          )

## 点线图：
ggdotchart(dfm, x = "name", y = "mpg",
           color = "cyl",                                # Color by groups
           palette = c("#00AFBB", "#E7B800", "#FC4E07"), # Custom color palette
           sorting = "ascending",                        # Sort value in descending order
           add = "segments",                             # Add segments from y = 0 to dots
           ggtheme = theme_pubr()                        # ggplot2 theme
           )
```



### base-plot()/ggplot2()

#### 符号和线条


```r
## base-R:
pch指定图形（在type=”p”/”o”/”b”时）,可加cex参数，用来控制图中的符号大小，默认为cex=1,
lty指定线形,lwd改变线条粗细;
type指定线性的类型：
“p”点图 、 “l”线图 、 “b”点线图，线不穿过点 、“c”虚线图 、“o”点线图，线穿过点
“h”直方图、 “s”阶梯图、 “S”步骤图 、“n”无图

## 绘制：
x<-c(1:10)
png("~/plotSamples.png",width=9,height=9,unit="in",res=108)   #在工作目录下创建plotSamples.png图
par(mfcol=c(3,3))
plot(x,type="p",main="p")
dev.off()
```

#### 颜色参数：

##### 颜色参数配置：


```r
## base-R
  col：默认绘图颜色。某些函数(如lines、pie)可以接受一个含有颜色值的向量，并自动循环使用。
  例如：col=c("red", "blue")需要绘制三条线，那么三条颜色分别为red、blue、red
  col.axis：坐标轴刻度文字的颜色，不是坐标轴的颜色
  col.lab：坐标轴标签(名称)的颜色
  col.main：标题的颜色
  col.sub：副标题的颜色
  fg：图形的前景色
  bg：图形的背景色
col指定图形颜色
colors()方法可以查看R中所有可用的颜色名，一共有657种颜色名，根据颜色名可直接设置图形的显示颜色。下面是部分颜色，完整的图见链接：R语言颜色表

除了名称外，同样可以用下标，十六进制颜色值，RGB值和HSV值来指定颜色。例子：col=1、col="white"、col="#FFFFFF"、col=rgb(1,1,1)和col=hsv(0,0,1)。

另外，R中还有许多生成颜色变量的函数。有rainbow()、heat.colors()、terrain.colors()、topo.colors()、cm.colors()方法，gray()方法生成多阶灰度色。
```

##### 颜色梯度：


```r
# 设置
# 方法1：
col = grey(1:100/100)
# 方法2：
ttt <- colorRampPalette(colors = c('black', 'white'))( 100 )
plot(env_bgExt_na,col=ttt )
```
```
## 00:全透明
## FF:不透明
### 蓝色：#0000FF7F
### 黄色：#FFFF008c
### 绿色：#00FF0080
##  透明色：#FFFFFF0
浅黄色：khaki
深黄色：yellow
深蓝色："lightskyblue"
浅蓝色："slateblue1"
浅绿色：seagreen1
亮绿色：chartreuse
浅红色：pink
深红色：red

## 参见网站：https://c.runoob.com/front-end/55
透明度	16进制值
00%	FF（不透明）
5%	F2
10%	E5
15%	D8
20%	CC
25%	BF
30%	B2
35%	A5
40%	99
45%	8c
50%	7F
55%	72
60%	66
65%	59
70%	4c
75%	3F
80%	33
85%	21
90%	19
95%	0c
100% 00（全透明  ncores = 30

## 参见网站：
https://www.cnblogs.com/summary-2017/p/7504126.html

## 写16进制代码的方式是将颜色加入到前面，最后再加透明度即可！！
```



#### 4.3 文本配置

```
## base-R:
## 配置全局的文字：
windowsFonts(myFont=windowsFont("华文中宋"))


## 文本属性(用来指定字号、字体、字样)
cex.axis：坐标轴刻度文字的缩放倍数
cex.lab：坐标轴标签(名称)的缩放倍数
cex.main：标题的缩放倍数
cex.sub：副标题的缩放倍数
font：整数。用于指定字体样式。1常规、2粗体、3斜体、4粗斜体

## 
```

#### 图形尺寸与边界

```
## base -R
pin：以英寸表示图形的宽和高
mai：以数值向量表示边界大小，顺序为"下、左、上、右"，单位为英寸
mar：以数值向量表示边界大小，顺序为"下、左、上、右"，单位为英分，默认值c(5, 4, 4, 2)+0.1
```

#### 坐标轴：

```
## base -R:
plot参数
     axes=FALSE  将禁用全部坐标轴，框架和刻度全部没有了
     xaxt="n"   禁用x轴的刻度线
     yaxt="n"   禁用y轴的刻度线
也可以通过axis函数自定义axis(……)    
     side：一个整数。表示在图形的哪边绘制坐标轴（1=下，2=左，3=上，4=右）
     at：一个数值向量，表示需要绘制刻度线的位置
     labels：一个字符型向量(也可以是数值型)，表示刻度线旁边的文字标签(刻度值)，如果整个不写，则直接使用at的值
     col：线条和刻度的颜色
     lty：线条类型
     las：标签的字体是否平行(=0)或者垂直(=2)坐标轴
     tck：刻度线的长度(默认值-0.01，负值表示刻度在图形外，正值表示刻度在图形内侧)
```
```
## GGPLOT2:
# 笛卡尔坐标：从来看coord_cartesian的参数相对比较简单，x和y的数据限定范围
coord_cartesian(xlim = NULL, ylim = NULL)
# 横向转换坐标：把x轴和y轴互换，没有特殊参数
coord_flip(...)

# 坐标形式转换：包括对数转换，平方根转换等，这里x和y 的值可以是log10,log2或squal等，另外两个参数也是限定坐标范围
coord_trans(x = "identity", y = "identity", limx = NULL, limy = NULL)

# 等坐标转换：使用这个函数后，x轴和y轴坐标会被转换成相等形式，此时图形会产生较大的缩放，radio可以进一步调整缩放比例（x和y的比值）
coord_equal(ratio=1, ...)

# 极坐标转换：可以做出蜘蛛图或饼图的效果，参数方面theta 可以选择x或y，表示外延的坐标，start是坐标开始的角度，默认其实位置是12点钟，
coord_polar(theta = "x", start = 0, direction = 1)
direction 表示数据的方向，1是顺时针，-1为逆时针。
```

#### 参考线/辅助线：

```
## base -R:
abline(h=yvalues, v=xvalues)
例如：plot(1:10)
     abline(h = c(1, 5))  #则在y=1和5处各有一条水平线
     abline(v = c(1, 5))  #则在x=1和5处各有一条垂直线

contour(elev_sa, nlevels=7, 
levels=c(0, 500, 1000, 1500, 2000, 3000, 4000, 5000), 
add=T, labels=““, lwd=0.2)


## GGPLOT2:
 geom_vline(xintercept = 0, color = 'gray', size = 0.4) + 
  geom_hline(yintercept = 0, color = 'gray', size = 0.4) 
```

#### 标题配置：

```
# base-R
title(main = " ", sub = " ", xlab = " ",  ylab = " ")
也可以直接把title里面的参数直接放在plot()里面
```

#### 图例设置：


```r
## base-R:
# 通过legend函数我们可以添加图例。
x <- seq(-pi, pi, len = 65)
plot(x, sin(x), type = "l", ylim = c(-1.2, 1.8), col = 3, lty = 2)
points(x, cos(x), pch = 3, col = 4)
lines(x, tan(x), type = "b", lty = 1, pch = 4, col = 6)
title("legend(..., lty = c(2, -1, 1), 
       pch = c(NA, 3, 4), merge = TRUE)",
       cex.main = 1.1)
legend(-1, 1.9, 
       c("sin", "cos", "tan"), 
       col = c(3, 4, 6),
       text.col = "green4", 
       lty = c(2, -1, 1), 
       pch = c(NA, 3, 4),
       merge = TRUE, 
       bg = "gray90")

legend("bottom", legend = levels(iris$Species),
       col =  c("#999999", "#E69F00", "#56B4E9"), 
       pch = c(16, 17, 18), 
       inset = -0.25, xpd = TRUE, horiz = TRUE)
```

#### 图形边距设置：

```
## base-R
par函数可以用来设置图形参数
par函数设置的参数默认值
## 调整图片间的间距：
par(mfcol=c(2,1),mar=c(1,4,1,1),oma=c(2,2,2,2))
## 参见：
https://www.cnblogs.com/mmtinfo/p/12106309.html
## 解决plot.new() : figure margins too large
op <- par(mar = rep(0, 4))  
plot.new()
par(op)
```

#### 添加额外图层：


```r
library(ggplot2)
library(cowplot)
p <- ggplot(mpg, aes(displ, cty)) +
  geom_point() +
  theme_minimal_grid()
cowplot::ggdraw(p) + draw_label("Draft", colour = "#80404080", size = 120, angle = 45)
```

### 绘图

#### 绘制组图


```r
## base-R:
## 借助par扩大画幅：
par(mar=c(2,2,2,2), mfrow=c(1,3), oma=c(0,0,2,0))
plot(nndist(ppp.random), main='independant', pch=16, cex=0.8)
plot(nndist(ppp.regular), main='regular', pch=16, cex=0.8)
plot(nndist(ppp.locations), main='clustered', pch=16, cex=0.8)
mtext("Nearest Neighbor Distances", outer = TRUE, cex = 1.5)

## ggplot2系列：
library(ggtree)
multiplot(p1, revts(p1), ncol = 2)

## 多图-相似图例：
library(rasterVis)
levelplot(all2,layout=c(4, 4),col.regions=cols,
          main="HTHI_FOREEST")


## 另外的组图高级R包：
## cowplot和patchwork
```

#### 导出图形


```r
## base-R:
png("./fut_hthi_pro/BIOSNOW2.png",width =3200, height = 2000,res =300)
plot(raster(all[1]),col = cols)
dev.off()

## 配置图片参数：
png('plot1.png',height = 20,width = 20,units = 'cm',res = 800)
print(p1_cor)
dev.off()

## 
tiff(file = "./f3_论文出图/try4.tiff", res = 600, width = 2000, 
height =2500, compression = "lzw")


## GGPLOT2风格：
## 批量出图：
## 使用ggpubr::ggexport()导出成pdf文件
library(ggpubr)
##一张图一页；
ggexport(plotlist = list(scree.plot, ind.plot, var.plot), 
         filename = "PCA.pdf")
## 一张图多页；
ggexport(plotlist = list(scree.plot, ind.plot, var.plot), 
         nrow = 2, ncol = 2,
         filename = "PCA.pdf")

##将图表导出到png文件。如果指定绘图列表，则将自动创建多个png文件以保存每个绘图
ggexport(plotlist = list(scree.plot, ind.plot, var.plot),
         filename = "PCA.png")

## 批量导出图片到pdf格式：
ff <- list(bio11,BIO16)
library(cowplot)
dir.create("./f3_论文出图")
pdf("./f3_论文出图/boxplot4.pdf")
# tiff(file = "./f3_论文出图/boxplot.tiff", res = 300, width = 2000, height =6000, compression = "lzw")
ggdraw(plot_grid(plot_grid(plotlist = ff,ncol=1,nrow=2, align='v'),
                 # plot_grid(NULL, legend,ncol=2),
                 rel_widths=c(1, 0.2)))
dev.off()

### GGSAVE():
## 保存图片：使用ggsvae：
ggsave(filename ="ggplot2_point.png",plot =p1,
      width=8.9,height= 6,units="cm",dpi=600)

ggsave(filename = "ggolot2-point.pdf",plot= p1,device="pdf",
       width=8.9,height= 6,units="cm",dpi=600)

## 保存为jpg格式：
ggsave("p1.jpg",width=6,height =5)
jpeg("p1.jpg",width =1200,height = 2000,units ="px")
p1 
dev.off()
```

### 统计图形代码

#### 线性模型

##### 一般线性模型可视化base-R：


```r
## base-R:
plot(data$height, data$weight)
abline(model, col = 'red')
legend("topleft",
       legend = c('data points', 'regression line'), 
       cex = 0.7, 
       col = c('black', 'red'), 
       lwd = c(NA, 1), 
       pch = c(1, NA))

## GGPLOT2 - 标记R2和p：
#读取鱼类物种丰度和水体环境数据
dat <- read.delim('fish_data.txt', sep = '\t', row.names = 1)
 
#绘制二维散点图观测各环境变量与鱼类物种丰度的关系
library(ggplot2)
 
dat_plot <- reshape2::melt(dat, id = 'fish')
 
p <- ggplot(dat_plot, aes(value, fish)) +
geom_point() +
facet_wrap(~variable, ncol = 3, scale = 'free') +
geom_smooth(method = 'lm')
 
p
 
#拟合各环境变量与鱼类物种丰度的一元回归，并提取各自 R2 和 p 值
env <- c('acre', 'do2', 'depth', 'no3', 'so4', 'temp')
R2_adj <- c()
p_value <- c()
 
for (i in env) {
    fit_stat <- summary(lm(dat[['fish']]~dat[[i]]))  #一元线性回归
    R2_adj <- c(R2_adj, fit_stat$adj.r.squared)  #提取校正后 R2
    p_value <- c(p_value, fit_stat$coefficients[2,4])  #提取显著性 p 值
}
 
env_stat <- data.frame(row.names = env, R2_adj, p_value)
env_stat  #数据框中存储了各环境变量与鱼类物种丰度的一元回归的 R2 和 p 值
 
#在散点图中添加各环境变量与鱼类物种丰度的一元回归的 R2 和 p 值作为标识
#注：该 R2 和 p 值仅为单个变量一元回归的 R2 和 p 值，和下文即将提到的多元回归的 R2 和 p 值存在区别
env_stat$fish <- max(dat$fish) * 0.8
for (i in env) env_stat[i,'value'] <- max(dat[[i]]) * 0.8  #这句和上一句，定义文字在图中的展示坐标，这里仅粗略设置下
env_stat$variable <- rownames(env_stat)
env_stat$label <- paste('R2.adj =', round(env_stat$R2_adj, 3), '\np =', round(env_stat$p_value, 3))  #文字标签
 
env_stat <- env_stat[c('fish', 'variable', 'value', 'label')]
dat_plot$label <- NA
dat_plot <- rbind(dat_plot, env_stat)  #和先前的数据框合并，便于作图
 
p + geom_text(data = dat_plot, aes(label = label), size = 3)
```

##### 一般线性模型可视化ggplot2-R：


```r
label_df = data.frame(
  x = c(-0.5,-0.5,-0.5),
  y = c(0.3,0.25,0.2),
  label = c(paste0("COR = ", round(cor(train_simu,train$shortwave),2)),
            paste0("p = ", formatC(cor.test(train_simu,train$shortwave)$p.value)),
            paste0("RMSE = ", round(rmse(train_simu,train$shortwave),3)))
)

p1_cor = ggplot()+
  geom_point(data = pl_point_df,
             aes(x = SIMU, y= TRAIN),
             size = 5,
             color = 'blue',
             shape = 1)+
  geom_abline(intercept = 0,slope = 1,size = 1)+
  geom_text(data = label_df,aes(x = x,y = y,label = label),size = 6,color = 'black',
            hjust = 0)+
  theme_bw()+
  theme(
    axis.text =  element_text(face = 'bold',color = 'black',size = 12),
    axis.title =  element_text(face = 'bold',color = 'black',size = 14, hjust = .5)
  )+
  xlab('SIMULATION RESULT')+
  ylab('REAL SHORTWAVE')

### 添加P值和R2:
https://lukemiller.org/index.php/2012/10/adding-p-values-and-r-squared-values-to-a-plot-using-expression/
```

##### 多变量相关性图：


```r
library(GGally)
### 第一种形式：
ggpairs(dat[which(names(dat) != 'fish')])

### 第二种形式：
ggcorr(mtcars[-c(5,7,9)], nbreaks = NULL, label = TRUE, low = 'red3', high = 'green3',
        label_round = 2, name = 'Correlation Scale', label_alpha = TRUE, hjust = 0.75) +
ggtitle(label = 'Correlation Plot') +
theme(plot.titl = element_text(hjust = 0.6))

## 第三种形式
library(lattice)
splom(df)
```

##### 三维散点图：


```r
## 可视化数据框：
library(scatterplot3d)
scatter.3d <- with(df, scatterplot3d(Girth, Height, Volume, 
                              pch = 16, highlight.3d = TRUE, 
                              angle = 60))
scatter.3d$plane3d(model2)
```



#### 分组统计图：

##### 豆荚图


```r
## 学习制作豆荚图来优化数据的可视化流程：
install.packages("beanplot")

library("beanplot")


# 创建三组数据，分别为双峰，均匀和正态分布
bimodal <- c(rnorm(250, -2, 0.6), rnorm(250, 2, 0.6))
uniform <- runif(500, -4, 4)
normal <- rnorm(500, 0, 1.5)

# 制作豆荚图
beanplot(bimodal, uniform, normal, # 注明用于作图的三组数据
         main = "Beanplot", # 题目
         col = c("#CAB2D6", "#33A02C", "#B2DF8A"), # 修改颜色：分布，豆荚内横线，豆荚外横线
         border = "#CAB2D6") # 豆荚图的边缘颜色


## 另外一种例子：
data("singer", package = "lattice")
mydata <- singer

# 查看数据集
summary(mydata)

beanplot(height ~ voice.part, # 可以使用熟悉的模式: y ~ x
         data = mydata, # 数据
         ll = 0.05, # 改变豆荚内横线的长度
         main = "Beanplot", # 题目
         ylab = "Body height", # y轴标签
         col = "gray90") # 改变豆荚内的分布颜色
```

##### 森林图

###### 6.2.2.1 ggplot2

```r
### 森林图：
forest_35 <- glm(inpatient_time ~ fbg_diff + fbg_reg + age + sex + region + cad + hbp + stroke + ckd + group_1 + treatment_plan, family = "gaussian", data = secondary5) %>%
  tidy(conf.int = TRUE) %>%
  mutate(across(where(is.numeric), round, digits = 2),
           #set order of levels to appear along y-axis
         term = fct_relevel(term, ...),
         term = recode(term,
                       "fbg_diff" = "空腹血糖变化差值",
                       "fbg_reg" = "末次空腹血糖<6.1mmol/l",
                       "age" = "年龄",
                       "sex女" = "性别：女",
                       # "insurance_type医保类型缺失" = "医保类型缺失",
                       # "insurance_type有基本医疗保险" = "有基本医疗保险",
                       "region东部地区" = "东部地区",
                       "region西部地区" = "西部地区",
                       "cad" = "冠心病",
                       "hbp" = "高血压",
                       "stroke" = "卒中",
                       "ckd" = "慢性肾功能不全",
                       # "disease_num1" = "合并1种疾病",
                       # "disease_num2" = "合并2种疾病",
                       # "disease_num3" = "合并3种疾病",
                       # "disease_num4" = "合并4种疾病",
                       "group_1预混胰岛素组" = "预混胰岛素",
                       "treatment_plan二联治疗" = "二联治疗",
                       "treatment_plan三联治疗" = "三联治疗",
                       "treatment_plan胰岛素多次治疗" = "胰岛素多次治疗")) %>%
  filter(term != "(Intercept)") %>%

  ## plot with variable on the y axis and estimate (OR) on the x axis
  ggplot(aes(x = estimate, y = term)) +

  ## show the estimate as a point
  geom_point() +

  ## add in an error bar for the confidence intervals
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high)) +

  ## show where OR = 1 is for reference as a dashed line
  geom_vline(xintercept = 0, linetype = "dashed") +
  
  ylab("变量") +
  
  xlab("Beta") +
  
  ggtitle("表35森林图")

forest_35
```

###### 6.2.1.2 森林表


```r
library(survival)
library(survminer)
library(eoffice) #导出ppt用

# install.packages("eoffice")


# 第一种方法： -----
# 以colon数据为示例数据
# 时间变量是time，结局变量是status，自变量选择 sex，rx和 adhere
model <- coxph( Surv(time, status) ~ sex + rx + adhere,
                data = colon )
# 森林图
ggforest(model,  #coxph得到的Cox回归结果
         data = colon,  #数据集
         main = 'Hazard ratio of colon',  #标题
         cpositions = c(0.05, 0.15, 0.35),  #前三列距离
         fontsize = 1, #字体大小
         refLabel = 'reference', #相对变量的数值标签，也可改为1
         noDigits = 3 #保留HR值以及95%CI的小数位数)

# 第二种方法：-----
# 安装R包
install.packages("devtools")
devtools::install_github("ddsjoberg/bstfun")
# 载入
library("gtsummary")   # 表格制作
library("dplyr")       # 数据处理
library("bstfun")      # 画出森林图
# 建立线性回归模型
linear_model <- lm(mpg ~ am + drat + wt + hp, data = mtcars)
# 制作回归结果的表格
linear_model %>% tbl_regression()
linear_model %>%
  tbl_regression() %>%
  add_inline_forest_plot()


## 第三种方法：----
## 参见：https://zhuanlan.zhihu.com/p/493258386
## 参数配置：https://www.jianshu.com/p/7331c8f40d87
install.packages("forestplot")
library(forestplot)
result <-read.table("clipboard", header = F, sep = '\t')
View(result[,c(1:3,7:8)])
## 核心构建思路是将图表中数据进行重构，搭建缺失数形式，然后其他数据自然出表，
## 而森林图组分使用mean/lower/upper来构建对应的参数形式：
forestplot(result[,c(1:3,7:8)],            # {1:3，7:8指Excel中的列号}
                  mean=result[,4],
                  lower=result[,5],
                  upper=result[,6],
                  zero=1,                         # {表示我们以坐标x轴=1为中心}
                  boxsize=0.3,
                  graph.pos = 4,
                  hrzl_lines=list('1'=gpar(lty=1,lwd=2),
                                  '2'=gpar(lty=1,lwd=2),
                                  '24'=gpar(lwd=2,lty=1)),      #  {绘制表格线，33表示所有行数+1}
                  graphwidth=unit(.25,'npc'),
                  xticks=c(0,1,2,3,4),    # {此处定义横坐标，so就设了1,2,3,4}
                  # is.summary=c(T,
                  #              T,F,F,
                  #              T,F,F,
                  #              T,F,F,F,
                  #              T,F,F,
                  #              T,F,F,
                  #              T,F,F,F,F,
                  #              T,F,F,
                  #              T,F,F,
                  #              T,F,F,F),  #{T,F表示从第一行开始是否需要横杠，T表示不需要跑出}
                  txt_gp=fpTxtGp(label = gpar(cex=1),
                                 ticks = gpar(cex=1.1),
                                 xlab = gpar(cex=1),
                                 title = gpar(cex=2)),
                   xlab='Hazard ratio',   
                  lwd.zero=2,
                  lwd.ci=2,
                  lwd.xaxis=1,          
                  lty.ci=1,
                  ci.vertices=T,
                  ci.vertices.height=0.2,
                  clip=c(0,4),
                  ineheight=unit(8,'mm'),
                  line.margin=unit(8,'mm'),
                  colgap=unit(6,'mm'),
                  col=fpColors(zero = 'black',
                               box = 'black',
                               lines = 'black'),
                  fn.ci_norm='fpDrawCircleCI'
)
fig
```





##### 蜜蜂图：


```r
## 方案1：
library(beeswarm)
data(breast)
beeswarm(time_survival ~ ER, data = breast,
         pch = 16, pwcol = 1 + as.numeric(event_survival),
         xlab = "", ylab = "Follow-up time (months)",
         labels = c("ER neg", "ER pos"))
legend("topright", legend = c("Yes", "No"),
       title = "Censored", pch = 16, col = 1:2)
## 方案2
ggplot(breast,aes(x=ER,y=time_survival))+
  geom_beeswarm(aes(color=factor(event_survival)),cex=1.5)+#cex用于设置点的密集程度
  theme_bw()+
  theme(
    legend.position = c("top"),
    panel.grid = element_blank()
    
  )+
  scale_color_manual(values=c("Black","Red"),name="Censored",labels=c("Yes","No"))+
  scale_x_discrete(labels=c("ER neg","ER pos"))+
  xlab("")+
  ylab("Follow-up time (months)")
```



#### 栅格密度图：


```r
## 单个栅格：
library(rastervis)
levelplot(tmin1.c)

## 多个栅格：
raster::pairs(clim_subset,maxpixels=1000
              
## 使用par(mfrow =c(2,4)) 
       
##  在给定范围内可视化：
## 绘图中extent的范围：
plot(predictors[[1]], col="gray80", ext=c(-8, -3, 42, 44))
              
              
## 地图可视化：
#plot the map
ggplot() +
  geom_polygon(data = G1,
               aes(x = long, y = lat, group = group),
               colour = "grey10", fill = "#fff7bc") +
  geom_point(data = dwd.data,
             aes(x = LON, y = LAT, colour = data.subset),
             alpha = .5,
             size = 2) +
  theme_bw() +
  xlab("Longitude") + ylab("Latitude") +
  coord_map()
              
              
## 高程叠加可视化：3D：
## 不能使用library(rayshader)这个包，需要更换：
library(rasterVis)
## 添加投影坐标系下的栅格；前置高程，后置叠加栅格；
plot3D(demms,drape= biox , adjust = FALSE)
```

#### 交互式绘图

##### plotly::ggplotly()


```r
## 使用 R、plotly 和闪亮的基于 Web 的交互式数据可视化
https://plotly-r.com/index.html
### 交互式绘图的方法，是将静态图传递给ggplotly()
deaths_plot <- ggplot(data = weekly_deaths)+            # begin with weekly deaths data
  geom_line(mapping = aes(x = epiweek, y = pct_death))  # make line 
deaths_plot   # print
deaths_plot %>% plotly::ggplotly()

## 常用交互式绘图的三件套：
metrics_plot %>% 
  plotly::ggplotly() %>%   ## 对应R包指令；
  plotly::partial_bundle() %>%  ## 抽取部分数据集展示；
  ### 调整绘图展示参数；
  plotly::config(displaylogo = FALSE, modeBarButtonsToRemove = plotly_buttons_remove)
```

##### ggvis-shiny

###### 6.4.2.1  ggvis-shiny-动态图

```
ggvis VS ggplot2主要区别：http://ggvis.rstudio.com/ggplot2.html
ggplot→ggvis
geom→layer function
stat→compute function
aes→props
+→%>%
ggvis目前不支持分面；
使用ggvis而不添加任何层类似于qplot。
```


```r
library(ggvis)
library(dplyr)
mtcars %>%
  ggvis(~wt, ~mpg) %>%
  layer_points(fill = ~factor(cyl),
               size := 25, shape := "diamond",
               stroke := "red") %>%
  group_by(cyl) %>%
  layer_model_predictions(model = "lm", se = TRUE)


## 提供shiny-Bar来绘制R图的修改参数：
mtcars %>%
  ggvis(~wt, ~mpg,
        size := input_slider(10, 100),
        opacity := input_slider(0, 1)
  ) %>%
  layer_points()

## 鼠标查看：
mtcars %>% ggvis(~wt, ~mpg) %>%
  layer_points() %>%
  add_tooltip(function(df) df$wt)


## 其他调整图层的shiny参数：
p %>% layer_points(fill := "red", stroke := "black")
p %>% layer_points(size := 300, opacity := 0.4)
p %>% layer_points(shape := "cross")

## 参见资源；
https://ggvis.rstudio.com/interactivity.html
```

###### 6.6.4.2.2  ggvis-构造shiny


```r
# ui.R
library(shiny)
 
# Define UI for miles per gallon application
shinyUI(sidebarLayout(
  sidebarPanel(
    sliderInput("size", "Area", 10, 1000, 500)
  ),
  mainPanel(
    uiOutput("ggvis_ui"),
    ggvisOutput("ggvis")
  )
))

# server.R
library(shiny)
library(ggvis)
library(dplyr)
mpgData <- mtcars
mpgData$am <- factor(mpgData$am, labels = c("Automatic", "Manual"))
# Define server logic required to plot various variables against mpg
shinyServer(function(input, output) {
  input_size <- reactive(input$size)
   
  mtcars %>%
    ggvis(~disp, ~mpg, size := input_size) %>%
    layer_points() %>%
    bind_shiny("ggvis", "ggvis_ui")
})
```



##### highcharter-高级交互式


```r
# install.packages("highcharter")
## 参见网页：https://jkunst.com/highcharter/ 提供海量图层资源；
## 这个R包与ggplot2不同的地方在于使用管道夫进行添加图层；
library(highcharter)
library(tidyverse)
#导入数据集
data<-data.frame(index = c(1:7),
                 value = c(1,2,5,3,7,6,8))
data
#散点图1-普通散点图
data_point<-data
hchart(data_point,"scatter",hcaes(x = "index",y = "value"))

#散点图3-设置坐标轴标题
hchart(data_point,"scatter",hcaes(x = "index",y = "value"))%>%
  hc_xAxis(title = list(text = "x轴"))%>%
  hc_yAxis(title = list(text = "y轴"))
```

### 医学分析图

#### jinseob2kim

```
## 数据资料：
(https://github.com/jinseob2kim/jskm/commits?author=jinseob2kim)

## R包开发目的：
## 优化生存曲线展示：
Kaplan-Meier Plot with ‘ggplot2’: ‘survfit’ and ‘svykm’ objects from ‘survival’ and ‘survey’ packages.
```

#### gg-km曲线


```r
# Kaplan-Meier Survival Curve
KM <- ggsurvplot(
  fit,
  
  # 添加内容
  pval = TRUE,
  conf.int = TRUE,
  risk.table = TRUE,
  risk.table.col = "strata",
  linetype = "strata",
  fun = 'event',
  
  # 主题设置
  palette = c('#4f81bd','#c0504d'),
  ggtheme = theme_bw(),
  
  # 横纵坐标设置
  break.x.by = 1,
  xlim = c(0,6),
  ylim = c(0,0.3),
  
  # labs设置
  legend.labs = c("control group","exposed group"), 
  xlab = "Months",
  ylab = "Cumulative incidence",
  title = "Kaplan-Meier survival curve of *** (6 Months)"
  )

KM$plot <- KM$plot+
  theme(
    legend.background = element_rect(fill = "white",
                                     colour = "black",
                                     size =0),
    legend.position = c(0.999,0.999),
    legend.justification = c(1,1))

KM

# Log Rank
p.val <- 1-pchisq(fit$chisq,length(fit$n)-1)
p.val
```



### 补充图形库

#### ggplot2的相关扩展

``` 
## 参见：提供了ggplot2出图的大量扩展：
https://exts.ggplot2.tidyverse.org/ggstance.html

## ggiraph  --使得图形交互性：
geom_bar_interactive
geom_boxplot_interactive
geom_histogram_interactive
geom_line_interactive
geom_map_interactive
geom_path_interactive
geom_point_interactive
geom_polygon_interactive
geom_rect_interactive
geom_segment_interactive
geom_text_interactive
geom_tile_interactive

## ggstance:
使得ggplot2的图形倒置；

## ggalt 
添加额外的坐标轴和设置；

## ggtech 、ggthemes
ggplot2 技术主题、比例和几何。

## gganimate -坑需要提供第一个对应的序列，比如时间序列；
gganimate 包装动画包以创建动画 ggplot2 绘图。

## ggradar -雷达图
有时候这个用于展示数据结构差异，还是挺有用的；或者汇总结果比较；

## ggridges包 
绘制山脊图

## ggsignif包
添加显著性统计P值

## ggstatsplot -添加P值和R平方；
自动化绘图：
```

