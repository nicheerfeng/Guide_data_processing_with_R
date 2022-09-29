# (PART\*) 模型展示 {.unnumbered}

# 模型发布与应用 {#06-Model-release-and-application}
## flexboard
### 安装工作环境
```
install.packages("flexdashboard")
library(flexdashboard)
```
### 多级导航模块
#### 构建一级导航
```
---
title: "Multiple Pages"
output: flexdashboard::flex_dashboard
---
# Page 1  ## 使用等号来赋值新的导航页。默认为纵向；
# `===================================== `
### Chart 1  
  
# 修改默认页为纵向
# Page 2 {data-orientation=rows}
# `===================================== ` 
```  
#### 构建二级下拉菜单 
```
# ---
#   title: "Page Navigation Menus"
# output: flexdashboard::flex_dashboard
# ---
#   
#   Page 1 {data-navmenu="Menu A"}
# `=====================================`
#   Page 2 {data-navmenu="Menu A"}
# `=====================================`  
#   Page 3 {data-navmenu="Menu B"}
# `=====================================`
#   Page 4 {data-navmenu="Menu B"}
# `=====================================`  
```
### 超级链接表
#### 提供显著引用
```
# Page 1
# `===================================== `
# You can link to a dashboard page with either of the following syntaxes:
# # 方法1  
# [Page 2]
# # 方法2可以修改指引的名称；
# [Page Two](#page-2)
```  
#### 使用图标提供显著引用
```
# Page 1 {data-icon="fa-list"}
# `=====================================`
```  
#### 隐藏最后一级的引用
```
# 暂时没想到这个怎么用；
# Page 3 {.hidden} -- 就不能直接引用这个；
```
### 给导航栏添加Social Links
```
---
  title: "Social Links"
output: 
  flexdashboard::flex_dashboard:
  social: [ "twitter", "facebook", "menu" ]
---
```
### 给导航栏添加Source Code
```
---
  title: "Source Code"
output: 
  flexdashboard::flex_dashboard:
  ## 源代码嵌入
  source_code: embed
  ## GITHUB:url嵌入；
  source_code: "http:/"
---
```
### 给导航栏添加一个`about`
```
  ---
    title: "Navigation Bar"
  output: 
    flexdashboard::flex_dashboard:
    navbar:
    - { title: "About", href: "https://example.com/about", align: left }
  ---
```    
  
### 布局参数
#### 行列布局参数配置 
```
## 默认为行构建；
## 按列指定构建；
Column -- 单列
-
### Chart 1

Column -- 多列
-
### Chart 2
### Chart 3

Column {.tabset} - 形成合并框构建；
Column{.tabset .tabset-fade} - 渐隐合并框；
-    
### Chart 2
### Chart 3  

## 直接指定数据展示的高度，有时候比定义图片高度更好用；    
Row {data-height=600}
-  
```
      
#### 图形参数配置
```
# 常用通常修改图形方式；
    {r, fig.width=5, fig.height=5}
# 默认情况下，flexdashboard 在图表边缘放置 8 个像素的填充。
# 添加.no-padding属性以指定完全没有填充，也可以添加属性data-padding以指定特定数量的像素。  
### Chart 1 {.no-padding}
  
### Chart 2 {data-padding=10}
  
```  
#### 表格参数配置
```
## 默认静态配置
knitr::kable(mtcars)
DT::datatable(mtcars, options = list(
  bPaginate = FALSE
))
## 动态参数配置--更新；
renderTable({
  head(mtcars, n = input$rows)
})
DT::renderDataTable({
  data <- head(mtcars, n = input$maxrows)
  DT::datatable(data, options = list(
    bPaginate = FALSE
  ))
})
```
#### 文字描述
正常可以在一页的任意位置写入即可，表示单独的块；可以给他用`###`起名；
也可以在新的一页中定义；

#### 排除标题
All Lung Deaths {.no-title} ---不展示标题；

```r
dygraph(ldeaths)
```

### 功能块参数
#### 图标网站来源
https://fontawesome.com/icons/twitter?s=solid&f=brands
引用方式"fa-comments"
#### 计数box
```
### Comments per Day --内置到box中的数字标签；
comments <- computeComments() # --输出数字；
valueBox(comments, icon = "fa-comments")
## shiny样式；--即时输入；
renderValueBox({
  articles <- computeArticles(input$types)
  valueBox(articles, 
           icon = "fa-pencil",
           color = ifelse(articles > 100, "success", "info"))
})

```
#### 仪表盘（比例动态）


```r
rate <- computeContactRate()
gauge(rate, min = 0, max = 100, symbol = '%', gaugeSectors(
  success = c(80, 100), warning = c(40, 79), danger = c(0, 39)
))
```


```r
renderGauge({
  rate <- computeContactRate(input$region)
  gauge(rate, min = 0, max = 100, symbol = '%', gaugeSectors(
    success = c(80, 100), warning = c(40, 79), danger = c(0, 39)
  ))
})
```


  

### 故事版
#### 主题故事版构建
```
## 在titile中提供storyboard: true；
## 其他部分正常些即可；
---
  title: "Storyboard"
output: 
  flexdashboard::flex_dashboard:
  storyboard: true
---
 ```

此时`page1`将被当做导航标题：
`##` page1 `

```r
# 讲述一个故事
```


#### 插入某一页面为故事版 
```
---
  title: "Storyboard Page"
  output: flexdashboard::flex_dashboard
---
    
Analysis {.storyboard}
=========================================
```    

### 辅助dashboard展示参数
#### 去除手机显示中的参数展示
```
---
  title: "Storyboard Commentary"
  output: 
    flexdashboard::flex_dashboard:
    storyboard: true
---
```

#### 指定展示的主题
```
---
  title: "Themes"
output: 
  flexdashboard::flex_dashboard:
  theme: bootstrap
---

## 可以使用的主题列表
default
cosmo
bootstrap
cerulean
journal
flatly
readable
spacelab
united
lumen
paper
sandstone
simplex
yeti 
```
#### 给展示网站提供logo
```
## 图片控制在(48 pixels high for the default “cosmo
---
  title: "Logo and Favicon"
output: 
  flexdashboard::flex_dashboard:
  logo: logo.png
favicon: favicon.png
---
```
  
  
## shinydashboard 
### 基本框架和流程
#### 基本组成框架
```
## ui.R 
library(shinydashboard)

dashboardPage(
  dashboardHeader(),
  dashboardSidebar(), ## 侧边栏
  dashboardBody() ## 主体；
)
## app.R ##
library(shiny)
library(shinydashboard)
## 其中skin可用的颜色为The default is blue, also black, purple, green, red, and yellow； 
ui <- dashboardPage(header, sidebar, body, skin = skin)

server <- function(input, output) { }

shinyApp(ui, server)
```
#### 基于ui和server来开发shiny的流程范式
```
1、侧边栏内部可以写`sliderInput`，body内部也可以写；
2、每个ui中的输入都需要指定输出的`name`,然后在
`output$name`接受ui的参数，然后再进行运算。
3、ui和server都会存在显示效果；
```
### 仪表板的结构
#### dashboardHeader()标题栏 
```
# 默认形式
# 其中titleWidth = 450可以修改默认的标题长度，适用于特别长的标题选择；
dashboardHeader(title = "My Dashboard",titleWidth = 450)
```
##### 消息菜单
##### 静态消息 
```
# 直接放置在dashboardHeader()内部即可；
dropdownMenu(type = "messages",
             messageItem(
               from = "Sales Dept",
               message = "Sales are steady this month."
             ),
             messageItem(
               from = "New User",
               message = "How do I register?",
               icon = icon("question"),
               time = "13:45"
             ))
```

##### 动态消息
```
## 感觉可以用于统计访问患者数等；
## ui:
dashboardHeader(dropdownMenuOutput("messageMenu"))
## server:
output$messageMenu <- renderMenu({
  # Code to generate each of the messageItems here, in a list. This assumes
  # that messageData is a data frame with two columns, 'from' and 'message'.
  msgs <- apply(messageData, 1, function(row) {
    messageItem(from = row[["from"]], message = row[["message"]])
  })
  
  # This is equivalent to calling:
  #   dropdownMenu(type="messages", msgs[[1]], msgs[[2]], ...)
  dropdownMenu(type = "messages", .list = msgs)
})
```
##### 通知菜单 
```
dropdownMenu(type = "notifications",
             notificationItem(
               text = "5 new users today",
               icon("users")
             ),
             notificationItem(
               text = "12 items delivered",
               icon("truck"),
               status = "success"
             ))
```
##### 任务菜单 
```
## 用于显示项目进度； ---这个如果能做成动态的效果会很好；
dropdownMenu(type = "tasks", badgeStatus = "success",
             taskItem(value = 90, color = "green",
                      "Documentation"
             ),
             taskItem(value = 17, color = "aqua",
                      "Project X"
             ),
             taskItem(value = 75, color = "yellow",
                      "Server deployment"
             ),
             taskItem(value = 80, color = "red",
                      "Overall project"
             ))
```
##### 禁用标题
```
dashboardHeader(disable = TRUE)
```
#### dashboardSidebar()侧边栏 
##### 构建侧边栏
```
# 其中"Dashboard"为侧边栏的实际显示名，`tabName`为引用名；
dashboardSidebar(
  ## 调整侧边栏的宽度；
  width = 350,
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Widgets", tabName = "widgets", icon = icon("th"))
  )
)
```
##### menuItem() 参数补充
```
# badgeLabel 在侧边栏中提供提示，用于说明功能性；
menuItem("Widgets", icon = icon("th"), tabName = "widgets",
         badgeLabel = "new", badgeColor = "green")
```
#####  提供不同网站间的相互引用
```
menuItem("Source code", icon = icon("file-code-o"), 
         href = "https://github.com/rstudio/shinydashboard/")
```
##### menuSubItem() 次级标签引入；-
```
# 需要包裹在menuItem内部，也即
menuItem("Charts", icon = icon("bar-chart-o"),
         menuSubItem("Sub-item 1", tabName = "subitem1"),
         menuSubItem("Sub-item 2", tabName = "subitem2"))
```
##### sidebarUserPanel() 展示用户信息-
```
# A panel displaying user information in a sidebar
sidebarUserPanel("User Name",
                 subtitle = a(href = "#", icon("circle", class = "text-success"), "Online")
                 # Image file should be in www/ subdir
                 # image = "./code_review.Ruserimage.png")
```                 
##### 提供多功能组件间的检索功能  
```
## 这个功能可以检索多个tib，需要参考
?dashboardSidebar()
sidebarSearchForm(label = "Enter a number", "searchText", "searchButton"), 

## 用法实现还是用处不大；
sidebarSearchForm(textId = "searchText", buttonId = "searchButton", 
                  label = "Search dataset", icon = shiny::icon("search"))
example_data <- data.frame(ID = 1:7, word = c("random", "words", "to", 
                                              "test", "the", "search", "function")) 

output$filtered_table <- renderTable({
  req(input$searchButton == TRUE)
  example_data[input$searchText,]
```
##### 添加文字和换行
```
# 换行
br(),br(),br(),br(),
# 添加文字方式1:
# p(HTML("<b>                           Teague Tian</b>"),style="white-space: pre-wrap"),
# p(HTML("<b>                           医学统计师</b>"),style="white-space: pre-wrap")
# 添加文字方式2
# p(HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),
#   HTML('&nbsp;'),strong("Teague Tian")),
# p(HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),
#   HTML('&nbsp;'),strong("医学统计师"))
```                 
##### 给sider()内部提供ID,用于动态监测多级程序运行 -
```
## 如果不提供ID，由于无法知道程序运行的检测，可能导致部分分项无法打开运行；  
  dashboardSidebar(
    sidebarMenu(
      # Setting id makes input$tabs give the tabName of currently-selected tab
      id = "tabs",
      
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      textOutput("res")))
  
  server <- function(input, output, session) {
    output$res <- renderText({
      paste("You've selected:", input$tabs)
    })
  }
```  
```
## 此外，在之前的版本中如果使用子项快速检索提取的位置，但不会返回来自哪一项；
# 目前的版本中可以使用input$sidebarItemExpanded来返回总级别
server{
req(input$sidebarItemExpanded)
paste("Expanded menuItem:", input$sidebarItemExpanded)}
```
##### 在server内用renderMenu{}构建多级标签 
```
dashboardSidebar(
    sidebarMenu(
      # Setting id makes input$tabs give the tabName of currently-selected tab
      id = "tabs",
      
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Widgets", icon = icon("th"), tabName = "widgets"),
      menuItem("Charts", icon = icon("bar-chart-o"),
               menuSubItem("Sub-item 1", tabName = "subitem1"),
               menuSubItem("Sub-item 2", tabName = "subitem2"))))
## 等价于
ui <- dashboardPage(
    dashboardHeader(), 
    dashboardSidebar(
      sidebarMenuOutput("menu"),
      textOutput("res")),
    dashboardBody(
      tabItems(
        tabItem("dashboard", "Dashboard tab content"),
        tabItem("widgets", "Widgets tab content"),
        tabItem("subitem1", "Sub-item 1 tab content"),
        tabItem("subitem2", "Sub-item 2 tab content") )))
  
server <- function(input, output, session) {
    output$res <- renderText({
      paste("You've selected:", input$tabs)})
    output$menu <- renderMenu({
      sidebarMenu(
        # Setting id makes input$tabs give the tabName of currently-selected tab
        id = "tabs",
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Widgets", icon = icon("th"), tabName = "widgets"),
        menuItem("Charts", icon = icon("bar-chart-o"),
                 menuSubItem("Sub-item 1", tabName = "subitem1"),
                 menuSubItem("Sub-item 2", tabName = "subitem2")))})}  
  
```  
  
#### dashboardBody()内部主体构建 
```
大多数仪表板的基本构建块是box. 盒子又可以包含任何内容。
  box(title = "Box title", height = 300, "Box content")
```
##### 在body内部`引用`多级侧边栏的方法
```
## 使用tabItems()来获取侧边栏，然后利用tabItem()来捕获每个侧边栏的内容；
dashboardBody(
  tabItems(
    # First tab content
    tabItem(tabName = "dashboard",
            fluidRow(
              box(plotOutput("plot1", height = 250)),
              
              box(
                title = "Controls",
                sliderInput("slider", "Number of observations:", 1, 100, 50)
              ))),
    # Second tab content
    tabItem(tabName = "widgets",
            h2("Widgets tab content")
    )))
```
##### 在body内部创建多个多级标签页
```
## 使用 tabBox 来创建多级标签页
##   
body <- dashboardBody(
  fluidRow(
    tabBox(
      title = "First tabBox",
      # The id lets us use input$tabset1 on the server to find the current tab
      id = "tabset1", height = "250px",
      tabPanel("Tab1", "First tab content"),
      tabPanel("Tab2", "Tab content 2")
    ),
    tabBox(
      side = "right", height = "250px",
      selected = "Tab3",
      tabPanel("Tab1", "Tab content 1"),
      tabPanel("Tab2", "Tab content 2"),
      tabPanel("Tab3", "Note that when side=right, the tab order is reversed.")
    )),
  fluidRow(
    tabBox(
      # Title can include an icon
      title = tagList(shiny::icon("gear"), "tabBox status"),
      tabPanel("Tab1",
               "Currently selected tab from first box:",
               verbatimTextOutput("tabset1Selected")
      ),
      tabPanel("Tab2", "Tab content 2"))))

shinyApp(
  ui = dashboardPage(
    dashboardHeader(title = "tabBoxes"),
    dashboardSidebar(),
    body),
  server = function(input, output) {
    # The currently selected tab from the first box
    output$tabset1Selected <- renderText({
      input$tabset1})})

```

  
  
### 布局参数
#### 行列布局
##### fluidRow() 
```
创造自适应的布局页面应用在body内部；
# Create a page with fluid layout
fluidRow(
  box(plotOutput("plot1", height = 250)),
  
  box(
    title = "Controls",
    sliderInput("slider", "Number of observations:", 1, 100, 50)
  ))

# 此外，还有fluidPage,与此fluidRow类似，但fluidRow更适合管理多级页面；
```


##### column(width = 4,...box())纵向展示
```
body <- dashboardBody(
  fluidRow(
    column(width = 4,
           box(
             title = "Box title", width = NULL, status = "primary",
             "Box content"
           ),
           box(
             title = "Title 1", width = NULL, solidHeader = TRUE, status = "primary",
             "Box content"
           ),
           box(
             width = NULL, background = "black",
             "A box with a solid black background"
           ))))
``` 
##### 混合模式布局
```
# 交叉使用fluidRow()和column()来构建参数分布图
body <- dashboardBody(
  fluidRow(
    box(
      title = "Box title", width = 6, status = "primary",
      "Box content"
    ),
    box(
      status = "warning", width = 6,
      "Box content"
    )
  ),
  
  fluidRow(
    column(width = 4,
           box(
             title = "Title 1", width = NULL, solidHeader = TRUE, status = "primary",
             "Box content"
           ),
           box(
             width = NULL, background = "black",
             "A box with a solid black background"  ))))
```         


### 在dashboard中使用css技巧 
暂定，还没找到合适的资源学习；
### 功能块参数
#### infoBox和valueBox
```
library(shinydashboard)
ui <- dashboardPage(
  dashboardHeader(title = "Value boxes"),
  dashboardSidebar(),
  dashboardBody(
    fluidRow(
      # A static valueBox
      valueBox(10 * 2, "New Orders", icon = icon("credit-card")),
      # Dynamic valueBoxes
      ## 基本逻辑是输入任意一种命名的box；
      ## 然后在下面使用input函数来承接；
      valueBoxOutput("progressBox"),
      valueBoxOutput("approvalBox")),
    fluidRow(
      # Clicking this will increment the progress amount
      ## 这里的actionButton就是承接用户输入，并反馈输出；
      box(width = 4, actionButton("count", "Increment progress")))))

server <- function(input, output) {
  output$progressBox <- renderValueBox({
    valueBox(
      paste0(25 + input$count, "%"), "Progress", icon = icon("list"),
      color = "purple")})
  
output$approvalBox <- renderValueBox({
    valueBox( "80%", "Approval", icon = icon("thumbs-up", lib = "glyphicon"),
      color = "yellow")})}

shinyApp(ui, server)
```

### 适配于shiny的特殊函数
**req()**
```
--判断数据返回的真假；
  如果数据返回不符合条件限制，则该运算停止；
output$filtered_table <- renderTable({
    +     req(input$searchButton == TRUE) 
  ......}
```  

## Rmarkdown使用进阶
### 参考书目

```
### 权威指南：
https://bookdown.org/yihui/rmarkdown/
# 下面这本书非常有用；！！！
https://bookdown.org/yihui/rmarkdown-cookbook/multi-column.html
## 其他参考博客：

速查表：https://www.rstudio.com/wp-content/uploads/2015/03/rmarkdown-reference.pdf?_ga=2.258283010.941366294.1644810156-143414957.1644300675
全局代码设置；
（https://yihui.org/knitr/options/）

```

### title和auther/YMAL

```
---
title: "ters"
author: "nicheerfeng"
date: "2022/2/14"
output: html_document  ## 另外还有pdf_document // word_document
---
```
复杂常用参数；
```
---
title: "Habits"
output: 
  html_document:
    toc: ture                  #是否展示目录
    toc_float: true             #目录的形式，是否浮动
    number_sections: true       #各个标题的数字标记是否展示
    df_print: paged           #表格的形式，paged创建可分页的表
    theme: cerulean              #文档主题，来源于Bootswatch(https://bootswatch.com/)
    highlight: tango               #指定语法高亮样式
    #css: css/styles.css       #加入额外的CSS，如果想从自己的css为文档提供所有样式，theme和highlight可设置为null
    #fig_width: 7                  #图片宽度
    #fig_height: 6                 #图片高度
    #fig_caption: TRUE       #图片设置，控制图形是否带有标题
    #code_folding: hide        #是否隐藏代码块
    #self_contained: false    #在外部文件中保留依赖关系
    #keep_md: true              #是否在knitr处理，pandoc渲染之后保存一份markdown文件的副本
    #template: quarterly_report.html   #可以使用模板选项替换基础pandoc模板
---
```

include参数：
主要用于在输出文档中添加额外的内容（在文档标题中或文档正文之前/之后包含内容）
```
---
title: "Habits"
output:
  html_document:
    includes:
      in_header: header.html
      before_body: doc_prefix.html
      after_body: doc_suffix.html
---
```

author选项的补充：
```
---
title:  'This is the title: it contains a colon'
author:
  - name: Author One
    affiliation: University of Somewhere
  - name: Author Two
    affiliation: University of Nowhere
tags: [nothing, nothingness] # 添加tag选项；
abstract: | ## 添加abstract
  This is the abstract.

  It consists of two paragraphs.
---
```

输出结果的补充：

```
---
title = data_view ##注意这里的``的作用是外循环中执行；
author: "Rsh"
date: '2020/09/1'
params:
	name: = 'input you data name'
output:
	html_document:  ## 输出html格式的 文件；
		theme:cerulean
		highlight:tango
---
```


### 字体与格式

```
###  字体：
**这是加粗的文字**
*这是倾斜的文字*`
***这是斜体加粗的文字***
~~这是加删除线的文字~~

### 使用内嵌的html来编辑rmarkdown中字号和颜色；
<font face="宋体">宋体</font>
<font face="宋体" color=red>红色宋体</font>
<font face="宋体" color=red size=5>5号红色宋体</font>

## 修改字体颜色：使用html作为元素插入到代码块中；
我是\textcolor{blue}{庄闪闪}呀！欢迎关注我的\textcolor{red}{公众号}：\textcolor{blue}{庄闪闪的R语言手册}。

## 另外一种打印红色字体的方法;
<span style="color: red;">**_DANGER:_** This is a warning.</span>  
```

### 辅助标记
```
## 分割线：
---这是分割线
***这也是分割线

### 代码注释不被编号，仅适用于rmarkdwon，而不适用于markdown中：
# Preface {-}

## 注释掉文本：
ctrl+shift+c:富文本注释方法，可以将标记的markdown文本注释掉；
```

时间设置：

```r
# date: "`r Sys.Date`" # 就可以输出当前的时间了；
```

内联代码：
在Rmarkdown语法中使用内联代码 `r函数+外部参数` 的方法来运行；
主要用于脚本的 title修改等方面使用；
比如说：[date:"2022-09-29"]


添加浮动导航栏：
```
---
title: "FAF"
author: "nicheerfeng"
date: "2022-09-27"
output: 
  html_document: ## 注意缩进很重要！
    toc: true
    toc_float: true
# toc_collapsed: true
# toc_depth: 3
# number_sections: true
# theme: lumen
---
```


### 常用代码标签

```
### 关闭打印：
(```){r example label, echo = FALSE, warning = FALSE}
  coding
(```)
# 解释：
注意：这里的代码即为使用R语言的代码，设置了代码块的名称为 example label，使代码块不包括在文档中，同时不输出警告信息。为了防止转译添加的小括号，正式代码中没有。注意代码行注释的意义在于格底部包含的代码块导航器中方便于查询；

### 主要常用代码标签: 注意这些标签仅在生成对应html文件时才有用；

1)echo = FALSE：隐藏代码，但运行代码并产生所有输出，曲线图，警告和消息。
2)eval = FALSE：显示代码，但不实际运行。这样的代码段如果有标签， 可以在后续代码段中被引用。
3)fig.show = "hide"：隐藏图。
4)eval=FALSE：运行代码，但不显示输出。这对于设置代码和注释条件很有用很有帮助。打开新的R Markdown文档时，您可以在第一个代码块中看到一个示例
5)message = FALSE：防止软件包在加载时打印消息。这也抑制了函数生成的消息。
6)results = "hide"：隐藏打印输出。
7)warning = FALSE：防止软件包和功能显示警告。


### 非常用代码块：
1) collapse选项：
一个代码块的代码、输出通常被分解为多个原样文本块中， 如果一个代码块希望所有的代码、输出都写到同一个原样文本块中， 加选项collapse=TRUE。 
2) results = 结果补充；
hide, 运行了代码后不显示运行结果。
hold, 一个代码块所有的代码都显示完， 才显示所有的结果。
3) 



### 全局代码设置：##### 
（https://yihui.org/knitr/options/）
（注意其他所有参数都可以通过knitr::opts_knit$set()来设计；）
### 修改工作目录：
knitr::opts_knit$set(root.dir = 'desired/directorypath')

```

### 非常用代码标签
#### 解决重复标签序列的问题
options(knitr.duplicate.label = 'allow')

#### 将R代码转为rmarkdown形式
```
在R脚本中使用注释时，使用 `#+` 的形式，而不是`#` ,然后就可以得到R脚本；

然后脚本在R中另外新建，进行命名运算，就可以得到一个新的Rmarkdown；
knitr::spin("r_script.R", knit = FALSE, format = "Rmd")
# 例如：
#' noew1     ## 注意#' 将代码块分行；
#+ hh        ## 构建新的代码块；
print("he")

#' noew21
#+ fhaufh
print("jj")
```

#### 将rmarkdown转为R脚本的形式
```
## 您必须指定documentation = 2返回#'注释中的完整文档。如果您的文档是纯代码，请指定documentation = 0。
knitr::purl("r_script.Rmd", documentation = 2)
```

#### Rmarkdown中执行换行：
使用对应的脚本注释后，添加两个空格，在再下一行添加信息的注释信息就可以得到对应的显示输出；

#### Rmarkdown中添加空白行：
在对应的注释行中添加： <br>

#### rmarkdown中添加分页：
插入：
\anewpage

#### 中文正常显示：


```r
pdf.options(family="GB1")
```
或者
pdf.options(height=10/2.54, width=10/2.54, family="GB1")

#### 代码整洁;

加选项tidy=TRUE可以自动重新排列代码段， 使得代码段格式更符合规范

#### 缓存代码结果，减少反复运行代码的风险：
cache=TRUE，
```{r  cache=TRUE}```


#### 修改页边距：
#### 在ymal中修改页边距：添加下面代码即可；
geometry: "left=2cm,right=2cm,top=2cm,bottom=2cm"

#### 文字缩进：
`
| When dollars appear its a sign
|   that your code does not quite align   ## 这行会自动缩进；
| Ensure that your math  
|   in xaringan hath  
|   been placed on a single long line
`

#### 控制文本输出的宽度：

```r
options(width = 300) ##这个越小，实际显示宽度越宽；
matrix(runif(100), ncol = 20)
```

```
##           [,1]      [,2]      [,3]      [,4]      [,5]      [,6]       [,7]       [,8]        [,9]     [,10]      [,11]     [,12]      [,13]     [,14]     [,15]      [,16]     [,17]     [,18]     [,19]     [,20]
## [1,] 0.6960712 0.3069383 0.9819571 0.8552895 0.7653925 0.5024550 0.67082752 0.53488795 0.405182251 0.7797764 0.34720928 0.3540547 0.89839023 0.4740390 0.7280266 0.55356090 0.4618603 0.1577412 0.7340524 0.4677173
## [2,] 0.6202972 0.4887921 0.5384296 0.2036990 0.4295990 0.3109159 0.18254373 0.19132696 0.754489071 0.2670022 0.78905704 0.5442485 0.93291664 0.7920058 0.5394844 0.55208703 0.5965311 0.4785389 0.6611132 0.6020849
## [3,] 0.4730650 0.4431196 0.6209938 0.3665569 0.1121109 0.8334048 0.97565366 0.59504944 0.007302545 0.8437772 0.51490802 0.8921057 0.57208919 0.1267545 0.4708121 0.47503083 0.3709792 0.6230875 0.6867100 0.9836900
## [4,] 0.1569810 0.8768297 0.6342785 0.6505393 0.8196403 0.7680294 0.07120192 0.08931203 0.250627395 0.5689549 0.04105461 0.5459946 0.99335892 0.5193698 0.6739556 0.05065583 0.4283386 0.9834827 0.3078525 0.2747295
## [5,] 0.3466511 0.6871590 0.5980750 0.7980536 0.5086072 0.8790518 0.84841277 0.54083108 0.148621666 0.1266417 0.00790655 0.6887134 0.01653424 0.5588134 0.2882896 0.63822283 0.3312142 0.7579220 0.2892632 0.3921596
```

#### 文档内的引用：这里就是根据代码块中的命名进行引用方法了；
See Figure \@ref(fig:cars-plot)


#### 全局设置的一些案例：

**案例1：**
结尾的斜线Figs/很重要。如果您使用 fig.path='Figs'，那么这些数字将进入主目录，但Figs作为其名称的初始部分。可以指定图的输出位置；

```r
knitr::opts_chunk$set(fig.width=12, fig.height=8, fig.path='Figs/',
                      echo=FALSE, warning=FALSE, message=FALSE)
```

**案例2：**
全局打印,是否打印对应的代码；选择FALSE则不打印代码，只输出结果；

```r
knitr: opts_chunk$set(echo = TRUE)
```


### 快捷键

```
## 添加新的代码块：
ctrl+alt + i 

## 运行当前块上方的所有块
Ctrl + Alt + P

## 运行当前的代码块：
Ctrl + Shift + Enter

## 运行所有块：
Ctrl + A + Enter
```

### 标题修改

```
## 参见https://zhuanlan.zhihu.com/p/435553617

## 自动左对齐；添加多行标题；（适用导出文件类型：html、pdf (不适用word））
---
title: |
  | Veryyyyyyy  

## 自动居中对齐（适用导出文件类型：html、pdf (不适用word））
---
title: |
  <center> Veryyyyyyy </center>
  <center> yyyyyyyyyyyyyy Looooo </center>

## 自动换行对齐: 换行符法（适用文件类型： word 、html）
---
title: "YYYY  \n  mdmdmdmd"

```

### 图片处理
#### 修改默认形式

默认形式为html格式的svg；

需要将其修改为pdf格式或者png格式：
knitr::opts_chunk$ set ( dev = "cairo_pdf" )
knitr::opts_chunk$ set ( dev = "png" , dev.args = list ( type = "cairo-png" ))


#### fig.show() 设置图片合适：
fig.show=‘asis’：表示plot在产生他们的代码后面 fig.show=‘hold’：所有代码产生的图片都放在一个完整的代码块之后 fig.show=‘animate’：表示将所有生成的图片合成一个动画图片  可以参考：https://bookdown.org/yihui/rmarkdown-cookbook/animation.html fig.show=‘hide’：表示产生所有图片,但是并不展示


#### 其他参数：
fig.width：设置图片输出的宽度
fig.height：设置图片输出的高度
fig.align 设置图片位置排版格式，默认为left,可以为right或者center
fig.cap ：设置图片的标题
fig.subcap：设置图片的副标题out.width和out.height选项指定在输出中实际显示的宽和高，如果使用如"90%"这样的百分数单位则可以自动适应输出的大小。

fig.keep = "all", 会把低级图形函数修改后的结果单独保存； "last", 仅保留最后一个图形； "first", 仅保留第一个图； "none", 所有图都不显示出来。

#### 插入外界图片：
```
![图的标题](xxx.png){width=50%}

## 举例：
{r fig.width=10/2.54, fig.height=10/2.54, out.width="80%"}
```

#### 优化PNG代码图显示：
注意需要安装OptiPNG
参见：https://bookdown.org/yihui/rmarkdown-cookbook/optipng.html
knitr::knit_hooks$set(optipng = knitr::hook_optipng)


### 表格输出
#### 美化表格输出
**使用kableExtra**
kable()函数的digits=选项可以控制小数点后数字位数， caption=选项可以指定表的标题内容。
knitr::kable(co)

用kableExtra(Zhu 2020[7])、huxtable (Hugh-Jones 2020[8])等扩展包来美化表格。
```
library(knitr)
library(kableExtra)
kable(iris) %>%
  kable_styling(latex_options = "striped")
```

**添加外边框：**
bootstrap_options = "bordered"
x_html <- knitr:: kable(head(rock), "html")
kableExtra::kable_styling(x_html,bootstrap_options = "bordered")


**设置表格的宽度：**
```
使用full_width = F使得表格横向不会填满整个页面
x_html <- knitr:: kable(head(rock), "html")
kableExtra::kable_styling(x_html,bootstrap_options = "striped",
                          full_width = F)
```

**表格对齐：**
```
x_html <- knitr:: kable(head(rock), "html")
kableExtra::kable_styling(x_html,bootstrap_options = "striped",
                          full_width = F,
                          position = "left")
```

**设置表格字体：**
```
x_html <- knitr:: kable(head(rock), "html")
kableExtra::kable_styling(x_html,bootstrap_options = "striped",
                          full_width = T,
                          font_size = 20) ## 设置表格字体；
```

**针对指定行或者列设置填充颜色：**
```
x_html <- knitr:: kable(head(rock), "html")
x_html <- kableExtra::kable_styling(x_html,
                                    bootstrap_options = "striped",
                                    full_width = T)
kableExtra::column_spec(x_html,1:2, ## 针对第1和2列；
                        bold = T,
                        color = "white", # 内部线颜色；
                        background = "#D7261E") # 外部背景颜色；
```

**针对指定单元格实行指定函数式运算：**
```
x_html <- knitr:: kable(head(rock), "html")
x_html <- kableExtra::kable_styling(x_html,
                                    bootstrap_options = "striped",
                                    full_width = T)
kableExtra::column_spec(x_html,1:2,
                        bold = T,
                        color = "white",
                        background = "#D7261E")
```
**使用滚动表格**
```
DT::datatable(linelist, 
              rownames = FALSE, 
              options = list(pageLength = 5, scrollX = TRUE), 
              class = 'white-space: nowrap' )
```

### 结果输出

在另外的R脚本中执行命令，然后输出输出rmd的结果：

```r
# rmarkdown::render('report.Rmd', 'html_document')
```

#### 生成可重复性的报告

使用get()函数来调用外部命令函数或者参数接口，从而实现函数批循环调用：

**例如：**
```
# ----- 在rmarkdown 中的 文件中执行这个文件 -------
mydata = get(params$name)
summary(mydata)
```
```
# ----- 在rmarkdown的ymal文件中加入这个参数 -----
---
title = data_view: ## 这种格式也称之为内联代码；
author: "Rsh"
date = '2020/09/1'
params:
	name: = 'input you data name'
output:
	html_document:  ## 输出html格式的 文件；
		theme:cerulean
		highlight:tango
---
```
```
# ----- 在外界R环境中调用下面循环体 
library(datasets)name_list <-c("airquality","mtcars","LifeCycleSavings")for(name in name_list){  render("用rmarkdown定制你的数据分析报告/可重复分析报告/模版.Rmd",         params = list(name=name),         output_file = paste0(name,'数据集概览'),  )}
```

### 补充知识点
#### 使用params
```
---
title: Parameterized reports
output: html_document
params: ## 这个参数可以提供的更多元的内置设置，这样就更方便引入外界参数；
  state: Nebraska
  year: 2019
  midwest: true
---

上面的代码，还可以启动gui来提高代码的访问效率：
rmarkdown::render("input.Rmd", params = "ask")
```
#### 代码块计时



```r
knitr::knit_hooks$set(time_it = local({
  now <- NULL
  function(before, options) {
    if (before) {
      # record the current time before each chunk
      now <<- Sys.time()
    } else {
      # calculate the time difference after a chunk
      res <- difftime(Sys.time(), now)
      # return a character string to show the time
      paste("Time for this code chunk to run:", round(res,3))
      ## 或者只记录时间不打印出来：
      # all_times[[options$label]] <<- res
    }
  }
}))
```

#### 纸张翻转

该函数适用于rmd文件中；
当在rmd文件中输出超长宽表时，希望能够使用word纸张横转的范式：
使用方法如下（注意在rmd文件中使用）：
\newpage 
<!---BLOCK_LANDSCAPE_START---> 

需要source()这个函数体；

```r
LANDSCAPE_STOP <-
  block_section( 
    prop_section(page_size = page_size(orient = "landscape"), type = "continuous" ))
```
而部分表格则希望有不横转的情况：
[r LANDSCAPE_STOP] -- 将此页横表转长表；    


