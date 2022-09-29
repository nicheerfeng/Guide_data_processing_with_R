# 构筑R package {#-Build-R-package}
## 构筑R package相关参数配置
### 创建本地环境并使用renv初始化本地安装环境

```
## 1 创建本地环境并使用renv初始化本地安装环境：----
library(devtools)
library(usethis)
library(roxygen2)
library(tidyverse)
library(lubridate)
library(gtsummary)

setwd("")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# ## 在指定路径下创建R包：
usethis::create_package("./")

## 进行本地版本控制：
# install.packages("renv")
# 初始化环境
renv::init()
# 保存当前所用的包环境，当然我们才刚刚开始开发，别的包都没有引入
renv::snapshot()

```

### 常用配置

#### 描述文件（description）的常用配置

```
## 2.1 描述文件（description）的常用配置 ----
## Description：
# Description是对包的描述，每行不超过80个字符，行间使用4个空格分开

## Version：
# Version表示版本号，版本号最少要有2个整数中间用点号或者横线隔开
# 推荐的格式：
# releaesd版本由3个数字构成：<major>.<minor>.<patch>
# In-development版本由4个数字构成，第四个是开发版本，从9000开始，所以包的第一个版本是0.0.0.9000

## Auther@R：
Authors@R:
  person(given = "First", ## given在前(名)，family在后(姓)
         family = "Last",
         role = c("aut", "cre"),
         # cre creator or maintainer 有问题时应该联系额人
         # aut 对包贡献最大的人
         # ctb 贡献者
         # cph copyright holder nicheerfeng@gmail.com 如果版权是作者以外的人或机构，要注明
         email = "first.last@example.com",
         comment = c(ORCID = "YOUR-ORCID-ID"))

## 添加R包：在DESCRIPTION中添加说明文件；
## 添加依赖的R包：
# Imports
# 描述的是包工作所必需的包，在我们的包被安装的时候，
# 如果这些包之前没有被安装，这个时候会被安装
# Suggests
# 不是必需安装的，可能在示例数据，运行测试，

# 从R包中指定函数输出；
use_package("forcats")
use_package(package, type = "Imports", min_version = NULL)

## 添加许可：
# mit -- 任意使用；
# gpl3 -- 任意使用，需公开源码；
# ccby -- 不可用于商用；

##  添加mit-license:
# 这一步会在三个文件中添加参数：
# 第一个是在LICENSE中添加字符描述；
# 第二个是在LICENSE.md中添加详细描述；
# 第三个是在Description中添加license的字段描述；
## 这里还可以再引号内输入姓名，用于指明该文件的说明者；
usethis::use_mit_license("MIT license")

## 在description中添加指定描述：
# URL: https://github.com/swsoyee/rPackageTutorial
# BugReports: https://github.com/swsoyee/rPackageTutorial/issues
```

#### namespace的常用配置

```
## namespace的常用配置：----
创建一个专门用于该R包进行环境开发的R文件，用于封装参数；
注意：下面的null是必须的；
此外， `%>%`的使用需要使用usethis::pip()来生成描述文件，
把描述文件的内容复制下来和这个一起即可。
另外，这个函数文件中通常还会涉及到由非标准引用造成的check错误；
需要使用utils::globalvirables()来添加对应的描述。
#' @import dplyr
#' @import tibble
#' @import lubridate
#' @import parallel
#' @import gtsummary
#' @import tidyselect
#' @import methods
#' @importFrom stringr str_replace str_c
#' @importFrom rlang enquo
#' @importFrom utils globalVariables
#' @importFrom utils data
#' @importFrom pacman p_load
#' @importFrom purrr map map_df
#' @importFrom magrittr %>%
#' @importFrom tidyr spread separate_rows replace_na crossing separate gather
#' @importFrom stats sd
NULL

#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL
```

### 构建函数

#### 构建函数文
```
use_r("test")
```

#### 函数参数说明

##### 常规顺序

```
## 3.2.1 常规顺序：----
# 函数名、描述(Description)、函数体(Usage)、参数(Arguments)、
# 详情补充(Details)、输出值(Value)、举例(Examples)、
#
# 其中：
# 默认常规顺序是：
# 1 函数名
# 2 描述(Description)
# 3 详情补充(Details)
# 3.1 补充说明(see also) @seealso
#
# 次参数顺序：
# 4 参数(Arguments) -- @param
# 5 输出值(value) -- @return
# 6 举例(Examples) -- @examples

```

##### 补充参数

```
## 3.2.2 补充参数：-----

## 继承参数：
# @rdname 从主函数中引用，然后在同一描述文档中展示功能；
#' @rdname tr -- 其中tr为主函数；
#' @family dfa  -- 一般用于函数族的描述，类似于ggplot2中的函数控件；

## 参考文献：
#' @references  -- \emph ：表示斜体；
#' Michael Friendly (2002).
#' \emph{Corrgrams: Exploratory displays for correlation matrices}.
#' The American Statistician, 56, 316--324.
#' D.J. Murdoch, E.D. Chow (1996).

## 添加作者：
#' @author
#'

## 添加声明文档：
#' @note
#' @seealso

## 添加示例文档：
## @example可以 vignettes/example-corrplot.R

#' @example


```

##### 辅助描述

```
## 添加网页引用：
# \url{https://en.wikipedia.org/wiki/Integer_overflow}

## 添加本R包的函数引用：
\link{function}
## 引用其他R包的另外一种写法：
\code{\link{print}}

## 添加其他R包的函数引用：
\link[base]{cut()}

## 代码提示：
`function` / [function()]


```

#### 函数构建的经验

```
# 使用invisible()来隐藏函数输出；
funs = function(x,y){
  z = x+y
  invisible(z)
}
# 使用...来输入无限多的参数：
funs = function(...){
  args = list(...)
  Reduce(`+`,args)
}
funs(1,2,3,4,5,6)

# 函数中套用函数：
funs = func(a,b){
  b(a)
}
funs(c(1,2),mean)

## 辅助函数构建的非标准引入：
## 参数的非标准引用：必须发生在函数内部：
terst = function(data,test){
  var = enquo(test)
  datatable2 %>% select(!!var) %>% dplyr::n_distinct(.)}
terst(datatable2,patient)

## 字符串的非标准引用：
raw_patient_id = "patient"
var <- sym(raw_patient_id)
datatable2 %>% select(!!var) %>% dplyr::n_distinct(.)

```

### 函数测试 

#### 函数快速测试

```
devtools::load_all() # - 快捷键Ctrl + Shift + L ；

```



#### 函数单元测试

```
# 使用testthat包：
# context  写一个简短的介绍文件中的测试内容
#
# expect_equal()是基于all.equal()的 - 相对估计；
# expect_identical()是基于identical - 相对精准；
# expect_match 是基于grepl，识别字符使用；
# expect_output()匹配输出类型；
# expect_message()检查信息；
# expect_warning()检查warning；
# expect_error()检查错误
# expect_is()检查某个对象是不是继承自一个特定的类：
# expect_true() and expect_false() 当没有其他的expectation可用时使用

## 测试案例：
## expect_is() 检查某个对象是不是继承自一个特定的类：
model <- lm(mpg ~ wt, data = mtcars)
class(model)
#[1] "lm"
expect_is(model, "lm")

## 匹配输出类型：
a <- list(1:10, letters)
str(a)
# List of 2
# $ : int [1:10] 1 2 3 4 5 6 7 8 9 10
# $ : chr [1:26] "a" "b" "c" "d" ...
expect_output(str(a), "List of 2")

## 测试案例的基本完整写法：
## 包含生成覆盖率检验---
首先一个逻辑是这个测试是针对的函数本身，并不针对函数内部的细节参数；
其次，他有一个相对固定的格式来判断的数据 的结果可行性；
本质：他是example的一个结果验证：
context("Just testing printer")
test_that(
  "just test whether printer is ok".{
    set.seed(1)
    res = printer(x = rnorm(5),r -r(norm(5)))
    expect_equal(nrow(res),5)})
写完测试后的检查：
devtools::test()
检查完后输出：测试覆盖率；
在addins中输入coverage，点击即可输出每个函数的测试覆盖率。
然后输入：
covr::package_coverage(type ="all")
输入该参数后：
会在.trasvis.yml文件中生成：
after_success:
`Rescript e `covr::codecov(type ="all")``
```

### 添加项目内置数据集

#### 仅供函数内部使用

```
# 如果想要存储原始数据，可以放到inst/extdata里面
usethis::use_data_raw()
usethis::use_data(data_med,internal = TRUE,overwrite = TRUE)
```

#### 使用inst 或者DATA

```
usethis::use_data(data_med,internal = FALSE,overwrite = TRUE)
# 内部使用形式的具体说明：
# 下面的data不需要指明加载路径，只需要提供输入参数的名称即可；
#' @format  .....
#' \describe{
#' \item{price}{price,in Us dollars}
#' \item{carrt}{....}
#' ...
#' }
#' @source \url{http://www.doa.info/}
"data"

```

### 添加辅助描述文件

#### 添置readme/NEWS/buideignore代码美化和代码规范

```
## buideignore文件的写法：
^data$
^data\.md$


## 添加说明文件：
usethis::use_readme_md()
# 创建可执行的read.me文件方便进行参数构建；
use_readme_rmd()

## 增加 NEWS 页面，用于记录每一次升级所做出的变更
usethis::use_news_md()
# 添加 Code of Conduct
# 根据弹出的提示，把自动生成的内容添加到 README.Rmd 中并且重新生成 README.md 后提交本次变更
usethis::use_code_of_conduct()

## 使用styler进行代码美化：
install.packages("styler")
styler::style_pkg()

## 代码规范：
## install.packages("lintr")
# 对整个包进行不符合规范的代码查询
lintr::lint_package()
```

#### 构建vignette

```
usethis::use_vignette("pmed_RWD")
```

#### 添加包的说明在线网站

```r 
## 添加pkg：
library(usethis)
usethis::use_pkgdown()
## 安装远程厂库到本地：
use_github_file()
## 管理issuse:
use_github_labels()
## 还可以使用：直接在R本地执行远程推送；
use_github(protocol = "https")

## 添加origin
#git remote add main https://github.com/nicheerfeng/pmed
## 添加token
# git remote set-url origin https://oauth2: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@github.com/nicheerfeng/pmed.git
## 推送token：
# git push -u origin main

## 补充：
# 以前的github里的master，就是现在的main。


# 实现将 pkgdown 站点自动发布到 GitHub 页面所需的 GitHub 设置：
# 创建新的分支，用于可视化展示站点信息；
use_pkgdown_github_pages()

# 构建站点：
pkgdown::build_site()

# 关于构建网站之后404原因的解释：
# https://pkgdown.r-lib.org/reference/build_site.html?q=development#development-mode
# _pkgdown.yml 需要设置：
development:
  mode: release
# 因为原始的mode为auto，自动识别0.0.0类型为禁止发布类型；

```

### R包整体测试

#### 常规测试

```
devtools::check() # -Ctrl + Shift + E

## 拼写检查：
devtools::spell_check()

## 多平台测试：
devtools::check_win_devel() # win 平台
usethis::use_github_action_check_standard() # git多平台；

## 复杂检查：
devtools::release()
```

#### 测试报错结局方案

```
## (1) 测试问题：--- no visible binding for global variable a ->用了dplyr
# 解决方案 1 (这里用到了utils包哦)
utils::globalVariables(c("a"))
# 这种方法的具体实现思路：
在R文件下新创建一个env的R文件，并在其中
添加对应的，使用usethis::check()显示参数未发现的变量；
将所有参数未发现的变量，打包成字符串集合的形式放置在globalVariables()内部；
if(getRversion() >= "2.15.1") utils::globalVariables(c("!!",":=",".","*"))

# 解决方案二
# 这种方案是在每个执行函数文件中将对应的未check到的变量赋值未null。不能使用字符串的形式，必须单个命名为null。
a <- NULL
```

### 构建及安装R包

```
devtools::build()

## 然后就可以通过install()函数来安装这个包：
devtools::install()
library(tdftest)
a = c('a')
b = c('b')
fbind(a,b)

## 另外一种本地安装的方法为：
install.packages("~/test/test.tar.gz", repos = NULL, type = "source")
```

### 构建后上传到github多平台检查

```
暂定；
```

