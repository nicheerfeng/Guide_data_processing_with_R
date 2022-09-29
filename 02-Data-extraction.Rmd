# (PART\*) 数据准备 {.unnumbered}

# 数据提取{#Data-extraction}

## 数据提取的基本范式

- 数据提取/采集（探索和检查数据质量问题，发掘数据的内部属性）
- 数据核验（数据有效性、时效性）
- 采用PDCA循环法来构造项目处理流程（计划、实施、检查和反馈处理）
- 关键主键的主导的多源数据融合

 
## 数据导入与导出
### 剪切板中处理数据

```R
## 读入粘贴板数据：
t1 = read.table("clipboard", header = T, sep = '\t')

# export the linelist data frame to your system's clipboard
clipr::write_clip(linelist)

## 调用文件选择器
read.csv(file.choose())
```

### 读取excle文件

```R
## 读取excel表数据：
library(xlsx)
st1 <- xlsx::read.xlsx("mtcars.xlsx", 1) # 1为sheet1；

## 另外一种优化的方式：
st1 <- openxlsx::read.xlsx("./fn_out/v11_log.xlsx") # 1为sheet1；
st1

## 使用openxlsx来读取和写入数据：
### 学习使用：openxlsx
library(openxlsx)
df <- data.frame(a=1:10,b=1:10,d=1:10)
# 第一步创建workbook
wb <- createWorkbook(creator = 'zhongyf',title = 'test')
# 第二步添加addworksheet
addWorksheet(wb,sheetName = 'test')
# 第三步写入数据writeDataTable
writeData(wb,sheet = 'test',x = df)
# 第四步保存saveworkbook。
saveWorkbook(wb, "test.xlsx", overwrite = TRUE)
```

### 加速读取及导出常用数据

#### 3.1 RIO包读取数据和导出一般数据

```R
##### rio包的数据读入与导出练习  ####
### 注意rio包调用的是data.table::fread()进行读取函数；
library(rio)
install.packages("rio")

### 导入csv文件：
mtcars <- import("data/mtcars.csv")
head(mtcars)

### 导入excle的多个子表：
multi <- import_list("data/multi.xlsx")

### 批量导入多个格式相同的项目文件：
paths <- Sys.glob("data/unemployment/*.csv")

### 借助here包引用进行数据导入：
# 这里是借助linelist_raw的路径来导入data数据；
linelist <- import(here("data", "linelist_raw.xlsx"))

### 借助export来批量导出数据：
export(linelist, here("data","clean", "my_linelist.rds"))

### 保存图片：
# 这里是将赋值outputs 输出到epicurves文件夹中的对应文件编号中
ggsave(here("outputs", "epicurves", "epicurve_2021-02-15.png"))
```

```r
## 优化现有的代码读入规则：
### RIO包读取特定条件下文档
# 读取指定的excle表：
my_data <- import("my_excel_file.xlsx", which = "Sheetname")

# 使用here()来辅助数据读取：
# Demonstration: importing a specific Excel sheet when using relative pathways with the 'here' package
linelist_raw <- import(here("data", "linelist.xlsx"), which = "Sheet1")`  


## 	缺失值读入：
## 将缺失值重新定义为为99；
linelist <- import(here("data", "my_linelist.xlsx"), na = "99")

## 将矩阵中的missing和空格等转为
linelist <- import(here("data", "my_linelist.csv"), na = c("Missing", "", " "))

## 跳过行：
linelist_raw <- import("linelist_raw.xlsx", skip = 1)  # does not import header row

###  指定文档的列名读入：
# note argument for csv files is 'col.names = '
linelist_raw <- import("linelist_raw.csv",
                       skip = 2,
                       col.names = linelist_raw_names)


```

#### 3.2 data.table包中的fread()

```R
## 用于高性能处理数据：
# 浅拷贝：只是拷贝列指针向量(对应数据框中的列)，而实际数据在内存中不做物理拷贝；
# 深拷贝：拷贝整个数据列到内存的另一位置，深拷贝这种冗余拷贝极大地影响性能。


@ 简要用法：
data.table() :生成数据框，并将数据框、列表和矩阵转为data.table()

## 键和索引：
data.table() 支持键和索引。
setkey(dt,v1,v3) ## 设置键；
setindex(dt,v1,v3) # 设置索引；


## 特殊符号： data.table()提供了一些辅助操作的特殊符号；
.():代替uist(
:=:按引用方式增加、修改列
.N:行数
.SD:每个分组的数据子集,除了by或 kelby的列
.SCots:与.sD连用,用来选择包含在SD中的列
.BY:包含所有by分组变量的1ist
.I:整数向量seq_len(nrow(x),例如DT[,,I[ whi ch.max( somecol)],by=grp]
.GRP:分组索引,1代表第1分组,2代表第2分组,
.NGRP:分组数
.EACHI:用于by/ kerby=. EACHI表示根据i表达式的每一行分组

## 管道符操作：也称为链式操作：
DT[...][...][...] 

## 数据读写：
函数fread()和fwrite()是data.tabel最强大的函数之二。
# 常用函数读取，不支持直接读取excel文件；
fread("DT.csv")
fread("DT.csv",sep="\t")
# 选择部分行列读取：
HH = fread("DT.csv",select =c("v1","v4"))
fread("DT.csv",dr
      op="v4",nrows=100)
# 读取压缩文件：
fread(cmd ="unzip - cq myfile.zip")
fread("myfile.gz")
# 批量读取：
c("DT.csv","FFF.csv") %>% 
	lapply(fread) %>% 
	rbindlist() ## 多个数据扛，按列表、按行合并；

## 写出数据：
fwrite(DT,"DT.csv")
fwrite(DT,"DT.csv",append =TRUE)
fwrite(DT,"DT.csv",sep="\t")
fwrite(setDT(list(0,list(1:5))),"DT2.csv") ##将列表作为对象写出；
fwrite(DT,"myfile.csv.gz",compress ="gzip")

##  数据链接：数据链接部分参见
# 左连接：
y[x,onm= "v1"] # 注意是以x为左表；
y[x]           # 若v1是键；
merge(x,y,all.x=TRUE,by="v1") ## 注意是只要加载了data.table包，将自动动用更快速的data.table::merge()，而不是R base中的merge()，二者语法相同；

# 非等链接：
通常数据连接是等值链接，即匹配列的值相等，才认为匹配陈宫，再将匹配成功的其他列连接起来；
很多时候需要非等连接，相当于是按条件链接，即匹配列的值不要求必须相等，只要满足一定条件就认为是匹配成功，再将匹配成功的其他列连接进来。
```

#### 3.3 Vroom包- 超大量级数据集读取

```R
library(here)
source(here("R", "common.R"), encoding = "utf-8")
## 数据读入：
patient_filepath <- fs::dir_ls(here("data", "preprocess", "patient_clean"),
                               glob = "*/*.csv")

patient <- vroom(patient_filepath) %>% 
  select(-"...1") %>% 
  distinct()

lab <- vroom(lab_filepath) %>% 
  select(-"...1") %>% 
  distinct() %>% 
  mutate(
    pbg_diff = end_pbg - start_pbg) %>% 
  replace_with_na_at(.vars = c("pbg_diff"),
                     condition = ~.x == 0)

## 数据写出：

```

#### 3.3 Arrow包- 数据整洁编码读取

```R
library(here)
source(here("R", "prepare and analyse data", "prepare.R"), encoding = "UTF-8")

patient <- open_dataset(
  here("data", "preprocess", "lan", "patient_clean")) %>% 
  collect() 

write_dataset(
  data_endpoint3, 
  here("data", "analysis", "lan", "endpoint3"),
  format = "parquet")
```

##### 3.3.1 open_dataset的高阶用法：

```R
# Task 1 - join
start <- Sys.time()
left_join(
    ## 使用open paraquet方法来加速数据的合并与读取
  open_dataset(
    source = "/home/data/CLAIM_HISTORY_MONTH"
  ) %>%
    filter(CLAIM_STATUS_TYPE_CD == "PC") %>%
    select(CH_SUBM_PROVIDER_ID,
           BNFT_TYPE_CD, 
           CH_REND_AMT),
  open_dataset(sources = "/home/data/Provider") %>%
    select(provider_id, provider_type, benefit_description),
  by = c("CH_SUBM_PROVIDER_ID" = "provider_id")) %>%
  collect()
  
end <- Sys.time()
end - start
```


### OFFICE格式及图片导出

#### 4.1 export()

```r
library(export)
## 导出ppt"
setwd("c:/Users/admin/Desktop/")
graph2ppt(file="effect plot.pptx", width=7, height=5)

## 导出word：
graph2doc

## 导出图片：
graph2bitmap
graph2eps
graph2jpg
graph2png
graph2svg
graph2tif

## 导出pdf版本：
graph2pdf
## 导出数据表在excle平台：
table2csv
table2csv2
table2excel
## 导出数据在word生成的数据表：
table2doc
## 导出数据生成对应的网页：
table2html
## 导出表到ppt中：
table2ppt
```

#### 4.2 eoiffice()

```R
## eoiffice ####
install.packages("eoffice")
library(eoffice)
library(ggplot2)
## 写入文件指定类型 ##
# topptx 将数字写入 powerpoint 文件
# todocx 将数字写入 word 文件
# tofigure 将图形写入不同的输出格式。

## 写入ppt和word中：
plot(mtcars$mpg, mtcars$disp, col = factor(mtcars$cyl), pch = 20)
topptx(p, filename = file.path(tempdir(), "mtcars.pptx"), width = 6, height = 4)

## 写入表格：
totable(head(mtcars), filename = file.path(tempdir(), "mtcars2.pptx"))

## 读取数据：
## 读取ptx和doc文件：
tabs <- inpptx(filename = file.path(tempdir(), "mtcars.pptx"), header = TRUE)

## 写入图片：可以到处pdf/png/eps等文件格式：
p <- ggplot(mtcars, aes(mpg, disp, color = factor(cyl))) + geom_point()
tofigure(p, filename = file.path(tempdir(), "mtcars.pdf"))
```

#### 4.3 officer()

```R
## officer ## 
library(officer)
library(magrittr) # Package `magrittr` makes officer usage easier.
library(ggplot2)

my_doc <- read_docx()  #初始化一个docx , 里面不填路径使用默认模板
styles_info(my_doc)  #显示信息

gg <- ggplot(data = iris, aes(Sepal.Length, Petal.Length)) + 
  geom_point()

my_doc %>%   #可以使用magrittr方式一步步添加
  body_add_par(value = "Table of content", style = "heading 1") %>% 
  body_add_toc(level = 2) %>% 
  body_add_break() %>% 
  
  body_add_par(value = "dataset iris", style = "heading 2") %>% 
  body_add_table(value = head(iris), style = "table_template" ) %>% 
  
  body_add_par(value = "plot examples", style = "heading 1") %>% 
  body_add_gg(value = gg, style = "centered" ) %>% 
  
  print(target = "body_add_demo.docx")
```

#### 4.4 rrtable()

```R
# install.packages("rrtable")
library(rrtable)
## 自动化报表输出：
## 生成报表图片：
df2flextable2( sampleData3 ,vanilla= FALSE )

## 生成描述性统计图片：
mytable2flextable( mytable(Dx~.,data=acs) ,vanilla= FALSE )

## 生成对应统计分类的html格式：
data2HTML(sampleData3) 

## 导出OFFICE
data2docx(sampleData3) 
data2pptx(sampleData3) 
data2pdf(sampleData3) 

## 指定形式文件的格式的输出：
## 表导出ppt和word：
table2pptx(ft)
table2docx

## 导出图片到word或者ppt中：
code2docx(plot(iris))
require(ggplot2)
gg=ggplot(data=mtcars,aes(x=wt,y=mpg))+geom_point()
code2docx(ggobj=gg)
code2pptx(ggobj=gg)

## 读取office数据：
file2docx()
file2HTM()
file2pdf()
file2pptx
```

### 在R中使用sql

#### 5.1链接数据库：

```
#安装RODBC包  
install.packages("RODBC")   
library(RODBC)  
mycon<-odbcConnect("mydsn",uid="user",pwd="rply")  
#通过一个数据源名称（mydsn）和用户名（user）以及密码（rply，如果没有设置，可以直接忽略）打开了一个ODBC数据库连接  
  
data(USArrests)  
#将R自带的“USArrests”表写进数据库里  
sqlSave(mycon,USArrests,rownames="state",addPK=TRUE)  
#将数据流保存，这时打开SQL Server就可以看到新建的USArrests表了  
rm(USArrests)  
#清除USArrests变量  
  
sqlFetch(mycon, "USArrests" ,rownames="state")  
#输出USArrests表中的内容  
sqlQuery(mycon,"select * from USArrests")  
#对USArrests表执行了SQL语句select，并将结果输出  
  
sqlDrop(channel,"USArrests")  
#删除USArrests表  
close(mycon)  
#关闭连接  
————————————————
版权声明：本文为CSDN博主「悟乙己」的原创文章，遵循CC 4.0 BY-SA版权协议，转载请附上原文出处链接及本声明。
原文链接：https://blog.csdn.net/sinat_26917383/article/details/51601539
```

#### 5.2 在R中执行sql命令：

```R
## 参见：
https://dept.stat.lsa.umich.edu/~jerrick/courses/stat701/notes/sql.html#sql-queries

library(dplyr)
library(sqldf)

## SQL版本：
SQL3 = sqldf("SELECT sex, COUNT(primaryid) as Total
FROM demo_all
GROUP BY sex
ORDER BY Total DESC ;")

## R版本：
R3 = demo_all%>%group_by(sex) %>%
        summarise(Total = n())%>%arrange(desc(Total))
 
compare(SQL3,R3, allowAll=TRUE)
TRUE

```

