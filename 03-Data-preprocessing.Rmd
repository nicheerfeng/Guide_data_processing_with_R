# 数据预处理 {#Data-preprocessing}

**该专题涉及到数据抽样、数据清洗、重组、转换及衍生及特征处理**

## 数据抽取

- 样本中的缺失值分布（频率）也要与全集一致。
- 针对稀有事件抽样时，由于人为增加了目标事件的浓度，导致样本中的事件和非事件的比例与全集不一致，所以需要在建模过程中，对新样本使用加权。
- 评估抽样设计方法与下游工作链的承接关系
- 基于抽样数据做规模分析评估（可信度），保证后续分析的有效性

```R
## 方法1：within
dragons <- within(dragons, sample <- factor(mountainRange:site))

## 方法2：
# 简单随机抽样：
sample（x,n,replace=T）
dplyr::sample_n((tbl, size, replace = FALSE)

参数说明：tbl数据，size选取的数据行数，replace=true/false是否替换样本（主要参数）

# 分层抽样：
strata（data，stratanames=NULL,size,method=c("srswor","srswr","poisson","systematic"),pik,description=FALSE）

# 整群抽样：
cluster（data，clusteraname,size,method=c("srswor","srswr","poisson","systematic"),pik,description=FALSE）
```

## 数据清洗
### 选择数据子集

- 根据项目方案中不同策略人群和研究终点来设计不同的取样数据子集
- 数据子集的确定应使用统一的函数构造体系，并且保存成对应的rds文件方便后续读取
- 在此步骤提取的数据子集为粗子集

### 数据去重

```r
## 数据去重：
distinct()
unique()
dplyr::n_distinct()

## 删除重复数据： 
new_data1 <- unique (data )   ---删除的为所有字段均重复的记录

## duplicated
# 使用这个函数返回的是重复元素的下表或者布尔逻辑；
rattlerdups=duplicated(rattler[, c("lon", "lat")])
rattler <-rattler[!rattlerdups, ]

## 使用dplyr包内置的distinct()进行去重：
# added to a chain of pipes (e.g. data cleaning)
obs %>% 
  distinct(across(-recordID), # reduces data frame to only unique rows (keeps first one of any duplicates)
           .keep_all = TRUE) 

## 使用 janitor来使用get_dupes()来实现复杂去重，实际上是观测重叠值；
obs %>% 
  janitor::get_dupes() ## 全部列重复；
obs %>% 
  janitor::get_dupes(-recordID)   ##除recordID 以外的列均重复
obs %>% 
  janitor::get_dupes(name, purpose) ## 仅name和purpose 列重复；
```

### 缺失值处理

- 从业务逻辑和商业意义上，分析缺失原因，是否需要补全缺失值，应该如何做。
- 直接删除带有缺失值的观察对象（或称数据元组），适用于带有缺失值的数据元组比例很少的情况。
- 直接删除有大量缺失的变量，适用于缺失值占比超过50%，且不具备商业意义。
- 对缺失值进行替换，利用全集中的代表属性（众数、均值、MAX、MIN等），或者人为定义替换。
- 对缺失值进行赋值，用回归、决策树、贝叶斯定理等模型去预测缺失值。
#### 删除缺失：

```r
###################################### 缺失值删除： ######################
## 去除包含缺失值的行：
na.omit 函数语法： na.omit (x)
drop_na(case_id, date_onset, age)  ## 删除指定缺失值的行：

## 删除包含所有含0的值或者行：
row_sub = apply(nall1, 1, function(row) all(row !=0 ))      
nall1$.[row_sub]   

# 删除某些列都是na的行，借助if_all()也可以；
df_dup %>% filter(!if_all(where(is.numeric),is.na))                

## 使用filter()定向删除na值：               
linelist <- filter(linelist, !is.na(case_id))               
                
#################################### 缺失值替换 ######################## 
## 重新对缺失值赋值：
elmat[is.na(elmat)]= 0

## 缺失值替换：
## replace_na()仅能针对单行进行匹配：
linelist %>%   mutate(hospital = replace_na(hospital, "Missing"))
linelist %>%  mutate(hospital = na_if(hospital, "Missing"))

## 使用mutate_at()来指定列进行条件替换：
data %>% 
  mutate_at(vars("高血压", "心脏病", "糖尿病"), ~ replace_na(., 0)) 

## 全表的缺失值替换：                
linelist %>% replace(is.na(.),0)
                
## 逻辑判断条件下的na值定义：
linelist <- linelist %>% 
  mutate(temp = replace(temp, temp > 40, NA))
                                
## 使用forcats进行缺失值填充：
mutate(hospital = forcats::fct_explicit_na(hospital))
mutate(delay_cat = forcats::fct_explicit_na(delay_cat, na_level = "Missing delay"))
```

#### 缺失值处理R包：ZOO

```r
RMSE <- function(sim, obs){
  sqrt(sum((sim-obs)^2)/length(sim))
}
library(zoo)
library(forecast)

load(url("https://userpage.fu-berlin.de/soga/300/30100_data_sets/NA_datasets.Rdata"))

plot.ts(temp.sample, 
        main = 'Weather station Berlin-Dahlem', ylab = "°C")

plot.ts(temp.NA, ylab = expression("°C"), 
        cex.main = 0.85,
        type = 'o', 
        cex = 0.3, 
        pch = 16)


length(is.na(temp.NA))
na.perc <- round(sum(is.na(temp.NA))/length(temp.sample),3)*100
na.perc

## IMPUTING ##
temp.NA.imp <- na.aggregate(temp.NA, FUN = mean, as.yearmon)

## 除了na.aggregate外，还有另外的几种形式：￥￥￥￥￥￥￥￥￥
## na.locf()：
用最近的非NA取代每个NA的通用函数。
对于每个个体，缺失的值将由该变量的最后一个观察值替换。
## na.StructTS()
用季节卡尔曼滤波器填充NA值的泛型函数。
# na.interp()
对非季节序列采用线性插值，对季节序列进行周期性stl分解，替换缺失值。

## ERROR ##
rmse.NA <- RMSE(temp.NA.imp, temp.sample)
rmse.NA

## PLOTTING ##
plot.ts(temp.NA.imp, ylab = expression("°C"), 
        main = "na.aggregate()",
        cex.main = 0.85, col = 'red')
points(temp.NA,
       cex = 0.3, 
       pch = 16)
text(2013.5, -10, paste('RMSE: ', round(rmse.NA,4)), cex = 0.85)
legend('bottomright', legend = 'Imputed values', 
       lty = 1, col = 'red', cex = 0.65)

## IMPUTING ##
temp.NA.imp <- na.locf(temp.NA, fromLast = F)
## ERROR ##
rmse.NA <- RMSE(temp.NA.imp, temp.sample)
rmse.NA

## PLOTTING ##
plot.ts(temp.NA.imp, ylab = expression("°C"), 
        main = "na.locf()",
        cex.main = 0.85, col = 'red')
points(temp.NA,
       type = 'o', 
       cex = 0.3, 
       pch = 16)
text(2013.5, -10, 
     paste('RMSE: ', round(rmse.NA,4)), 
     cex = 0.85)
legend('bottomright', legend = 'Imputed values', 
       lty = 1, col = 'red', cex = 0.65)

## IMPUTING ##
temp.NA.imp <- na.interp(temp.NA)
## ERROR ##
rmse.NA <- RMSE(temp.NA.imp, temp.sample)
rmse.NA
## [1] 1.982686


## PLOTTING ##
plot.ts(temp.NA.imp, ylab = expression("°C"), 
        main = "na.interp()",
        cex.main = 0.85, col = 'red')
points(temp.NA,
       cex = 0.3, 
       pch = 16)
text(2013.5, -10, 
     paste('RMSE: ', round(rmse.NA,4)), cex = 0.85)
legend('bottomright', legend = 'Imputed values', 
       lty = 1, col = 'red', cex = 0.65)
```

#### 缺失值评估

```R
参见:https://zhuanlan.zhihu.com/p/455056143
## 常用函数:
summary()
str()
skimr::skim
dplyr::glimpse() ## 展示套叠数据格式；
dplur::ff_glimpse() ## 展示数据全貌，包含缺失值和summary的汇总；


## 使用visdat:
library(visdat)
vis_dat(airquality)

## 使用可视化方法探索缺失值的存在：
library(naniar)

ggplot(airquality, 
       aes(x = Solar.R, 
           y = Ozone)) + 
  geom_miss_point()

- 分面查看缺失值：
ggplot(airquality, 
       aes(x = Solar.R, 
           y = Ozone)) + 
  geom_miss_point() + 
  facet_wrap(~Month) + 
  theme_dark()

- 统计缺失值：
pct_miss(linelist)
pct_complete_case(linelist)
```


##### 3.2.5 一致化处理

- 数据集中会存在某一个数据列的数据至标准不一致或命名规则不一致的情况，可以使用分列功能将不一致的数据列中的数据值进行拆分。例如将薪水7k-9k划分为最高薪资（9k）和最低薪资（7k）。

- 标准化处理：

  对于很多模型，如线性回归、逻辑回归、Kmeans聚类等，需要计算不同特征的系数，或者计算样本距离。

  这种情况下，如果不同特征的数值量级差的特别大，会严重影响系数和距离的计算，甚至这种计算都会失去意义；所以在建模前必须要做的就是要去量纲，做标准化处理。

  当然有些模型是不需要做数据标准化处理的，如决策树、随机森林、朴素贝叶斯等。

- 字符型数据转化成数值性数据

- 将分类变量转为虚拟亚变量，实现one-hot编码

##### 3.2.6 异常值处理

- 看数据是否处于异常，可以用 3σ原则，PCA，箱线图等等，至于是否要处理也要看建模的目标对于异常值的考虑。

### 一致化处理

```
- 数据集中会存在某一个数据列的数据至标准不一致或命名规则不一致的情况，可以使用分列功能将不一致的数据列中的数据值进行拆分。例如将薪水7k-9k划分为最高薪资（9k）和最低薪资（7k）。

## 标准化处理：
  对于很多模型，如线性回归、逻辑回归、Kmeans聚类等，需要计算不同特征的系数，或者计算样本距离。
  这种情况下，如果不同特征的数值量级差的特别大，会严重影响系数和距离的计算，甚至这种计算都会失去意义；所以在建模前必须要做的就是要去量纲，做标准化处理。
  当然有些模型是不需要做数据标准化处理的，如决策树、随机森林、朴素贝叶斯等。

- 字符型数据转化成数值性数据

- 将分类变量转为虚拟亚变量，实现one-hot编码
```

### 异常值处理

```
看数据是否处于异常，可以用 3σ原则，PCA，箱线图等等	，至于是否要处理也要看建模的目标对于异常值的考虑。
```

#### 1.6.1 处理零值数据

```R
##1.1 去除那些每个值都是零的行
##转换思维，每个数据都为0的行，则该行的和也为0
data1=data[which(rowSums(data) > 0),]
head(data1)

##1.2 去除那些存在值为零的行
data2=data[which(rowSums(data==0)==0),]
head(data2)

##换一种思路，将为零的值替换为NA
#2.1 把０替换成NA,然后按照处理缺失值的办法，
data0[data0==0]<-NA
data3=na.omit(data0)##去除所有含NA的行
head(data3)

##3.1 使用complete()函数自主操作
data4=data0[complete.cases(data0),]

##3.2 去除前六列含有NA的z值，可自由设置
data5=data0[complete.cases(data0[,1:6]),]

###4.1 x[is.na(x)] <- 0使用该函数对NA值进行赋值
data0=read.csv("eSet.csv",row.names = 1)
data0[data0==0]<-NA
data0[is.na(data0)]<-0

##4.2 可以把NA赋值给任何值,相互换
data0[data0==111]<-NA
data0[is.na(data0)]<-"h"
```

### 插值填补

```R
data %>% group_by(patient_id) %>% arrange(result_datetime) %>% 
    tidyr::fill(lab_fp_6,.direction ="updown")
```



## 数据重组与转换
### 数据修改-数据增加与删除

#### 数据增加：

```R
## 添加行：dplyr::add_row()
# 使用.before和.after.指定要添加的行的位置。.before = 3将新行放在当前第三行之前。默认行为是将行添加到末尾。未指定的列将留空 ( NA)。
linelist <- linelist %>% 
  add_row(row_num = 666,
          case_id = "abc",
          generation = 4,
          `infection date` = as.Date("2020-10-10"),
          .before = 2)

## 添加新列：
transform(BOD,ZIMU=(paste0("A",1:6)))

## 使用tribble()生成新的数据：
## 只需要添加一个~x1即可；
df <- tribble( ~x1, "a,b,c", "d,e,f,g") 
 
```



#### 数据合并：

```R
## 左右列合并：
# 常用包括左右链接、全链接、内链接、半链接和反链接；
left_join(x,y,by) ##仅保留与左侧相对应的行
right(x,y,by)
full_join(x,y,by) ## 保留全部对应的行，对不上为空值；
inner_join(x,y,by) ## 保留相同对应的行，对不上删除；
semi_join(x,y,by) ##  与内链接类似；
anti_join(x,y,by)  ## 

## 多个表相互链接：
前面讲过，reduce可以实现逐步迭代的功能，因此可以使用reduce来逐步实现累计合并的功能：
files = list.files("./da/",pattern = "xlsx", full.names =TRUE)
map(files,read_xlsx) %>% reduce(full_join,by = "name")
## 单个表多个sheet链接：
path  = "./yue.xlsx"
map(excel_sheet(path)),~ read-xlsx(path,sheet = .x)) %>% 
				reduce(full_join, by ="name")
```

#### 添加分类列：

```R
#### # 生成快速随机代码 
x <- rep(1:14, 14)  ## 1:14 按次序生成14遍

y <- as.integer(gl(14, 14)) ## 这里是生成分组序列，然后再换位字符串；1到14各自14遍

age_seq = seq(from = 0, to = 90, by = 5) # 0-90,5做间隔

 (f <- gl(2,5, labels=c("CK", "T")))
# [1] CK CK CK CK CK T  T  T  T  T  
```

#### 数据删除：

```R
## 方法1：base::transform()
## 删除某列：
transform(BOD,demand= NULL )

## 方法2：
data$size      <- NULL
data[["size"]] <- NULL
data[[3]]      <- NULL
data           <- subset(data, select=-size)

## tidy:
rows_delete(tibble(var="AAA"))
```

#### 数据分列与合并
##### 数据分列与合并：

```R
## 数据分列与合并：
## 1 数据分列：
## 1.1 一列转多列：
test %>% 
  separate(.,"ad_date",c("y","m",'d'),sep= "-", remove = FALSE) %>%

## 1.2 一列转一列，列内数据按分组及字符分割展开：
data %>% separate_rows(.,diag,sep=",") 

## 1.3 列数据横向展开，独热编码：
data %>% separate_rows(.,diag,sep=",") %>% 
  mutate(x =1) %>% 
  spread(diag,x) %>% 

## 2 数据合并：
## 2.1 对应1.1 一列转多列：合并多列到一列中：
df = data.frame(n, s, b) %>% 
  unite(x, c(n, s), sep = " ", remove = FALSE)
df = data.frame(n, s, b) %>% 
 mutate(x = paste0(s,b,collapse= ""))

## 2.2 对应1.2 一列数据按分组展开：
df <- tibble(
  x = 1:3,y = c("a", "d_e_f", "g_h"),
  z = c("1", "2", "5"))
separate_rows(df,y, z,convert=TRUE,sep="_") %>%
  group_by(x,z) %>%  
  ## 注意这里不能使用mutate来构建，mutate并不会汇总数据；
  summarize(y= str_c(y,collapse="_")) %>% ungroup()
```

##### 字符串分列与去重（条件匹配）：

```R
## 多分类数据的合并汇总实现方法：
data <- data.frame(
  patient_id = c(rep(1:4,each=3)),
  visit_id = paste0("v",c(1:12)),
  lab_name = rep("血肌酐",12),
  result_num = c(1000:1006,10:14),
  result_unit = c(rep("umol/l",7),rep("mg/dl",5)),
  sex = c(rep(c("male","men"),time = 6)),
  age = c(sample(18:40,size = 12,replace = T)),
  diag =  c(rep(paste("糖尿病","高血压","心脏病",sep =","),time = 4),
            rep(paste("糖尿病","心脏病",sep =","),time = 4),
            rep(paste("高血压",sep =","),time = 4))
)

data <- data %>% mutate(caldiag_1 = case_when(
      grepl("糖尿病|心脏病", diag) ~ "大病",
      TRUE ~ "0"
    )) %>% 
  mutate(caldiag_2 = case_when(
    grepl("高血压", diag) ~ "小病",
    TRUE ~ "0"
  )) 

## 改进方法2：
data %>% 
  separate_rows(.,diag,sep=",") %>% 
  mutate(caldiag = case_when(
          grepl("糖尿病|心脏病", diag) ~ "大病",
          diag == "高血压" ~ "小病",
          TRUE ~ "缺失"
        )) %>% select(!diag) %>% 
  group_by(visit_id) %>% 
  summarize(y= str_c(unique(caldiag),collapse=",")) %>% 
  ungroup() %>% right_join(data,by="visit_id")  %>% View()

## 改进方法3：
## 字符串去重：只适合全部变量均可匹配：
## 这种方法不好的原因在于全局匹配，置换后的数据行还会包含未被分类的数据集：
# 类似这样："大病,终末期肾脏疾病"  
data %>% 
  mutate(
    cla = str_replace_all(.$diag,c("糖尿病|心脏病" = "大病", "高血压" = "小病")) %>% 
      map(., function(x) paste(unique(unlist(str_split(x,","))), collapse = ",")) %>% 
      unlist() )
```

##### 批量重命名（列名）

```
## 单个变量重命名：
iris %>% rename("1" = "Sepal.Length","2" ="Sepal.Width" ) %>% names()

## 批量构造虚拟数据集进行填充后重新批量命名：
out1_df =  matrix("NA",ncol = length(out1),nrow =  dim(lab_fp)[1]) %>% data.frame() %>% rename_at(vars(names(.)) ,~ out1)    

## 数据列重命名：
rename(flights,YEAR=year)

## 构造列名：
setNames( 1:3, c("foo", "bar", "baz") )
# this is just a short form of
tmp <- 1:3
names(tmp) <-  c("foo", "bar", "baz")
```



### 数据值重编码（非列名）
#### 二分类重编码

```R
## 方法1，构建函数：
f3 <- function(x) ifelse(x >= 0, 1, 0)

## 方法2：使用plyr包中的revalue() 或 mapvalues()
library(plyr)
data$scode <- revalue(data$sex, c(M = "1", F = "2"))
data$scode <- mapvalues(data$sex, from = c("M", "F"), to = c("1", 
  "2"))
str <- c("alpha", "beta", "gamma")
mapvalues(str, from = c("beta", "gamma"), to = c("two",  "three"))
 
## 方法3：base-R直接赋值：
data$scode[data$sex == "M"] <- "1"
data$category[data$control < 7] <- "low"
str[str == "beta"] <- "two"

## 方法4： 使用正则来进行选择匹配：
str <- c("alpha", "beta", "gamma")
sub("^alpha$", "one", str)
# 把所有列的 'a' 替代为 'X'
gsub("a", "X", str)
#> [1] "XlphX" "betX"  "gXmmX"

## 方法5使用if-else()
# 注意这种mutate指定的方法比赋值参数更好用；
df <- df %>%
        mutate(status= if_else(.$points > 20, 'Good', 'Bad'))
```

#### 多分类重编码

```R
## 方法1：使用age_categories()进行数据的多分类(连续变量)           
linelist <- linelist %>% 
  mutate(age_cat = cut(
    age_years,
    breaks = c(0, 5, 10, 15, 20, 30, 50, 70, 100),          
    right = FALSE,
    include.lowest = TRUE,        
    labels = c("0-4", "5-9", "10-14", "15-19", "20-29", "30-49", "50-69", "70-100")),
    age_cat = fct_explicit_na(
      age_cat,
      na_level = "Missing age"))    
 
## 方法2：使用cut函数进行数据多分组分类：
age = c(23, 15,36,47,65,53)
cut(age,breaks =c(0,18,45,100),labels = c("y","m","o"))


### 方法3使用dplyr::ntile()来实现数据的均等分类：
# make groups with ntile()
ntile_data <- linelist %>% 
  mutate(even_groups = ntile(age_years, 10))

### 方法4：使用sjmisc::rec()函数来匹配数据后，生成对应编码新值；
library(sjmisc)
df %>% rec(math,rec ="min:59= 不及格"；60:74 =中；75:85= 良；85：max= "优"，	append = FALSE)

##  方法5：case_when()：多条件编码；
df %>% mutate(sex = if_else(sex ="男","M","F")) # 等于男则为M，否则为F；
df %>% mutate(math =case_when(math >= 75~"high",
							math >= 60~"middle",
							TRUE ~ "LOW"))  ## 最后一个TRUE表示所有剩余分支； 



```

#### 批量分类case_when/fcase：

```R
## 批量分类方法1：
linelist <- linelist %>%  
  mutate(across(           ## 下面这个all_of()的用法很棒！               
    .cols = all_of(c(explanatory_vars, "outcome")), 
    .fns = ~case_when(                              
      . %in% c("m", "yes", "Death")   ~ 1,  ## 将其分类转为1和0；         
      . %in% c("f", "no",  "Recover") ~ 0,          
      TRUE                            ~ NA_real_)  
    )
  )

## case_when是一种极其占有内存和浪费时间的方法：
## 优先使用data.table()中的fcase来实现条件替换,效率提高10倍：
## 与case_when()体系不同的是：
同义替换间使用逗号，而不是~；
最后的默认参数，使用default，而不是TRUE;

linelist <- linelist %>%  
  mutate(across(           ## 下面这个all_of()的用法很棒！               
    .cols = all_of(c(explanatory_vars, "outcome")), 
    .fns = ~fcase(                              
      . %in% c("m", "yes", "Death")   ,1,  ## 将其分类转为1和0；      
      . %in% c("f", "no",  "Recover") , 0,   
        ## 注意：default必须是一个固定的参数值，不能像case_when那样实现非过滤等价；
      default =  NA_real_)  
    ))


```

#### 数据类型重编码

```R
# transform函数
 mtcars %>%
  transform(cyl = factor(cyl),
            hp = factor(hp)) %>%  as_tibble()
## 指定数据形式的转换：
transform(BOD,as.numeric(Time))
 
# mutate函数
dta %>%
  mutate(cyl = factor(cyl),  hp = factor(hp)) %>%
  as_tibble() 

## 较多变量：使用.cross()
dta %>%
  mutate(across(where(is.integer), factor)) %>%  is.interger()  %>% as_tibble()    # 判断数字为整形；
  
## 但在实际项目中R的行列参数类型往往为double()类型，因此单纯使用上面两种类型，往往找不到对应的类型变量；
# 因此可以使用整除的方法进行筛选变量：
dta %>%
  mutate(across(function(col) all(col %% 1==0), factor)) %>%  as_tibble()  ## 即整数除以1后的余数为0
 
## 补充一种时间序列的处理方法：
mutate(across(.cols = where(is.POSIXct), .fns = as.Date))
            
## 自定义高效函数重新编码：                
tr2 = function(data,aim_col,funss){
  ## 将变量解析为字符串：
  aim_cha = deparse(substitute(funss))
  ## 使用bangbang来运行解析式：
  if(!!aim_cha == "num"){
    ## 使用{{}}来纳入变量参数：
    return(
      data %>% mutate(across(.cols = {{aim_col}}, .fns = as.numeric)))
  }else if(!!aim_cha == "dat"){
    return(
      data %>% mutate(across(.cols = {{aim_col}}, .fns = as_date)))
  }else if(!!aim_cha == "cha"){
    return(
      data %>% mutate(across(.cols = {{aim_col}}, .fns = as.character)))
  }else if(!!aim_cha == "fac"){
    return(
      data %>% mutate(across(.cols = {{aim_col}}, .fns = as.factor))) 
  }else{"input function fasle"}}

iris %>% tr2(.,Species,fac)
## 对于多参数模型，还不能很好的纳入across中.col这种方法；
iris %>% tr(.,c("Sepal.Length","Sepal.Width"),num)            
```

#### 因子编码调整

##### 基于forcats包的分类

```R
# base R
mutate(pbg_cat = factor(
    pbg_cat,
    levels = c("[0, 7.8)", "[7.8, 10.0)", "[10.0, 11.1)", 
               "[11.1, 12.0)", "[12.0, +)")))
# 调整分组排序：
mutate(delay_cat = fct_relevel(delay_cat, "<2 days", "2-5 days", ">5 days"))
# 添加额外分组标化：
mutate(delay_cat = fct_expand(delay_cat, "Not admitted to hospital", "Transfer to other jurisdiction"))
# 删除分组：
mutate(delay_cat = fct_drop(delay_cat))  ##
# 分组重编码：
mutate(delay_cat = fct_recode(
    delay_cat,
    "Less than 2 days" = "<2 days",
    "2 to 5 days"      = "2-5 days",
    "More than 5 days" = ">5 days"))

## fct_recode重编码的另外一种形式：
meldata <- meldata %>% 
  mutate(sex.factor =             # Make new variable  
           sex %>%                # from existing variable
           factor() %>%           # convert to factor
           fct_recode(            # forcats function
             "Female" = "0",      # new on left, old on right
             "Male"   = "1") %>% 
           ff_label("Sex"),       # Optional label for finalfit
         
         # same thing but more condensed code:
         ulcer.factor = factor(ulcer) %>% 
           fct_recode("Present" = "1",
                      "Absent"  = "0") %>% 
           ff_label("Ulcerated tumour"),
         
         status.factor = factor(status) %>% 
           fct_recode("Died melanoma"       = "1",
                      "Alive"               = "2",
                      "Died - other causes" = "3") %>% 
           ff_label("Status"))


## 合并分组后重编码：
# 新生成变量命名为new，列内将原来Species中的setosa和virginica修改命名为newss；暂时不知道其底层逻辑是多少，但可能是一种便捷的处理方法：
iris %>% mutate(new = 
                    fct_collapse(Species,
                                 newss = c("setosa","virginica")) ) %>% View()
```

##### 批量标签分类调整：

```R
## 批量分类方法2：
expss::apply_labels(fbg_diff = "空腹血糖变化差值",
               fbg_reg = "末次空腹血糖<6.1m/mol",
               age = "年龄",
               sex = "性别" )

## 批量分类方法3：
visit_clean <- visit %>%
  mutate( insurance_type = recode(insurance_type,
                                  is.na = "医保类型缺失")) 

## 批量修改重分类方法4：
tmp$group <- plyr::mapvalues(tmp$group, from = c("GFP- early","GFP+ early","GFP- late","GFP+ late"),
                                      to = c("HhOFF early", "HhON early", "HhOFF late", "HhON late"))
 
tmp$group <- factor(tmp$group, levels = c("HhOFF early", "HhON early", "HhOFF late", "HhON late"))

```

##### 高效标签编码`datapasta` 

```R
## datapasta 提供了一种范式，可以将变量名按照固定格式输出到代码块中，方便粘贴使用：
install.packages("datapasta")
library(datapasta )
iris %>% 
  distinct(Species) %>% 
  arrange(Species) %>% 
  pull() %>% 
  datapasta::vector_paste_vertical()

## 纵向输出，同时将代码结果掩盖：
c("setosa",
  "versicolor",
  "virginica")

## 横向输出，同时将代码结果掩盖：
iris %>% 
  distinct(Species) %>% 
  arrange(Species) %>% 
  pull() %>% 
  datapasta::vector_paste()
c("setosa", "versicolor", "virginica")

## 借助上面的函数实现高效的case_when之类的数据替换：
## 数据替换后：可以借助Rstudi0的快捷键使用批量多项选择：
Mac 上的 Opt 键 + 鼠标左键拖动
Windows 上的 Alt 键 + 鼠标左键拖动
# 这个过程简单来说，就是按住alt+左键之后，批量选择纵向输出列，然后对这些列进行批量代码替换和处理。
```

#### 构造哑变量

```R
library(caret)
library(ISLR)
dummies <- dummyVars(~League+Division+NewLeague, data = Hitters)
dummies <- predict(dummies, newdata = Hitters)
head(dummies)
```





### 数据塑形
#### 数据排序

```R
> # 第一列升序，然后是第三列降序
> r2 = iris[order(iris[,1],-iris[3]),]
> # 第一列升序，然后是第三列升序序
> r2 = iris[order(iris[,1],iris[3]),]
## 或者使用dplyr::arrange()
library(dplyr)
data("iris")
head(iris)
# 第一列升序，然后是第三列升序
arrange(iris,iris[,1],iris[,3])
# 第一列升序，然后是第三列降序
arrange(iris,iris[,1],-iris[,3])
# # 如果列名不止一个，会将后面的列在前面列的基础上进行排序
arrange(flights,year,month,day)

# min_rank()，从小到大排名；需逆向排序的话，加一个desc()
df %>% mutate(rank =min_rank(desc(math)))  %>%
	arrange(ranks)

### desc 逆排序：
df_dup() %>% arrange(math,sex,decreasing=TRUE) ## 反向排序

## 数据列位置的位移
# select()与everything()连用可以将某几列移到数据库的开头
select(flights,day,dep_time,everything())
```



#### 行列互转

```R
## 宽表格变长表格：gather(),melt(),pivot_longer()
## 长表格变宽表格：spread(),cast(),pivot_wider()

####################### 宽表变长表： ####################
## 宽矩阵转为长矩阵：
reshape2::melt()

## 宽表变长表：
df_q %>% pivot_wider(names_from =q,values_from =math_qs,names_prefix ="q_")


###################### 长表变宽表 ##########################
###  长表变宽表；
 pivot_longer(cols = starts_with("malaria_"),
    names_to = "age_group",values_to = "counts")
## cols:用选择列语法选择要变形的列；
# names_to:为存放变形列的列名中的值，指定新列名；
# values_to:为存放变形列中的值，指定新列名；

## 比较gather和spread的用法：均是tidyr包的函数；
library(tidyr)
data2 <- gather(data, category, value, X, Y, Z)
# 上面代码是指将data中x、y、z列值转为以分类数据行为基础的数据长矩阵，其中x、y、z原有的值转为列值；
spread的用法与gather相反，是将长矩阵转为宽矩阵：
gather(data, category, value, -time)  ## 这里的-time是指被忽略的那一列；


## 参见案例如下：
https://www.cnblogs.com/jialinliu/p/15228201.html
```



#### 列表转数据框
###### 列表常用处理

```R
# 代码细节：
as.list() # 将数据进行分为若干个列表； 
is.list() # 将数据收纳为一个列表；

## 删除列表的子表：
y <- list(a = 1, b = 2)
y["b"] <- list(NULL)
str(y)
  
## 列表的赋值定义方法：
x <- c("m", "f", "u", "f", "f", "m", "m")
lookup <- c(m = "Male", f = "Female", u = NA)
lookup[x]
#3 删除对应的属性：
unname(lookup[x])

## 列表调整内部子集的顺序：
## 前提是设计
Freedom <- c(1, 2, 3, 2, 1, 2)
Equality <- c(2, 3, 1, 1, 2, 1)
TypeCountry <- c("South", "East", "East", "North", "South", "West")

Example <- list(Freedom, Equality, TypeCountry)
names(Example) <- c("Freedom", "Equality", "TypeCountry")
Ex <- Example[c("TypeCountry","Freedom", "Equality")]
```

###### 列表与purrr

```r
# list_modify()有很多用途，其中之一可以是删除列表元素
# keep()保留指定给 的元素.p =，或者提供给 的函数的.p =计算结果为 TRUE
# discard()删除指定给 的元素.p，或者提供给的函数.p =计算结果为 TRUE的元素
# compact()删除所有空元素

## 举例：
combined %>% 
  list_modify("Central Hospital" = NULL)   # remove list element by name

# keep only list elements with more than 500 rows
combined %>% 
  keep(.p = ~nrow(.x) > 500)  

# Discard list elements that are not data frames
## 丢弃列表中非数据框的类型数据；
combined %>% 
  discard(.p = ~class(.x) != "data.frame")

# keep only list elements where ct_blood column mean is over 25
# 深入列表进行计算和筛选：
combined %>% 
  discard(.p = ~mean(.x$ct_blood) > 25)  

```



#### 日期数据重塑

```
查看本地环境下的时区设计：
## 查看时区：
Sys.timezone(location = TRUE)
```

##### 3.4.1 lubridate

```R
install.packages("lubridate")
library(lubridate)
today()  ## 查看年月日；
now()    ## 最详细的日期；

## 时间修正形式：
ymd("20110604")
mdy("06-04-2011")
dmy("04/06/2011")
lubridate::as_datetime("2012.04/1612:00:05")
lubridate::as_datetime(44677.4375*24*60*60, origin = as.POSIXct("1899-12-30 00:00:00",tz = "UTC"))
#[1] "2022-04-26 10:30:00 UTC"


## 提取信息：
## 使用下面这些函数直接提取对应数据中参数值：
# minute、hour、day、wday、yday、week、month、year
arrive <- ymd_hms("2011-06-04 12:00:00", tz = "Pacific/Auckland")
second(arrive) <- 25

## 修改时间：
ymd(20110101) + dyears(1)
#> [1] "2012-01-01 06:00:00 UTC"
ymd(20110101) + years(1)
#> [1] "2012-01-01"
```

##### 3.4.2 base-r-date

```r
## 规整日期；
as.Date("2012/12/14")
[1] "2012-12-14"
as.Date(index_date, format='%Y-%m-%d'),
as.Date(admission_dt, format='%Y-%m-%d'),units='days')))))  

# 组建时间：
make_data(2020,8,27 ) # 输出"2020-08-27"
make_datatime(2020,8,27,21,27,15)  2020-08-27 21:27:15 UTc

# 还可以使用format来构建函数：
d = make_data(2020,3,5)
format(d ,"%Y/%m/%d")

## 时间序列分析：
ts(data, start =1, end, frequency =1, ...)函数：
其中data为数值向量或矩阵；start设置起始时刻；end设置结束时刻；
frequency设置时间频率，默认为1；当填入4时表示季度；填入12表示月份；
例如：
ts(data =1:10,start =2010,frequency =4)
```

##### 3.4.3 计算时间差：

```R
# 提取时间差：
> a <- '2016-06-02 23:29:00'
> b <- '2016-06-05 03:24:00'
> as.double(difftime(b,a))
[1] 2.163194
> as.double(difftime(b,a,units="weeks"))
[1] 0.3090278
#还可以指定units
> as.double(difftime(b,a,units="weeks"), units = 'days')
[1] 2.163194

## 计算时间差的其他方法：
inpatient_time = as.integer(discharge_datetime - admission_datetime)

## 计算时间差：
as.period(interval(birthDate, refDate), unit = 'year')$year

## 计算时间差：
as.numeric(time_length(interval(index_date, 
     as.Date("2021-12-31")), 'month'))


```



### 数据提取
#### 字符串提取

```R
## 查看字符的正则类型：
## 查看字符的正则类型：
## 正则表达式：
x <- c("New theme", "Old times", "In the present theme")
str_view(x, "the")
```

##### 字符串统计与转换：

```R
## R语言字符串处理：
################# 统计字符串长度： ########################
library(stringr)
x <- c('abcd', 1379, '偷闲阁', NA)
nchar(x)
nchar(x, type = 'bytes')
nchar(x, keepNA = F)
nzchar(x)
str_length(c("a", "bc", "def", "北京"))


```

##### 字符串连接、构建、删除：

```R
###################### 字符串 连接  ########################
## base -R - 连接；
paste() 函数；
rep()

## 字符连接：
library(stringr)
str_c(c("x", "y"), c("a", "b"), sep="*")
str_c("data", 1:3, ".txt") ## 类似与paste0
str_c("x",1:3,sep = "",collaspse = "_")  # 输出结果为："x1_x2_x3"

## 字符连接：
paste0("hh","123")
output_file = stringr::str_glue("outputs/Report_{Sys.Date()}.docx")


########################## 字符构建： ########################
str_dup(c("a","b"),c(3,2)) # 输出为  [1] "aaa" "bb"
## 使用正则匹配来实现字符串构建：interesting！
sprintf("tour%03d.jpg", c(1, 5, 10, 15, 100))
## 字符串中插入变量值：
name <- "李明"
tele <- "13512345678"
str_glue("姓名: {name}\n电话号码: {tele}\n")
str_glue_data(list(name = "王五", tele = "13500000000"),
              "姓名: {name}", "电话号码: {tele}", .sep="; ")

######################## 字符串删除 ##########################
## 去掉空白字符串：
tt[nchar(x)>0]

## 移除字符集中的空格：
str_trim(c("a  ","b  ", "a b"))
# [1] "a" "b" "a b"
## 选项which="left"可以仅删去开头的空格， 选项which="right"可以仅删去结尾的空格。
trimws(c("  李明", "李明  ", "  李明  ", "李  明"), which="left")

```

##### 字符位置排序与检索

```R
##################### 字符排序 ################
str_sort(x ,decreasing,locale , ....) # locale可指定语言，默认为en，可变为"ch"。并且这种排序方法会改变字符串的序列，按照汉语拼音的方法；
x= c("banana","apple","pear")
str_sort(x)
# [1]  "apple", "banana",  "pear"


##################### 字符位置查询 ################# 
### 主要有grepl()和grep()函数：
x <- c('describe', 'the', 'city', 'you', 'live', 'in')
## 字符检索grep--返回下标数字：
grep('i', x) ## 检索下标 [1] 1 3 5 6
grep('i', x, invert = T) # 返回不匹配的选项；
grep('i', x, value = T) # 返回检索值；

## 字符检索grepl-- 返回逻辑值：
grepl('i', x) -- true/false 
startsWith(c("xyz123", "tu004"), "tu") # 条件判断并范湖逻辑值；

## 字符检索regexpr- regexec：
## 字符返回位置：结果包含了匹配的具体位置和字符串长度信息
text <- c("Hellow,Adam!", "Hi, Adam!") 
regexpr("Adam", text)  ## 返回具体位置
gregexpr("Adam", text) # 下面两个返回的结果是相似；
regexec("Adam", text) 

## 基于string包中的str_（）相关匹配函数：
str_detect(string, pattern, negaate =FALSE) 检测是否存在匹配；
str_which(string,pattern,negate = FALSE) _查找匹配的索引；
str_count(string, pattern) ：计算匹配的次数
str_locate(string, pattern) ：定位匹配的位置，返回end和start；
str starts( string, pattern) : 检测是否以pattern开头
str_ends(string, pattern) : 检测是否以pattern结尾
	string:为要检测的字符串
	pattern:为匹配的模式,可以是正则表达式
	negate:默认为 FALSE表示正常匹配,若为TRUE则反匹配(找不匹配)。

```

##### 字符串转换/替换：

```R
######################## 字符串 转换 ###############################
## 大小写转换：
tolower()
toupper()

# 批量将字符串转为数值型数据：
xh2 <- as.data.frame(lapply(xh[,1:2],as.numeric)) %>% data.frame(.,xh[,3])

## 批量构建循环体中的字符串： 
explanatory_vars %>% str_c("outcome ~ ", .)

## 字符串格式化输出：将字符串转为变量名：
str_splot_fixed(string,pattern ,n) # 返回矩阵，n控制行数；

######################## 字符串 替换 ##############################
### 字符筛选后替换：chartr
DNA <- "AtGCtttACC"
chartr("Tt","Bb",DNA) ## 转为小写
chartr("Tt","BB",DNA) ## 转为大写
chartr("!;", ".,", c("Hi; boy!", "How do you do!")) ## 多条件替换；

## 字符查询位置后替换：base-sub、gsub
## sub 只做一次检索替换：
text<-c("Hello, Adam","Hi,Adam!Adam!","How are you,Ava")
sub(pattern="Adam",replacement="word",text) ##在该输出结果中"Hi,word!Adam!"

## gsub 遇到即发生替换
gsub(pattern="Adam|Ava",replacement="word",text)
[1] "Hello, word"      "Hi,word!"         "How are you,word"

## 字符查询位置后替换：string-str_sub:
s <- "term2017" 
str_sub(s, 5, 8) <- "18" ## 返回"term18"

## substr替换
##　substr(x,from,to) 
hh <- c("ahbdhfdf")
substr(hh,2,4) <- ' jjj'

## 字符串提取后替换：
str_replace(string,pattern,replacement)
str_replace(x,"-","/") # 将-转为/
```

##### 字符串拆分/提取：

```R
########################  拆分 ###################
### base-R:
strsplit(x, split, fixed = FALSE, perl = FALSE, useBytes = FALSE)
## 注意：
1、split为特殊字符时，使用“[$]”来进行分割；
2、strsplit分割后，split作为分割字符串不再保留；

### library(stringr)
str_split(string, pattern, n = Inf, simplify = FALSE) ## 返回列表，与strsplit类似；
str_splot_fixed(string,pattern ,n) # 返回矩阵，n控制行数；

## 拆分数字并提取：
drug_spec_unit = as.numeric(readr::parse_number(drug_spec_unit))

######################## 提取 #######################
## 提取string - substr/str_sub
substr/(x, start, stop)  ## 给定范围提取：
substring(text, first, last = 1000000L) 
> x <- "123456789" 
str_sub(x,5,8)  ## 5678
> substr(x, c(2,4), c(4,5,8)) 
[1] "234" 
> substring(x, c(2,4), c(4,5,8)) 
[1] "234"     "45"      "2345678" 

## 基于str_split()
str_split(string,pattern) ## 返回列表；
str_splot_fixed(string,pattern ,n) # 返回矩阵，n控制行数；

## 基于正则提取：
# 提取所有中文字符：
words_vec <- str_extract_all(poem_autumnwindow, "[[:alpha:]]")[[1]]
head(words_vec)

# 提取数字：
s <- c("10-0.16-1700.0-42.csv", "12-0.22-1799.1.csv")
pat <- "[0-9]+[.][0-9]+|[0-9]+"
s1 <- str_match_all(s, pat); s1

## 正则匹配提取：str_extract()和str_match()
str_extract(string,pattern) :只提取匹配的内容；
str_match(string,pattern) ：提取匹配的内容以及各个分组捕获，返回结果矩阵；
c("1978-2000","2011-2020-2099")
pat<-"\b(19|20)([e-9]{2})\b" #正则表达式
str_extract(x, pat)
##[1]"1978""2011"
str_match(x, pat)
	[,1] [,2] [,3]
##[1,]"1978""19""78"
##[2,]"2011""26""11

## 利用正则仅提取数据：
str_match_all( "[0-9]+[.][0-9]+|[0-9]+")
```

#### 列批量提取

```R
## 快速数据框处理：
library(dplyr)
## 使用select函数筛选：
############################### select选择特定的列；
select(flights,year:day)
# 
select()函数可以搭配使用一些辅助函数：
starts_with(“abc”)：匹配以abc开头的名称
end_with(abc)：匹配以abc结尾的名称
contains(“xyz”)：匹配含xyz的名称
matches(“”)
num_range(“x”，1：3)：匹配x1，x2，x3
# .col用于指定列，.fns用于对指定列进行函数运算；
across(.col =everything(),.fns =NULL,...,.names)
df %>%  ## 选定所有列，进行归一化；
mutate(across(where(is.numeric),rescale)) 



# select()与everything()连用可以将某几列移到数据库的开头
select(flights,day,dep_time,everything())

## select()函数的其他用法：
select(name, sex>0.6 & math< 0.9)
```

#### 条件筛选提取
##### 使用subset、filter、mutate过滤：

```r
## base R :
tt = tt[,-1]
tt2 = subset(data_set, select = -c(val1))


### filter() 根据值或条件筛选行：
# filter() ::按数据需求，行或者列进行筛选；
# 如果想要过滤得到11月与12月出发的所有航班
filter(flights,month == 11 | month == 12)  ## 注意这里的 “|”，表示并且的意思；

df_dup %>% 
	filter(sex == "男"，math >80)
# 多条件筛选；
df_dup %>% filter(sex == '女'，is.na(english) | math>80))
# 闭区间筛选：
df_dup %>% filter(between(math,70,80))
# 指定列范围内根据条件筛选：
df[,4:6] %>% filter(across(everything()),~ .x>75)
df[,4:6] %>% filter(if_any(where(is.numeric)),~ .x>75) # 上述两公式等价；
df[,4:6] %>% filter(across(everything()),~ !is.na(.x))
df[,4:6] %>% filter(if_any(where(is。character)),is.na)
(# 根据包含指定值筛选行)
df[,4:6] %>% filter(across(everything()),~str_detect(.x,"bl")))

## 按行号过滤数据：
linelist %>% filter(row_number() == 5)

## 除了使用filter进行过滤外，还可以使用mutate()函数进行过滤：
## 例如：
iris %>% mutate(test = fcase(iris$Sepal.Length>5,1, default = 2),
                iris$Species == 'virginica'  )
```

##### 取交并补

```R
## 集合运算(或者说向量比较运算)
dplyr::intersect(x, y) ## 返回x和y共同包含的观测
dplyr::union(x,y)   ## 返回x和y中所有的唯一观测；
dplyr::setdiff(x,y)  ## 返回在x中但不在y中的观测
```

##### 嵌套构建与提取

####### 嵌套构建

```R
library(tidyr)
df <- tibble( ## 构建一种嵌套数据集：用metadata来嵌套数据内部：
  character = c("Toothless", "Dory"),
  metadata = list(
    list(
      species = "dragon",
      color = "black",
      films = c(
        "How to Train Your Dragon",
        "How to Train Your Dragon 2",
        "How to Train Your Dragon: The Hidden World"
      )
    ),
    list(
      species = "blue tang",
      color = "blue",
      films = c("Finding Nemo", "Finding Dory")
    )
  )
)
## 使用hoist来提取数据中参数，并指定或创建新的列名；
# 其中有趣的点是1L或3L这种形式，表示的是从list表中提取对应参数位置；
df %>% hoist(metadata,
             "species",
             first_film = list("films", 1L),
             third_film = list("films", 3L)
) %>% View()

character species   first_film      third_film metadata    
  <chr>     <chr>     <chr>           <chr>      <list>      
1 Toothless dragon    How to Train Y~ How to Tr~ <named list>
2 Dory      blue tang Finding Nemo    NA         <named list>
```

###### 嵌套提取

```R
## 第一种方法：使用map_depth实现指定提取深度；
testlist2 %>% map_depth(-2, 'veggie')

## 使用`[`和`[[`做深层提取：可以通过?`[`查询；
> test <- list(a=1:10,b=letters[1:10])
> test
$a
 [1]  1  2  3  4  5  6  7  8  9 10
$b
 [1] "a" "b" "c" "d" "e" "f" "g" "h" "i" "j"

> "[["(test,1) #> [1]  1  2  3  4  5  6  7  8  9 10
> "[["(test,2) #> [1] "a" "b" "c" "d" "e" "f" "g" "h" "i" "j"
> "["(test,1)  #> $a [1]  1  2  3  4  5  6  7  8  9 10
> "["(test,2)  #> $b [1] "a" "b" "c" "d" "e" "f" "g" "h" "i" "j"

## `[`和`[[`与lapply结合来提取深层列表元素：
lapply(x, '[[', VarNames[[type]])
```



##### 行筛选

```R
## 行筛选：
data.frame() %>% filter(!row_number() == 1) 

## 上一行和下一行的值进行条件筛选：
## lag是访问当前行之前的行：lead是访问当前行的下一行：
data %>%  mutate(trend = ifelse(newdosage > lead(newdosage),'lower','stable'))  %>% 
 replace_na(list(trend = "stable")) %>% ungroup() %>% distinct()

## 批量行值筛选：
# 在这里是获取每个患者每个变量值的最大值；
data  %>% group_by( patient_id) %>% mutate(across(.col=c("xjgs","xjgs_one","xjgs_more","cz","cz_one","cz_more","gzdm","bwdxjt","xlsj","qxxxzb"), max,.names = "{.col}")) 
                         
```

