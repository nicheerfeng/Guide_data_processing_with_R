# 模型评估 {#Model-performance}

## 模型评估辅助：

### purrr包的pluck()函数字符提取

```R
## 使用purrr包的pluck()函数帮助快速提取统计结果：
## 与传统代码提取结果相比，传统提取往往需要在统计结果中使用列表查询或者子查询；
## 而使用purrr::pluck() 可以指定查询的字符串（实现多级查询），然后输出结果，提高索引效率。

t.test_results %>% 
  pluck("age")        # alternatively, use pluck(1)

t.test_results %>% 
  pluck("age", "p.value") # 多级索引，age列表下的p.value()

## 使用map()函数来辅助批量提取：
t.test_results %>%
  map(pluck, "p.value")   # return every p-value

### 更简单的辅助提取方法，是直接使用purrr族的相关函数：
t.test_results %>% 
  {tibble(
    variables = names(.),
    p = map_dbl(., "p.value"), ## 不用经过pluck()函数，可以直接提取对应统计值；
    means = map(., "estimate"))}
```
## 常用模型辅助评估R包：

performance：💪计算、分析和测试统计模型的性能。

parameters：📊提取几乎所有统计模型参数的综合数据框，并提供帮助以优雅的表格和图表呈现它们。
