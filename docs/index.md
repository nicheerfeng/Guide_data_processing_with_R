--- 
title: "基于 R 的数据处理流程指南"
author: "Teague Tian"
date: "2022-09-29"
documentclass: ctexbook
bibliography: [book.bib, packages.bib]
biblio-style: apalike
link-citations: yes
colorlinks: yes
lot: yes
lof: yes
geometry: [b5paper, tmargin=2.5cm, bmargin=2.5cm, lmargin=3.5cm, rmargin=2.5cm]
site: bookdown::bookdown_site
description: "R快速查询工具指南" 
github-repo: nicheerfeng/git_book_ftidy
favicon: "image/favicon.ico"
---



# 前言 {-}

<img src="image/cover.png" class="cover" width="500" height="400"//>



这是一本笔者在日常工作和学习中进行工具整合的R在线工具书。它旨在为数据统计人员提供快速查询和多方法比较的学习建议。

如果读者已经掌握了一些R的编程知识，可以翻看目录挑选感兴趣的内容阅读；

本书尽可能按照数据分析的构筑流程来搭建系统的框架，以便未来此书籍能够持续纳入和迭代更新。
与此同时，数据也尽可能的纳入更多的R社区新工具来帮助减少重复，以及增强代码的可读性。在编写这本书时，还尽可能的将笔者在探索学习R相关工具（Shiny、R pacakge构建、函数式编程）等方面的经验也纳入其中，希望这有助于读者快速入门。

本书的章节概要如下：

- 第 1 章 如何构建数据分析的目标和构建方法
- 第 2 章 介绍数据提取的方法和导入导出等相关R操作
- 第 3 章 介绍数据抽取、转换、探索和函数构建等操作
- 第 4 章 介绍描述模型、复杂模型、临床统计模型等操作
- 第 5 章 介绍模型评估方法
- 第 6 章 介绍模型Shiny应用、rmarkdown和Quarto语法
- 第 7 章 介绍R的可视化图和表
- 第 8 章 介绍常用R系统命令、快捷键、正则等
- 第 9 章 介绍临床医学统计的系统流程
- 第 10 章 介绍R统计相关实现方法书籍
- 第 11 章 介绍统计与研究设计
- 第 12 章 介绍构筑R package的方法

这是我第一次编写比较系统的R语言教程，难免存在错误和不当，恳请读者批评指正。另外，有部分R代码设计库可能存在直接引用的情况，如有侵权，请邮件联系。目前我在业余时间对本书内容进行积极的开发，如果读者有任何建议，欢迎到 GitHub 仓库 Issue 中进行讨论（）。


**致谢**

感谢互联网各个渠道的知识博主提供的案例参考。站在巨人的肩膀上，simple code creat world！

\BeginKnitrBlock{flushright}<p class="flushright">By Teague Tian 
2022/09/28</p>\EndKnitrBlock{flushright}

