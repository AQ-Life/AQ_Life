---
categories:
- SAS
- SAS Advance
- SAS SQL
date: '2021-11-10'
description: ""
tags:
- SAS
- SQL
title: SAS SQL（一）：语法顺序与执行顺序
---


SAS 语言吸纳了很多其他编程语言的优势，比如 SQL procedure，在 SAS 当中也可以使用 SQL 进行增删查改。SQL procedure 一般的语法结构如下。
```SAS
PROC SQL;
    SELECT *
    FROM table1
    WHERE expression
    GROUP BY column1
    HAVING expression
    ORDER BY column1;
QUIT;
```
为了方便记住每个从句关键字的顺序，人们打趣的说到：" **S**o **f**ew **w**orkers **g**o **h**ome **o**n time."

SQL procedure 的语法顺序固定的，如上code，从上至下。而执行顺序却和语法顺序不同。
**FROM -&zwnj;-> WHERE --> GROUP BY -&zwnj;-> HAVING -&zwnj;-> SELECT -&zwnj;-> ORDER BY**
理解了执行顺序也就更容易理解 SQL 的逻辑。