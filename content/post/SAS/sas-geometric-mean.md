---
categories:
- SAS
- SAS Advance
date: '2023-03-21'
description: ""
katex: true
tags:
- SAS 
- Proc Step
title: SAS计算几何平均数（Geometric Mean）
---


## 几何均值
在一组数据满足对数正态分布时，我们需要用到几何平均数来评估这组数据的“中心”，计算方法为这组数据的连乘积开项数次方根，计算公式如下。


$$G_n = \sqrt[n]{\prod_{i=1}^n x_i} = \sqrt[n]{x_1 x_2 x_3 \cdots x_n}$$

我们这里介绍几种方法供大家参考。

### 几何均值的计算方法（一）
```SAS
*** 1. 对数据列（aval）求对数 ***
    logaval = log(aval);
*** 2. proc means 计算logaval的均值 ***
proc means data=adis n mean lclm uclm noprint;
	by trtan avisitn;
	var logaval;
	output out=gmt1 n=n mean=mean lclm=lclm uclm=uclm;
run;
*** 3. 10的次幂取回， 10**mean ***
```
### 几何均值的计算方法（二）
```SAS
*** 1. 对数据列（aval）求对数 ***
logaval = log(aval);
*** 2. 采用proc sql衍生几何均值***
proc sql noprint;
	create table gmt2 as select distinct trtan, avisitn, exp(mean(logaval)) as gmean from adis group by trtan, avisitn;
quit;
```
### 几何均值的计算方法（三）
```SAS
*** proc ttest, dist 选项指定为lognormal， 即对数正态分布 ***;
ods output ConfLimits=gmt3;
proc ttest data=adis dist=lognormal;
	by trtpn avisitn;
    var aval;
run;
```
### 几何均值的计算方法（四）
```SAS
*** proc surveymeans, 添加geomean gmclm等keywords ***;
ods output GeometricMeans=gmt4;
proc surveymeans data=adis geomean gmclm ;
	by trtpn avisitn;
	var aval;
	run;
```
### 几何均值的计算方法（五）
```SAS
*** proc univariate, 输出数据集中取_GEOMEAN_ ***;
proc univariate data=adis outtable=DescStats noprint;
	by trtpn avisitn;
	var aval;
run;
```
