---
categories:
- CDISC
- SDTM
date: '2021-11-16'
description: ""
tags:
- CDISC
- SDTM
title: VS Domain
---


## VS = Vital Signs，生命体征

VS 是 findings domain，主要包含了血压、体温、呼吸、脉搏、身高、体重等检查结果。

### **VSTESTCD/VSTEST**

-&zwnj;-TEST 是 findings domain 的标志。
生命体征检查项和简称都有 Terminology，比如常见的
- SYSBP = Systolic Blood Pressure
- DIABP = Diastolic Blood Pressure
- PULSE = Pulse Rate
- RESP = Respiratory Rate
- TEMP = Temperature
- HEIGHT = Height
- WEIGHT = Weight
- INTP = Interpretation

国内一些试验习惯将身高体重放置在 CRF Demographic 页面，但我们在注释CRF的时候应该注意要将其注释到 VS domain.

### **VSPOS**

体位的收集在有些时候十分有必要，受试者由于特殊情况，可能导致无法坐立，只能躺着，而其他受试者坐立，此时两者的血压差异并不能作为后续分析时判断的依据。

### **VSORRES/VSORRESU**

-&zwnj;-ORRES 是收集到的原始结果，-&zwnj;-ORRESU 是原始结果的单位，需要注意 -&zwnj;-ORRESU 是有 Terminology，那么如果EDC中未设置好标准术语的单位，需要对其进行转换。比如EDC中单位是 “s”，表示秒，需要将其转换为 “sec”，以满足数据递交的标准。

### **VSSTRESC/VSSTRESN/VSSTRESU**

这组变量强调的是在标准单位下的字符型或数值型结果。表示结果的 -&zwnj;-ORRES，-&zwnj;-STRESC，-&zwnj;-STRESN 这三个变量的衍生顺序为：
-&zwnj;-ORRES -&zwnj;-> -&zwnj;-STRESC -&zwnj;-> -&zwnj;-STRESN
当 -&zwnj;-STRESC -&zwnj;-> -&zwnj;-STRESN 无法进行转换时，-&zwnj;-STRESN 为missing，比如：
|-&zwnj;-ORRES|-&zwnj;-STRESC|-&zwnj;-STRESN|
|:--|:--|:--|
|<100|<100|.|

如果原始单位与标准单位不一致，就会存在相应的转换关系，特别是在 LB Domain 中会经常碰到，需要注意 -&zwnj;-ORRES -&zwnj;-> -&zwnj;-STRESC 的转换。比如从原始单位转换为标准单位需要乘以10，

|-&zwnj;-ORRES|-&zwnj;-STRESC|-&zwnj;-STRESN|
|:--|:--|:--|
|<100|<1000|.|

可以看到我们需要将 -&zwnj;-ORRES 中的100*10之后再保留<，放置在 -&zwnj;-STRESC中。

### **VSSTAT/VSREASND**

当检查项未做时，需要衍生这一组 -&zwnj;-STAT/-&zwnj;-REASND 变量。

### **VSLOBXFL/VSBLFL**

-&zwnj;-LOBXFL是 SDTM IG 3.3 新增的变量，定义为用药前的最后一条非缺失记录，可能大家会认为该定义似乎和 - -BLFL 一样，认为基线也是定义为用药前的最后一条非缺失记录。
那么在临床试验中，的确是很多临床试验都将基线定义为用药钱的最后一条非缺失记录，但也存在一些特殊情况。比如可能选择对某一指标每隔1h连续三次检测，将三次结果的平均值作为基线，也是可以的。



