---
categories:
- CDISC
- SDTM
date: '2021-11-11'
description: ""
tags:
- CDISC
- SDTM
title: AE Domain
---


## AE = Adverse Events，不良事件

不良事件可以简单理解为受试者发生的任何医学事件，不一定与试验用药有关系。

### Topic Variable
- **AETERM**
- **AEBODSYS / AEBODSYSCD**
- **AEDECOD / AEPTCD**
- **AESOC / AESOCCD**
- **AEHLGT / AEHLGTCD**
- **AEHLT / AEHLTCD**
- **AELLT / AELLTCD**

AETERM是EDC收集的不良事件名称，那么在实际记录过程中，对于同样的不良事件，却可能有着不同的AETERM，所以使用MedDRA编码很有必要，在后续分析过程中，也是主要采用的编码后的AEBODSYS/AEDECOD进行不良事件的分析。

### **AESTDTC / AEENDTC / AEDUR**

AE的开始结束日期时间，这类-&zwnj;-DTC的变量都需要采用ISO8601的格式，在后续分析中判断TEAE时需要关注这两个变量，特别是AESTDTC，如果刚好和试验用药同一天，最好可以收集 Time，方便与试验用药的 Time 进行比较。
AEDUR是收集的AE发生的时间区间，并不是由开始结束日期时间衍生的，那么-&zwnj;-DUR的这个规则也是适用于其他Domain的。

### **AEOUT**

AE的转归，考虑到受试者权益的最大化，对不良事件的随访需要随访至事件达到改善稳定的结果。值参考Termnilogy即可。

### **AEREL**

相关性，比较常见的是5种分类，肯定有关，可能有关，无法判断，可能无关，肯定无关。相关性的收集尽量不要缺失，后续分析种如果相关性缺失，会采取科学严谨的原则，以“相关”进行填补。

### **AESEV / AETOXGR**

严重程度分为轻度，中度，重度
毒性等级分为1级，2级，3级，4级，5级
一般只会选择其中一个用于试验，这两个变量的收集同样尽量不要缺失，否则也会以更严谨的原则进行填补。

### SAE
- **AESER**
- **AESDTH**
- **AESLIFE**
- **AESHOSP**
- **AESCONG**
- **AESDISAB**
- **AESMIE**

严重不良事件，那么严重不良事件和重度的不良事件是完全不同的，SAE是有定义的，那么就按照定义来判断是否属于上面的哪一种SAE。而重度体现的是程度，可能SAE但是严重程度却是轻度。比如在国内医疗体制，为了报销的问题，选择住院进行治疗，但其实是无关紧要的小疾病，那么这条AE是SAE但是轻度的AE。SAE与肯定相关的AE也是完全不同的，比如受试者发生车祸意外身亡，那么这条AE是SAE但与用药无关（当然了精神类试验药物，也可能是有关的）。

### **AESPID / AEGRPID**

说到AE的编号，首先提一下AE的记录规则，一般提倡AE发生了任何变化，比如相关性改变，严重程度改变，采取的措施改变，转归改变等等，就需要重新记录一条AE。那么在分析的时候，需要对AE发生的例次进行汇总。那么对于这样的连续的几条AE在汇总例次的时候应该计为1次。这个时候AE编号的运用显得尤为重要，将多条连续的AE的标记为一个GROUP，方便后续的分析。

### **AEPRESP**

用于标记医学特别关注的不良事件，比如在前期探索性试验中发现了某些AE发生频率较高，会对这些感兴趣的AE提前标记在CRF中。

### End Date Point Variable
**AEENRF**
**AEENRTPT / AEENTPT**

这两组变量用于表示当AEENDTC缺失时，AE结束的大致区间。前者是和reference start date and reference end date作比较进行衍生，后者是两个变量配套使用的。
那这两组变量都是可以的，但是更推荐使用-&zwnj;-ENRTPT/-&zwnj;-ENTPT。因为前者是要求和RFSTDTC/RFENDTC作比较，那么对于未接受试验用药的受试者而言，无法衍生-&zwnj;-STRF/-&zwnj;-ENRF，索性都采取第二组变量更方便实用。

### **AETRTEM**

这个变量大家可能会觉得奇怪，因为它不是SDTM IG中要求的变量，最终会被放到SUPPAE中，但不同的是，该变量是FDA validator rules中要求 "A treatment-emergent flag must be submitted."，看到这段英文就知道，其实就是TEAE的flag.