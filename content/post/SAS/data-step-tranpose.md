---
categories:
- SAS
- SAS Base
date: '2021-11-09'
tags:
- SAS
- Data Step
title: Data Step 转置
---


SAS Programming 中想到转置，可能都会知道 Transpose Procdure，比如有一份每个人的三次评分数据，需要进行转置。

```SAS
data score1;
	input name $ score1 score2 score3;
	datalines
	;
	lisa 100 92 .
	bill 89 80 83
	iris 77 . .
	;
run;

proc sort data=score1;
	by name;
	run;

proc transpose data=score1 out=score1_tran1(drop=_name_) prefix=col;
	by name;
	var score1 score2 score3;
run;

```

这种方法想必大家已经熟知，那么在 Data Step 该如何进行转置，其实就是十分强大的 Do & Array 工具。

```SAS
data score1_tran2(drop=i score1-score3);
	set score1;
	array ary score1-score3;
	do i=1 to dim(ary);
		score=ary(i);
		output;
	end;
run;
```

可以实现和 transpose 同样的结果，当然也可以再转置回来。

```SAS
data score1_tran3(drop=i score);
	array ary score1-score3;
	do i=1 to dim(ary);
		set score_tran2;
		ary(i)=score;
	end;
run;
```

利用 Do & Array 实现转置的强大功能体现在于下面的示例，比如有一份数据如下。

```SAS
data score2;
	input name $ level score mean ;
	datalines
	;
	lisa 1 100 57
	lisa 2 98 56
	lisa 3 91 66
	bill 1 90 55
	bill 2 89 51
	bill 3 84 47
	iris 1 88 54
	iris 2 79 62
	iris 3 77 43
	;
run;
```

想要实现同时对 score 和 mean 两个变量的转置，如果是利用 transpose 方法。

```SAS
proc sort data=score2;
	by name;
	run;

proc transpose data=score2 out=score2_tran1 prefix=level;
    by name;
    var score mean;
    id level;
run;

data score2_tran2(drop=_name_);
    merge score2_tran1(where=(_name_="score") rename=(level1=score1 level2=score2 level3=score3))
        score2_tran1(where=(_name_="mean") rename=(level1=mean1 level2=mean2 level3=mean3));
    by name;
run;
```

可以看到没有办法一步实现，需要后面采用 merge 语句，那么利用 Do & Array 的方法。

```SAS
data score2_tran3(drop= level score mean);
    array ary{3,2} score1 mean1
        score2 mean2
        score3 mean3;

    do until (last.name);
        set score2;
        by name;
        ary{level,1}=score;
        ary{level,2}=mean;
    end;
run;
```

可以一步实现对两个变量的转置。
