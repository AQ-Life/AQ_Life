---
title: SAS 绘制动态爱心
author: 阿Q
date: '2023-05-12'
slug: sas
categories:
  - SAS
tags:
  - SAS Advance
---

五一过后，也预示着***520***也快来了，又到了想破脑袋送礼物的时候了。SAS programmer 该如何表达自己的浪漫，这次我们利用SAS来绘制一颗动态的爱心。

![SAS动态爱心](images/heart.gif)


## 准备数据
下面绘制心形曲线的函数是来源于广大的网友，我们再按照函数生成一系列的模拟数据。
```SAS
data heart;
	pi=constant('PI');
	do t=0 to 65 by 100/400;
		x = -.01*(-t**2+35*t+1950)*sin(pi*t/180);
		y = .01*(-t**2+35*t+1950)*cos(pi*t/180);
	output;
	x=-x;
	output;
	end;
run;
```

## 定制GTL template
常见的是scatterplot绘制散点图，来描绘心形曲线。
```SAS
ods path(prepend) work.templat(update);

proc template;
define statgraph sgdesign;
begingraph / border=false;
   layout overlay / walldisplay=(fill) xaxisopts=( linearopts=( viewmin=-12 viewmax=12) display=none) yaxisopts=( linearopts=( viewmin=0 viewmax=22.0) display=none);
		scatterplot x=x y=y / markerattrs=(color=red symbol=circlefilled );
   endlayout;
endgraph;
end;
run;
```
## 绘制动态爱心曲线
SAS绘制动态曲线，主要是通过options animate=start;与options animate=stop; 这里的动态图就是多张静态图汇集而成的，我们再通过控制帧数等参数达到动态图的效果。
```SAS
title; footnote;
options noxwait missing='';
options printerpath=gif nonumber nodate animduration=0.001 animloop=yes nobyline;
options animate=start;

ods printer;

%macro aa;
	%do i=1 %to 65;
		proc sgrender data=heart template=sgdesign;
			where t<=&i.;
		run;
	%end;
%mend;
%aa;

ods printer close;
options animate=stop; 
```
