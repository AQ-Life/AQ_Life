---
title: Shiny package 绘制 COVID-19 Plot
author: 阿Q
date: '2021-11-18'
slug: shiny-package-covid-19-plot
categories:
  - R
tags:
  - ggplot2
  - shiny
---


Shiny package 是在R中进行数据分析可视化非常著名的一个包，我们就采用这个包来展示他的一些功能。
数据来源和前期数据处理的解释，请参考上一篇文章 [R绘制COVID-19新增病例趋势图](https://www.jianshu.com/p/eceae510733a) ，这里直接放上前半部分的程序。
```
#清空工作环境
rm(list = ls())
options(digits = 4)
#设置工作目录
setwd("D:/360Downloads/R/project/APP2")

library(xlsx)
library(dplyr)
library(shiny)
library(ggplot2)
covid <- read.csv("owid-covid-data.csv")
country <- read.xlsx("国家和地区代码.xlsx",
                     sheetIndex="Sheet1",
                     header=F,
                     startRow=9)

covid1 <- transform(covid, 
                    peo_vac=people_vaccinated/10000, 
                    peo_fvac=people_fully_vaccinated/10000,
                    ana_new=new_cases,
                    ana_dea=new_deaths,
                    low=0)

covidvac1 <- subset(covid1, subset = (peo_vac!="NA"), select = c(iso_code, date, people_vaccinated_per_hundred, people_fully_vaccinated_per_hundred))
covidvac2 <- aggregate(covidvac1, by = list(covidvac1$iso_code), FUN = last)
covidvac3 <- rename(covidvac2, c(peo_vac1 = "people_vaccinated_per_hundred", peo_fvac1 = "people_fully_vaccinated_per_hundred"))
covid2 <- left_join(covid1,
                    covidvac3,
                    by=c("iso_code"="iso_code", "date"="date"))
                    
country1 <- subset(country, 
                   subset = (X6!="NA"), 
                   select = c(X2,X3,X6))

anadata1 <- left_join(covid2,
                      country1,
                      by=c("iso_code"="X6"))

#定义移动平均数的函数
mav <- function(a,n){
  stats::filter(a,rep(1/n,n),sides = 1)
}
anadata2 <- transform(anadata1,
                      mean_new=mav(ana_new,7),
                      mean_dea=mav(ana_dea,7),
                      country_code=paste(iso_code,"_",X2,"_",X3))

countryname <- unique(anadata2$country_code)
```
接下来就是 shiny 包，准备针对每个国家绘制 新增病例、死亡、接种疫苗 三个图形，UI 设计程序如下：
```
ui <- fluidPage(
  titlePanel("COVID-19"),
  sidebarLayout(
    sidebarPanel(
      selectInput("country_code",
                  "Country:",
                  countryname),
      radioButtons("ratio", label = "R Plot",
                   choices = list("新增病例" = 1, "死亡" = 2, "接种疫苗" = 3),
                   selected = 1)
    ),
    mainPanel(
      h2(textOutput("textoutput")),
      plotOutput("plotoutput")
    )
  )
)
```
SERVER 服务器端程序基本同上一篇的文章的 ggplot2 绘图。
```

server <- function(input, output){
  formulaText <- reactive({
    input$country_code
  })
  
  output$textoutput <- renderText({
    formulaText()
  })
  
  output$plotoutput <- renderPlot({
    plotdata <- switch (input$country_code,
        subset(anadata2,country_code==input$country_code)
    )
    
    p1 <- ggplot(plotdata,aes(x=as.Date(date))) + 
      {if(input$ratio == 1) geom_col(aes(y=ana_new,fill="g_col"))} +
      {if(input$ratio == 2) geom_col(aes(y=ana_dea,fill="g_col"))} +
      {if(input$ratio == 3) geom_area(aes(y=peo_vac,fill="g_area1"))} +
      {if(input$ratio == 3) geom_area(aes(y=peo_fvac,fill="g_area2"))} +
      {if(input$ratio == 1) geom_line(aes(y=mean_new,color="g_line"),size=1)} +
      {if(input$ratio == 2) geom_line(aes(y=mean_dea,color="g_line"),size=1)} +
      {if(input$ratio == 3) geom_text(aes(y=peo_vac, label=peo_vac1))} +
      {if(input$ratio == 3) geom_text(aes(y=peo_fvac, label=peo_fvac1))} +
      
      labs(x=NULL,y=NULL) +
      scale_x_date(date_label="%y/%m/%d",
                   date_breaks = "3 month",
                   minor_breaks = "1 month") +
      {if(input$ratio == 1) scale_fill_manual(breaks = c("g_col"), 
                             values = c("#cad5e5"), 
                             label = c("New Case"))} + 
      {if(input$ratio == 2) scale_fill_manual(breaks = c("g_col"), 
                                              values = c("#c5c4c5"), 
                                              label = c("Death"))} + 
      {if(input$ratio == 3) scale_fill_manual(breaks = c("g_area1", "g_area2"), 
                                              values = c("#b9cfe7", "#8ca6ce"), 
                                              label = c("Vaccined", "Fully Vaccined"))} +
      {if(input$ratio == 1) scale_color_manual(breaks = c("g_line"),
                                               values = c("blue"), 
                                               label = c("Monving Average"))} +
      {if(input$ratio == 2) scale_color_manual(breaks = c("g_line"),
                                               values = c("#616161"), 
                                               label = c("Monving Average"))} +
      
      theme(plot.title =element_text(hjust = 0.5, vjust = 0.5), 
            legend.position = "bottom", 
            legend.title = element_blank(), 
            legend.background = element_blank()) 
    
    p2 <- p1 + theme(panel.background=element_rect(fill='transparent', 
                                                   color='gray'),
                     legend.key=element_rect(fill='transparent', 
                                             color='transparent'))
    p2
  })
}
```
最后就是 run script，并进行成果展示。
```
shinyApp(ui = ui, server = server)
```
![COVID-19.png](https://upload-images.jianshu.io/upload_images/27304938-baec0588521f8a04.png?imageMogr2/auto-orient/strip%7CimageView2/2/w/1240)
![COVID-19.png](https://upload-images.jianshu.io/upload_images/27304938-dea0d58aff0956d0.png?imageMogr2/auto-orient/strip%7CimageView2/2/w/1240)
![COVID-19.png](https://upload-images.jianshu.io/upload_images/27304938-bc0440240a75b343.png?imageMogr2/auto-orient/strip%7CimageView2/2/w/1240)

如图，我们就可以非常方便的通过下拉框和选项查看每个国家的这三张趋势图了。
