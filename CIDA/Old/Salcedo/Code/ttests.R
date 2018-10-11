## 2 sample t-tests for Dr. Salcedo
## Author: Ilana Trumble
## Date: 10/5/208

setwd('C:/Users/trumblei/Desktop/CIDA/Salcedo')

library(tidyverse)
library(readxl)

df <- read_excel("RawData/MitraClip EES.xlsx", sheet = "General")
head(df)

names<-c("name","mrn","etiology","clips",
                "pre_ann","post_ann","perc_ann",
                "pre_bic","post_bic","perc_bic",
                "pre_ap","post_ap","perc_ap","pre_elipp",
                "post_ellip","pre_mr","post_mr","mr_drop")
df<-df[1:60,]
length(names)
dim(df)
colnames(df)<-names


# Groups for 2 sample t-test
df<-mutate(df, group = ifelse(mr_drop<=2,"small","large"))

# Outcomes for 2 sample t-test
df<-mutate(df,d_ann=post_ann-pre_ann) # annular circumference
df<-mutate(df,d_bic=post_bic-pre_bic) # bi-commisural diameter
df<-mutate(df,d_ap=post_ap-pre_ap) # antero-posterior diameter

df %>% glimpse()

table(df$group)

d_ann_l<-df$d_ann[df$group=="large"]
d_ann_s<-df$d_ann[df$group=="small"]

d_bic_l<-df$d_bic[df$group=="large"]
d_bic_s<-df$d_bic[df$group=="small"]

d_ap_l<-df$d_ap[df$group=="large"]
d_ap_s<-df$d_ap[df$group=="small"]

t_ann<-t.test(d_ann_l,d_ann_s,alternative="two.sided",var.equal=F,conf.level=0.95)
t_bic<-t.test(d_bic_l,d_bic_s,alternative="two.sided",var.equal=F,conf.level=0.95)
t_ap<-t.test(d_ap_l,d_ap_s,alternative="two.sided",var.equal=F,conf.level=0.95)


t_ann$conf.int
t_bic$conf.int
t_ap$conf.int

t_ann$estimate
t_bic$estimate
t_ap$estimate

t_ann$p.value
t_bic$p.value
t_ap$p.value

