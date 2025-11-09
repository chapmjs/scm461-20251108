# script a
library(readr)
d1<-read_csv("financial_data.csv")
d2<-read_csv("inventory_levels.csv")
x<-d1$sales_revenue-d1$material_costs
d2$i<-d2$raw_materials_value+d2$wip_value+d2$finished_goods_value
y<-d1$labor_costs+d1$overhead_costs+d1$utilities+d1$depreciation
d<-merge(d1,d2,by="month")
d$t<-x
d$oe<-y
d$np<-d$t-d$oe
d$roi<-d$np/d$i
print(d[,c("month","t","i","oe","np","roi")])
cat("\nthe plant is making money in some months but losing in others. month 6 is better than month 1 but not by much. inventory is going up which is bad. throughput is going down which is also bad. we need to figure out why.")
