pairwise.fisher.test(x = c(5, 2 , 1, 0), n = c(6, 4, 6, 6), p.adjust.method = "bonferroni")
color <- factor(c(rep("Yellow", 6), rep("Orange", 4), rep("Green", 6), rep("Brown", 6),rep("Yellow", 30), rep("Orange", 31), rep("Green",33), rep("Brown", 30)))
likes <- factor(c(rep("Yes",5), rep("No", 1), rep("Yes",2), rep("No", 2), rep("Yes", 1), rep("No", 5), rep("No", 6), rep("Yes", 14), rep("No",16), rep("Yes", 22), rep("No", 9), rep("Yes",14), rep("No", 19), rep("Yes",11), rep("No",19)))
city <- factor(c(rep("Patagonia", 22), rep("New York", 124)))
tabs <- table(likes, color, city)
tabs2 <- table(likes, city, color)

# si hay diferencias en gustos entre colores controlando por ciudades
mantelhaen.test(tabs)
# Post hoc
lor2 <- oddsratio(tabs2)  # capture log odds ratios
summary(lor2)
plot(lor2)

lor <- oddsratio(tabs)  # capture log odds ratios
summary(lor)
plot(lor)

z test of coefficients:
  
  Estimate Std. Error z value Pr(>|z|)  
No:Yes/Brown:Green|New York     0.23180    0.50842  0.4559  0.64844  
No:Yes/Green:Orange|New York    1.15849    0.51957  2.2297  0.02577 *
  No:Yes/Orange:Yellow|New York  -0.99144    0.52847 -1.8761  0.06065 .
No:Yes/Brown:Green|Patagonia    1.26567    1.73272  0.7304  0.46512  
No:Yes/Green:Orange|Patagonia   1.29928    1.28393  1.0120  0.31156  
No:Yes/Orange:Yellow|Patagonia  1.29928    1.28393  1.0120  0.31156  

woolf_test(tabs)

ftable(tabs)

col <- c("#99CCFF", "#6699CC", "#F9AFAF", "#6666A0", "#FF0000", "#000080")
fourfold(tabs,mfrow=c(2,3), color=col)
mosaicplot(tabs, main = NULL)



mydata<- data.frame(city, color, likes)

mytable <- xtabs(~city + color + likes, data=mydata)
ftable(mytable)    # print table
summary(mytable)   # chi-squa

