# plotting with ggplot.
#
WHO = read.csv("WHO.csv")
str(WHO)
plot(WHO$GNI, WHO$FertilityRate)

# ggplot
install.packages("ggplot2")
library(ggplot2)
colors()
# http://www.cookbook-r.com/Graphs/Shapes_and_line_types/

scatterplot = ggplot(WHO, aes(x=GNI, y=FertilityRate))
scatterplot + geom_point()
scatterplot + geom_line()
scatterplot + geom_point(color="blue", size=3, shape=17)
scatterplot + geom_point(color="darkred", size=3, shape=15)
fertilityGNIplot = scatterplot + geom_point(color="darkred", size=3, shape=8) + ggtitle("Fertility Rate vs. Gross Nationation Income")
pdf("MyPlotFertilityvsGNI")
print(fertilityGNIplot)
dev.off()


ggplot(WHO, aes(x = GNI, y=FertilityRate, color=Region)) + geom_point()
ggplot(WHO, aes(x = GNI, y=FertilityRate, color=LifeExpectancy)) + geom_point()
ggplot(WHO, aes(x = FertilityRate, y=Under15)) + geom_point()
ggplot(WHO, aes(x = log(FertilityRate), y=Under15)) + geom_point()
model = lm(Under15 ~ log(FertilityRate), data=WHO)
summary(model)

# 95% CI (default) around linear regression line
ggplot(WHO, aes(x = log(FertilityRate), y=Under15)) + geom_point() + stat_smooth(method="lm")

# 99% CI around linear regression line
ggplot(WHO, aes(x = log(FertilityRate), y=Under15)) + geom_point() + stat_smooth(method="lm", level = 0.99)

# no CI around linear regression line
ggplot(WHO, aes(x = log(FertilityRate), y=Under15)) + geom_point() + stat_smooth(method="lm", se = FALSE, color="orange")

ggplot(WHO, aes(x = FertilityRate, y = Under15, color=Region)) + geom_point() + scale_color_brewer(palette="Dark2")
