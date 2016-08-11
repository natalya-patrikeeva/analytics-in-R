intl = read.csv("intl.csv")
str(intl)
library(ggplot2)
ggplot(intl, aes(x=Region, y=PercentOfIntl)) + geom_bar(stat="identity") + geom_text(aes(label=PercentOfIntl))

intl = transform(intl, Region = reorder(Region, -PercentOfIntl))
str(intl)
intl$PercentOfIntl = intl$PercentOfIntl*100

ggplot(intl, aes(x=Region, y=PercentOfIntl)) + 
  geom_bar(stat = "identity", fill="dark blue") +
  geom_text(aes(label=PercentOfIntl), vjust=-0.4) +
  ylab("Percent of International Students") +
  theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))


# world map
library(ggmap)
intlall = read.csv("intlall.csv", stringsAsFactors = FALSE)
head(intlall)
intlall[is.na(intlall)] = 0
head(intlall)
world_map = map_data("world")
str(world_map)

world_map = merge(world_map, intlall, by.x="region", by.y="Citizenship")
str(world_map)
ggplot(world_map, aes(x=long, y=lat, group=group)) + geom_polygon(fill="white", color="black") +
  coord_map("mercator")
world_map = world_map[order(world_map$group, world_map$order),]
ggplot(world_map, aes(x=long, y=lat, group=group)) + geom_polygon(fill="white", color="black") +
  coord_map("mercator")
table(intlall$Citizenship)
intlall$Citizenship[intlall$Citizenship == "China (People's Republic Of)"] = "China"
world_map = merge(map_data("world"), intlall, by.x="region", by.y="Citizenship")
world_map = world_map[order(world_map$group, world_map$order),]
ggplot(world_map, aes(x=long, y=lat, group=group)) + geom_polygon(aes(fill=Total), color="black") +
  coord_map("mercator")
ggplot(world_map, aes(x=long, y=lat, group=group)) + geom_polygon(aes(fill=Total), color="black") +
  coord_map("ortho", orientation = c(20,30,0))
ggplot(world_map, aes(x=long, y=lat, group=group)) + geom_polygon(aes(fill=Total), color="black") +
  coord_map("ortho", orientation = c(58,56,10))
