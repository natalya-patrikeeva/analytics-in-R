# murders by state
murders = read.csv("murders.csv")
str(murders)
statesMap = map_data("state")
str(statesMap)
ggplot(statesMap, aes(x = long, y = lat, group=group)) + geom_polygon(fill="white", color="black")
murders$region = tolower(murders$State)
murderMap = merge(statesMap, murders, by="region")
str(murderMap)
ggplot(murderMap, aes(x = long, y=lat, group=group, fill=Murders)) + geom_polygon(color="black") + scale_fill_gradient(low = "black", high="red", guide="legend")
ggplot(murderMap, aes(x = long, y=lat, group=group, fill=Population)) + geom_polygon(color="black") + scale_fill_gradient(low = "black", high="red", guide="legend")

# plot by murder rate per 100,000 population
murderMap$MurderRate = murderMap$Murders/murderMap$Population*100000
ggplot(murderMap, aes(x = long, y=lat, group=group, fill=MurderRate)) + geom_polygon(color="black") + scale_fill_gradient(low = "black", high="red", guide="legend")

# Washington DC is an outlier - remove it
ggplot(murderMap, aes(x = long, y=lat, group=group, fill=MurderRate)) + geom_polygon(color="black") + scale_fill_gradient(low = "black", high="red", guide="legend", limits=c(0,10))

# gun ownership
ggplot(murderMap, aes(x = long, y=lat, group=group, fill=GunOwnership)) + geom_polygon(color="black") + scale_fill_gradient(low = "black", high="red", guide="legend")
tapply(murderMap$GunOwnership, murderMap$region, summary)$california
tapply(murderMap$GunOwnership, murderMap$region, summary)$montana
tapply(murderMap$GunOwnership, murderMap$region, summary)$texas
tapply(murderMap$GunOwnership, murderMap$region, summary)$louisiana
tapply(murderMap$GunOwnership, murderMap$region, summary)$missouri
