normalize_weights = function(w) {
  diag(w) = 0
  rs = rowSums(w)
  rs[rs == 0] = 1
  w/rs
}

gearys_C = function(y, w) {
  w = normalize_weights(w)
  
  n = length(y)
  y_bar = mean(y)
  y_i = y %*% t(rep(1,n))
  y_j = t(y_i)
  num = sum(w * (y_i-y_j)^2)  
  denom = sum( (y-y_bar)^2 )
  ((n-1)/(2*sum(w))) * (num/denom)
}

w = 1*st_touches(nc, sparse=FALSE)

gearys_C(y = nc$SID74, w = w)


morans_I = function(y, w) {
  w = normalize_weights(w)
  n = length(y)
  y_bar = mean(y)
  num = sum(w * (y-y_bar) %*% t(y-y_bar))  
  denom = sum( (y-y_bar)^2 )
  (n/sum(w)) * (num/denom)
}

w = 1*st_touches(nc, sparse=FALSE)

morans_I(y = nc$SID74, w)

library(spdep)
library(maps)
library(maptools)
library(classInt)
library(RColorBrewer)

##Create an adjacency matrix for the states in the US
usa.state = map(database="state", fill=TRUE, plot=FALSE)
state.ID <- sapply(strsplit(usa.state$names, ":"), function(x) x[1])
usa.poly = map2SpatialPolygons(usa.state, IDs=state.ID)
usa.nb = poly2nb(usa.poly)
usa.adj.mat = nb2mat(usa.nb, style="B")

##Write the 0-1 adjacency matrix (not really needed for Moran's I)
W = usa.adj.mat
W[(W>0)] = 1

dc = match("district of columbia",rownames(W))

W = W[-dc, -dc]

%>% filter(state != "district of columbia" )

sd = state_data %>% filter(state %in% rownames(W)) %>% mutate(state = factor(state, levels = rownames(W)))
gearys_C(y = sd$total_clicks, w = W) 

morans_I(y = sd$total_clicks, w = W)


listW = mat2listw(W)
spdep::moran.test(sd$clicks_per_person, listW)

spdep::geary.test(sd$clicks_per_person, listW)


us_sar = spautolm(formula = total_clicks~population, data = sd, listw = listW, family = "SAR")
summary(us_car)


sd$sar_pred = us_sar$fit$fitted.values

##########plot

p1 <- ggplot(sd, aes(map_id = state)) + 
  # map points to the fifty_states shape data
  geom_map(aes(fill = total_clicks), map = fifty_states) + 
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  theme(legend.position = "bottom", 
        panel.background = element_blank()) +
  theme(plot.margin = unit(c(0, 0, 0, 0), "inches")) +
  theme_gray()
p1

p2 <- ggplot(sd, aes(map_id = state)) + 
  # map points to the fifty_states shape data
  geom_map(aes(fill = sar_pred), map = fifty_states) + 
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  theme(legend.position = "bottom", 
        panel.background = element_blank()) +
  theme(plot.margin = unit(c(0, 0, 0, 0), "inches")) +
  theme_gray() 
p2


library(gridExtra)
grid.arrange(p1, p2, ncol = 1)
