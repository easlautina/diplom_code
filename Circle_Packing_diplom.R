library("packcircles")
library("viridis")
library("ggiraph")
library("ggplot2")
library("xlsx")

df <- read.xlsx(file = "table.xlsx", 
                sheetIndex = "r", header = TRUE)
df

min_max_norm <- function (x) {
  (x - min(x)) / (max(x) - min(x))
}

df$Прирост.выручки.по.сравнению.с.4.кв..2022.года <- min_max_norm(df$Прирост.выручки.по.сравнению.с.4.кв..2022.года)
df[which(df$Прирост.выручки.по.сравнению.с.4.кв..2022.года == 0), 4] <- 0.01

# Create data
data <- data.frame(group=paste("Group_", df$X., df$Итоговый.кластер), 
                   value=df$Прирост.выручки.по.сравнению.с.4.кв..2022.года) 

# Add a column with the text you want to display for each bubble:
data$text <- paste("name: ",data$group)

# Generate the layout
packing <- circleProgressiveLayout(data$value, sizetype='area')
data <- cbind(data, packing)
dat.gg <- circleLayoutVertices(packing, npoints=50)

# Make the plot with a few differences compared to the static version:
p <- ggplot() + 
  geom_polygon_interactive(data = dat.gg, aes(x, y, group = id, fill=id, tooltip = data$text[id], data_id = id), colour = "black", alpha = 0.6) +
  scale_fill_viridis() +
  geom_text(data = data, aes(x, y, label = gsub("Group_", "", group)), size=4, color="black") +
  theme_void() + 
  theme(legend.position="right", plot.margin=unit(c(0,0,0,0),"cm") ) + 
  labs(color = "Позиция компании в рейтинге относительно объему выручки за 1 кв. 2023 г.") + 
  coord_equal()
p
# Turn it interactive
widg <- girafe(ggobj = p, width_svg = 7, height_svg = 7)
