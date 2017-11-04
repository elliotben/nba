library(hexbin)
x <- nbaData$pointsPerGame
y <- nbaData$X2016.17
bin<-hexbin(x, y, xbins=50) 
plot(bin, main="Hexagonal Binning")

Sys.setenv("plotly_username" = "elliotben")
Sys.setenv("plotly_api_key" = "r0Xlv5XqpHQfdmNtIQqv")

library(plotly)
library(plyr)

#-------------------------------------------------------------------------------------
p0 <- plot_ly(nbaData, x = ~doubleDoubles, y = ~tripleDoubles, color = ~averageSalaryPerYear, colors = c('#BF382A', '#0C4B8E'), text = ~paste(firstName, lastName))

#-------------------------------------------------------------------------------------

p1 <- plot_ly(nbaData, x = ~pointsPerGame, y = ~ASTPerGame, z = ~REBPerGame, color = ~averageSalaryPerYear, colors = c('#BF382A', '#0C4B8E'), text = ~paste(firstName, lastName)) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'PPG'),
                      yaxis = list(title = 'APG'),
                      zaxis = list(title = 'RPG')))

#-------------------------------------------------------------------------------------

p2 <- plot_ly(nbaData, x = ~STLPerGame, y = ~BLKPerGame, z = ~REBPerGame, color = ~averageSalaryPerYear, colors = c('#BF382A', '#0C4B8E'), text = ~paste(firstName, lastName)) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'STLPG'),
                      yaxis = list(title = 'BLKPG'),
                      zaxis = list(title = 'REBPG')))

#-------------------------------------------------------------------------------------

p3 <- function() {
total <- count(nbaData$age)
total <- total[(which(total$freq >= 5)),]
x <- c()
y <- c()
for (i in 1:nrow(total)) {
  fgPercent <- round(mean(na.omit(nbaData[(which(nbaData$age == total[i,1] & nbaData$minutesPerGame >= 25 )), 14])), digits = 3)
  x <- c(x, fgPercent)
}
total$fgPercent <- x

for (i in 1:nrow(total)) {
  ppg <- round(mean(na.omit(nbaData[(which(nbaData$age == total[i,1])), 12])), digits = 2)
  y <- c(y, ppg)
}
total$ppg <- y

fit <- fitted(loess(total$ppg ~ total$x))

p3 <<- plot_ly(data = total, x = ~x, y = ~ppg, type = "bar", showlegend=FALSE, text = ~fgPercent,
              marker=list(color = ~fgPercent, showscale=TRUE, colors = c('#BF382A', '#0C4B8E'))) %>%
  add_lines(y = fit, showlegend=FALSE, color = 'black') %>%
  layout(showlegend=FALSE, xaxis = list(title = 'age'),
         yaxis=list(showgrid=FALSE)) 
return(p3)
}

p5 <- plot_ly(nbaData, x = ~pointsPerGame, y = ~minutesPerGame, color = ~averageSalaryPerYear, colors = 'RdYlBu', text = ~paste(firstName, lastName))
p6 <- plot_ly(nbaData, x = ~pointsPerGame, y = ~averageSalaryPerYear, color = ~FGPercentage, colors = 'YlOrRd', text = ~paste(firstName, lastName))
#-------------------------------------------------------------------------------------
#'RdGy'
#p4 <- function() {
  total <- count(nbaData$position)
  x <- c()
  y <- c()
  for (i in 1:nrow(total)) {
    fgPercent <- round(mean(na.omit(nbaData[(which(nbaData$position == total[i,1])), 14])), digits = 3)
    x <- c(x, fgPercent)
  }
  total$fgPercent <- x
  
  for (i in 1:nrow(total)) {
    ppg <- round(mean(na.omit(nbaData[(which(nbaData$position == total[i,1])), 12])), digits = 3)
    y <- c(y, ppg)
  }
  total$ppg <- y
  
  total <- total[order(total[,4]),]
  
 # fit <- fitted(loess(total$ppg ~ total$x))
  
  p4 <- plot_ly(data = total, x = ~x, y = ~ppg, type = "bar", showlegend=FALSE, text = ~fgPercent, color = ~fgPercent, colors = 'Greens',
                 marker=list(color = ~fgPercent, colors = 'Greens', showscale=TRUE)) %>%
#    add_lines(y = fit, showlegend=FALSE, color = 'black') %>%
    layout(showlegend=FALSE, xaxis = list(title = 'position'),
           yaxis=list(showgrid=FALSE)) 
#  return(p4)
#}

#   chart_link = plotly_POST(p0, filename="doubledoubles_tripledoubles_NBA")
#   chart_link = plotly_POST(p1, filename="3d_ppg_apg_rpg_NBA")
#   chart_link = plotly_POST(p2, filename="3d_stlpg_blkpg_rebpg_NBA")
#   chart_link = plotly_POST(p3, filename="ppg_fg_age_NBA")
#   chart_link = plotly_POST(p4, filename="ppg_salary_1_NBA")
#   chart_link = plotly_POST(p5, filename="ppg_salary_1_NBA")
#   chart_link = plotly_POST(p6, filename="ppg_salary_2_NBA")
  