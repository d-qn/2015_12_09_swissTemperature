library(dplyr)
library(tidyr)
library(swiTheme)
library(animation)


translation.file <- "input/translations_temperatureDataVis.csv"

url_stations <- c(
  "http://www.meteosuisse.admin.ch/product/output/climate-data/homogenous-monthly-data-processing/data/homog_mo_BAS.txt",
  "http://www.meteosuisse.admin.ch/product/output/climate-data/homogenous-monthly-data-processing/data/homog_mo_BER.txt",
  "http://www.meteosuisse.admin.ch/product/output/climate-data/homogenous-monthly-data-processing/data/homog_mo_CHD.txt",
  "http://www.meteosuisse.admin.ch/product/output/climate-data/homogenous-monthly-data-processing/data/homog_mo_CHM.txt",
  "http://www.meteosuisse.admin.ch/product/output/climate-data/homogenous-monthly-data-processing/data/homog_mo_DAV.txt",
  "http://www.meteosuisse.admin.ch/product/output/climate-data/homogenous-monthly-data-processing/data/homog_mo_ENG.txt",
  "http://www.meteosuisse.admin.ch/product/output/climate-data/homogenous-monthly-data-processing/data/homog_mo_GVE.txt",
  "http://www.meteosuisse.admin.ch/product/output/climate-data/homogenous-monthly-data-processing/data/homog_mo_LUG.txt",
  "http://www.meteosuisse.admin.ch/product/output/climate-data/homogenous-monthly-data-processing/data/homog_mo_SAE.txt",
  "http://www.meteosuisse.admin.ch/product/output/climate-data/homogenous-monthly-data-processing/data/homog_mo_SIA.txt",
  "http://www.meteosuisse.admin.ch/product/output/climate-data/homogenous-monthly-data-processing/data/homog_mo_SIO.txt",
  "http://www.meteosuisse.admin.ch/product/output/climate-data/homogenous-monthly-data-processing/data/homog_mo_SMA.txt"
)

#### READ DATA
# Read each txt file and save it as CSV
tables <- lapply(paste0("input/", basename(url_stations)), function(filen) {
  read.table(filen, skip = 28, stringsAsFactors = FALSE)
})
names(tables) <- gsub(".*\\_(.*)\\.txt$", "\\1", basename(url_stations))

# create a long table with year, month, temperature and station
data <- do.call(rbind, lapply(1:length(tables), function(i) {
  station <- names(tables)[i]
  cbind(tables[[i]][,1:3], station = station)
}))
colnames(data)[1:3] <- c('year', 'month', 'temp')

# read translation file
txt <- read.csv(translation.file, row.names = 1, stringsAsFactors = F)



## Shape the data
shapeData <- function(data, stations) {
  stopifnot(length(stations) > 0)

  ## Subset to take only the stations
  dd <- data %>% filter(station %in% stations)
  ## Compute the monthly average
  df <- dd %>% group_by(year, month) %>% summarise(monthlyAverage = mean (temp, na.rm = T)) %>% ungroup()
  ## Compute the yearly average
  df <- df %>% group_by(year) %>% mutate(yearlyAverage = mean(monthlyAverage)) %>% ungroup()
  # set the month as factor
  df$month <- as.factor(df$month)

  # compute the average by month prior to 2015
  average <- df %>% filter(year < 2015) %>% group_by(month) %>%
    summarise(average = mean(monthlyAverage)) %>% ungroup()
  # add a boolean column if the yearly average was hotter than the average over all years & a new cumulative max
  df$hotterThanAverage <- df$yearlyAverage > mean(average$average) & df$yearlyAverage == cummax(df$yearlyAverage)

  list(df, average)
}


## Define all ggplot theme and graphic parameters
smoothAv.col <- "#8c8c8c"
hline.size <- 0.5
line.text.size <- 4

fontv <- c("Open Sans", "Open Sans Semibold", "Open Sans Light")

plot.intro <- function(df, plot.title, average, y.range, monthly.average.label, overall.average.label, fontv) {
  # add average overall smoothed curved
  bg.p1 <- ggplot() +
    geom_smooth(data = average, aes(month, average, group = 1),
      size = 3, se = F, alpha = 0.2, color = smoothAv.col) +
    ggtheme() +
    scale_y_continuous(limits = y.range, expand = c(0, 0), name = "",
                       labels = function(x) paste0(x,'°'), breaks = pretty_breaks(6)) +
    scale_x_discrete(expand = c(0, 0), name = "") + ggtitle(plot.title)
  bg.p2 <- bg.p1 + geom_text(data = data.frame(x = 6.5, y = y.range[2] - (diff(y.range) / 8),
                                               label = monthly.average.label), size = 6,
                             aes(x = x,y = y,label = label), family = fontv[2], hjust = 0.5, color = smoothAv.col)

  bg.p3 <- bg.p2 + geom_hline(yintercept = mean(average$average),
                              linetype = 2, alpha = 0.9, color = smoothAv.col)
  gridFormat(bg.p3)

  bg.p4 <- bg.p3 +
    geom_text(data = data.frame(x = 1, y = mean(average$average),
      label = paste0(" ", overall.average.label, ": ", formatC(mean(average$average), digits = 3), "°")),
      size = line.text.size, color = smoothAv.col,
      aes(x = x,y = y,label = label), family = fontv[1], hjust =  0, vjust = 1.4)

  invisible(sapply(1:5, function(i) gridFormat(bg.p4)))

  bg.p <- bg.p1 +
    geom_hline(yintercept = mean(average$average),
      linetype = 2, alpha = 0.5, color = smoothAv.col) +
    geom_text(data = data.frame(x = 1, y = mean(average$average),
      label = paste0(" ", overall.average.label, ": ", formatC(mean(average$average), digits = 3), "°")),
      size = line.text.size, color = smoothAv.col, alpha = 0.9,
      aes(x = x,y = y,label = label), family = fontv[1], hjust =  0, vjust = 1.4)
  gridFormat(bg.p)
  bg.p
}

plot.year <- function(df, y, bg.p, group.colors, fontv) {

  ddd <- filter(df, year == y)
  # background : past years
  p1 <- bg.p +
    geom_line(data = df %>% filter(year < y, hotterThanAverage == FALSE),
              aes(month, monthlyAverage, group = year, colour = colorGroup), size = 0.2, alpha = 0.4) +
    scale_color_manual(values = group.colors) + guides(color = "none")

  # background : the last year hotter than the average & previous max
  prior.max <- df %>% filter(year < y, hotterThanAverage == TRUE) %>%
    group_by(year, colorGroup) %>% summarise(max = last(yearlyAverage)) %>% ungroup()

  temp.max <- prior.max %>% tail(1)
  old.max <- prior.max[-nrow(prior.max),]
  old.max$alpha <- seq(0.05, 0.4, length.out = nrow(old.max))

  # add horizontal line for the latest max yearly temp
  if(nrow(temp.max) == 0) {
    p2 <- p1
  } else {
    p2 <- p1 +
      geom_hline(yintercept = temp.max$max, linetype = 5, alpha = 0.9,
        colour =  as.vector(group.colors[temp.max$colorGroup])) +
      geom_text(data = data.frame(x = 12, y = temp.max$max,
        label = paste0(formatC(temp.max$max), "° ", temp.max$year)),
        aes(x = x, y = y, label = label), family = fontv[1], size = line.text.size,
        colour = as.vector(group.colors[temp.max$colorGroup]),
        hjust = 1, vjust = 1.25, alpha = 0.9)

    if(nrow(old.max) > 0) {
      p2 <- p2 + geom_hline(yintercept = old.max$max, linetype = "dotted", alpha = old.max$alpha,
        colour =  as.vector(group.colors[old.max$colorGroup]))
    }
  }

  y.range <- ggplot_build(bg.p)$panel$ranges[[1]]$y.range
  main.p <- p2 +
    geom_line(data = ddd, aes(month, monthlyAverage, group = year, colour = colorGroup), size = 1.5) +
    # display year
    annotate("text", x = 10.5, y = y.range[2] - y.range[2]/10, label = y,
             family = fontv[1], size = 20, color = group.colors[unlist(ddd[1,'colorGroup'])])

  if(unique(ddd$hotterThanAverage)) {
    main.p2 <- main.p +
      geom_hline(yintercept = unlist(ddd[1, 'yearlyAverage']),
                 linetype = 5, size = 1,
                 colour = as.vector(group.colors[unlist(ddd[1, 'colorGroup'])])) +
      geom_text(data = data.frame(x = 12, y = unlist(ddd[1, 'yearlyAverage']),
                                  label = paste0(formatC(unlist(ddd[1, 'yearlyAverage'])), "° ", y)),
                aes(x = x, y = y, label = label), family = fontv[1], size = 4,
                colour = as.vector(group.colors[unlist(ddd[1,'colorGroup'])]),
                hjust = 1, vjust = -0.3)
  } else {
    main.p2 <- main.p
  }
  gridFormat(main.p2)
  if(y == 2015) sapply(1:15, function(i) gridFormat(main.p2))
}

render.gif <- function(
  data, stations, plot.title, monthly.average.label, fontv,
  overall.average.label, output.file = paste0("swissTemperature_", paste(stations, collapse ="_"), ".gif")
) {

  tmp <- shapeData(data, stations)
  df <- tmp[[1]]
  average <- tmp[[2]]

  ## Get global plot settings
  y.range <- range(df$monthlyAverage)
  y.range <- c(floor(y.range[1]), ceiling(y.range[2]))

  # define colors
  ncol.breaks <- 7
  cpal <- colorRampPalette(colors = c("#69b8c9", "#cc0000"))
  df$colorGroup <- cut(df$yearlyAverage, breaks = ncol.breaks)
  group.colors <- structure(cpal(nlevels(df$colorGroup)), names = levels(df$colorGroup))

  saveGIF({
    bg.p <- plot.intro(df, plot.title, average, y.range, monthly.average.label, overall.average.label, fontv)
    sapply(unique(df$year), function(y) plot.year(df, y, bg.p, group.colors, fontv))
  }, interval = 0.4, movie.name = output.file, ani.width = 640, ani.height = 720)

}

## Create GIF
for(lang in colnames(txt)) {

  cat("\n", lang)
  stations <- c("SAE", "ENG")

  fontbase <- txt['font', lang]
  fontv <- c(fontbase,
    ifelse(lang %in% c('ja', 'zh'), paste0(fontbase, " Bold"), paste0(fontbase, " Semibold")),
    paste0(fontbase, " Light"))

  gridFormat <- function(gg, top = txt['title', lang], bottom = txt['footer', lang]) {
    grid.arrange(gg,
      top = textGrob(top, x = 0.05, y = 0.1, vjust = 0, hjust = 0,
        gp = gpar(fontsize = 26, fontfamily = fontv[3], col = "black")),
      bottom = textGrob(bottom, x = 0.98, y = 1, vjust = 1, hjust = 1,
        gp = gpar(fontsize = 9, fontfamily = fontv[1], col = "#737373"))
    )
  }

  ggtheme <- function (yaxis = FALSE, base_family = fontv[1], base_family2 = fontv[2],
                       base_size = 11, axisColor = "#7E8279")
  {
    choose_font(base_family, FALSE)
    choose_font(base_family2, FALSE)
    ret <- theme_minimal(base_family = base_family, base_size = base_size) +
      theme(
        plot.title = element_text(hjust = -0.1, vjust = 0,
                                  size = rel(1.05), family = base_family, color = "#4d4d4d"),
        axis.text = element_text(size = rel(1.4)),
        axis.text.x = element_text(size = rel(1.1)),
        axis.title = element_blank(),
        axis.line = element_line(linetype = "solid",
          size = 0.2, color = axisColor, lineend = "round"),
        axis.line.y = element_blank(),
        axis.ticks = element_line(size = 0.5, color = axisColor),
        axis.ticks.length = unit(2.5, "mm"),
        plot.margin = unit(c(3, 7, 4, 3), "mm"),
        panel.grid = element_blank()
      )
    ret
  }

  render.gif(data, stations = stations, plot.title = paste0(txt['subtitle', lang], "\n"),
    monthly.average.label = txt['monthly.average', lang], overall.average.label = txt['overall.average', lang],
    fontv,
    output.file = paste0("swissTemperature_", paste(stations, collapse ="_"), "_", lang, ".gif"))
}

