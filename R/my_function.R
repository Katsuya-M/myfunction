#'myfunction
#'
#'
#'@export

se <- function(x) {
  if(length(x) < 3) {
   0
  } else {
    sd(x) / sqrt(length(x))
  }
}

sem <- function(x, na.rm = TRUE) {
  if(na.rm) {
    x <- x[!is.na(x)]
  }
  sd(x) / sqrt(length(x))
}

mean_bar <- function(x, mult = 1) {
  x <- stats::na.omit(x)
  mean <- mean(x)
  data.frame(y = mean, ymin = mean, ymax = mean)
}

y.scale <- function(x) {
  x <- x[!is.na(x)]
  mx <- max(x)
  b <- mx * 7 / 6
  if (b == 0) {
    s <- 0
    msd <- 0
  } else {
    s <- floor(log10(b))
    msd <- b * 10 ^ ((-1) * s)
  }

  ym <- if(msd < 1.8) {
    if((ceiling(msd * 10)) %% 2 == 1) {
      ceiling(msd * 10) * 10 ^ (s - 1)-(1 * 10 ^ (s - 1))
    }else ceiling(msd * 10) * 10 ^ (s - 1)
  }else if(floor(msd) < 4) {
    if((msd*10)-(floor(msd)*10) < 5){
      (floor(msd)*10+5)*10^(s-1)
    }else (floor(msd)*10+10)*10^(s-1)
  }else{
    ceiling(msd)*10^(s)
  }

  if(ym*10^((-1)*s) == 1.4 | ym*10^((-1)*s) == .2*7){
    ym <- ym * 1.5/1.4
  }
  if(ym*10^((-1)*s) == 7){
    ym <- ym * 8/7
  }
  if(ym*10^((-1)*s) == 9){
    ym <- ym + 10^(s)
  }

  yby <- if(ym*10^(-s) < 1.2){
    2*10^(s-1)
  }else if (ym*10^(-s) < 1.4){
    4*10^(s-1)
  }else if (ym*10^(-s) < 1.6){
    5*10^(s-1)
  }else if(ym*10^(-s) <1.7 ){
    4*10^(s-1)
  }else if(ym*10^(-s) <1.9 ){
    6*10^(s-1)
  }else if(ym*10^(-s) <2.0 ){
    2*10^(s-1)
  }else if(ym*10^(-s) < 3) {
    5*10^(s-1)
  }else if(ym*10^(-s) <4){
    10*10^(s-1) # 5*10^(s-1)から変更
  }else if(ym*10^(-s) <8){
    2*10^(s)
  }else if(ym*10^(-s) <9){
    2*10^(s)
  }else 20*10^(s-1)

  f <- max(x, na.rm = T) + yby/4
  g <- f + yby/4
  return(list(ymax = ym, yscale = seq(0, ym, by = yby), yby = yby, f = f, g = g))
}

mean_se_katsu <- function(x, mult = 1){
  x <- stats::na.omit(x)
  se <- if (length(x) < 3){
    NA
  }else{
    mult * sqrt(stats::var(x)/length(x))
  }
  mean <- mean(x)
  data.frame(y = mean, ymin = mean - se, ymax = mean + se)
}



ss <- mark_pvalue <-  function(x) {
  sapply(x, function(x) {
  if (is.na(x)) {
    ""
  } else if (x == "NaN") {
    ""
  } else if (x < 0.001) {
    "***"
  } else if (x < 0.01) {
    "**"
  } else if (x < 0.05) {
    "*"
  } else {
    ""
  }})
}


chart.csv <- function(path, height.base = F) {
  require("tidyverse")
  require("magrittr")
  x <- list.files(path) %>% str_subset("^Chart.*\\.csv$")

  if (height.base == F) {
    d <- read_csv(paste0(path, "/", x[[1]])) %>%
      dplyr::select(-`height-base[uV]`)
    for (i in 2:length(x)) {
      d1 <- read_csv(paste0(path, "/", x[[i]])) %>%
        dplyr::select(-`height-base[uV]`)
      names(d1)[2] <- paste("height", i)
      d <- left_join(d, d1, by = "time[min]")
    }
    d <- d %>% set_colnames(c("time", paste(x, "(nA)"))) %>%
      mutate_at(-1, ~ ./10000)
  } else {
    d <- read_csv(paste0(path, "/", x[[1]])) %>%
      dplyr::select(-`CH1 height[uV]`)
    for (i in 2:length(x)) {
      d1 <- read_csv(paste0(path, "/", x[[i]])) %>%
        dplyr::select(-`CH1 height[uV]`)
      names(d1)[2] <- paste("height", i)
      d <- left_join(d, d1, by = "time[min]")
    }
    d <- d %>% set_colnames(c("time", paste(x, "(nA)"))) %>%
      mutate_at(-1, ~ ./10000)
     }


  return(d)
}

theme_my <- function (base_size = 25, base_family = "", base_line_size = .8,
                      base_rect_size = .8) {
  theme_bw(base_size = base_size, base_family = base_family,
           base_line_size = base_line_size, base_rect_size = base_rect_size) %+replace%
    theme(panel.border = element_blank(), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black", size = rel(1)),
          plot.title = element_text(size = base_size * 1.2, colour = "black", face = "bold", hjust = .5),
          legend.key = element_blank(),
          legend.title = element_blank(),
          axis.text = element_text(colour = "black"),
          axis.title.x = element_blank(),
          axis.ticks = element_line(colour = "black"),
          strip.background = element_rect(colour = "black", fill = NA))
}

theme_my2 <- function (base_size = 25, base_family = "", base_line_size = .8,
                      base_rect_size = .8) {
  theme_bw(base_size = base_size, base_family = base_family,
           base_line_size = base_line_size, base_rect_size = base_rect_size) %+replace%
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black", size = rel(1)),
          axis.text = element_text(colour = "black"),
          axis.ticks = element_line(colour = "black")
    )
}

my_colour <- c("#577DC7", "#C74B43", "#9BBB59", "#A5A5A5", "#55A3C8")

my_colour_palette <- colorRampPalette(c("#969696", RColorBrewer::brewer.pal(8, "Set1")))

my_colour_palette2 <- colorRampPalette(c("#969696", "#3366cc", "#dc3912", "#ff9900", "#109618", "#990099", "#0099c6", "#dd4477", "#66aa00", "#b82e2e", "#316395"))

my_colour_palette3 <- c("gray", "#265DAB", "#DF5C24", "#059748", "#E5126F", "#9D722A", "#7B3A96", "#C7B42E", "#CB2027")

# my_theme <- theme(
#   legend.text = element_text(colour="black"),
#   legend.title = element_blank(),
#   plot.title = element_text(hjust = 0.5, face = "bold"),
#   #axis.title.x = element_text(colour="black"),
#   axis.title.x = element_blank(),
#   axis.title.y = element_text(colour="black"),
#   axis.text.x = element_text(colour ="black"),
#   axis.text.y = element_text(colour="black"),
#   axis.ticks.length = unit(0.4, "lines"),
#   axis.ticks = element_line(colour = "black"),
#   axis.line = element_line(linetype = "solid")
# )

combn_df <- function(df, ...) {
  c(...) %>%
    gtools::permutations(length(.), 2, v = ., repeats.allowed = TRUE) %>%
    as.data.frame() %>%
    mutate_if(is.factor, as.character) %>%
    magrittr::set_colnames(c("name_x", "name_y")) %>%
    as_tibble() %>%
    right_join(df %>% pivot_longer(c(...), names_to = "name_x", values_to = "value_x"),
               by = "name_x") %>%
    left_join(df %>% pivot_longer(c(...), names_to = "name_y", values_to = "value_y"),
              by = c("name_y", colnames(df)[!(colnames(df) %in% c(...))]))
}

ttest_mean_df <- function(dfl, mean = "mean", sem = "sem", n = "n", group = "group") {
  data <- as.data.frame(dfl[, c(mean, sem, n, group)])
  mu <- 0
  conf.level <- 0.95
  var_sem <- function(sem, n) {
    sem ^ 2 * n
  }
  gx <- as.character(unique(data[[group]])[[1]])
  gy <- as.character(unique(data[[group]])[[2]])
  mx <- as.double(data[data[[group]] == gx, "mean"])
  my <- as.double(data[data[[group]] == gy, "mean"])
  sx <- as.double(data[data[[group]] == gx, "sem"])
  sy <- as.double(data[data[[group]] == gy, "sem"])
  nx <- as.double(data[data[[group]] == gx, "n"])
  ny <- as.double(data[data[[group]] == gy, "n"])
  vx <- var_sem(sx, nx)
  vy <- var_sem(sy, ny)
  method <- "Two Sample t-test"
  estimate <- c(mx, my)
  names(estimate) <- c("mean of x", "mean of y")
  df <- nx + ny - 2
  v <- 0
  if (nx > 1)
    v <- v + (nx - 1) * vx
  if (ny > 1)
    v <- v + (ny - 1) * vy
  v <- v/df
  stderr <- sqrt(v * (1/nx + 1/ny))
  tstat <- (mx - my - mu)/stderr
  pval <- 2 * pt(-abs(tstat), df)
  alpha <- 1 - conf.level
  cint <- qt(1 - alpha/2, df)
  cint <- tstat + c(-cint, cint)
  cint <- mu + cint * stderr
  names(tstat) <- "t"
  names(df) <- "df"
  names(mu) <- "mean"
  attr(cint, "conf.level") <- conf.level
  rval <- list(statistic = tstat, parameter = df, p.value = pval,
               conf.int = cint, estimate = estimate, null.value = mu,
               alternative = "two.sided", method = method, data.name = paste(gx, "and", gy))
  class(rval) <- "htest"
  return(rval)
}

