#' @title
#' ryx
#'
#' @description
#' The ryx function prints a
#' table of correlations between a dependent variable y and one
#' or more independent x variables.
#'
#' @export
#'
#' @import MASS
#'
#' @examples
#' ryx(mtcars, "mpg")
#'

ryx <- function(data, y, x){
  if(missing(x)){
    x <- names(data)[sapply(data, class)=="numeric"]
    x <- setdiff(x, y)
  }
  df <- data.frame()
  for (var in x){
    res <- cor.test(data[[y]], data[[var]])
    df_temp <- data.frame(variable = var,
                          r = res$estimate,
                          p = res$p.value)
    df <- rbind(df, df_temp)
    df <- df[order(-abs(df$r)),]
  }

  df$sigif <- ifelse(df$p < .001, "***",
                     ifelse(df$p < .01, "**",
                            ifelse(df$p < .05, "*", " ")))
  results <- list(y=y, x=x, df=df)
  class(results) <- "ryx"
  return(results)
}


#' @title
#' print.ryx
#'
#' @description
#' The print function takes correlations
#' created by the ryx function and prints
#' a table of correlations that notes when the p value
#' is < 2e-16
#'
#' @export
#'
#' @import MASS
#'
#' @examples
#' library(MASS)
#' x <- ryx(Boston, y="medv")
#' x
#'


print.ryx <- function(x, digits = 3){
  df <- x$df
  df$p[df$p<2.0e-16] <- 2.0e-16
  df$p[df$p == 2.0e-16] <- "<2.0e-16"
  for (y in df$p){
    if(y != "<2.0e-16"){
      df$p[df$p == y] <-signif(as.numeric(y), digits)
    }
  }
  print(df)
}


#' @title
#' summary.ryx
#'
#' @description
#' The summary function takes correlations
#' created by the ryx function and prints
#' a summary of the median correlation, the range,
#' and how many variables are significant
#' at the p < 0.05 level.
#'
#' @export
#'
#' @import MASS
#'
#' @examples
#' library(MASS)
#' x <- ryx(Boston, y="medv")
#' summary(x)



summary.ryx<- function(x){
  vars <-  paste(as.list(x$df$variable),collapse = " ")
  med <- round(median(abs(x$df$r)), 3)
  signif <- length(which(x$df$sigif=="***"))
  txt<- paste0("Correlating ", x$y, " with ", vars,
               ". The median absolute correlation was ",
               med, " with a range from ", round(min(x$df$r),3), " to ",
               round(max(x$df$r),3), ". ", signif, " out of ",
               length(x$df$sigif), " variables were significant at the p < 0.05 level."
  )
  cat(txt)
}



#' @title
#' plot.ryx
#'
#' @description
#' The plot function takes correlations
#' created by the ryx function and prints
#' a plot of r values
#'
#' @export
#'
#' @import ggplot2 dplyr MASS
#'
#' @examples
#' library(MASS)
#' x <- ryx(Boston, y="medv")
#' plot(x)
#'


plot.ryx<- function(x){
  x$df$variable <- factor(x$df$variable, levels = x$df$variable[order(abs(x$df$r))])
  ggplot(data=x$df,aes(x = abs(x$df$r), y= x$df$variable))+
    geom_point(aes(color = r >0 ))+
    scale_color_manual(values = c("red", "blue"), labels = c("negative", "positive"))+
    geom_segment( aes( xend=0, yend = variable)) +
    scale_x_continuous(breaks = seq(0.0, 1.0, 0.1)) +
    geom_vline(xintercept= seq(0.0, 1.0, 0.1),linetype=2, color = "light grey") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black")) +
    labs(title= paste("Correlations with", x$y), y="Variables", x = "Correlation (absolute value)",
         color = "Direction")

}
