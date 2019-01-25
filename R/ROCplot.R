#' @title plot the roc curve
#' 
#' @description
#' Plot the ROC curve base on the \code{[ggplot2]}
#' @details
#' TO DO
#' 
#' @param rocplotdata dataframe, a dataframe of result contained the sensitivity and specificity.
#' @param x character, the header name in dataframe.
#' @param y character, the header name in dataframe.
#' @param xlab character, the label for the x-axis.
#' @param ylab character, the label for the y-axis.
#' @param roccolors vector, the colors for the roc curve.
#' @param legendkeyheight the height of the legend, default is `unit(0.05, "mm")`. 
#' @param legendposition vector, the position of legend, default is `c(0.67, 0.17)`. 
#' @param ... Additional arguments passed to \code{\link[ggplot2]{aes}}
#' @return Returns a ggplot object.
#' @author Shuangbin Xu
#' @export 
#' @importFrom ggplot2 ggplot geom_abline geom_path scale_x_reverse scale_y_continuous scale_color_manual theme_bw theme xlab ylab 
#' @examples
#'
#'
#'
ROCplot <- function(rocplotdata,
		      x, 
		      y, 
		      xlab,
		      ylab,
		      roccolors, 
		      legendkeyheight=0.05,
		      legendposition=c(0.67,0.17), 
		      ...){
       #require(ggplot2)
       p <- ggplot2::ggplot() +
           ggplot2::geom_abline(intercept = 1, slope = 1, color='grey')+                                                           
	       ggplot2::geom_path(data=rocplotdata,
                     ggplot2::aes_string(x, y, ...))+
           ggplot2::scale_x_reverse(expand = c(0,0)) +
           ggplot2::scale_y_continuous(expand=c(0,0))
	  if (!is.null(roccolors)) {
			p <- p +
           ggplot2::scale_color_manual(values=roccolors) 
	  }
	  	   p <- p +
           ggplot2::xlab(xlab)+
           ggplot2::ylab(ylab)+
           ggplot2::theme_bw() +
           ggplot2::theme(panel.grid = ggplot2::element_blank(),
                 legend.title = ggplot2::element_blank(),
                 legend.key.height=ggplot2::unit(legendkeyheight,"mm"),
                 legend.position=legendposition)
       return(p)
}
