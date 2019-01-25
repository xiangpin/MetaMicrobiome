##' @title forest plot base ggplot2 with the result of RunPoolEffect and multiVarRRTab.
##' 
##' @description
##' Polt a forest with the result of RunPoolEffect and multiVarRRTab base-on the \code{\link[ggplot2]{ggplot}}.
##' @details
##' TODO
##'
##' @param dataset dataframe; a dataframe of result of RunPoolEffect and multiVarRRTab
##' @param manualcolors character; the ponit colors. 
##' @param manualshapes character; the point shape.
##' @param Logscale logical; log2 for x-axis (default) or not?  
##' @param x character; the header name in the dataframe for map to axes of point.
##' @param y character; the header name in the dataframe for map to ayes of point.
##' @param lower character; the header name in the dataframe for map to lower of the errorbar.
##' @param upper character; the header name in the dataframe for map to upper of the errorbar.
##' @param pointsize numeric; the point size, default is 3.0 .
##' @param linesize numeric; the errorbar line size, default is 0.4 .
##' @param errorbarhheight numeric; the errorbar height size, default is 0.2 .
##' @param colorVar character; the header name in the dataframe for map to the color of point.
##' @param shapeVar character; the header name in the dataframe for map to the shape of point.
##' @param facetx character; the header name in the dataframe for map to the facet of row, default is NULL.
##' @param facety character; the header name in the dataframe for map to the facet of col, default is NULL.
##' @param xlabs character; label for the x-axis.
##' @param ylabs character; label for the y-axis.
##' @param setTheme logical; whether set the default theme.
##'
##' @return Returns a ggplot object.
##' @author Shuangbin Xu
##' @export
##' 
##' @importFrom ggplot2 ggplot geom_point geom_errorbarh geom_vline scale_color_manual scale_shape_manual labs facet_wrap  
##'
##' @examples
##'
##' library("MetaMicrobiome")
##' data <- system.file("data", package="MetaMicrobiome", "ggforestDemo.rda")
##' load(data)
##' head(ggforestDemoData)
##' ggforestDemoData$study <- factor(ggforestDemoData$study, 
##'                                levels=rev(unique(ggforestDemoData$study)))
##' print(levels(ggforestDemoData$study))
##' pointcolors <- rev(c("#E41A1C",
##'                      "#4DAF4A", 
##'			    "#984EA3", 
##'			    "#FF7F00", 
##'			    "#FFFF33", 
##'			    "#A65628",
##'			    "#F781BF",
##'			    "#999999"))
##' pointshape <- c(18, 20)
##' data1 <- ggforestDemoData[ggforestDemoData$measure=="Shannon",]
##' head(data1)
##' data2 <- ggforestDemoData
##' head(data2)
##' p1 <- ggforest(dataset=data1,
##'	      manualcolors=pointcolors,
##'	      manualshapes=pointshape,
##'	      x="est",
##'	      y="study",
##'	      Logscale=TRUE,
##'	      lower="lower",
##'	      upper="upper",
##'	      colorVar="study",
##'	      shapeVar="unite",
##'	      pointsize=3,
##'	      linesize=0.4,
##'	      errorbarhheight=0.05,
##'	      xlabs="Odds Ratio",
##'	      ylabs="",
##'	      setTheme=TRUE)
##'
##' p2 <- ggforest(dataset=data2,
##'          manualcolors=pointcolors,
##'          manualshapes=pointshape,
##'          x="est",
##'          y="study",
##'          Logscale=TRUE,
##'          lower="lower",
##'          upper="upper",
##'          colorVar="study",
##'          shapeVar="unite",
##'          pointsize=3,
##'          linesize=0.4,
##'          errorbarhheight=0.05,
##'          xlabs="Odds Ratio",
##'          ylabs="",
##'	      facetx="measure",
##'          facety="group",
##'          setTheme=TRUE)


ggforest <- function(dataset, 
			manualcolors, 
			manualshapes,
			Logscale=TRUE,
			x,
			y,
			lower,
			upper,
			pointsize=2,
			linesize=0.4,
			errorbarhheight=0.2,
			colorVar,
			shapeVar,
			facetx,
			facety,
			xlabs,
			ylabs,
			setTheme=TRUE 
			){

    if (isTRUE(Logscale)){
        xintercept <- 0.0
        xlabs <- bquote(paste(Log[2],"(",.(xlabs), ")"))
       trans_fun <- function(x) {
           log2(x)
        }
    } else {
        xintercept  <- 1.0
        trans_fun <- function(x) {
           x
        }
    }

    .deparse <- function(x) {
        paste0('trans_fun(', as.expression(x), ')')
    }

    p <- ggplot2::ggplot(data=dataset) +
        ggplot2::geom_vline(xintercept = xintercept, linetype=2, alpha=0.75) +
        ggplot2::geom_errorbarh(data=dataset,
                                ggplot2::aes_string(y= paste0('as.factor(', as.expression(y), ')'),
                                                    xmin= .deparse(lower),
                                                    xmax= .deparse(upper)),
                                alpha=0.9,                                  
                                color="gray50",
                                size=linesize,
                                height=errorbarhheight,
                                show.legend=F)+

        ggplot2::geom_point(ggplot2::aes_string(x= .deparse(x),
                                                y=paste0('as.factor(', as.expression(y), ')'),
                                                color=colorVar,                                
                                                shape=shapeVar),
                            show.legend = F, size=pointsize)+
        ggplot2::labs(x = xlabs, y = ylabs)

    if(!is.null(manualcolors)){
        p <- p + ggplot2::scale_color_manual(values=manualcolors)

    }

	if(!is.null(manualshapes)){
		p <- p + ggplot2::scale_shape_manual(values=manualshapes)
	}
	if (!is.null(facetx)){
		formulgrid <- as.formula(paste(c("",facetx), collapse= "~"))
		p <- p + ggplot2::facet_wrap(formulgrid, scale="free")
	}
	if (!is.null(facetx) & !is.null(facety)){
	    formulgrid <- as.formula(paste(c(facety, facetx), collapse= "~"))
		p <- p + ggplot2::facet_grid(formulgrid, scales="free")
	
	}
	if (isTRUE(setTheme)){
		p <- p + ggforesttheme()
	}
	return(p)
}


ggforesttheme <- function() {
    ggplot2::theme_bw()+
        ggplot2::theme(panel.grid=ggplot2::element_blank(),
                       strip.background = ggplot2::element_rect(colour=NA,
                                                                fill="grey"))
}
