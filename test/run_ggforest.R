library(MetaMicrobiome)
data <- system.file("data", package="MetaMicrobiome", "ggforestDemo.rda")
load(data)
head(ggforestDemoData)
ggforestDemoData$study <- factor(ggforestDemoData$study,
								 levels=rev(unique(ggforestDemoData$study)))
pointcolors <- rev(c("#E41A1C","#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#A65628", "#F781BF","#999999"))
pointshape <- c(18, 20)
data1 <- ggforestDemoData[ggforestDemoData$measure=="Shannon",]
head(data1)
data2 <- ggforestDemoData
p1 <- ggforest(dataset=data1,
       manualcolors=pointcolors,
       manualshapes=pointshape,
       x="est",
       y="study",
       Logscale=TRUE,
       lower="lower",
       upper="upper",
       colorVar="study",
       shapeVar="unite",
       pointsize=3,
       linesize=0.4,
       errorbarhheight=0.05,
       xlabs="Odds Ratio",
       ylabs="",
       setTheme=TRUE)

 p2 <- ggforest(dataset=data2,
          manualcolors=pointcolors,
          manualshapes=pointshape,
          x="est",
          y="study",
          Logscale=FALSE,
          lower="lower",
          upper="upper",
          colorVar="study",
          shapeVar="unite",
          pointsize=3,
          linesize=0.4,
          errorbarhheight=0.05,
          xlabs="Odds Ratio",
          ylabs="",
          facetx="measure",
          facety="group",
          setTheme=TRUE)

svg("single_measure_OR.svg")
p1
dev.off()
svg("multi_measure_OR.svg")
p2
dev.off()
