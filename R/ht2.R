# ht2()
# Requires
library(ggplot2)
source('htThemes.R')
load('vState.RData')

# Arguments:
#
# dat:  a data.frame that has
#   1) A a collumn of state names or state postal codes
#   2) Two or  more columns indicating state membership
#      in 1 of three classes.  For example there
#   can be columns for 4th and 8th grade subjects such
#   as Math and Reading
#
#   In this example the state classes are the
#   result of hypothesis tests comparing state
#   estimates to the corresponding national estimate.
#   The class labels are "Below", "Similar",
#   and "Above".  This is the factor level order used in
#    plot production plot.
#
# vars: two or more column numbers or names of
#     dat factor variables to use
#     The default numbers are 2 and 3.
#
# idVar: A name or column number
#        of state names or postal ids.
#        The default value is 1.
#
# idType: "State"  or "Postal"
#   The default, "State"
#   means the full state name.
#
# title:  The plot title has a default
#   that should be replaced.
#
# layout: "Col","Row", "Both"
#   The default is "Col"
#
#   Currently
#     "Col" means three columns of map in a row.
#       Fill color will encode 2nd test result varible.
#     "Row" means three rows of maps in column.
#       Fill color will encode a 2nd test result variable.
#     "Both" a 3 x 3 grid of maps.
#       A common fill color is used.  a single map with three fill colors.
#
#  fillColors:
#    Default:  c(rgb(.9,.45,1), gray(.9), rgb(.3,.9,.1))
#    The fill colors are used in all three layouts
#    The encoding reinforces the Col and Row
#        highlighting states in different panels
#
# bothFill:
#   Default: rgb(.9,.45,1)
#
# plotI
#   Default is TRUE
#   This plots postal codes in
#     highlighted state polygons


ht2 <- function(dat, vars = 2:3,
    idVar= 1, idType="State",
    title = 'Covid-19 Case Rate Compared to National Average (January 2020 - April 2021)',
    layout = c('Col','Row','Both')[1],
    fillCol = c(rgb(0,.2,.8,.5), gray(.9),rgb(1,0,.2,.5)),
    bothFill = rgb(0,.5,1),
    plotId = TRUE){

# 1. Select the variable and state id columns
#    by positions or names.

  cnames <- colnames(dat)
  if (is.numeric(vars)) varNams <- cnames[vars] else
    varNams = vars
  if (is.numeric(idVar)) stateNam <- cnames[idVar] else
    stateNam = idVar
  dat <- dat[,c(stateNam,varNams)]
  varLabs <- varNams

# 2. Remove non-state rows
#    Convert full state sames to
#    to postal codes for matching
#    polygon boundary ids

if (idType == "State") {
  good <- !is.na(match(dat[, 1], vStateIds$Full))
  datG <- dat[good, ]
  subs <- match(datG[,1],vStateIds$Full)
  datG[, 1] <- vStateIds$Postal[subs]
} else {
  good <- !is.na(match(dat[, 1], vStateIds$Postal))
  datG <- dat[good, ]
}
colnames(datG)[1] <- "Postal"

# 3. Convert polygon Row and/or Column numbers
#    to factors using foreground Row and/or Column
#    factor levels

switch(layout,
  Both = {
    stBg <- vStateBg3x3
    colF <- levels(datG[, 2])
    rowF <- levels(datG[, 3])
    stBg$Col <- factor(colF[stBg$colInt], levels = colF)
    stBg$Row <- factor(rowF[stBg$rowInt], levels = rowF)
  },
  Col = {
    stBg <- vStateBg3
    colF <- levels(datG[, 2])
    stBg$Col <- factor(colF[stBg$colInt], levels = colF)
  },
  Row =  {
    stBg <- vStateBg3
    rowF <- levels(datG[, 2])
    stBg$Row <- factor(rowF[stBg$colInt], levels = rowF)
  })

# 4. Build the foreground data.frame for highlighting states
#    This associates the 2 class values for each state
#    with all of its polygon vertices.
#    Two states have more than one polygon.  No problem.

vertexStateNams <- as.character(vStatePolys$st)
subs <- match(vertexStateNams, datG[, 1])
fgDat <- data.frame(datG[subs, ])
colNams <- switch(
  layout,
  Col = c('st', 'Col', 'Fill'),
  Row = c('st', 'Row', 'Fill'),
  Both = c('st', 'Col', 'Row')
)
colnames(fgDat) <- colNams
fg <- cbind(vStatePolys, fgDat[, -1])

# 5. Build a data.frame to plot postalIds

if (plotId) {
  useNams = switch(
    layout,
    Both = c('st', 'Col', 'Row'),
    Col = c('st', 'Col', 'Fill'),
    Row = c('st', 'Row', 'Fill')
  )
  colnames(datG) = useNams
  subs <- match(datG[, 1], vStateIdLocs$st)
  fgText <- cbind(vStateIdLocs[subs, ], datG[, 2:3])
}

# 6. Get labels for the fill color legend

names(fillCol) <- levels(datG[,3])

# 7. Produce the plot

switch(layout,
  Both = {
    if (plotId) {
      ggplot(stBg, aes(x = x, y = y)) +
        geom_polygon(color = gray(.75), fill = 'white') +
        geom_polygon(data = fg,
          aes(x = x, y = y),
          fill = bothFill,
          color = "black") +
        geom_text(
          data = fgText,
          aes(x = x, y = y, label = st),
          #fontface = "bold",
          size = 3.0,
        ) +
        facet_grid(Row ~ Col, switch = "both", as.table = FALSE) +
        labs(x = varLabs[1], y = varLabs[2], title = title) +
        htTheme3
    } else {
      ggplot(stBg, aes(x = x, y = y)) +
        geom_polygon(color = gray(.75), fill = 'white') +
        geom_polygon(data = fg,
          aes(x = x, y = y),
          fill = bothFill,
          color = "black") +
        facet_grid(Row ~ Col, switch = "both", as.table = FALSE) +
        htTheme3
    }
  },
  Col = {
    if (plotId) {
      ggplot(stBg, aes(x = x, y = y)) +
        geom_polygon(color = gray(.75), fill = 'white') +
        geom_polygon(data = fg,
          aes(x = x, y = y, fill = Fill),
          color = "black") +
        geom_text(
          data = fgText,
          aes(x = x, y = y, label = st),
          fontface = "bold",
          size = 3.0,
        ) +
        facet_grid(. ~ Col, switch = "both", as.table = FALSE) +
        scale_fill_manual(values = fillCol) +
        labs(x = varLabs[1], y = NULL, title = title) +
        guides(
          fill = guide_legend(
            legend.position = "top",
            title.position = 'top',
            title = varLabs[2],
            title.hjust = .5,
            title.vjust = .5,
            legend.margin = unit(c(0, 0, 0, 0), "cm"),
            label.position = "bottom",
            label.hjust = .5,
            label.theme = element_text(
              color = 'black',
              angle = 0,
              size = 10
            ),
            keywidth = 5,
            keyheight = 0.8
          )
        ) +
        htTheme3
    } else {
      ggplot(stBg, aes(x = x, y = y)) +
        geom_polygon(color = gray(.75), fill = 'white') +
        geom_polygon(data = fg,
          aes(x = x, y = y, fill = Fill),
          color = "black") +
        facet_grid(. ~ Col, switch = "both", as.table = FALSE) +
        scale_fill_manual(values = fillCol) +
        labs(x = varLabs[1], y = NULL, title = title) +
        guides(
          fill = guide_legend(
            legend.position = "top",
            title.position = 'top',
            title = varLabs[2],
            title.hjust = .5,
            title.vjust = .3,
            legend.margin = unit(c(0, 0, 0, 0), "cm"),
            label.position = "bottom",
            label.hjust = .5,
            label.theme = element_text(
              color = 'black',
              angle = 0,
              size = 10
            ),
            keywidth = 5,
            keyheight = 0.8
          )
        ) +
        htTheme3
    }
  },
  Row = {
    if (plotId) {
      ggplot(stBg, aes(x = x, y = y)) +
        geom_polygon(color = gray(.75), fill = 'white') +
        geom_polygon(data = fg,
          aes(x = x, y = y, fill = Fill),
          color = "black") +
        geom_text(
          data = fgText,
          aes(x = x, y = y, label = st),
          fontface = "bold",
          size = 2.2,
        ) +
        facet_grid(Row ~ ., switch = "both", as.table = FALSE) +
        scale_fill_manual(values = fillCol) +
        labs(y = varLabs[1], x = NULL, title = title) +
        guides(
          fill = guide_legend(
            legend.position = "bottom",
            legend.margin = unit(c(0, 0, 0, 0), "cm"),
            title.position = 'bottom',
            title.margin = unit(c(0, 0, 0, 0), "cm"),
            title = varLabs[2],
            title.hjust = .5,
            title.vjust = 1,
            label.position = "top",
            label.hjust = .5,
            label.vjust = 1,
            label.theme = element_text(
              color = 'black',
              angle = 0,
              size = 10
            ),
            keywidth = 2.5,
            keyheight = 0.7
          )
        ) +
        htTheme3
    } else {
      ggplot(stBg, aes(x = x, y = y)) +
        geom_polygon(color = gray(.75), fill = 'white') +
        geom_polygon(data = fg,
          aes(x = x, y = y, fill = Fill),
          color = "black") +
        facet_grid(Row ~ ., switch = "both", as.table = FALSE) +
        scale_fill_manual(values = fillCol) +
        labs(y = varLabs[1], x = NULL, title = title) +
        guides(
          fill = guide_legend(
            legend.position = "bottom",
            title.position = 'bottom',
            title = varLabs[2],
            title.hjust = .5,
            title.vjust = .5,
            title.margin = unit(c(0, 0, 0, 0), "cm"),
            legend.margin = unit(c(0, 0, 0, 0), "cm"),
            label.position = "top",
            label.hjust = .5,
            label.vjust = .5,
            label.theme = element_text(
              color = 'black',
              angle = 0,
              size = 10
            ),
            keywidth = 2.5,
            keyheight = 0.8
          )
        ) +
        htTheme3
    }
  })
}

