# This code much cut down from code here:
# https://raw.githubusercontent.com/dieghernan/dieghernan.github.io/master/assets/functions/patternfun.R

checkpatternLayer <- function(x, mode, pattern) {
  if (length(sf::st_geometry(x)) == 0) {
    stop("No layer added, Check input object")
  }
  tolines = c("left2right", "right2left")
  
  if (!unique(sf::st_geometry_type(x)) %in% c("POLYGON", "MULTIPOLYGON")) {
    stop("Input should be  MULTIPOLYGON or POLYGON")
  }
  
  if (!mode %in% c("plot", "legend", "sfc")) {
    stop("mode should be  'plot' or 'sfc'")
  }
  
  
  if (!pattern %in% tolines) {
    stop(
      paste("Patterns available are",
            gsub(",",
                 ", ",
                 paste(tolines, sep = "", collapse = ",")
            ),
            sep = " "
      )
    )
  }
}

# B. Final functions - to be exported----
# patternLayer #
patternLayer = function(x, pattern = "left2right", ...) {
  dots <- list(...)
  options(warn = -1)
  # Assign default options #
  mode = ifelse(is.null(dots$mode), "plot", dots$mode)
  col = ifelse(is.null(dots$col), par()$col, dots$col)
  pch = ifelse(is.null(dots$pch), par()$pch, dots$pch)
  lty = ifelse(is.null(dots$lty), par()$lty, dots$lty)
  cex = ifelse(is.null(dots$cex), par()$cex, dots$cex)
  lwd = ifelse(is.null(dots$lwd), par()$lwd, dots$lwd)
  density = ifelse(is.null(dots$density), 1, dots$density)
  add = ifelse(is.null(dots$add), FALSE, dots$add)
  # End defaults #

  # Crop to device when adding the layer #
  if (mode != "legend" & add == TRUE) {
    devplot = par()$usr
    devplot <- devplot[c(1, 3, 2, 4)]
    class(devplot) <- "bbox"
    x = sf::st_crop(x, devplot)
  }
  # End crop #
  
  # Check inputs #
  checkpatternLayer(x, mode, pattern)
  # End check
  
  tolines = c("left2right","right2left")

  bb <- st_bbox(x)
  w <- diff(bb[c(1, 3)])
  h <- diff(bb[c(2, 4)])

  fillgrid = sf::st_make_grid(x, n = ceiling(c(w, h) / density))

  # Create patterns #
  ex = list( horizontal = c(1, 2),
             vertical = c(1, 4),
             left2right = c(2, 4),
             right2left = c(1, 3)
  )
  endsf = lapply(1:length(fillgrid), function(j)
    sf::st_linestring(
      sf::st_coordinates(
        fillgrid[j])[ex[[pattern]], 1:2]
    )
  )
  endsf = sf::st_sfc(endsf, crs = sf::st_crs(x)) 
  endsf = sf::st_intersection(endsf,x)
  endsf = endsf[sf::st_geometry_type(endsf)
                %in% c("LINESTRING", "MULTILINESTRING")
  ]
  endsf = sf::st_line_merge(sf::st_union(endsf))
  endsf = sf::st_intersection(endsf,x)
  endsf = endsf[sf::st_geometry_type(endsf) %in% c("LINESTRING", "MULTILINESTRING")]
  endsf = sf::st_line_merge(sf::st_union(endsf))
  # End patterns#
  options(warn = 0)
  
  #Outputs
  # Mode plot: plotting
  # Mode sfc: return object plotted
  # Mode legend: return object to plot on legend
  if (mode == "plot") {
    plot(
      sf::st_geometry(endsf),
      add = add,
      col = col,
      lwd = lwd,
      lty = lty
    )
  } else {
    return(endsf)
  }
}


legendPattern <- function(pos = c(0, 0), title.txt = "Title of the legend",
                          title.cex = 1, values.cex = 0.8,
                          categ, patterns, ptrn.bg = "white",
                          ptrn.text = "X", density = 1,
                          dot.cex = 0.5, text.cex = 0.65, cex = 1,
                          frame = FALSE, ...) {
  # Basic controls #
  tolines = c("left2right","right2left")
  
  if (!unique(patterns %in% tolines) ||
      length(unique(patterns %in% tolines)) > 1) {
    stop(
      paste("Patterns available are",
        gsub(",", ", ", 
        paste(tolines, sep = "", collapse = ",")), sep = " "))
  }
  
  # Store defaults #
  # Goal is to create a df with all the graphical params to be applied
  dots = list(...) #additional params
  ncat = length(categ)
  params = data.frame(categ = categ, stringsAsFactors = FALSE)
  params$pattern = rep(patterns, ncat)[1:ncat]
  params$legendfill = rep(ptrn.bg, ncat)[1:ncat]
  params$density = rep(density, ncat)[1:ncat]
  params$col = ifelse(rep(is.null(dots$col), ncat), par()$col, dots$col)

  # params forLines
  nlines = nrow(params[params$pattern %in% tolines, ])
  ltydef = ifelse(is.null(dots$lty), par()$lty, NA)
  if (!is.na(ltydef)) {
    ltytext = c("blank", "solid", "dashed", "dotted",
                "dotdash", "longdash", "twodash")
    ltytopar <- match(ltydef, ltytext) - 1
    ltytopar = rep(ltytopar, nlines)[1:nlines]
  } else {
    ltytopar = rep(dots$lty, nlines)[1:nlines]
  }
  auxlist = rep(NA, ncat)
  auxlist[params$pattern %in% tolines] <- ltytopar
  params$line.lty = auxlist
  lwd = ifelse(rep(is.null(dots$lwd), nlines), par()$lwd, dots$lwd)
  auxlist[params$pattern %in% tolines] <- lwd
  params$line.lwd = auxlist

  #Reversing table 
  params = params[nrow(params):1,]
  # End params table
  
  # figdim in geo coordinates
  x1 <- par()$usr[1]
  x2 <- par()$usr[2]
  y1 <- par()$usr[3]
  y2 <- par()$usr[4]
  
  # offsets
  delta1 <- xinch(0.15) * cex
  delta2 <- delta1 / 2
  
  # variables internes
  width <- (x2 - x1) / (30 / cex)
  height <- width / 1.5
  
  # xsize
  categ <- params$categ
  
  longVal <- categ[
    strwidth(categ, cex = values.cex) == max(strwidth(categ, cex = values.cex))
  ][1]
  longVal <- max(strwidth(c(longVal), cex = values.cex))
  legend_xsize <- max(width + longVal, 
                      strwidth(title.txt, cex = title.cex) - delta2) - delta2
  # ysize
  legend_ysize <-
    (length(categ)) * height + delta2 * (length(categ)) +
    strheight(title.txt, cex = title.cex) - delta2
  # Get legend position
  xref <- pos[1]
  yref <- pos[2]
  # Frame
  if (frame == TRUE) {
    rect(xref - delta1, yref - delta1,
         xref + legend_xsize + delta1 * 2,
         yref + legend_ysize + delta1 * 2,
         border = "black",
         col = "white"
    )
  }
  for (i in 0:(length(categ) - 1)) {
    j <- i + 1
    # Overlay pattern
    rect = c(xref,
             yref + i * height + i * delta2,
             xref + width,
             yref + height + i * height + i * delta2)
    
    class(rect) <- "bbox"
    rect = sf::st_as_sfc(rect)
    plot(sf::st_geometry(rect), col = params$legendfill[j],
      border = "black", lwd = 0.4, add = TRUE)
    patt = patternLayer(rect, pattern = params$pattern[j],
                        density = params$density[j], mode = "legend")
    plot(sf::st_geometry(patt), add = TRUE, col = params$col[j],
         lwd = as.double(params$line.lwd[j]), lty = as.integer(params$line.lty[j]))
    # Add border #
    plot(sf::st_geometry(rect), add = TRUE, 
         col = NA, border = "black", lwd = 0.4)
    # Label Legend
    text(x = xref + width + delta2, 
         y = yref + height / 2 + i * height + i * delta2,
         labels = params$categ[j], adj = c(0, 0.5), cex = values.cex)
  }

  # Affichage du titre
  text(x = xref, 
       y = yref + length(categ) * height + length(categ) * delta2 + delta2,
       labels = title.txt, adj = c(0, 0), cex = title.cex)
}