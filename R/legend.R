#' Add a color legend to a map
#'
#' When a color palette function is used in a map (e.g.
#' \code{\link{colorNumeric}}), a color legend can be automatically derived from
#' the palette function. You can also manually specify the colors and labels for
#' the legend.
#'
#' The \code{labFormat} argument is a function that takes the argument
#' \code{type = c("numeric", "bin", "quantile", "factor")}, plus, arguments for
#' different types of color palettes. For the \code{colorNumeric()} palette,
#' \code{labFormat} takes a single argument, which is the breaks of the numeric
#' vector, and returns a character vector of the same length. For
#' \code{colorBin()}, \code{labFormat} also takes a vector of breaks of length
#' \code{n} but should return a character vector of length \code{n - 1}, with
#' the \code{i}-th element representing the interval \code{c(x[i], x[i + 1])}.
#' For \code{colorQuantile}, \code{labFormat} takes two arguments, the quantiles
#' and the associated probabilities (each of length \code{n}), and should return
#' a character vector of length \code{n - 1} (similar to the \code{colorBin()}
#' palette). For \code{colorFactor()}, \code{labFormat} takes one argument, the
#' unique values of the factor, and should return a character vector of the same
#' length.
#'
#' By default, \code{labFormat} is basically \code{format(scientific = FALSE,
#' big.mark = ",")} for the numeric palette, \code{as.character()} for the
#' factor palette, and a function to return labels of the form \samp{x[i] - x[i
#' + 1]} for bin and quantile palettes (in the case of quantile palettes,
#' \code{x} is the probabilities instead of the values of breaks).
#' @inheritParams setView
#' @param position the position of the legend
#' @param pal the color palette function, generated from
#'   \code{\link{colorNumeric}()}, \code{colorBin()}, \code{colorQuantile()}, or
#'   \code{colorFactor()}
#' @param values the values used to generate colors from the palette function
#' @param na.label the legend label for \code{NA}s in \code{values}
#' @param bins an approximate number of tick-marks on the color gradient for the
#'   \code{colorNumeric} palette if it is of length one; you can also provide a
#'   numeric vector as the pre-defined breaks (equally spaced)
#' @param colors a vector of (HTML) colors to be used in the legend if
#'   \code{pal} is not provided
#' @param opacity the opacity of colors
#' @param labels a vector of text labels in the legend corresponding to
#'   \code{colors}
#' @param labFormat a function to format the labels derived from \code{pal} and
#'   \code{values} (see Details below to know what \code{labelFormat()} returns
#'   by default; you can either use the helper function \code{labelFormat()}, or
#'   write your own function)
#' @param title the legend title
#' @param className extra CSS classes to append to the control, space separated
#' @param layerId the ID of the legend; subsequent calls to \code{addLegend}
#'   or \code{addControl} with the same \code{layerId} will replace this
#'   legend. The ID can also be used with \code{removeControl}.
#' @param group \code{group} name of a leaflet layer group.
#'   Supplying this value will tie the legend to the leaflet layer group
#'   with this name and will auto add/remove the legend as the
#'   group is added/removed, for example via layerControl.
#'   You will need to set the \code{group} when you add a layer
#'   (e.g. \code{\link{addPolygons}}) and supply the same name here.
#' @template data-getMapData
#' @param orientation a string specifying the orientation of the legend. Default:
#'   "vertical".
#' @param width Specifies the legends width (the color-bar; not the overall box) in 'px'. If NULL it will be calculated according to the orientation and tick number. Default = NULL.
#' @param height Specifies the legends height (the color-bar; not the overall box) in 'px'. If NULL it will be calculated according to the orientation and tick number. Default = NULL.
#' @example inst/examples/legend.R
#' @export
addLegend <- function(
  map, position = c("topright", "bottomright", "bottomleft", "topleft"),
  pal, values, na.label = "NA", bins = 7, colors, opacity = 0.5, labels = NULL,
  labFormat = labelFormat(), title = NULL, className = "info legend",
  layerId = NULL, group = NULL, data = getMapData(map),
  orientation = c( "vertical", "horizontal" ), width = NULL, height = NULL
) {
  position <- match.arg(position)
  orientation <- match.arg(orientation)
  type <- "unknown"
  na_color <- NULL
  extra <- NULL  # only used for numeric palettes to store extra info

  if (!missing(pal)) {
    if (!missing(colors)) {
      stop("You must provide either 'pal' or 'colors' (not both)")
    }

    # a better default title when values is formula
    if (missing(title) && inherits(values, "formula")) {
      title <- deparse(values[[2]])
    }
    values <- evalFormula(values, data)

    generate_legend <- function(bins = bins) {
      type <- attr(pal, "colorType", exact = TRUE)
      args <- attr(pal, "colorArgs", exact = TRUE)
      na_color <- args$na.color
      # If args$na.color is transparent, don't show it on the legend
      if (!is.null(na_color) && col2rgb(na_color, alpha = TRUE)[[4]] == 0) {
        na_color <- NULL
      }

      if (type != "numeric" && !missing(bins)) {
        warning("'bins' is ignored because the palette type is not numeric")
      }

      if (type == "numeric") {

        # choose pretty cut points to draw tick-marks on the color gradient if
        # 'bins' is the number of bins, otherwise 'bins' is just the breaks
        cuts <- if (length(bins) == 1) pretty(values, n = bins) else bins
        if (length(bins) > 2) {
          if (!all(abs(diff(bins, differences = 2)) <= sqrt(.Machine$double.eps))) {
            stop("The vector of breaks 'bins' must be equally spaced")
          }
        }
        n <- length(cuts)
        r <- range(values, na.rm = TRUE)
        # pretty cut points may be out of the range of `values`
        cuts <- cuts[cuts >= r[1] & cuts <= r[2]]
        n <- length(cuts)
        p <- (cuts - r[1]) / (r[2] - r[1])  # percents relative to min(values)

        ## [    |       |       |  ...  |    ]
        ## min  p1      p2      p3 ...  pn   max
        ##  |   +   |   +   |   +  ...  +   |
        ## here |+| denotes a table row, and there are n rows

        # Since min and max may exceed the limits of the cut points, the client
        # needs to know the first and last cut points in order to place the tick
        # marks properly relative to the gradient.
        extra <- list(p_1 = p[1], p_n = p[n])
        # syntax for the color gradient: linear-gradient(start-color, color1 p1%,
        # color2 p2%, ..., colorn pn%, end-color])
        p <- c("", paste0(100 * p, "%"), "")
        colors <- pal(c(r[1], cuts, r[2]))
        colors <- paste(colors, p, sep = " ", collapse = ", ")
        labels <- labFormat(type = "numeric", cuts)

        ## Calculating the width and height of the color-bar
        ## taken from the original JS wrapper
        default_thickness <- 18 # [px]; default width/height
        ## If width/height is given (depending on the orientation)
        ## this variable will be calculated from them
        single_bin_length <- 20 # [px]; distance between the ticks
        single_bin_percentage <- (extra$p_n - extra$p_1) / (n - 1)
        if (orientation == "vertical") {
          if (is.null(height)) {
            height <- single_bin_length / single_bin_percentage + 1
          } else {
            single_bin_length <- height * single_bin_percentage - 1
          }
          if (is.null(width)) {
            width <- default_thickness
          }
        } else {
          if (is.null(height)) {
            height <- default_thickness
          }
          if (is.null(width)) {
            width <- single_bin_length / single_bin_percentage + 1
          } else {
            single_bin_length <- width * single_bin_percentage - 1
          }
        }
        ## calculating the tickOffset from the original JS wrapper
        ## via the extra$p_1, the total length and the single_bin_percentage
        if (orientation == "vertical") {
          tick_offset_beginning <- (height - 1 / single_bin_percentage) * extra$p_1
          tick_offset_end <- (height - 1 / single_bin_percentage) * (1 - extra$p_n)
        } else {
          tick_offset_beginning <- (width - 1 / single_bin_percentage) * extra$p_1
          tick_offset_end <- (width - 1 / single_bin_percentage) * (1 - extra$p_n)
        }

      } else if (type == "bin") {

        cuts <- args$bins
        n <- length(cuts)
        # use middle points to represent intervals
        mids <- (cuts[-1] + cuts[-n]) / 2
        colors <- pal(mids)
        labels <- labFormat(type = "bin", cuts)

        tick_offset_beginning <- tick_offset_end <- single_bin_length <- NULL

      } else if (type == "quantile") {

        p <- args$probs
        n <- length(p)
        # the "middle points" in this case are the middle probabilities
        cuts <- quantile(values, probs = p, na.rm = TRUE)
        mids <- quantile(values, probs = (p[-1] + p[-n]) / 2, na.rm = TRUE)
        colors <- pal(mids)
        labels <- labFormat(type = "quantile", cuts, p)

        tick_offset_beginning <- tick_offset_end <- single_bin_length <- NULL

      } else if (type == "factor") {

        v <- sort(unique(na.omit(values)))
        colors <- pal(v)
        labels <- labFormat(type = "factor", v)

        tick_offset_beginning <- tick_offset_end <- single_bin_length <- NULL

      } else stop("Palette function not supported")

      if (!any(is.na(values))) {
        na_color <- NULL
      }

      ## For convenience I will also provide the former singleBinHeight variable.
      ## It would just cause errors if defined at both this script and the wrapper.
      legend <- list(
        colors = I(unname(colors)), labels = I(unname(labels)),
        na_color = na_color, na_label = na.label, opacity = opacity,
        position = position, type = type, title = title, extra = extra,
        layerId = layerId, className = className, group = group,
        orientation = orientation, totalWidth = width, totalHeight = height,
        tickOffset = tick_offset_beginning, tickOffsetEnd = tick_offset_end,
        singleBinLength = single_bin_length
      )
      return(legend)
    }

    legend <- generate_legend(bins)

    if (orientation == "horizontal") {
      ## In case of the vertical orientation the labels can be whatever
      ## Now we have to check if the labels actually fit in the color-bar
      ## I will assign a default width of a character. (Via the inspector)
      ## With the two spaces in the collapse argument I took care of the
      ## spaces between the labels (which should be present)
      # character_width <- 4 # [px]
      character_width <- 7.784 # - Barret - I found it to be 7.784 px wide in the svg

      # calculate label width with two spaces inbetween labels
      # (using a single space buffer on each side where applicable)
      calculate_width <- function(legend) {
        labels_with_space <- legend$labels
        n_labels <- length(labels_with_space)
        if (length(labels_with_space) > 1) {
          # add space to right
          labels_with_space[1:(n_labels - 1)] <- paste0(
            labels_with_space[1:(n_labels - 1)], " "
          )
          # add space to left
          labels_with_space[2:n_labels] <- paste0(
            " ", labels_with_space[2:n_labels]
          )
        }
        label_char_groups <- strsplit(labels_with_space, ",|\\.")
        max_label_width <- max(
          vapply(label_char_groups, FUN.VALUE = numeric(1), function(char_groups) {
            # periods and commas were half the width
            periods_and_commas_len <- (length(char_groups) - 1) * character_width / 2
            char_len <- sum(vapply(char_groups, nchar, numeric(1))) * character_width
            char_len + periods_and_commas_len
          })
        )
        # pretend all labels are the max size
        labels_width <- max_label_width * n_labels
        if (legend$type == "numeric") {
          total_width <- labels_width + legend$tickOffset + legend$tickOffsetEnd
        } else {
          total_width <- labels_width
        }
        return(total_width)
      }

      ## reduce the number of bins until the labels fit below the color-bar
      while (calculate_width(legend) > legend$totalWidth) {
        ## It does not fit. So lets take less bins.
        bins <- bins - 1
        legend <- generate_legend(bins) # TODO needs to respect bin size
        if (bins == 1) {
          warning("No labels fitting below your leaflet legend could be found. Please set the 'width' parameter to add more space")
          break
        }
      }
    }

  } else {
    if (missing(labels) || missing(colors)) {
      stop("'colors' and 'labels' must be supplied when 'pal' if omitted!")
    }
    if (length(colors) != length(labels)) {
      stop("'colors' and 'labels' must be of the same length")
    }
    if (orientation == "horizontal") {
      warning("To use the horizontal orientation of the legend, please supply a palette.")
      orientation <- "vertical"
    }

    ## Heuristic width and height for supplied colors and corresponding labels
    singleBinLength <- 18 # size of colored square
    if (is.null(title)) {
      title_height <- title_width <- 0
    } else {
      title_height <- 18 # 16px character + 2px padding
      title_width <- nchar(title) * 8
    }
    ## width of colored bar + margin + label
    column_width <- singleBinLength + 8 + max(nchar(labels)) * 8
    ## height colored bins + title + padding
    if (is.null(height)) {
      height <- singleBinLength * length(colors) + title_width + 2 * 6
    }
    ## the widest element controls the width + padding
    if (is.null(width)) {
      width <- max(column_width, title_width) + 2 * 8
    }

    legend <- list(
      colors = I(unname(colors)), labels = I(unname(labels)),
      na_color = na_color, na_label = na.label, opacity = opacity,
      position = position, type = type, title = title, extra = extra,
      layerId = layerId, className = className, group = group,
      orientation = orientation, totalWidth = width, totalHeight = height,
      tickOffset = 0, tickOffsetEnd = 0,
      singleBinLength = singleBinLength
    )
  }

  invokeMethod(map, getMapData(map), "addLegend", legend)
}

#' @param prefix a prefix of legend labels
#' @param suffix a suffix of legend labels
#' @param between a separator between \code{x[i]} and \code{x[i + 1]} in legend
#'   labels (by default, it is a dash)
#' @param digits the number of digits of numeric values in labels
#' @param big.mark the thousand separator
#' @param transform a function to transform the label value
#' @rdname addLegend
#' @export
labelFormat <- function(
  prefix = "", suffix = "", between = " &ndash; ", digits = 3, big.mark = ",",
  transform = identity
) {

  formatNum <- function(x) {
    format(
      round(transform(x), digits), trim = TRUE, scientific = FALSE,
      big.mark = big.mark
    )
  }

  function(type, ...) {
    switch(
      type,
      numeric = (function(cuts) {
        paste0(prefix, formatNum(cuts), suffix)
      })(...), # nolint
      bin = (function(cuts) {
        n <- length(cuts)
        paste0(prefix, formatNum(cuts[-n]), between, formatNum(cuts[-1]), suffix)
      })(...), # nolint
      quantile = (function(cuts, p) {
        n <- length(cuts)
        p <- paste0(round(p * 100), "%")
        cuts <- paste0(formatNum(cuts[-n]), between, formatNum(cuts[-1]))
        # mouse over the legend labels to see the values (quantiles)
        paste0(
          "<span title=\"", cuts, "\">", prefix, p[-n], between, p[-1], suffix,
          "</span>"
        )
      })(...), # nolint
      factor = (function(cuts) {
        paste0(prefix, as.character(transform(cuts)), suffix)
      })(...) # nolint
    )
  }

}
