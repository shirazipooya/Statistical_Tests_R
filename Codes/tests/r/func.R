
plotDistStat <- function(distribution,
                         statistic,
                         alpha,
                         alternative,
                         from = NULL,
                         to = NULL,
                         # The Normal Distribution
                         mean = 0,
                         sd = 1,
                         # The Student t Distribution
                         df = NULL,
                         # The F Distribution
                         df1 = NULL,
                         df2 = NULL,
                         # The Chi-Squared Distribution
                         ncp = 0,
                         # Distribution of the Wilcoxon Rank Sum Statistic
                         m = NULL,
                         n = NULL
                         ) {

# The Normal Distribution -------------------------------------------------

  if (distribution == "norm") {
    
    # Compute The P-value
    p_value = pnorm(q = statistic, mean = mean, sd = sd, lower.tail = TRUE)
    p_value = switch(EXPR = alternative,
                     two.sided = 2 * min(p_value, 1 - p_value),
                     less = p_value,
                     greater = 1 - p_value)
    
    if (alternative == "two.sided") {
      
      # Find Upper and Lower Values
      lowerValue <- qnorm(p = (alpha / 2), mean = mean, sd = sd, lower.tail = TRUE)
      upperValue <- qnorm(p = (alpha / 2), mean = mean, sd = sd, lower.tail = FALSE)
      
      # Find from and to Values
      if (is.null(x = from)) {
        if (abs(x = statistic) < 4) {
          from = -4
          to = 4
        } else{
          from = ceiling(x = abs(x = statistic)) * -1
          to = abs(x = from)
        }
      }
      
      # Create Vector of x Values
      x_lower <- seq(from = from, to = lowerValue, by = 0.001)
      x_upper <- seq(from = upperValue, to = to, by = 0.001)
      
      # Create Vector of probabilities Values
      p_lower <- dnorm(x = x_lower, mean = mean, sd = sd)
      p_upper <- dnorm(x = x_upper, mean = mean, sd = sd)
      
      # Create Density Curve
      curve(expr = dnorm(x = x, mean = mean, sd = sd),
            from = from,
            to = to,
            xlab = "x",
            ylab = "Probability Density",
            lwd = 3,
            add = FALSE)
      
      # Fill in Portion of the Density Plot - from:x_lower
      polygon(x = c(x_lower, rev(x = x_lower)),
              y = c(p_lower, rep(x = 0, length(p_lower))),
              col = adjustcolor(col = 'red', alpha.f = 0.6),
              border = NA)
      
      # Fill in Portion of the Density Plot - x_upper:to
      polygon(x = c(x_upper, rev(x = x_upper)),
              y = c(p_upper, rep(0, length(p_upper))),
              col = adjustcolor(col = 'red', alpha.f = 0.6),
              border = NA)
      
      # Add Statistic Point
      points(x = statistic,
             y = dnorm(x = statistic, mean = mean, sd = sd),
             pch = 21,
             cex = 1.5,
             col = "black",
             bg = "deepskyblue")
      
      # Add Text
      mtext(text = paste0("Statistic = ",
                          round(x = statistic, digits = 4),
                          "\n",
                          "p-value = ",
                          round(x = p_value, digits = 4)), 
            side = 3)
      
    } else if (alternative == "less") {
      
      # Find Lower Values
      lowerValue <- qnorm(p = alpha, mean = mean, sd = sd, lower.tail = TRUE)

            # Find from and to Values
      if (is.null(x = from)) {
        if (abs(x = statistic) < 3) {
          from = -3
          to = 3
        } else{
          from = ceiling(x = abs(x = statistic)) * -1
          to = abs(x = from)
        }
      }
      
      # Create Vector of x Values
      x_lower <- seq(from = from, to = lowerValue, by = 0.001)

      # Create Vector of probabilities Values
      p_lower <- dnorm(x = x_lower, mean = mean, sd = sd)

      # Create Density Curve
      curve(expr = dnorm(x = x, mean = mean, sd = sd),
            from = from,
            to = to,
            xlab = "x",
            ylab = "Probability Density",
            lwd = 3,
            add = FALSE)
      
      # Fill in Portion of the Density Plot - from:x_lower
      polygon(x = c(x_lower, rev(x = x_lower)),
              y = c(p_lower, rep(x = 0, length(p_lower))),
              col = adjustcolor(col = 'red', alpha.f = 0.6),
              border = NA)

      # Add Statistic Point
      points(x = statistic,
             y = dnorm(x = statistic, mean = mean, sd = sd),
             pch = 21,
             cex = 1.5,
             col = "black",
             bg = "deepskyblue")
      
      # Add Text
      mtext(text = paste0("Statistic = ",
                          round(x = statistic, digits = 4),
                          "\n",
                          "p-value = ",
                          round(x = p_value, digits = 4)), 
            side = 3)
      
    } else if (alternative == "greater") {
      
      # Find Upper Values
      upperValue <- qnorm(p = alpha, mean = mean, sd = sd, lower.tail = FALSE)
      
      # Find from and to Values
      if (is.null(x = from)) {
        if (abs(x = statistic) < 3) {
          from = -3
          to = 3
        } else{
          from = ceiling(x = abs(x = statistic)) * -1
          to = abs(x = from)
        }
      }
      
      # Create Vector of x Values
      x_upper <- seq(from = upperValue, to = to, by = 0.001)
      
      # Create Vector of probabilities Values
      p_upper <- dnorm(x = x_upper, mean = mean, sd = sd)
      
      # Create Density Curve
      curve(expr = dnorm(x = x, mean = mean, sd = sd),
            from = from,
            to = to,
            xlab = "x",
            ylab = "Probability Density",
            lwd = 3,
            add = FALSE)

      # Fill in Portion of the Density Plot - x_upper:to
      polygon(x = c(x_upper, rev(x = x_upper)),
              y = c(p_upper, rep(0, length(p_upper))),
              col = adjustcolor(col = 'red', alpha.f = 0.6),
              border = NA)
      
      # Add Statistic Point
      points(x = statistic,
             y = dnorm(x = statistic, mean = mean, sd = sd),
             pch = 21,
             cex = 1.5,
             col = "black",
             bg = "deepskyblue")
      
      # Add Text
      mtext(text = paste0("Statistic = ",
                          round(x = statistic, digits = 4),
                          "\n",
                          "p-value = ",
                          round(x = p_value, digits = 4)), 
            side = 3)
      
    }
    
  }

# The Student t Distribution ----------------------------------------------

  if (distribution == "t") {
    
    # Compute The P-value
    p_value = pt(q = statistic, df = df, lower.tail = TRUE)
    p_value = switch(EXPR = alternative,
                     two.sided = 2 * min(p_value, 1 - p_value),
                     less = p_value,
                     greater = 1 - p_value)
    
    if (alternative == "two.sided") {
      
      # Find Upper and Lower Values
      lowerValue <- qt(p = (alpha / 2), df = df, lower.tail = TRUE)
      upperValue <- qt(p = (alpha / 2), df = df, lower.tail = FALSE)
      
      # Find from and to Values
      if (is.null(x = from)) {
        if (abs(x = statistic) < 4) {
          from = -4
          to = 4
        } else{
          from = ceiling(x = abs(x = statistic)) * -1
          to = abs(x = from)
        }
      }
      
      # Create Vector of x Values
      x_lower <- seq(from = from, to = lowerValue, by = 0.001)
      x_upper <- seq(from = upperValue, to = to, by = 0.001)
      
      # Create Vector of probabilities Values
      p_lower <- dt(x = x_lower, df = df)
      p_upper <- dt(x = x_upper, df = df)
      
      # Create Density Curve
      curve(expr = dt(x = x, df = df),
            from = from,
            to = to,
            xlab = "x",
            ylab = "Probability Density",
            lwd = 3,
            add = FALSE)
      
      # Fill in Portion of the Density Plot - from:x_lower
      polygon(x = c(x_lower, rev(x = x_lower)),
              y = c(p_lower, rep(x = 0, length(p_lower))),
              col = adjustcolor(col = 'red', alpha.f = 0.6),
              border = NA)
      
      # Fill in Portion of the Density Plot - x_upper:to
      polygon(x = c(x_upper, rev(x = x_upper)),
              y = c(p_upper, rep(0, length(p_upper))),
              col = adjustcolor(col = 'red', alpha.f = 0.6),
              border = NA)
      
      # Add Statistic Point
      points(x = statistic,
             y = dt(x = statistic, df = df),
             pch = 21,
             cex = 1.5,
             col = "black",
             bg = "deepskyblue")
      
      # Add Text
      mtext(text = paste0("Statistic = ",
                          round(x = statistic, digits = 4),
                          "\n",
                          "df = ",
                          round(x = df, digits = 1),
                          ", p-value = ",
                          round(x = p_value, digits = 4)), 
            side = 3)
      
    } else if (alternative == "less") {
      
      # Find Lower Values
      lowerValue <- qt(p = alpha, df = df, lower.tail = TRUE)
      
      # Find from and to Values
      if (is.null(x = from)) {
        if (abs(x = statistic) < 3) {
          from = -3
          to = 3
        } else{
          from = ceiling(x = abs(x = statistic)) * -1
          to = abs(x = from)
        }
      }
      
      # Create Vector of x Values
      x_lower <- seq(from = from, to = lowerValue, by = 0.001)
      
      # Create Vector of probabilities Values
      p_lower <- dt(x = x_lower, df = df)
      
      # Create Density Curve
      curve(expr = dt(x = x, df = df),
            from = from,
            to = to,
            xlab = "x",
            ylab = "Probability Density",
            lwd = 3,
            add = FALSE)
      
      # Fill in Portion of the Density Plot - from:x_lower
      polygon(x = c(x_lower, rev(x = x_lower)),
              y = c(p_lower, rep(x = 0, length(p_lower))),
              col = adjustcolor(col = 'red', alpha.f = 0.6),
              border = NA)
      
      # Add Statistic Point
      points(x = statistic,
             y = dt(x = statistic, df = df),
             pch = 21,
             cex = 1.5,
             col = "black",
             bg = "deepskyblue")
      
      # Add Text
      mtext(text = paste0("Statistic = ",
                          round(x = statistic, digits = 4),
                          "\n",
                          "df = ",
                          round(x = df, digits = 1),
                          ", p-value = ",
                          round(x = p_value, digits = 4)), 
            side = 3)
      
    } else if (alternative == "greater") {
      
      # Find Upper Values
      upperValue <- qt(p = alpha, df = df, lower.tail = FALSE)
      
      # Find from and to Values
      if (is.null(x = from)) {
        if (abs(x = statistic) < 3) {
          from = -3
          to = 3
        } else{
          from = ceiling(x = abs(x = statistic)) * -1
          to = abs(x = from)
        }
      }
      
      # Create Vector of x Values
      x_upper <- seq(from = upperValue, to = to, by = 0.001)
      
      # Create Vector of probabilities Values
      p_upper <- dt(x = x_upper, df = df)
      
      # Create Density Curve
      curve(expr = dt(x = x, df = df),
            from = from,
            to = to,
            xlab = "x",
            ylab = "Probability Density",
            lwd = 3,
            add = FALSE)
      
      # Fill in Portion of the Density Plot - x_upper:to
      polygon(x = c(x_upper, rev(x = x_upper)),
              y = c(p_upper, rep(0, length(p_upper))),
              col = adjustcolor(col = 'red', alpha.f = 0.6),
              border = NA)
      
      # Add Statistic Point
      points(x = statistic,
             y = dt(x = statistic, df = df),
             pch = 21,
             cex = 1.5,
             col = "black",
             bg = "deepskyblue")
      
      # Add Text
      mtext(text = paste0("Statistic = ",
                          round(x = statistic, digits = 4),
                          "\n",
                          "df = ",
                          round(x = df, digits = 1),
                          ", p-value = ",
                          round(x = p_value, digits = 4)), 
            side = 3)
      
    }
    
  }

# The F Distribution ------------------------------------------------------

  if (distribution == "f") {
    
    # Compute The P-value
    p_value = pf(q = statistic, df1 = df1, df2 = df2, lower.tail = TRUE)
    p_value = switch(EXPR = alternative,
                     two.sided = 2 * min(p_value, 1 - p_value),
                     less = p_value,
                     greater = 1 - p_value)
    
    if (alternative == "two.sided") {
      
      # Find Upper and Lower Values
      lowerValue <- qf(p = (alpha / 2), df1 = df1, df2 = df2, lower.tail = TRUE)
      upperValue <- qf(p = (alpha / 2), df1 = df1, df2 = df2, lower.tail = FALSE)
      
      # Find from and to Values
      if (is.null(x = from) & is.null(x = to)) {
        if (statistic < upperValue) {
          from = 0
          to = ceiling(x = upperValue) + 2
        } else{
          from = 0
          to = ceiling(x = statistic) + 2
        }
      }
      
      # Create Vector of x Values
      x_lower <- seq(from = from, to = lowerValue, by = 0.001)
      x_upper <- seq(from = upperValue, to = to, by = 0.001)
      
      # Create Vector of probabilities Values
      p_lower <- df(x = x_lower, df1 = df1, df2 = df2)
      p_upper <- df(x = x_upper, df1 = df1, df2 = df2)
      
      # Create Density Curve
      curve(expr = df(x = x, df1 = df1, df2 = df2),
            from = from,
            to = to,
            xlab = "x",
            ylab = "Probability Density",
            lwd = 3,
            add = FALSE)
      
      # Fill in Portion of the Density Plot - from:x_lower
      polygon(x = c(x_lower, rev(x = x_lower)),
              y = c(p_lower, rep(x = 0, length(p_lower))),
              col = adjustcolor(col = 'red', alpha.f = 0.6),
              border = NA)
      
      # Fill in Portion of the Density Plot - x_upper:to
      polygon(x = c(x_upper, rev(x = x_upper)),
              y = c(p_upper, rep(0, length(p_upper))),
              col = adjustcolor(col = 'red', alpha.f = 0.6),
              border = NA)
      
      # Add Statistic Point
      points(x = statistic,
             y = df(x = statistic, df1 = df1, df2 = df2),
             pch = 21,
             cex = 1.5,
             col = "black",
             bg = "deepskyblue")
      
      # Add Text
      mtext(text = paste0("Statistic = ",
                          round(x = statistic, digits = 4),
                          "\n",
                          "df1 = ",
                          round(x = df1, digits = 1),
                          ", df2 = ",
                          round(x = df2, digits = 1),
                          ", p-value = ",
                          round(x = p_value, digits = 4)), 
            side = 3)
      
    } else if (alternative == "less") {
      
      # Find Lower Values
      lowerValue <- qf(p = alpha, df1 = df1, df2 = df2, lower.tail = TRUE)
      
      # Find from and to Values
      if (is.null(x = from) & is.null(x = to)) {
        if (statistic < upperValue) {
          from = 0
          to = ceiling(x = upperValue) + 2
        } else{
          from = 0
          to = ceiling(x = statistic) + 2
        }
      }
      
      # Create Vector of x Values
      x_lower <- seq(from = from, to = lowerValue, by = 0.001)
      
      # Create Vector of probabilities Values
      p_lower <- df(x = x_lower, df1 = df1, df2 = df2)
      
      # Create Density Curve
      curve(expr = df(x = x, df1 = df1, df2 = df2),
            from = from,
            to = to,
            xlab = "x",
            ylab = "Probability Density",
            lwd = 3,
            add = FALSE)
      
      # Fill in Portion of the Density Plot - from:x_lower
      polygon(x = c(x_lower, rev(x = x_lower)),
              y = c(p_lower, rep(x = 0, length(p_lower))),
              col = adjustcolor(col = 'red', alpha.f = 0.6),
              border = NA)
      
      # Add Statistic Point
      points(x = statistic,
             y = df(x = statistic, df1 = df1, df2 = df2),
             pch = 21,
             cex = 1.5,
             col = "black",
             bg = "deepskyblue")
      
      # Add Text
      mtext(text = paste0("Statistic = ",
                          round(x = statistic, digits = 4),
                          "\n",
                          "df1 = ",
                          round(x = df1, digits = 1),
                          ", df2 = ",
                          round(x = df2, digits = 1),
                          ", p-value = ",
                          round(x = p_value, digits = 4)), 
            side = 3)
      
    } else if (alternative == "greater") {
      
      # Find Upper Values
      upperValue <- qf(p = alpha, df1 = df1, df2 = df2, lower.tail = FALSE)
      
      # Find from and to Values
      if (is.null(x = from) & is.null(x = to)) {
        if (statistic < upperValue) {
          from = 0
          to = ceiling(x = upperValue) + 2
        } else{
          from = 0
          to = ceiling(x = statistic) + 2
        }
      }
      
      # Create Vector of x Values
      x_upper <- seq(from = upperValue, to = to, by = 0.001)
      
      # Create Vector of probabilities Values
      p_upper <- df(x = x_upper, df1 = df1, df2 = df2)
      
      # Create Density Curve
      curve(expr = df(x = x, df1 = df1, df2 = df2),
            from = from,
            to = to,
            xlab = "x",
            ylab = "Probability Density",
            lwd = 3,
            add = FALSE)
      
      # Fill in Portion of the Density Plot - x_upper:to
      polygon(x = c(x_upper, rev(x = x_upper)),
              y = c(p_upper, rep(0, length(p_upper))),
              col = adjustcolor(col = 'red', alpha.f = 0.6),
              border = NA)
      
      # Add Statistic Point
      points(x = statistic,
             y = df(x = statistic, df1 = df1, df2 = df2),
             pch = 21,
             cex = 1.5,
             col = "black",
             bg = "deepskyblue")
      
      # Add Text
      mtext(text = paste0("Statistic = ",
                          round(x = statistic, digits = 4),
                          "\n",
                          "df1 = ",
                          round(x = df1, digits = 1),
                          ", df2 = ",
                          round(x = df2, digits = 1),
                          ", p-value = ",
                          round(x = p_value, digits = 4)), 
            side = 3)
      
    }
    
  }

# The (non-central) Chi-Squared Distribution ------------------------------

  if (distribution == "chisq") {
    
    # Compute The P-value
    p_value = pchisq(q = statistic, df = df, ncp = ncp, lower.tail = TRUE)
    p_value = switch(EXPR = alternative,
                     two.sided = 2 * min(p_value, 1 - p_value),
                     less = p_value,
                     greater = 1 - p_value)
    
    if (alternative == "two.sided") {
      
      # Find Upper and Lower Values
      lowerValue <- qchisq(p = (alpha / 2), df = df, ncp = ncp, lower.tail = TRUE)
      upperValue <- qchisq(p = (alpha / 2), df = df, ncp = ncp, lower.tail = FALSE)
      
      # Find from and to Values
      if (is.null(x = from) & is.null(x = to)) {
        if (statistic < upperValue) {
          from = 0
          to = ceiling(x = upperValue) + 2
        } else{
          from = 0
          to = ceiling(x = statistic) + ceiling(x = upperValue / 2)
        }
      }
      
      # Create Vector of x Values
      x_lower <- seq(from = from, to = lowerValue, by = 0.001)
      x_upper <- seq(from = upperValue, to = to, by = 0.001)
      
      # Create Vector of probabilities Values
      p_lower <- dchisq(x = x_lower, df = df, ncp = ncp)
      p_upper <- dchisq(x = x_upper, df = df, ncp = ncp)
      
      # Create Density Curve
      curve(expr = dchisq(x = x, df = df, ncp = ncp),
            from = from,
            to = to,
            xlab = "x",
            ylab = "Probability Density",
            lwd = 3,
            add = FALSE)
      
      # Fill in Portion of the Density Plot - from:x_lower
      polygon(x = c(x_lower, rev(x = x_lower)),
              y = c(p_lower, rep(x = 0, length(p_lower))),
              col = adjustcolor(col = 'red', alpha.f = 0.6),
              border = NA)
      
      # Fill in Portion of the Density Plot - x_upper:to
      polygon(x = c(x_upper, rev(x = x_upper)),
              y = c(p_upper, rep(0, length(p_upper))),
              col = adjustcolor(col = 'red', alpha.f = 0.6),
              border = NA)
      
      # Add Statistic Point
      points(x = statistic,
             y = dchisq(x = statistic, df = df, ncp = ncp),
             pch = 21,
             cex = 1.5,
             col = "black",
             bg = "deepskyblue")
      
      # Add Text
      mtext(text = paste0("Statistic = ",
                          round(x = statistic, digits = 4),
                          "\n",
                          "df = ",
                          round(x = df, digits = 1),
                          ", p-value = ",
                          round(x = p_value, digits = 4)), 
            side = 3)
      
    } else if (alternative == "less") {
      
      # Find Lower Values
      lowerValue <- qchisq(p = alpha, df = df, ncp = ncp, lower.tail = TRUE)
      
      # Find from and to Values
      if (is.null(x = from) & is.null(x = to)) {
        if (statistic < upperValue) {
          from = 0
          to = ceiling(x = upperValue) + 2
        } else{
          from = 0
          to = ceiling(x = statistic) + ceiling(x = upperValue / 2)
        }
      }
      
      # Create Vector of x Values
      x_lower <- seq(from = from, to = lowerValue, by = 0.001)
      
      # Create Vector of probabilities Values
      p_lower <- dchisq(x = x_lower, df = df, ncp = ncp)
      
      # Create Density Curve
      curve(expr = dchisq(x = x, df = df, ncp = ncp),
            from = from,
            to = to,
            xlab = "x",
            ylab = "Probability Density",
            lwd = 3,
            add = FALSE)
      
      # Fill in Portion of the Density Plot - from:x_lower
      polygon(x = c(x_lower, rev(x = x_lower)),
              y = c(p_lower, rep(x = 0, length(p_lower))),
              col = adjustcolor(col = 'red', alpha.f = 0.6),
              border = NA)
      
      # Add Statistic Point
      points(x = statistic,
             y = dchisq(x = statistic, df = df, ncp = ncp),
             pch = 21,
             cex = 1.5,
             col = "black",
             bg = "deepskyblue")
      
      # Add Text
      mtext(text = paste0("Statistic = ",
                          round(x = statistic, digits = 4),
                          "\n",
                          "df = ",
                          round(x = df, digits = 1),
                          ", p-value = ",
                          round(x = p_value, digits = 4)), 
            side = 3)
      
    } else if (alternative == "greater") {
      
      # Find Upper Values
      upperValue <- qchisq(p = alpha, df = df, ncp = ncp, lower.tail = FALSE)
      
      # Find from and to Values
      if (is.null(x = from) & is.null(x = to)) {
        if (statistic < upperValue) {
          from = 0
          to = ceiling(x = upperValue) + 2
        } else{
          from = 0
          to = ceiling(x = statistic) + ceiling(x = upperValue / 2)
        }
      }
      
      # Create Vector of x Values
      x_upper <- seq(from = upperValue, to = to, by = 0.001)
      
      # Create Vector of probabilities Values
      p_upper <- dchisq(x = x_upper, df = df, ncp = ncp)
      
      # Create Density Curve
      curve(expr = dchisq(x = x, df = df, ncp = ncp),
            from = from,
            to = to,
            xlab = "x",
            ylab = "Probability Density",
            lwd = 3,
            add = FALSE)
      
      # Fill in Portion of the Density Plot - x_upper:to
      polygon(x = c(x_upper, rev(x = x_upper)),
              y = c(p_upper, rep(0, length(p_upper))),
              col = adjustcolor(col = 'red', alpha.f = 0.6),
              border = NA)
      
      # Add Statistic Point
      points(x = statistic,
             y = dchisq(x = statistic, df = df, ncp = ncp),
             pch = 21,
             cex = 1.5,
             col = "black",
             bg = "deepskyblue")
      
      # Add Text
      mtext(text = paste0("Statistic = ",
                          round(x = statistic, digits = 4),
                          "\n",
                          "df = ",
                          round(x = df, digits = 1),
                          ", p-value = ",
                          round(x = p_value, digits = 4)), 
            side = 3)
      
    }
    
  }

# Distribution of the Wilcoxon Rank Sum Statistic -------------------------

  if (distribution == "wilcox") {
    
    # Compute The P-value
    p_value = pwilcox(q = statistic, m = m, n = n, lower.tail = TRUE)
    p_value = switch(EXPR = alternative,
                     two.sided = 2 * min(p_value, 1 - p_value),
                     less = p_value,
                     greater = 1 - p_value)
    
    if (alternative == "two.sided") {
      
      # Find Upper and Lower Values
      lowerValue <- qwilcox(p = (alpha / 2), m = m, n = n, lower.tail = TRUE)
      upperValue <- qwilcox(p = (alpha / 2), m = m, n = n, lower.tail = FALSE)
      
      # # Find from and to Values
      # if (is.null(x = from) & is.null(x = to)) {
      #   if (statistic > upperValue) {
      #     from = (lowerValue%/%10)
      #     print(from)
      #     to = upperValue
      #     print(to)
      #   } else if (statistic < lowerValue) {
      #     from = floor(x = lowerValue) - 7
      #     to = ceiling(x = statistic) + 7
      #   } else {
      #     from = floor(x = statistic)
      #     to = ceiling(x = upperValue + ((upperValue + lowerValue) / 2))
      #   }
      # }
      
      # Create Vector of x Values
      x_lower <- seq(from = from, to = lowerValue, by = 1)
      x_upper <- seq(from = upperValue, to = to, by = 1)
      
      # Create Vector of probabilities Values
      p_lower <- dwilcox(x = x_lower, m = m, n = n)
      p_upper <- dwilcox(x = x_upper, m = m, n = n)
      
      # Create Density Curve
      curve(expr = dwilcox(x = x, m = m, n = n),
            from = from,
            to = to,
            xlab = "x",
            ylab = "Probability Density",
            lwd = 3,
            add = FALSE)
      
      # Fill in Portion of the Density Plot - from:x_lower
      polygon(x = c(x_lower, rev(x = x_lower)),
              y = c(p_lower, rep(x = 0, length(p_lower))),
              col = adjustcolor(col = 'red', alpha.f = 0.6),
              border = NA)
      
      # Fill in Portion of the Density Plot - x_upper:to
      polygon(x = c(x_upper, rev(x = x_upper)),
              y = c(p_upper, rep(0, length(p_upper))),
              col = adjustcolor(col = 'red', alpha.f = 0.6),
              border = NA)
      
      # Add Statistic Point
      points(x = statistic,
             y = dwilcox(x = statistic, m = m, n = n),
             pch = 21,
             cex = 1.5,
             col = "black",
             bg = "deepskyblue")
      
      # Add Text
      mtext(text = paste0("Statistic = ",
                          round(x = statistic, digits = 4),
                          "\n",
                          "m = ",
                          m,
                          ", n = ",
                          n,
                          ", p-value = ",
                          round(x = p_value, digits = 4)), 
            side = 3)
      
    } else if (alternative == "less") {
      
      # Find Lower Values
      lowerValue <- qwilcox(p = alpha, m = m, n = n, lower.tail = TRUE)
      
      # # Find from and to Values
      # if (is.null(x = from) & is.null(x = to)) {
      #   if (statistic < upperValue & statistic > lowerValue) {
      #     from = floor(x = lowerValue) - 7
      #     to = ceiling(x = upperValue) + 7
      #   } else if (statistic >= upperValue) {
      #     from = floor(x = lowerValue) - 7
      #     to = ceiling(x = statistic) + 7
      #   } else if (statistic <= lowerValue) {
      #     from = floor(x = statistic) - 7
      #     to = ceiling(x = upperValue) + 7
      #   }
      # }
      
      # Create Vector of x Values
      x_lower <- seq(from = from, to = lowerValue, by = 1)
      
      # Create Vector of probabilities Values
      p_lower <- dwilcox(x = x_lower, m = m, n = n)
      
      # Create Density Curve
      curve(expr = dwilcox(x = x, m = m, n = n),
            from = from,
            to = to,
            xlab = "x",
            ylab = "Probability Density",
            lwd = 3,
            add = FALSE)
      
      # Fill in Portion of the Density Plot - from:x_lower
      polygon(x = c(x_lower, rev(x = x_lower)),
              y = c(p_lower, rep(x = 0, length(p_lower))),
              col = adjustcolor(col = 'red', alpha.f = 0.6),
              border = NA)
      
      # Add Statistic Point
      points(x = statistic,
             y = dwilcox(x = statistic, m = m, n = n),
             pch = 21,
             cex = 1.5,
             col = "black",
             bg = "deepskyblue")
      
      # Add Text
      mtext(text = paste0("Statistic = ",
                          round(x = statistic, digits = 4),
                          "\n",
                          "m = ",
                          m,
                          ", n = ",
                          n,
                          ", p-value = ",
                          round(x = p_value, digits = 4)), 
            side = 3)
      
    } else if (alternative == "greater") {
      
      # Find Upper Values
      upperValue <- qwilcox(p = alpha, m = m, n = n, lower.tail = FALSE)
      
      # # Find from and to Values
      # if (is.null(x = from) & is.null(x = to)) {
      #   if (statistic < upperValue & statistic > lowerValue) {
      #     from = floor(x = lowerValue) - 7
      #     to = ceiling(x = upperValue) + 7
      #   } else if (statistic >= upperValue) {
      #     from = floor(x = lowerValue) - 7
      #     to = ceiling(x = statistic) + 7
      #   } else if (statistic <= lowerValue) {
      #     from = floor(x = statistic) - 7
      #     to = ceiling(x = upperValue) + 7
      #   }
      # }
      
      # Create Vector of x Values
      x_upper <- seq(from = upperValue, to = to, by = 1)
      
      # Create Vector of probabilities Values
      p_upper <- dwilcox(x = x_upper, m = m, n = n)
      
      # Create Density Curve
      curve(expr = dwilcox(x = x, m = m, n = n),
            from = from,
            to = to,
            xlab = "x",
            ylab = "Probability Density",
            lwd = 3,
            add = FALSE)
      
      # Fill in Portion of the Density Plot - x_upper:to
      polygon(x = c(x_upper, rev(x = x_upper)),
              y = c(p_upper, rep(0, length(p_upper))),
              col = adjustcolor(col = 'red', alpha.f = 0.6),
              border = NA)
      
      # Add Statistic Point
      points(x = statistic,
             y = dwilcox(x = statistic, m = m, n = n),
             pch = 21,
             cex = 1.5,
             col = "black",
             bg = "deepskyblue")
      
      # Add Text
      mtext(text = paste0("Statistic = ",
                          round(x = statistic, digits = 4),
                          "\n",
                          "m = ",
                          m,
                          ", n = ",
                          n,
                          ", p-value = ",
                          round(x = p_value, digits = 4)), 
            side = 3)
      
    }
    
  }
  
}




# Function to Extract the F-statistics and p-value from the lm Class.
extract_lm_F_p <- function(modelobject) {

  if (class(modelobject) != "lm"){
    stop("Not An Object Of Class 'lm'.")
  }

  f <- summary(modelobject)$fstatistic

  F_statistics <- f[1]

  p_value <- pf(q = f[1], df1 = f[2], df2 = f[3], lower.tail=F)
  attributes(p_value) <- NULL

  result <- data.frame(F_statistics = F_statistics,
                       p_value = p_value)

  return(result)
}






