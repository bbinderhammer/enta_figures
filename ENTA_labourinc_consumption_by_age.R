################################################################################
# Labour income and consumption by age with annotations
#
# This script draws an annotated graph for labour income (YL) and consumption (C)
# by single year of age. It downloads the ENTA dataset from Zenodo, finds where
# the two profiles cross, draws vertical lines at those ages, and
# annotates the life-cycle surplus and the childhood / old-age deficit.
#
# Annotations:
#   - Grey shaded area: childhood deficit (age 0 to first crossing)
#   - Grey shaded area: old-age deficit (last crossing to oldest age)
#   - Centre text: life-cycle surplus in 3 lines with rounded ages
#   - Left text: life-cycle deficit in childhood
#   - Right text: life-cycle deficit in old age in 2 lines
#   - Vertical lines at first and last crossing ages
#
# Sections
#   1. Parameters and packages
#   2. Download data from Zenodo
#   3. Interpolate crossing ages for one country
#   4. Draw the annotated graph
#
# In RStudio: Session > Set Working Directory > To Source File Location
################################################################################

################################################################################
# 1. Parameters and packages
################################################################################

# Install missing packages automatically
for (pkg in c("arrow", "ggplot2")) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
}
library(arrow)
library(ggplot2)

# Check ggplot2 version for legend.position = "inside"
if (packageVersion("ggplot2") < "3.5.0") {
  stop("Please update ggplot2: install.packages('ggplot2')")
}

# ENTA colour scheme: blue for labour income, red for consumption
enta_blue <- rgb(0, 85, 135, maxColorValue = 255)
enta_red  <- rgb(180, 50, 50, maxColorValue = 255)

# Parameters
country <- "AT"       # two-letter country code (e.g. "AT", "DE", "ES")
year    <- 2023
sex     <- "Total"    # "Men", "Women" or "Total"

country_names <- c(AT = "Austria", DE = "Germany", ES = "Spain", FR = "France",
                   IT = "Italy", PT = "Portugal", FI = "Finland", SE = "Sweden",
                   NL = "Netherlands", BE = "Belgium", GR = "Greece", CZ = "Czechia",
                   SK = "Slovakia", HU = "Hungary", RO = "Romania", BG = "Bulgaria",
                   HR = "Croatia", SI = "Slovenia", LT = "Lithuania", LV = "Latvia",
                   EE = "Estonia", IE = "Ireland", LU = "Luxembourg", CY = "Cyprus",
                   MT = "Malta")

options(timeout = 600)

message("Working directory: ", getwd())

################################################################################
# 2. Download data from Zenodo
################################################################################

# The ENTA dataset is published at https://doi.org/10.5281/zenodo.20360412.
# The Zenodo URL below points to record 22550976 (v2.0.0), which is the
# version associated with that concept DOI.

url  <- "https://zenodo.org/records/22550976/files/ENTA_nta.parquet?download=1"
path <- "data/ENTA_nta.parquet"

if (!file.exists(path)) {
  dir.create("data", showWarnings = FALSE, recursive = TRUE)
  message("Downloading ENTA data from Zenodo...")
  # Download straight to path; delete on failure.
  ok <- tryCatch(
    download.file(url, path, mode = "wb"),
    error = function(e) { file.remove(path); FALSE }
  )
  if (!is.logical(ok) || !ok) {
    stop("Download failed")
  }
  message("Data saved to: ", path)
}

# Read the parquet file and convert to a regular data frame
ENTA <- as.data.frame(arrow::read_parquet(path))

# Filter to the indicators we need before reshaping
ENTA <- ENTA[ENTA$IND %in% c("YL", "C"),
             c("CNT2", "YEAR", "AGE", "SEX", "IND", "VALUE")]

# Check that YL and C are present
if (!setequal(unique(ENTA$IND), c("YL", "C"))) {
  stop("YL and C must both be present in the ENTA data")
}

# The dataset now has one row per country, year, age, sex, indicator:
#   CNT2 : country code (e.g. "AT" = Austria)
#   YEAR : year (e.g. 2023)
#   AGE  : single year of age (0, 1, 2, ..., 80+)
#   SEX  : "Men", "Women", "Total"
#   IND  : "YL" (labour income) or "C" (consumption)
#   VALUE: nominal euro per capita

################################################################################
# 3. Interpolate crossing ages
################################################################################

# The dataset gives YL and C at integer ages, but the curves usually cross
# between two ages. We find the crossing point by linear interpolation.
#
#   crossing_age = age_low + (0 - surplus_low) / (surplus_high - surplus_low)
#
# where surplus = YL - C. This gives the exact age where surplus changes sign.

# Small helper: linear interpolation for one crossing point
crossing <- function(surplus, age) {
  lo <- surplus[-length(surplus)]
  hi <- surplus[-1]
  crosses <- (lo > 0 & hi <= 0) | (lo <= 0 & hi > 0)
  if (!any(crosses)) return(NA_real_)
  idx <- which(crosses)[1]
  age[idx] + (0 - lo[idx]) / (hi[idx] - lo[idx])
}

# Subset to the chosen country-year-sex
d_long <- ENTA[ENTA$CNT2 == country & ENTA$YEAR == year & ENTA$SEX == sex, ]

if (nrow(d_long) == 0) {
  yrs <- paste(sort(unique(ENTA$YEAR[ENTA$CNT2 == country])), collapse = ", ")
  cts <- paste(sort(unique(ENTA$CNT2)), collapse = ", ")
  stop("No data for ", country, " ", year, "\n  Available countries: ", cts,
       "\n  Available years for ", country, ": ", yrs)
}

# Reshape to wide: one column per indicator (YL, C)
d <- reshape(d_long, timevar = "IND", idvar = c("CNT2", "YEAR", "AGE", "SEX"),
             direction = "wide")
names(d) <- gsub("^VALUE\\.", "", names(d))

d <- d[order(d$AGE), ]

# Compute surplus and check for contiguous positive block
d$surplus <- d$YL - d$C
pos <- which(d$surplus > 0)
if (length(pos) == 0) {
  stop("No positive surplus for ", country, " ", sex, " ", year)
}
if (!all(diff(pos) == 1)) {
  i <- which(diff(pos) != 1)
  warning("Surplus dips below zero between ages ",
          d$AGE[pos[i] + 1], " and ",
          d$AGE[pos[i + 1] - 1],
          " for ", country, " ", sex, " ", year)
}

# Guard: crossing search at the edges
if (pos[1] == 1) {
  stop("Surplus starts at age 0 for ", country, " ", sex, " ", year,
       " -- cannot interpolate first crossing")
}
if (pos[length(pos)] == nrow(d)) {
  stop("Surplus ends at the oldest age for ", country, " ", sex, " ", year,
       " -- cannot interpolate last crossing")
}

# Linear interpolation for first and last crossing
first <- crossing(d$surplus, d$AGE)

after  <- d$surplus[pos[length(pos)] + 1]
last   <- d$AGE[pos[length(pos)]] + (0 - d$surplus[pos[length(pos)]]) /
                       (after - d$surplus[pos[length(pos)]])

# Compute n_years from rounded ages so the total matches the printed range
first_rounded <- round(first, 0)
last_rounded  <- round(last, 0)
n_years       <- last_rounded - first_rounded

################################################################################
# 4. Draw the annotated graph
################################################################################

max_age <- max(d_long$AGE)
max_val <- max(d_long$VALUE)

# Build the graph: d_long is in long format, aes(colour = IND) maps YL and C
p <- ggplot(d_long, aes(x = AGE, y = VALUE, colour = IND)) +

  # Grey shaded areas for deficit periods (clipped at 0-80)
  annotate("rect",
           xmin = 0, xmax = first, ymin = -Inf, ymax = Inf,
           fill = "grey70", alpha = 0.1) +
  annotate("rect",
           xmin = last, xmax = 80, ymin = -Inf, ymax = Inf,
           fill = "grey70", alpha = 0.1) +

  # Lines coloured by IND (YL = blue, C = red)
  geom_line(linewidth = 1.2) +

  ## Horizontal floor line at y=0 (replaces default grid)
  #geom_hline(yintercept = 0, colour = "grey80", linewidth = 0.3) +

  # Axes from age 0 to the oldest age, no padding, no clipping.
  coord_cartesian(xlim = c(0, max_age), ylim = c(0, 1.05 * max_val), expand = FALSE, clip="off") +
  scale_x_continuous(breaks = seq(0, 80, by = 10)) +

  # Vertical lines at the crossing ages
  geom_vline(xintercept = first, linetype = "solid", colour = "grey70", alpha = 0.1) +
  geom_vline(xintercept = last,  linetype = "solid", colour = "grey70", alpha = 0.1) +

  # Centre annotation: life-cycle surplus in 3 lines
  annotate("text",
           x = (first + last) / 2,
           y = 0.35 * max_val,
           label = paste0(n_years, " years of life-cycle surplus\n",
                          "from age ", first_rounded, " to ", last_rounded, "\n",
                          "(labour income > consumption)"),
           hjust = 0.5, vjust = 0.5, size = 5) +

  annotate("text",
           x = first / 2,
           y = 0.25 * max_val,
           label = "Life-cycle deficit\nin childhood",
           hjust = 0.5, vjust = 0.5, size = 5) +

  # Right annotation: life-cycle deficit in old age (2 lines)
  annotate("text",
           x = (last + max_age) / 1.9,
           y = 0.25 * max_val,
           label = "Life-cycle deficit\nin old age",
           hjust = 0.5, vjust = 0.5, size = 5) +

  # Map colours: blue for YL, red for C
  scale_colour_manual(
    breaks = c("YL", "C"),
    name   = NULL,
    labels = c(YL = "Labour income", C = "Consumption"),
    values = c(YL = enta_blue, C = enta_red)) +

  # Title and axis labels
  labs(title = paste0("Labour income and consumption, ", country_names[country], " ", year),
       x = "Age",
       y = "Euro per capita",
       caption = "Source: ENTA dataset v2.0.0, doi:10.5281/zenodo.20360412") +

  # Clean, light styling with legend inside the plot
  theme_minimal(base_size = 18) +
  theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        panel.grid.major.y = element_line(colour = "grey80", linewidth = 0.3),
        panel.grid.minor = element_blank(),
        legend.position = "inside",
        legend.position.inside = c(0.05, 0.95),
        legend.justification = c("left", "top"),
        legend.background = element_rect(fill = "white", colour = NA),
        axis.text.y   = element_text(hjust = 0.5, angle = 90, vjust = 0.5),
        plot.caption = element_text(hjust = 0, size = 11, colour = "grey50"),
        plot.margin = margin(10, 10, 5, 10))

# Show the graph on screen
print(p)

# Export the graph to PNG
outdir <- "figures_percap"
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
outpath <- file.path(outdir, paste0("yl_c_", country, "_", year, ".png"))
ggsave(filename = outpath,
       plot     = p, width = 12, height = 9, dpi = 300)

message("Graph saved to: ", normalizePath(outpath, mustWork = FALSE))
