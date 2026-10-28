################################################################################
# Government expenditure by function over time (ENTA, aggregates)
#
# Lines: public expenditure on pensions, healthcare, education, family and
# unemployment for one country, from 1995 to the latest year. Each line is
# labelled at its end.
#
# How to run it (no R knowledge needed):
#   1. Open enta_figures.Rproj in RStudio, then this file.
#   2. Choose country and unit in section 1.
#   3. Click "Source" (top right of the script window).
# The graph appears in the "Plots" window and is saved in figures/. The
# data are downloaded once into data/.
#
# The lines (ENTA indicator codes in brackets):
#   Pensions        social benefits in cash, pensions (BSPEN)
#   Healthcare      public consumption, health and long-term care (CGH)
#   Education       public consumption, education (CGE)
#   Family          social benefits in cash, family (BSFAM)
#   Unemployment    social benefits in cash, unemployment (BSU)
#
# Data: ENTA dataset v2.0.0, https://doi.org/10.5281/zenodo.20360412
# (CC BY 4.0): aggregates (nominal totals, million euro) and units
# (population, prices, exchange rates, GDP).
################################################################################

################################################################################
# 1. Settings (change here)
################################################################################

country <- "AT"       # two-letter country code, e.g. "AT", "DE", "EL", "SE"
unit    <- "realpc"   # "realpc"  = euro per capita, real (2020 prices)
                      # "gdp"     = percent of GDP
                      # "billion" = billion euro, nominal

# Folder for the PNG file, relative to the project folder. The graph is
# saved only if the folder exists; otherwise it is only shown. Or give
# another existing folder, e.g. outdir <- "C:/Users/me/Desktop".
outdir <- "figures"

################################################################################
# 2. Packages
################################################################################

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2", repos = "https://cloud.r-project.org")
}
library(ggplot2)

################################################################################
# 3. Download (first run only) and read the ENTA data
################################################################################

zenodo <- "https://zenodo.org/records/22550976/files/"
dir.create("data", showWarnings = FALSE)
options(timeout = 600)
for (f in c("ENTA_aggregates.csv", "ENTA_units.csv")) {
  path <- file.path("data", f)
  if (!file.exists(path)) {
    ok <- tryCatch(download.file(paste0(zenodo, f, "?download=1"), path, mode = "wb") == 0,
                   error = function(e) FALSE)
    if (!ok) {
      if (file.exists(path)) file.remove(path)
      stop("Download from Zenodo failed: ", f)
    }
  }
}

# Aggregates: one row per country (CNT2), year, indicator (IND) and sector;
# VALAGGR is the nominal total in million euro.
# Units: one row per country and year (population, HICP, exchange rates, GDP).
aggr  <- read.csv("data/ENTA_aggregates.csv", encoding = "UTF-8")
units <- read.csv("data/ENTA_units.csv", encoding = "UTF-8")

################################################################################
# 4. Select the country and compute the unit
################################################################################

# An indicator can appear in several sectors with the same value, so each is
# taken from one sector only
sectors <- c(BSPEN = "Households", BSFAM = "Households", BSU = "Households",
             CGH   = "Government", CGE   = "Government")
d <- aggr[aggr$CNT2 == country & aggr$IND %in% names(sectors), ]
d <- d[d$SECTOR == sectors[d$IND], ]
if (nrow(d) == 0) {
  stop("No data for ", country,
       "\n  Countries: ", paste(sort(unique(aggr$CNT2)), collapse = ", "))
}
d <- merge(d, units, by = c("CNT2", "YEAR"))

if (unit == "realpc") {
  # Per capita, deflated with the HICP (2020 = 100) in national currency and
  # converted back to euro at the 2020 exchange rate, as in the ENTA manual
  d$value <- d$VALAGGR * 1e6 / d$POPtot * d$EXR_NAC_EUR / d$HICP * 100 / d$EXR_2020
  ylab    <- "Euro per capita, real (2020 prices)"
} else if (unit == "gdp") {
  d$value <- d$VALAGGR / d$GDP * 100
  ylab    <- "Percent of GDP"
} else if (unit == "billion") {
  d$value <- d$VALAGGR / 1000
  ylab    <- "Billion euro, nominal"
} else {
  stop('unit must be "realpc", "gdp" or "billion"')
}
d <- d[!is.na(d$value), ]

################################################################################
# 5. Draw and save the graph
################################################################################

# ENTA colour scheme, one colour per function
labels  <- c(BSPEN = "Pensions", CGH = "Healthcare", CGE = "Education",
             BSFAM = "Family", BSU = "Unemployment")
colours <- c(BSPEN = "#d7af19", CGH = "#e66e00", CGE = "#1e6e78",
             BSFAM = "#b43232", BSU = "grey65")

# Labels at the end of each line instead of a legend. Labels that would
# overlap are moved up so that they are at least 'gap' apart.
ends <- d[d$YEAR == max(d$YEAR), ]
ends <- ends[order(ends$value), ]
gap  <- 0.045 * max(d$value)
ends$label_y <- cummax(ends$value - gap * seq_len(nrow(ends))) + gap * seq_len(nrow(ends))

first <- min(d$YEAR)
last  <- max(d$YEAR)

p <- ggplot(d, aes(x = YEAR, y = value, colour = IND)) +
  geom_line(linewidth = 1.2) +
  geom_text(data = ends, aes(x = YEAR + 0.6, y = label_y, label = labels[IND]),
            hjust = 0, size = 5.5, fontface = "bold") +
  scale_colour_manual(values = colours, guide = "none") +
  scale_x_continuous(breaks = seq(1995, last, 5), limits = c(first, last + 6.5),
                     expand = expansion(add = c(0.5, 0))) +
  scale_y_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE),
                     limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
  labs(title = paste0("Government expenditure by function, ", d$COUNTRY[1], " ",
                      first, "-", last),
       x = NULL, y = ylab,
       caption = "Source: ENTA dataset v2.0.0, doi:10.5281/zenodo.20360412.") +
  theme_minimal(base_size = 16) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        panel.grid.minor   = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey85", linewidth = 0.3),
        plot.caption = element_text(hjust = 0, size = 10, colour = "grey50"))

print(p)

# Save as PNG, only if the folder exists
if (dir.exists(outdir)) {
  file <- paste0("ENTA_gov_expenditure_function_aggr_", country, "_", unit, ".png")
  ggsave(file.path(outdir, file), p, width = 12, height = 7.5, dpi = 300, bg = "white")
  message("Graph saved to: ", normalizePath(file.path(outdir, file)))
} else {
  message("Graph not saved: folder ", outdir, " does not exist.",
          "\n  Open enta_figures.Rproj first, or set outdir in section 1.")
}
