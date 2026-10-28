################################################################################
# Social benefits by type and age, with taxes paid (ENTA, per capita)
#
# Stacked bars: the public transfers (social benefits) a person receives on
# average at each age, by type. Line: the taxes and social contributions a
# person pays on average at each age. Values in nominal euro per capita.
#
# How to run it (no R knowledge needed):
#   1. Open enta_figures.Rproj in RStudio, then this file.
#   2. Choose country, year and sex in section 1.
#   3. Click "Source" (top right of the script window).
# The graph appears in the "Plots" window and is saved in figures/. The
# data are downloaded once into data/.
#
# The bars (ENTA indicator codes in brackets) add up to total public
# transfers received (TGI):
#   Pensions        public pensions, cash (TGICPEN)
#   Healthcare      public health and long-term care, in kind (CGH)
#   Education       public education, in kind (CGE)
#   Family          family benefits, cash (TGICF)
#   Unemployment    unemployment benefits, cash (TGICU)
#   Other           other cash benefits (TGICX) and other public
#                   consumption, e.g. defence, administration (CGX)
# The line: taxes and social contributions paid (TGOT).
# The oldest age, 80, is the open age group 80 and older.
#
# Data: ENTA dataset v2.0.0, https://doi.org/10.5281/zenodo.20360412
# (CC BY 4.0).
################################################################################

################################################################################
# 1. Settings (change here)
################################################################################

country <- "FR"      # two-letter country code, e.g. "AT", "DE", "EL", "SE"
year    <- 2023      # a year from 2008 to 2023
sex     <- "Total"   # "Total", "Men" or "Women"

# Folder for the PNG file, relative to the project folder. The graph is
# saved only if the folder exists; otherwise it is only shown. Or give
# another existing folder, e.g. outdir <- "C:/Users/me/Desktop".
outdir <- "figures"

################################################################################
# 2. Packages
################################################################################

# Install missing packages (only the first time)
for (pkg in c("arrow", "ggplot2")) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
}
library(ggplot2)
if (packageVersion("ggplot2") < "3.5.0") stop("Please update ggplot2: install.packages('ggplot2')")

################################################################################
# 3. Download (first run only) and read the ENTA data
################################################################################

url  <- "https://zenodo.org/records/22550976/files/ENTA_nta.parquet?download=1"
path <- file.path("data", "ENTA_nta.parquet")

if (!file.exists(path)) {
  dir.create(dirname(path), showWarnings = FALSE)
  options(timeout = 600)
  ok <- tryCatch(download.file(url, path, mode = "wb") == 0,
                 error = function(e) FALSE)
  if (!ok) {
    if (file.exists(path)) file.remove(path)
    stop("Download from Zenodo failed: ", url)
  }
}

# One row per country (CNT2), year, age, sex and indicator (IND);
# VALUE is nominal euro per capita
ENTA <- as.data.frame(arrow::read_parquet(path,
          col_select = c("CNT2", "COUNTRY", "YEAR", "AGE", "SEX", "IND", "VALUE")))

################################################################################
# 4. Select country, year and sex
################################################################################

d <- ENTA[ENTA$CNT2 == country & ENTA$YEAR == year & ENTA$SEX == sex, ]
if (nrow(d) == 0) {
  stop("No data for ", country, " ", year, " ", sex,
       "\n  Countries: ", paste(sort(unique(ENTA$CNT2)), collapse = ", "),
       "\n  Years for ", country, ": ",
       paste(sort(unique(ENTA$YEAR[ENTA$CNT2 == country])), collapse = ", "))
}

# Bars: the benefit types, with TGICX and CGX summed to "OTHER"
types <- c("TGICPEN", "CGH", "CGE", "TGICF", "TGICU", "OTHER")
bars  <- d[d$IND %in% c(types, "TGICX", "CGX"), ]
bars$IND[bars$IND %in% c("TGICX", "CGX")] <- "OTHER"
bars  <- aggregate(VALUE ~ AGE + IND, data = bars, FUN = sum)
bars$IND <- factor(bars$IND, levels = types)   # first type is on top

# Line: taxes and social contributions paid
taxes <- d[d$IND == "TGOT", ]

################################################################################
# 5. Draw and save the graph
################################################################################

labels  <- c(TGICPEN = "Pensions", CGH = "Healthcare", CGE = "Education",
             TGICF = "Family", TGICU = "Unemployment",
             OTHER = "Other")
colours <- c(TGICPEN = "#d7af19", CGH = "#e66e00", CGE = "#1e6e78",
             TGICF = "#b43232", TGICU = "grey65", OTHER = "grey35")

title <- paste0("Social benefits and government consumption by function and age , ",
                d$COUNTRY[1], " ", year,
                if (sex != "Total") paste0(" (", sex, ")"))

p <- ggplot() +
  geom_col(data = bars, aes(x = AGE, y = VALUE, fill = IND),
           width = 1, colour = "grey15", linewidth = 0.1) +
  geom_line(data = taxes, aes(x = AGE, y = VALUE,
                              linetype = "Taxes & social contributions"),
            linewidth = 1) +
  scale_fill_manual(values = colours, labels = labels, name = NULL) +
  scale_linetype_manual(values = "solid", name = NULL) +
  scale_x_continuous(breaks = seq(0, 80, 10),
                     labels = function(x) ifelse(x == 80, "80+", x),
                     expand = c(0, 0.6)) +
  scale_y_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE),
                     expand = expansion(mult = c(0, 0.05))) +
  labs(title = title, x = "Age", y = "Euro per capita",
       caption = "Source: ENTA dataset v2.0.0, doi:10.5281/zenodo.20360412.") +
  guides(fill = guide_legend(order = 1), linetype = guide_legend(order = 2)) +
  theme_minimal(base_size = 16) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        panel.grid.major.x = element_blank(),
        panel.grid.minor   = element_blank(),
        panel.grid.major.y = element_line(colour = "grey85", linewidth = 0.3),
        legend.position    = "inside",
        legend.position.inside = c(0.02, 0.98),
        legend.justification = c("left", "top"),
        legend.background  = element_rect(fill = "white", colour = "grey60"),
        legend.spacing.y   = unit(0, "pt"),
        legend.margin      = margin(4, 6, 4, 6),
        plot.caption = element_text(hjust = 0, size = 10, colour = "grey50"))

print(p)

# Save as PNG, only if the folder exists
if (dir.exists(outdir)) {
  file <- paste0("ENTA_gov_expenditure_function_age_", country, "_", year,
                 if (sex != "Total") paste0("_", sex), ".png")
  ggsave(file.path(outdir, file), p, width = 12, height = 7.5, dpi = 300, bg = "white")
  message("Graph saved to: ", normalizePath(file.path(outdir, file)))
} else {
  message("Graph not saved: folder ", outdir, " does not exist.",
          "\n  Open enta_figures.Rproj first, or set outdir in section 1.")
}
