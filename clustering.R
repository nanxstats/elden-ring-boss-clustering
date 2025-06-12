# Read and preprocess data ----
df <- read.csv("data/elden-ring-boss-stats-ng-1.10.csv", header = TRUE)

# Clean up row (boss) names
df <- df[, apply(df, 2L, function(x) !all(is.na(x)))]
df <- df[grepl("\\[Boss\\]", x = df$Name), ]

name <- gsub(" \\[Boss\\]", replacement = "", df$Name)
location <- gsub(" - .*", "", df$Location)
boss_name <- paste(name, location, sep = " | ") |>
  make.names(unique = TRUE) |>
  gsub("\\.\\.\\.\\.", replacement = " - ", x = _) |>
  gsub("\\.\\.\\.", replacement = " - ", x = _) |>
  gsub("\\.\\.", replacement = " ", x = _) |>
  gsub("\\.", replacement = " ", x = _)
rownames(df) <- boss_name

df <- df[, !(names(df) %in% c("Location", "Name", "ID"))]

# Encode "Immune" values as ordered factor
factorize_order <- function(x) {
  factor(
    x,
    ordered = TRUE,
    levels = stringr::str_sort(
      unique(x),
      numeric = TRUE
    )
  )
}

is_pure_numeric <- function(x) !any(is.na(suppressWarnings(as.numeric(x))))

idx_number <- apply(df, 2, is_pure_numeric)
idx_factor <- !idx_number
var_number <- names(df)[idx_number]
var_factor <- names(df)[idx_factor]
for (i in var_number) df[, i] <- as.numeric(df[, i])
for (i in var_factor) df[, i] <- factorize_order(df[, i])

skimr::skim(df)

# Fit model ----
set.seed(42)
fit <- randomForest::randomForest(x = df, ntree = 50000)
ragg::agg_png("variable-importance.png", width = 2560, height = 1920, res = 220, scaling = 1.2)
varImpPlot(fit, main = "Variable importance")
dev.off()
p <- fit$proximity
d <- as.dist(1 - p)

# Use HDBSCAN for clustering
cl <- dbscan::hdbscan(d, minPts = 2)
hc <- cl$hc
hc$labels <- boss_name

# Plot dendrogram ----
source("lib/ggdendro-lite.R")
source("lib/ggsci-adaptive.R")

k <- 60
hcdata <- dendro_data_k(hc, k = k)

pdf(
  "dendrogram.pdf",
  title = "Elden Ring boss clustering",
  height = 10, width = 10
)
plot_ggdendro(
  hcdata,
  direction = "lr",
  label.size = 1,
  branch.size = 0.3,
  expand.y = 0.2
) +
  scale_color_adaptive("observable", "observable10") +
  ggplot2::theme_void()
dev.off()

# Generate HTML ordered list with colored labels ----
labels <- hcdata$labels
labels <- labels[sort(labels$x, decreasing = TRUE), ]
labels$color <- pal_adaptive("observable", "observable10")(k)[labels$clust]

html_list <- paste0(
  "<ol>",
  paste0(
    "<li style='color:", labels$color, "'>",
    "<span style='color:", labels$color, "'>",
    labels$label,
    "</span>",
    "</li>",
    collapse = ""
  ),
  "</ol>"
)

htmltools::browsable((htmltools::HTML(html_list)))
