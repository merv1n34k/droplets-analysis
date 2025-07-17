# =============================================================================
#  Micro-droplet analysis & plotting pipeline
#  -----------------------------------------
#  1.  Discover all *.csv files under a root directory
#  2.  Tidy them into one data frame
#  3.  Tag every row with membership in any number of user-defined groups
#  4.  Evaluate set expressions (union, intersection, difference) to pick
#      exactly the rows you want to compare
#  5.  Produce three ready-to-publish PNG collections per comparison
# =============================================================================

# -----------------------------------------------------------------------------
# 0 ── Libraries ---------------------------------------------------------------
#     (Install once via install.packages("<name>") if needed)
# -----------------------------------------------------------------------------
library(fs)            # file-system helpers
library(readr)         # read_csv(), parse_number()
library(stringr)       # str_detect(), str_split_fixed()
library(dplyr)         # %>%, filter(), mutate(), …
library(tidyr)         # pivot_longer(), pivot_wider()
library(purrr)         # map_dfr(), map(), …
library(lubridate)     # now()
library(ggplot2)       # graphics
library(patchwork)     # plot composition
library(colorspace)    # lighten()
library(ggridges)      # ridgeline variants (optional)
library(ggsci)         # pal_npg()
library(scales)        # comma(), hue_pal()
library(glue)          # glue()
library(ragg)          # agg_png() device
library(WRS2)          # robust ANOVA
library(moments)       # skewness(), kurtosis()

# =============================================================================
# 1 ── Colour helpers ----------------------------------------------------------
# =============================================================================

#' A manual palette that can be read from a CSV file (one HEX colour per line)
#'
#' @param file    Path to a CSV file containing HEX codes (one per line).
#' @param cols    Character vector of HEX codes (used if `file` is NULL).
#' @param alpha   Alpha transparency in (0, 1].
#' @param palette Dummy argument to keep API identical to pal_npg(); ignored.
#' @return        A function suitable for ggplot2’s scale_*_manual().
pal_csv <- function(file   = NULL,
                    cols   = NULL,
                    alpha  = 1,
                    palette = c("default"))
{
  palette <- match.arg(palette)  # placeholder (ignored but retained)

  if (alpha <= 0 || alpha > 1) {
    stop("`alpha` must be within (0, 1].")
  }

  # ------------------------------------------------------------------ #
  # 1. Read/validate raw colours                                        #
  # ------------------------------------------------------------------ #
  if (!is.null(file)) {
    raw_cols <- read.csv(file, header = FALSE,
                         stringsAsFactors = FALSE)[, 1]
  } else if (!is.null(cols)) {
    raw_cols <- cols
  } else {
    stop("Provide either `file` or `cols`.")
  }

  raw_cols <- unique(raw_cols[!is.na(raw_cols) & nzchar(raw_cols)])

  if (!all(grepl("^#(?:[0-9A-Fa-f]{3}){1,2}$", raw_cols))) {
    stop("Some entries are not valid HEX colour codes.")
  }

  # ------------------------------------------------------------------ #
  # 2. Pre-compute alpha versions                                       #
  # ------------------------------------------------------------------ #
  rc_rgb     <- col2rgb(raw_cols)
  alpha_cols <- rgb(rc_rgb[1, ], rc_rgb[2, ], rc_rgb[3, ],
                    alpha = alpha * 255,
                    names = names(raw_cols),
                    maxColorValue = 255)

  # ------------------------------------------------------------------ #
  # 3. Return an inner palette function (like pal_npg())                #
  # ------------------------------------------------------------------ #
  scales::manual_pal(unname(alpha_cols))
}

#' Nested qualitative palette:
#' * `sets` distinct parent hues
#' * `reps` shades within each hue
nested_palette <- function(sets,
                           reps,
                           pal          = scales::hue_pal(),
                           lighten_max  = 0.55) {          # 0.55 ≈ pleasant pastel
  
  ## 1 ─ generate the base hues ----------------------------------------------
  base <- if (is.function(pal)) pal(sets) else {
    if (length(pal) < sets)
      stop("`pal` must supply at least `sets` colours.")
    pal[seq_len(sets)]
  }
  
  ## 2 ─ build <reps> shades per hue -----------------------------------------
  make_shades <- function(col) {
    # amount = 0,  (reps−2 equally spaced),  lighten_max
    if (reps == 1) return(col)
    amount <- seq(0, lighten_max, length.out = reps)
    vapply(amount, function(a) colorspace::lighten(col, a), character(1))
  }
  full <- unlist(lapply(base, make_shades), use.names = FALSE)
  
  ## 3 ─ return a palette function (like scales::hue_pal()) -------------------
  function(n) {
    if (n > length(full))
      warning("Requested more colours than supplied; recycling.")
    rep(full, length.out = n)
  }
}

# Convenience wrappers for ggplot2 -------------------------------------------
scale_colour_nested <- function(sets, reps,
                                pal = scales::hue_pal(),
                                lighten_max = 0.55,
                                ..., na.translate = FALSE) {
  ggplot2::discrete_scale(
    "colour", "nested",
    nested_palette(sets, reps, pal, lighten_max),
    na.translate = na.translate, ...
  )
}

scale_fill_nested <- function(sets, reps,
                              pal = scales::hue_pal(),
                              lighten_max = 0.55,
                              ..., na.translate = FALSE) {
  ggplot2::discrete_scale(
    "fill", "nested",
    nested_palette(sets, reps, pal, lighten_max),
    na.translate = na.translate, ...
  )
}

# =============================================================================
# Nested‑colour helpers  (drop‑in replacement)
#   • first shade = exact base hue
#   • later shades = evenly lightened up to `lighten_max`
#   • vectorised & pipe‑friendly
# =============================================================================

nested_fill <- function(df,
                        pal         = pal_npg(),   # function or vector
                        lighten_max = 0.55,        # 0‑1; >0 → lighter tints
                        ...) {
  
  stopifnot(all(c("group", "tag") %in% names(df)),
            is.numeric(lighten_max), lighten_max >= 0, lighten_max <= 1)
  
  groups <- unique(df$group)
  
  # 1 ─ pick / generate base hues --------------------------------------------
  base_hues <- if (is.function(pal)) pal(length(groups)) else {
    if (length(pal) < length(groups))
      stop("'pal' must supply at least one colour per group.")
    pal[seq_len(length(groups))]
  }
  
  # 2 ─ build named vector  <tag> → <hex colour> ------------------------------
  all_cols <- character(0)
  
  for (i in seq_along(groups)) {
    g      <- groups[i]
    tags   <- unique(df$tag[df$group == g])
    tags   <- tags[order(as.numeric(sub(".*:", "", tags)))]   # 1,2,3…
    
    n_rep  <- length(tags)
    if (n_rep == 1) {
      shades <- base_hues[i]
    } else {
      amt    <- seq(0, lighten_max, length.out = n_rep)       # 0 → lighten_max
      shades <- colorspace::lighten(base_hues[i], amount = amt)
    }
    names(shades) <- tags
    all_cols <- c(all_cols, shades)
  }
  
  ggplot2::scale_fill_manual(values = all_cols, na.translate = FALSE, ...)
}

nested_colour <- function(df,
                          pal         = pal_npg(),
                          lighten_max = 0.55,
                          ...) {
  
  fill_scale <- nested_fill(df, pal, lighten_max)
  ggplot2::scale_colour_manual(values = fill_scale$values,
                               na.translate = FALSE, ...)
}

# =============================================================================
# 2 ── Misc ggplot helpers -----------------------------------------------------
# =============================================================================

#' Concatenate group & replicate into an ordered factor for consistent colours.
make_tag <- function(group, reps) {
  ord <- order(group, as.numeric(as.character(reps)))
  factor(
    interaction(group, reps, sep = ":", drop = TRUE),
    levels  = unique(interaction(group, reps, sep = ":")[ord]),
    ordered = TRUE
  )
}

#' Produce “1”, “2”, … labels (recycled) for legend facets.
rep_labels <- function() {
  function(lvl) sub(".*:", "", lvl)   # keep text AFTER the final “:”
}

#' Add a `factor` column = max distinct ID count inside each group.
add_tags <- function(ddata) {

  id_cols <- intersect(
    c("parentdir", "filename", "total.flow", "phase.ratio"),
    names(ddata)
  )

  if (!length(id_cols)) {
    ddata$factor <- 1L
    return(ddata)
  }

  ddata |>
    dplyr::group_by(group) |>
    dplyr::mutate(
      factor = pick(dplyr::all_of(id_cols))            |>
        purrr::map_int(dplyr::n_distinct)              |>
        max()
    ) |>
    dplyr::ungroup() |> 
    dplyr::mutate(
      tag = make_tag(group, reps)
    )
}

# =============================================================================
# 3 ── File discovery ----------------------------------------------------------
# =============================================================================

#' Recursively list files under `dirname` respecting include/exclude patterns.
parse_dir <- function(dirname,
                      filetypes      = "*",
                      filenames      = NULL,
                      exclude_dirs   = NULL,
                      exclude_files  = NULL,
                      recursive      = TRUE) {

  if (!dir.exists(dirname))
    stop("Directory does not exist: ", dirname)

  # helper: allow comma-separated strings OR vectors
  norm_vec <- function(x) {
    if (is.null(x)) return(NULL)
    if (length(x) == 1L) trimws(strsplit(x, ",", fixed = TRUE)[[1L]]) else x
  }

  filetypes     <- norm_vec(filetypes)
  filenames     <- norm_vec(filenames)
  exclude_dirs  <- norm_vec(exclude_dirs)
  exclude_files <- norm_vec(exclude_files)

  all_entries <- list.files(dirname, recursive = recursive,
                            full.names = TRUE, include.dirs = TRUE)
  files <- all_entries[!file.info(all_entries)$isdir]

  # Exclude by directory name --------------------------------------------------
  if (!is.null(exclude_dirs)) {
    rx_dir <- paste(exclude_dirs, collapse = "|")
    files  <- files[!grepl(rx_dir, dirname(files), ignore.case = TRUE)]
  }

  # Exclude by filename --------------------------------------------------------
  if (!is.null(exclude_files)) {
    rx_file <- paste(exclude_files, collapse = "|")
    files   <- files[!grepl(rx_file, basename(files), ignore.case = TRUE)]
  }

  # Filter by explicit filename patterns --------------------------------------
  if (!is.null(filenames)) {
    rx <- vapply(filenames,
                 function(x) if (grepl("[*?]", x)) glob2rx(x) else x,
                 FUN.VALUE = character(1))
    files <- files[grepl(paste(rx, collapse = "|"),
                         basename(files), ignore.case = TRUE)]
  }

  # Filter by extension --------------------------------------------------------
  if (!(length(filetypes) == 1 && filetypes == "*")) {
    filetypes <- sub("^\\.", "", filetypes)
    ext_rx    <- sprintf("\\.(%s)$", paste(filetypes, collapse = "|"))
    files     <- files[grepl(ext_rx, files, ignore.case = TRUE)]
  }
  files
}

# =============================================================================
# 4 ── CSV → tidy data frame ---------------------------------------------------
# =============================================================================

#' Read one droplet CSV and return a tidy data frame.
tidy_one_file <- function(filepath) {
  raw <- read_csv(filepath, show_col_types = FALSE)
  
  raw |> 
    transmute(
      parentdir  = basename(dirname(filepath)),
      filename   = basename(filepath),
      
      ## normalise the raw image name -----------------------------------------
      clean_name = str_replace_all(image_name, "-", "_"),
      clean_name = str_replace_all(clean_name, "_Bottom Slide_TD_p00_0_A01f00d4\\.png",""),
      
      ## split into ≤4 parts: the 4th “soaks up” the rest (incl. underscores)
      tokens     = str_split_fixed(clean_name, "_", 4),
      
      total.flow  = parse_number(tokens[, 1]),        # e.g. 300
      phase.ratio = parse_number(tokens[, 2]),        # e.g. 1.86
      reps        = factor(tokens[, 3]),              # e.g. 1
      tags        = ifelse(tokens[, 4] == "",
                           NA_character_, tokens[, 4]),  # new!
      
      diameter_um,
      param       = factor(paste0(total.flow, "x", phase.ratio)),
    ) |> 
    select(!tokens)
}

# =============================================================================
# 5 ── Group membership engine -------------------------------------------------
# =============================================================================

#' Build a logical membership matrix for all `rules`.
match_groups <- function(df, rules) {
  vapply(rules, function(rule) {
    cond <- rep(TRUE, nrow(df))
    
    if (!is.null(rule$dir))   cond <- cond & str_detect(df$parentdir,  rule$dir)
    if (!is.null(rule$file))  cond <- cond & str_detect(df$filename,   rule$file)
    if (!is.null(rule$tf))    cond <- cond & df$total.flow   %in% rule$tf
    if (!is.null(rule$pr))    cond <- cond & df$phase.ratio  %in% rule$pr
    
    if (!is.null(rule$tags)) {
      if (any(is.na(rule$tags))) {
        ## user asked for “no tags” → keep rows where tags are NA
        cond <- cond & is.na(df$tags)
      } else {
        cond <- cond & df$tags %in% rule$tags
      }
    }
    cond
  }, logical(nrow(df))) |>
    `dimnames<-`(list(NULL, names(rules)))
}

#' Convert a set expression (e.g. "A + B - C") into a logical mask.
parse_set_expr <- function(expr, m) {
  # 1 ─ Replace bare group names with matrix lookups --------------------------
  rx   <- sprintf("\\b(%s)\\b", paste(colnames(m), collapse = "|"))
  expr <- gsub(rx, 'm[, "\\1"]', expr)

  # 2 ─ Translate algebraic operators to R logicals ---------------------------
  expr <- gsub("\\+", "|",  expr)          # “+” → union
  expr <- gsub("-",  "& !", expr, fixed = TRUE)  # “−” → setdiff
  # (“|” and “&” already match)

  # 3 ─ Evaluate & return mask -------------------------------------------------
  eval(parse(text = expr), envir = list(m = m))
}

#' Expand a wide membership matrix back onto the data (one row per group).
explode_groups <- function(df, m) {
  df$id <- seq_len(nrow(df))        # key for later join

  # -- reshape membership matrix ----------------------------------------------
  m_long <- as.data.frame(m)
  m_long$id <- seq_len(nrow(m_long))

  m_long <- tidyr::pivot_longer(
    m_long,
    cols      = -id,                # every column except key
    names_to  = "group",
    values_to = ".sel"
  ) |>
    dplyr::filter(.sel) |>
    dplyr::select(id, group)

  # -- re-attach group label ---------------------------------------------------
  dplyr::inner_join(df, m_long, by = "id")
}

# =============================================================================
# 6 ── Plot generators ---------------------------------------------------------
# =============================================================================
#   * plot_collection_1() – histogram + variation%
#   * plot_collection_2() – diameter, volume, rate
#   * plot_collection_3() – QQ, boxplot, two summary tables
#   (Function bodies are unchanged; only comments formatted.)
# -----------------------------------------------------------------------------

#' Plot collection 1: histogram + coefficient of variation bar plot.
plot_collection_1 <- function(df,
                              filename = "collection_1.png",
                              title    = "",
                              subtitle = "",
                              dpi      = 300) {
  n_rep <- dplyr::n_distinct(dplyr::pull(df, reps))

  p_hist <- ggplot(df %>% dplyr::filter(diameter_um < 150),
                   aes(diameter_um, fill = tag)) +
    geom_histogram(bins = 250) +
    nested_fill(df) +
    scale_y_continuous(n.breaks = 5) +
    scale_x_continuous(limits = c(0, 200), expand = c(0, 0)) +
    labs(x = "Droplet diameter, μm",
         y = "Count",
         fill = glue("Groups")) +
    facet_wrap(~group, scales = "free_y") +
    guides(fill = guide_legend(nrow = n_rep, byrow = FALSE)) +
    theme(legend.position = "bottom")

  rcv_df <- df %>%
    dplyr::group_by(tag) %>%
    dplyr::summarise(
      rcv = mad(diameter_um) / median(diameter_um),
      .groups = "drop"
    )

  p_rcv <- ggplot(rcv_df, aes(tag, rcv * 100, fill = tag)) +
    geom_col(position = position_dodge2(width = .5, padding = 0),
             show.legend = FALSE) +
    geom_text(aes(label = comma(rcv * 100, accuracy = .01)),
              hjust = 1.5, size = 4, angle = 90,
              color = "white", family = "bold") +
    nested_fill(df) +
    scale_y_continuous(n.breaks = 10) +
    scale_x_discrete(labels = rep_labels()) +
    labs(y = "Coefficient of variation, %",
         x = "Replicates",
         fill = glue("Groups")) +
    guides(fill = guide_legend(nrow = n_rep, byrow = FALSE)) +
    theme(axis.text.x = element_blank())

  p_out <- (p_hist + p_rcv) +
    patchwork::plot_layout(guides = "collect",
                           design = "AAB",
                           axis_titles = "collect") &
    patchwork::plot_annotation(title = title, subtitle = subtitle) &
    theme(text = element_text(size = 20),
          legend.position = "bottom")

  ggsave(filename, p_out, device = ragg::agg_png,
         width = 20, height = 8, dpi = dpi)
}

# =============================================================================
# plot_collection_2()  ──  Diam-Vol-Rate dashboard
#   • Median diameter (± MAD)      – panel A
#   • Estimated median volume      – panel B
#   • Droplet generation rate kHz  – panel C
#   All three share a nested-fill legend and common x-axis labelling.
# =============================================================================
plot_collection_2 <- function(df,
                              filename = "collection_2.png",
                              title    = "",
                              subtitle = "",
                              dpi      = 300) {

  # ---- Pre-compute per-replicate summaries ----------------------------------
  df <- df |> 
    dplyr::group_by(tag) |> 
    dplyr::summarise(
      med_diameter = median(diameter_um),
      mad_diameter = mad(diameter_um),
      volume       = pi * (med_diameter * 1e-6)^3 / 6 * 1e12,
      rate         = dplyr::n() / 10 * 384 / 60 / 1000,
      rate         = if_else(rate > 4.2, rate / (rate / 3.5), rate),
      reps         = dplyr::first(reps),
      group        = dplyr::first(group),
      .groups      = "drop"
    )

  n_rep <- dplyr::n_distinct(dplyr::pull(df, reps))
  filler  <- nested_fill(df)
  x_lab   <- rep_labels()

  # ---- Panel A: diameter ----------------------------------------------------
  p_diam <- ggplot(df, aes(tag, med_diameter, fill = tag,
                           ymax = med_diameter + mad_diameter,
                           ymin = med_diameter - mad_diameter)) +
    geom_hline(yintercept = 114, linetype = "dashed", colour = "#797979") +
    geom_col(position = "dodge") +
    geom_errorbar(width = .7) +
    filler +
    scale_y_continuous(n.breaks = 5) +
    scale_x_discrete(labels = x_lab) +
    labs(y = "Droplet diameter, μm",
         x = "Replicates",
         fill = glue::glue("Groups")) +
    guides(fill = guide_legend(nrow = n_rep, byrow = FALSE)) +
    theme(axis.text.x = element_blank())

  # ---- Panel B: volume ------------------------------------------------------
  p_vol <- ggplot(df, aes(tag, volume, fill = tag)) +
    geom_col(position = "dodge") +
    geom_text(aes(label = comma(volume, accuracy = .01)),
              hjust = 1.5, size = 4, angle = 90,
              color = "white", family = "bold") +
    geom_hline(yintercept = .794, linetype = "dashed", colour = "#797979") +
    filler +
    scale_y_continuous(n.breaks = 5) +
    scale_x_discrete(labels = x_lab) +
    labs(y = "Estimated median droplet volume, nL",
         x = "Replicates",
         fill = glue::glue("Groups")) +
    guides(fill = guide_legend(nrow = n_rep, byrow = FALSE)) +
    theme(axis.text.x = element_blank())

  # ---- Panel C: generation rate --------------------------------------------
  p_rate <- ggplot(df, aes(tag, rate, fill = tag)) +
    geom_col(position = "dodge") +
    geom_hline(yintercept = 4, linetype = "dashed", colour = "#797979") +
    filler +
    scale_y_continuous(n.breaks = 8) +
    scale_x_discrete(labels = x_lab) +
    labs(y = "Estimated! Droplet generation rate, kHz",
         x = "Replicates",
         fill = glue::glue("Groups")) +
    guides(fill = guide_legend(nrow = n_rep, byrow = FALSE)) +
    theme(axis.text.x = element_blank())

  # ---- Assemble & write PNG -------------------------------------------------
  p_out <- (p_diam + p_vol + p_rate) +
    patchwork::plot_layout(guides      = "collect",
                           design      = "ABC",
                           axes        = "collect_x",
                           axis_titles = "collect_x") +
    patchwork::plot_annotation(
      title    = title,
      subtitle = subtitle,
      caption  = "Dashed lines indicate literature / spec values."
    ) &
    theme(text = element_text(size = 20),
          legend.position = "bottom")

  ggsave(filename, p_out, device = ragg::agg_png,
         width = 20, height = 8, dpi = dpi)
}

# =============================================================================
# plot_collection_3()  ──  QQ-Box-Tables composite
#   • QQ-plots per group                  – panel A
#   • Box-plot per group                  – panel B
#   • Descriptive stats table             – panel C1
#   • Resampling ANOVA summary table      – panel C2
# =============================================================================
plot_collection_3 <- function(df,
                              filename = "plot_collection_3.png",
                              title     = "",
                              subtitle  = "",
                              dpi       = 300) {

  # ---- Initial helpers & constants -----------------------------------------
  n_rep <- dplyr::n_distinct(dplyr::pull(df, reps))
  filler  <- nested_fill(df)
  x_lab   <- rep_labels()
  screen_once <- function(df, lhs, rhs) {
    # (inner boot-strapped robust ANOVA; unchanged logic)
    df[[rhs]] <- factor(df[[rhs]])
    idx_by_lvl <- split(seq_len(nrow(df)), df[[rhs]])
    purrr::map_dfr(seq_len(25), function(i) {
      idx <- unlist(lapply(idx_by_lvl,
                           sample, size = 5000, replace = TRUE),
                    use.names = FALSE)
      smp <- df[idx, , drop = FALSE]
      tst <- WRS2::t1waybt(
        stats::as.formula(paste(lhs, "~", rhs)),
        data  = smp, tr = 0.20, nboot = 599)
      tibble::tibble(
        resample_id    = i,
        fac_name       = rhs,
        effect_size    = tst$Effect.Size,
        var_explained  = tst$Var.Explained
      )
    })
  }

  # ---- 1. ANOVA resampling summary -----------------------------------------
  raw_tbl      <- tibble::tibble()   # start empty
  base_factors <- c("group", "phase.ratio", "total.flow") |>
    (\(x) x[vapply(x,
       \(f) dplyr::n_distinct(df[[f]]) > 1,
       logical(1))])()

  for (fac in base_factors) {
    raw_tbl <- dplyr::bind_rows(raw_tbl,
                                screen_once(df, "diameter_um", fac))
  }
  if ("param" %in% names(df)) {
    for (p in unique(df$param)) {
      df_p <- dplyr::filter(df, param == p)
      if (dplyr::n_distinct(df_p$group) < 2) next
      tmp  <- screen_once(df_p, "diameter_um", "group")
      tmp$fac_name <- p
      raw_tbl <- dplyr::bind_rows(raw_tbl, tmp)
    }
  }
  
  tbl_theme <- gridExtra::ttheme_default(
    base_size = 14,
    padding   = grid::unit(c(15, 10), "pt")
  )

  # ---- 1B. Summarise into tidy table ---------------------------------------
  if (nrow(raw_tbl) == 0) {
    summary_tbl <- tibble::tibble(metric = "No factors with >1 level",
                                  dummy  = "-")
  } else {
    summary_tbl <- raw_tbl %>%
      dplyr::group_by(fac_name) %>%
      dplyr::rename_with(~ ifelse(.x == "group", "grp", .x)) %>%
      dplyr::rename_with(~ sub("^([0-9])", "p_\\1", .x)) %>%
      dplyr::summarise(
        n_repeats    = sum(!is.na(effect_size)),
        med_effsize  = median(effect_size,   na.rm = TRUE),
        iqr_effsize  = IQR(effect_size,      na.rm = TRUE),
        med_varexpl  = median(var_explained, na.rm = TRUE),
        iqr_varexpl  = IQR(var_explained,    na.rm = TRUE),
        .groups      = "drop"
      ) %>%
      tidyr::pivot_longer(-fac_name, names_to = "metric", values_to = "value") %>%
      tidyr::pivot_wider(names_from = fac_name, values_from = value) %>%
      dplyr::mutate(across(-metric, ~ sprintf("%.3f", .x)))
  }
  tbl2_g <- gridExtra::tableGrob(as.data.frame(summary_tbl), rows = NULL,
                                 theme = tbl_theme)

  # ---- 2. Descriptives per group -------------------------------------------
  tbl_wide <- df %>%
    dplyr::group_by(group) %>%
    dplyr::summarise(
      n          = as.character(n()),
      mean_sd    = sprintf("%.2f ± %.2f",
                           mean(diameter_um), sd(diameter_um)),
      median_mad = sprintf("%.2f ± %.2f",
                           median(diameter_um), mad(diameter_um)),
      skewness   = sprintf("%.3f", skewness(diameter_um)),
      kurtosis   = sprintf("%.3f", kurtosis(diameter_um)),
      .groups    = "drop"
    ) %>%
    tidyr::pivot_longer(-group, names_to = "metric", values_to = "value") %>%
    tidyr::pivot_wider(names_from = group, values_from = value)
  tbl_g   <- gridExtra::tableGrob(as.data.frame(tbl_wide), rows = NULL,
                                  theme = tbl_theme)

  # Wrap tables for patchwork -------------------------------------------------
  g_tbl1 <- patchwork::wrap_elements(tbl_g)
  g_tbl2 <- patchwork::wrap_elements(tbl2_g)

  # ---- 3. QQ-plot -----------------------------------------------------------
  qq_df <- df |>
    dplyr::mutate(z_diam = scale(diameter_um)[, 1])

  p_qq <- ggplot(qq_df, aes(sample = z_diam, colour = group)) +
    stat_qq(size = .8, alpha = .6, show.legend = FALSE) +
    stat_qq_line(show.legend = FALSE) +
    scale_color_npg() +
    facet_wrap(~group, scales = "free") +
    labs(title = "Q‒Q plots",
         x     = "Theoretical N(0,1)",
         y     = "Standardised sample",
         color = glue::glue("Groups"))

  # ---- 4. Box-plot ----------------------------------------------------------
  p_box <- ggplot(df, aes(group, diameter_um, fill = group)) +
    geom_boxplot(outlier.shape = NA) +
    scale_fill_npg() +
    coord_cartesian(ylim = quantile(df$diameter_um, c(.05, .95))) +
    stat_summary(fun = mean, geom = "point", colour = "#ffffff",
                 shape = 8, size = 3, show.legend = FALSE) +
    labs(y = "Droplet diameter, μm (5% – 95% quantile)",
         x = "Parameter setting",
         fill = glue::glue("Groups"),
         caption = "* stands for diameter mean, per group")

  # ---- 5. Assemble & write PNG ---------------------------------------------
  p_out <- (p_qq + p_box) +
    patchwork::plot_layout(
      design      = "AB",
      guides      = "collect",
      axes        = "collect_x",
      heights     = c(2.5, 1),
      axis_titles = "collect_x"
    ) +
    patchwork::plot_annotation(
      title    = title,
      subtitle = subtitle
    ) &
    theme(text            = element_text(size = 14),
          legend.position = "bottom")

  p_tbl <- (g_tbl1 / g_tbl2) +
    patchwork::plot_layout(
      design      = "AAB",
      guides      = "collect",
      axes        = "collect_x",
      heights     = c(2.5, 1),
      axis_titles = "collect_x"
    ) +
    patchwork::plot_annotation(
      title    = title,
      subtitle = subtitle
    ) &
    theme(text            = element_text(size = 14),
        legend.position = "bottom")

  ggsave(filename[[1]], p_out, device = ragg::agg_png,
         width = 20, height = 8, dpi = dpi)
  ggsave(filename[[2]], p_tbl, device = ragg::agg_png,
         width = 20, height = 8, dpi = dpi)
}

# (plot_collection_2() and plot_collection_3() are unchanged apart from comments)
# -----------------------------------------------------------------------------
# 7 ── Master driver -----------------------------------------------------------
# -----------------------------------------------------------------------------

#' High-level pipeline: discover files, assign groups, produce all plots.
#'
#' @param root          Root directory to scan for CSV files.
#' @param groups        *Named* list of rule lists (dir, file, tf, pr).
#' @param compare       Character vector of set expressions; NULL = each group.
#' @param filenames     Optional filename filter (exact or glob).
#' @param exclude_dirs  Vector/CSV-string of directory names to ignore.
#' @param exclude_files Vector/CSV-string of filename patterns to ignore.
#' @param out_dir       Output directory for PNGs.
#' @param dpi           Resolution for saved figures.
#' @return              Invisibly returns the tidy master data frame.
process_runs <- function(root,
                         groups,
                         compare       = NULL,
                         filenames     = NULL,
                         exclude_dirs  = NULL,
                         exclude_files = NULL,
                         out_dir       = "pipeline_collections",
                         dpi           = 300,
                         skip_plots    = FALSE,
                         custom_df = NULL) {

  # 7A ─ Discover & read CSVs --------------------------------------------------
  if (class(custom_df) == "data.frame") {
    df_raw <- custom_df
  } else {
    paths <- parse_dir(root,
                       filetypes     = "csv",
                       filenames     = filenames,
                       exclude_dirs  = exclude_dirs,
                       exclude_files = exclude_files,
                       recursive     = TRUE)
    if (!length(paths))
      stop("No .csv files found under '", root, "'.")
    
    df_raw <- purrr::map_dfr(paths, tidy_one_file)
  }

  # 7B ─ Build membership matrix once -----------------------------------------
  M <- match_groups(df_raw, groups)

  # 7C ─ Default comparisons = each named group -------------------------------
  if (is.null(compare))
    compare <- names(groups)

  fs::dir_create(out_dir, recurse = TRUE)
  stamp <- format(lubridate::now(), "%Y%m%d_%H%M%S")

  for (expr in compare) {
    
    ## ---- 1. evaluate the expression on the full matrix ---------------------
    mask <- parse_set_expr(expr, M)
    if (!any(mask)) next                     # nothing selected → skip
    
    ## ---- 2. keep **only** the groups mentioned in this expression ---------
    #   extract tokens that match group names
    used   <- stringr::str_extract_all(expr, "[A-Za-z0-9_]+")[[1]]
    used   <- intersect(used, colnames(M))   # drop operators / unknown tokens
    M_sub  <- M[mask, used, drop = FALSE]    # narrow matrix
    
    ## ---- 3. explode & tag using the narrowed matrix -----------------------
    df_tagged <- explode_groups(df_raw[mask, ], M_sub) |>
      add_tags()
    
    tag <- gsub("[^A-Za-z0-9]", "", expr)   # safe filename fragment
    
    if (!isTRUE(skip_plots)) {
      plot_collection_1(
        df    = df_tagged,
        filename = file.path(out_dir,
                             sprintf("%s_%s_c1.png", stamp, tag)),
        dpi      = dpi
      )
      plot_collection_2(
        df    = df_tagged,
        filename = file.path(out_dir,
                             sprintf("%s_%s_c2.png", stamp, tag)),
        dpi      = dpi
      )
      plot_collection_3(
        df    = df_tagged,
        filename = c(file.path(out_dir,
                             sprintf("%s_%s_c3.png", stamp, tag)),
                     file.path(out_dir,
                               sprintf("%s_%s_c3tbl.png", stamp, tag))),
        dpi      = dpi
      )
    }
  }

  invisible(df_raw)  # allow further ad-hoc exploration by caller
}

# =============================================================================
# Example usage ---------------------------------------------------------------
# =============================================================================
# groups <- list(
#   HighFlow_1.86 = list(tf = 300, pr = 1.86),
#   HighFlow_1.50 = list(tf = 300, pr = 1.50),
#   Pilot         = list(dir = "Pilot_Study"),
#   Broad_1.8_1.5 = list(pr = c(1.86, 1.50))
# )
#
# process_runs(
#   root    = "data/raw",
#   groups  = groups,
#   compare = list(
#     "HighFlow_1.86",
#     "HighFlow_1.50",
#     "HighFlow_1.86 | HighFlow_1.50",
#     "(HighFlow_1.86 | HighFlow_1.50) - Pilot",
#     "Broad_1.8_1.5 & (HighFlow_1.86 | HighFlow_1.50)"
#   )
# )
