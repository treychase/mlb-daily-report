# ── Plot Functions ─────────────────────────────────────────────────────────

# ── Pitch movement scatter ──────────────────────────────────────────────────
create_movement_plot <- function(ars) {
  dark_bg <- "#0d1117"
  no_data <- function(msg)
    ggplot2::ggplot() +
      ggplot2::annotate("text", x=0, y=0, label=msg, color="#8b949e", size=5) +
      ggplot2::theme_void() +
      ggplot2::theme(plot.background=ggplot2::element_rect(fill=dark_bg, color=NA))

  if (is.null(ars) || !is.data.frame(ars) || nrow(ars)==0)
    return(no_data("No movement data available"))

  # Filter real pitches only
  ars <- ars[!toupper(as.character(ars$pitch_type %||% "")) %in% NON_PITCH_TYPES, ]
  if (nrow(ars) == 0) return(no_data("No pitch movement data"))

  x_col <- intersect(c("h_break_in","pitcher_break_x","pfx_x"), names(ars))[1]
  z_col <- intersect(c("v_break_in","pitcher_break_z","pfx_z"), names(ars))[1]
  if (is.na(x_col) || is.na(z_col))
    return(no_data("Movement columns not available"))

  pcolors <- c("FF"="#ef4444","SI"="#f97316","FC"="#f59e0b","SL"="#3b82f6",
               "SW"="#60a5fa","ST"="#93c5fd","CH"="#22c55e","FS"="#14b8a6",
               "FO"="#10b981","CU"="#a855f7","KC"="#9333ea","CS"="#7c3aed",
               "KN"="#8b949e")

  usage_vec <- {
    if ("pitch_percent" %in% names(ars) && length(ars$pitch_percent)==nrow(ars))
      suppressWarnings(as.numeric(ars$pitch_percent)*100)
    else {
      cnt_col <- intersect(c("n_pitches","count","pitches"), names(ars))[1]
      if (!is.na(cnt_col) && length(ars[[cnt_col]])==nrow(ars)) {
        tot <- sum(suppressWarnings(as.numeric(ars[[cnt_col]])), na.rm=TRUE)
        suppressWarnings(as.numeric(ars[[cnt_col]]))/max(tot,1)*100
      } else rep(10, nrow(ars))
    }
  }
  pd <- data.frame(
    bx    = suppressWarnings(as.numeric(ars[[x_col]])),
    bz    = suppressWarnings(as.numeric(ars[[z_col]])),
    type  = toupper(as.character(ars$pitch_type %||% "??")),
    usage = usage_vec,
    stringsAsFactors = FALSE
  )
  pd <- pd[!is.na(pd$bx) & !is.na(pd$bz) & !pd$type %in% NON_PITCH_TYPES, ]
  if (nrow(pd) == 0) return(no_data("No valid movement data"))

  pd$label <- sapply(pd$type, pitch_full_name)  # full names for legend
  pd$fill  <- ifelse(pd$type %in% names(pcolors), pcolors[pd$type], "#6e7681")
  u  <- pmax(pd$usage, 1, na.rm=TRUE)
  mn <- min(u, na.rm=TRUE); mx <- max(u, na.rm=TRUE)
  pd$sz <- if (mx > mn) (u-mn)/(mx-mn)*14+10 else rep(14, nrow(pd))

  ggplot2::ggplot(pd, ggplot2::aes(x=bx, y=bz)) +
    ggplot2::geom_hline(yintercept=0, color="#30363d", linewidth=0.6) +
    ggplot2::geom_vline(xintercept=0, color="#30363d", linewidth=0.6) +
    ggplot2::geom_point(ggplot2::aes(fill=I(fill)), size=pd$sz,
                        shape=21, color="#0d1117", stroke=1.5, alpha=0.92) +
    ggplot2::geom_text(ggplot2::aes(label=type), color="white", fontface="bold", size=3.5) +
    ggplot2::annotate("text",x=Inf,y=-Inf,hjust=1.1,vjust=-1,
                      label="Arm Side \u2192",color="#6e7681",size=3.2) +
    ggplot2::annotate("text",x=-Inf,y=-Inf,hjust=-0.1,vjust=-1,
                      label="\u2190 Glove Side",color="#6e7681",size=3.2) +
    ggplot2::labs(x="Horizontal Break (in)", y="Induced Vertical Break (in)") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.background  = ggplot2::element_rect(fill=dark_bg, color=NA),
      panel.background = ggplot2::element_rect(fill=dark_bg, color=NA),
      panel.grid.major = ggplot2::element_line(color="#1d2633", linewidth=0.4),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text  = ggplot2::element_text(color="#8b949e", size=9),
      axis.title = ggplot2::element_text(color="#6e7681", size=10),
      plot.margin = ggplot2::margin(12, 20, 8, 16)
    )
}

# ── Strike zone with plate ──────────────────────────────────────────────────
# Draws average pitch location per pitch type over a regulation strike zone.
# plate_x / plate_z come from Statcast (feet; positive x = arm side for RHP).
create_strike_zone_plot <- function(mov_data) {
  dark_bg <- "#0d1117"
  no_data_plot <- function(msg)
    ggplot2::ggplot() +
      ggplot2::annotate("text", x=0, y=2, label=msg, color="#8b949e", size=4.5) +
      ggplot2::theme_void() +
      ggplot2::theme(plot.background=ggplot2::element_rect(fill=dark_bg,color=NA))

  if (is.null(mov_data) || !is.data.frame(mov_data) || nrow(mov_data)==0)
    return(no_data_plot("No plate location data"))
  if (!all(c("plate_x","plate_z","pitch_type") %in% names(mov_data)))
    return(no_data_plot("plate_x / plate_z not available"))

  pd <- mov_data %>%
    dplyr::filter(
      !toupper(as.character(pitch_type %||% "")) %in% NON_PITCH_TYPES,
      !is.na(suppressWarnings(as.numeric(plate_x))),
      !is.na(suppressWarnings(as.numeric(plate_z)))
    ) %>%
    dplyr::mutate(
      px    = suppressWarnings(as.numeric(plate_x)),
      pz    = suppressWarnings(as.numeric(plate_z)),
      ptype = toupper(as.character(pitch_type)),
      label = sapply(ptype, pitch_full_name),
      col   = sapply(ptype, pitch_col)
    )
  if (nrow(pd) == 0) return(no_data_plot("No valid plate locations"))

  # Strike zone box (regulation: 17" wide, ~1.5–3.5 ft tall)
  sz_hw <- 17/24       # half-width in feet
  sz_lo <- 1.50; sz_hi <- 3.50

  # Home plate (pentagon, centred at x=0, bottom near y=0)
  pl_hw <- 8.5/12      # half plate width = 8.5 inches
  plate <- data.frame(
    x = c(-pl_hw, pl_hw,  pl_hw,  0, -pl_hw),
    y = c(0.25,   0.25,  -0.05, -0.20, -0.05)
  )

  # Catcher/batter silhouette boxes (decorative context)
  pcolors <- c("FF"="#ef4444","SI"="#f97316","FC"="#f59e0b","SL"="#3b82f6",
               "SW"="#60a5fa","ST"="#93c5fd","CH"="#22c55e","FS"="#14b8a6",
               "FO"="#10b981","CU"="#a855f7","KC"="#9333ea","CS"="#7c3aed",
               "KN"="#8b949e")
  pd$fill_col <- ifelse(pd$ptype %in% names(pcolors), pcolors[pd$ptype], "#6e7681")

  ggplot2::ggplot() +
    # Plate
    ggplot2::geom_polygon(data=plate, ggplot2::aes(x=x,y=y),
                          fill="#c9d1d9", color="#e6edf3", linewidth=1) +
    # Strike zone
    ggplot2::annotate("rect",
      xmin=-sz_hw, xmax=sz_hw, ymin=sz_lo, ymax=sz_hi,
      fill=NA, color="#8b949e", linewidth=1, linetype="dashed") +
    # Inner zone quadrants (subtle)
    ggplot2::annotate("segment",
      x=0, xend=0, y=sz_lo, yend=sz_hi, color="#1d2633", linewidth=0.5) +
    ggplot2::annotate("segment",
      x=-sz_hw, xend=sz_hw, y=(sz_lo+sz_hi)/2, yend=(sz_lo+sz_hi)/2,
      color="#1d2633", linewidth=0.5) +
    # Average pitch locations
    ggplot2::geom_point(data=pd,
      ggplot2::aes(x=px, y=pz, fill=I(fill_col)),
      size=9, shape=21, color="#0d1117", stroke=1.8, alpha=0.92) +
    ggplot2::geom_text(data=pd,
      ggplot2::aes(x=px, y=pz, label=ptype),
      color="white", fontface="bold", size=2.8) +
    # Labels outside zone
    ggplot2::annotate("text",x=-sz_hw-0.05,y=(sz_lo+sz_hi)/2,
      label="Inside", color="#6e7681", size=2.8, angle=90) +
    ggplot2::annotate("text",x= sz_hw+0.05,y=(sz_lo+sz_hi)/2,
      label="Outside",color="#6e7681", size=2.8, angle=90) +
    ggplot2::coord_fixed(ratio=1, xlim=c(-1.8, 1.8), ylim=c(-0.4, 4.5)) +
    ggplot2::scale_x_continuous(
      breaks=c(-sz_hw, 0, sz_hw),
      labels=c("-8.5\"","0","8.5\"")) +
    ggplot2::scale_y_continuous(
      breaks=c(0, sz_lo, (sz_lo+sz_hi)/2, sz_hi),
      labels=c("Plate","1.5ft","2.5ft","3.5ft")) +
    ggplot2::labs(x="Horizontal Position", y="Height") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.background  = ggplot2::element_rect(fill=dark_bg, color=NA),
      panel.background = ggplot2::element_rect(fill=dark_bg, color=NA),
      panel.grid.major = ggplot2::element_line(color="#1d2633", linewidth=0.3),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text  = ggplot2::element_text(color="#8b949e", size=8),
      axis.title = ggplot2::element_text(color="#6e7681", size=9),
      plot.margin = ggplot2::margin(10, 16, 8, 16)
    )
}
