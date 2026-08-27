# Build the Word reference documents the reports render against.
#
# Quarto takes page size, margins, and styles for docx output from a reference
# document; there is no `geometry:` equivalent the way there is for PDF. The
# pandoc default is Letter portrait with 1" margins and 12pt text everywhere,
# including inside tables, which leaves roughly 90 characters of usable width.
# Most of the tables in these reports need 100-160, so they wrap badly.
#
# This patches the pandoc default into two variants:
#   reference-portrait.docx   0.75" margins, 8pt tables  (~115 chars)  - report
#   reference-landscape.docx  landscape, 0.6" margins, 8pt tables (~160) - the
#                             tables-only report, where prose width is moot
#
# Run once from the project root; the outputs are committed. Re-run only if the
# page setup needs changing.
#
#   Rscript doc/make-reference-docx.R

TWIPS_PER_INCH <- 1440

#' Patch pandoc's default reference.docx for a given page setup
#'
#' @param out Output .docx path.
#' @param landscape Landscape orientation?
#' @param margin_in Page margin in inches.
#' @param table_pt Font size for table body text, in points.
build_reference <- function(out, landscape, margin_in, table_pt = 8) {
  tmp <- tempfile("refdocx")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  base <- file.path(tmp, "base.docx")
  system2(
    "quarto",
    c("pandoc", "--print-default-data-file", "reference.docx"),
    stdout = base
  )

  unpacked <- file.path(tmp, "unpacked")
  dir.create(unpacked)
  utils::unzip(base, exdir = unpacked)

  short <- 12240 # 8.5" in twips
  long <- 15840 # 11" in twips
  margin <- round(margin_in * TWIPS_PER_INCH)
  page <- if (landscape) {
    sprintf('<w:pgSz w:w="%d" w:h="%d" w:orient="landscape"/>', long, short)
  } else {
    sprintf('<w:pgSz w:w="%d" w:h="%d"/>', short, long)
  }

  # Word requires sectPr children in schema order: footnotePr, then pgSz, then
  # pgMar. The default sectPr carries only footnotePr, so append to it.
  doc_path <- file.path(unpacked, "word", "document.xml")
  doc <- readLines(doc_path, warn = FALSE) |> paste(collapse = "\n")
  doc <- sub(
    "</w:sectPr>",
    paste0(
      page,
      sprintf(
        paste0(
          '<w:pgMar w:top="%d" w:right="%d" w:bottom="%d" w:left="%d" ',
          'w:header="720" w:footer="720" w:gutter="0"/>'
        ),
        margin,
        margin,
        margin,
        margin
      ),
      "</w:sectPr>"
    ),
    doc,
    fixed = TRUE
  )
  writeLines(doc, doc_path)

  # Table cell text uses the "Compact" paragraph style, which inherits the 12pt
  # document default. Giving it an explicit size shrinks tables only, leaving
  # body text alone. Word sizes are in half-points.
  styles_path <- file.path(unpacked, "word", "styles.xml")
  styles <- readLines(styles_path, warn = FALSE) |> paste(collapse = "\n")
  half_points <- table_pt * 2
  styles <- sub(
    '<w:spacing w:before="36" w:after="36" />\n    </w:pPr>\n  </w:style>',
    sprintf(
      paste0(
        '<w:spacing w:before="20" w:after="20" />\n    </w:pPr>\n    ',
        '<w:rPr><w:sz w:val="%d" /><w:szCs w:val="%d" /></w:rPr>\n  </w:style>'
      ),
      half_points,
      half_points
    ),
    styles,
    fixed = TRUE
  )
  writeLines(styles, styles_path)

  # Resolve to an absolute path before changing directory to zip from inside
  # the unpacked tree, otherwise the relative path resolves against the tempdir.
  out <- file.path(
    normalizePath(dirname(out), mustWork = TRUE),
    basename(out)
  )
  if (file.exists(out)) {
    file.remove(out)
  }
  files <- list.files(unpacked, recursive = TRUE, all.files = TRUE)
  withr_dir <- setwd(unpacked)
  on.exit(setwd(withr_dir), add = TRUE)
  status <- utils::zip(out, files, flags = "-qr9X")
  if (status != 0 || !file.exists(out)) {
    stop("Failed to write ", out)
  }
  invisible(out)
}

build_reference(
  "doc/reference-portrait.docx",
  landscape = FALSE,
  margin_in = 0.75
)
build_reference(
  "doc/reference-landscape.docx",
  landscape = TRUE,
  margin_in = 0.6
)
message("Wrote doc/reference-portrait.docx and doc/reference-landscape.docx")
