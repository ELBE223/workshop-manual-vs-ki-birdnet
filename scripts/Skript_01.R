# Xeno-canto API v3 downloader - Group A (Beginner)
# Requirements: XC_KEY env var (Sys.setenv), packages jsonlite + curl
# Optional: ffmpeg for trimming/concat

out_dir <- "/Users/lucasbeseler/Desktop/Workshop/XC_GroupA"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

clip_len_sec <- 25
gap_sec <- 1

species_A <- c(
  "Turdus merula",
  "Fringilla coelebs",
  "Phylloscopus collybita",
  "Parus major",
  "Erithacus rubecula",
  "Troglodytes troglodytes",
  "Sylvia atricapilla",
  "Turdus philomelos"
)


xc_key <- Sys.getenv("XC_KEY")


if (!requireNamespace("jsonlite", quietly = TRUE)) install.packages("jsonlite")
if (!requireNamespace("curl", quietly = TRUE)) install.packages("curl")

norm_key <- function(x) {
  x <- trimws(x)
  x <- gsub("_", " ", x)
  x <- gsub("\\s+", " ", x)
  tolower(x)
}

parse_len_to_sec <- function(x) {
  x <- trimws(x)
  parts <- strsplit(x, ":", fixed = TRUE)
  sapply(parts, function(p) {
    if (length(p) != 2) return(NA_real_)
    as.numeric(p[1]) * 60 + as.numeric(p[2])
  })
}

fix_url <- function(u) {
  u <- trimws(u)
  if (startsWith(u, "//")) u <- paste0("https:", u)
  u
}

# v3 request with a custom User-Agent (helps with some anti-bot filters)
xc_get_page_v3 <- function(query, page = 1, per_page = 50) {
  base <- "https://xeno-canto.org/api/3/recordings"
  url <- paste0(
    base,
    "?query=", utils::URLencode(query, reserved = TRUE),
    "&page=", page,
    "&per_page=", per_page,
    "&key=", utils::URLencode(xc_key, reserved = TRUE)
  )
  
  h <- curl::new_handle()
  curl::handle_setheaders(h,
                          "User-Agent" = "WorkshopNRW/1.0 (R; polite; contact: lucas)"
  )
  
  raw <- curl::curl_fetch_memory(url, handle = h)$content
  jsonlite::fromJSON(rawToChar(raw), flatten = TRUE)
}

xc_search_v3 <- function(query, max_pages = 2) {
  all <- list()
  for (p in 1:max_pages) {
    res <- xc_get_page_v3(query, page = p, per_page = 50)
    recs <- res$recordings
    if (is.null(recs) || nrow(recs) == 0) break
    all[[length(all) + 1]] <- recs
    if (!is.null(res$numPages) && p >= as.integer(res$numPages)) break
    Sys.sleep(0.4) # be polite
  }
  if (length(all) == 0) return(data.frame())
  do.call(rbind, all)
}

prefer_best <- function(df) {
  if (nrow(df) == 0) return(df)
  
  # Prefer A/B quality, no ND license, longer, and if possible NRW-like locality text
  q <- if ("q" %in% names(df)) df$q else NA_character_
  len <- if ("length" %in% names(df)) parse_len_to_sec(df$length) else NA_real_
  loc <- if ("loc" %in% names(df)) norm_key(df$loc) else ""
  lic <- if ("lic" %in% names(df)) tolower(df$lic) else ""
  
  q_score <- ifelse(q == "A", 0, ifelse(q == "B", 1, ifelse(q == "C", 2, 3)))
  nrw_bonus <- ifelse(grepl("nordrhein|westfalen|nrw|koeln|köln|ruhr|rhein", loc), 0, 1)
  nd_penalty <- ifelse(grepl("/nd/", lic), 1, 0)
  
  ord <- order(nd_penalty, q_score, nrw_bonus, -len, na.last = TRUE)
  df[ord, , drop = FALSE]
}

dl_dir <- file.path(out_dir, "mp3")
dir.create(dl_dir, showWarnings = FALSE)

meta_all <- list()

for (sp in species_A) {
  message("Searching: ", sp)
  
  # v3 requires at least one tag; we use multiple tags
  query <- paste0('sp:"', tolower(sp), '" cnt:"germany" type:song')
  
  df <- xc_search_v3(query, max_pages = 2)
  
  if (nrow(df) == 0) {
    warning("No results for: ", sp)
    next
  }
  
  # Filter by minimum length
  if ("length" %in% names(df)) {
    df$len_sec <- parse_len_to_sec(df$length)
    df <- df[!is.na(df$len_sec) & df$len_sec >= clip_len_sec, , drop = FALSE]
  }
  if (nrow(df) == 0) {
    warning("No recordings >= clip length for: ", sp)
    next
  }
  
  df <- prefer_best(df)
  pick <- df[1, , drop = FALSE]
  
  file_url <- fix_url(pick$file)
  rec_id <- pick$id
  out_file <- file.path(dl_dir, paste0("A_", gsub(" ", "_", sp), "_XC", rec_id, ".mp3"))
  
  curl::curl_download(file_url, out_file, quiet = TRUE)
  
  meta_all[[length(meta_all) + 1]] <- data.frame(
    species = sp,
    id = rec_id,
    english = if ("en" %in% names(pick)) pick$en else NA,
    recordist = if ("rec" %in% names(pick)) pick$rec else NA,
    country = if ("cnt" %in% names(pick)) pick$cnt else NA,
    locality = if ("loc" %in% names(pick)) pick$loc else NA,
    type = if ("type" %in% names(pick)) pick$type else NA,
    quality = if ("q" %in% names(pick)) pick$q else NA,
    length = if ("length" %in% names(pick)) pick$length else NA,
    url = if ("url" %in% names(pick)) fix_url(pick$url) else NA,
    license = if ("lic" %in% names(pick)) pick$lic else NA,
    file_url = file_url,
    local_mp3 = out_file,
    stringsAsFactors = FALSE
  )
  
  Sys.sleep(0.4) # be polite
}

meta_df <- if (length(meta_all)) do.call(rbind, meta_all) else data.frame()
meta_csv <- file.path(out_dir, "GroupA_xenocanto_metadata.csv")
write.csv(meta_df, meta_csv, row.names = FALSE, fileEncoding = "UTF-8")
message("Saved metadata: ", meta_csv)

# Optional: trim + concat into ~3:27 mix (needs ffmpeg)
ff <- Sys.which("ffmpeg")
if (nzchar(ff) && nrow(meta_df) > 0) {
  clips_dir <- file.path(out_dir, "clips_25s")
  dir.create(clips_dir, showWarnings = FALSE)
  
  silence_wav <- file.path(clips_dir, "silence_1s.wav")
  system2(ff, c("-y", "-f", "lavfi", "-i", "anullsrc=r=44100:cl=mono", "-t", gap_sec, silence_wav),
          stdout = TRUE, stderr = TRUE)
  
  clip_paths <- c()
  for (i in seq_len(nrow(meta_df))) {
    in_mp3 <- meta_df$local_mp3[i]
    out_wav <- file.path(clips_dir, paste0("clip_", sprintf("%02d", i), ".wav"))
    system2(ff, c("-y", "-ss", 0, "-t", clip_len_sec, "-i", in_mp3, "-ar", 44100, "-ac", 1, out_wav),
            stdout = TRUE, stderr = TRUE)
    clip_paths <- c(clip_paths, out_wav)
    if (i < nrow(meta_df)) clip_paths <- c(clip_paths, silence_wav)
  }
  
  list_file <- file.path(clips_dir, "concat_list.txt")
  writeLines(paste0("file '", clip_paths, "'"), list_file)
  
  mix_wav <- file.path(out_dir, "GroupA_mix.wav")
  system2(ff, c("-y", "-f", "concat", "-safe", 0, "-i", list_file, "-c", "copy", mix_wav),
          stdout = TRUE, stderr = TRUE)
  
  message("Saved mix: ", mix_wav)
} else {
  message("ffmpeg not found or no files -> skipped trimming/concat.")
}
