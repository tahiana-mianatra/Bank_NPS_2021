# First, check what we're working with
check_logo_sizes <- function() {
  banks <- c("BNI", "BMOI", "BOA", "BRED")
  for (b in banks) {
    path <- here::here("02_Input", paste0(b, "_logo.", ifelse(b %in% c("BMOI", "BRED"), "jpg", "png")))
    if (file.exists(path)) {
      img <- image_read(path)
      info <- image_info(img)
      cat(b, ": ", info$width, "x", info$height, 
          " (ratio:", round(info$width/info$height, 2), ")\n")
    }
  }
}

check_logo_sizes()
