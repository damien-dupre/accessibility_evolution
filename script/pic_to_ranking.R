read_from_path <- function(file) {
  
  if (is.character(file)) {
    
    if (grepl("^https?://", file)) {
      con <- url(file, "rb")
      on.exit(close(con))
      pic <- raw()
      
      while (length(buf <- readBin(con, raw(), 1e+06))) {
        pic <- c(pic, buf)
      }
      
    }
    else if (file.exists(file)) {
      pic <- readBin(file, raw(), file.info(file)$size)
    }
    else {
      stop("Argument 'file' must be a file path, or url.")
    }
  }
  stopifnot(is.raw(pic))
  return(pic)
}

alpha_deleter <- function(arr) {
  
  if (dim(arr)[3] == 4) {
    
    # Helper matrix
    hm <- matrix(
      ncol = ncol(arr),
      nrow = nrow(arr),
      byrow = F,
      arr[,,4] > 0
    )
    
    # Subset array
    arr2 <- array(arr[hm], dim = c(1, length(arr[,,1][hm]), 4))
    
    return(arr2)
    
  } else {
    
    return(arr)
    
  }
  
}


pic_to_ranking <- function(file) {
  
  # Read to hex
  if (stringr::str_detect(tolower(file), "\\.svg")) pic <- suppressWarnings(pixmap::pixmapRGB(alpha_deleter(rsvg::rsvg(file))))
  if (stringr::str_detect(tolower(file), "\\.png")) pic <- suppressWarnings(pixmap::pixmapRGB(alpha_deleter(png::readPNG(read_from_path(file)))))
  if (stringr::str_detect(tolower(file), "\\.jpg|\\.jpeg")) pic <- suppressWarnings(pixmap::pixmapRGB(alpha_deleter(jpeg::readJPEG(read_from_path(file)))))
  if (stringr::str_detect(tolower(file), "\\.tif")) pic <- suppressWarnings(pixmap::pixmapRGB(alpha_deleter(tiff::readTIFF(read_from_path(file)))))
  if (stringr::str_detect(tolower(file), "\\.bmp")) pic <- suppressWarnings(pixmap::pixmapRGB(alpha_deleter(bmp::read.bmp(file))))
  
  val <- grDevices::rgb(pic@red, pic@green, pic@blue)
  
  # Ranking
  result <- tibble::tibble(
    col_hex = names(sort(table(val), decreasing = TRUE)),
    col_freq = as.vector(sort(table(val), decreasing = TRUE))
  )
  
  result$col_share <- result$col_freq / sum(result$col_freq)
  
  return(result)
  
}