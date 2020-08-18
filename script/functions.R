# osm geocoding api ------------------------------------------------------------
nominatim_osm <- function(address = NULL)
{
  if(suppressWarnings(is.null(address)))
    return(data.frame())
  tryCatch(
    d <- jsonlite::fromJSON( 
      gsub('\\@addr\\@', gsub('\\s+', '\\%20', address), 
           'http://nominatim.openstreetmap.org/search/@addr@?format=json&addressdetails=0&limit=1')
    ), error = function(c) return(data.frame(osm_address = NA, lng = NA, lat = NA))
  )
  if(!exists("d")) {
    print("FAILED")
    return(data.frame(osm_address = address, lng = NA, lat = NA, status = "FAILED"))
  } else if(length(d) == 0) {
    print("FAILED")
    return(data.frame(osm_address = address, lng = NA, lat = NA, status = "FAILED"))
  } else {
    print("SUCCESS")
    return(data.frame(
      osm_address = d$display_name, 
      lng = as.numeric(d$lon), 
      lat = as.numeric(d$lat), 
      status = "SUCCESS")
    )
  }
}

# osm feature list -------------------------------------------------------------
osm_tag_processing <- function(key){
  list_tags <- osmdata::available_tags(key)
  if (length(list_tags)>0){
    tibble::enframe(list_tags, name = NULL, value = "value")
  } else {
    tibble::tibble(value = NA)
  }
}

# density too far --------------------------------------------------------------
my_viz <- function (x, view = NULL, cond = list(), n.grid = 40, too.far = 0, 
                    col = NA, color = "heat", contour.col = NULL, se = -1, type = "link", 
                    plot.type = "persp", zlim = NULL, nCol = 50, ...) 
{
  fac.seq <- function(fac, n.grid) {
    fn <- length(levels(fac))
    gn <- n.grid
    if (fn > gn) 
      mf <- factor(levels(fac))[1:gn]
    else {
      ln <- floor(gn/fn)
      mf <- rep(levels(fac)[fn], gn)
      mf[1:(ln * fn)] <- rep(levels(fac), rep(ln, fn))
      mf <- factor(mf, levels = levels(fac))
    }
    mf
  }
  dnm <- names(list(...))
  v.names <- names(x$var.summary)
  if (is.null(view)) {
    k <- 0
    view <- rep("", 2)
    for (i in 1:length(v.names)) {
      ok <- TRUE
      if (is.matrix(x$var.summary[[i]])) 
        ok <- FALSE
      else if (is.factor(x$var.summary[[i]])) {
        if (length(levels(x$var.summary[[i]])) <= 1) 
          ok <- FALSE
      }
      else {
        if (length(unique(x$var.summary[[i]])) == 1) 
          ok <- FALSE
      }
      if (ok) {
        k <- k + 1
        view[k] <- v.names[i]
      }
      if (k == 2) 
        break
    }
    if (k < 2) 
      stop("Model does not seem to have enough terms to do anything useful")
  }
  ok <- TRUE
  for (i in 1:2) if (is.factor(x$var.summary[[view[i]]])) {
    if (length(levels(x$var.summary[[view[i]]])) <= 1) 
      ok <- FALSE
  }
  else {
    if (length(unique(x$var.summary[[view[i]]])) <= 1) 
      ok <- FALSE
  }
  if (!ok) 
    stop(gettextf("View variables must contain more than one value. view = c(%s,%s).", 
                  view[1], view[2]))
  if (is.factor(x$var.summary[[view[1]]])) 
    m1 <- fac.seq(x$var.summary[[view[1]]], n.grid)
  else {
    r1 <- range(x$var.summary[[view[1]]])
    m1 <- seq(r1[1], r1[2], length = n.grid)
  }
  if (is.factor(x$var.summary[[view[2]]])) 
    m2 <- fac.seq(x$var.summary[[view[2]]], n.grid)
  else {
    r2 <- range(x$var.summary[[view[2]]])
    m2 <- seq(r2[1], r2[2], length = n.grid)
  }
  v1 <- rep(m1, n.grid)
  v2 <- rep(m2, rep(n.grid, n.grid))
  newd <- data.frame(matrix(0, n.grid * n.grid, 0))
  for (i in 1:length(x$var.summary)) {
    ma <- cond[[v.names[i]]]
    if (is.null(ma)) {
      ma <- x$var.summary[[i]]
      if (is.numeric(ma)) 
        ma <- ma[2]
    }
    if (is.matrix(x$var.summary[[i]])) 
      newd[[i]] <- matrix(ma, n.grid * n.grid, ncol(x$var.summary[[i]]), 
                          byrow = TRUE)
    else newd[[i]] <- rep(ma, n.grid * n.grid)
  }
  names(newd) <- v.names
  newd[[view[1]]] <- v1
  newd[[view[2]]] <- v2
  fv <- predict.gam(x, newdata = newd, se.fit = TRUE, type = type)
  z <- fv$fit
  if (too.far > 0) {
    ex.tf <- exclude.too.far(v1, v2, x$model[, view[1]], 
                             x$model[, view[2]], dist = too.far)
    fv$se.fit[ex.tf] <- fv$fit[ex.tf] <- NA
  }
  if (is.factor(m1)) {
    m1 <- as.numeric(m1)
    m1 <- seq(min(m1) - 0.5, max(m1) + 0.5, length = n.grid)
  }
  if (is.factor(m2)) {
    m2 <- as.numeric(m2)
    m2 <- seq(min(m1) - 0.5, max(m2) + 0.5, length = n.grid)
  }
  if (se <= 0) {
    old.warn <- options(warn = -1)
    av <- matrix(c(0.5, 0.5, rep(0, n.grid - 1)), n.grid,
                 n.grid - 1)
    options(old.warn)
    max.z <- max(z, na.rm = TRUE)
    z[is.na(z)] <- max.z * 10000
    z <- matrix(z, n.grid, n.grid)
    surf.col <- t(av) %*% z %*% av
    surf.col[surf.col > max.z * 2] <- NA
    if (!is.null(zlim)) {
      if (length(zlim) != 2 || zlim[1] >= zlim[2])
        stop("Something wrong with zlim")
      min.z <- zlim[1]
      max.z <- zlim[2]
    }
    else {
      min.z <- min(fv$fit, na.rm = TRUE)
      max.z <- max(fv$fit, na.rm = TRUE)
    }
    z <- matrix(fv$fit, n.grid, n.grid)
  }
  dat <- as.data.frame(z)
  rownames(dat) <- m1
  colnames(dat) <- m2
  
  dat %>%
    tibble::rownames_to_column("lng") %>%
    tidyr::gather(lat,value, -lng) %>%
    dplyr::mutate(lng = as.numeric(lng)) %>%
    dplyr::mutate(lat = as.numeric(lat))
}

# osm feature distance ---------------------------------------------------------
osm_feature_dist <- function(Key,Value) {
  
  variable_osm <- osmdata::opq('Dublin, Ireland') %>%
    osmdata::add_osm_feature(key = Key, value = Value)
  
  # variable_osm %>%
  #   osmdata::osmdata_sp() %>%
  #   magrittr::use_series(osm_points) %>%
  #   sp::plot()
  
  variable_sf <- variable_osm %>%
    osmdata::osmdata_sf() %>%
    magrittr::use_series(osm_lines) %>%
    magrittr::use_series(geometry) %>%
    st_sf()
  
  if (nrow(variable_sf) == 0) {
    
    return(NA)
    
  } else {
    st_crs(variable_sf) <- st_crs(4326) # assign crs
    variable_sf <- st_transform(variable_sf, crs = 32721) # transform
    variable_sf <- st_combine(variable_sf)
    
    # distance
    
    return(as.numeric(st_distance(x = data_sf, y = variable_sf)))
    
  }
  
}