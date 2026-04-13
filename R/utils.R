.get_pal <- function(pal, val) {
  return(pal)
}


#' @importFrom SpatialExperiment imgData
.sub_imgData <- function(spe, sample_ids, image_ids) {
    .get_img_idx <- SpatialExperiment:::.get_img_idx
    if (is.null(image_ids)) {
        # default to first available image for each sample
        idx <- .get_img_idx(spe, TRUE, NULL)
    } else {
        if (length(image_ids) == 1) {
            idx <- .get_img_idx(spe, TRUE, image_ids)
        } else {
            stopifnot(length(image_ids) == length(sample_ids))
            idx <- mapply(s = sample_ids, i = image_ids,
                          function(s, i) .get_img_idx(spe, s, i))
        }
    }
    imgData(spe)[idx, ]
}


.y_reverse <- function(df, ix, y, img) {
    y_tmp <- df[ix, y]
    if (!is.null(img)) {
        y_tmp <- nrow(img) - y_tmp
    } else {
        y_tmp <- max(y_tmp) - y_tmp
    }
    df[ix, y] <- y_tmp
    return(df)
}
