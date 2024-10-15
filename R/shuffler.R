
#returns a vector of the names of the pngs
open_pdf <- function(file_name, png_names = c(),
                     current_page = 1,
                     last_page = pdftools::pdf_info(file_name)$pages) {

  #creates copy
  file.copy(file_name, "holder_copy.pdf")

  #render the next page
  bitmap <- pdftools::pdf_render_page("holder_copy.pdf", page = current_page, dpi = 300)
  png::writePNG(bitmap, paste0(current_page,file_name,".png"))

  #add file name to vector list
  png_names[current_page] <- paste0(current_page,file_name,".png")

  #deletes copy
  file.remove("holder_copy.pdf")

  #moves on to next iteration
  if (current_page + 1 <= last_page)
    open_pdf(file_name, png_names ,current_page + 1)
  else
    return(png_names)
}

#cuts pngs, returns a vector of double size
cut_pngs <- function(png_vector) {
  #Should end up double the size of the png_vector
  output_vector <- c()

  for (i in 1:length(png_vector)) {

    #Skips if the file does not exist
    if (!file.exists(png_vector[i]))
      next

    magick_arg <- magick::image_read(png_vector[i])

    pairing_vector <- c(magick::image_crop(magick_arg,"2550x1650"),
                        magick::image_crop(magick_arg,"2550x1650+0+1650"))

    #Generate images
    magick::image_write(pairing_vector[1], paste0(png_vector[i],"A.png"))
    magick::image_write(pairing_vector[2], paste0(png_vector[i],"B.png"))

    #Delete the original image
    file.remove(png_vector[i])

    output_vector <- c(output_vector, paste0(png_vector[i],"A.png"), paste0(png_vector[i],"B.png"))
  }

  return(output_vector)
}

stack_images <- function(image1,image2) {

  image_vector <- c(magick::image_read(image1), magick::image_read(image2))

  output <- magick::image_append(image_vector, stack=TRUE)
  magick::image_write(output, format = "png" ,paste0(image1,image2,".png"))
  file.remove(image1)
  file.remove(image2)
  return(paste0(image1,image2,".png"))
}

#Will shuffle based on Pi algorithm in the future
pi_shuffle <- function(vector, 
seed = as.numeric(Sys.time())) {
  vector2 <- sample(vector, size = length(vector))
}

#stitches a vector of filenames into a pdf
create_new_pdf <- function(vector) {
  magick::image_write(magick::image_read(vector), format = "pdf", "output.pdf")

  #Dumpster the files
  for (i in 1:length(vector))
    file.remove(vector[i])
}

#' Takes in and shuffles demo pdfs
#'
#' @param visual_pdf Variable for Visual Demo Pdf
#' @param mental_pdf Variable for Mental Demo Pdf
#'
#' @return None (invisible `NULL`)
#' @export
#'
#' @examples
#' shuffle_blue_cards("1.pdf","2.pdf")
shuffle_blue_cards <- function(visual_pdf, mental_pdf) {
  #open and move into one vector
  full_png_vector <- c(open_pdf(visual_pdf), open_pdf(mental_pdf))

  #split up the images
  full_cut_images <- cut_pngs(full_png_vector)

  #Reassemble vector
  reassemble <- c()
  half_length <- length(full_cut_images)/2
  for (i in 1:half_length) {
    pairing_vector <- c()
    tempname <- stack_images(full_cut_images[i], full_cut_images[i+half_length])
    reassemble <- c(reassemble, tempname)
  }

  #Output
  create_new_pdf(pi_shuffle(reassemble))
  message("Output created")

  invisible(NULL)
}
