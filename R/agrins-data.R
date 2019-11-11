# Author: Robert J. Hijmans
# Sept 2018
# version 1
# license GPL3


.setExtension <- function(filename, value) {
	value <- trimws(value)
	if (value != "" & substr(value, 1, 1) != ".") {
		value <- paste(".", value, sep="") 
	}
	lfn <- nchar(filename)
	fname <- list()
	for (f in 1:length(filename)) {
		extstart <- -1
		for (i in lfn[f] : 2) {
			if (substr(filename[f], i, i) == ".") {
				extstart <- i
				break 
			}
		}
		if (extstart > 0 & (lfn[f] - extstart) < 8) {
			fname[f] <- paste(substr(filename[f], 1, extstart-1), value, sep="")
		} else { 
			fname[f] <- paste(filename[f], value, sep="")  
		}
	}
	return( unlist(fname) ) 
}   



.extension <- function(filename, value=NULL, maxchar=10) {
	if (!is.null(value)) {
		filename <- .setExtension(filename, value)
		return(filename)
	}   
	lfn <- nchar(filename)
	ext <- list()
	for (f in 1:length(filename)) {
		extstart <- -1
		for (i in lfn[f] : 2) {
			if (substr(filename[f], i, i) == ".") {
				extstart <- i
				break
			}
		}
		if (extstart > 0) {
			ext[f] <- substr(filename[f], extstart, lfn[f])
		} else { 
			ext[f] <- "" 
		}   
	}
	ext <- unlist(ext)
	ext[nchar(ext) > maxchar] <- ''
	return(ext)
}   


.get_data <- function(name, path, ext="") {
	name <- .setExtension(name, ext)
	fn <- system.file(file.path(path, name), package="agrins")
	if (!(file.exists(fn))) {
		stop(paste(name, "is not a valid data set name"))
	}
	x <- readRDS(fn)
 	if (class(x) == "PackedSpatVector") {
		x <- vect(x)
	} 
	x
}

.get_shp <- function(name, path) {
	name <- .setExtension(name, ".shp")
	fn <- system.file(file.path(path, name), package="agrins")
	if (!(file.exists(fn))) {
		stop(paste(name, "is not a valid data set name"))
	}
	terra::vect(fn)
}

.get_zip <- function(datadir, url, md5, pattern, nfiles) {
	dir.create(datadir, recursive=TRUE, showWarnings=FALSE)
	f <- file.path(datadir, basename(url))
	if (file.exists(f)) {
		if (tools::md5sum(f) == md5) {
			ff <- list.files(pattern, path=datadir, full=TRUE)
			if (length(ff) == nfiles) {
				return(ff)
			} else {
				unzip(f, junkpaths=TRUE, exdir=datadir)
				ff <- list.files(pattern, path=datadir, full=TRUE)
				return(ff)
			}
		}
	}
	tmpfile <- tempfile()
	download.file(url, tmpfile, mode="wb")
	if (tools::md5sum(tmpfile) == md5) {
		file.copy(tmpfile, f)
		unzip(f, junkpaths=TRUE, exdir=datadir)
		ff <- list.files(pattern, path=datadir, full=TRUE)
		return(ff)
	} else {
		stop("download failed")
	}
}



.data_agrins <- function(name) {
	name <- tolower(name[1])
	n <- nchar(name)
	x <- .get_data(name) 
	if (class(x) == "data.frame") return(x)
	if (class(x) == "SpatVector") return(vect(x))
	x
}


data_ibli <- function(name, path) {
	name <- tolower(name)

	if (name == "marsabit_modis_ndvi") {
		if (missing(path)) {
			stop("provide a path (where the data should be stored)")
		}
		ff <- .get_zip(path, "https://gfc.ucdavis.edu/events/agrin/data/marsabit_modis_ndvi.zip", "16fe5aa1e4080725d0b6cce123f25bf4", "^MOD09A1.*ndvi\\.tif$", 360)
		return(ff)
	} else {
		.get_data(name, "ibli")
	}
}


data_rice <- function(name) {
	.get_data(name, "rice")
}


.data_crop <- function(name, path) {
	name <- tolower(name)
	if (name %in% c("xxxcrop_ref")) {
		.get_shp(name, "crop")	
	} else if (name == "sentinel1") {
		if (missing(path)) {
			stop("provide a path (where the data should be stored)")
		}
		ff <- .get_zip(path, "https://gfc.ucdavis.edu/events/agrins/data/sentinel1.zip", "5ded28dab9d73bccd6ea83379f730e66", "^S1A.*_Clip.tif$", 29)
		return(ff)
		
	} else {
		.get_data(name, "crop")
	}
}

