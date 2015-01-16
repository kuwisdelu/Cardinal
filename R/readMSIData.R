
#### Read any MS imaging data file ####
## -----------------------------------

readMSIData <- function(file) {
	path <- normalizePath(file)
	ext <- file_ext(path)
	name <- basename(file_path_sans_ext(path))
	folder <- dirname(path)
	if ( is_Analyze_ext(ext) ) {
		readAnalyze(name=name, folder=folder)
	} else if ( is_imzML_ext(ext) ) {
		readImzML(name=name, folder=folder)
	}
}

file_path_sans_ext <- function(x) {
	sub("([^.]+)\\.[[:alnum:]]+$", "\\1", x)
}

file_ext <- function(x) {
	pos <- regexpr("\\.([[:alnum:]]+)$", x)
    ifelse(pos > -1L, substring(x, pos + 1L), "")
}

is_Analyze_ext <- function(x) {
	tolower(x) %in% c("img", "hdr", "t2m")
}

is_imzML_ext <- function(x) {
	tolower(x) %in% c("ibd", "imzml")
}
