require(sfsmisc)
require(raster)
#' Convert to data.frame, without factors
#'
#' Shortcut for: \code{as.data.frame(x, row.names=NULL, stringsAsFactors=FALSE)}
#'
#' This function is useful for dealing with errors due to
#' automatic conversion of some columns to factors.  Another solution may be to prepend
#' \code{options(stringsAsFactors = FALSE)} at the start of one's script, to turn off all default stringsAsFactors silliness.
#'
#' @param x matrix or other object transformable to data.frame
#' @return data.frame
#' @export
#' @examples
#' x = matrix(c(1,2,3,4,5,6), nrow=3, ncol=2)
#' adf(x)
adf <- function(x)
	{
	return(as.data.frame(x, row.names=NULL, stringsAsFactors=FALSE))
	}
#######################################################
# slashslash:
#######################################################
#' Remove double slash (slash a slash)
#'
#' Shortcut for: \code{gsub(pattern="//", replacement="/", x=tmpstr)}
#'
#' This function is useful for removing double slashes that can
#' appear in full pathnames due to inconsistencies in trailing
#' slashes in working directories etc.
#'
#' @param tmpstr a path that you want to remove double slashes from
#' @return outstr a string of the fixed path
#' @export
#' @examples
#' tmpstr = "/Library/Frameworks/R.framework/Versions/2.15/Resources/library/
#' MOD03.A2016209.0515.005.NRT.hdf"
#'
#' outstr = slashslash(tmpstr)
#' outstr
#'
slashslash <- function(tmpstr)
	{
	outstr = gsub(pattern="//", replacement="/", x=tmpstr)
	return(outstr)
	}
#######################################################
# extract_fn_from_path:
#######################################################
#' Get the filename from a path
#'
#' The filename is split on slashes, and the last item is taken; this should be just
#' the filename.
#'
#' @param fn_with_path The filename, with partial or full path
#' @return \code{fn} The extracted filename
#' @export
#' @examples
#' fn_with_path = "/Library/Frameworks/R.framework/Versions/2.15/Resources/library/
#' MOD021KM.A2016209.0515.005.NRT.hdf"
#' extract_fn_from_path(fn_with_path)
#'
extract_fn_from_path <- function(fn_with_path)
	{
	words = strsplit(fn_with_path, split="/")[[1]]
	fn = words[length(words)]
	return(fn)
	}

#######################################################
# check_for_matching_geolocation_files_mod02nrt:
#######################################################
#' Checks that every MODIS calibrated radiance project HDF has a matching MOD03 file
#'
#' Each MOD02 calibrated radiance product file requires a corresponding
#' MOD03 geolocation file to be successfully processed with the MRTSwath tool.
#'
#' MRTSwath is the MRT (MODIS Reprojection Tool) for the MODIS
#'
#' E.g. this calibrated radiance file:
#'
#' MOD021KM.A2016209.0515.005.NRT.hdf
#'
#' ...goes with this corresponding geolocation file:
#'
#' MOD03.A2016209.0515.005.NRT.hdf
#'
#' ...which is a large file (~30 MB) containing detailed information
#' on the position, tilt, etc. of the MODIS satellite.
#' MRTSwath tool needs one of each, however.
#'
#' @param moddir the string describing the directory containing the MOD02 and MOD03 files; both must be in the same directory.  Default: getwd(), which gives the present working directory.
#' @param modtxt the text string indicating which HDF files are the MODIS calibrated radiance product (or hypothetically, other product). Default: MOD02 (MODIS calibrated radiance product)
#' @param geoloctxt the text string indicating which HDF files are the MODIS geolocation files (or hypothetically, another set of files). Default: MOD03
#' @param return_geoloc if TRUE, return the list of unmatched geolocation files (e.g. MOD03 )
#' @param return_product if TRUE, return the list of unmatched product files (e.g. MOD02)
#' @return data.frame of matching files; or a list of non-matching files, if \code{return_geoloc} or \code{return_product} are TRUE.
#' @export

#' @author Rishabh Gupta \email{rishabh.uk@gmail.com}
#' @examples
#' # Check your working directory
#' moddir = getwd()
#'
#' # Here are some example MODIS files in mod02nrt/extdata/
#' # Code excluded from CRAN check because it depends on modiscdata
#' \dontrun{
#' library(devtools)
#' library(modiscdata)
#' moddir = system.file("extdata/2002raw/", package="modiscdata")
#'
#' # You need to have some e.g. MOD files in it (from the MODIS-TERRA platform)
#' list.files(path=moddir, pattern="MOD")
#' list.files(path=moddir, pattern="MOD")
#'
#' # Check for matches (for MODIS-TERRA platform)
#' check_for_matching_geolocation_files_mod02nrt(moddir=moddir, modtxt="MOD02", geoloctxt="MOD03",
#'  return_geoloc=FALSE, return_product=FALSE)
#' }
#'
check_for_matching_geolocation_files_mod02nrt <- function(moddir=getwd(), modtxt="MOD02", geoloctxt="MOD03", return_geoloc=FALSE, return_product=FALSE)
	{

	# Get fns with suffix, from either directory or fns list
	# Do NOT use dot in the suffix
	get_fns_matching_txt <- function(tmpdir=NULL, fns=NULL, text_to_match = NULL, returnfullnames=TRUE)
		{
		# If tmpdir is NOT null, get those files from list.files.
		if (is.null(tmpdir) == FALSE)
			{
			fns = list.files(tmpdir, full.names=returnfullnames)
			fns_without_paths = list.files(tmpdir, full.names=FALSE)
			}

		# Return true/false for matched text
		TF = grepl(pattern=text_to_match, x=fns_without_paths)

		matching_fns = fns[TF]
		return(matching_fns)
		}



	mod03_fns = sort(slashslash(get_fns_matching_txt(moddir, text_to_match=geoloctxt)))
	mod02_fns = sort(slashslash(get_fns_matching_txt(moddir, text_to_match=modtxt)))
	#head(mod03_fns)
	#head(mod02_fns)

	# Check if you have the right # of files
	if (length(mod03_fns) == length(mod02_fns))
		{
		cat("\nProcessing ", length(mod03_fns), " files.\n", sep="")

		# Return the list, sorted
		fns_df = cbind(mod02_fns, mod03_fns)
		fns_df = adf(fns_df)

		return(fns_df)

		} else {
		cat("\nWARNING: You have ", length(mod02_fns), " ", modtxt, " files & ", length(mod03_fns), " ", geoloctxt, " files.\nWill attempt to find just the matching files", sep="")
		}


	# Get the datestring for each MOD03 file
	mod03_idstrings = rep(NA, times = length(mod03_fns))
	for (i in 1:length(mod03_fns))
		{
		words = strsplit(x=mod03_fns[i], split="\\.")[[1]]
		idnums = words[2:4]
		mod03_idstrings[i] = paste(idnums, sep="", collapse=".")

		}
	#mod03_idstrings

	# Get the datestring for each mod02 file
	mod02_idstrings = rep(NA, times = length(mod02_fns))
	for (i in 1:length(mod02_fns))
		{
		words = strsplit(x=mod02_fns[i], split="\\.")[[1]]
		idnums = words[2:4]
		mod02_idstrings[i] = paste(idnums, sep="", collapse=".")
		}
	#mod02_idstrings


	# Find which match
	mod02_in_mod03_TF = mod02_idstrings %in% mod03_idstrings
	mod02_fns_dropped = mod02_fns[mod02_in_mod03_TF == FALSE]
	mod02_fns = mod02_fns[mod02_in_mod03_TF == TRUE]
	mod02_idstrings = mod02_idstrings[mod02_in_mod03_TF == TRUE]

	mod03_in_mod02_TF = mod03_idstrings %in% mod02_idstrings
	mod03_fns_dropped = mod03_fns[mod03_in_mod02_TF == FALSE]
	mod03_fns = mod03_fns[mod03_in_mod02_TF == TRUE]
	mod03_idstrings = mod03_idstrings[mod03_in_mod02_TF == TRUE]


	# Correct // to / (if any)
	# Could also use slashslash
	mod03_fns = gsub(pattern="//", replacement="/", x=mod03_fns)
	mod02_fns = gsub(pattern="//", replacement="/", x=mod02_fns)
	mod02_fns_dropped = gsub(pattern="//", replacement="/", x=mod02_fns_dropped)
	mod03_fns_dropped = gsub(pattern="//", replacement="/", x=mod03_fns_dropped)

	# Check lengths (manual)
	length(mod02_fns)
	length(mod03_fns)
	length(mod02_idstrings)
	length(mod03_idstrings)
	sum(mod02_idstrings == mod03_idstrings)

	# Return the list or matching files, sorted
	fns_df = cbind(mod02_fns, mod03_fns)
	fns_df = adf(fns_df)

	# Print the dropped files
	cat("\n", sep="")
	cat("\nWarning: ", length(mod02_fns_dropped), " ", modtxt, " files dropped with no matches:\n", sep="")
	cat(head(mod02_fns_dropped), sep="\n")
	cat("...", sep="")
	cat("\n", sep="")

	cat("\nWarning: ", length(mod03_fns_dropped), " ", geoloctxt, " files dropped with no matches:\n", sep="")
	cat(head(mod03_fns_dropped), sep="\n")
	cat("...", sep="")
	cat("\n", sep="")

	# Return unmatched geolocation files, if desired
	if (return_geoloc == TRUE)
		{
		return(mod03_fns_dropped)
		}

	# Return unmatched product files, if desired
	if (return_product == TRUE)
		{
		return(mod02_fns_dropped)
		}

	# Otherwise, return the matched files...
	return(fns_df)
	}
#' @importFrom utils head write.table
NULL
	#######################################################
# write_MRTSwath_param_file_mod02nrt:
#######################################################
#' Write a parameter control file for MRTSwath
#'
#' MRTSwath is the "MODIS Reprojection Tool for swath products".  See:
#' \url{https://lpdaac.usgs.gov/tools/modis_reprojection_tool_swath}).
#'
#' If you want this function to use MRTSwath tool successfully, you should
#' add the directory with the MRTSwath executable to the default R PATH
#' by editing \code{~/.Rprofile}.
#'
#' This function hard-codes these options into the parameter file:\cr
#' * all the bands are extracted\cr
#' * the output file is a GeoTIFF\cr
#' * the output projection is Geographic (plain unprojected Latitude/Longitude)\cr
#' * the resampling is Nearest Neighbor (NN), which of course is the only one which makes sense when the pixels encode bytes that encode bits that encode discrete classification results, 0/1 error flags, etc.\cr
#'
#' MRTswath can do many other projections and output formats; users can modify this function to run those options.
#'
#' @param prmfn The name of the parameter/control file which will be the input to MRTSwath's \code{swath2grid} function.
#' @param tifsdir The directory to save the output TIF files in
#' @param modfn The filename of the MODIS data
#' @param geoloc_fn The filename of the corresponding geolocation file (annoyingly, this is a much larger
#' file than the data file!)
#' @param ul_lon Upper left (ul) longitude (x-coordinate) for subsetting
#' @param ul_lat Upper left (ul) latitude (y-coordinate) for subsetting
#' @param lr_lon Lower right (lr) longitude (x-coordinate) for subsetting
#' @param lr_lat Lower right (lr) latitude (y-coordinate) for subsetting
#' @return \code{prmfn} The name of the temporary parameter file
#' @export
#' @seealso \code{\link{run_swath2grid_mod02nrt}}
#' @seealso \url{http://landweb.nascom.nasa.gov/cgi-bin/QA_WWW/newPage.cgi?fileName=hdf_filename}
#'   @cite NASA2001
#' @author Rishabh Gupta \email{rishabh.uk@gmail.com}
#' @examples
#'
#' # Source MODIS files (both data and geolocation)
#' # Code excluded from CRAN check because it depends on modiscdata
#' \dontrun{
#' library(devtools)
#' library(modiscdata)
#' moddir = system.file("extdata/2002raw/", package="modiscdata")
#'
#' # Get the matching data/geolocation file pairs
#' fns_df = check_for_matching_geolocation_files_mod02nrt(moddir, modtxt="MOD02", geoloctxt="MOD03")
#' fns_df
#'
#' # Resulting TIF files go in this directory
#' tifsdir = getwd()
#'
#'
#' # Box to subset
#' ul_lat = 13
#' ul_lon = -87
#' lr_lat = 8
#' lr_lon = -82
#'
#' for (i in 1:nrow(fns_df))
#' 	{
#'
#' 	prmfn = write_MRTSwath_param_file_mod02nrt(prmfn="tmpMRTparams.prm", tifsdir=tifsdir,
#' 	 modfn=fns_df$mod02_fns[i], geoloc_fn=fns_df$mod03_fns[i], ul_lon=ul_lon, ul_lat=ul_lat,
#' 	  lr_lon=lr_lon, lr_lat=lr_lat)
#' 	print(scan(file=prmfn, what="character", sep="\n"))
#'
#' 	}
#' }
#'
write_MRTSwath_param_file_mod02nrt <- function(prmfn="tmpMRTparams.prm", tifsdir, modfn, geoloc_fn, ul_lon, ul_lat, lr_lon, lr_lat)
{
  # Initialize the list of lines in the parameter file
  prmfile = NULL
  pnum = 0
  prmfile[[(pnum=pnum+1)]] = 	" "

  # Input files
  prmfile[[(pnum=pnum+1)]] = 	paste("INPUT_FILENAME = ", modfn, sep="")
  prmfile[[(pnum=pnum+1)]] = 	" "
  prmfile[[(pnum=pnum+1)]] = 	paste("GEOLOCATION_FILENAME = ", geoloc_fn, sep="")
  prmfile[[(pnum=pnum+1)]] = 	" "
  prmfile[[(pnum=pnum+1)]] = 	paste("INPUT_SDS_NAME = EV_1KM_RefSB, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1; EV_1KM_RefSB_Uncert_Indexes, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1; EV_1KM_Emissive, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1; EV_1KM_Emissive_Uncert_Indexes, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1; EV_250_Aggr1km_RefSB, 1, 1; EV_250_Aggr1km_RefSB_Uncert_Indexes, 1, 1; EV_250_Aggr1km_RefSB_Samples_Used, 1, 1; EV_500_Aggr1km_RefSB, 1, 1, 1, 1, 1; EV_500_Aggr1km_RefSB_Uncert_Indexes, 1, 1, 1, 1, 1; EV_500_Aggr1km_RefSB_Samples_Used, 1, 1, 1, 1, 1; EV_Band26; EV_Band26_Uncert_Indexes", sep="")

  # Subset parameters
  prmfile[[(pnum=pnum+1)]] = 	" "
  prmfile[[(pnum=pnum+1)]] = 	paste("OUTPUT_SPATIAL_SUBSET_TYPE = LAT_LONG", sep="")
  prmfile[[(pnum=pnum+1)]] = 	paste("OUTPUT_SPACE_UPPER_LEFT_CORNER (LONG LAT) =", ul_lon, ul_lat, sep=" ")
  prmfile[[(pnum=pnum+1)]] = 	paste("OUTPUT_SPACE_LOWER_RIGHT_CORNER (LONG LAT) = ", lr_lon, lr_lat, sep=" ")


  # Output filename
  prmfile[[(pnum=pnum+1)]] = 	" "

  outfn = gsub(pattern=".hdf", replacement=".tif", modfn)
  outfn = extract_fn_from_path(fn_with_path=outfn)
  outfn = slashslash(paste(tifsdir, outfn, sep="/"))

  prmfile[[(pnum=pnum+1)]] = 	paste("OUTPUT_FILENAME = ", outfn, sep="")
  prmfile[[(pnum=pnum+1)]] = 	paste("OUTPUT_FILE_FORMAT = GEOTIFF_FMT", sep="")

  # Reprojection information (for Geographic Projection, with nearest-neighbor resampling)
  prmfile[[(pnum=pnum+1)]] = 	" "
  prmfile[[(pnum=pnum+1)]] = 	"KERNEL_TYPE (CC/BI/NN) = NN"
  prmfile[[(pnum=pnum+1)]] = 	" "
  prmfile[[(pnum=pnum+1)]] = 	"OUTPUT_PROJECTION_NUMBER = GEO"
  prmfile[[(pnum=pnum+1)]] = 	" "
  prmfile[[(pnum=pnum+1)]] = 	"OUTPUT_PROJECTION_PARAMETER = 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0"
  prmfile[[(pnum=pnum+1)]] = 	" "
  prmfile[[(pnum=pnum+1)]] = 	"OUTPUT_PROJECTION_SPHERE = 8"
  prmfile[[(pnum=pnum+1)]] = 	" "

  #prmfile


  # Write the MRTSwath tool parameter file
  write.table(x=prmfile, file=prmfn, append=FALSE, quote=FALSE, sep="\n", row.names=FALSE, col.names=FALSE)
  #moref(prmfn)

  return(prmfn)
}
#' @importFrom utils head write.table
NULL
#######################################################
# run_swath2grid_mod02nrt:
#######################################################
#' Run MRTSwath swath2grid tool
#'
#' MRTSwath is the "MODIS Reprojection Tool for swath products".  See:
#' \url{https://lpdaac.usgs.gov/tools/modis_reprojection_tool_swath}).
#'
#' If you want this function to use MRTSwath tool successfully, you should
#' add the directory with the MRTSwath executable to the default R PATH
#' by editing \code{~/.Rprofile}.
#'
#' @param mrtpath This is the path to the MRTSwath executable \code{swath2grid}. If your \code{~/.Rprofile}
#' file has the location of \code{swath2grid} in the PATH, then you can just use \code{mrtpath="swath2grid"}.
#' Otherwise, the user must provide the full path to swath2grid.
#' @param prmfn The name of the parameter/control file which will be the input to MRTSwath's \code{swath2grid} function.
#' @param tifsdir The directory to save the output TIF files in
#' @param modfn The filename of the MODIS data
#' @param geoloc_fn The filename of the corresponding geolocation file (annoyingly, this is a much larger
#' file than the data file!)
#' @param ul_lon Upper left (ul) longitude (x-coordinate) for subsetting
#' @param ul_lat Upper left (ul) latitude (y-coordinate) for subsetting
#' @param lr_lon Lower right (lr) longitude (x-coordinate) for subsetting
#' @param lr_lat Lower right (lr) latitude (y-coordinate) for subsetting
#' @return \code{cmdstr} The string giving the system command that ran \code{swath2grid}
#' @export
#' @seealso \code{\link{write_MRTSwath_param_file_mod02nrt}}
#' @seealso \url{http://landweb.nascom.nasa.gov/cgi-bin/QA_WWW/newPage.cgi?fileName=hdf_filename}
#'   @cite NASA2001
#' @examples
#' #######################################################
#' # Run MRTSwath tool "swath2grid"
#' #######################################################
#'
#' # Source MODIS files (both data and geolocation)
#' # Code excluded from CRAN check because it depends on modiscdata
#' \dontrun{
#' library(devtools)
#' library(modiscdata)
#' moddir = system.file("extdata/2002raw/", package="modiscdata")
#'
#' # Get the matching data/geolocation file pairs
#' fns_df = check_for_matching_geolocation_files(moddir, modtxt="MOD02", geoloctxt="MOD03")
#' fns_df
#'
#' # Resulting TIF files go in this directory
#' tifsdir = getwd()
#'
#'
#' # Box to subset
#' ul_lat = 13
#' ul_lon = -87
#' lr_lat = 8
#' lr_lon = -82
#'
#' for (i in 1:nrow(fns_df))
#' 	{
#'
#'	prmfn = write_MRTSwath_param_file_mod02nrt(prmfn="tmpMRTparams.prm", tifsdir=tifsdir,
#'	 modfn=fns_df$mod02_fns[i], geoloc_fn=fns_df$mod03_fns[i], ul_lon=ul_lon, ul_lat=ul_lat,
#'	  lr_lon=lr_lon, lr_lat=lr_lat)
#'	print(scan(file=prmfn, what="character", sep="\n"))
#'
#'	run_swath2grid_mod02nrt(mrtpath="swath2grid", prmfn="tmpMRTparams.prm", tifsdir=tifsdir,
#'	 modfn=fns_df$mod302_fns[i], geoloc_fn=fns_df$mod03_fns[i], ul_lon=ul_lon, ul_lat=ul_lat,
#'	  lr_lon=lr_lon, lr_lat=lr_lat)
#'
#' 	}
#'
#' list.files(tifsdir, pattern=".tif", full.names=TRUE)
#' }
#'
run_swath2grid_mod02nrt <- function(mrtpath="swath2grid", prmfn="tmpMRTparams.prm", tifsdir, modfn, geoloc_fn, ul_lon, ul_lat, lr_lon, lr_lat)
	{

	# Write the temporary parameter file
	prmfn = write_MRTSwath_param_file_mod02nrt(prmfn=prmfn, tifsdir=tifsdir, modfn=modfn, geoloc_fn=geoloc_fn, ul_lon=ul_lon, ul_lat=ul_lat, lr_lon=lr_lon, lr_lat=lr_lat)

	# Run MRTSwath tool (swath2grid)
	cmdstr = paste(mrtpath, " -pf=", prmfn, sep="")
	system(cmdstr)

	return(cmdstr)
	}
