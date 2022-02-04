// This script is used to develope spectral and topographic raster derivatives for the s2026 initiative
// The final workig script was completed on 2/4/2022
// This is an archieve of the original script

// Imports
// var chaneFree: Image projects/teui-292214/assets/US_Landsat_ChangeFree
// var hucs: Table "HUC12: USGS Watershed Boundary Dataset of Subwatersheds L 12"
// var Elevation Data: Image USGS/3DEP/10,
//var table : importyour asset (project boundary shapefile)


// hhttps://code.earthengine.google.com/00cc6244e2afbd7342a4e1d995d82213
// Calculate Topographic Derivatives in GEE using the TAGEE functions : https://github.com/zecojls/tagee
// Calculate Spectral Derivatives 

/////////////////////////////////////////////////////////////////////////////////////
// 1. IMPORT LIBRARIES 
/////////////////////////////////////////////////////////////////////////////////////
// Do Not Change the following vars : TAGEE, getImagesLib, outputFolder, compositeArea
var TAGEE = require('users/joselucassafanelli/TAGEE:TAGEE-functions');
var getImagesLib = require('users/USFS_GTAC/modules:getImagesLib2.js');
var outputFolder = 's2026-swssr-gee-cov-bucket';
//Your study Area -> this should be a shapefile imported as an asset
//This script will grab all HUC12 units that intersect this AOI, buffer the merged HUC12 -> this is output AOI
var compositeArea = table; //table for asset import geometry for polygon drawing

/////////////////////////////////////////////////////////////////////////////////////
// 2. USER EDITABLE PARAMETERS: ESSENTIAL PARAMETERS TO LOOK THROUGH & EDIT
/////////////////////////////////////////////////////////////////////////////////////

//Name to tag, to seperate cov by soil survey area ID, should match the folder name in the bucket
// e.g. 'ca793'
var nameTag = 'ca695';

//FOR TOPO: Resolution of exported topographic derivatives
var scale = 10;

// CRS of exported derivatives - this is the national standard, better to leave in 5070
var crs = 'EPSG:5070';

//Parameters for Spectral data processing:

// FOR SPECTRAL: Specify scale if transform is null 
args.scale = 10;//null;

// FOR SPECTRAL: Specify transform if scale is null and snapping to known grid is needed
var transform =null;// [10,0,-2361915.0,0,-10,3177735.0];

//Which sensor do you want to use for spectral data? If useSentinel is set to true, this script will grab sentinel2 data
//if useSentinel is set to false, this script will pull Landsat data 
var useSentinel = true; 

// Update the startJulian and endJulian variables to indicate your seasonal 
// constraints. This supports wrapping for tropics and southern hemisphere.
// If using wrapping and the majority of the days occur in the second year, the system:time_start will default 
// to June 1 of that year.Otherwise, all system:time_starts will default to June 1 of the given year
// startJulian: Starting Julian date 
// endJulian: Ending Julian date
args.startJulian = 152;
args.endJulian = 181; 

// Specify start and end years for all analyses
// More than a 3 year span should be provided for time series methods to work 
// well. If using Fmask as the cloud/cloud shadow masking method, or providing
// pre-computed stats for cloudScore and TDOM, this does not 
// matter
args.startYear = 2019;
args.endYear = 2021;

// whether to create NDVI yearly composite timeseries (will export NDVI composite for each year within range)
//(this has no bearing on whether a multiyear NDVI composite will be created - determine this by setting param exportNDVI)
var createNDVI_timeseries = true;

//If true, this script will use a Landsat Composite created from Imagery from 1985 - 2019, with disturbance removed 
//i.e. imagery will be source from pre-disturbance only. 
//Use this option if a) 30 m Landsat imagery is acceptable and b) the years of imagery aren't important 
// Note that this product is generally going to be noisier than if sourcing imagery for compositing from recent years 
//This product is pre-computed so no spectral collection parameters must be set beyond the Essential parameters in this section
var useDisturbanceFreeLandsat = false;


// list of spectral indices to export: Enter True if you want to export the names index
//NOTE: depending on Sensor that is selected (S2 vs Landsat), some bands may not be exported (if they aren't available)
// e.g. NIR2 doesnt exist for Landsat

// Spectral Bands
var exportRed = true;
var exportGreen = true;
var exportBlue = true;
var exportNIR = true;
var exportNIR2 = true;
//Red Edge 1
var exportRE1= true;
//Red Edge 2
var exportRE2 = true;
//Red Edge 3
var exportRE3 = true;
var exportSWIR1 = true;
var exportSWIR2 = true;

/// Spectral Indicies

//Normalized Difference Vegetation Index (NIR, Red)
var exportNDVI = true;
//Normalized Burn Ratio (NIR, SWIR2)
var exportNBR = true;
//Bare Soil Index (((SWIR1 + RED) - (NIR + BLUE)) / ((SWIR1 + RED) + (NIR + BLUE)))
var exportBSI = true;
//Normalized Difference Mineral Index (NIR, SWIR1)
var exportNDMI = true;
//Normalized Difference red,SWIR2
var exportNDII = true;
//Normalized Difference green,SWIR1
var exportNDSI = true;
//Hydroxyl Index (SWIR1 / SWIR2)
var exportHI = true;
//Enhanced Vegetation Index
var exportEVI = true;
//Soil Adjusted Vegetation Index
var exportSAVI = true;
//Normalized Difference Carbon Index (red, green)
var exportNDCI = true;
//Normalized Difference Fraction Index (SWIR1, NIR)
var exportNDFI = true;
//Gypsum Index
var exportGyp = true;
//fe oxide Index
var exportFeox = true;
//Clay
var exportClay = true;
//Rock Index
var exportRock = true;
// Natric Index
var exportNat = true;
//Water Index
var exportWI = true;
//MSAVI2
var exportMSAVI2 = true;

//Tasseled Cap Brightness (1st component)
var exportBrightness = true;
//Tasseled Cap Greenness (2nd component)
var exportGreenness = true;
//Tasseled Cap Wetness (3rd component)
var exportWetness = true;
//Tasseled Cap 4th component
var exportTcap4 = true;
//Tasseled Cap 5th component
var exportTcap5 = true;
//Tasseled Cap 6th component)
var exportTcap6 = true;



// List of Topographic Layers to export
var exportElevation = true;
var exportSlope = true;
var exportAspect = true;
var exportHillshade = true;
var exportNorthness = true;
var exportEastness = true;
var exportHorizontalCurvature = true;
var exportVerticalCurvature = true;
var exportMeanCurvature = true;
var exportGaussianCurvature = true;
var exportMinimalCurvature = true;
var exportMaximalCurvature = true;
var exportShapeIndex = true;


/////////// ** END OF ESSENTIAL PARAMETERS ** /////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
// 3. USER-EDITABLE PARAMETERS: Not essential to check/change these
///////////////////////////////////////////////////////////////////////////////

// Specify an annual buffer to include imagery from the same season 
// timeframe from the prior and following year. timeBuffer = 1 will result 
// in a 3 year moving window. If you want single-year composites, set to 0
args.timebuffer =0;

// Specify the weights to be used for the moving window created by timeBuffer
// For example- if timeBuffer is 1, that is a 3 year moving window
// If the center year is 2000, then the years are 1999,2000, and 2001
// In order to overweight the center year, you could specify the weights as
// [1,5,1] which would duplicate the center year 5 times and increase its weight for
// the compositing method. If timeBuffer = 0, set to [1]
args.weights = [1];

// Choose medoid or median compositing method. 
// Median tends to be smoother, while medoid retains 
// single date of observation across all bands
// The date of each pixel is stored if medoid is used. This is not done for median
// If not exporting indices with composites to save space, medoid should be used
args.compositingMethod = 'medoid';

// Choose Top of Atmospheric (TOA) or Surface Reflectance (SR) 
// SR S2 data also has a terrain correction applied which may or may not be best depending on how you are using the data
// If using data from humid climates, terrain correction can be useful. Since vegetation types differ more with respect to slope/aspect 
// in dryer climates, terrain correction can remove some of the signal in dryer climates.  In higher latitudes terrain correction can fail.
args.toaOrSR = 'SR';

//S2 only: Whether to convert S2 images from the military grid reference system(MGRS) tiles to daily mosaics to avoid arbitrary
//MGRS tile artifacts or not. In most cases, it is best to set this to true.
args.convertToDailyMosaics = true;


//only relevant if Landsat is selected:
// Choose whether to include Landat 7
// Generally only included when data are limited
args.includeSLCOffL7 = false;

//only relevant if Landsat is selected:
// Whether to defringe L4 and L5
// Landsat 4 and 5 data have fringes on the edges that can introduce anomalies into 
// the analysis.  This method removes them, but is somewhat computationally expensive
args.defringeL5 = true;

// Choose cloud/cloud shadow masking method
// Choices are a series of booleans for applyQABand (S2),cloudScore, TDOM, applyShadowShift (S2), 
// and elements of Fmask (LS)
// Fmask masking options will run fastest since they're precomputed
// Fmask cloud mask is generally very good, while the fMask cloud shadow
// mask isn't great. TDOM tends to perform better than the Fmask cloud shadow mask. cloudScore 
// is usually about as good as the Fmask cloud mask overall, but each fails in different instances.
// CloudScore runs pretty quickly, but does look at the time series to find areas that 
// always have a high cloudScore to reduce commission errors- this takes some time
// and needs a longer time series (>5 years or so)
// This an be turned off by setting "performCloudScoreOffset" to false
// The cloud probability (S2) is provided as a pre-computed asset and seems better than cloudScore.
// The cloudScoreThresh is applied to both the cloudScore and cloud probability as they work in a similar manner
// TDOM also looks at the time series and will need a longer time series
// If pre-computed cloudScore offsets and/or TDOM stats are provided below, cloudScore
// and TDOM will run quite quickly and a long time sereies is not needed 
// QA band method is fast but is generally awful- don't use if you like good composites
// Shadow shift is intended if you don't have a time series to use for TDOM or just want individual images - best not to use this method
// It will commit any dark area that the cloud mask is cast over (water, hill shadows, etc)
args.applyCloudScore = false; //default for Landsat wrapper is T; default for S2 wrapper is F
args.applyFmaskCloudMask = true;
args.applyTDOM = true;
args.applyFmaskCloudShadowMask = true;
args.applyFmaskSnowMask = false;
args.applyQABand = false; //S2 only
args.applyShadowShift = false; //S2 Only

// S2 ONLY: Whether to use the pre-computed cloud probabilities to mask
// clouds for Sentinel 2
// This method works really well and should be used instead of cloudScore (applyCloudScore)
args.applyCloudProbability = true;

//S2 only: If cloudProbability is chosen, choose a threshold 
//(generally somewhere around 40-60 works well) - default was 40
args.cloudProbThresh = 40;

// If applyCloudScore is set to true
// cloudScoreThresh: lower number masks more clouds.  Between 10 and 30 generally 
// works best - default was 20
args.cloudScoreThresh = 20;

// Whether to find if an area typically has a high cloudScore
// If an area is always cloudy, this will result in cloud masking omission
// For bright areas that may always have a high cloudScore
// but not actually be cloudy, this will result in a reduction of commission errors
// This procedure needs at least 5 years of data to work well
// Precomputed offsets can be provided below
args.performCloudScoreOffset = true;

// If performCloudScoreOffset = true:
// Percentile of cloud score to pull from time series to represent a minimum for 
// the cloud score over time for a given pixel. Reduces comission errors over 
// cool bright surfaces. Generally between 5 and 10 works well. 0 generally is a
// bit noisy but may be necessary in persistently cloudy areas
args.cloudScorePctl = 10; 

// S2 only: Height of clouds to use to project cloud shadows
args.cloudHeights = ee.List.sequence(500,10000,500);

// zScoreThresh: If applyTDOM is true, this is the threshold for cloud shadow masking- 
// lower number masks out less. Between -0.8 and -1.2 generally works well
args.zScoreThresh = -1;

// shadowSumThresh:  If applyTDOM is true, sum of IR bands to include as shadows within TDOM and the 
//    shadow shift method (lower number masks out less)
args.shadowSumThresh = 0.35;

// contractPixels: The radius of the number of pixels to contract (negative 
//    buffer) clouds and cloud shadows by. Intended to eliminate smaller cloud 
//    patches to reduce commission and then buffer cloud edges to reduce omission
// (1.5 results in a -1 pixel buffer)(0.5 results in a -0 pixel buffer)
// (1.5 or 2.5 generally is sufficient)
args.contractPixels = 1.5; 

// dilatePixels: The radius of the number of pixels to dilate (buffer) clouds 
//    and cloud shadows by. Intended to include edges of clouds/cloud shadows 
//    that are often missed
// (1.5 results in a 1 pixel buffer)(0.5 results in a 0 pixel buffer)
// (2.5 or 3.5 generally is sufficient)
args.dilatePixels = 2.5;

// Choose the resampling method: 'aggregate','near', 'bilinear', or 'bicubic'
// Defaults to 'aggregate'

// Aggregate is generally useful for aggregating pixels when reprojecting instead of resampling
// A good example would be reprojecting S2 data to 30 m

// If method other than 'near' is chosen, any map drawn on the fly that is not
// reprojected, will appear blurred or not really represented properly
// Use .reproject to view the actual resulting image (this will slow it down)
args.resampleMethod = 'bilinear'; //could choose: near, bilinear, bicubic or aggregate

// The TDOM stats are the mean and standard deviations of the two bands used in TDOM
// By default, TDOM uses the nir and swir1 bands
var preComputedTDOMStats = getImagesLib.getPrecomputedTDOMStats();
// If available, bring in preComputed cloudScore offsets and TDOM stats
// Set to null if computing on-the-fly is wanted
// These have been pre-computed for all CONUS for Landsat and Setinel 2 (separately)
// and are appropriate to use for any time period within the growing season
// The cloudScore offset is generally some lower percentile of cloudScores on a pixel-wise basis
if (useSentinel){
  args.preComputedCloudScoreOffset = getImagesLib.getPrecomputedCloudScoreOffsets(args.cloudScorePctl).sentinel2;
  args.preComputedTDOMIRMean = preComputedTDOMStats.sentinel2.mean;
  args.preComputedTDOMIRStdDev = preComputedTDOMStats.sentinel2.stdDev;
}
else{
  args.preComputedCloudScoreOffset = getImagesLib.getPrecomputedCloudScoreOffsets(args.cloudScorePctl).landsat
  args.preComputedTDOMIRMean = preComputedTDOMStats.landsat.mean;
  args.preComputedTDOMIRStdDev = preComputedTDOMStats.landsat.stdDev;
}

// FOR Landsat: correctIllumination: Choose if you want to correct the illumination using
// Sun-Canopy-Sensor+C correction. Additionally, choose the scale at which the
// correction is calculated in meters.
args.correctIllumination = false;
args.correctScale = 250;//Choose a scale to reduce on- 250 generally works well


// CRS- must be provided.  
// Common crs codes: Web mercator is EPSG:4326, USGS Albers is EPSG:5070, 
// WGS84 UTM N hemisphere is EPSG:326+ zone number (zone 12 N would be EPSG:32612) and S hemisphere is EPSG:327+ zone number
args.crs = crs; //default is to set to same as topographic parameters - likely won't need to change this

// Specify study area for Spectral:
// (only change this if you need different extents for spectra and topo)
// Can be a featureCollection, feature, or geometry
args.transform=transform;

//topographic No Data Value for export topo rasters
var topoNoData = -9999;


//harmonizeOLI

///////////////////////////////////////////////////////////////////////
// END OF USER PARAMETERS
///////////////////////////////////////////////////////////////////////

//////////
/////////////////////////////////////////////////////////////////////////////////////
// 4. CALCULATE TOPOGRAPHIC DERIVATIVES, STACK LAYERS
/////////////////////////////////////////////////////////////////////////////////////
Map.centerObject(compositeArea);

compositeArea = ee.FeatureCollection(compositeArea);

var huc12 = hucs.filterBounds(compositeArea);
var huc12_merge_buffer = ee.Feature(huc12.union().first()).buffer(4000,10);//.geometry());

Map.addLayer(huc12_merge_buffer, {color:'blue'},'huc12 merge');

var elevClip = ElevationData.clip(huc12_merge_buffer.bounds());
var gaussianFilter = ee.Kernel.gaussian({
  radius:3, sigma:2, units:'pixels', normalize:true
});
var elevSM = elevClip.convolve(gaussianFilter).resample('bilinear');
// get other terrain attributes using terrainAnalysis package
// Importing module
 // calculate derivatives
var DEMattributes = TAGEE.terrainAnalysis(TAGEE, elevSM, huc12_merge_buffer.bounds());
DEMattributes= DEMattributes.clip(huc12_merge_buffer);

//add study area parameter to get image collection
args.studyArea =huc12_merge_buffer.geometry();

/////////////////////////////////////////////////////////////////////////////////////
// 5. GET SPECTRAL DATA; PROCESS
/////////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////
//Call on master wrapper function to get Sentinel2 scenes and composites

if (useSentinel){
  //params in LandsatWrapper that are NOT in this script (so in default s2 wrapper?)
  delete args.includeSLCOffL7;
	delete args.defringeL5;
	delete args.applyFmaskCloudMask;
	delete args.applyFmaskCloudShadowMask;
	delete args.applyFmaskSnowMask;
	delete args.correctIllumintion;
	delete args.correctScale;
	delete args.exportComposites;
	delete args.outputName;
	delete args.exportPathRoot;
	delete args.harmonizeOLI;//43

  print('USING SENTINEL2');
  var spectral =getImagesLib.getSentinel2Wrapper(args);
  
  //get fill-in image: reset args to ensure no masking, just raw landsat values
  args.applyQABand=false;
  args.applyCloudScore=false;
  args.applyShadowShift =false;
  args.applyTDOM=false;
  args.performCloudScoreOffset=false;
  args.applyCloudProbability = false;
  
  //turn off masking and grab imagery to potentially fill nodata holes in mosaic
  var spectralFill =getImagesLib.getSentinel2Wrapper(args).processedComposites;
    }
else{
  print ('Using Landsat');
  var spectral=getImagesLib.getLandsatWrapper(args)//.processedComposites;
  
  args.includeSLCOffL7=true
  args.defringeL5=false
  args.applyCloudScore=false
  args.applyFmaskCloudMask=false
  args.applyTDOM=false
  args.applyFmaskCloudShadowMask=false
  args.applyFmaskSnowMask=false
  args.performCloudScoreOffset=false
  //turn off masking and grab imagery to potentially fill nodata holes in mosaic
  var spectralFill =getImagesLib.getLandsatWrapper(args).processedComposites;
  
}


args.transform = transform;

//Separate into scenes and composites for subsequent analysis
var processedScenes = spectral.processedScenes;
var yearlyComposites = spectral.processedComposites.map(function(im) 
  {return im.clip(huc12_merge_buffer)}//args.studyArea)}
);//yearly composites for all years in the Time series 


if (createNDVI_timeseries){
  exportCompositeCollection(yearlyComposites,'NDVI');
}

if (useSentinel){
  var bandList = ['blue','green','red','re1','re2','re3','nir2','nir','swir1','swir2']
  //, 'NDVI','NBR','NDMI','NDSI','brightness','greenness',]
}
else{
  var bandList = ['blue','green','red','nir','swir1','swir2'];
}


// composite yearly scenes
var processedComposites = getImagesLib.medoidMosaicMSD(processedScenes, bandList).select(bandList)

spectralFill=spectralFill.map(function(im) {return im.select(bandList)});
spectralFill = getImagesLib.medoidMosaicMSD(spectralFill).clip(args.studyArea).select(bandList);

//clip output composite to study area (buffered watersheds)
processedComposites = processedComposites.clip(args.studyArea);

if (useDisturbanceFreeLandsat){
  processedComposites = changeFree.select(changeFree.bandNames(),['blue','green','red','nir','swir1','swir2']); //rename bands
}


//add additional indices
function addNCSSIndices(composite_renamed){
  //spectral indices (DW)
  var gyp = composite_renamed.select('swir1', 'swir2').normalizedDifference().rename('gypIdx');
  composite_renamed = composite_renamed.addBands(gyp);
  // //Ferrous Index
  // var ferr = composite_renamed.select('red', 'swir2').normalizedDifference().rename('ferrIdx');
  // var composite_renamed = composite_renamed.addBands(ferr);
  var feox = composite_renamed.select('red', 'blue').normalizedDifference().rename('feoxIdx');
  composite_renamed = composite_renamed.addBands(feox);
  //clay
  var clay = composite_renamed.select('swir2', 'swir1').normalizedDifference().rename('clayIdx');
  composite_renamed = composite_renamed.addBands(clay);
  //Rock Index
  var rock = composite_renamed.select('swir1', 'green').normalizedDifference().rename('rockIdx');
  composite_renamed = composite_renamed.addBands(rock);
  //Natric Index
  // var nat = composite_renamed.select('swir1', 'nir').normalizedDifference().rename('natricIdx');
  // var composite_renamed = composite_renamed.addBands(nat);
  //Water Index
  var wi = composite_renamed.select('green', 'nir').normalizedDifference().rename('waterIdx');
  composite_renamed = composite_renamed.addBands(wi);
  // compute MSAVI
  // compute MSAVI2 using expression
  var msavi2 = composite_renamed.expression(
    '(2 * NIR + 1 - sqrt(pow((2 * NIR + 1), 2) - 8 * (NIR - RED)) ) / 2', 
    {
      'NIR': composite_renamed.select('nir'), 
      'RED': composite_renamed.select("red")
    }
  ).rename('msavi2');
  composite_renamed = composite_renamed.addBands(msavi2);
  return composite_renamed;
}

//fill any holes in imagery
processedComposites= fillNoDataHoles(processedComposites, spectralFill);

//call functions to add additional spectral indices
processedComposites = getImagesLib.simpleAddIndices(processedComposites);
processedComposites = getImagesLib.addSoilIndices(processedComposites);
processedComposites = getImagesLib.addSAVIandEVI(processedComposites);
processedComposites = addNCSSIndices(processedComposites);
processedComposites = getImagesLib.getTasseledCap(processedComposites);


function fillNoDataHoles(image, fillImage){
  var focal = image.focalMedian({iterations: 10});
  image=image.unmask(-9999).clip(args.studyArea); //reset mask "value" to -9999
  var mask =image.eq(-9999); //get bool mask where value of 1 is where we'll replace values
  var outImage=image.where(mask, focal);//image.unmask(focal)//image = 

  outImage = outImage.unmask(-9999).clip(args.studyArea);
  mask = outImage.eq(-9999);

  outImage = outImage.where(mask, fillImage);
  // var boxcar = ee.Kernel.square({
  //   radius: 3, units: 'pixels', normalize: true
  // });
  // // Smooth the image by convolving with the boxcar kernel.
  //var outImage = outImage.convolve(boxcar);
  // var gaussianFilter = ee.Kernel.gaussian({
  //   radius:3, sigma:2, units:'pixels', normalize:true
  // });
  // outImage = outImage.convolve(gaussianFilter);
  return outImage;//.resample('bilinear');
}

////////////////////////////////////////////////////////////////////////////////
// Load the study region, with an outline.
// Create an empty image into which to paint the features, cast to byte.
// Paint all the polygon edges with the same number and width, display.
var empty = ee.Image().byte();
var outline = empty.paint({
  featureCollection: args.studyArea,
  color: 2,
  width: 3
});
Map.addLayer(outline, {palette: '0000FF'}, "Study Area", false);

//define function to export layers
//define function to export layers
function exportFunc(image, description, noData){
  image = getImagesLib.setNoData(image, noData);
  Export.image.toCloudStorage({
      image: image,
      description:description,
      bucket: outputFolder,
      fileNamePrefix: nameTag+ '/'+ description,
      'scale': args.scale,
      crs: args.crs,
      crsTransform:args.transform,
      region: args.studyArea,
      maxPixels: 1e13});
}

// Function to export composite collection
//See below for necessary arguments
//All parameters must be provided
function exportCompositeCollection(collection, exportBands){
  
  //var args = prepArgumentsObject(arguments,defaultArgs);
  args.pyramidingPolicy = 'mean';

  collection = collection.map(function(im){
    return im.select(exportBands)});
  args.collection = collection;
  //Take care of date wrapping
  args.dateWrapping = getImagesLib.wrapDates(args.startJulian,args.endJulian);
  args.wrapOffset = args.dateWrapping[0];
  args.yearWithMajority = args.dateWrapping[1];
  
  //Clean up output name
  args.outputName = args.outputName.replace(/\s+/g,'-');
  args.outputName = args.outputName.replace(/\//g,'-');
  
  var years = ee.List.sequence(args.startYear+args.timebuffer,args.endYear-args.timebuffer).getInfo()
    .map(function(year){
      
    // Set up dates
    var startYearT = year-args.timebuffer;
    var endYearT = year+args.timebuffer+args.yearWithMajority;
    
    // Get yearly composite
    var composite = args.collection.filter(ee.Filter.calendarRange(year+args.yearWithMajority,year+args.yearWithMajority,'year'));
    composite = ee.Image(composite.first())
  
    // Reformat data for export
    var compositeBands = composite.bandNames();
    composite = composite.multiply(10000).int16();
    args.startYearComposite = startYearT;
    args.endYearComposite = endYearT;
    args.systemTimeStartYear = year+args.yearWithMajority;
    args.yearOriginal = year;
    args.yearUsed = args.systemTimeStartYear;
    args['system:time_start'] = ee.Date.fromYMD(args.systemTimeStartYear,6,1).millis();
    
    // Add metadata, cast to integer, and export composite
    composite = composite.set(args);
      
    exportFunc(composite, 'cNDVI'+ startYearT, -32768);
    });
}

//EXPORT all indices individually:

if (exportRed){
  exportFunc(processedComposites.select('red').multiply(10000).int16(), 'red',-32768)
}
if (exportGreen){
  exportFunc(processedComposites.select('green').multiply(10000).int16(), 'green',-32768)
}
if (exportBlue){
  exportFunc(processedComposites.select('blue').multiply(10000).int16(), 'blue',-32768)
}
if (exportRE1 && useSentinel){
  exportFunc(processedComposites.select('re1').multiply(10000).int16(), 're1',-32768)
}
if (exportRE2 && useSentinel){
  exportFunc(processedComposites.select('re2').multiply(10000).int16(), 're2',-32768)
}
if (exportRE3 && useSentinel){
  exportFunc(processedComposites.select('re3').multiply(10000).int16(), 're3',-32768)
}
if (exportNIR){
  exportFunc(processedComposites.select('nir').multiply(10000).int16(), 'nir1',-32768)
}
if (exportNIR2 && useSentinel){
  exportFunc(processedComposites.select('nir2').multiply(10000).int16(), 'nir2',-32768)
}
if (exportSWIR1){
  exportFunc(processedComposites.select('swir1').multiply(10000).int16(), 'swir1',-32768)
}
if (exportSWIR2){
  exportFunc(processedComposites.select('swir2').multiply(10000).int16(), 'swir2',-32768)
}
if (exportNDVI){
  exportFunc(processedComposites.select('NDVI').multiply(10000).int16(), 'ndvi',-32768)
}
if (exportNBR){
  exportFunc(processedComposites.select('NBR').multiply(10000).int16(), 'nbr',-32768)
}
if (exportBSI){
  exportFunc(processedComposites.select('BSI').multiply(10000).int16(), 'bsi',-32768)
}
if (exportNDMI){
  exportFunc(processedComposites.select('NDMI').multiply(10000).int16(), 'ndmi',-32768)
}
if (exportNDII){
  exportFunc(processedComposites.select('NDII').multiply(10000).int16(), 'ferrIdx',-32768) // rename NDII as ferrIdx
}
if (exportNDSI){
  exportFunc(processedComposites.select('NDSI').multiply(10000).int16(), 'ndsi',-32768)
}
if (exportNDCI){
  exportFunc(processedComposites.select('NDCI').multiply(10000).int16(), 'ndci',-32768)
}
if (exportNDFI){
  exportFunc(processedComposites.select('NDFI').multiply(10000).int16(), 'natricidx',-32768) //rename NDFI to Natric Index
} 
if (exportHI){
  exportFunc(processedComposites.select('HI').multiply(10000).int16(), 'hi',-32768);
}
if (exportEVI){
  exportFunc(processedComposites.select('EVI').multiply(10000).int16(),'evi',-32768);
}
if (exportSAVI){
  exportFunc(processedComposites.select('SAVI').multiply(10000).int16(),'savi',-32768);
}

if (exportBrightness){
  exportFunc(processedComposites.select('brightness').multiply(10000).int16(), 'TCbright',-32768);
}
if (exportGreenness){
  exportFunc(processedComposites.select('greenness').multiply(10000).int16(), 'TCgreen',-32768);
}
if (exportWetness){
  exportFunc(processedComposites.select('wetness').multiply(10000).int16(), 'TCwet',-32768);
}
if (exportTcap4){
  exportFunc(processedComposites.select('fourth').multiply(10000).int16(), 'TC4',-32768);
}
if (exportTcap5){
  exportFunc(processedComposites.select('fifth').multiply(10000).int16(), 'TC5',-32768);
}
if (exportTcap6){
  exportFunc(processedComposites.select('sixth').multiply(10000).int16(), 'TC6',-32768);
}
if (exportGyp){
  exportFunc(processedComposites.select('gypIdx').multiply(10000).int16(), 'gypidx',-32768);
}
if (exportFeox){
  exportFunc(processedComposites.select('feoxIdx').multiply(10000).int16(), 'feoxidx', -32768);
}
if (exportClay){
  exportFunc(processedComposites.select('clayIdx').multiply(10000).int16(), 'clayidx', -32768);
}
if (exportRock){
  exportFunc(processedComposites.select('rockIdx').multiply(10000).int16(), 'rockidx', -32768);
}
if (exportWI){
  exportFunc(processedComposites.select('waterIdx').multiply(10000).int16(), 'wateridx', -32768);
}
if (exportMSAVI2){
  exportFunc(processedComposites.select('msavi2'), 'msavi2', -32768); //32 bit float
}

var filler = ee.Image.constant(0).clip(huc12_merge_buffer);

if (exportElevation){
  exportFunc(DEMattributes.select('Elevation').float(), 'elev', topoNoData);
}
if (exportSlope){
  exportFunc(DEMattributes.select('Slope').float(), 'slope',topoNoData);
}
if (exportAspect){
  exportFunc(DEMattributes.select('Aspect').float(),'aspect',topoNoData);
}
if (exportHillshade){
  exportFunc(DEMattributes.select('Hillshade').float(), 'hillshade',topoNoData);
}
if (exportNorthness){
  exportFunc(DEMattributes.select('Northness').float(), 'northness',topoNoData);
}
if (exportEastness){
  exportFunc(DEMattributes.select('Eastness').float(),'eastness',topoNoData);
}
if (exportHorizontalCurvature){
  var im = DEMattributes.select('HorizontalCurvature');
  im= fillNoDataHoles(im, filler);
  exportFunc(im.float(), 'horzcrv',topoNoData);
}
if (exportVerticalCurvature){
  var im = DEMattributes.select('VerticalCurvature');
  im= fillNoDataHoles(im, filler);
  exportFunc(im.float(), 'vertcrv',topoNoData);
}
if (exportMeanCurvature){
  var im = DEMattributes.select('MeanCurvature');
  im= fillNoDataHoles(im, filler);
  exportFunc(im.float(), 'meancrv',topoNoData);
}
if (exportGaussianCurvature){
  var im = DEMattributes.select('GaussianCurvature');
  im= fillNoDataHoles(im, filler);
  exportFunc(im.float(), 'gauscrv',topoNoData);
}
if (exportMinimalCurvature){
  var im = DEMattributes.select('MinimalCurvature');
  im= fillNoDataHoles(im, filler);
  exportFunc(im.float(), 'mincrv',topoNoData);
}
if (exportMaximalCurvature){
  var im = DEMattributes.select('MaximalCurvature');
  im= fillNoDataHoles(im, filler);
  exportFunc(im.float(), 'maxcrv',topoNoData);
}
if (exportShapeIndex){
  var im = DEMattributes.select('ShapeIndex');
  im= fillNoDataHoles(im, filler);
  exportFunc(im.float(), 'shpidx',topoNoData);
}
 
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
Map.setOptions('HYBRID')
////////////////////////////////////////////////////////////////////////////////
//Code for starting all tasks once this script has run (to eliminate need to click all 'Run' buttons in Tasks)
//1st uncomment code lines starting with 'function runTaskList()..
// Then: Press f12, then paste functions into console
//Finally, paste function calls into console
function runTaskList(){
    // var tasklist = document.getElementsByClassName('task local type-EXPORT_IMAGE awaiting-user-config');
    // for (var i = 0; i < tasklist.length; i++)
    //         tasklist[i].getElementsByClassName('run-button')[0].click();
    $$('.run-button' ,$$('ee-task-pane')[0].shadowRoot).forEach(function(e) {
         e.click();
    })
}

 

function confirmAll() {
    // var ok = document.getElementsByClassName('goog-buttonset-default goog-buttonset-action');
    // for (var i = 0; i < ok.length; i++)
    //     ok[i].click();
    $$('ee-table-config-dialog, ee-image-config-dialog').forEach(function(e) {
         var eeDialog = $$('ee-dialog', e.shadowRoot)[0]
         var paperDialog = $$('paper-dialog', eeDialog.shadowRoot)[0]
         $$('.ok-button', paperDialog)[0].click()
    })
}

 
