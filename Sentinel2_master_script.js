//The goal of this code is to extract NDVI and NDBI at 3000m extent to be processed in R and ArcPro

//Load locations of sample sites and create AOI
var sample = ee.FeatureCollection("users/shreenapyakurel/Sample_Sites"),

//create a bounding box that encompasses the location   
    AOI = 
    /* color: #ffc82d */
    /* shown: false */
    /* displayProperties: [
      {
        "type": "rectangle"
      }
    ] */
    ee.Geometry.Polygon(
        [[[-72.30321260892146, 42.69707031707116],
          [-72.30321260892146, 42.02124129930544],
          [-71.53416964017146, 42.02124129930544],
          [-71.53416964017146, 42.69707031707116]]], null, false);
//-----------------------------------------------------------------------------------------------------------------------
//Goal: Filter image to get a cloud free composite and display true and false color composite images

// Function to mask clouds using the Sentinel-2 QA band.
function maskS2clouds(image) {
  var qa = image.select('QA60')

  // Bits 10 and 11 are clouds and cirrus, respectively.
  var cloudBitMask = 1 << 10;
  var cirrusBitMask = 1 << 11;

  // Both flags should be set to zero, indicating clear conditions.
  var mask = qa.bitwiseAnd(cloudBitMask).eq(0).and(
             qa.bitwiseAnd(cirrusBitMask).eq(0))

  // Return the masked and scaled data, without the QA bands.
  return image.updateMask(mask).divide(10000)
      .select("B.*")
      .copyProperties(image, ["system:time_start"])
}

// Load Sentinel-2 surface  reflectance data. Images are from may 2019 to August 2019 reflecting when sound recorders are placed
var collection = ee.ImageCollection('COPERNICUS/S2_SR')
    .filterDate('2019-05-01', '2019-08-31')
    // Pre-filter to get less cloudy granules.
    .filter(ee.Filter.lt('CLOUDY_PIXEL_PERCENTAGE', 10))
    .map(maskS2clouds)
    .filterBounds(AOI)

//Filter images that meet both clear onditions and fall within the range and take the median value 
var composite = collection.median()

//divide by 1000 to fix exactly in AOI
var image = composite.clip(AOI).divide(1000);


//clip image to a bounding box with all points 
var imgclip = image.clip(AOI)

//True color composite and traditional False color composite, to see on map change false to true
Map.addLayer(composite, {bands: ['B4', 'B3', 'B2'], min: 0, max: 0.3}, 'RGB', false)
Map.addLayer(composite, {bands: ['B8', 'B4', 'B3'], min: 0, max: 0.3}, 'fcc', false)

//Add in sample points to map
Map.addLayer(sample, {color: 'yellow'}, 'sample');

// 3000m buffer all points 
var buffer3000 = sample.map(function(f) {
  return f.buffer(3000, 1); // Note that the errorMargin is set to 100.
});
Map.addLayer(buffer3000, {color: 'green'}, '3000buff', true);
//----------------------------------------------------------------------------------------------------------------------------------
//Goal: calculate NDVI and NDBI at different buffers used in proccessing

// NDVI of site
var ndvi = imgclip.normalizedDifference(['B8', 'B4']);
var ndviParams =  /*{min: -0.2, max: 0.8, palette: 'FFFFFF, CE7E45, DF923D, F1B555, FCD163, 99B718, 74A901, 66A000, 529400,' +
    '3E8601, 207401, 056201, 004C00, 023B01, 012E01, 011D01, 011301'};*/
  {
  min: 0.0,
  max: 1.0,
  palette: [
    'blue', 'white', 'green'
  ],
};
Map.addLayer(ndvi, ndviParams, 'NDVI image', false);


//NDBI
var ndbi = imgclip.normalizedDifference(['B11', 'B8']);

// Display the result.
var ndbiParams = {min: -1, max: 0,  palette: ['blue', 'purple', 'grey'

  ],
};
Map.addLayer(ndbi, ndbiParams, 'NDBI image', false);



//clip ndvi and ndbi, fcc, and tcc to sample sites/buffers 

var fcc_clip = composite.clip(buff3000)
Map.addLayer(fcc_clip, {bands: ['B8', 'B4', 'B3'], min: 0, max: 0.3}, 'fcc_clip', true)

var ndviimage = ndvi.clip(buff3000);
Map.addLayer(ndviimage, ndviParams, 'NDVI clip', true);

var ndbiimage = ndbi.clip(buff3000);
Map.addLayer(ndbiimage, ndbiParams, 'NDBI clip', true);

var tcc_clip = composite.clip(buff3000)
Map.addLayer(tcc_clip, {bands: ['B4', 'B3', 'B2'], min: 0, max: 0.3}, 'clip_RGB', true)


//-----------------------------------------------------------------------------------------------------------------------------------
//export images as geotiffs to be used in ArcPro/R
//export images as geotiffs to be used in ArcPro/R
// Export a cloud-optimized GeoTIFF.
Export.image.toDrive({
  image: collection,
  description: 'imageToCOGeoTiffExample',
  scale: 30,
  region: AOI,
  fileFormat: 'GeoTIFF',
  formatOptions: {
    cloudOptimized: true
  }
});

Export.image.toDrive({
  image: ndviimage,
  description: 'NDVI_3000m',
  scale: 30,
  region: AOI,
  fileFormat: 'GeoTIFF',
  formatOptions: {
    cloudOptimized: true
  }
});

Export.image.toDrive({
  image: ndbiimage,
  description: 'NDBI_3000m',
  scale: 30,
  region: AOI,
  fileFormat: 'GeoTIFF',
  formatOptions: {
    cloudOptimized: true
  }
});





















