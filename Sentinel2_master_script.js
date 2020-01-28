var sample = ee.FeatureCollection("users/shreenapyakurel/Sample_Sites"),
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

// Load Sentinel-2 surface  reflectance data.
var collection = ee.ImageCollection('COPERNICUS/S2_SR')
    .filterDate('2019-05-01', '2019-08-31')
    // Pre-filter to get less cloudy granules.
    .filter(ee.Filter.lt('CLOUDY_PIXEL_PERCENTAGE', 10))
    .map(maskS2clouds)
    .filterBounds(AOI)

var composite = collection.median()

//divide by 1000 to fix exactly in AOI
var image = composite.clip(AOI).divide(1000);

var properties = image.propertyNames();
print('Metadata properties: ', properties); // ee.List of metadata properties
print (image);

//clip image to a bounding box with all points 
var imgclip = image.clip(AOI)
Map.addLayer(composite, {bands: ['B4', 'B3', 'B2'], min: 0, max: 0.3}, 'RGB', false)
Map.addLayer(composite, {bands: ['B8', 'B4', 'B3'], min: 0, max: 0.3}, 'fcc', false)

//Add in sample points--need to figure how to distuinguish each--

Map.addLayer(sample, {color: 'yellow'}, 'sample');

// 3000m buffer all points 
var buff3000 = sample.map(function(f) {
  return f.buffer(3000, 1); // Note that the errorMargin is set to 100.
});
Map.addLayer(buff3000, {color: 'green'}, '3000buff', true);

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

print(ndbi, "ndbi")

//clip ndvi and ndbi to sample sites (buffer of 3000m)

var fcc_clip = composite.clip(buff3000)
Map.addLayer(fcc_clip, {bands: ['B8', 'B4', 'B3'], min: 0, max: 0.3}, 'fcc_clip', true)

var ndviimage = ndvi.clip(buff3000);
Map.addLayer(ndviimage, ndviParams, 'NDVI clip', true);

var ndbiimage = ndbi.clip(buff3000);
Map.addLayer(ndbiimage, ndbiParams, 'NDBI clip', true);

var tcc_clip = composite.clip(buff3000)
Map.addLayer(tcc_clip, {bands: ['B4', 'B3', 'B2'], min: 0, max: 0.3}, 'clip_RGB', true)
