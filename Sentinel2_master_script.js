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
var buffer3000 = sample.map(function(f) {
  return f.buffer(3000, 1); // Note that the errorMargin is set to 100.
});
Map.addLayer(buffer3000, {color: 'green'}, '3000buff', true);

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

// 2500m buffer all points 
var buffer2500 = sample.map(function(f) {
  return f.buffer(2500, 1); // Note that the errorMargin is set to 100.
});
Map.addLayer(buffer2500, {color: 'green'}, '2500buff', true);

// 2000m buffer all points 
var buffer2000 = sample.map(function(f) {
  return f.buffer(2000, 1); // Note that the errorMargin is set to 100.
});
Map.addLayer(buffer2000, {color: 'green'}, '2000buff', true);

// 1500m buffer all points 
var buffer1500 = sample.map(function(f) {
  return f.buffer(1500, 1); // Note that the errorMargin is set to 100.
});
Map.addLayer(buffer1500, {color: 'green'}, '1500buff', true);

// 1000m buffer all points 
var buffer1000 = sample.map(function(f) {
  return f.buffer(1000, 1); // Note that the errorMargin is set to 100.
});
Map.addLayer(buffer1000, {color: 'green'}, '1000buff', true);

// 500m buffer all points 
var buffer500 = sample.map(function(f) {
  return f.buffer(500, 1); // Note that the errorMargin is set to 100.
});
Map.addLayer(buffer500, {color: 'green'}, '500buff', true);



//MEAN VALUE CALCULATIONS FOR INDICES

//NDVI




// Reduce the region for the Laughing Brook Site. The region parameter is the Feature geometry of the site.
var ndviMean_site = ndvi.reduceRegion({
  reducer: ee.Reducer.mean(),
  geometry: laughingbrook,
  scale: 30,
  maxPixels: 1e9
});

// The result is a Dictionary.  Print it.
print('Laughing Brook Site NDVI Mean: ',ndviMean_site);

// Reduce the region for the Buffer 3000 site. The region parameter is the Feature geometry of the site.
var ndviMean_buffer3000 = ndvi.reduceRegion({
  reducer: ee.Reducer.mean(),
  geometry: buffer3000,
  scale: 30,
  maxPixels: 1e9
});

// The result is a Dictionary.  Print it.
print('Buffer 3000 NDVI Mean: ', ndviMean_buffer3000);




// Reduce the region for the Buffer 2500 site. The region parameter is the Feature geometry of the site.
var ndviMean_buffer2500 = ndvi.reduceRegion({
  reducer: ee.Reducer.mean(),
  geometry: buffer2500,
  scale: 30,
  maxPixels: 1e9
});

// The result is a Dictionary.  Print it.
print('Buffer 2500 NDVI Mean: ', ndviMean_buffer2500);


// Reduce the region for the Buffer 2000 site. The region parameter is the Feature geometry of the site.
var ndviMean_buffer2000 = ndvi.reduceRegion({
  reducer: ee.Reducer.mean(),
  geometry: buffer2000,
  scale: 30,
  maxPixels: 1e9
});

// The result is a Dictionary.  Print it.
print('Buffer 2000 NDVI Mean: ', ndviMean_buffer2000);


// Reduce the region for the Buffer 1500 site. The region parameter is the Feature geometry of the site.
var ndviMean_buffer1500 = ndvi.reduceRegion({
  reducer: ee.Reducer.mean(),
  geometry: buffer1500,
  scale: 30,
  maxPixels: 1e9
});

// The result is a Dictionary.  Print it.
print('Buffer 1500 NDVI Mean: ', ndviMean_buffer1500);

// Reduce the region for the Buffer 1000 site. The region parameter is the Feature geometry of the site.
var ndviMean_buffer1000 = ndvi.reduceRegion({
  reducer: ee.Reducer.mean(),
  geometry: buffer1000,
  scale: 30,
  maxPixels: 1e9
});

// The result is a Dictionary.  Print it.
print('Buffer 1000 NDVI Mean: ', ndviMean_buffer1000);



// Reduce the region for the Buffer 500 site. The region parameter is the Feature geometry of the site.
var ndviMean_buffer500 = ndvi.reduceRegion({
  reducer: ee.Reducer.mean(),
  geometry: buffer500,
  scale: 30,    
  maxPixels: 1e9
});

// The result is a Dictionary.  Print it.
print('Buffer 500 NDVI Mean: ', ndviMean_buffer500);




//NDBI



// Reduce the region for the Buffer 3000 site. The region parameter is the Feature geometry of the site.
var ndbiMean_buffer3000 = ndbi.reduceRegion({
  reducer: ee.Reducer.mean(),
  geometry: buffer3000,
  scale: 30,
  maxPixels: 1e9
});

// The result is a Dictionary.  Print it.
print('Buffer 3000 NDBI Mean: ', ndbiMean_buffer3000);




// Reduce the region for the Buffer 2500 site. The region parameter is the Feature geometry of the site.
var ndbiMean_buffer2500 = ndbi.reduceRegion({
  reducer: ee.Reducer.mean(),
  geometry: buffer2500,
  scale: 30,
  maxPixels: 1e9
});

// The result is a Dictionary.  Print it.
print('Buffer 2500 NDBI Mean: ', ndbiMean_buffer2500);


// Reduce the region for the Buffer 2000 site. The region parameter is the Feature geometry of the site.
var ndbiMean_buffer2000 = ndbi.reduceRegion({
  reducer: ee.Reducer.mean(),
  geometry: buffer2000,
  scale: 30,
  maxPixels: 1e9
});

// The result is a Dictionary.  Print it.
print('Buffer 2000 NDBI Mean: ', ndbiMean_buffer2000);


// Reduce the region for the Buffer 1500 site. The region parameter is the Feature geometry of the site.
var ndbiMean_buffer1500 = ndbi.reduceRegion({
  reducer: ee.Reducer.mean(),
  geometry: buffer1500,
  scale: 30,
  maxPixels: 1e9
});

// The result is a Dictionary.  Print it.
print('Buffer 1500 NDBI Mean: ', ndbiMean_buffer1500);

// Reduce the region for the Buffer 1000 site. The region parameter is the Feature geometry of the site.
var ndbiMean_buffer1000 = ndbi.reduceRegion({
  reducer: ee.Reducer.mean(),
  geometry: buffer1000,
  scale: 30,
  maxPixels: 1e9
});

// The result is a Dictionary.  Print it.
print('Buffer 1000 NDBI Mean: ', ndbiMean_buffer1000);




// Reduce the region for the Buffer 500 site. The region parameter is the Feature geometry of the site.
var ndbiMean_buffer500 = ndbi.reduceRegion({
  reducer: ee.Reducer.mean(),
  geometry: buffer500,
  scale: 30,
  maxPixels: 1e9
});

// The result is a Dictionary.  Print it.
print('Buffer 500 NDBI Mean: ', ndbiMean_buffer500);





