//Sentinel 2 processing on Google Earth Engine

//draw the area of interest
var geometry = ee.FeatureCollection(ee.Geometry.Rectangle(28.5, -12.5, 41, -0.5));

var s2 = ee.ImageCollection("COPERNICUS/S2");

// Choose Image Collection: Sentinel-2 (s2) or Landsat 8 Raw (l8raw):
var collection = s2;//l8raw

// Choose start and end year
var startyear = 2016;
var endyear = 2016;

// variables for start and end month
var startmonth = 1;
var endmonth = 1;

// create list for years and months
var years = ee.List.sequence(startyear,endyear);
var months = ee.List.sequence(1,12);


// define start and end date
var startdate = ee.Date.fromYMD(startyear,startmonth,1);
var enddate = ee.Date.fromYMD(endyear+1,endmonth,1);


//var startJulian =1;
//var endJulian = 365;
//var metadataCloudCoverMax = 60;


//var dakNong = ee.FeatureCollection('ft:1iovW6_vHSX3JOx5OvcFr-ljJdx5mGmqK6Q_ETiSO','geometry');
//var countries = ee.FeatureCollection('ft:1tdSwUL7MVpOauSgRzqVTOwdfy17KDbw-1d9omPw');
//var dakNong = countries.filter(ee.Filter.eq('Country', 'Tanzania, United Republic of'));

var myarea = geometry;


var empty = ee.Image().byte();
var outline = empty.paint({featureCollection: myarea, color: 1, width: 3});
Map.addLayer(outline, {palette: 'FF0000'}, 'myarea');

var s2 = collection
                  .filterDate(startdate,enddate)
                  // .filter(ee.Filter.calendarRange(startJulian,endJulian))
                  .sort('system:time_start', false)
                  .filterBounds(myarea)
                  .select(['B2', 'B3', 'B4', 'B8', 'B5', 'B6', 'B7', 'B8A', 'B11', 'B12']);

//print('collection',s2);

var rgb_viz = {min:0, max:2000, bands:['B4','B3','B2']};
var ndvi_viz = {bands:'NDVI', min:0, max:0.3, palette:'000000,00FF00'};


//s2 = s2.select(['B2','B3','B4','B8'], ['B','G','R','N']);


function addNdvi(img) {
  var nd = img.normalizedDifference(['B8', 'B4']);
  return img.float().addBands(nd.rename('NDVI'));
}

var ndvi = s2.map(addNdvi);
var greenest = ndvi.qualityMosaic('NDVI');

//print('NDVI',ndvi);

Map.addLayer(s2.median().clip(myarea), rgb_viz, 'RGB (median)');
Map.addLayer(ndvi, ndvi_viz, 'NDVI', false);
Map.addLayer(greenest.clip(myarea), rgb_viz, 'RGB (greenest pixel)');

print('NDVIImages',greenest);

var empty = ee.Image().byte();
var outline = empty.paint({featureCollection: myarea, color: 1, width: 3});
Map.centerObject(myarea,9);
Map.addLayer(outline, {palette: 'FF0000'}, 'myarea');

Export.image.toDrive(
      {maxPixels: 1e13,
        image: greenest.select(['NDVI']),
        description: 'Mosaic_GEE_NDVI',
        crs:  'EPSG:4326',
        scale: 100,
        region: myarea
        });
