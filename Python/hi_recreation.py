from qgis.core import QgsApplication, QgsProviderRegistry, QgsRasterLayer, QgsRasterPipe, QgsRectangle, QgsException
from PyQt5.QtCore import QDir
import urllib.parse
import time
import numpy as np
import geopandas as gpd
from shapely.geometry import Point
from scipy.spatial import cKDTree
import math
import os
import warnings

warnings.filterwarnings("ignore", category=RuntimeWarning, message="QFontDatabase*")

def hi_rec_py(point_r, cloudfront, plugins_path):
    data = point_r
    #need to change the crs
    frame = gpd.GeoDataFrame(data, geometry=gpd.points_from_xy(data['geometry'].apply(lambda x: x[0]), data['geometry'].apply(lambda x: x[1]), crs = "EPSG:4326"))
    #and change it again, something strange with coming from R
    if frame.crs.to_epsg() != 3857:
        frame = frame.to_crs('EPSG:3857')
    else:
        frame = frame
    if (((int(frame.total_bounds[2]) - int(frame.total_bounds[0])) * (int(frame.total_bounds[3]) - int(frame.total_bounds[1]))) > 652000000000):
        print("Study size is too large, please subset your data to an area smaller than 650 billion metres squared and try again. Try using a smaller number of individuals from the dataset. ")
        print("Returning original dataset. ")
        return(point_r)
    try:
        if (os.environ.get('LOCAL_DEV', 'off') == 'on'):
            QgsApplication.setPrefixPath("./Library/bin/", True)
        #https://gis.stackexchange.com/questions/155745/layer-is-not-valid-error-in-my-standalone-pyqgis-script-app/155852#155852
        # and https://gis.stackexchange.com/a/263853
        elif (os.environ.get("R_SESSION_INITIALIZED") is not None):
            QgsProviderRegistry.instance().setLibraryDirectory(path=QDir(plugins_path))
            QgsApplication.setPluginPath(plugins_path)
            os.environ["QT_QPA_PLATFORM"] = "offscreen"
            QgsApplication.setPrefixPath("./Library/python", False)
        else:
            #kudos: https://gis.stackexchange.com/a/263853
            os.environ["QT_QPA_PLATFORM"] = "offscreen"
            QgsApplication.setPrefixPath("./Library/python", False)
    except Exception as e:
        print("Error setting application path. \n" + str(e))
    try:
        qgs = QgsApplication([], False)
        qgs.initQgis()
    except Exception as e:
        print("Error initializing QGIS for analysis. \n" + str(e))
    urlKeysEntry = cloudfront
    if urlKeysEntry == None:
        print("No information passed. Please input correct parameter and try again. ")
        exit()
    elif (urlKeysEntry.find("Key-Pair-Id") == -1):
        print("Key-Pair-Id incorrect.")
        exit()
    elif (urlKeysEntry.find("Policy") == -1):
        print("Policy incorrect.")
        exit()
    elif (urlKeysEntry.find("Signature") == -1):
        print("Signature incorrect.")
        exit()
    urlStart = "type=xyz&url=https://heatmap-external-a.strava.com/tiles-auth/all/blue/"
    urlZoomSet = "{z}/{x}/{y}.png"
    urlQ = "?"
    urlKeys = urlKeysEntry
    urlZoomParams = "&zmax=12&zmin=0"
    try:
        url = urlStart + urllib.parse.quote(urlZoomSet) + urlQ + urllib.parse.quote(urlKeys) + urlZoomParams
        time.sleep(5)
        strava_raster = QgsRasterLayer(url, "Red", "wms")
        if not strava_raster.isValid():
            print("error summary:", strava_raster.error().summary())
    except Exception as e:
        print("Error initializing the connection to Strava. \nThere is a problem with the url provided. Please ensure url is formatted correctly and try again.\n" + str(e))
        exit()
    if (strava_raster.isValid() == True):
        try:
            pipe = QgsRasterPipe()
            provider = strava_raster.dataProvider()
            pipe.set(provider.clone())
        except Exception as e:
            print("Error writing file. " + str(e))
        try:
            input_qgs_rect = QgsRectangle(frame.total_bounds[0], frame.total_bounds[1], frame.total_bounds[2], frame.total_bounds[3])
        except Exception as e:
            print("Error obtaining the shapefile. Is it in a shapefile? " + str(e))
        try:
            pixel_size = 100  #meters (approximately)
            xres = int((input_qgs_rect.xMaximum() - input_qgs_rect.xMinimum()) / pixel_size)
            yres = int((input_qgs_rect.yMaximum() - input_qgs_rect.yMinimum()) / pixel_size)
        except Exception as e:
            print("Error formatting resolution. " + str(e))
            exit()
    else:
        print("Raster layer was invalid, please ensure the url parameters you entered were correct and up to date. ")
        exit()
    try:
        raster_bands = provider.block(1, input_qgs_rect, xres, yres)
    except Exception as e:
        print("Error converting to byte array. " + str(e))
        exit()
    except QgsException as e:
        print("Timer exception. ")
        exit()
    try:
        byte_array = raster_bands.data()
    except Exception as e:
        print("Error converting to byte array. " + str(e))
        exit()
    try:
        numpy_array = np.frombuffer(byte_array.data(), dtype=np.uint8)
        reshaped_array = np.reshape(numpy_array, (yres, xres, 4))
        geo_crs = frame.crs
        pixel_x_size = (input_qgs_rect.xMaximum() - input_qgs_rect.xMinimum()) / (reshaped_array.shape[1])
        pixel_y_size = (input_qgs_rect.yMaximum() - input_qgs_rect.yMinimum()) / (reshaped_array.shape[0])
        xmin = input_qgs_rect.xMinimum()
        ymin = input_qgs_rect.yMinimum()
        data = []
        x_coords = np.arange(0, xres) * pixel_x_size + xmin
        y_coords = np.arange(yres - 1, -1, -1) * pixel_y_size + ymin
        x_mesh, y_mesh = np.meshgrid(x_coords, y_coords)
        x_flattened = x_mesh.flatten()
        y_flattened = y_mesh.flatten()
        x_midpoints = x_flattened + (pixel_x_size / 2)
        y_midpoints = y_flattened + (pixel_y_size / 2)
        geometries = [Point(x, y) for x, y in zip(x_midpoints, y_midpoints)]
        band_data = {
                'band_1': reshaped_array[:, :, 2].flatten(),
                'band_2': reshaped_array[:, :, 1].flatten(),
                'band_3': reshaped_array[:, :, 0].flatten(),
                'band_4': reshaped_array[:, :, 3].flatten()
            }
        data = {'geometry': geometries, **band_data}
        df = gpd.GeoDataFrame(data, crs=geo_crs)
        df = df[~df['geometry'].isnull()]
        if df.crs != 'EPSG:3857':
            df = df.to_crs('EPSG:3857')
        maxBandValue = 255
        geoframe_coords = [(geom.x, geom.y) for geom in frame.geometry]
        bandGdf_coords = [(geom.x, geom.y) for geom in df.geometry]
        tree = cKDTree(bandGdf_coords)
        distances, indices = tree.query(geoframe_coords)
        frame['nearest_distance'] = distances
        band1_values = df.loc[indices, 'band_1'].tolist()
        band2_values = df.loc[indices, 'band_2'].tolist()
        band3_values = df.loc[indices, 'band_3'].tolist()
        band4_values = df.loc[indices, 'band_4'].tolist()
        frame['band_1'] = band1_values
        frame['band_2'] = band2_values
        frame['band_3'] = band3_values
        frame['band_4'] = band4_values
        hypotenuse = math.sqrt((pixel_x_size*pixel_x_size) + (pixel_y_size*pixel_y_size))
        selected_columns = ["band_1", "band_2", "band_3"]
        frame['intensity'] = (frame[selected_columns].sum(axis=1) / (maxBandValue*3))
        criterion = frame['nearest_distance'] > (hypotenuse / 2)
        frame.loc[criterion, ['band_1', 'band_2', 'band_3', 'intensity']] = np.nan
    except ZeroDivisionError:
        point_r['band_1'] = np.nan
        point_r['band_2'] = np.nan
        point_r['band_3'] = np.nan
        point_r['band_4'] = np.nan
        point_r['intensity'] = np.nan
        point_r['nearest_distance'] = np.nan
        return point_r
    except Exception as e:
        print("Reshaping/assignment error, returning null. There may be something wrong with the input data? " + str(e))
        return(frame)
    #running this causes a crash with reticulate, leaving cleanup to python stops if from crashing
    #qgs.exitQgis()
    return frame
