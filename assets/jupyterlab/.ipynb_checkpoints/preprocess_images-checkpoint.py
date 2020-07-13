import os
import time
import numpy as np
import pandas as pd
from PIL import Image

# Path to Image Data in Library Project
librarydir = '/user-home/_global_/libraryProjects/Diabetic Retinopathy Data/datasets/'

def find_black_images(file_path, df):
    """
    Creates a column of images that are not black (np.mean(img) != 0)
    Labels Missing Images as Black for Omission

    INPUT
        file_path: file_path to the images to be analyzed.
        df: Pandas DataFrame that includes all labeled image names.
        column: column in DataFrame query is evaluated against.

    OUTPUT
        Column indicating if the photo is pitch black or not.
    """

    lst_imgs = [l for l in df['image']]
    
    # Check if File Exists and is Not a Blank (Black) Image
    return [0 if os.path.isfile(file_path + img) 
      and np.mean(np.array(Image.open(file_path + img))) != 0 
      else 1 for img in lst_imgs]


if __name__ == '__main__':
    start_time = time.time()
    trainLabels = pd.read_csv(librarydir + 'trainLabels.csv')

    trainLabels['image'] = [i + '.jpeg' for i in trainLabels['image']]
    trainLabels['black'] = np.nan

    trainLabels['black'] = find_black_images(librarydir + 'train-resized-256/', trainLabels)
    trainLabels = trainLabels.loc[trainLabels['black'] == 0]
    trainLabels.to_csv(os.environ['DSX_PROJECT_DIR'] + '/datasets/trainLabels_master.csv', index=False, header=True)

    print("Completed")
    print("--- %s seconds ---" % (time.time() - start_time))
