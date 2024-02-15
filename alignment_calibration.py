import sys
import cv2
import numpy as np

path_in  = sys.argv[1]
path_out = sys.argv[2]
M11      = float(sys.argv[3])
M12      = float(sys.argv[4])
M13      = float(sys.argv[5])
M21      = float(sys.argv[6])
M22      = float(sys.argv[7])
M23      = float(sys.argv[8])
M31      = float(sys.argv[9])
M32      = float(sys.argv[10])
M33      = float(sys.argv[11])

img = cv2.imread(path_in,cv2.IMREAD_UNCHANGED)

transformation_matrix = np.array([[M11, M12, M13],[M21, M22, M23],[M31, M32, M33]], dtype=np.float32)
image_corrige         = cv2.warpPerspective(img, transformation_matrix, (2592,1944))

cv2.imwrite(path_out, image_corrige)