import sys
import cv2
import numpy as np

path_in  = sys.argv[1]
path_out = sys.argv[2]
fx = float(sys.argv[3])
fy = float(sys.argv[4])
cx = float(sys.argv[5])
cy = float(sys.argv[6])
k1 = float(sys.argv[7])
k2 = float(sys.argv[8])
p1 = float(sys.argv[9])
p2 = float(sys.argv[10])
k3 = float(sys.argv[11])

img = cv2.imread(path_in,cv2.IMREAD_UNCHANGED)
camera_matrix = np.array([[fx, 0, cx],[0, fy, cy],[0, 0, 1]], dtype=np.float32)
dist_coeff = np.array([k1, k2, p1, p2, k3], dtype=np.float32)
newcameramatrix, _ = cv2.getOptimalNewCameraMatrix(camera_matrix, dist_coeff, (2592, 1944), 1, (2592, 1944))
image_corrige = cv2.undistort(img, camera_matrix, dist_coeff, None, newcameramatrix)
cv2.imwrite(path_out, image_corrige)