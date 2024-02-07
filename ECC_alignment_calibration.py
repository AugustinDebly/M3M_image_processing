import sys
import cv2
import numpy as np

#https://learnopencv.com/image-alignment-ecc-in-opencv-c-python/

path_in_1  = sys.argv[1]
path_in_2  = sys.argv[2]
path_out   = sys.argv[3]

img2TIF = cv2.imread(path_in_2,cv2.IMREAD_UNCHANGED)
img1 = cv2.imread(path_in_1)
img2 = cv2.imread(path_in_2)
img1gray = cv2.cvtColor(img1,cv2.COLOR_BGR2GRAY)
img2gray = cv2.cvtColor(img2,cv2.COLOR_BGR2GRAY)
sz = img1.shape
warp_mode = cv2.MOTION_TRANSLATION
warp_matrix = np.eye(2, 3, dtype=np.float32)
number_of_iterations = 5000
termination_eps = 1e-10
criteria = (cv2.TERM_CRITERIA_EPS | cv2.TERM_CRITERIA_COUNT, number_of_iterations,  termination_eps)
(cc, warp_matrix) = cv2.findTransformECC(img1gray,img2gray,warp_matrix, warp_mode, criteria)
img2_corr = cv2.warpAffine(img2TIF, warp_matrix, (sz[1],sz[0]), flags=cv2.INTER_LINEAR + cv2.WARP_INVERSE_MAP)
cv2.imwrite(path_out, img2_corr)