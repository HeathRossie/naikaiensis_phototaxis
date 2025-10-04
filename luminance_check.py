import numpy as np
import cv2, matplotlib.pyplot as plt, os

IMG_PATH = "/Users/matsuihiroshi/Dropbox/ongoingExperiment/naikaiensis_Experiment/phototaxis/20250817-lightavoidance/pic_31200s.png"

# 読み込み & グレースケール化
img = cv2.imread(IMG_PATH)
gray = cv2.cvtColor(img, cv2.COLOR_BGR2GRAY)


blur = cv2.medianBlur(gray, 11)  # 3x3メディアン

_, binary = cv2.threshold(blur, 150, 255, cv2.THRESH_BINARY)



# 可視化
os.chdir("/Users/matsuihiroshi/Dropbox/ongoingExperiment/naikaiensis_Experiment/phototaxis")
plt.figure(figsize=(12,6))
plt.subplot(1,3,1); plt.title("Original"); plt.imshow(cv2.cvtColor(img, cv2.COLOR_BGR2RGB)); plt.axis("off")
plt.subplot(1,3,2); plt.title("Gray"); plt.imshow(gray, cmap="gray"); plt.axis("off")
plt.subplot(1,3,3); plt.title("Binary (bright=white, dark=black)"); plt.imshow(binary, cmap="gray"); plt.axis("off")
plt.savefig("th.png")


np.savetxt("binary.csv", binary, fmt="%d", delimiter=",")
