 import os, glob, cv2, numpy as np, pandas as pd

# ===== 設定 =====
INPUT_PATH = "/Users/matsuihiroshi/Dropbox/ongoingExperiment/naikaiensis_Experiment/phototaxis/20250822-lightavoidance"   # 画像ファイル or 画像フォルダ
OUT_DIR = "./out_blobs"
os.makedirs(OUT_DIR, exist_ok=True)

# 点の物理感に合わせてチューニング
MIN_AREA, MAX_AREA = 6, 500        # 連結成分の面積レンジ（px^2相当）
MIN_INERTIA = 0.03                 # 0に近いほど細長い粒も通す（0.03〜0.1で調整）
BG_SIGMA = 101                       # 背景ぼかしのσ（大きいほど照明ムラ補正が強い）

def plate_mask(gray):
    g = cv2.GaussianBlur(gray, (0,0), 1.2)
    # 明るい領域（皿）をOtsuで取る→大きい成分だけ残す
    _, otsu = cv2.threshold(g, 0, 255, cv2.THRESH_BINARY + cv2.THRESH_OTSU)
    num, lab, stats, _ = cv2.connectedComponentsWithStats(otsu, 8)
    mask = np.zeros_like(otsu)
    for i in range(1, num):
        if stats[i, cv2.CC_STAT_AREA] >= 15000:
            mask[lab == i] = 255
    # 少し閉じて穴埋め
    mask = cv2.morphologyEx(mask, cv2.MORPH_CLOSE,
                            cv2.getStructuringElement(cv2.MORPH_RECT,(25,25)))
    return mask

def detect_points(img_path):
    img = cv2.imread(img_path, cv2.IMREAD_COLOR)
    if img is None:
        print(f"[skip] cannot read: {img_path}")
        return None
    base = os.path.splitext(os.path.basename(img_path))[0]

    # LabのLで処理（白い皿と暗い粒のコントラストが素直）
    L = cv2.cvtColor(img, cv2.COLOR_BGR2LAB)[:,:,0].astype(np.uint8)
    L = cv2.GaussianBlur(L, (0,0), 1.2)

    # 皿マスク
    mask = plate_mask(L)

    # 背景差分: 大きいσで平滑化した背景を引くと「暗い点が明るく」なる
    bg = cv2.GaussianBlur(L, (0,0), BG_SIGMA)
    norm = cv2.subtract(bg, L)

    # SimpleBlobDetector（暗点→明にしたので blobColor=255）
    params = cv2.SimpleBlobDetector_Params()
    params.minThreshold = 5
    params.maxThreshold = 200
    params.thresholdStep = 5

    params.filterByColor = True
    params.blobColor = 255

    params.filterByArea = True
    params.minArea = MIN_AREA
    params.maxArea = MAX_AREA

    params.filterByCircularity = False
    params.filterByConvexity = False

    params.filterByInertia = True
    params.minInertiaRatio = MIN_INERTIA  # 小さくすると細長い粒も通る

    detector = cv2.SimpleBlobDetector_create(params)
    kps = detector.detect(norm)

    # 皿内のみに制限
    H, W = mask.shape
    pts = []
    for kp in kps:
        x, y = int(round(kp.pt[0])), int(round(kp.pt[1]))
        if mask[min(max(y,0),H-1), min(max(x,0),W-1)] == 0:
            continue
        pts.append((kp.pt[0], kp.pt[1], kp.size))

    # 出力
    df = pd.DataFrame(pts, columns=["cx","cy","size"])
    csv_path = os.path.join(OUT_DIR, f"{base}_points.csv")
    df.to_csv(csv_path, index=False)

    # 可視化
    vis = img.copy()
    for x,y,s in pts:
        cv2.circle(vis, (int(round(x)), int(round(y))), int(max(3, s/2)), (0,255,0), 1)
        cv2.circle(vis, (int(round(x)), int(round(y))), 1, (0,0,255), -1)
    cnts,_ = cv2.findContours(mask, cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_SIMPLE)
    cv2.drawContours(vis, cnts, -1, (255,0,0), 2)
    png_path = os.path.join(OUT_DIR, f"{base}_overlay.png")
    cv2.imwrite(png_path, vis)

    print(f"[ok] {base}: {len(pts)} dots  |  CSV:{os.path.basename(csv_path)}  PNG:{os.path.basename(png_path)}")
    return {"n": len(pts), "csv": csv_path, "png": png_path}

# 1枚/フォルダどちらでも
paths = []
if os.path.isdir(INPUT_PATH):
    for ext in ("*.png","*.jpg","*.jpeg","*.tif","*.bmp"):
        paths += glob.glob(os.path.join(INPUT_PATH, ext))
else:
    paths = [INPUT_PATH]

results = [detect_points(p) for p in paths if p]
