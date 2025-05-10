import cv2
import numpy as np
from rembg import remove
from PIL import Image

# Load original image
input_path = "avatar.jpg"
original_pil = Image.open(input_path).convert("RGB")
original_np = np.array(original_pil)

# Remove background
removed = remove(original_np)
foreground_rgb = removed[:, :, :3]
alpha = removed[:, :, 3] / 255.0

# Convert foreground to greyscale
foreground_gray = cv2.cvtColor(foreground_rgb, cv2.COLOR_RGB2GRAY)
foreground_gray_rgb = np.stack([foreground_gray]*3, axis=-1)

# Slight background blur
blurred_bg = cv2.GaussianBlur(original_np, (25, 25), 0)

# Blend greyscale foreground with blurred background
composite_gray = np.zeros_like(original_np, dtype=np.uint8)
for c in range(3):
    composite_gray[:, :, c] = (alpha * foreground_gray_rgb[:, :, c] +
                               (1 - alpha) * blurred_bg[:, :, c]).astype(np.uint8)


def add_vignette(image, strength=0.5):
    rows, cols = image.shape[:2]

    # Create Gaussian mask
    X_resultant_kernel = cv2.getGaussianKernel(cols, cols * strength)
    Y_resultant_kernel = cv2.getGaussianKernel(rows, rows * strength)
    kernel = Y_resultant_kernel * X_resultant_kernel.T
    mask = kernel / kernel.max()

    vignette = np.empty_like(image)
    for i in range(3):
        vignette[:, :, i] = image[:, :, i] * mask

    return vignette.astype(np.uint8)


# Example use:

vignetted = add_vignette(composite_gray, strength=0.6)
cv2.imwrite("portrait_vignette.jpg", cv2.cvtColor(vignetted, cv2.COLOR_RGB2BGR))
print("✅ Saved: portrait_vignettee.jpg")
