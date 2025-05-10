import cv2
import numpy as np
from rembg import remove
from PIL import Image

# Load image using PIL (to preserve RGB format)
input_path = "avatar.jpg"
original_pil = Image.open(input_path).convert("RGB")
original_np = np.array(original_pil)

# Remove background (result has RGBA channels)
removed = remove(original_np)

# Separate foreground and alpha channel
foreground_rgb = removed[:, :, :3]
alpha = removed[:, :, 3] / 255.0  # Normalize alpha to 0-1

# Create a blurred version of the original background
# Blur kernel: lower values = less blur
blurred_bg = cv2.GaussianBlur(original_np, (25, 25), 0)

# Composite: blend foreground with blurred background using alpha
composite = np.zeros_like(original_np, dtype=np.uint8)
for c in range(3):
    composite[:, :, c] = (alpha * foreground_rgb[:, :, c] +
                          (1 - alpha) * blurred_bg[:, :, c]).astype(np.uint8)

# Save result
cv2.imwrite("portrait_blurred_fixed.jpg", cv2.cvtColor(composite, cv2.COLOR_RGB2BGR))
print("✅ Saved: portrait_blurred_fixed.jpg")
