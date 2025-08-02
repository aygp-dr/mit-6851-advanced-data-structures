#!/bin/bash

# Download problem sets from MIT OCW 6.851

MATERIALS_DIR="../materials/problem-sets"
cd "$(dirname "$0")" || exit 1

echo "Downloading MIT 6.851 Problem Sets..."

# Create directory if it doesn't exist
mkdir -p "$MATERIALS_DIR"

# Problem set URLs from OCW
PROBLEM_SETS=(
    "https://ocw.mit.edu/courses/6-851-advanced-data-structures-spring-2012/7737b417ec851a04c1ca02850cf44706_MIT6_851S12_ps1.pdf"
    "https://ocw.mit.edu/courses/6-851-advanced-data-structures-spring-2012/00cf7ab6f79bbcd5ba646b32e3e7d0f0_MIT6_851S12_ps2.pdf"
    "https://ocw.mit.edu/courses/6-851-advanced-data-structures-spring-2012/d1bb1b2defe969b67b86e4c963275a42_MIT6_851S12_ps3.pdf"
    "https://ocw.mit.edu/courses/6-851-advanced-data-structures-spring-2012/3c96797e93f87e28d1b2edcdcf79d0f0_MIT6_851S12_ps4.pdf"
    "https://ocw.mit.edu/courses/6-851-advanced-data-structures-spring-2012/bd02f5c9e8e4b82de0b1e17b3c83fcd2_MIT6_851S12_ps5.pdf"
    "https://ocw.mit.edu/courses/6-851-advanced-data-structures-spring-2012/6cf7ae8b44faea2ebeeeef5b1fce8c6f_MIT6_851S12_ps6.pdf"
    "https://ocw.mit.edu/courses/6-851-advanced-data-structures-spring-2012/e987ed0bc3b42c6c2ad5e84de21a9a32_MIT6_851S12_ps7.pdf"
)

# LaTeX source files
LATEX_SOURCES=(
    "https://ocw.mit.edu/courses/6-851-advanced-data-structures-spring-2012/6072a34b9c3c3bff35a037bb48f72f3f_MIT6_851S12_ps1.tex"
    "https://ocw.mit.edu/courses/6-851-advanced-data-structures-spring-2012/2dd2f686dcb3ad5f87b3e582e826f41f_MIT6_851S12_ps2.tex"
    "https://ocw.mit.edu/courses/6-851-advanced-data-structures-spring-2012/96e7dd2afb70798cf1c3bcc8d690e1f5_MIT6_851S12_ps3.tex"
    "https://ocw.mit.edu/courses/6-851-advanced-data-structures-spring-2012/7e3e670e83bce2c8b8b88f19b96d9a32_MIT6_851S12_ps4.tex"
    "https://ocw.mit.edu/courses/6-851-advanced-data-structures-spring-2012/33c6795b98dd5c9b4a7e8a72bb1b8b3f_MIT6_851S12_ps5.tex"
    "https://ocw.mit.edu/courses/6-851-advanced-data-structures-spring-2012/53eac3bc87094f18a4ba47bb2f22ca50_MIT6_851S12_ps6.tex"
    "https://ocw.mit.edu/courses/6-851-advanced-data-structures-spring-2012/fcd58e087b14d97efaaef8fe0ed90527_MIT6_851S12_ps7.tex"
)

# Download PDFs
echo "Downloading PDF files..."
for i in "${!PROBLEM_SETS[@]}"; do
    url="${PROBLEM_SETS[$i]}"
    filename="ps$((i+1)).pdf"
    echo "Downloading Problem Set $((i+1)) PDF..."
    wget -q -O "$MATERIALS_DIR/$filename" "$url" || echo "Failed to download $filename"
done

# Download LaTeX sources
echo ""
echo "Downloading LaTeX source files..."
for i in "${!LATEX_SOURCES[@]}"; do
    url="${LATEX_SOURCES[$i]}"
    filename="ps$((i+1)).tex"
    echo "Downloading Problem Set $((i+1)) LaTeX source..."
    wget -q -O "$MATERIALS_DIR/$filename" "$url" || echo "Failed to download $filename"
done

echo ""
echo "✅ Problem sets downloaded to $MATERIALS_DIR"
ls -la "$MATERIALS_DIR"