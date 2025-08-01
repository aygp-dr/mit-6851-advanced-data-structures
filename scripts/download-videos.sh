#!/bin/bash
# Download video lectures using yt-dlp

set -e

# Load environment
if [ -f .env ]; then
    source .env
else
    echo "Error: .env file not found. Run 'make setup' first."
    exit 1
fi

# Activate virtual environment
if [ -f .venv/bin/activate ]; then
    source .venv/bin/activate
else
    echo "Error: Virtual environment not found. Run 'make setup' first."
    exit 1
fi

# Create video directory
mkdir -p "${VIDEO_DIR}"

echo "Starting video download..."

# Video URLs mapping (from OCW)
declare -A VIDEO_URLS=(
    ["L01"]="https://www.youtube.com/watch?v=T0yzrZL1py0"
    ["L02"]="https://www.youtube.com/watch?v=WqCWghETNDc"
    ["L03"]="https://www.youtube.com/watch?v=-KBPG-oZfco"
    ["L04"]="https://www.youtube.com/watch?v=P8eQTwE5rWg"
    ["L05"]="https://www.youtube.com/watch?v=E5RP8uu7E8g"
    ["L06"]="https://www.youtube.com/watch?v=hT42KThQFfg"
    ["L07"]="https://www.youtube.com/watch?v=i1Zg-JNpDZo"
    ["L08"]="https://www.youtube.com/watch?v=LXpmSAXQAiE"
    ["L09"]="https://www.youtube.com/watch?v=CpGaVFqjBcI"
    ["L10"]="https://www.youtube.com/watch?v=BXcqQ01nKEE"
    ["L11"]="https://www.youtube.com/watch?v=vwtpPcLWmHQ"
    ["L12"]="https://www.youtube.com/watch?v=Z5DQ_FReaXo"
    ["L13"]="https://www.youtube.com/watch?v=fKCdKOxJMvc"
    ["L14"]="https://www.youtube.com/watch?v=G6nJNNhP3rA"
    ["L15"]="https://www.youtube.com/watch?v=JN6rBq1D_xk"
    ["L16"]="https://www.youtube.com/watch?v=F3XGNJa19Eg"
    ["L17"]="https://www.youtube.com/watch?v=WLdARvbXbNI"
    ["L18"]="https://www.youtube.com/watch?v=RP9j3bFhQT8"
    ["L19"]="https://www.youtube.com/watch?v=FYVqt98pxgE"
    ["L20"]="https://www.youtube.com/watch?v=iWcDa9UYSF4"
    ["L21"]="https://www.youtube.com/watch?v=0LAtu9vcInM"
    ["L22"]="https://www.youtube.com/watch?v=UxICsjrdlJA"
)

# Download each video
for lecture in "${!VIDEO_URLS[@]}"; do
    echo "Downloading ${lecture}..."
    yt-dlp -f "best[height<=${VIDEO_QUALITY%p}]" \
           --output "${VIDEO_DIR}/${lecture}-%(title)s.%(ext)s" \
           "${VIDEO_URLS[$lecture]}" || echo "Failed to download ${lecture}"
done

echo "✅ Video download complete!"