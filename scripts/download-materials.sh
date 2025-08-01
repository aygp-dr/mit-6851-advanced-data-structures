#!/bin/bash
# Download course materials from CSAIL

set -e

# Load environment
if [ -f .env ]; then
    source .env
else
    echo "Error: .env file not found. Run 'make setup' first."
    exit 1
fi

# Create directories
mkdir -p "${PDF_DIR}"
mkdir -p "${MIRROR_DIR}"

echo "Starting material download..."

# Download scribe notes and instructor notes
BASE_URL="${COURSE_URL}"

# List of lectures (1-22)
for i in {1..22}; do
    LECTURE=$(printf "L%02d" $i)
    echo "Downloading materials for ${LECTURE}..."
    
    # Scribe notes
    wget -q -nc -P "${PDF_DIR}" "${BASE_URL}${LECTURE}-scribe.pdf" || echo "  No scribe notes for ${LECTURE}"
    
    # Instructor notes
    wget -q -nc -P "${PDF_DIR}" "${BASE_URL}${LECTURE}.pdf" || echo "  No instructor notes for ${LECTURE}"
done

# Download all scribe notes compilation
echo "Downloading complete scribe notes..."
wget -q -nc -P "${PDF_DIR}" "${BASE_URL}scribe-notes.pdf" || echo "Failed to download complete notes"

# Mirror the CSAIL site
echo "Mirroring CSAIL lecture site..."
wget --mirror --convert-links --adjust-extension \
     --page-requisites --no-parent \
     --directory-prefix="${MIRROR_DIR}" \
     --wait="${MIRROR_WAIT}" \
     --limit-rate=200k \
     "${COURSE_URL}" || echo "Mirror completed with some errors"

echo "✅ Material download complete!"