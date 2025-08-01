#!/bin/bash
# Download course materials from CSAIL and MIT OCW

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
mkdir -p "${MIRROR_DIR}/ocw"

echo "Starting material download..."

# Download scribe notes and instructor notes from CSAIL
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

# Mirror MIT OCW resources
echo "Mirroring MIT OCW resources..."
OCW_URL="https://ocw.mit.edu/courses/6-851-advanced-data-structures-spring-2012"

# Mirror the entire resources directory
wget --recursive \
     --no-clobber \
     --page-requisites \
     --html-extension \
     --convert-links \
     --restrict-file-names=windows \
     --domains ocw.mit.edu \
     --no-parent \
     --directory-prefix="${MIRROR_DIR}/ocw" \
     --accept="pdf,txt,html,htm,srt,mp4,css,js,jpg,png,gif" \
     --wait="${MIRROR_WAIT}" \
     --random-wait \
     --limit-rate=200k \
     "${OCW_URL}/resources/" || echo "OCW mirror completed with some errors"

# Also download OCW PDFs directly to ensure we have them
echo "Ensuring all OCW PDFs are downloaded..."
for i in {1..22}; do
    LECTURE_PDF="MIT6_851S12_lec$(printf "%02d" $i).pdf"
    OCW_PDF_URL="${OCW_URL}/resources/${LECTURE_PDF}"
    
    if [ ! -f "${PDF_DIR}/${LECTURE_PDF}" ]; then
        echo "Downloading OCW lecture $i PDF..."
        wget -q -nc -P "${PDF_DIR}" "${OCW_PDF_URL}" 2>/dev/null || echo "  OCW lecture $i PDF not available"
    fi
    
    # Also get transcripts
    TRANSCRIPT_SRT="MIT6_851S12_lec$(printf "%02d" $i)_300k.srt"
    SRT_URL="${OCW_URL}/resources/${TRANSCRIPT_SRT}"
    
    if [ ! -f "${PDF_DIR}/${TRANSCRIPT_SRT}" ]; then
        echo "Downloading OCW lecture $i transcript..."
        wget -q -nc -P "${PDF_DIR}" "${SRT_URL}" 2>/dev/null || echo "  OCW lecture $i transcript not available"
    fi
done

# Download problem sets from OCW
for i in {1..6}; do
    PSET_PDF="MIT6_851S12_ps$i.pdf"
    PSET_URL="${OCW_URL}/resources/${PSET_PDF}"
    
    if [ ! -f "${PDF_DIR}/${PSET_PDF}" ]; then
        echo "Downloading OCW problem set $i..."
        wget -q -nc -P "${PDF_DIR}" "${PSET_URL}" 2>/dev/null || echo "  OCW problem set $i not available"
    fi
done

echo "✅ Material download complete!"