# MIT 6.851 Advanced Data Structures

Implementation of data structures from MIT's 6.851 Advanced Data Structures course (Spring 2012) in Guile Scheme.

## Course Information

- **Course**: 6.851 Advanced Data Structures
- **Instructor**: Prof. Erik Demaine
- **Term**: Spring 2012
- **Implementation Language**: Guile Scheme

## Setup

### Prerequisites

- Emacs
- Guile 2.2+ (or Guile 3.0)
- SQLite3
- Python 3 (for downloading tools)
- wget (for mirroring)
- yt-dlp (for video downloads)

### Installation

1. Clone the repository:
   ```bash
   git clone <repository-url>
   cd mit-6851-advanced-data-structures
   ```

2. Check dependencies:
   ```bash
   make check-deps
   ```

3. Setup environment:
   ```bash
   make setup
   ```

4. Download course materials:
   ```bash
   make download-materials  # PDFs and website mirror
   make download-videos     # Video lectures (optional)
   ```

5. Setup scribe templates:
   ```bash
   make scribe-template
   ```

## Usage

### Running Tests

```bash
make test
```

### Creating Scribe Notes

1. Copy the template:
   ```bash
   cp templates/lec-template.org scribes/lec01.org
   ```

2. Edit in Emacs with org-mode

3. Export to LaTeX:
   ```bash
   make export-scribe ORG_FILE=scribes/lec01.org
   ```

## Project Structure

- `lib/` - Data structure implementations
- `tests/` - Test suite
- `scribes/` - Lecture notes in org-mode
- `scripts/` - Utility scripts
- `materials/` - Downloaded course materials (gitignored)

## Implemented Data Structures

- [x] Persistent Stack
- [ ] Persistent Queue
- [ ] Retroactive Queue
- [ ] Point Location
- [ ] Orthogonal Range Trees
- [ ] Link-Cut Trees
- [ ] Splay Trees
- [ ] B-Trees
- [ ] Cache-Oblivious B-Trees
- [ ] Fusion Trees
- [ ] Van Emde Boas Trees
- [ ] Suffix Arrays
- [ ] Succinct Rank/Select

## License

Educational implementation based on MIT OpenCourseWare materials.