-- MIT 6.851 Course Database Schema

-- Lectures table
CREATE TABLE IF NOT EXISTS lectures (
    id INTEGER PRIMARY KEY,
    number INTEGER NOT NULL,
    title TEXT NOT NULL,
    date TEXT,
    description TEXT,
    video_url TEXT,
    scribe_pdf TEXT,
    instructor_pdf TEXT
);

-- Topics table
CREATE TABLE IF NOT EXISTS topics (
    id INTEGER PRIMARY KEY,
    name TEXT NOT NULL UNIQUE,
    description TEXT
);

-- Lecture-Topic mapping
CREATE TABLE IF NOT EXISTS lecture_topics (
    lecture_id INTEGER,
    topic_id INTEGER,
    FOREIGN KEY (lecture_id) REFERENCES lectures(id),
    FOREIGN KEY (topic_id) REFERENCES topics(id),
    PRIMARY KEY (lecture_id, topic_id)
);

-- Insert lecture data
INSERT OR IGNORE INTO lectures (number, title) VALUES
    (1, 'Persistent Data Structures'),
    (2, 'Retroactive Data Structures'),
    (3, 'Geometric Structures I'),
    (4, 'Geometric Structures II'),
    (5, 'Dynamic Optimality I'),
    (6, 'Dynamic Optimality II'),
    (7, 'Memory Hierarchy Models'),
    (8, 'Cache-Oblivious Structures I'),
    (9, 'Cache-Oblivious Structures II'),
    (10, 'Dictionaries'),
    (11, 'Integer Models'),
    (12, 'Fusion Trees'),
    (13, 'Integer Lower Bounds'),
    (14, 'Sorting in Linear Time'),
    (15, 'Static Trees'),
    (16, 'Strings'),
    (17, 'Succinct Structures I'),
    (18, 'Succinct Structures II'),
    (19, 'Dynamic Graphs I'),
    (20, 'Dynamic Graphs II'),
    (21, 'Dynamic Connectivity Lower Bound'),
    (22, 'History of Memory Models');