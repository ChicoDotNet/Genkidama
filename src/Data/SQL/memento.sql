-- Memento in declarative SQL: immutable relations model live state and snapshot.
WITH
originator(title, tags) AS (
    VALUES ('draft', 'pattern')
),
memento(title, tags) AS (
    SELECT title, tags FROM originator
),
mutated(title, tags) AS (
    VALUES ('published', 'pattern,edited')
),
restored(title, tags) AS (
    SELECT title, tags FROM memento
),
post_restore_originator(title, tags) AS (
    VALUES ('restored-edit', 'restored')
)
SELECT CASE
    WHEN (SELECT title FROM mutated) = 'published'
     AND (SELECT title FROM restored) = 'draft'
     AND (SELECT tags FROM restored) = 'pattern'
     AND (SELECT title FROM post_restore_originator) = 'restored-edit'
     AND (SELECT title FROM memento) = 'draft'
     AND (SELECT tags FROM memento) = 'pattern'
    THEN 'SQL Memento: passed'
    ELSE 'SQL Memento: failed'
END;
