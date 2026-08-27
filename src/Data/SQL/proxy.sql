PRAGMA foreign_keys = ON;

CREATE TABLE document_backend (
    id INTEGER PRIMARY KEY,
    body TEXT NOT NULL
);

CREATE TABLE proxy_policy (
    document_id INTEGER PRIMARY KEY REFERENCES document_backend(id),
    allowed INTEGER NOT NULL CHECK (allowed IN (0, 1))
);

INSERT INTO document_backend (id, body) VALUES
    (42, 'doc(42)'),
    (7, 'doc(7)');

INSERT INTO proxy_policy (document_id, allowed) VALUES
    (42, 1),
    (7, 0);

CREATE VIEW document_proxy AS
SELECT backend.id, backend.body
FROM document_backend AS backend
JOIN proxy_policy AS policy ON policy.document_id = backend.id
WHERE policy.allowed = 1;

SELECT 'rows=' || COUNT(*) || ';first=' ||
       COALESCE(MAX(CASE WHEN id = 42 THEN body END), 'missing')
FROM document_proxy;
