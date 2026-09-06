PRAGMA foreign_keys = ON;

-- State expressed as a declarative transition relation. Missing transitions are
-- deliberate no-ops, so the current state is preserved.
CREATE TABLE state_transition (
    current_state TEXT NOT NULL,
    event TEXT NOT NULL,
    next_state TEXT NOT NULL,
    PRIMARY KEY (current_state, event),
    CHECK (current_state IN ('locked', 'unlocked')),
    CHECK (next_state IN ('locked', 'unlocked'))
);

INSERT INTO state_transition(current_state, event, next_state) VALUES
    ('locked', 'coin', 'unlocked'),
    ('unlocked', 'push', 'locked');

CREATE TABLE event_sequence (
    step INTEGER PRIMARY KEY,
    event TEXT NOT NULL
);

INSERT INTO event_sequence(step, event) VALUES
    (1, 'push'),
    (2, 'coin'),
    (3, 'coin'),
    (4, 'push');

CREATE TABLE state_trace (
    step INTEGER PRIMARY KEY,
    state TEXT NOT NULL
);

INSERT INTO state_trace(step, state)
WITH RECURSIVE trace(step, state) AS (
    VALUES(0, 'locked')
    UNION ALL
    SELECT
        event_sequence.step,
        COALESCE(state_transition.next_state, trace.state)
    FROM trace
    JOIN event_sequence ON event_sequence.step = trace.step + 1
    LEFT JOIN state_transition
      ON state_transition.current_state = trace.state
     AND state_transition.event = event_sequence.event
)
SELECT step, state FROM trace;

-- An unknown state has no legal transition in the relation and is rejected
-- explicitly rather than silently becoming a valid lifecycle state.
CREATE TABLE invalid_state_probe (
    observed_state TEXT NOT NULL
);

INSERT INTO invalid_state_probe(observed_state)
SELECT CASE
    WHEN EXISTS (
        SELECT 1
        FROM state_transition
        WHERE current_state = 'jammed'
          AND event = 'coin'
    )
    THEN 'reachable'
    ELSE 'invalid'
END;
