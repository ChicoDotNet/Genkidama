-- Observer in declarative SQL: subscription relations determine event delivery.
WITH
initial_subscriptions(observer) AS (
    VALUES ('audit'), ('dashboard')
),
subscribe_duplicate(observer) AS (
    SELECT observer FROM initial_subscriptions
    UNION
    SELECT 'audit'
),
first_event(state) AS (
    VALUES ('draft')
),
first_deliveries(observer, state) AS (
    SELECT subscriptions.observer, event.state
    FROM subscribe_duplicate AS subscriptions
    CROSS JOIN first_event AS event
),
after_unsubscribe(observer) AS (
    SELECT observer
    FROM subscribe_duplicate
    WHERE observer <> 'dashboard'
),
after_second_unsubscribe(observer) AS (
    SELECT observer
    FROM after_unsubscribe
    WHERE observer <> 'dashboard'
),
second_event(state) AS (
    VALUES ('published')
),
second_deliveries(observer, state) AS (
    SELECT subscriptions.observer, event.state
    FROM after_second_unsubscribe AS subscriptions
    CROSS JOIN second_event AS event
)
SELECT CASE
    WHEN (SELECT COUNT(*) FROM initial_subscriptions) = 2
     AND (SELECT COUNT(*) FROM subscribe_duplicate) = 2
     AND (SELECT COUNT(*) FROM first_deliveries) = 2
     AND (SELECT COUNT(*) FROM first_deliveries WHERE observer = 'audit' AND state = 'draft') = 1
     AND (SELECT COUNT(*) FROM first_deliveries WHERE observer = 'dashboard' AND state = 'draft') = 1
     AND (SELECT COUNT(*) FROM after_unsubscribe) = 1
     AND (SELECT COUNT(*) FROM after_unsubscribe WHERE observer = 'audit') = 1
     AND (SELECT COUNT(*) FROM after_second_unsubscribe) = 1
     AND (SELECT COUNT(*) FROM second_deliveries) = 1
     AND (SELECT COUNT(*) FROM second_deliveries WHERE observer = 'audit' AND state = 'published') = 1
     AND (SELECT COUNT(*) FROM second_deliveries WHERE observer = 'dashboard') = 0
    THEN 'SQL Observer: passed'
    ELSE 'SQL Observer: failed'
END;
