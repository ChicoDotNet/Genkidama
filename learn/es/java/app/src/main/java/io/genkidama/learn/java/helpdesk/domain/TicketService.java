package io.genkidama.learn.java.helpdesk.domain;

import io.genkidama.learn.java.helpdesk.persistence.InMemoryTicketStore;
import io.genkidama.learn.java.helpdesk.persistence.TicketPersistenceException;
import io.genkidama.learn.java.helpdesk.persistence.TicketStore;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

/**
 * Owns deterministic ticket creation, queries and lifecycle rules independently of HTTP and JSON.
 * Mutations persist a complete candidate snapshot before it becomes visible in memory.
 */
public final class TicketService {
    private static final int MAX_TITLE_LENGTH = 120;
    private static final int MAX_DESCRIPTION_LENGTH = 2_000;

    private final TicketStore store;
    private final Map<Long, Ticket> tickets = new LinkedHashMap<>();
    private long nextId;

    /** Creates a zero-configuration service backed by memory. */
    public TicketService() {
        this(new InMemoryTicketStore());
    }

    /**
     * Creates a service and restores its complete state from a persistence boundary.
     * @param store persistence implementation
     * @throws TicketPersistenceException when stored state is unreadable or violates domain invariants
     */
    public TicketService(TicketStore store) {
        this.store = Objects.requireNonNull(store, "store");
        List<Ticket> loaded = store.load();
        validateStoredState(loaded);
        for (Ticket ticket : loaded) {
            tickets.put(ticket.id(), ticket);
        }
        nextId = loaded.stream().mapToLong(Ticket::id).max().orElse(0L) + 1;
    }

    /**
     * Creates one open ticket and persists it before exposing it.
     *
     * @param title required summary, trimmed before storage
     * @param description optional detail, trimmed before storage
     * @param priority optional priority; {@code NORMAL} is used when omitted
     * @return immutable created ticket
     * @throws IllegalArgumentException when text violates the domain limits
     * @throws TicketPersistenceException when persistence fails; visible state remains unchanged
     */
    public synchronized Ticket create(String title, String description, TicketPriority priority) {
        String normalizedTitle = requireText(title, "title", MAX_TITLE_LENGTH);
        String normalizedDescription = normalizeOptional(description, MAX_DESCRIPTION_LENGTH);
        TicketPriority normalizedPriority = priority == null ? TicketPriority.NORMAL : priority;

        Ticket ticket = new Ticket(
                nextId,
                normalizedTitle,
                normalizedDescription,
                normalizedPriority,
                TicketStatus.OPEN);
        Map<Long, Ticket> candidate = new LinkedHashMap<>(tickets);
        candidate.put(ticket.id(), ticket);
        persistCandidate(candidate);
        nextId++;
        return ticket;
    }

    /** Returns an immutable snapshot in creation order. */
    public synchronized List<Ticket> list() {
        return list(TicketQuery.all());
    }

    /**
     * Returns tickets matching every configured filter while preserving creation order.
     * @param query optional status/priority filters
     * @return immutable matching snapshot
     */
    public synchronized List<Ticket> list(TicketQuery query) {
        TicketQuery normalized = query == null ? TicketQuery.all() : query;
        return tickets.values().stream().filter(normalized::matches).toList();
    }

    /**
     * Computes operational counts from the current immutable ticket snapshot.
     * @return deterministic summary that does not change or persist ticket state
     */
    public synchronized TicketSummary summary() {
        List<Ticket> snapshot = List.copyOf(tickets.values());
        return new TicketSummary(
                snapshot.size(),
                countStatus(snapshot, TicketStatus.OPEN),
                countStatus(snapshot, TicketStatus.IN_PROGRESS),
                countStatus(snapshot, TicketStatus.RESOLVED),
                countPriority(snapshot, TicketPriority.LOW),
                countPriority(snapshot, TicketPriority.NORMAL),
                countPriority(snapshot, TicketPriority.HIGH));
    }

    /**
     * Finds one ticket.
     * @param id positive ticket identifier
     * @return existing ticket
     * @throws TicketNotFoundException when the identifier is unknown
     */
    public synchronized Ticket get(long id) {
        Ticket ticket = tickets.get(id);
        if (ticket == null) {
            throw new TicketNotFoundException(id);
        }
        return ticket;
    }

    /**
     * Changes ticket priority without changing its lifecycle state.
     * @param id ticket identifier
     * @param priority new non-null priority
     * @return updated immutable ticket
     * @throws TicketNotFoundException when the ticket is absent
     * @throws NullPointerException when priority is null
     * @throws TicketPersistenceException when persistence fails; visible state remains unchanged
     */
    public synchronized Ticket changePriority(long id, TicketPriority priority) {
        Ticket current = get(id);
        Ticket updated = new Ticket(
                current.id(),
                current.title(),
                current.description(),
                Objects.requireNonNull(priority, "priority"),
                current.status());
        return replacePersisted(updated);
    }

    /**
     * Advances OPEN -> IN_PROGRESS -> RESOLVED.
     * @param id ticket identifier
     * @return updated immutable ticket
     * @throws TicketNotFoundException when the ticket is absent
     * @throws InvalidTicketTransitionException when the ticket is already resolved
     * @throws TicketPersistenceException when persistence fails; visible state remains unchanged
     */
    public synchronized Ticket advance(long id) {
        Ticket current = get(id);
        TicketStatus nextStatus = switch (current.status()) {
            case OPEN -> TicketStatus.IN_PROGRESS;
            case IN_PROGRESS -> TicketStatus.RESOLVED;
            case RESOLVED -> throw new InvalidTicketTransitionException(current);
        };
        Ticket updated = new Ticket(
                current.id(),
                current.title(),
                current.description(),
                current.priority(),
                nextStatus);
        return replacePersisted(updated);
    }

    private Ticket replacePersisted(Ticket updated) {
        Map<Long, Ticket> candidate = new LinkedHashMap<>(tickets);
        candidate.put(updated.id(), updated);
        persistCandidate(candidate);
        return updated;
    }

    private void persistCandidate(Map<Long, Ticket> candidate) {
        store.save(List.copyOf(candidate.values()));
        tickets.clear();
        tickets.putAll(candidate);
    }

    private static long countStatus(List<Ticket> tickets, TicketStatus status) {
        return tickets.stream().filter(ticket -> ticket.status() == status).count();
    }

    private static long countPriority(List<Ticket> tickets, TicketPriority priority) {
        return tickets.stream().filter(ticket -> ticket.priority() == priority).count();
    }

    private static void validateStoredState(List<Ticket> loaded) {
        if (loaded == null) {
            throw new TicketPersistenceException("stored ticket state must not be null");
        }
        Set<Long> ids = new HashSet<>();
        for (Ticket ticket : new ArrayList<>(loaded)) {
            if (ticket == null || ticket.id() <= 0 || !ids.add(ticket.id())) {
                throw new TicketPersistenceException("stored ticket state contains an invalid or duplicate id");
            }
            try {
                requireText(ticket.title(), "title", MAX_TITLE_LENGTH);
                normalizeOptional(ticket.description(), MAX_DESCRIPTION_LENGTH);
                Objects.requireNonNull(ticket.priority(), "priority");
                Objects.requireNonNull(ticket.status(), "status");
            } catch (IllegalArgumentException | NullPointerException exception) {
                throw new TicketPersistenceException("stored ticket state violates domain invariants", exception);
            }
        }
    }

    private static String requireText(String value, String field, int maxLength) {
        if (value == null || value.isBlank()) {
            throw new IllegalArgumentException(field + " must not be blank");
        }
        String normalized = value.strip();
        if (normalized.length() > maxLength) {
            throw new IllegalArgumentException(field + " exceeds " + maxLength + " characters");
        }
        return normalized;
    }

    private static String normalizeOptional(String value, int maxLength) {
        if (value == null) {
            return "";
        }
        String normalized = value.strip();
        if (normalized.length() > maxLength) {
            throw new IllegalArgumentException("description exceeds " + maxLength + " characters");
        }
        return normalized;
    }
}
