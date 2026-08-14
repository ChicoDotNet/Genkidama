package io.genkidama.learn.java.helpdesk.domain;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

/**
 * Owns deterministic ticket creation and lifecycle rules independently of HTTP and JSON.
 * The first course increment keeps state in memory; later persistence can replace this boundary.
 */
public final class TicketService {
    private static final int MAX_TITLE_LENGTH = 120;
    private static final int MAX_DESCRIPTION_LENGTH = 2_000;

    private final Map<Long, Ticket> tickets = new LinkedHashMap<>();
    private long nextId = 1;

    /**
     * Creates one open ticket.
     *
     * @param title required summary, trimmed before storage
     * @param description optional detail, trimmed before storage
     * @param priority optional priority; {@code NORMAL} is used when omitted
     * @return immutable created ticket
     * @throws IllegalArgumentException when text violates the domain limits
     */
    public synchronized Ticket create(String title, String description, TicketPriority priority) {
        String normalizedTitle = requireText(title, "title", MAX_TITLE_LENGTH);
        String normalizedDescription = normalizeOptional(description, MAX_DESCRIPTION_LENGTH);
        TicketPriority normalizedPriority = priority == null ? TicketPriority.NORMAL : priority;

        Ticket ticket = new Ticket(
                nextId++,
                normalizedTitle,
                normalizedDescription,
                normalizedPriority,
                TicketStatus.OPEN);
        tickets.put(ticket.id(), ticket);
        return ticket;
    }

    /** Returns an immutable snapshot in creation order. */
    public synchronized List<Ticket> list() {
        return List.copyOf(new ArrayList<>(tickets.values()));
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
     * Advances OPEN -> IN_PROGRESS -> RESOLVED.
     * @param id ticket identifier
     * @return updated immutable ticket
     * @throws TicketNotFoundException when the ticket is absent
     * @throws InvalidTicketTransitionException when the ticket is already resolved
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
        tickets.put(id, updated);
        return updated;
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
