package io.genkidama.learn.java.helpdesk.domain;

/**
 * Optional filters for ticket queries. A {@code null} field means "all values" for that dimension.
 * @param status lifecycle filter or {@code null}
 * @param priority priority filter or {@code null}
 */
public record TicketQuery(TicketStatus status, TicketPriority priority) {
    /** Returns a query that matches every ticket. */
    public static TicketQuery all() {
        return new TicketQuery(null, null);
    }

    /**
     * Tests whether a ticket satisfies every configured filter.
     * @param ticket ticket to inspect
     * @return {@code true} when all non-null filters match
     */
    public boolean matches(Ticket ticket) {
        return (status == null || ticket.status() == status)
                && (priority == null || ticket.priority() == priority);
    }
}
