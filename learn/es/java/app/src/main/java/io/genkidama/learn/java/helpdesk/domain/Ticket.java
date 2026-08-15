package io.genkidama.learn.java.helpdesk.domain;

/**
 * Immutable help-desk ticket exposed by the application domain.
 *
 * @param id stable positive identifier assigned by {@link TicketService}
 * @param title normalized non-blank summary
 * @param description optional normalized detail, never {@code null}
 * @param priority business priority
 * @param status current lifecycle state
 */
public record Ticket(
        long id,
        String title,
        String description,
        TicketPriority priority,
        TicketStatus status) {
}
