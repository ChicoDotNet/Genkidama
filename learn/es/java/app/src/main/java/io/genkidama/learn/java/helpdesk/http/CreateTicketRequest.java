package io.genkidama.learn.java.helpdesk.http;

import io.genkidama.learn.java.helpdesk.domain.TicketPriority;

/** JSON request accepted by {@code POST /api/tickets}. */
public record CreateTicketRequest(String title, String description, TicketPriority priority) {
}
