package io.genkidama.learn.java.helpdesk.domain;

/** Indicates that a requested ticket does not exist. */
public final class TicketNotFoundException extends RuntimeException {
    /** Creates the exception for the missing ticket identifier. */
    public TicketNotFoundException(long id) {
        super("Ticket not found: " + id);
    }
}
