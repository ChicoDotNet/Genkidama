package io.genkidama.learn.java.helpdesk.domain;

/** Indicates that a ticket cannot move beyond its current lifecycle state. */
public final class InvalidTicketTransitionException extends RuntimeException {
    /** Creates an exception for a ticket whose transition is not allowed. */
    public InvalidTicketTransitionException(Ticket ticket) {
        super("Ticket " + ticket.id() + " cannot advance from " + ticket.status());
    }
}
