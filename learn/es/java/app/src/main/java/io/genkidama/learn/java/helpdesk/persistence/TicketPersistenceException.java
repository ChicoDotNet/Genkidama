package io.genkidama.learn.java.helpdesk.persistence;

/** Signals that ticket state could not be loaded or persisted safely. */
public final class TicketPersistenceException extends RuntimeException {
    /**
     * Creates an actionable persistence failure.
     * @param message human-readable operation failure without sensitive payload data
     * @param cause underlying I/O or serialization error
     */
    public TicketPersistenceException(String message, Throwable cause) {
        super(message, cause);
    }

    /**
     * Creates a persistence failure caused by invalid stored state.
     * @param message human-readable validation failure
     */
    public TicketPersistenceException(String message) {
        super(message);
    }
}
