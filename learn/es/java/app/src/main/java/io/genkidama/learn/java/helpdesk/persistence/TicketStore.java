package io.genkidama.learn.java.helpdesk.persistence;

import io.genkidama.learn.java.helpdesk.domain.Ticket;

import java.util.List;

/**
 * Persistence boundary for complete ticket snapshots.
 * Implementations must return tickets in deterministic display order and either persist a complete snapshot or fail explicitly.
 */
public interface TicketStore {
    /**
     * Loads the current persisted snapshot.
     * @return immutable or caller-owned ticket list in display order; never {@code null}
     * @throws TicketPersistenceException when persisted data cannot be read or interpreted safely
     */
    List<Ticket> load();

    /**
     * Replaces the persisted snapshot.
     * @param tickets complete ticket state in deterministic display order
     * @throws TicketPersistenceException when the replacement cannot be completed
     */
    void save(List<Ticket> tickets);
}
