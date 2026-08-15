package io.genkidama.learn.java.helpdesk.persistence;

import io.genkidama.learn.java.helpdesk.domain.Ticket;

import java.util.ArrayList;
import java.util.List;

/** In-memory ticket store used by fast domain tests and the zero-configuration constructor. */
public final class InMemoryTicketStore implements TicketStore {
    private List<Ticket> tickets = List.of();

    @Override
    public synchronized List<Ticket> load() {
        return List.copyOf(tickets);
    }

    @Override
    public synchronized void save(List<Ticket> tickets) {
        this.tickets = List.copyOf(new ArrayList<>(tickets));
    }
}
