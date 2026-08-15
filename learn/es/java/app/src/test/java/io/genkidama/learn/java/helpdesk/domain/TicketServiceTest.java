package io.genkidama.learn.java.helpdesk.domain;

import io.genkidama.learn.java.helpdesk.persistence.TicketPersistenceException;
import io.genkidama.learn.java.helpdesk.persistence.TicketStore;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

class TicketServiceTest {
    @Test
    void createsNormalizedOpenTicketWithNormalPriorityByDefault() {
        var service = new TicketService();
        Ticket ticket = service.create("  No puedo iniciar sesión  ", "  Error al autenticar  ", null);
        assertEquals(1L, ticket.id());
        assertEquals("No puedo iniciar sesión", ticket.title());
        assertEquals("Error al autenticar", ticket.description());
        assertEquals(TicketPriority.NORMAL, ticket.priority());
        assertEquals(TicketStatus.OPEN, ticket.status());
    }

    @Test
    void assignsStableSequentialIdsAndPreservesCreationOrder() {
        var service = new TicketService();
        service.create("Primero", "", TicketPriority.LOW);
        service.create("Segundo", "", TicketPriority.HIGH);
        assertEquals(1L, service.list().get(0).id());
        assertEquals(2L, service.list().get(1).id());
    }

    @Test
    void advancesOnlyThroughTheSupportedLifecycle() {
        var service = new TicketService();
        Ticket created = service.create("Impresora detenida", "Piso 2", TicketPriority.HIGH);
        assertEquals(TicketStatus.IN_PROGRESS, service.advance(created.id()).status());
        assertEquals(TicketStatus.RESOLVED, service.advance(created.id()).status());
        assertThrows(InvalidTicketTransitionException.class, () -> service.advance(created.id()));
    }

    @Test
    void filtersByStatusAndPriorityWithoutMutatingOrder() {
        var service = new TicketService();
        Ticket high = service.create("VPN", "", TicketPriority.HIGH);
        service.create("Monitor", "", TicketPriority.LOW);
        service.advance(high.id());

        List<Ticket> matches = service.list(new TicketQuery(TicketStatus.IN_PROGRESS, TicketPriority.HIGH));

        assertEquals(List.of(high.id()), matches.stream().map(Ticket::id).toList());
    }

    @Test
    void computesSummaryFromTheCurrentSnapshot() {
        var service = new TicketService();
        Ticket first = service.create("VPN", "", TicketPriority.HIGH);
        service.create("Monitor", "", TicketPriority.LOW);
        service.create("Correo", "", TicketPriority.NORMAL);
        service.advance(first.id());

        TicketSummary summary = service.summary();

        assertEquals(3, summary.total());
        assertEquals(2, summary.open());
        assertEquals(1, summary.inProgress());
        assertEquals(0, summary.resolved());
        assertEquals(1, summary.low());
        assertEquals(1, summary.normal());
        assertEquals(1, summary.high());
    }

    @Test
    void concurrentCreatorsReceiveUniqueIdsWithoutLosingTickets() throws Exception {
        var service = new TicketService();
        var executor = Executors.newFixedThreadPool(4);
        try {
            var futures = new ArrayList<Future<Ticket>>();
            for (int index = 0; index < 20; index++) {
                int ticketNumber = index;
                futures.add(executor.submit(
                        () -> service.create("Ticket " + ticketNumber, "", TicketPriority.NORMAL)));
            }
            var ids = new HashSet<Long>();
            for (Future<Ticket> future : futures) {
                ids.add(future.get().id());
            }
            assertEquals(20, ids.size());
            assertEquals(20, service.list().size());
        } finally {
            executor.shutdownNow();
        }
    }

    @Test
    void changesPriorityWithoutChangingLifecycleState() {
        var service = new TicketService();
        Ticket ticket = service.create("Correo lento", "", TicketPriority.NORMAL);
        service.advance(ticket.id());

        Ticket updated = service.changePriority(ticket.id(), TicketPriority.HIGH);

        assertEquals(TicketPriority.HIGH, updated.priority());
        assertEquals(TicketStatus.IN_PROGRESS, updated.status());
    }

    @Test
    void failedPersistenceDoesNotExposeCandidateOrConsumeId() {
        var service = new TicketService(new FailingStore());

        assertThrows(TicketPersistenceException.class,
                () -> service.create("No debe quedar visible", "", TicketPriority.NORMAL));
        assertEquals(List.of(), service.list());
    }

    @Test
    void rejectsBlankAndOversizedTitles() {
        var service = new TicketService();
        assertThrows(IllegalArgumentException.class, () -> service.create("   ", "", null));
        assertThrows(IllegalArgumentException.class, () -> service.create("x".repeat(121), "", null));
    }

    @Test
    void reportsMissingTicketExplicitly() {
        var service = new TicketService();
        assertThrows(TicketNotFoundException.class, () -> service.get(404));
    }

    private static final class FailingStore implements TicketStore {
        @Override
        public List<Ticket> load() { return List.of(); }

        @Override
        public void save(List<Ticket> tickets) {
            throw new TicketPersistenceException("simulated persistence failure");
        }
    }
}
