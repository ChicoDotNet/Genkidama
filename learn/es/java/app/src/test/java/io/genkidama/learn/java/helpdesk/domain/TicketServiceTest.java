package io.genkidama.learn.java.helpdesk.domain;

import org.junit.jupiter.api.Test;

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
}
