package io.genkidama.learn.java.helpdesk.persistence;

import com.fasterxml.jackson.databind.ObjectMapper;
import io.genkidama.learn.java.helpdesk.domain.Ticket;
import io.genkidama.learn.java.helpdesk.domain.TicketPriority;
import io.genkidama.learn.java.helpdesk.domain.TicketService;
import io.genkidama.learn.java.helpdesk.domain.TicketStatus;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

class JsonFileTicketStoreTest {
    @TempDir
    Path tempDir;

    @Test
    void roundTripsTicketsAndRestoresNextIdentifier() {
        Path file = tempDir.resolve("tickets.json");
        var store = new JsonFileTicketStore(new ObjectMapper(), file);
        store.save(List.of(
                new Ticket(1, "Uno", "", TicketPriority.NORMAL, TicketStatus.OPEN),
                new Ticket(2, "Dos", "", TicketPriority.HIGH, TicketStatus.RESOLVED)));

        var service = new TicketService(new JsonFileTicketStore(new ObjectMapper(), file));
        Ticket created = service.create("Tres", "", TicketPriority.LOW);

        assertEquals(3L, created.id());
        assertEquals(3, service.list().size());
    }

    @Test
    void missingFileMeansFirstRunButCorruptFileFailsExplicitly() throws Exception {
        Path file = tempDir.resolve("tickets.json");
        var store = new JsonFileTicketStore(new ObjectMapper(), file);
        assertEquals(List.of(), store.load());

        Files.writeString(file, "not-json");
        assertThrows(TicketPersistenceException.class, store::load);
    }

    @Test
    void duplicateStoredIdentifiersAreRejectedByTheDomainBoundary() throws Exception {
        Path file = tempDir.resolve("tickets.json");
        Files.writeString(file, """
                [
                  {"id":1,"title":"A","description":"","priority":"NORMAL","status":"OPEN"},
                  {"id":1,"title":"B","description":"","priority":"HIGH","status":"OPEN"}
                ]
                """);

        assertThrows(TicketPersistenceException.class,
                () -> new TicketService(new JsonFileTicketStore(new ObjectMapper(), file)));
    }
}
