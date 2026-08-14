package io.genkidama.learn.java.helpdesk.persistence;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import io.genkidama.learn.java.helpdesk.domain.Ticket;

import java.io.IOException;
import java.nio.file.AtomicMoveNotSupportedException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.util.List;
import java.util.Objects;

/** Persists the complete HelpDesk ticket snapshot as a local JSON file. */
public final class JsonFileTicketStore implements TicketStore {
    private static final TypeReference<List<Ticket>> TICKET_LIST = new TypeReference<>() { };

    private final ObjectMapper json;
    private final Path file;

    /**
     * Creates a JSON-backed store.
     * @param json mapper used for serialization
     * @param file target state file
     */
    public JsonFileTicketStore(ObjectMapper json, Path file) {
        this.json = Objects.requireNonNull(json, "json");
        this.file = Objects.requireNonNull(file, "file");
    }

    @Override
    public List<Ticket> load() {
        if (!Files.exists(file)) {
            return List.of();
        }
        try {
            List<Ticket> tickets = json.readValue(file.toFile(), TICKET_LIST);
            return tickets == null ? List.of() : List.copyOf(tickets);
        } catch (IOException exception) {
            throw new TicketPersistenceException("could not read ticket state from " + file, exception);
        }
    }

    @Override
    public void save(List<Ticket> tickets) {
        Path parent = file.toAbsolutePath().getParent();
        Path temporary = file.resolveSibling(file.getFileName() + ".tmp");
        try {
            if (parent != null) {
                Files.createDirectories(parent);
            }
            json.writerWithDefaultPrettyPrinter().writeValue(temporary.toFile(), tickets);
            try {
                Files.move(temporary, file, StandardCopyOption.REPLACE_EXISTING, StandardCopyOption.ATOMIC_MOVE);
            } catch (AtomicMoveNotSupportedException exception) {
                Files.move(temporary, file, StandardCopyOption.REPLACE_EXISTING);
            }
        } catch (IOException exception) {
            try {
                Files.deleteIfExists(temporary);
            } catch (IOException ignored) {
                exception.addSuppressed(ignored);
            }
            throw new TicketPersistenceException("could not persist ticket state to " + file, exception);
        }
    }
}
