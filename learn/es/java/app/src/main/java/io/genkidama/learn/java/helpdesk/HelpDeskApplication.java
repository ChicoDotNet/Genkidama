package io.genkidama.learn.java.helpdesk;

import com.fasterxml.jackson.databind.ObjectMapper;
import io.genkidama.learn.java.helpdesk.domain.TicketService;
import io.genkidama.learn.java.helpdesk.http.HelpDeskHttpServer;
import io.genkidama.learn.java.helpdesk.persistence.JsonFileTicketStore;

import java.nio.file.Path;

/** Application entry point for the local HelpDesk API. */
public final class HelpDeskApplication {
    private HelpDeskApplication() { }

    /**
     * Starts the HTTP API with local JSON persistence and keeps it alive until stopped.
     * @param args unused command-line arguments
     * @throws Exception when persistence initialization or the HTTP server cannot start
     */
    public static void main(String[] args) throws Exception {
        int port = Integer.parseInt(System.getenv().getOrDefault("HELPDESK_PORT", "8080"));
        Path dataFile = Path.of(System.getenv().getOrDefault("HELPDESK_DATA_FILE", "data/tickets.json"));
        ObjectMapper json = new ObjectMapper();
        var store = new JsonFileTicketStore(json, dataFile);
        var server = new HelpDeskHttpServer(new TicketService(store), json, port);
        Runtime.getRuntime().addShutdownHook(new Thread(server::close));
        server.start();
        System.out.printf("HelpDesk API listening on http://localhost:%d%n", server.port());
    }
}
