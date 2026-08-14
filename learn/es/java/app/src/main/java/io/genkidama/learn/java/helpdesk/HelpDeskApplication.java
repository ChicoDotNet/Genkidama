package io.genkidama.learn.java.helpdesk;

import com.fasterxml.jackson.databind.ObjectMapper;
import io.genkidama.learn.java.helpdesk.domain.TicketService;
import io.genkidama.learn.java.helpdesk.http.HelpDeskHttpServer;

/** Application entry point for the local HelpDesk API. */
public final class HelpDeskApplication {
    private HelpDeskApplication() {
    }

    /**
     * Starts the HTTP API and keeps it alive until the process is stopped.
     * @param args unused command-line arguments
     * @throws Exception when the HTTP server cannot start
     */
    public static void main(String[] args) throws Exception {
        int port = Integer.parseInt(System.getenv().getOrDefault("HELPDESK_PORT", "8080"));
        var server = new HelpDeskHttpServer(new TicketService(), new ObjectMapper(), port);
        Runtime.getRuntime().addShutdownHook(new Thread(server::close));
        server.start();
        System.out.printf("HelpDesk API listening on http://localhost:%d%n", server.port());
    }
}
