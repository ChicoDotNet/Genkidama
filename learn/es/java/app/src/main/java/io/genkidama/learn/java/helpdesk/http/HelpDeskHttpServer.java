package io.genkidama.learn.java.helpdesk.http;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpServer;
import io.genkidama.learn.java.helpdesk.domain.InvalidTicketTransitionException;
import io.genkidama.learn.java.helpdesk.domain.Ticket;
import io.genkidama.learn.java.helpdesk.domain.TicketNotFoundException;
import io.genkidama.learn.java.helpdesk.domain.TicketService;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.nio.charset.StandardCharsets;
import java.util.Map;

/**
 * Minimal HTTP adapter for the HelpDesk ticket domain.
 * Domain behavior stays in {@link TicketService}; this class translates HTTP/JSON to domain calls.
 */
public final class HelpDeskHttpServer implements AutoCloseable {
    private final TicketService tickets;
    private final ObjectMapper json;
    private final HttpServer server;

    /**
     * Creates a server bound to the requested port.
     * @param tickets domain service
     * @param json JSON mapper used only at the HTTP boundary
     * @param port TCP port; use {@code 0} in tests to request an ephemeral port
     * @throws IOException when the socket cannot be created
     */
    public HelpDeskHttpServer(TicketService tickets, ObjectMapper json, int port) throws IOException {
        this.tickets = tickets;
        this.json = json;
        this.server = HttpServer.create(new InetSocketAddress(port), 0);
        this.server.createContext("/health", this::handleHealth);
        this.server.createContext("/api/tickets", this::handleTickets);
    }

    /** Starts accepting HTTP requests. */
    public void start() {
        server.start();
    }

    /** Returns the actual bound port, useful when tests requested port 0. */
    public int port() {
        return server.getAddress().getPort();
    }

    @Override
    public void close() {
        server.stop(0);
    }

    private void handleHealth(HttpExchange exchange) throws IOException {
        if (!"GET".equals(exchange.getRequestMethod())) {
            send(exchange, 405, new ErrorResponse("method not allowed"));
            return;
        }
        send(exchange, 200, Map.of("status", "ok"));
    }

    private void handleTickets(HttpExchange exchange) throws IOException {
        try {
            String path = exchange.getRequestURI().getPath();
            if ("/api/tickets".equals(path)) {
                handleCollection(exchange);
                return;
            }
            if (path.matches("/api/tickets/\\d+/advance")) {
                handleAdvance(exchange, path);
                return;
            }
            send(exchange, 404, new ErrorResponse("route not found"));
        } catch (TicketNotFoundException exception) {
            send(exchange, 404, new ErrorResponse(exception.getMessage()));
        } catch (InvalidTicketTransitionException exception) {
            send(exchange, 409, new ErrorResponse(exception.getMessage()));
        } catch (IllegalArgumentException | JsonProcessingException exception) {
            send(exchange, 400, new ErrorResponse(exception.getMessage()));
        }
    }

    private void handleCollection(HttpExchange exchange) throws IOException {
        switch (exchange.getRequestMethod()) {
            case "GET" -> send(exchange, 200, tickets.list());
            case "POST" -> {
                CreateTicketRequest request = json.readValue(exchange.getRequestBody(), CreateTicketRequest.class);
                Ticket created = tickets.create(request.title(), request.description(), request.priority());
                send(exchange, 201, created);
            }
            default -> send(exchange, 405, new ErrorResponse("method not allowed"));
        }
    }

    private void handleAdvance(HttpExchange exchange, String path) throws IOException {
        if (!"POST".equals(exchange.getRequestMethod())) {
            send(exchange, 405, new ErrorResponse("method not allowed"));
            return;
        }
        String rawId = path.substring("/api/tickets/".length(), path.length() - "/advance".length());
        long id = Long.parseLong(rawId);
        send(exchange, 200, tickets.advance(id));
    }

    private void send(HttpExchange exchange, int statusCode, Object body) throws IOException {
        byte[] payload = json.writeValueAsString(body).getBytes(StandardCharsets.UTF_8);
        exchange.getResponseHeaders().set("Content-Type", "application/json; charset=utf-8");
        exchange.sendResponseHeaders(statusCode, payload.length);
        try (var output = exchange.getResponseBody()) {
            output.write(payload);
        }
    }
}
